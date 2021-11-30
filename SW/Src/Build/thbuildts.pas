unit ThBuildTS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  LCLIntf, LCLType,
  AsmTypes, BuildBase, BuildCpuX;

Type
  TThBuild = class;

  TOnLogWritten = Procedure ( Const ALogName : string ) of Object;

  TThBuild = class (TThread)
  private
    FErrorList,
    FVerifList          : TStringList;
    FSrcList,
    FIncList            : TStringList;
    FPrjPath,
    FPrjName,
    FDstPath            : string;

    FBuildAnyEvt        : PRtlEvent;

    FDebugActions       : string;
    FLst                : TStringList;
    FDbg                : TStringList;
    FBin                : TMemoryStream;
    FMem                : TStringList;
    FMap                : TStringList;
    FHex                : TStringList;

    FTtyInList,
    FTtyList            : TStringList;
    FTtyInLock          : TCriticalSection;

    FBinName,
    FLstName,
    FDbgName,
    FMemName,
    FMapName,
    FHexName            : string;

    FCompileStart,
    FCompileBusy,
    FCompileReady       : boolean;

    FOnLogWritten       : TOnLogWritten;
    FOnViewAny          : TOnViewAny;

    FIsEnded            : boolean;

    Procedure AppendError ( Const AComment : string );
    Procedure AppendVerifA ( Const AComment : string );

    Procedure ViewAny ( Const AMessage : string );

    Procedure TestSuiteTty ( Const ATty : string );

    Function DeleteOld : boolean;
    Function SaveNew : boolean;
    Procedure ProcessTS;
  protected
    Procedure Execute; Override;
  public
    Constructor Create ( CreateSuspended : boolean );
    Destructor Destroy; Override;

    Function CanCompile : boolean;
    Procedure CompileStart ( Const APrjName : string; Const ASrcItems : string; Const ADebugActions : string );

    property SrcList : TStringList read FSrcList;
    property IncList : TStringList read FIncList;

    property CompileReady : boolean read FCompileReady;
    property OnLogWritten : TOnLogWritten read FOnLogWritten write FOnLogWritten;
    property OnViewAny : TOnViewAny read FOnViewAny write FOnViewAny;

    property IsEnded : boolean read FIsEnded;

    property TextDbg : TStringList read FDbg;
  end;

implementation

Uses
  ConComL, ConComS;

{
 *** TThBuild
}

Constructor TThBuild.Create ( CreateSuspended : boolean );
Begin
 Inherited Create(CreateSuspended);
 FBuildAnyEvt:=RtlEventCreate;
 InitializeCriticalSection(FTtyInLock);
 FErrorList:=TStringList.Create;
 FVerifList:=TStringList.Create;
 FSrcList:=TStringList.Create;
 FIncList:=TStringList.Create;
 FLst:=TStringList.Create;
 FDbg:=TStringList.Create;
 FBin:=TMemoryStream.Create;
 FMem:=TStringList.Create;
 FMap:=TStringList.Create;
 FHex:=TStringList.Create;
 FTtyList:=TStringList.Create;
 FTtyInList:=TStringList.Create;
 FIsEnded:=FALSE;
End;

Destructor TThBuild.Destroy;
Begin
 FTtyInList.Free;
 FTtyList.Free;
 FHex.Free;
 FMap.Free;
 FMem.Free;
 FBin.Free;
 FDbg.Free;
 FLst.Free;
 FIncList.Free;
 FSrcList.Free;
 FVerifList.Free;
 FErrorList.Free;
 DeleteCriticalSection(FTtyInLock);
 RtlEventDestroy(FBuildAnyEvt);
 Inherited;
End;

Function TThBuild.CanCompile : boolean;
Begin
 Result:=(FCompileStart=FALSE) and (FCompileBusy=FALSE);
End;

Procedure TThBuild.CompileStart ( Const APrjName : string; Const ASrcItems : string; Const ADebugActions : string );
Var
  BSrcItems     : string;
  BItemsA,
  BParamA       : string;
  BExt          : string;
Begin
 SplitFilename(APrjName,FPrjPath,FPrjName,BExt);

 BSrcItems:=ASrcItems;

 BItemsA:=ReadTillC(BSrcItems,#13);
 FSrcList.Clear;
 repeat
 BParamA:=ReadParamStr(BItemsA);
 if BParamA='' then break;
 FSrcList.Append(AbsFilename(FPrjPath,BParamA));
 until FALSE;

 BItemsA:=ReadTillC(BSrcItems,#13);
 FIncList.Clear;
 repeat
 BParamA:=ReadParamStr(BItemsA);
 if BParamA='' then break;
 FIncList.Append(AbsFilename(FPrjPath,BParamA));
 until FALSE;

 FDstPath:=AbsFilename(FPrjPath,ReadParamStr(BSrcItems));
 if FDstPath='' then FDstPath:=FPrjPath;
 FDebugActions:=ADebugActions;

 repeat
 if FSrcList.Count=0 then begin ViewAny('eProject source list is empty'); break;  end;

 FBinName:=AssembleFullName(FDstPath,FPrjName,'bin');
 FLstName:=AssembleFullName(FDstPath,FPrjName,'lst');
 FDbgName:=AssembleFullName(FDstPath,FPrjName,'dbg');
 FMemName:=AssembleFullName(FDstPath,FPrjName,'mem');
 FMapName:=AssembleFullName(FDstPath,FPrjName,'map');
 FHexName:=AssembleFullName(FDstPath,FPrjName,'hex');

 FCompileStart:=TRUE;
 RtlEventSetEvent(FBuildAnyEvt);
 until TRUE;
End;

Procedure TThBuild.AppendError ( Const AComment : string );
Begin
 FErrorList.Append(AComment);
 ViewAny(AComment);
End;

Procedure TThBuild.AppendVerifA ( Const AComment : string );
Begin
 FVerifList.Append(AComment);
End;

Procedure TThBuild.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Function DeleteFileA ( Const AFilename : string ) : boolean;
Begin
 if FileExists(AFilename) then Result:=DeleteFile(AFilename)
 else Result:=TRUE;
End;

Function TThBuild.DeleteOld : boolean;
Begin
 Result:=TRUE;
 if DeleteFileA(FBinName)=FALSE then begin AppendError('wCannot delete old file '+FBinName); Result:=FALSE; end;
 if DeleteFileA(FLstName)=FALSE then begin AppendError('wCannot delete old file '+FLstName); Result:=FALSE; end;
 if DeleteFileA(FDbgName)=FALSE then begin AppendError('wCannot delete old file '+FDbgName); Result:=FALSE; end;
 if DeleteFileA(FMemName)=FALSE then begin AppendError('wCannot delete old file '+FMemName); Result:=FALSE; end;
 if DeleteFileA(FMapName)=FALSE then begin AppendError('wCannot delete old file '+FMapName); Result:=FALSE; end;
 if DeleteFileA(FHexName)=FALSE then begin AppendError('wCannot delete old file '+FHexName); Result:=FALSE; end;
End;

Function TThBuild.SaveNew : boolean;
Begin
 Result:=TRUE;
 repeat
 try
   if ForceDirectories(FDstPath)=FALSE then
    begin
    AppendError('eCannot create directory '+FDstPath);
    Result:=FALSE;
    break;
    end;
 except
   AppendError('eUnexpected error when creating directory '+FDstPath);
   Result:=FALSE;
   break;
 end;
 try FBin.SaveToFile(FBinName); except AppendError('eCannot save file '+FBinName); Result:=FALSE; end;
 try FLst.SaveToFile(FLstName); except AppendError('eCannot save file '+FLstName); Result:=FALSE; end;
 try FDbg.SaveToFile(FDbgName); except AppendError('eCannot save file '+FdbgName); Result:=FALSE; end;
 try FMem.SaveToFile(FMemName); except AppendError('eCannot save file '+FMemName); Result:=FALSE; end;
 try FMap.SaveToFile(FMapName); except AppendError('eCannot save file '+FMapName); Result:=FALSE; end;
 try FHex.SaveToFile(FHexName); except AppendError('eCannot save file '+FHexName); Result:=FALSE; end;
 until TRUE;
End;

Procedure TThBuild.Execute;
Var
  BSleep        : Integer;
Begin
 FLst.Clear;
 FDbg.Clear;
 FBin.Clear;
 FMem.Clear;
 FMap.Clear;
 FHex.Clear;

 ViewAny('u0');

 repeat
 BSleep:=50;
 RtlEventWaitFor(FBuildAnyEvt,50);
 if FCompileStart then
  begin
  FCompileBusy:=TRUE;
  FCompileReady:=FALSE;
  FCompileStart:=FALSE;
  ViewAny('u1');

  ProcessTS;

  FCompileReady:=TRUE;
  FCompileBusy:=FALSE;
  ViewAny('u0');
  BSleep:=0;
  end;

 if BSleep<>0 then Sleep(BSleep);
 until Terminated;

 FIsEnded:=TRUE;
 ViewAny('mClose');
End;

Procedure TThBuild.ProcessTS;
Var
  BBuild        : TBuildBase;  //todo: # make this FBuild
  BIndex        : Integer;
  BTimeA,
  BTimeB        : TDateTime;
  BDummyS       : string;
  BPos          : Integer;
  BTimeSpent    : Double;
  BTimeSpentS   : string;
Begin
 BBuild:=TBuildCpuX.Create;
 BBuild.OnAppendError:=@AppendError;
 BBuild.OnAppendVerif:=@AppendVerifA;

 FErrorList.Clear;
 FVerifList.Clear;

 FBin.Clear;
 FLst.Clear;
 FDbg.Clear;
 FMem.Clear;
 FMap.Clear;
 FHex.Clear;

 repeat
 AppendError('iCompilation started '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
 if DeleteOld=FALSE then break;
 AppendError('iOld compilation files are deleted '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 for BIndex:=0 to FSrcList.Count-1 do BBuild.AppendSrcName(FSrcList.Strings[BIndex]);
 for BIndex:=0 to FIncList.Count-1 do BBuild.AppendIncPath(FIncList.Strings[BIndex]);

 BTimeA:=Now;
 if BBuild.Build(FPrjPath,FDstPath,8,1024,FBin,FLst,FDbg,FMem,FHex,FMap)=FALSE then
  begin
  AppendError('iCompilation stopped due to errors '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
  break;
  end;
 BTimeB:=Now;
 BTimeSpent:=BTimeB-BTimeA;
 //BTimeSpentS:=FormatDateTime('HH:NN.SS,zzz',BTimeSpent);
 BTimeSpentS:=FormatFloat('#0.000',BTimeSpent*86400);
 AppendError('iCompilation time: '+BTimeSpentS+'s');

 AppendError('iProject compiled successfully '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
 if SaveNew=FALSE then break;

 AppendError('iCompilation files are generated '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 ViewAny('u2');
 BDummyS:='d'+FPrjName+#32+FPrjPath+#32+FDstPath+#32+FLstName+'#'+FLst.Text+#0;
 BPos:=Length(BDummyS);
 SetLength(BDummyS,BPos+FBin.Size);
 FBin.Read(BDummyS[BPos+1],FBin.Size);
 ViewAny(BDummyS);
 ViewAny('c'+FDebugActions);
 FDebugActions:='';

 until TRUE;

 BBuild.Free;
End;

Procedure TThBuild.TestSuiteTty ( Const ATty : string );
Begin
 EnterCriticalSection(FTtyInLock);
 FTtyInList.Append(ATty);
 LeaveCriticalSection(FTtyInLock);
End;

end.

