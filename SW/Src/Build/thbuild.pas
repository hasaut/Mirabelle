unit ThBuild;

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
    FErrorList          : TStringList;
    FCoreList,
    FSegListS,
    FDefSegNames        : string;
    FSrcList,
    FIncList            : TStringList;
    FPrjFullName        : string;
    FPrjPath,
    FPrjName,
    FDstPath            : string;

    FBuild              : TBuildBase;
    FBuildAnyEvt        : PRtlEvent;

    FDebugActions       : string;

    FCompileStart,
    FCompileBusy,
    FCompileReady       : boolean;

    FOnLogWritten       : TOnLogWritten;
    FOnViewAny          : TOnViewAny;

    FIsEnded            : boolean;

    Procedure AppendError ( Const AComment : string );

    Procedure ViewAny ( Const AMessage : string );

    Procedure ProcessBuild;
    Function GetDbg : TStringList;
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

    property TextDbg : TStringList read GetDbg;
    property Builder : TBuildBase read FBuild;
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
 FErrorList:=TStringList.Create;
 FSrcList:=TStringList.Create;
 FIncList:=TStringList.Create;
 FBuild:=TBuildCpuX.Create;
 FBuild.OnAppendError:=@AppendError;
 FIsEnded:=FALSE;
End;

Destructor TThBuild.Destroy;
Begin
 FBuild.Free;
 FIncList.Free;
 FSrcList.Free;
 FErrorList.Free;
 RtlEventDestroy(FBuildAnyEvt);
 Inherited;
End;

Function TThBuild.GetDbg : TStringList;
Begin
 Result:=FBuild.Dbg;
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
 FPrjFullName:=APrjName;
 SplitFilename(APrjName,FPrjPath,FPrjName,BExt);

 BSrcItems:=ASrcItems;

 FCoreList:=ReadTillC(BSrcItems,#13);
 FSegListS:=ReadTillC(BSrcItems,#13);
 FDefSegNames:=ReadTillC(BSrcItems,#13);

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

 FCompileStart:=TRUE;
 RtlEventSetEvent(FBuildAnyEvt);
 until TRUE;
End;

Procedure TThBuild.AppendError ( Const AComment : string );
Begin
 FErrorList.Append(AComment);
 ViewAny(AComment);
End;

Procedure TThBuild.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TThBuild.Execute;
Var
  BSleep        : Integer;
Begin
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

  ProcessBuild;

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

Procedure TThBuild.ProcessBuild;
Var
  BIndex        : Integer;
  BTimeA,
  BTimeB        : TDateTime;
  BTimeSpent    : Double;
  BTimeSpentS   : string;
Begin
 FBuild.Clear;
 FErrorList.Clear;

 repeat
 AppendError('iCompilation started '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
 if FBuild.DeleteOld=FALSE then break;
 AppendError('iOld compilation files are deleted '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 for BIndex:=0 to FSrcList.Count-1 do FBuild.AppendSrcName(FSrcList.Strings[BIndex]);
 for BIndex:=0 to FIncList.Count-1 do FBuild.AppendIncPath(FIncList.Strings[BIndex]);

 BTimeA:=Now;
 FBuild.InitPrj(FPrjFullName,FDstPath,FCoreList,FSegListS,FDefSegNames);
 if FBuild.Build=FALSE then
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
 if FBuild.SaveNew=FALSE then break;

 AppendError('iCompilation files are generated '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 ViewAny('u2');
 ViewAny('d'+FCoreList+#13+FPrjFullName+#13+FBuild.LstName+#13+FBuild.Lst.Text);
 ViewAny('x'+FBuild.ExportExec);
 ViewAny('c'+FDebugActions);
 FDebugActions:='';

 until TRUE;

End;

end.

