unit MsModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Dynlibs, Math, AsmTypes_sd;

Type
  TDataSA = array [0..63] of QWord;

  PStimData = ^QWord;
  TModInit = Function ( APrjPath : PChar; AModelPath : PChar ) : Integer; Stdcall;
  TModDone = Function : Integer; Stdcall;
  TModStep = Function ( ADataSA : PStimData ) : Double; Stdcall;
  TModVersion = Function : Cardinal; StdCall;
  TModGetLastError = Function : string; StdCall;

  TMsModel = class(TObject)
  private
    FPrjPath    : string;
    FModLib     : TLibHandle;
    FOnViewAny  : TOnViewAny;
    FModLibName : string;

    FModInit    : TModInit;
    FModDone    : TModDone;
    FModStep    : TModStep;
    FModVersion : TModVersion;
    //FModGetLastError : TModGetLastError;

    FVersCode   : Cardinal;
    FVersStrC,
    FVersStrV   : string;

    FDateTimeS  : string;

    FMdVersion      : Cardinal;

    FIteration      : Integer;
    FEndReached     : boolean; // When the last iteration was processed
    FModelExecStart : Double;
    FLogTime        : Cardinal;

    FLastProgress   : Single;           // Keep track on progress for sanity check

    FStepCount      : Cardinal;
    FExecActive     : boolean;

    FExecTickTime   : QWord;

    Function LoadProcOpti ( ALib : TLibHandle; Const AProcName : string ) : pointer;
    Function LoadStructOpti ( ALib : TLibHandle; Const AStructName : string ) : pointer;
    Function LoadVarOpti ( ALib : TLibHandle; Const AVarName : string ) : pointer;
    Procedure ViewAny ( Const AMessage : string );
    Procedure ViewAny ( Const AMessage : string; Const AProcName : string );
    Procedure TraceExtLibCrash ( Const AModelLibName : string; AException : Exception );
    Function ModCheck : boolean;

    Procedure ScenReportEnd;

    Function ExecLoadLib : boolean;
    Procedure ExecUnloadLib;

    Procedure ClearFiles;
    Procedure AppendFile ( AFilename : string );

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Clear;
    Function ExecInit ( Const APrjPath : string; Const AModelFiles : string; AOnViewAny : TOnViewAny; ASwVersion : Cardinal ) : boolean;
    Procedure ExecDone;
    Function ExecStep ( Var ADataSA : TDataSA ) : Double;

    property Iteration : Integer read FIteration;
    property ExecActive : boolean read FExecActive;

    property MdVersion : Cardinal read FMdVersion;
  end;

implementation

Uses
  ConComL, ConComS, FileUtil;

{*** TMsModel ***}

Constructor TMsModel.Create;
Begin
 Inherited;
End;

Destructor TMsModel.Destroy;
Begin
 ExecDone;
 Inherited;
End;

Procedure TMsModel.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TMsModel.ViewAny ( Const AMessage : string; Const AProcName : string );
Begin
 ViewAny(AMessage+'[R:TMsModel.'+AProcName+']');
End;

Procedure TMsModel.TraceExtLibCrash ( Const AModelLibName : string; AException : Exception );
Var
  BReport   : string;
  //BFrames   : PPointer;
  //BFrameIdx : Integer;
Begin
 BReport:='';
 repeat
 if AException<>nil then BReport:=AException.Message;
 ViewAny('MeError in external library "'+AModelLibName+'": crash | Message: "'+BReport+'"','TraceExtLibCrash');
{ if AException=nil then break;
 BReport:=BackTraceStrFunc(ExceptAddr);
 ViewAny('Me * '+BReport,'TraceExtLibCrash');
 BFrames:=ExceptFrames;
 for BFrameIdx:=0 to ExceptFrameCount-1 do
  begin
  BReport:=BackTraceStrFunc(BFrames[BFrameIdx]);
  ViewAny('Me * '+BReport,'TraceExtLibCrash');
  end; }
 until TRUE;
End;

Function TMsModel.LoadProcOpti ( ALib : TLibHandle; Const AProcName : string ) : pointer;
Begin
 Result:=DynLibs.GetProcedureAddress(ALib,AProcName);
 if Result=nil then ViewAny('eCannot find function "'+AProcName+'"');
End;

Function TMsModel.LoadStructOpti ( ALib : TLibHandle; Const AStructName : string ) : pointer;
Begin
 Result:=DynLibs.GetProcedureAddress(ALib,AStructName);
 if Result=nil then ViewAny('eCannot find Structure "'+AStructName+'"');
End;

Function TMsModel.LoadVarOpti ( ALib : TLibHandle; Const AVarName : string ) : pointer;
Begin
 Result:=DynLibs.GetProcedureAddress(ALib,AVarName);
 if Result=nil then ViewAny('eCannot find Variable "'+AVarName+'"');
End;

Function TMsModel.ModCheck : boolean;
Begin
 Result:=FALSE; FVersStrC:=''; FVersStrV:='';
 repeat
 if (FModInit=nil) or (FModStep=nil) or (FModDone=nil) or (FModVersion=nil){ or (FModGetLastError=nil)} then
  begin
  ViewAny('eSome of model mandatory procedures/variables/structures are not specified [R:TMsModel.ModelCheck]');
  break;
  end;
 FVersCode:=FModVersion();
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsModel.ScenReportEnd;
Var
  BTimeA    : TDateTime;
  BTimeB    : TDateTime;
Begin
 ViewAny('p1 Model');
 ViewAny('p0 ');
 BTimeA:=now;
 BTimeB:=FExecTickTime/(24*60*60*1000);
 ViewAny('MiModel execution Ended ('+FormatDateTime('YYYY.MM.DD_HH:NN:SS_zzz',BTimeA)+'); Processing time = '+FormatDateTime('HH:NN:SS_zzz',BTimeA-FModelExecStart)+'; Model time = '+FormatDateTime('HH:NN:SS_zzz',BTimeB));

 ViewAny('iModel scenario finished [R:TMsModel.ScenTerminate]');
End;

Procedure TMsModel.ClearFiles;
Begin
 FModLibName:='';
End;

Procedure TMsModel.AppendFile ( AFilename : string );
Var
  BPath,
  BName,
  BExt      : string;
  BExtS     : string;
Begin
 SplitFilename(AFilename,BPath,BName,BExt);
 repeat
 BExtS:=LowerCase(BExt);
 if (BExtS='so') or (BExtS='dll') then
  begin
  if FModLibName<>'' then begin ViewAny('wDuplicate model library '+AFilename+' [R:TMsModel.AppendFile]'); break; end;
  FModLibName:=AbsFilename(FPrjPath,AFilename);
  break;
  end;
 ViewAny('wInvalid model file extension. Not clear what to do with this file '+AFilename+' [R:TMsModel.AppendFile]');
 until TRUE;
End;

Procedure TMsModel.ExecUnloadLib;
Begin
 FModInit:=nil; FModStep:=nil; FModDone:=nil;
 if FModLib<>NilHandle then begin UnloadLibrary(FModLib); FModLib:=NilHandle; end;
End;

Function TMsModel.ExecLoadLib : boolean;
Var
  BParams   : string;
  BLibName  : string;
Begin
 Result:=FALSE;
 FDateTimeS:='';

 repeat
 BParams:=FModLibName;
 BLibName:=ReadParamStr(BParams);
 if FileExists(BLibName)=FALSE then begin ViewAny('eLibrary file "'+BLibName+'" is not found'); break; end;
 FDateTimeS:=FormatDateTime('YYYY.DD.MM_HH:NN:SS',FileDateToDateTime(FileAge(BLibName)));
 FModLib:=SafeLoadLibrary(BLibName);
 if FModLib=NilHandle then
  begin
  ViewAny('eCannot load library "'+BLibName+'" (Reported error code: "'+GetLoadErrorStr+'")');
  break;
  end;

 FModInit:=TModInit(LoadProcOpti(FModLib,'ModInit'));
 FModStep:=TModStep(LoadProcOpti(FModLib,'ModStep'));
 FModDone:=TModDone(LoadProcOpti(FModLib,'ModDone'));
 FModVersion:=TModVersion(LoadProcOpti(FModLib,'ModVersion'));
 //FModGetLastError:=TModGetLastError(LoadProcOpti(FModLib,'ModGetLastError'));

 if ModCheck=FALSE then break;

 Result:=TRUE;
 until TRUE;
End;

Procedure TMsModel.Clear;
Begin
 FModLibName:='';
End;

Procedure VerboseVersion ( AVersCode : QWord; Out AVersCodeC, AVersCodeV : string );
Begin
 AVersCodeC:='Mlx'+
             StrPadResL(IntToStr((AVersCode shr 56) and $FF),'0',2)+
             StrPadResL(IntToStr((AVersCode shr 40) and $FFFF),'0',2)+
             Chr(Ord('A')+((AVersCode shr 36) and $F))+
             Chr(Ord('A')+((AVersCode shr 32) and $F));
 AVersCodeV:=IntToStr((AVersCode shr 24) and $FF)+'.'+
             IntToStr((AVersCode shr 16) and $FF)+'.'+
             IntToStr((AVersCode shr  8) and $FF)+'.'+
             IntToStr((AVersCode shr  0) and $FF);
End;

Function TMsModel.ExecInit ( Const APrjPath : string; Const AModelFiles : string; AOnViewAny : TOnViewAny; ASwVersion : Cardinal ) : boolean;
Var
  BVersCode     : QWord;
  BModelFiles,
  BFilename     : string;
Begin
 Result:=FALSE;
 FPrjPath:=APrjPath;
 FOnViewAny:=AOnViewAny;

 FStepCount:=0;

 repeat
 ExecDone;

 ClearFiles;
 BModelFiles:=AModelFiles;
 repeat
 BFilename:=ReadParamStr(BModelFiles);
 if BFilename='' then break;
 AppendFile(BFilename);
 until FALSE;

 FVersStrC:=''; FVersStrV:='';
 FMdVersion:=0;

 if FModLibName='' then begin Result:=TRUE; break; end;
 if ExecLoadLib=FALSE then begin ExecUnloadLib; ViewAny('eError loading library (see errors above)','ExecStart'); break; end;

 try
   if FModInit(PChar(FPrjPath),PChar(FModLibName))<>0 then
    begin
    //ViewAny('MeModel error: '+FModGetLastError(),'ExecStart');
    break;
    end;
 except
   ViewAny('MeError in external library "'+FModLibName+'": crash in mod_init','ExecStart');
   break;
 end;
 BVersCode:=FVersCode;
 if BVersCode=0 then begin ViewAny('eModel version code is not specified','ExecStart'); break; end;
 VerboseVersion(BVersCode,FVersStrC,FVersStrV);
 ViewAny('MiModel init. Project: '+FVersStrC+' | Version: '+FVersStrV+' | Date: '+FDateTimeS,'ExecStart');
 FMdVersion:=BVersCode and $FFFFFFFF;
 if ((BVersCode shr 16) and $FFFF)<>((ASwVersion shr 16) and $FFFF) then ViewAny('MwModel version incompatibility','ExecStart');

 FLastProgress:=0;
 FExecTickTime:=0;

 FExecActive:=TRUE; FEndReached:=FALSE;
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsModel.ExecDone;
Begin
 repeat
 if FExecActive=FALSE then break;

 ScenReportEnd;

 if FModDone<>nil then
  begin
  try
    FModDone();
  except
    ViewAny('MeError in external library "'+FModLibName+'": mod_done','ExecStop');
  end;
  ViewAny('MiModel done','ExecStop');
  end;
 until TRUE;

 ExecUnloadLib; FExecActive:=FALSE;
 FIteration:=0; FLogTime:=0;
End;

Function RdParamFloatB ( Var ADataS : string ) : Double;
Var
  BParamH   : Cardinal;
  BParamF   : Single absolute BParamH;
Begin
 Result:=0;
 repeat
 if ADataS='' then break;
 BParamH:=Ord(ADataS[1]); Delete(ADataS,1,1);
 Result:=BParamF;
 until TRUE;
End;

Function RdParamFloatW ( Var ADataS : string ) : Double;
Var
  BParamH   : Cardinal;
  BParamF   : Single absolute BParamH;
Begin
 Result:=0;
 repeat
 if ADataS='' then break;
 BParamH:=Ord(ADataS[1]); Delete(ADataS,1,1);
 if ADataS='' then break;
 BParamH:=BParamH+(Word(ADataS[1]) shl 8); Delete(ADataS,1,1);
 Result:=BParamF;
 until TRUE;
End;

Function RdParamFloatD ( Var ADataS : string ) : Double;
Var
  BParamH   : Cardinal;
  BParamF   : Single absolute BParamH;
Begin
 Result:=0;
 repeat
 if ADataS='' then break;
 BParamH:=Ord(ADataS[1]); Delete(ADataS,1,1);
 if ADataS='' then break;
 BParamH:=BParamH+(Cardinal(ADataS[1]) shl  8); Delete(ADataS,1,1);
 if ADataS='' then break;
 BParamH:=BParamH+(Cardinal(ADataS[1]) shl 16); Delete(ADataS,1,1);
 if ADataS='' then break;
 BParamH:=BParamH+(Cardinal(ADataS[1]) shl 24); Delete(ADataS,1,1);
 Result:=BParamF;
 until TRUE;
End;

Function RdParamDataX ( Var ADataS : string; ASize : byte ) : Cardinal;
Var
  BByteIdx  : byte;
Begin
 Result:=0;
 repeat
 if Length(ADataS)<ASize then begin ADataS:=''; break; end;
 BByteIdx:=0;
 while BByteIdx<ASize do
  begin
  Result:=(Result shl 8)+Ord(ADataS[ASize-BByteIdx]);
  inc(BByteIdx);
  end;
 Delete(ADataS,1,ASize);
 until TRUE;
End;

Procedure WrParamFloatD ( Var ADataS : string; AData : Double );
Var
  BWrIdx    : Integer;
  BData     : Integer;
Begin
 BData:=Round(AData);
 BWrIdx:=Length(ADataS); SetLength(ADataS,BWrIdx+4);
 ADataS[1+BWrIdx]:=Chr(BData); Inc(BWrIdx); BData:=BData shr 8;
 ADataS[1+BWrIdx]:=Chr(BData); Inc(BWrIdx); BData:=BData shr 8;
 ADataS[1+BWrIdx]:=Chr(BData); Inc(BWrIdx); BData:=BData shr 8;
 ADataS[1+BWrIdx]:=Chr(BData);
End;

Function TMsModel.ExecStep ( Var ADataSA : TDataSA ) : Double;
Var
  BProgress     : Double;
  BDummyS       : string;
  BTickPrev,
  BTickThis     : QWord;
  BResultA      : boolean;
Begin
 Result:=1.0;
 BTickPrev:=GetTickCount64;
 repeat                      // BDummyS = 'E8030000 09000000 0000803F 0000803F 0000A041 00000000000000000000000000000000000000000000'
 if FExecActive=FALSE then break;
 if FIteration=0 then
  begin
  FModelExecStart:=now;
  BDummyS:=FormatDateTime('YYYY.MM.DD_HH:NN:SS_zzz',FModelExecStart);
  ViewAny('MiModel execution started ('+BDummyS+')');
  end;
 BResultA:=FALSE;
 try
   BProgress:=FModStep(@ADataSA[0]);
   BResultA:=TRUE;
 except
   on E: Exception do TraceExtLibCrash(FModLibName,E);
 end;
 if BResultA=FALSE then begin FEndReached:=TRUE; break; end;

 if (BProgress<FLastProgress) or ((BProgress<>0) and (BProgress=FLastProgress)) then // Sanity check
  begin
  ViewAny('MeSanity check error: Progress did not increase [R:TMsModel.ExecCmd]');
  FEndReached:=TRUE;
  end;
 if BProgress>=1.0 then begin BProgress:=1; FEndReached:=TRUE; ViewAny('p'+FloatToStr(BProgress)+' D0D000 Model'); end
 else if (FIteration and $1F)=0 then ViewAny('p'+FloatToStr(BProgress)+' D0D000 Model');

 Result:=BProgress;

 inc(FIteration);
 until TRUE;

 BTickThis:=GetTickCount64;
 FExecTickTime:=FExecTickTime+(BTickThis-BTickPrev);
End;

end.

