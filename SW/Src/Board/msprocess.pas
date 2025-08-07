unit MsProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComPort, AsmTypes_sd, BuildBase_sd, BuildCpuX_sd, ProcModel_sd,
  MemSeg_sd, MemSegHelper_sd, DasmBase_sd, DasmMs_sd, DasmRV_sd, DasmWA_sd, DataFiles,
  MsModel;

Type
  TCommState = (csNone, csClosed, csConnecting, csConnected, csActive, csSimulator);
  TPlayer = (plNone, plFpga, plIss, plComb);

  TFlashSize = (fsUnknown, fs64M, fs128M, fs256M, fs512M, fs01G, fs02G);

  TMsProcess = class(TThread)
  private
    FSwVersion      : Cardinal;
    FComStream      : TComPort;
    FIsEnded        : boolean;
    FCmdList,
    FCmdListA       : TStringList;
    FCmdListLock    : TRtlCriticalSection;
    FCmdWait        : boolean;
    FProcAnyEvt     : PRtlEvent;
    FPlayerParams   : string;
    FPlayer         : TPlayer;
    FSetPlayer      : boolean;
    FCommState      : TCommState;
    FWait           : Cardinal;
    FAttReq         : boolean;
    FHsRepeatToFail : Integer;

    FBuild          : TBuildBase;
    FPrjFullName,
    FDstPath,
    FModelLibs,
    FCoreListPrj,
    FCoreListHW,
    FSegListS,
    FDefSegNames,
    FExtCompiler,
    FLinkOptions,
    FSrcList,
    FIncList,
    FLocList        : string;
    FPrjPath        : string;

    FMsModel        : TMsModel;
    FModelDataSend  : string;
    FModelTerminate : boolean;

    FIoSpace        : Cardinal;
    FTermBase       : Cardinal;

    FMcuType        : byte;
    FBrdVers        : byte;

    FCpuWarn        : boolean;
    FGetCpuInfo     : boolean;
    FForceActions   : boolean;
    FDebugActions   : string;

    FMcxActive      : boolean; // i.e. MCU is in execution and registers cannot be displayed
    FMcxPrev,
    FMcxThis        : string;

    FCpuCtrl        : byte;

    FBreakList      : TBreakList;
    FTtyRecv        : string;
    FModelCommWrS,
    FModelCommRdS   : string;

    FOnViewAny      : TOnViewAny;
    FProcModel      : TProcModel;
    FExecLog        : TStringList;
    FExecLogPos     : Integer;
    FExecLogIsIn    : boolean; // TRUE if inside exec log

    FUtestList      : TStringList;
    FUtestStartTime : TDateTime;
    FBatchMode      : boolean;
    FBatchCount,
    FBatchFail      : Integer;

    FFlashLog       : TStringList;
    FFlashLogKeep   : boolean;
    FFlashSize      : TFlashSize;
    FFlashAddrLen   : byte;
    FFlashCmdEr,
    FFlashCmdRd,
    FFlashCmdWr     : byte;

    Procedure ViewAny ( Const AMessage : string );
    Procedure CloseAll;
    Procedure OpenAny;
    Function FormatCommError ( Const AError : string ) : string;
    Function FormatCommState : string;
    Procedure SetCommState ( ANewState : TCommState );

    //Function CanReadAny : boolean;
    Procedure ClearRecvBuf;
    Function RecvByteAny ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
    Function RecvByteAny ( Out AData : byte ) : boolean;
    Procedure SendByteAny ( AData : byte );
    Procedure SendStringAny ( Const AData : string );
    Function RecvByteA ( ASenseAtt : boolean; Out AData : byte ) : boolean;
    Function RecvByteA ( ASenseAtt : boolean; Out AData : byte; ATimeOut : Cardinal ) : boolean;
    Function PollModel ( Out AIsModelRun : boolean ) : boolean;

    Function SendS ( AAddr : word; Const ADataS : string ) : boolean;
    Function RecvS ( AAddr : word; ASize : word ) : string;
    Function SendRecvS ( AAddr : word; Var ADataS : string ) : boolean;
    //Function SendRecvS2 ( AAddr : word; Var ADataSA, ADataSB : string ) : boolean;
    Procedure SendDataOpti ( AAddr : word; Const ADataS : string );
    Function RecvDataOpti ( Var ADataS : string ) : boolean;

    Procedure RecvModel;

    Function SendB ( AAddr : word; AData : byte ) : boolean;
    Function SendW ( AAddr : word; AData : word ) : boolean;
    Function SendD ( AAddr : word; AData : Cardinal ) : boolean;

    Function RecvB ( AAddr : word; Out AData : byte ) : boolean;
    Function RecvW ( AAddr : word; Out AData : word ) : boolean;
    Function RecvD ( AAddr : word; Out AData : Cardinal ) : boolean;
    Function RecvQ ( AAddr : word; Out AData : QWord ) : boolean;

    Function SendRecvSpi ( Var ADataBin : string ) : boolean;

    Procedure ProcessAtt;
    Procedure ProcessTty ( Const ATtyRecv : string );
    Procedure ModelProcessTerm ( AData : Char );
    Procedure ModelCommWr ( AData : byte );
    Procedure ModelCommRd ( Out AData : byte );
    //Procedure ProbaA ( AData : byte );

    Procedure CpuIdWarn ( Const AMessage : string );
    Function GetCpuId : boolean;
    Function GetBrdVersion : boolean;
    Function WrMemSeg ( AMemSeg : TMemSeg; AShowProgr : boolean ) : boolean;
    Function RdMemSeg ( AMemSeg : TMemSeg; AShowProgr : boolean ) : boolean;
    Function LoadMem : boolean;

    Procedure IssExec;
    Procedure CombExec;

    Procedure RdRegsOpti;
    Procedure SetBreakList ( Const ABreakList : string );
    Procedure ProcessCmdList;
    Procedure ProcessDebugActions ( Const AActions : string );
    Procedure ProcessCmdExt ( Const AReadS : string );

    Function BatchExec ( Const AConfigParams : string ) : boolean;
    Procedure BatchCheck ( ASuccessful : boolean );
    Procedure ReportBatchStart;
    Procedure ParseConfigParams ( Const AConfigParams : string; Out ADebugActions, AUtestDir : string );
    Function ProcessBuild ( Const ADebugActions : string ) : boolean;
    Procedure RegChange;
    Procedure InvalidateMcx;
    Function RegBinToHex ( Const ASrc : string ) : string;
    Function RecvMcxStateA ( Const ARegsBin : string ) : string;
    Procedure RecvMcxState ( Const ARegsBin : string );

    Function GetCpuStat ( Out AStat : QWord ) : boolean;
    Function SetCpuCtrl ( ACtrl : byte ) : boolean;
    Function MemOperEn : boolean;
    Function MemOperDis : boolean;

    Function RdRegsAll : string;
    Function DutRdRegs : string;
    Procedure DutReset;
    Procedure DutFirstStep;
    Procedure DutStart;
    Procedure DutStop;
    Procedure DutSetBreakList;
    Function DutStepCombi ( Out ARegs : string ) : boolean;
    Procedure DutStepBack;
    Procedure DutStepInto;
    Procedure DutStepInto ( Const ADataS : string );
    Procedure DutStepOver ( Const ADataS : string );
    Procedure DutRunTo ( AAddr : Cardinal );
    Procedure DutRunTo ( Const AAddrS : string );

    Function CallOrJmp ( AAddr : Cardinal; ACpuType : char ) : TCallOrJmp;

    Function FpgaResetA ( ASrc : byte ) : boolean;
    Procedure ReadSegData ( Const ASegParams : string );
    Procedure ReadStack ( Const AParams : string );
    Function GetFlashSize ( AFlashSizeCode : byte ) : boolean;
    Function FlashAddrToStr ( AAddr : Cardinal ) : string;
    Function ProbeFlash : boolean;
    Procedure FpgaFlashOp ( Const ASectList : string );
    Procedure FpgaReset;

    Function ReadBinFile ( Const AFilename : string; Out ADataBin : string ) : boolean;
    Function ReadHexFile ( Const AFilename : string; Out ADataBin : string ) : boolean;
    Function TryReadEfinixHex ( Const AFilename : string; Out ADataBin : string ) : Integer;

    Function EraseFlashBlockGen ( AAddr : Cardinal ) : boolean;
    Function ReadFlashFlagGen : boolean;
    Function WriteFlashPageGen ( AAddr : Cardinal; Const APageData : string ) : boolean;
    Function ReadFlashGen ( AAddr : Cardinal; ASize : Cardinal; Out ADataS : string ) : boolean;
    //Procedure ReadFlashGenS ( AAddr : Cardinal; ASize : Cardinal );
    //Function ReadFlashGenR ( ASize : Cardinal; Out ADataS : string ) : boolean;
    Function FpgaReflashSect ( Const ASectInfo : string; Var AIsExt : boolean ) : boolean;
    Function FpgaReadSect ( Const ASectInfo : string ) : boolean;
    Function FpgaWriteReg ( AAddr : Cardinal; Const ADataS : string ) : boolean;

    Function DasmMissing ( AIpPrev, AIpThis : Cardinal ) : boolean;

    Procedure FlashLogS ( Const ADataS : string );
    Procedure FlashLogR ( Const ADataS : string );
  protected
    Procedure Execute; Override;
  public
    Constructor Create ( CreateSuspended : boolean; AVersion : Cardinal ); Virtual;
    Destructor Destroy; Override;

    Procedure AppendCmd ( Const ACmdS : string );
    Function HasPendingCmd : boolean;
    Function IpToLine ( AAddr : Cardinal ) : Integer;

    Function TextDbg : TStringList;

    property OnViewAny : TOnViewAny read FOnViewAny write FOnViewAny;
    property IsEnded : boolean read FIsEnded;
    property ProcModel : TProcModel read FProcModel;
    property MsModel : TMsModel read FMsModel;
    property FlashLog : TStringList read FFlashLog;
  end;

Const
  CCommStateS   : array [TCommState] of string = ('None', 'Closed', 'Connecting', 'Connected', 'Active', 'Simulator');

Const
  CDbgCpuCtrlWr   = $000;
  CDbgCpuStepWr   = $001;
  CDbgMemDataWr   = $002;
  CDbgMemAddrWr   = $003;
  CDbgBrkListWr   = $005;
  CDbgBrkThisWr   = $006;
  CDbgStartupWr   = $007;

  CDbgCpuStatRd   = $000;
  CDbgCpuInfoRd   = $001;
  CDbgMemDataRd   = $002;
  CDbgMemAddrRd   = $003;
  CDbgCpuRegsRd   = $004;
  CDbgTtyDataRd   = $007;

Function BuildParamsToStr ( Const APrjFilename : string; AParams : TStringList; Out APlayerS : string ) : string;

implementation

Uses
  ConComL, ConComS;

// *** AUX ***
Procedure AddSizeChsOpti ( Var ADataS : string; ASize : Cardinal; AChs : word );
Var
  BIndex    : Integer;
  BData     : Cardinal;
Begin
 BIndex:=Length(ADataS)-8;
 BData:=ASize;
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 BData:=0;
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 BData:=AChs;
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
 ADataS[1+BIndex]:=Chr(BData and $FF); BData:=BData shr 8; inc(BIndex);
End;

Const
  CVerifBlockLen    = 256;

Procedure InvertBytes ( Var ADataS : string );
Var
  BDataIdx,
  BDataLen  : Integer;
  BDataRd,
  BDataWr   : byte;
  BBitIdx   : Integer;
Begin
 BDataLen:=Length(ADataS);
 BDataIdx:=0;
 while BDataIdx<BDataLen do
  begin
  BDataRd:=Ord(ADataS[1+BDataIdx]);
  BDataWr:=0;
  for BBitIdx:=0 to 7 do
   begin
   BDataWr:=BDataWr shl 1;
   if (BDataRd and $01)<>0 then BDataWr:=BDataWr or $01;
   BDataRd:=BDataRd shr 1;
   end;
  ADataS[1+BDataIdx]:=Chr(BDataWr);
  inc(BDataIdx);
  end;
End;

Procedure CropJicFile ( Var ADataBin : string );
Var
  BPos,
  BIndex    : Integer;
Begin
 repeat
 BPos:=Pos(#$FF+#$FF+#$FF+#$FF+#$FF+#$FF+#$FF,ADataBin);
 if BPos<>0 then Delete(ADataBin,1,BPos-1);
 if ADataBin='' then break;
 BIndex:=PosBack(#$FF+#$FF+#$FF+#$FF+#$FF+#$FF+#$FF,ADataBin)-1;
 while BIndex>0 do
  begin
  if ADataBin[1+BIndex]<>#$FF then break;
  dec(BIndex);
  end;
 if BIndex=0 then break;
 BIndex:=BIndex+256;
 if BIndex>=Length(ADataBin) then break;
 Delete(ADataBin,1+BIndex,Length(ADataBin)-BIndex);
 until TRUE;
End;

Function AllDataIsEmpty ( Const ADataS : string ) : boolean;
Var
  BIndex    : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(ADataS) do
  begin
  if Ord(ADataS[1+BIndex])<>$FF then break;
  inc(BIndex);
  end;
 Result:=BIndex=Length(ADataS);
End;

// TMsProcess

Constructor TMsProcess.Create ( CreateSuspended : boolean; AVersion : Cardinal );
Var
  BData     : byte;
Begin
 Inherited Create(CreateSuspended);
 FSwVersion:=AVersion;
 InitCriticalSection(FCmdListLock);
 FProcAnyEvt:=RtlEventCreate;
 FCmdList:=TStringList.Create;
 FCmdListA:=TStringList.Create;
 FUtestList:=TStringList.Create;
 FProcModel:=TProcModel.Create; FProcModel.OnViewAny:=@ViewAny;
 FBuild:=TBuildCpuX.Create; FBuild.OnAppendError:=@ViewAny;
 FExecLog:=TStringList.Create; FExecLogIsIn:=FALSE;
 FFlashLog:=TStringList.Create;
 FIsEnded:=FALSE;
 FWait:=1000;
 FMsModel:=TMsModel.Create;
 BData:=10;
 //if Self=nil then ProbaA(BData); // Very strange bug, coming perhaps from the Intel CPU
End;

Destructor TMsProcess.Destroy;
Begin
 FOnViewAny:=nil;
 FMsModel.Free;
 CloseAll;
 FFlashLog.Free;
 FExecLog.Free;
 FBuild.Free;
 FProcModel.Free;
 FUtestList.Free;
 FCmdListA.Free;
 FCmdList.Free;
 RtlEventDestroy(FProcAnyEvt);
 DoneCriticalSection(FCmdListLock);
 Inherited;
End;

Procedure TMsProcess.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Function TMsProcess.IpToLine ( AAddr : Cardinal ) : Integer;
Var
  BSeg  : TMemSeg;
Begin
 BSeg:=MemSegSearch(FBuild.MemSegList,AAddr);
 if BSeg<>nil then Result:=BSeg.IpToLine[AAddr-BSeg.HwBase] else Result:=-1;
End;

Procedure TMsProcess.CloseAll;
Begin
 if FComStream<>nil then
  begin
  FComStream.Close;
  FComStream.Free;
  FComStream:=nil;
  end;
 SetCommState(csClosed);
End;

Procedure TMsProcess.AppendCmd ( Const ACmdS : string );
Begin
 EnterCriticalSection(FCmdListLock);
 FCmdList.Append(ACmdS); FCmdWait:=TRUE;
 LeaveCriticalSection(FCmdListLock);
 RtlEventSetEvent(FProcAnyEvt);
End;

Function TMsProcess.HasPendingCmd : boolean;
Begin
 Result:=FALSE;
 EnterCriticalSection(FCmdListLock);
 if FCmdList.Count<>0 then Result:=TRUE;
 if FCmdListA.Count<>0 then Result:=TRUE;
 LeaveCriticalSection(FCmdListLock);
End;

Function TMsProcess.BatchExec ( Const AConfigParams : string ) : boolean;
Var
  BDebugActions,
  BUtestDirList,
  BUtestDir     : string;
Begin
 Result:=FALSE;
 ParseConfigParams(AConfigParams,BDebugActions,BUtestDirList);
 FPrjPath:=ExtractFilePath(FPrjFullName);
 repeat
 if BUtestDirList='' then begin Result:=ProcessBuild(BDebugActions); break; end;
 FUtestList.Clear;
 repeat
 BUtestDir:=ReadParamStr(BUtestDirList);
 if BUtestDir='' then break;
 FileList(AbsFilename(FPrjPath,BUtestDir),'hex asm i elf.hex elf.i',FUtestList);
 until FALSE;
 FUtestStartTime:=Now;
 ViewAny('jUtest File List created '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',FUtestStartTime)+'. There are '+IntToStr(FUtestList.Count)+' files in total.');
 if FUtestList.Count=0 then break;
 FBatchMode:=TRUE; FBatchCount:=FUtestList.Count; FBatchFail:=0;
 FSrcList:=FUtestList.Strings[0]; FUtestList.Delete(0); DelFirstLastSpace(FSrcList);
 ReportBatchStart;
 Result:=ProcessBuild('RIMRnib.S');
 until TRUE;
End;

Procedure TMsProcess.BatchCheck ( ASuccessful : boolean );
Var
  BTestName     : string;
  BNow          : TDateTime;
Begin
 repeat
 if FBatchMode=FALSE then break;
 BTestName:=FSrcList; DelFirstLastSpace(BTestName);
 if ASuccessful then ViewAny('JTest ['+BTestName+']: PASS')
 else begin ViewAny('KTest ['+BTestName+']: FAIL'); inc(FBatchFail); end;
 if FUtestList.Count=0 then
  begin
  FBatchMode:=FALSE;
  BNow:=Now;
  ViewAny('jTest sequence finished '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',BNow)+'. Total Files: '+IntToStr(FBatchCount)+'. Failed Files: '+IntToStr(FBatchFail)+'. Execution time: '+FormatDateTime('HH:NN.ss',BNow-FUtestStartTime));
  break;
  end;
 AppendCmd('S');
 until TRUE;
End;

Procedure TMsProcess.ReportBatchStart;
Var
  BFileIdx  : Integer;
Begin
 BFileIdx:=FBatchCount-FUtestList.Count;
 ViewAny('kTest ['+FSrcList+'] started (File '+IntToStr(BFileIdx)+' out of '+IntToStr(FBatchCount)+')');
End;

Function BuildParamsToStr ( Const APrjFilename : string; AParams : TStringList; Out APlayerS : string ) : string;
Var
  BLineIdx      : Integer;
  BReadS,
  BParamS       : string;
  BCpuF, BMemF,
  BRvcF, BLnkF,
  BSrcF, BIncF,
  BLocF, BTstF,
  BOutF, BModF  : string;
  BSegName      : string;
  BSegFlags,
  BHwBase,
  BHwSize,
  BHwWidth      : Integer;
Begin
 APlayerS:='';
 BCpuF:=''; BMemF:=''; BRvcF:=''; BLnkF:=''; BSrcF:=''; BIncF:=''; BLocF:=''; BTstF:=''; BOutF:=''; BModF:='';

 BLineIdx:=0;
 while BLineIdx<AParams.Count do
  begin
  BReadS:=AParams.Strings[BLineIdx];
  BParamS:=LowerCase(ReadParamStr(BReadS));
  if BParamS='player' then APlayerS:=BReadS
  else if BParamS='cpuf' then BCpuF:=Trim(BReadS)
  else if BParamS='memf' then
   begin
   BSegName:=ReadParamStr(BReadS);
   BSegFlags:=Hex32ToInt(ReadParamStr(BReadS));
   BHwBase:=Hex32ToInt(ReadParamStr(BReadS));
   BHwSize:=Hex32ToInt(ReadParamStr(BReadS));
   BHwWidth:=Hex32ToInt(ReadParamStr(BReadS));
   BMemF:=BMemF+MemSegParamsToStr(BSegName,BSegFlags,BHwBase,BHwSize,BHwWidth);
   end
  else if BParamS='rvcf' then BRvcF:=Trim(BReadS)
  else if BParamS='lnkf' then BLnkF:=BLnkF+Trim(BReadS)+' '
  else if BParamS='srcf' then BSrcF:=BSrcF+Trim(BReadS)+' '
  else if BParamS='incf' then BIncF:=BIncF+Trim(BReadS)+' '
  else if BParamS='locf' then BLocF:=BLocF+Trim(BReadS)+' '
  else if BParamS='tstf' then BTstF:=BTstF+Trim(BReadS)+' '
  else if BParamS='outf' then BOutF:=BOutF+Trim(BReadS)+' '
  else if BParamS='modf' then BModF:=BModF+Trim(BReadS)+' ';
  inc(BLineIdx);
  end;
 Result:=APrjFilename+#13+BCpuF+#13+BMemF+#13+'code data'+#13+'1000 FE'+#13+BRvcF+#13+BLnkF+#13+BSrcF+#13+BIncF+#13+BLocF+#13+BTstF+#13+BOutF+#13+BModF;
End;

Procedure TMsProcess.ParseConfigParams ( Const AConfigParams : string; Out ADebugActions, AUtestDir : string );
Var
  BConfigParams : string;
  BDummyS       : string;
Begin
 BConfigParams:=AConfigParams;
 ADebugActions:=ReadTillC(BConfigParams,#13);
 FPrjFullName:=ReadTillC(BConfigParams,#13);   DelFirstLastSpace(FPrjFullName);
 FCoreListPrj:=ReadTillC(BConfigParams,#13);   DelFirstLastSpace(FCoreListPrj);
 FSegListS:=ReadTillC(BConfigParams,#13);      DelFirstLastSpace(FSegListS);
 FDefSegNames:=ReadTillC(BConfigParams,#13);   DelFirstLastSpace(FDefSegNames);
 BDummyS:=ReadTillC(BConfigParams,#13);        DelFirstLastSpace(BDummyS);        HexToDwordCheck(ReadParamStr(BDummyS),FIoSpace); HexToDwordCheck(ReadParamStr(BDummyS),FTermBase);
 FExtCompiler:=ReadTillC(BConfigParams,#13);   DelFirstLastSpace(FExtCompiler);
 FLinkOptions:=ReadTillC(BConfigParams,#13);   DelFirstLastSpace(FLinkOptions);
 FSrcList:=ReadTillC(BConfigParams,#13);       DelFirstLastSpace(FSrcList);
 FIncList:=ReadTillC(BConfigParams,#13);       DelFirstLastSpace(FIncList);
 FLocList:=ReadTillC(BConfigParams,#13);       DelFirstLastSpace(FLocList);
 AUtestDir:=ReadTillC(BConfigParams,#13);      DelFirstLastSpace(AUtestDir);
 FDstPath:=ReadTillC(BConfigParams,#13);       DelFirstLastSpace(FDstPath);
 FModelLibs:=ReadTillC(BConfigParams,#13);     DelFirstLastSpace(FModelLibs);
End;

Function TMsProcess.ProcessBuild ( Const ADebugActions : string ) : boolean;
Var
  BTimeA,
  BTimeB        : TDateTime;
  BTimeSpent    : Double;
  BTimeSpentS   : string;
  BNameList,
  BNameS        : string;
Begin
 Result:=FALSE;
 FBuild.Clear;
 //FMsModel.ExecDone;

 repeat
 if FSrcList='' then begin ViewAny('eProject list is empty [R:TMsProcess.ProcessBuild]'); break; end;

 FPrjPath:=ExtractFilePath(FPrjFullName);

 BNameList:=FSrcList; repeat BNameS:=ReadParamStr(BNameList); if BNameS='' then break; FBuild.AppendSrcName(AbsFilename(FPrjPath,BNameS)); until FALSE;
 BNameList:=FIncList; repeat BNameS:=ReadParamStr(BNameList); if BNameS='' then break; FBuild.AppendIncPath(AbsFilename(FPrjPath,BNameS)); until FALSE;
 BNameList:=FLocList; repeat BNameS:=ReadParamStr(BNameList); if BNameS='' then break; FBuild.AppendLocPath(AbsFilename(FPrjPath,BNameS)); until FALSE;

 FBuild.InitPrj(FPrjFullName,AbsFilename(FPrjPath,FDstPath),FCoreListPrj,FSegListS,FDefSegNames,FExtCompiler,FLinkOptions);
 ViewAny('iBuild started '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
 if FBuild.DeleteOld=FALSE then break;
 ViewAny('iOld files are deleted '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 BTimeA:=Now;
 if FBuild.Build=FALSE then
  begin
  ViewAny('iBuild stopped due to errors '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
  break;
  end;
 BTimeB:=Now;
 BTimeSpent:=BTimeB-BTimeA;
 //BTimeSpentS:=FormatDateTime('HH:NN.SS,zzz',BTimeSpent);
 BTimeSpentS:=FormatFloat('#0.000',BTimeSpent*86400);
 ViewAny('iBuild time: '+BTimeSpentS+'s');

 ViewAny('iProject compiled successfully '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));
 if FBuild.SaveNew=FALSE then break;

 ViewAny('iBuild files are generated '+FormatDateTime('[DD MMM YYYY, HH:NN.ss]',Now));

 ViewAny('u2');
 ViewAny('d'+FPrjFullName+#13+FCoreListPrj+#13+FBuild.LstName+#13+FBuild.Lst.Text);
 FProcModel.SetParams(FCoreListPrj,FBuild.DefCodeSeg,FBuild.DefDataSeg,FIoSpace,FTermBase,@DasmMissing,@ModelProcessTerm,@ModelCommWr,@ModelCommRd);
 ViewAny('a'+FSegListS);
 {if FCommState=csActive then AppendCmd('c'+BDebugActions)
 else begin FForceActions:=TRUE; FDebugActions:=BDebugActions; end;}
 FForceActions:=TRUE; FDebugActions:=ADebugActions;
 //if FMsModel.ExecInit(FPrjPath,FModelLibs,FOnViewAny,0)=FALSE then break;
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.FormatCommError ( Const AError : string ) : string;
Var
  BLen          : Integer;
  BLastSym      : char;
Begin
 Result:=AError;
 while Result<>'' do
  begin
  BLen:=Length(Result);
  BLastSym:=Result[BLen];
  if BLastSym in [#13, #10, #9, #8] then Delete(Result,BLen,1)
  else break;
  end;
End;

Function TMsProcess.FormatCommState : string;
Var
  BPlayerParams     : string;
Begin
 BPlayerParams:=FPlayerParams;
 Delete(BPlayerParams,1,1);
 if FCommState=csSimulator then Result:=CCommStateS[FCommState]
 else
  begin
  Result:=ReadParamStr(BPlayerParams)+' ['+ReadParamStr(BPlayerParams)+'] '+CCommStateS[FCommState];
  if (FCommState=csActive) and (FPlayer=plComb) then Result:=Result+' (Combi)';
  end;
End;

Procedure TMsProcess.SetCommState ( ANewState : TCommState );
Begin
 if FCommState<>ANewState then
  begin
  FCommState:=ANewState;
  if FCommState<>csClosed then ViewAny('s'+Chr(Byte(ANewState)+Byte('0'))+FormatCommState);
  end;
End;

Procedure TMsProcess.OpenAny;
Var
  BPlayer       : char;
  BPlayerParams : string;
  BResult       : boolean;
  BBaud         : Integer;
Begin
 BResult:=FALSE;
 BPlayerParams:=FPlayerParams;
 if FComStream<>nil then CloseAll;
 repeat
 if BPlayerParams='' then break;
 BPlayer:=BPlayerParams[1]; Delete(BPlayerParams,1,1);
 case BPlayer of
  'f',
  'b': begin
       FComStream:=TComPort.Create;
       SetCommState(csConnecting);
       FComStream.Connect(ReadParamStr(BPlayerParams));
       if FComStream.LastError<>0 then begin ViewAny('se'+FormatCommState+': '+#13+FormatCommError(FComStream.GetErrorDesc)); break; end;
       if TryStrToInt(ReadParamStr(BPlayerParams),BBaud)=FALSE then begin ViewAny('se'+FormatCommState+': '+#13+'Invalid baud rate'); break; end;
       FComStream.Config(BBaud);
       if FComStream.LastError<>0 then begin ViewAny('se'+FormatCommState+': '+#13+FormatCommError(FComStream.GetErrorDesc)); break; end;
       BResult:=TRUE;
       SetCommState(csConnected);
       end;
  'i': begin
       CloseAll;
       SetCommState(csSimulator);
       end;
  end; // case
 until TRUE;
 if (BResult=FALSE) and (FComStream<>nil) then begin Sleep(500); CloseAll; end;
End;

{Function TMsProcess.CanReadAny : boolean;
Begin
 if FComStream<>nil then Result:=FComStream.CanRead(0)
 else Result:=FALSE;
End;

Function TMsProcess.RecvByteAny ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if FComStream=nil then break;
 while ATimeOut>0 do
  begin
  if (FComStream.WaitingData>0) or (FComStream.WaitingDataEx>0) then break;
  Sleep(1);
  Dec(ATimeOut);
  end;
 if ATimeOut=0 then break;
 AData:=FComStream.RecvByte(ATimeOut);
 if FComStream.LastError<>sOK then break;
 Result:=TRUE;
 until TRUE;
End;}

Procedure TMsProcess.ClearRecvBuf;
Var
  BCount    : Integer;
  BData     : byte;
Begin
 BCount:=0;
 while BCount<1000 do
  begin
  if FComStream.RecvByte(BData,0)=FALSE then break;
  inc(BCount);
  end;
End;

Function TMsProcess.RecvByteAny ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if FComStream=nil then break;
 Result:=FComStream.RecvByte(AData,ATimeOut);
 until TRUE;
End;

Function TMsProcess.RecvByteAny ( Out AData : byte ) : boolean;
Begin
 Result:=RecvByteAny(AData,FWait);
End;

Procedure TMsProcess.SendByteAny ( AData : byte );
Begin
 if FComStream<>nil then FComStream.SendData(Chr(AData));
End;

Procedure TMsProcess.SendStringAny ( Const AData : string );
Begin
 if FComStream<>nil then FComStream.SendData(AData);
End;

Function TMsProcess.RecvByteA ( ASenseAtt : boolean; Out AData : byte ) : boolean;
Begin
 Result:=RecvByteA(ASenseAtt,AData,FWait);
End;

Function TMsProcess.RecvByteA ( ASenseAtt : boolean; Out AData : byte; ATimeOut : Cardinal ) : boolean;
Var
  BByte         : byte;
Begin
 Result:=FALSE;
 AData:=0;
 repeat
 if RecvByteAny(BByte,ATimeOut)=FALSE then break;
 if ASenseAtt and (BByte=$AA) then FAttReq:=TRUE
 //else if ASenseAtt and (BByte=$77) then begin RecvModel; break; end // ##Model old
 else if ASenseAtt and (BByte=$78) then RecvModel // ##Model new
 else if ASenseAtt and (BByte=$77) then
  begin
  if RecvByteAny(BByte,ATimeOut)=FALSE then break;
  if BByte<>$78 then break;
  RecvModel;
  end
 else begin AData:=BByte; Result:=TRUE; break; end;
 until FALSE;
End;

Function TMsProcess.PollModel ( Out AIsModelRun : boolean ) : boolean; // Same as RecvByteA but do Break in case of Model request and return False (i.e. no byte received)
Var
  BByte         : byte;
Begin
 Result:=FALSE;
 AIsModelRun:=FALSE;

 repeat
 if RecvByteAny(BByte,10)=FALSE then break;
 if BByte=$AA then FAttReq:=TRUE
 else if BByte=$78 then begin RecvModel; AIsModelRun:=TRUE; break; end
 else if BByte=$77 then
  begin
  if RecvByteAny(BByte,10)=FALSE then break;
  if BByte<>$78 then break;
  RecvModel;
  AIsModelRun:=TRUE;
  break;
  end
 else begin Result:=TRUE; break; end;
 until FALSE;
End;

Function TMsProcess.SendS ( AAddr : word; Const ADataS : string ) : boolean;
Var
  BDataS        : string;
  BLen          : Cardinal;
  BByte         : Byte;
Begin
 Result:=FALSE;
 repeat
 if FComStream=nil then break;
 BLen:=Length(ADataS);
 BDataS:=Chr($C0 or ((AAddr shr 8) and $F))+Chr(AAddr and $FF)+Chr(BLen and $FF)+Chr(BLen shr 8)+ADataS;
 FComStream.SendData(BDataS);
 if RecvByteA(TRUE,BByte)=FALSE then break;
 if BByte<>$00 then break;
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.RecvS ( AAddr : word; ASize : word ) : string;
Var
  BDataS        : string;
  BIndex        : word;
  BByte         : byte;
Begin
 Result:='';
 repeat
 if FComStream=nil then break;
 BDataS:=Chr($80 or ((AAddr shr 8) and $F))+Chr(AAddr and $FF)+Chr(ASize and $FF)+Chr(ASize shr 8);
 FComStream.SendData(BDataS);
 if RecvByteA(TRUE,BByte)=FALSE then break;
 if BByte<>$00 then break;
 SetLength(Result,ASize);
 BIndex:=0;
 while BIndex<ASize do
  begin
  if RecvByteA(FALSE,BByte)=FALSE then break;
  Result[1+BIndex]:=Chr(BByte);
  inc(BIndex);
  end;
 if BIndex<>ASize then Result:='';
 until TRUE;
End;

Function TMsProcess.SendRecvS ( AAddr : word; Var ADataS : string ) : boolean;
Var
  BDataS        : string;
  BIndex,
  BLen          : Cardinal;
  BByte         : Byte;
Begin
 Result:=FALSE;
 repeat
 if FComStream=nil then break;
 BLen:=Length(ADataS);
 BDataS:=Chr($D0 or ((AAddr shr 8) and $F))+Chr(AAddr and $FF)+Chr(BLen and $FF)+Chr(BLen shr 8)+ADataS;
 FComStream.SendData(BDataS);
 BIndex:=0;
 while BIndex<BLen do
  begin
  if RecvByteA(FALSE,BByte)=FALSE then break;
  ADataS[1+BIndex]:=Chr(BByte);
  inc(BIndex);
  end;
 if BIndex<>BLen then begin ViewAny('fe'+IntToStr(BLen)+' bytes expected, '+IntToStr(BIndex)+' bytes received'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.SendDataOpti ( AAddr : word; Const ADataS : string );
Var
  BDataS        : string;
  BLen          : Cardinal;
Begin
 BLen:=Length(ADataS);
 BDataS:=Chr($D0 or ((AAddr shr 8) and $F))+Chr(AAddr and $FF)+Chr(BLen and $FF)+Chr(BLen shr 8)+ADataS;
 FComStream.SendData(BDataS); FlashLogS(BDataS);
End;

Function TMsProcess.RecvDataOpti ( Var ADataS : string ) : boolean;
Var
  BIndex        : Cardinal;
  BByte         : Byte;
Begin
 Result:=FALSE;
 repeat
 BIndex:=0;
 while BIndex<Length(ADataS) do
  begin
  if RecvByteA(FALSE,BByte)=FALSE then break;
  ADataS[1+BIndex]:=Chr(BByte);
  inc(BIndex);
  end;
 if BIndex<>Length(ADataS) then break;
 FlashLogR(ADataS);
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.RecvModel;
Var
  BDataB        : byte;
  BDataQ        : QWord;
  BLenQ,
  BIndexQ,
  BIndexB       : Integer;
  BDataSA       : TDataSA;
  BSendS        : string;
  BResult       : boolean;
  BProgress     : Double;
Begin
 BDataQ:=0;
 repeat
 BResult:=FALSE;
 BIndexQ:=0;
 while BIndexQ<64 do
  begin
  BIndexB:=0;
  while BIndexB<8 do
   begin
   if RecvByteAny(BDataB)=FALSE then break;
   BDataQ:=(BDataQ shr 8) or (QWord(BDataB) shl 56);
   inc(BIndexB);
   end;
  if BIndexB<>8 then break;
  BDataSA[BIndexQ]:=BDataQ;
  if BIndexQ=0 then
  else if BDataQ=0 then begin BResult:=TRUE; break; end;
  inc(BIndexQ);
  end;
 if BResult=FALSE then break;
 BLenQ:=BIndexQ;
 BProgress:=FMsModel.ExecStep(BDataSA);
 BSendS:='';
 BIndexQ:=1; while BIndexQ<BLenQ do begin BSendS:=BSendS+QWordAsStr(BDataSA[BIndexQ]); inc(BIndexQ); end;
 FModelDataSend:=BSendS;
 if BProgress>=1.0 then begin FMsModel.ExecDone; FModelTerminate:=TRUE; end;
 until TRUE;
End;


{Function TMsProcess.SendRecvS2 ( AAddr : word; Var ADataSA, ADataSB : string ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if FComStream=nil then break;

 SendDataOpti(AAddr,ADataSA);
 SendDataOpti(AAddr,ADataSB);

 if RecvDataOpti(ADataSA)=FALSE then break;
 if RecvDataOpti(ADataSB)=FALSE then break;

 Result:=TRUE;
 until TRUE;
End;}

Function TMsProcess.SendB ( AAddr : word; AData : byte ) : boolean;
Var
  BDataS        : string;
Begin
 BDataS:=Chr(AData);
 Result:=SendS(AAddr,BDataS);
End;

Function TMsProcess.SendW ( AAddr : word; AData : word ) : boolean;
Var
  BDataS        : string;
Begin
 BDataS:=Chr(AData)+Chr(AData shr 8);
 Result:=SendS(AAddr,BDataS);
End;

Function TMsProcess.SendD ( AAddr : word; AData : Cardinal ) : boolean;
Var
  BDataS        : string;
Begin
 BDataS:=Chr(AData)+Chr(AData shr 8)+Chr(AData shr 16)+Chr(AData shr 24);
 Result:=SendS(AAddr,BDataS);
End;

Function TMsProcess.RecvB ( AAddr : word; Out AData : byte ) : boolean;
Var
  BDataS        : string;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RecvS(AAddr,1);
 if BDataS='' then break;
 AData:=Ord(BDataS[1]);
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.RecvW ( AAddr : word; Out AData : word ) : boolean;
Var
  BDataS        : string;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RecvS(AAddr,2);
 if BDataS='' then break;
 AData:=Word(BDataS[1])+(Word(BDataS[2]) shl 8);
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.RecvD ( AAddr : word; Out AData : Cardinal ) : boolean;
Var
  BDataS        : string;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RecvS(AAddr,4);
 if BDataS='' then break;
 AData:=Cardinal(BDataS[1])+(Cardinal(BDataS[2]) shl 8)+(Cardinal(BDataS[3]) shl 16)+(Cardinal(BDataS[4]) shl 24);
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.RecvQ ( AAddr : word; Out AData : QWord ) : boolean;
Var
  BDataS        : string;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RecvS(AAddr,8);
 if BDataS='' then break;
 AData:= QWord(BDataS[1])+
        (QWord(BDataS[2]) shl 8)+
        (QWord(BDataS[3]) shl 16)+
        (QWord(BDataS[4]) shl 24)+
        (QWord(BDataS[5]) shl 32)+
        (QWord(BDataS[6]) shl 40)+
        (QWord(BDataS[7]) shl 48)+
        (QWord(BDataS[8]) shl 56);
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.SendRecvSpi ( Var ADataBin : string ) : boolean;
Begin
 FlashLogS(ADataBin);
 Result:=SendRecvS($101,ADataBin);
 if Result then FlashLogR(ADataBin);
End;

Procedure TMsProcess.CpuIdWarn ( Const AMessage : string );
Begin
 if FCpuWarn=FALSE then
  begin
  FCpuWarn:=TRUE;
  ViewAny('e'+AMessage+'[R:TMsProcess.CpuIdWarn]');
  end;
End;

Function TMsProcess.GetCpuId : boolean;
Var
  BDataS    : string;
  BCoreList : string;
  BIndex    : Integer;
Begin
 Result:=FALSE;
 FMcuType:=0; FBrdVers:=0;
 repeat
 if FPlayer=plIss then begin FProcModel.SetMcuType(9); FMcuType:=FProcModel.McuType; Result:=TRUE; break; end;
 if FCommState<>csActive then break;
 BDataS:=RecvS(CDbgCpuInfoRd,4);
 if BDataS='' then break;
 BCoreList:=''; BIndex:=0; while BIndex<Ord(BDataS[1]) do begin BCoreList:=BCoreList+'s'; inc(BIndex); end; FCoreListHW:=BCoreList;
 if FCoreListPrj='' then
 else if FCoreListPrj<>FCoreListHW then CpuIdWarn('Number of cores specified by project params does not correspond to hardware');
 FMcuType:=Ord(BDataS[2]); FBrdVers:=Ord(BDataS[3]);
 FProcModel.SetMcuType(FMcuType);
 Result:=TRUE;
 until TRUE;
 if Result then FCpuWarn:=FALSE;
End;

Function TMsProcess.GetBrdVersion : boolean;
Var
  BDataS    : string;
  BCoreList : string;
  BIndex    : Integer;
Begin
 Result:=FALSE;
 FMcuType:=0; FBrdVers:=0;
 repeat
 if FCommState<>csActive then break;
 BDataS:=RecvS(CDbgCpuInfoRd,4);
 if BDataS='' then break;
 BCoreList:=''; BIndex:=0; while BIndex<Ord(BDataS[1]) do begin BCoreList:=BCoreList+'s'; inc(BIndex); end; FCoreListHW:=BCoreList;
 FMcuType:=Ord(BDataS[2]); FBrdVers:=Ord(BDataS[3]);
 FProcModel.SetMcuType(FMcuType);
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.IssExec;
Var
  BStepIdx  : Integer;
Begin
 BStepIdx:=0;
 while BStepIdx<10000 do
  begin
  if FProcModel.IsTrapHit then break;
  if FProcModel.IsBreakHit then break;
  if FProcModel.IsStuckAtError then break;
  FProcModel.StepInto;
  inc(BStepIdx);
  end;
 if FProcModel.IsTrapHit or FProcModel.IsBreakHit or FProcModel.IsStuckAtError then
  begin
  FMcxActive:=FALSE;
  ViewAny('h'+IntToHex(FProcModel.GetEtb,6));
  RecvMcxState(DutRdRegs);
  RegChange;
  BatchCheck(FProcModel.IsTestEnd);
  ViewAny('u3');
  end;
End;

Procedure TMsProcess.CombExec;
Var
  BStepIdx  : Integer;
  BRegs     : string;
Begin
 repeat
 BStepIdx:=0;
 while BStepIdx<10000 do
  begin
  if FProcModel.IsTrapHit then break;
  if FProcModel.IsBreakHit then break;
  if FProcModel.IsStuckAtError then break;
  if DutStepCombi(BRegs)=FALSE then break;
  inc(BStepIdx);
  end;
 if FProcModel.IsTrapHit or FProcModel.IsBreakHit or FProcModel.IsStuckAtError then
  begin
  FMcxActive:=FALSE;
  ViewAny('h'+IntToHex(FProcModel.GetEtb,6));
  RecvMcxState(DutRdRegs);
  RegChange;
  BatchCheck(FProcModel.IsTestEnd);
  ViewAny('u3');
  end;
 until TRUE;
End;

Procedure TMsProcess.Execute;
Var
  BTickHs,
  BTickThis     : QWord;
  BByte         : byte;
  BDbgCpuStat   : QWord;
  BRecvData     : string;
  BIsModelRun   : boolean;
Begin
 ViewAny('u0');
 //BTickHs:=GetTickCount64; // ##Model old
 BTickHs:=GetTickCount64; BTickThis:=BTickHs; // ##Model new

 Priority:=tpHigher;
 FModelDataSend:='';

 repeat
 if FMcxActive and (FPlayer=plIss) then RtlEventWaitFor(FProcAnyEvt,10)
 //else if RecvByteA(TRUE,BByte,10) then    // ##Model old <- model data can come asynchronously
 else if PollModel(BIsModelRun) then    // ##Model new <- model data can come asynchronously
  begin
  // Misleading byte
  Sleep(0);
  end;
 // ##Model old (empty line)
 if BIsModelRun then BTickHs:=BTickThis+500; // ##Model new
 if FModelDataSend<>'' then
  begin
  SendS($700,FModelDataSend);
  FModelDataSend:='';
  end;
 if FModelTerminate then
  begin
  FModelTerminate:=FALSE;
  DutStop;
  FMcxActive:=FALSE;
  if GetCpuStat(BDbgCpuStat)=FALSE then break;
  BRecvData:=RdRegsAll;
  if BRecvData<>'' then RecvMcxState(BRecvData);
  RegChange;
  end;
 BTickThis:=GetTickCount64;
 if FCmdWait then
  begin
  EnterCriticalSection(FCmdListLock);
  FCmdWait:=FALSE;
  FCmdListA.Assign(FCmdList); FCmdList.Clear;
  LeaveCriticalSection(FCmdListLock);
  ProcessCmdList;
  end;

 if FSetPlayer then begin FSetPlayer:=FALSE; CloseAll; OpenAny; FHsRepeatToFail:=5; end;

 if (BTickHs<BTickThis) or FForceActions then
  begin
  FForceActions:=FALSE;
  inc(BTickHs,500);
  repeat
  if (FPlayerParams='') and (FComStream<>nil) then begin CloseAll; break; end;
  if FPlayerParams='' then break;
  case FPlayer of
    plFpga,
    plComb:
        begin
        if (FPlayerParams<>'') and (FComStream=nil) then begin OpenAny; FHsRepeatToFail:=5; end;
        if FComStream=nil then begin ViewAny('u4'); inc(BTickHs,500); break; end;
        SendByteAny($55);
        if RecvByteA(TRUE,BByte) then
         begin
         if BByte=$55 then
          begin
          if FCommState<>csActive then
           begin
           SetCommState(csActive); ViewAny('u5'); // Connection established
           FGetCpuInfo:=FALSE;
           GetBrdVersion;
           end
          else if (FHsRepeatToFail<>5) or FCpuWarn or FGetCpuInfo then
           begin
           FGetCpuInfo:=FALSE;
           GetBrdVersion;
           end;
          SetCommState(csActive); ViewAny('u5'); // Connection established
          FHsRepeatToFail:=5;
          if FDebugActions<>'' then begin AppendCmd('c'+FDebugActions); FDebugActions:=''; end;
          break;
          end
         else // BByte<>$55
          begin
          Sleep(0);
          end
         end;
        if FHsRepeatToFail>0 then begin Dec(FHsRepeatToFail); break; end;
        ViewAny('u4'); // Connection error
        CloseAll;
        end;
    plIss:
        begin
        SetCommState(csSimulator); ViewAny('u6'); // Simulator mode
        if FDebugActions<>'' then begin AppendCmd('c'+FDebugActions); FDebugActions:=''; end;
        //ViewAny('r'+FProcModel.RdRegs);
        end;
   end; // case
  until TRUE;
  if BTickHs<BTickThis then BTickHs:=BTickThis;
  end;

 if FAttReq then begin ProcessAtt; FAttReq:=FALSE; end;

 if FMcxActive then
  begin
  case FPlayer of
    plIss:  IssExec;
    plComb: CombExec;
  end; // case
  end;

 until Terminated;

 FMsModel.ExecDone;
 CloseAll;
 FIsEnded:=TRUE;
 ViewAny('mClose');
End;

Procedure TMsProcess.ProcessAtt;
Var
  BDbgCpuStat   : QWord;
  BRegChange    : boolean;
  BAnyProcess   : boolean; // Used to repeat a loop again not to miss anything
  BActive       : boolean;
  BRecvData     : string;
  BTtyDataLen   : Cardinal;
  BEtb          : Cardinal; // {8'h0, TEnd[7:0], Trap[7:0], Break[7:0]}
Begin
 BRegChange:=FALSE;
 BActive:=FMcxActive;
 BEtb:=$0;

 repeat
 BAnyProcess:=FALSE;
 if GetCpuStat(BDbgCpuStat)=FALSE then break;
 // wire [63:0] BCpuStat =
 // {
 //  8'h0,
 //  {(8-CCoreCnt){1'b0}}, FTEndList,
 //  BTtySendSize[15:0],
 //  {(8-CCoreCnt){1'b0}}, FTrapList,
 //  {(8-CCoreCnt){1'b0}}, FBreakList,
 //  8'h0,
 //  FMemAccess, 3'h0, FIsStop, FDbgExecEn, FDbgResetS, FDbgClkHEn
 // };
 BEtb:=BEtb or ((BDbgCpuStat and $00FF000000000000) shr 32) or ((BDbgCpuStat and $00000000FFFF0000) shr 16);
 // CPU stat
 if (BDbgCpuStat and $04)=0 then
  begin
  BActive:=FALSE;
  BRecvData:=RdRegsAll;
  if BRecvData<>'' then begin BRegChange:=TRUE; RecvMcxState(BRecvData); end;
  //BAnyProcess:=TRUE;
  end;
 // Terminal
 BTtyDataLen:=(BDbgCpuStat shr 32) and $FFFF;
 if BTtyDataLen<>0 then
  begin
  BRecvData:=RecvS(CDbgTtyDataRd,BTtyDataLen);
  if BRecvData<>'' then ProcessTty(BRecvData);
  BAnyProcess:=TRUE;
  end;
 if BAnyProcess=FALSE then break;
 until FALSE;

 if BEtb<>0 then ViewAny('h'+IntToHex(BEtb,6)); // Set CPU index which caused break/trap

 if BRegChange or (BActive<>FMcxActive) then
  begin
  FMcxActive:=BActive;
  if FMcxActive=FALSE then BatchCheck(((BDbgCpuStat shr 48) and $FF)<>0);
  RegChange;
  end;
End;

Procedure TMsProcess.ProcessTty ( Const ATtyRecv : string );
Var
  BCharIdx      : Integer;
  BCharData     : char;
Begin
 BCharIdx:=0;
 while BCharIdx<Length(ATtyRecv) do
  begin
  BCharData:=ATtyRecv[1+BCharIdx];
  if (BCharData=#13) or (BCharData=#10) then begin ViewAny('t'+FTtyRecv); FTtyRecv:=''; end
  else FTtyRecv:=FTtyRecv+BCharData;
  inc(BCharIdx);
  end;
End;

Procedure TMsProcess.ModelProcessTerm ( AData : Char );
Begin
 if (AData=#13) or (AData=#10) then begin ViewAny('t'+FTtyRecv); FTtyRecv:=''; end
 else FTtyRecv:=FTtyRecv+AData;
End;

Procedure TMsProcess.ModelCommWr ( AData : byte );
Begin
 if AData in [10, 13] then begin ViewAny('AS'+FModelCommWrS); FModelCommWrS:=''; end
 else FModelCommWrS:=FModelCommWrS+Chr(AData);
End;

{Procedure TMsProcess.ProbaA ( AData : byte );
Begin
 AData:=13;
 if FModelCommRdS='' then AData:=13
 else begin AData:=Byte(FModelCommRdS[1]); Delete(FModelCommRdS,1,1); end;
End;}

Procedure TMsProcess.ModelCommRd ( Out AData : byte );
Begin
 AData:=13;
 if FModelCommRdS='' then AData:=13
 else begin AData:=Byte(FModelCommRdS[1]); Delete(FModelCommRdS,1,1); end;
End;

Function TMsProcess.LoadMem : boolean;
Var
  BMemSegIdx    : Integer;
  BMemSeg       : TMemSeg;
Begin
 Result:=FALSE;
 repeat
 if FPlayer=plIss then
  begin
  Result:=FProcModel.LoadMem(FBuild.MemSegList);
  break;
  end;
 if FPlayer=plComb then
  begin
  if FProcModel.LoadMem(FBuild.MemSegList)=FALSE then break;
  end;
 if FCommState<>csActive then break;
 if MemOperEn=FALSE then begin ViewAny('eCannot start memory operation [R:TMsProcess.LoadMem]'); break; end;
 ViewAny('p0');
 BMemSegIdx:=0;
 while BMemSegIdx<Length(FBuild.MemSegList) do
  begin
  BMemSeg:=FBuild.MemSegList[BMemSegIdx];
  if WrMemSeg(BMemSeg,TRUE)=FALSE then begin ViewAny('eCannot write memory segment "'+BMemSeg.SegName+'" [R:TMsProcess.LoadMem]'); break; end;
  inc(BMemSegIdx);
  end;
 if BMemSegIdx<>Length(FBuild.MemSegList) then break;
 ViewAny('p0');
 if MemOperDis=FALSE then begin ViewAny('eCannot end memory operation [R:TMsProcess.LoadMem]'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Const
  CBlockSize    = 1024;
  CColorOKS     = 'D0F880';
  CColorFLS     = 'F0F000';

Function TMsProcess.WrMemSeg ( AMemSeg : TMemSeg; AShowProgr : boolean ) : boolean;
Var
  BMemIdx   : Cardinal;
  BDataS    : string;
  BCopySize,
  BCopyIdx  : Cardinal;
  BMemAddr  : Cardinal;
Begin
 Result:=FALSE;
 repeat
 if ((AMemSeg.SegFlags and $8)=0) and ((AMemSeg.SegFlags and $2)=0) then begin Result:=TRUE; break; end;
 if AShowProgr then ViewAny('p0 '+CColorOKS+' '+AMemSeg.SegName);
 if SendD(CDbgMemAddrWr,AMemSeg.HwBase)=FALSE then break;
 BMemIdx:=0;
 while BMemIdx<AMemSeg.HwSize do
  begin
  BCopySize:=AMemSeg.HwSize-BMemIdx;
  if BCopySize>CBlockSize then BCopySize:=CBlockSize;
  BDataS:=''; SetLength(BDataS,BCopySize);
  BCopyIdx:=0;
  while BCopyIdx<BCopySize do
   begin
   if (BMemIdx+BCopyIdx)<Length(AMemSeg.Bin) then BDataS[1+BCopyIdx]:=AMemSeg.Bin[1+BMemIdx+BCopyIdx]
   else BDataS[1+BCopyIdx]:=#0;
   inc(BCopyIdx);
   end;
  if SendS(CDbgMemDataWr,BDataS)=FALSE then break;
  if RecvD(CDbgMemAddrRd,BMemAddr)=FALSE then break;
  if (AMemSeg.HwBase+BMemIdx+BCopySize)<>BMemAddr then break;
  inc(BMemIdx,BCopySize);
  if AShowProgr then ViewAny('p'+FloatToStr(BMemIdx/AMemSeg.HwSize)+' '+CColorOKS+' '+AMemSeg.SegName);
  end;
 if AShowProgr then
  begin
  if BMemIdx=AMemSeg.HwSize then ViewAny('p1 '+CColorOKS+' '+AMemSeg.SegName)
  else ViewAny('p'+FloatToStr(BMemIdx/AMemSeg.HwSize)+' 0000FF '+AMemSeg.SegName);
  ViewAny('p0');
  end;
 Result:=BMemIdx=AMemSeg.HwSize;
 until TRUE;
End;

Function TMsProcess.RdMemSeg ( AMemSeg : TMemSeg; AShowProgr : boolean ) : boolean;
Var
  BMemIdx   : Cardinal;
  BDataS    : string;
  BCopySize : Cardinal;
  BMemAddr  : Cardinal;
Begin
 Result:=FALSE;
 repeat
 if AShowProgr then ViewAny('p0 '+CColorOKS+' '+AMemSeg.SegName);
 if SendD(CDbgMemAddrWr,AMemSeg.HwBase)=FALSE then break;
 BMemIdx:=0;
 while BMemIdx<AMemSeg.HwSize do
  begin
  BCopySize:=AMemSeg.HwSize-BMemIdx;
  if BCopySize>CBlockSize then BCopySize:=CBlockSize;
  BDataS:=RecvS(CDbgMemDataRd,BCopySize);
  if Length(BDataS)<>BCopySize then break;
  AMemSeg.WrData(AMemSeg.HwBase+BMemIdx,BDataS);
  if RecvD(CDbgMemAddrRd,BMemAddr)=FALSE then break;
  if (AMemSeg.HwBase+BMemIdx+BCopySize)<>BMemAddr then break;
  inc(BMemIdx,BCopySize);
  if AShowProgr then ViewAny('p'+FloatToStr(BMemIdx/AMemSeg.HwSize)+' '+CColorOKS+' '+AMemSeg.SegName);
  end;
 if AShowProgr then
  begin
  if BMemIdx=AMemSeg.HwSize then ViewAny('p1 '+CColorOKS+' '+AMemSeg.SegName)
  else ViewAny('p'+FloatToStr(BMemIdx/AMemSeg.HwSize)+' 0000FF '+AMemSeg.SegName);
  ViewAny('p0');
  end;
 Result:=BMemIdx=AMemSeg.HwSize;
 until TRUE;
End;

Procedure TMsProcess.SetBreakList ( Const ABreakList : string );
Var
  BBreakList    : string;
  BIndex        : Integer;
  BAddrS        : string;
  BAddr         : Cardinal;
Begin
 BBreakList:=ABreakList;
 BIndex:=0;
 while BIndex<Length(FBreakList) do
  begin
  BAddrS:=ReadParamStr(BBreakList);
  if BAddrS='' then break;
  if HexToDWordCheck(BAddrS,BAddr)=FALSE then break;
  FBreakList[BIndex]:=BAddr;
  inc(BIndex);
  end;
 while BIndex<Length(FBreakList) do
  begin
  FBreakList[BIndex]:=$FFFFFFFF;
  inc(BIndex);
  end;
End;

Function DecPlayer ( APlayerParam : char ) : TPlayer;
Begin
 case APlayerParam of
  'f': Result:=plFpga;
  'b': Result:=plComb;
  'i': Result:=plIss;
  else Result:=plIss;
 end;
End;

Procedure TMsProcess.ProcessCmdList;
Var
  BLineIdx  : Integer;
  BReadS    : string;
  BCmd      : char;
  BFilename : string;
Begin
 BLineIdx:=0;
 while BLineIdx<FCmdListA.Count do
  begin
  BReadS:=FCmdListA.Strings[BLineIdx];
  BCmd:=BReadS[1]; Delete(BReadS,1,1);
  case BCmd of
   'y': begin
        FPlayerParams:=BReadS; if FPlayerParams<>'' then FPlayer:=DecPlayer(FPlayerParams[1]);
        FSetPlayer:=TRUE;
        end;
   'B': begin
        ViewAny('u1');
        if BatchExec(BReadS)=FALSE then ViewAny('AJeCannot start project [R:TMsProcess.ProcessCmdList]');
        ViewAny('u0');
        end;
   {'c': begin
        ProcessDebugActions(BReadS);
        end; }
   'S': begin
        if FUtestList.Count=0 then ProcessDebugActions(BCmd+BReadS)
        else
         begin
         BFilename:=FUtestList.Strings[0]; FUtestList.Delete(0);
         FSrcList:=BFilename; DelFirstLastSpace(FSrcList);
         ReportBatchStart;
         //ViewAny('u1');
         ProcessBuild('RIMRnib.S');
         ViewAny('u0');
         end;
        end;
   'b': begin
        ProcessDebugActions(BCmd+BReadS);
        end;
   'g': ReadSegData(BReadS);
   'o': ReadStack(BReadS);
   'u': ViewAny('u'+BReadS); // Echo
   'F': begin
        repeat
        if BReadS='' then break;
        case BReadS[1] of
         'n': FpgaReset;
         'l': ViewAny('fl'+IntToHex(Cardinal(@FFlashLog),8));
        else FpgaFlashOp(BReadS);
        end; // case
        until TRUE;
        end;
   'A': ProcessCmdExt(BReadS);
   else begin
        ProcessDebugActions(BCmd+BReadS);
        end;
  end;
  inc(BLineIdx);
  end;
 FCmdListA.Clear;
End;

Procedure TMsProcess.ProcessCmdExt ( Const AReadS : string );
Var
  BReadS    : string;
  BCmd      : char;
Begin
 BReadS:=AReadS;
 BCmd:=BReadS[1]; Delete(BReadS,1,1);
 case BCmd of
  'S': FModelCommRdS:=BReadS;
 end; // case
End;

Procedure TMsProcess.RdRegsOpti;
Var
  BRecvData : string;
Begin
 repeat
 BRecvData:=DutRdRegs; if BRecvData='' then break;
 RecvMcxState(BRecvData);
 until TRUE;
End;

Procedure TMsProcess.ProcessDebugActions ( Const AActions : string );
Var
  BActions      : string;
  BCmd          : char;
  BRegChange    : boolean;
Begin
 BRegChange:=FALSE;
 BActions:=AActions;
 while BActions<>'' do
  begin
  BCmd:=BActions[1]; Delete(BActions,1,1);
  case BCmd of
   'R': begin
        DutReset;
        FMcxActive:=FALSE;
        end;
   'I': GetCpuId;
   'M': LoadMem;
   'n': begin // FirstStep
        DutFirstStep;
        FMcxActive:=FALSE;
        end;
   'i': begin
        RdRegsOpti;
        BRegChange:=TRUE;
        end;
   'S': begin // Start
        DutStart;
        FMcxActive:=TRUE;
        BRegChange:=TRUE;
        end;
   'T': begin // Stop
        DutStop;
        FMcxActive:=FALSE;
        end;
   '1': begin
        DutStepBack;
        end;
   '7': begin // StepInto
        if FMcxActive then begin DutStop; RdRegsOpti; FMcxActive:=FALSE; BRegChange:=TRUE; end
        else DutStepInto(ReadTillC(BActions,'.'));
        end;
   '8': begin // StepOver
        if FMcxActive then begin DutStop; RdRegsOpti; FMcxActive:=FALSE; BRegChange:=TRUE; end
        else DutStepOver(ReadTillC(BActions,'.'));
        end;
   '4': begin // RunTo
        if FMcxActive then begin DutStop; RdRegsOpti; FMcxActive:=FALSE; BRegChange:=TRUE; end
        else DutRunTo(ReadTillC(BActions,'.'));
        end;
   'b': begin // Set Break List
        SetBreakList(ReadTillC(BActions,'.'));
        DutSetBreakList;
        end;
   'J': begin // Report state to external runner
        ViewAny('AJ+');
        end;
  end; // case
  end;

 if BRegChange then RegChange;
End;

Procedure TMsProcess.RegChange;
Var
  BActive       : string;
Begin
 if FMcxActive then BActive:='1' else BActive:='0';
 if FBatchMode then
 else ViewAny('r'+BActive+' '+IntToHex(FMcuType,2)+#13+FMcxPrev+#13+FMcxThis);
End;

Procedure TMsProcess.InvalidateMcx;
Begin
 FMcxThis:=FMcxPrev; FMcxThis:='';
End;

Function TMsProcess.RegBinToHex ( Const ASrc : string ) : string;
Var
  BByteCnt  : Integer;
  BRowCnt   : Integer;
  BColIdx,
  BRowIdx   : Integer;
Begin
 Result:='';
 if FMcuType=9 then BRowCnt:=12
 else BRowCnt:=8;
 BByteCnt:=BRowCnt*8;
 repeat
 if Length(ASrc)<>BByteCnt then
  begin
  BColIdx:=0; while BColIdx<BByteCnt do begin Result:=Result+'XX'; inc(BColIdx); end;
  break;
  end;
 //Result:=StrBinToHex(ASrc);
 BRowIdx:=0;
 while BRowIdx<BRowCnt do
  begin
  for BColIdx:=0 to 7 do
   begin
   Result:=Result+IntToHex(Ord(ASrc[1+BRowIdx*8+7-BColIdx]),2);
   end;
  inc(BRowIdx);
  end;
 until TRUE;
End;

Function TMsProcess.RecvMcxStateA ( Const ARegsBin : string ) : string;
Var
  BRowCnt,
  BByteCnt  : Integer;
  BRegsSrc,
  BRegsDst  : string;
Begin
 if FMcuType=9 then BRowCnt:=12
 else BRowCnt:=8;
 BByteCnt:=BRowCnt*8;
 BRegsSrc:=ARegsBin; BRegsDst:='';
 while BRegsSrc<>'' do
  begin
  if BRegsDst<>'' then BRegsDst:=BRegsDst+' ';
  BRegsDst:=BRegsDst+RegBinToHex(Copy(BRegsSrc,1,BByteCnt));
  Delete(BRegsSrc,1,BByteCnt);
  end;
 Result:=BRegsDst;
End;

Procedure TMsProcess.RecvMcxState ( Const ARegsBin : string );
Begin
 FMcxPrev:=FMcxThis; FMcxThis:=RecvMcxStateA(ARegsBin);
End;

Function ReadEip ( Const ARegsS : string; ACore : char; ACoreIdx : Cardinal ) : Cardinal;
Var
  BCharIdx  : Cardinal;
  BCharPos  : Cardinal;
Begin
 Result:=0;
 for BCharIdx:=0 to 3 do
  begin
  BCharPos:=ACoreIdx*64+3-BCharIdx;
  if BCharPos>=Length(ARegsS) then break;
  Result:=(Result shl 8) or Ord(ARegsS[1+BCharPos]);
  end;
 if ACore='s' then Result:=Result and $FFFFFE;
End;

Function TMsProcess.RdRegsAll : string;
Begin
 Result:='';
 if FMcuType=$9 then Result:=RecvS(CDbgCpuRegsRd,Length(FCoreListHW)*96)
 else Result:=RecvS(CDbgCpuRegsRd,Length(FCoreListHW)*64);
End;

Function TMsProcess.DutRdRegs : string;
Var
  BRegsA,
  BRegsB    : string;
Begin
 Result:='';
 repeat
 if FCoreListPrj='' then break;
 case FPlayer of
  plIss:
    begin
    Result:=FProcModel.RdRegsBin;
    end;
  plFpga:
    begin
    Result:=RdRegsAll;
    end;
  plComb:
    begin
    BRegsA:=FProcModel.RdRegsBin;
    BRegsB:=RdRegsAll;
    if BRegsA<>BRegsB then
     begin
     ViewAny('wCombi mode register missmatch '+StrBinToHex(BRegsA)+''+StrBinToHex(BRegsB)+' [R:TMsProcess.DutRdRegs]');
     ViewAny('- ISS: '+StrBinToHex(BRegsA));
     ViewAny('- Dig: '+StrBinToHex(BRegsB));
     end;
    Result:=BRegsB;
    end;
 end;
 until TRUE;
End;

Function TMsProcess.SetCpuCtrl ( ACtrl : byte ) : boolean;
Begin
 Result:=SendB(CDbgCpuCtrlWr,ACtrl);
 FCpuCtrl:=ACtrl;
End;

Function TMsProcess.GetCpuStat ( Out AStat : QWord ) : boolean;
Begin
 Result:=FALSE; AStat:=0;
 repeat
 if RecvQ(CDbgCpuStatRd,AStat)=FALSE then break;
 FCpuCtrl:=AStat and $F7;
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.MemOperEn : boolean;
Begin
 Result:=SetCpuCtrl(FCpuCtrl or $80);
End;

Function TMsProcess.MemOperDis : boolean;
Begin
 Result:=SetCpuCtrl(FCpuCtrl and $7F);
End;

Procedure TMsProcess.DutReset;
Begin
 FMsModel.ExecDone;
 repeat
 case FPlayer of
  plIss:
    begin
    FExecLogIsIn:=FALSE; FExecLog.Clear;
    FProcModel.Reset;
    end;
  plFpga:
    begin
    if SetCpuCtrl($03)=FALSE then break;
    if SetCpuCtrl($01)=FALSE then break;
    if SendD(CDbgBrkThisWr,0)=FALSE then break;
    end;
  plComb:
    begin
    FExecLogIsIn:=FALSE; FExecLog.Clear;
    FProcModel.Reset;
    if SetCpuCtrl($03)=FALSE then break;
    if SetCpuCtrl($01)=FALSE then break;
    if SendD(CDbgBrkThisWr,0)=FALSE then break;
    end;
 end;
 until TRUE;
 FTtyRecv:='';
End;

Procedure TMsProcess.DutFirstStep;
Begin
 repeat
 case FPlayer of
  plIss:
    begin
    FProcModel.FirstStep;
    end;
  plFpga:
    begin
    if SendB(CDbgCpuStepWr,$01)=FALSE then break;
    end;
  plComb:
    begin
    FProcModel.FirstStep;
    if SendB(CDbgCpuStepWr,$01)=FALSE then break;
    end;
 end;
 FMsModel.ExecInit(FPrjPath,FModelLibs,FOnViewAny,FSwVersion);
 until TRUE;
End;

Procedure TMsProcess.DutStart;
Begin
 repeat
 case FPlayer of
  plIss:
    begin
    FProcModel.StepInto; // One step to overcome breakpoint
    FProcModel.ClearTrapHit;
    end;
  plFpga:
    begin
    if SendD(CDbgBrkThisWr,0)=FALSE then break;
    if SendB(CDbgCpuStepWr,$01)=FALSE then break;
    if SetCpuCtrl($05)=FALSE then break;
    end;
  plComb:
    begin
    FProcModel.ClearTrapHit;
    DutStepInto; // One step to overcome breakpoint
    end;
 end;
 until TRUE;
End;

Procedure TMsProcess.DutStop;
Begin
 repeat
 case FPlayer of
  plIss:
    begin
    end;
  plFpga,
  plComb:
    begin
    if SetCpuCtrl($01)=FALSE then break;
    end;
 end;
 until TRUE;
End;

Procedure TMsProcess.DutSetBreakList;
Var
  BSendData     : string;
Begin
 repeat
 case FPlayer of
  plIss,
  plComb:
    begin
    FProcModel.SetBreakList(FBreakList);
    end;
  plFpga:
    begin
    BSendData:=''; SetLength(BSendData,8*4);
    Move(FBreakList[0],BSendData[1],8*4);
    if SendS(CDbgBrkListWr,BSendData)=FALSE then break;
    end;
 end;
 until TRUE;
End;

Procedure TMsProcess.DutStepBack;
Begin
 repeat
 if FExecLogIsIn=FALSE then
  begin
  if (FPlayer=plIss) or (FPlayer=plComb) then
   begin
   FExecLog.Assign(FProcModel.ExecLog);
   FExecLogPos:=FExecLog.Count-1;
   end
  else
   begin
   ViewAny('eFPGA step backward is not yet implemented [R:TMsProcess.DutStepBack]');
   break;
   end;
  FExecLogIsIn:=TRUE;
  end;
 if FExecLogPos<=0 then break;
 Dec(FExecLogPos);
 RecvMcxState(FExecLog.Strings[FExecLogPos]);
 RegChange;
 until TRUE;
End;

Function TMsProcess.DutStepCombi ( Out ARegs : string ) : boolean;
Var
  BRegsA,
  BRegsB    : string;
Begin
 Result:=FALSE; ARegs:='';
 repeat
 if SendB(CDbgCpuStepWr,$01)=FALSE then break;
 BRegsA:=RdRegsAll; if BRegsA='' then break;
 if FPlayer=plComb then
  begin
  FProcModel.StepCombi(BRegsA);
  BRegsB:=FProcModel.RdRegsBin;
  if BRegsA<>BRegsB then
   begin
   ViewAny('eCombi mode register missmatch [R:TMsProcess.DutStepInto]');
   ViewAny('- Dig: '+StrBinToHex(BRegsA));
   ViewAny('- Iss: '+StrBinToHex(BRegsB));
   ViewAny('- '+FProcModel.VerboseStat);
   FMcxActive:=FALSE;
   RecvMcxState(BRegsA);
   if FBatchMode then
   else ViewAny('r0 '+IntToHex(FMcuType,2)+#13+FMcxPrev+#13+FMcxThis+#13+RecvMcxStateA(FProcModel.RdRegsBin));
   BatchCheck(FALSE);
   break;
   end;
  end;
 ARegs:=BRegsA;
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.DutStepInto ( Const ADataS : string );
Var
  BRecvData     : string;
  BCore         : char;
  BCoreIdx      : Cardinal;
  BAddr,
  BAddrA        : Cardinal;
  BTryIdx       : Integer;
Begin
 repeat
 if FExecLogIsIn then
  begin
  inc(FExecLogPos);
  if FExecLogPos>=FExecLog.Count then FExecLogIsIn:=FALSE
  else
   begin
   RecvMcxState(FExecLog.Strings[FExecLogPos]);
   RegChange;
   break;
   end;
  end;

 if FPlayer=plIss then
  begin
  FProcModel.StepInto;
  RecvMcxState(FProcModel.RdRegsBin);
  RegChange;
  break;
  end;

 if FMcxActive then
  begin
  if SetCpuCtrl($01)=FALSE then break;
  if SendD(CDbgBrkThisWr,0)=FALSE then break;
  FMcxActive:=FALSE;
  BRecvData:=RdRegsAll; if BRecvData='' then break;
  RecvMcxState(BRecvData);
  RegChange;
  break;
  end;

 BCore:=#0; BCoreIdx:=0; BAddr:=0;
 if ADataS<>'' then
  begin
  BCore:=ADataS[1];
  if HexToDwordCheck(Copy(ADataS,2,2),BCoreIdx)=FALSE then break;
  if HexToDwordCheck(Copy(ADataS,4,8),BAddr)=FALSE then break;
  end;

 BRecvData:='';
 for BTryIdx:=0 to 4 do
  begin
  if DutStepCombi(BRecvData)=FALSE then break;
  if BCore=#0 then break;
  BAddrA:=ReadEip(BRecvData,BCore,BCoreIdx);
  if BAddrA<>BAddr then break;
  end;

 if BRecvData<>'' then
  begin
  RecvMcxState(BRecvData);
  RegChange;
  end;
 until TRUE;
End;

Procedure TMsProcess.DutStepInto;
Begin
 DutStepInto('');
End;

Function TMsProcess.CallOrJmp ( AAddr : Cardinal; ACpuType : char ) : TCallOrJmp;
Var
  BAddr         : Cardinal;
  BCodeBin      : string;
  BReadSize     : Cardinal;
  BExecLine     : TExecLineBase;
  BResult       : TCallOrJmp;
  BSeg          : TMemSeg;
Begin
 Result:=cjUnknown;
 BAddr:=AAddr;
 if ACpuType='e' then BExecLine:=TExecLineRV.Create
 else BExecLine:=TExecLineSD.Create;
 repeat
 BSeg:=MemSegSearch(FBuild.MemSegList,BAddr);
 if BSeg=nil then break;
 BCodeBin:=BSeg.RdData(BAddr,6);
 BReadSize:=Length(BCodeBin); if BReadSize=0 then break;
 if BExecLine.CmdDec(BAddr,BCodeBin)=FALSE then break;
 if BExecLine.IsDecStop then break;
 BResult:=BExecLine.CallOrJmp;
 if BResult<>cjUnknown then begin Result:=BResult; break; end;
 BAddr:=BAddr+Length(BExecLine.CodeBin);
 until FALSE;
 BExecLine.Free;
End;

Procedure TMsProcess.DutStepOver ( Const ADataS : string );
Var
  BCore     : char;
  BAddr     : Cardinal;
  BSeg      : TMemSeg;
  BDasm     : TExecLineBase;
  BDataBin  : string;
Begin
 BDasm:=nil;
 repeat
 if FMcxActive or FExecLogIsIn then
  begin
  DutStepInto;
  break;
  end;
 if ADataS='' then break;
 BCore:=ADataS[1];
 if HexToDwordCheck(Copy(ADataS,4,8),BAddr)=FALSE then break;
 BSeg:=MemSegSearch(FBuild.MemSegList,BAddr);
 if BSeg=nil then break;
 if BCore='s' then BDasm:=TExecLineSD.Create
 else BDasm:=TExecLineRV.Create;
 BDataBin:=BSeg.RdData(BAddr,6);
 if BDasm.CmdDec(BAddr,BDataBin)=FALSE then break;
 if BDasm.IsCall then
  begin
  DutRunTo(BAddr+Length(BDasm.CodeBin));
  end
 else if BDasm.IsJxx then
  begin
  if BDasm.DstAddr=0 then DutStepInto
  else if CallOrJmp(BDasm.DstAddr,BCore)=cjCall then DutRunTo(BAddr+Length(BDasm.CodeBin))
  else DutStepInto;
  end
 else DutStepInto;
 until TRUE;

 if BDasm<>nil then BDasm.Free;
End;

Procedure TMsProcess.DutRunTo ( AAddr : Cardinal );
Var
  BRecvData     : string;
Begin
 repeat
 if FPlayer=plIss then
  begin
  FProcModel.RunTo(AAddr);
  RecvMcxState(FProcModel.RdRegsBin);
  end
 else
  begin
  if FMcxActive then
   begin
   if SetCpuCtrl($01)=FALSE then break;
   if SendD(CDbgBrkThisWr,0)=FALSE then break;
   FMcxActive:=FALSE;
   BRecvData:=RdRegsAll; if BRecvData='' then break;
   RecvMcxState(BRecvData);
   end
  else
   begin
   if SendD(CDbgBrkThisWr,AAddr)=FALSE then break;
   FMcxActive:=TRUE;
   if SetCpuCtrl($05)=FALSE then break;
   end;
  end;
 RegChange;
 until TRUE;
End;

Procedure TMsProcess.DutRunTo ( Const AAddrS : string );
Var
  BAddr     : Cardinal;
  BSeg      : TMemSeg;
Begin
 repeat
 if HexToDwordCheck(AAddrS,BAddr)=FALSE then break;
 BSeg:=MemSegSearch(FBuild.MemSegList,BAddr);
 if BSeg=nil then break;
 DutRunTo(BAddr);
 until TRUE;
End;

Const
  CFpgaRhsCnt   = 100;

Function TMsProcess.FpgaResetA ( ASrc : byte ) : boolean;
Var
  BHsCount  : Integer;
  BByte     : byte;
Begin
 Result:=FALSE;
 repeat
 // ResetSrc
 if SendB(CDbgStartupWr,$10 or (ASrc and $01))=FALSE then begin ViewAny('feCommunication error (FPGA reset) [R:TMsProcess.FpgaResetA]'); break; end;
 Sleep(50);
 // ResetReq
 SendB(CDbgStartupWr,$80); // there is no response after this command because device is reset
 Sleep(1000);
 // Handshake
 BHsCount:=0;
 while BHsCount<CFpgaRhsCnt do
  begin
  ClearRecvBuf;
  SendByteAny($55);
  if RecvByteA(TRUE,BByte) and (BByte=$55) then break;
  Sleep(50);
  inc(BHsCount);
  end;
 if BHsCount>=CFpgaRhsCnt then begin ViewAny('feFPGA handshake error after reset [R:TMsProcess.FpgaResetA]'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.FpgaReset;
Begin
 ViewAny('fiFPGA will be reset (it can take up to a few seconds for read a new configuration) [R:TMsProcess.FpgaReset]');
 if FpgaResetA($00) then ViewAny('fiFPGA is reset [R:TMsProcess.FpgaReset]')
 else ViewAny('fiFPGA reset error [R:TMsProcess.FpgaReset]');
End;

Procedure TMsProcess.ReadSegData ( Const ASegParams : string );
Var
  BParams   : string;
  BSegName  : string;
  BSeg      : TMemSeg;
Begin
 BParams:=ASegParams;
 repeat
 BSegName:=ReadParamStr(BParams);
 if BSegName='' then break;

 case FPlayer of
   plIss:
     begin
     BSeg:=MemSegSearch(FProcModel.MemSegList,BSegName);
     if BSeg=nil then begin ViewAny('eInternal error: segment is not found [R:TMsProcess.ReadSegData]'); break; end;
     ViewAny('g'+BSegName+' '+IntToHex(BSeg.HwBase,8)+' '+MsBinExport(@BSeg.Bin));
     end;
   plFpga,
   plComb:
     begin
     BSeg:=MemSegSearch(FBuild.MemSegList,BSegName);
     if BSeg=nil then begin ViewAny('eInternal error: segment is not found [R:TMsProcess.ReadSegData]'); break; end;
     if MemOperEn=FALSE then begin ViewAny('eCannot start memory operation [R:TMsProcess.ReadSegData]'); break; end;
     if RdMemSeg(BSeg,FALSE)=FALSE then begin ViewAny('eError reading segment data [R:TMsProcess.ReadSegData]'); break; end;
     if MemOperDis=FALSE then begin ViewAny('eCannot stop memory operation [R:TMsProcess.ReadSegData]'); break; end;
     ViewAny('g'+BSegName+' '+IntToHex(BSeg.HwBase,8)+' '+MsBinExport(@BSeg.Bin));
     end;
 end;
 until TRUE;
End;

Procedure TMsProcess.ReadStack ( Const AParams : string );
Var
  BParams   : string;
  BSeg      : TMemSeg;
  BEip,
  BEsp      : Cardinal;
  BSize     : Cardinal;
  BDataBin  : string;
  BRdAddr   : Cardinal;
Begin
 BParams:=AParams;
 repeat
 if HexToDWordCheck(ReadParamStr(BParams),BEip)=FALSE then begin ViewAny('e 0 0 Internal error getting stack values [R:TMsProcess.ReadStack]'); break; end;
 if HexToDWordCheck(ReadParamStr(BParams),BEsp)=FALSE then begin ViewAny('e 0 0 Internal error getting stack values [R:TMsProcess.ReadStack]'); break; end;
 if HexToDWordCheck(ReadParamStr(BParams),BSize)=FALSE then begin ViewAny('e 0 0 Internal error getting stack values [R:TMsProcess.ReadStack]'); break; end;
 if BSize=0 then break;
 BRdAddr:=BEsp;
 if (BRdAddr and $F)<>0 then
  begin
  BRdAddr:=BRdAddr and $FFFFFFF0;
  BSize:=BSize+16;
  end;
 BSize:=(BSize+$F) and $FFFFFFF0;

 BDataBin:='';
 case FPlayer of
   plIss:
     begin
     BSeg:=MemSegSearch(FProcModel.MemSegList,BRdAddr);
     if BSeg=nil then
      begin
      //ViewAny('eInternal error: segment is not found [R:TMsProcess.ReadStack]');
      ViewAny('o '+IntToHex(BEip,8)+' '+IntToHex(BEsp,8)+' '+#13);
      break;
      end;
     BDataBin:=BSeg.RdData(BRdAddr,BSize);
     end;
   plFpga,
   plComb:
     begin
     BSeg:=MemSegSearch(FBuild.MemSegList,BRdAddr);
     if BSeg=nil then begin ViewAny('o '+IntToHex(BEip,8)+' '+IntToHex(BEsp,8)+' '+#13); break; end;
     if MemOperEn=FALSE then begin ViewAny('eCannot start memory operation [R:TMsProcess.ReadStack]'); break; end;
     if SendD(CDbgMemAddrWr,BRdAddr)=FALSE then begin ViewAny('eCannot set memory address [R:TMsProcess.ReadStack]'); break; end;
     BDataBin:=RecvS(CDbgMemDataRd,BSize);
     if Length(BDataBin)<>BSize then begin ViewAny('eError reading memory [R:TMsProcess.ReadStack]'); break; end;
     if MemOperDis=FALSE then begin ViewAny('eCannot stop memory operation [R:TMsProcess.ReadStack]'); break; end;
     end;
 end;

 if BRdAddr<>BEsp then Delete(BDataBin,1,BEsp-BRdAddr);
 ViewAny('o '+IntToHex(BEip,8)+' '+IntToHex(BEsp,8)+' '+StrBinToHex(BDataBin)+#13);
 until TRUE;
End;

Function TMsProcess.GetFlashSize ( AFlashSizeCode : byte ) : boolean;
Begin
 Result:=FALSE; FFlashSize:=fsUnknown;
 FFlashAddrLen:=3; FFlashCmdEr:=$D8; FFlashCmdWr:=$02; FFlashCmdRd:=$03;
 repeat
 case AFlashSizeCode of
   $17: FFlashSize:=fs64M;
   $18: FFlashSize:=fs128M;
   $19: FFlashSize:=fs256M;
   $20: FFlashSize:=fs512M;
   $21: FFlashSize:=fs01G;
   $22: FFlashSize:=fs02G;
   else break;
 end;
 if FFlashSize>fs128M then begin FFlashAddrLen:=4; FFlashCmdEr:=$DC; FFlashCmdWr:=$12; FFlashCmdRd:=$13;  end;
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.FlashAddrToStr ( AAddr : Cardinal ) : string;
Begin
 Result:=
   Chr((AAddr shr 16) and $FF)+
   Chr((AAddr shr 8) and $FF)+
   Chr(AAddr and $FF);
 if FFlashSize>fs128M then Result:=Chr((AAddr shr 24) and $FF)+Result;
End;

Function TMsProcess.ProbeFlash : boolean;
Var
  BDataBin      : string;
Begin
 Result:=FALSE;
 repeat
 BDataBin:=#$66;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCannot reset device [R:TMsProcess.FpgaFlashOp]'); break; end;
 BDataBin:=#$99;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCannot reset device [R:TMsProcess.FpgaFlashOp]'); break; end;
 Sleep(1);
 // Exit sleep mode
 BDataBin:=#$AB;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCannot exit Sleep mode [R:TMsProcess.FpgaFlashOp]'); break; end;
 // Read ID
 //            Manufacturer MemoryType MemorySize [17h = 64M, 18h = 128M and so on]
 // S25FL128  = 012018
 // W25Q128   = 176018 178018
 // MT25QL01G = 20BA21
 BDataBin:=#$9F+#$FF+#$FF+#$FF;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCannot obtain Flash ID [R:TMsProcess.FpgaFlashOp]'); break; end;
 ViewAny('fiDevice Flash ID: '+IntToHex(Ord(BDataBin[2]),2)+IntToHex(Ord(BDataBin[3]),2)+IntToHex(Ord(BDataBin[4]),2)+' [R:TMsProcess.FpgaFlashOp]');
 if GetFlashSize(Ord(BDataBin[4]))=FALSE then begin ViewAny('feUnsupported memory size [R:TMsProcess.FpgaFlashOp]'); break; end;
 // Erase Fail flag if set
 {BDataBin:=#$07+#$FF;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error [R:TMsProcess.FpgaFlashOp]'); break; end;
 BRegData:=Ord(BDataBin[2]);
 if (BRegData and $60)<>0 then
  begin
  BDataBin:=#$30;
  SendRecvSpi(BDataBin);
  end;
 BDataBin:=#$07+#$FF;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error [R:TMsProcess.FpgaFlashOp]'); break; end;
 BRegData:=Ord(BDataBin[2]);
 if (BRegData and $60)<>0 then
  begin
  BDataBin:=#$30;
  SendRecvSpi(BDataBin);
  end;}
 Result:=TRUE;
 until TRUE;
End;

Procedure TMsProcess.FpgaFlashOp ( Const ASectList : string );
Var
  BSectList     : string;
  BSectThis     : string;
  BCmd          : char;
  BSuccess      : boolean;
  BResetNeeded  : boolean;
  BIsExt        : boolean;
Begin
 FFlashLog.Clear; FFlashLogKeep:=TRUE;
 repeat
 if FCommState<>csActive then begin ViewAny('feCommunication with the board is not established [R:TMsProcess.FpgaFlashOp]'); break; end;
 case FPlayer of
   plIss:
     begin
     ViewAny('feFPGA reflash will not work in ISS mode [R:TMsProcess.FpgaFlashOp]');
     end;
   plFpga,
   plComb:
     begin
     BSuccess:=FALSE; BIsExt:=FALSE;
     // Probe Flash
     if ProbeFlash=FALSE then break;
     BSectList:=ASectList; if BSectList='' then break;
     BCmd:=BSectList[1]; Delete(BSectList,1,1);
     BResetNeeded:=FALSE;
     repeat
     BSectThis:=ReadParamStr(BSectList,';');
     if BSectThis='' then
      begin
      BSuccess:=TRUE;
      break;
      end;
     case BCmd of
       'w': begin
            if FpgaReflashSect(BSectThis,BIsExt)=FALSE then break;
            BResetNeeded:=TRUE;
            end;
       'r': begin
            if FpgaReadSect(BSectThis)=FALSE then break;
            end;
       else begin
            ViewAny('feInvalid operation [R:TMsProcess.FpgaFlashOp]');
            break;
            end;
       end;
     until FALSE;
     if BSuccess=FALSE then
      begin
      ViewAny('feFPGA operation stopped by error [R:TMsProcess.FpgaFlashOp]');
      break;
      end;
     // Reset Startup flags again (to main flash)
     if SendB(CDbgStartupWr,$00)=FALSE then begin ViewAny('feCommunication error (Reset startup flags) [R:TMsProcess.FpgaFlashOp]'); break; end;
     ViewAny('fiFPGA operation finished [R:TMsProcess.FpgaFlashOp]');
     if BIsExt then
      begin
      ViewAny('fiDevice will not be reset because it is used to reprogram external flash [R:TMsProcess.FpgaFlashOp]');
      end
     else if BResetNeeded then
      begin
      ViewAny('fiDevice will be reset to read a new configuration [R:TMsProcess.FpgaFlashOp]');
      if FpgaResetA($00)=FALSE then break;
      end;
     end;
 end;
 until TRUE;
 FFlashLogKeep:=FALSE;
End;

Function TMsProcess.ReadBinFile ( Const AFilename : string; Out ADataBin : string ) : boolean;
Var
  BStream   : TMemoryStream;
Begin
 Result:=FALSE; ADataBin:='';
 BStream:=TMemoryStream.Create;
 repeat
 try
   BStream.LoadFromFile(AFilename);
 except
   ViewAny('feCannot read file "'+AFilename+'" [R:TMsProcess.ReadBinFile]');
   break;
 end;
 BStream.Position:=0;
 SetLength(ADataBin,BStream.Size);
 BStream.Read(ADataBin[1],BStream.Size);
 Result:=TRUE;
 until TRUE;
 BStream.Free;
End;

Function TMsProcess.ReadHexFile ( Const AFilename : string; Out ADataBin : string ) : boolean;
Var
  BList     : TStringList;
  BBaseAddr : Cardinal;
  BErrorS   : string;
Begin
 Result:=FALSE; ADataBin:='';
 BList:=TStringList.Create;
 repeat
 try
   BList.LoadFromFile(AFilename);
 except
   ViewAny('feCannot read file "'+AFilename+'" [R:TMsProcess.ReadBinFile]');
   break;
 end;
 BErrorS:=DataFiles.HexFileParse(BList,BBaseAddr,ADataBin);
 if BErrorS<>'' then
  begin
  ViewAny('feError parsing file "'+AFilename+'": '+BErrorS+' [R:TMsProcess.ReadBinFile]');
  break;
  end;
 Result:=TRUE;
 until TRUE;
 BList.Free;
End;

Function TMsProcess.TryReadEfinixHex ( Const AFilename : string; Out ADataBin : string ) : Integer;
Var
  BList     : TStringList;
  BErrorS   : string;
Begin
 Result:=-1; ADataBin:='';
 BList:=TStringList.Create;
 repeat
 try
   BList.LoadFromFile(AFilename);
 except
   ViewAny('feCannot read file "'+AFilename+'" [R:TMsProcess.ReadBinFile]');
   break;
 end;
 if TryParseEfinixHex(BList,ADataBin,BErrorS)=FALSE then begin Result:=-2; break; end;
 if BErrorS<>'' then begin ViewAny('feError parsing file "'+AFilename+'": '+BErrorS+' [R:TMsProcess.ReadBinFile]'); break; end;
 Result:=0;
 until TRUE;
 BList.Free;
End;

Function TMsProcess.EraseFlashBlockGen ( AAddr : Cardinal ) : boolean;
Var
  BDataBin      : string;
  BRepeat       : Integer;
  BResult       : boolean;
  BRegData      : byte;
Begin
 Result:=FALSE;
 repeat
 // Status register 2 read
 //BDataBin:=#$35+#$FF;
 //if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Ready flag reading) [R:TMsProcess.ProgramFlashPageGen]'); break; end;

 // Write enable
 BDataBin:=#$06;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Erase block) [R:TMsProcess.EraseFlashBlockGen]'); break; end;

 // Check if flag is set
 BDataBin:=#$05+#$FF;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Ready flag reading) [R:TMsProcess.ProgramFlashPageGen]'); break; end;
 BRegData:=Ord(BDataBin[2]);
 if (BRegData and $02)=0 then begin ViewAny('feCannot set Write Enable Latch [R:TMsProcess.ProgramFlashPageGen]'); break; end;

 // Erase page
 BDataBin:=Chr(FFlashCmdEr)+FlashAddrToStr(AAddr);
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Erase block) [R:TMsProcess.EraseFlashBlockGen]'); break; end;

 // Read flag
 BRepeat:=1000;
 BResult:=FALSE;
 while BRepeat>0 do
  begin
  BDataBin:=#$05+#$FF;
  if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Erase block) [R:TMsProcess.EraseFlashBlockGen]'); break; end;
  BRegData:=Ord(BDataBin[2]);
  if (BRegData and $01)=0 then begin BResult:=TRUE; break; end;
  Sleep(10);
  dec(BRepeat);
  end;
 if BResult=FALSE then begin ViewAny('feBlock erase time out [R:TMsProcess.EraseFlashBlockGen]'); break; end;

 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.ReadFlashFlagGen : boolean;
Var
  BDataBin      : string;
  BRepeat       : Integer;
  BRegData      : byte;
Begin
 BRepeat:=1000;
 Result:=FALSE;
 while BRepeat>0 do
  begin
  BDataBin:=#$05+#$FF;
  if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Write block) [R:TMsProcess.ProgramFlashPageGen]'); break; end;
  BRegData:=Ord(BDataBin[2]);
  if (BRegData and $01)=0 then begin Result:=TRUE; break; end;
  Sleep(10);
  dec(BRepeat);
  end;
End;

Function TMsProcess.WriteFlashPageGen ( AAddr : Cardinal; Const APageData : string ) : boolean;
Var
  BDataBinF,
  BDataBinE,
  BDataBinW     : string;
  BRegData      : byte;
  BRepeat       : Integer;
Begin
 Result:=FALSE;

 repeat
 // Verify flag from previous time: this is somewhat faster
 BRepeat:=1000;
 Result:=FALSE;
 while BRepeat>0 do
  begin
  BDataBinF:=#$05+#$FF;
  if SendRecvSpi(BDataBinF)=FALSE then begin ViewAny('feCommunication error (Ready flag reading) [R:TMsProcess.ProgramFlashPageGen]'); break; end;
  BRegData:=Ord(BDataBinF[2]);
  if (BRegData and $01)=0 then begin Result:=TRUE; break; end;
  Sleep(1);
  dec(BRepeat);
  end;
 if Result=FALSE then break;
 Result:=FALSE;

 // Program this page
 BDataBinE:=#$06;
 BDataBinW:=Chr(FFlashCmdWr)+FlashAddrToStr(AAddr)+APageData;

 if SendRecvSpi(BDataBinE)=FALSE then begin ViewAny('feCommunication error (Program block Enable) [R:TMsProcess.WriteFlashPageGen]'); break; end;
 if SendRecvSpi(BDataBinW)=FALSE then begin ViewAny('feCommunication error (Program block Write) [R:TMsProcess.WriteFlashPageGen]'); break; end;

 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.ReadFlashGen ( AAddr : Cardinal; ASize : Cardinal; Out ADataS : string ) : boolean;
Var
  BDataS    : string;
  BDataIdx  : Integer;
Begin
 Result:=FALSE; ADataS:='';

 repeat
 BDataS:=Chr(FFlashCmdRd)+FlashAddrToStr(AAddr);
 BDataIdx:=0; while BDataIdx<ASize do begin BDataS:=BDataS+#$FF; inc(BDataIdx); end;
 if SendRecvSpi(BDataS)=FALSE then begin ViewAny('feCommunication error (Reading flash) [R:TMsProcess.ReadFlashGen]'); break; end;
 Delete(BDataS,1,1+FFlashAddrLen);
 ADataS:=BDataS;
 Result:=TRUE;
 until TRUE;
End;
{
Procedure TMsProcess.ReadFlashGenS ( AAddr : Cardinal; ASize : Cardinal );
Var
  BDataS    : string;
  BDataIdx  : Integer;
Begin
 BDataS:=Chr(FFlashCmdRd)+FlashAddrToStr(AAddr);
 BDataIdx:=0; while BDataIdx<ASize do begin BDataS:=BDataS+#$FF; inc(BDataIdx); end;
 SendDataOpti($101,BDataS);
End;

Function TMsProcess.ReadFlashGenR ( ASize : Cardinal; Out ADataS : string ) : boolean;
Var
  BDataS    : string;
Begin
 Result:=FALSE; ADataS:='';

 repeat
 BDataS:=''; SetLength(BDataS,ASize+4);
 if RecvDataOpti(BDataS)=FALSE then begin ViewAny('feCommunication error (Read block) [R:TMsProcess.ReadFlashGenR]'); break; end;
 Delete(BDataS,1,4);
 ADataS:=BDataS;
 Result:=TRUE;
 until TRUE;
End;
}

Function TMsProcess.FpgaReflashSect ( Const ASectInfo : string; Var AIsExt : boolean ) : boolean;
Var
  BSectInfo     : string;
  BAddrS,
  BSizeS        : string;
  BFilename     : string;
  BAddr,
  BSize         : Cardinal;
  BExt          : string;
  BDataBin      : string;
  BFileAge      : TDateTime;
  BBinLen,
  BBinLenWr,
  BBinLenEr     : Cardinal;
  BDataIdx      : Cardinal;
  BChecksum     : word;
  BDataVer      : string;
  BExtraBlock   : string;
  //BBlockLenS,
  //BBlockLenR    : Cardinal;
  BWriteS       : string;
  BViewIdx      : byte;
  BVerifError   : boolean;
  BPos          : Integer;
  BResultA      : Integer;
Begin
 Result:=FALSE;
 repeat
 BSectInfo:=ASectInfo;
 // First read Addr and Size
 BAddrS:=ReadParamStr(BSectInfo); if BAddrS='' then begin ViewAny('feAddress information is missing in section parameters [R:TMsProcess.FpgaReflashSect]'); break; end;
 BSizeS:=ReadParamStr(BSectInfo); if BSizeS='' then begin ViewAny('feSize information is missing in section parameters [R:TMsProcess.FpgaReflashSect]'); break; end;
 if TryStrToInt0x(BAddrS,BAddr)=FALSE then begin ViewAny('feCannot convert Address to Integer (Origin: "'+BAddrS+'") [R:TMsProcess.FpgaReflashSect]'); break; end;
 if TryStrToInt0x(BSizeS,BSize)=FALSE then begin ViewAny('feCannot convert Size to Integer (Origin: "'+BSizeS+'") [R:TMsProcess.FpgaReflashSect]'); break; end;
 // Check if this is a register and not memory
 if (BSize=1) and (BAddr<$100) then
  begin
  Result:=FpgaWriteReg(BAddr,ReadParamStr(BSectInfo));
  break;
  end;
 // Process with memory write
 BFilename:=ReadParamStr(BSectInfo); if BFilename='' then begin ViewAny('feFile information is missing in section parameters [R:TMsProcess.FpgaReflashSect]'); break; end;
 ViewAny('fiProcessing file '+BFilename+' [R:TMsProcess.FpgaReflashSect]');
 if BSize=0 then begin ViewAny('feSection size cannot be zero [R:TMsProcess.FpgaReflashSect]'); break; end;
 if (BSize and $FFFF)<>0 then begin ViewAny('feSection size must be multiple of 64K bytes [R:TMsProcess.FpgaReflashSect]'); break; end;
 if FileExists(BFilename)=FALSE then begin ViewAny('feFile "'+BFilename+'" is not found [R:TMsProcess.FpgaReflashSect]'); break; end;
 if FileAge(BFilename,BFileAge) then ViewAny('fiChange date: '+FormatDateTime('YYYY.MM.DD_HH:NN:ss',BFileAge)+' for file "'+BFilename+'" [R:TMsProcess.FpgaReflashSect]');
 BDataBin:='';
 BExt:=LowerCase(ExtractFileExt(BFilename));
 if StrInList(BExt,'.jic') then
  begin
  if ReadBinFile(BFilename,BDataBin)=FALSE then break;
  CropJicFile(BDataBin);
  InvertBytes(BDataBin);
  end
 else if BExt='.hex' then
  begin
  BResultA:=TryReadEfinixHex(BFilename,BDataBin);
  if BResultA=-1 then break
  else if BResultA=0 then
   begin
   if FBrdVers in [$36] then
   else begin ViewAny('feThis type of HEX file is supposed to be used with Efinix Trion devices and does not seem to be compatible with the board [R:TMsProcess.FpgaReflashSect]'); break; end;
   BPos:=Pos('Family: Trion',BDataBin);
   if (BPos<>0) and (BPos<$200) then
   else begin ViewAny('feThis BIN file does not have a recognised Efinix Trion header and can damage the device [R:TMsProcess.FpgaReflashSect]'); break; end;
   end
  else if ReadHexFile(BFilename,BDataBin)=FALSE then break;
  end
 else if BExt='.mcs' then
  begin
  if ReadHexFile(BFilename,BDataBin)=FALSE then break;
  InvertBytes(BDataBin);
  end
 else if BExt='.mcsi' then
  begin
  if ReadHexFile(BFilename,BDataBin)=FALSE then break;
  //InvertBytes(BDataBin);
  end
 else if StrInList(BExt,'.rpd') then
  begin
  if FBrdVers in [$05] then
  else begin ViewAny('feRPD file can only be flashed to Intel/Altera devices [R:TMsProcess.FpgaReflashSect]'); break; end;
  if ReadBinFile(BFilename,BDataBin)=FALSE then break;
  InvertBytes(BDataBin);
  end
 else if StrInList(BExt,'.bit') then
  begin
  BResultA:=TryReadEfinixHex(BFilename,BDataBin);
  if BResultA=0 then
   begin
   if FBrdVers in [$36] then
   else begin ViewAny('feThis type of HEX file is supposed to be used with Efinix Trion devices and does not seem to be compatible with the board [R:TMsProcess.FpgaReflashSect]'); break; end;
   BPos:=Pos('Family: Trion',BDataBin);
   if (BPos<>0) and (BPos<$200) then
   else begin ViewAny('feThis BIN file does not have a recognised Efinix Trion header and can damage the device [R:TMsProcess.FpgaReflashSect]'); break; end;
   end
  else
   begin // Not Efinix bit
   if FBrdVers in [$15, $25] then
   else begin ViewAny('feBIT file can only be flashed to Lattice and Xilinx/AMD devices [R:TMsProcess.FpgaReflashSect]'); break; end;
   if ReadBinFile(BFilename,BDataBin)=FALSE then break;
   if FBrdVers=$15 then
    begin
    BPos:=Pos(#$FF+#$FF+#$FF+#$BD+#$B3+#$FF+#$FF+#$FF+#$FF+#$3B,BDataBin);
    if (BPos<>0) and (BPos<$200) then
    else begin ViewAny('feThis BIT file does not have a recognised Lattice header and can damage Lattice device [R:TMsProcess.FpgaReflashSect]'); break; end;
    end
   else if FBrdVers=$25 then
    begin
    BPos:=Pos(#$FF+#$FF+#$FF+#$FF+#$AA+#$99+#$55+#$66,BDataBin);
    if (BPos<>0) and (BPos<$100) then
    else begin ViewAny('feThis BIT file does not have a recognised Xilinx/AMD header and can damage Xilinx/AMD device [R:TMsProcess.FpgaReflashSect]'); break; end;
    end;
   end; // Not Efinix bit
  end
 else if StrInList(BExt,'.bin') then
  begin
  if FBrdVers in [$36] then
  else begin ViewAny('feBIN file can only be flashed to Efinix Trion devices [R:TMsProcess.FpgaReflashSect]'); break; end;
  if ReadBinFile(BFilename,BDataBin)=FALSE then break;
  BPos:=Pos('Family: Trion',BDataBin);
  if (BPos<>0) and (BPos<$200) then
  else begin ViewAny('feThis BIN file does not have a recognised Efinix Trion header and can damage the device [R:TMsProcess.FpgaReflashSect]'); break; end;
  //InvertBytes(BDataBin);
  end
 else if StrInList(BExt,'.cgm') then
  begin
  if FBrdVers in [$46] then
  else begin ViewAny('feCGM file can only be flashed to Cologne GateMate devices [R:TMsProcess.FpgaReflashSect]'); break; end;
  if ReadBinFile(BFilename,BDataBin)=FALSE then break;
  end
 else if StrInList(BExt,'.ext') then
  begin
  AIsExt:=TRUE;
  ViewAny('fiSelect EXT flash [R:TMsProcess.FpgaReflashSect]');
  if SendB(CDbgStartupWr,$02)=FALSE then begin ViewAny('feCommunication error (Select EXT flash) [R:TMsProcess.FpgaReflashSect]'); break; end;
  if ProbeFlash=FALSE then break;
  BResultA:=TryReadEfinixHex(BFilename,BDataBin);
  if BResultA<>0 then
   begin
   if ReadBinFile(BFilename,BDataBin)=FALSE then break;
   end;
  end
 else
  begin
  if ReadBinFile(BFilename,BDataBin)=FALSE then break;
  //ViewAny('feUnknown file extension "'+BFilename+'". Do not know what to do with it [R:TMsProcess.FpgaReflashSect]');
  break;
  end;
 BBinLen:=Length(BDataBin);
 if BSize<(BBinLen+8) then
  begin
  ViewAny('feData obtained from the file "'+BFilename+'" will not fit to the section [R:TMsProcess.FpgaReflashSect]');
  break;
  end;
 BBinLenWr:=(BBinLen+$FF) and $FFFFFF00;
 BBinLenEr:=(BBinLen+$FFFF) and $FFFF0000;
 SetLength(BDataBin,BBinLenWr);
 BDataIdx:=BBinLen; while BDataIdx<BBinLenWr do begin BDataBin[1+BDataIdx]:=#$FF; inc(BDataIdx); end;
 BChecksum:=0;
 BDataIdx:=0; while BDataIdx<BBinLen do begin BChecksum:=BChecksum+Ord(BDataBin[1+BDataIdx]); inc(BDataIdx); end;

 BExtraBlock:='';
 if BBinLenWr<>BSize then
  begin
  SetLength(BExtraBlock,256);
  for BDataIdx:=0 to 255 do BExtraBlock[1+BDataIdx]:=#$FF;
  AddSizeChsOpti(BExtraBlock,BBinLen,BCheckSum);
  end
 else
  begin
  AddSizeChsOpti(BDataBin,BBinLen,BChecksum);
  end;

 // Erasing flash
 ViewAny('fiErasing flash [R:TMsProcess.FpgaReflashSect]');
 ViewAny('p0 '+CColorFLS+' Erasing flash');
 BDataIdx:=0;
 while BDataIdx<BBinLenEr do
  begin
  if EraseFlashBlockGen(BAddr+BDataIdx)=FALSE then break;
  ViewAny('p'+FloatToStr(BDataIdx/BBinLenEr)+' '+CColorFLS+' Erasing flash');
  inc(BDataIdx,$10000);
  end;
 if BDataIdx<BBinLenEr then break;
 if (BSize-BBinLenEr)>$10000 then
  begin
  if EraseFlashBlockGen(BAddr+BSize-$10000)=FALSE then break;
  end;
 ViewAny('p1 '+CColorFLS+' Erasing flash');
 ViewAny('p0');

 // Writing flash
 ViewAny('fiWriting flash [R:TMsProcess.FpgaReflashSect]');
 ViewAny('p0 '+CColorFLS+' Writing flash');
 BDataIdx:=0; BViewIdx:=0;
 while BDataIdx<BBinLenWr do
  begin
  BWriteS:=Copy(BDataBin,1+BDataIdx,256);
  if WriteFlashPageGen(BAddr+BDataIdx,BWriteS)=FALSE then break;
  if (BViewIdx and $0F)=0 then ViewAny('p'+FloatToStr(BDataIdx/BBinLenWr)+' '+CColorFLS+' Writing flash');
  //Sleep(10);
  inc(BDataIdx,$100); inc(BViewIdx);
  end;
 if BDataIdx<BBinLenWr then break;
 if ReadFlashFlagGen=FALSE then begin ViewAny('feFlash writing error (ready flag reading error) [R:TMsProcess.FpgaReflashSect]'); break; end;
 if BExtraBlock<>'' then
  begin
  if WriteFlashPageGen(BAddr+BSize-256,BExtraBlock)=FALSE then break;
  if ReadFlashFlagGen=FALSE then begin ViewAny('feFlash writing error (ready flag reading error) [R:TMsProcess.FpgaReflashSect]'); break; end;
  end;
 ViewAny('p1 '+CColorFLS+' Writing flash');
 ViewAny('p0');

 // Verifying flash
 BVerifError:=FALSE;
 //BBlockLenS:=CVerifBlockLen;
 //BBlockLenR:=CVerifBlockLen;
 ViewAny('fiVerifying flash [R:TMsProcess.FpgaReflashSect]');
 ViewAny('p0 '+CColorFLS+' Verifying flash');
 BDataIdx:=0; BViewIdx:=0;
 while BDataIdx<BBinLenWr do
  begin
  if ReadFlashGen(BAddr+BDataIdx,$100,BDataVer)=FALSE then break;
  if (BVerifError=FALSE) and (BDataVer<>Copy(BDataBin,1+BDataIdx,$100)) then
   begin
   ViewAny('feDevice verification error at address '+IntToHex(BDataIdx,6)+' [R:TMsProcess.FpgaReflashSect]');
   ViewAny('t'+StrBinToHex(Copy(BDataBin,1+BDataIdx,$100)));
   ViewAny('t'+StrBinToHex(BDataVer));
   ReadFlashGen(BAddr+BDataIdx,$100,BDataVer);
   //BVerifError:=TRUE;
   end;
  if (BViewIdx and $0F)=0 then ViewAny('p'+FloatToStr(BDataIdx/BBinLenWr)+' '+CColorFLS+' Verifying flash');
  inc(BDataIdx,$100); inc(BViewIdx);
  end;
 if (BDataIdx<BBinLenWr) or BVerifError then break;
 if BExtraBlock<>'' then
  begin
  if ReadFlashGen(BAddr+BSize-256,256,BDataVer)=FALSE then break;
  if BDataVer<>BExtraBlock then begin ViewAny('feDevice verification error (Extra block with size and checksum) [R:TMsProcess.FpgaReflashSect]'); break; end;
  end;
 ViewAny('p1 '+CColorFLS+' Verifying flash');
 ViewAny('p0');

 ViewAny('fiFile processed successfully [R:TMsProcess.FpgaReflashSect]');
 Result:=TRUE;
 until TRUE;
 ViewAny('p0');
 ViewAny('f-');
End;

Function TMsProcess.FpgaReadSect ( Const ASectInfo : string ) : boolean;
Var
  BSectInfo     : string;
  BAddrS,
  BSizeS        : string;
  BFilename     : string;
  BAddr,
  BSize         : Cardinal;
  BBinLen,
  BBinLenWr     : Cardinal;
  BDataIdx      : Cardinal;
  BDataRd       : string;
  BBlockLen     : Cardinal;
  BStream       : TMemoryStream;
Begin
 BStream:=TMemoryStream.Create;

 Result:=FALSE;
 repeat
 BSectInfo:=ASectInfo;
 BAddrS:=ReadParamStr(BSectInfo); if BAddrS='' then begin ViewAny('feAddress information is missing in section parameters [R:TMsProcess.FpgaReadSect]'); break; end;
 BSizeS:=ReadParamStr(BSectInfo); if BSizeS='' then begin ViewAny('feSize information is missing in section parameters [R:TMsProcess.FpgaReadSect]'); break; end;
 BFilename:=ReadParamStr(BSectInfo); if BFilename='' then begin ViewAny('feFile information is missing in section parameters [R:TMsProcess.FpgaReadSect]'); break; end;
 ViewAny('fiProcessing file '+BFilename+' [R:TMsProcess.FpgaReadSect]');
 if TryStrToInt0x(BAddrS,BAddr)=FALSE then begin ViewAny('feCannot convert Address to Integer (Origin: "'+BAddrS+'") [R:TMsProcess.FpgaReadSect]'); break; end;
 if TryStrToInt0x(BSizeS,BSize)=FALSE then begin ViewAny('feCannot convert Size to Integer (Origin: "'+BSizeS+'") [R:TMsProcess.FpgaReadSect]'); break; end;
 if BSize=0 then begin ViewAny('feSection size cannot be zero [R:TMsProcess.FpgaReadSect]'); break; end;
 if (BSize and $FFFF)<>0 then begin ViewAny('feSection size must be multiple of 64K bytes [R:TMsProcess.FpgaReadSect]'); break; end;
 BBinLen:=BSize;
 BBinLenWr:=(BBinLen+$FF) and $FFFFFF00;

 // Reading flash
 BBlockLen:=CVerifBlockLen;
 ViewAny('fiReading flash [R:TMsProcess.FpgaReadSect]');
 ViewAny('p0 '+CColorFLS+' Reading flash');
 BDataIdx:=0;
 while BDataIdx<BBinLenWr do
  begin
  if BBlockLen>(BBinLenWr-BDataIdx) then BBlockLen:=256;
  if ReadFlashGen(BAddr+BDataIdx,BBlockLen,BDataRd)=FALSE then break;
  InvertBytes(BDataRd);
  BStream.Write(BDataRd[1],BBlockLen);
  if (BDataIdx and $0F)=0 then ViewAny('p'+FloatToStr(BDataIdx/BBinLenWr)+' '+CColorFLS+' Reading flash');
  inc(BDataIdx,BBlockLen);
  end;
 ViewAny('p1 '+CColorFLS+' Reading flash');
 ViewAny('p0');

 try
   BStream.SaveToFile(BFilename);
 except
   ViewAny('fiError writing file '+BFilename+' [R:TMsProcess.FpgaReadSect]');
   break;
 end;

 ViewAny('fiFile processed successfully [R:TMsProcess.FpgaReadSect]');
 Result:=TRUE;
 until TRUE;
 ViewAny('p0');
 ViewAny('f-');

 BStream.Free;
End;

Function TMsProcess.FpgaWriteReg ( AAddr : Cardinal; Const ADataS : string ) : boolean;
Var
  BData     : Cardinal;
  BDataBin      : string;
  BRepeat       : Integer;
  BResult       : boolean;
  BRegData      : byte;
Begin
 Result:=FALSE;
 repeat
 if TryStrToInt0x(ADataS,BData)=FALSE then begin ViewAny('feCannot convert register Data to Integer (Origin: "'+ADataS+'") [R:TMsProcess.FpgaWriteReg]'); break; end;

 // Write enable
 BDataBin:=#$06;
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Write register) [R:TMsProcess.FpgaWriteReg]'); break; end;

 // Program register
 BDataBin:=
   Chr(AAddr and $FF)+
   Chr(BData and $FF);
 if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Write register) [R:TMsProcess.FpgaWriteReg]'); break; end;

 // Read flag
 BRepeat:=1000;
 BResult:=FALSE;
 while BRepeat>0 do
  begin
  BDataBin:=#$05+#$FF;
  if SendRecvSpi(BDataBin)=FALSE then begin ViewAny('feCommunication error (Write register) [R:TMsProcess.FpgaWriteReg]'); break; end;
  BRegData:=Ord(BDataBin[2]);
  if (BRegData and $01)=0 then begin BResult:=TRUE; break; end;
  Sleep(10);
  dec(BRepeat);
  end;
 if BResult=FALSE then begin ViewAny('feWrite register time out [R:TMsProcess.FpgaWriteReg]'); break; end;

 ViewAny('fiWrite register successful. Addr = 0x'+IntToHex(AAddr,4)+' Data = 0x'+IntToHex(BData,2)+' [R:TMsProcess.FpgaWriteReg]');
 Result:=TRUE;
 until TRUE;
End;

Function TMsProcess.DasmMissing ( AIpPrev, AIpThis : Cardinal ) : boolean;
Var
  BIsMissing    : boolean;
Begin
 Result:=FALSE;
 repeat
 if FBuild.DasmMissing(AIpPrev,AIpThis,BIsMissing)=FALSE then break;
 Result:=TRUE;
 if BIsMissing then ViewAny('D'+#13+FBuild.Lst.Text);
 until TRUE;
End;

Function TMsProcess.TextDbg : TStringList;
Begin
 if FBuild=nil then Result:=nil
 else Result:=FBuild.Dbg;
End;

Procedure TMsProcess.FlashLogS ( Const ADataS : string );
Begin
 if FFlashLogKeep then FFlashLog.Append(StrBinToHex(ADataS)+' ->');
End;

Procedure TMsProcess.FlashLogR ( Const ADataS : string );
Begin
 if FFlashLogKeep then FFlashLog.Append(' <- '+StrBinToHex(ADataS));
End;

{
10800000B88000001E8100003E8100000103000001849C10086394100000010301000185F600010304000185F500026300100000016300040F0021960463A01000000163FC80000000E146000463E010000001630E81000000E13E0004632011000001631881000000E136000463B812000001635681000000E12E000163001000000923000084EF01130000018DF4000113AA55018DF4000389F400010355000185F4000103010001849C1001809C1010010300C5DF81EF01230A0061EBF5DF01809C1010010100C5DF0103030001849C1081EF81978397859703031000455201230000519643EBE5DF819008004194000024968592839284928AEF2DEB2DEB
10800000B88000001E8100003E8100000103000001849C10086394100000010301000185F600010304000185F500026300100000016300040F0021960463A01000000163FC80000000E146000463E010000001630E81000000E13E0004632011000001631881000000E136000463B812000001635681000000E12E000163001000000923000084EF01130000018DF4000113AA55018DF4000389F400010355000185F4000103010001849C1001809C1010        DF81EF01230A0061EBF5DF01809C1010010100C5DF0103030001849C1081EF81978397859703031000455201230000519643EBE5DF819008004194000024968592839284928AEF2DEB2DEB2DEB2DEB

10800000B88000001E8100003E8100000103000001849C10086394100000010301000185F600010304000185F500026300100000016300040F0021960463A01000000163FC80000000E146000463E010000001630E81000000E13E0004632011000001631881000000E136000463B812000001635681000000E12E000163001000000923000084EF01130000018DF4000113AA55018DF4000389F400010355000185F4000103010001849C1001809C1010010300C5DF81EF01230A0061EBF5DF01809C1010010100C5DF0103030001849C1081EF81978397859703031000455201230000519643EBE5DF819008004194000024968592839284928AEF2DEB2DEB
10800000B88000001E8100003E8100000103000001849C10086394100000010301000185F600010304000185F500026300100000016300040F0021960463A01000000163FC80000000E146000463E010000001630E81000000E13E0004632011000001631881000000E136000463B812000001635681000000E12E000163001000000923000084EF01130000018DF4000113AA55018DF4000389F400010355000185F4000103010001849C1001809C1010        DF81EF01230A0061EBF5DF01809C1010010100C5DF0103030001849C1081EF81978397859703031000455201230000519643EBE5DF819008004194000024968592839284928AEF2DEB2DEB2DEB2DEB

}

end.


