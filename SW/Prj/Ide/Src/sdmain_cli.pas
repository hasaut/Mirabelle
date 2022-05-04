unit SdMain_cli;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MgCmdLine, ChangeLog_sd, MsProcess, AsmTypes_sd,
  Keyboard, MemSeg_sd, DataFiles, ProcModel_sd, MemSegHelper_sd;

Type
  TSdMain = class(TObject)
  private
    FPrjParams  : TStringList;
    FMsState    : Cardinal;
    FMsProcess  : TMsProcess;
    FProcAnyEvt : PRtlEvent;
    FPathThis   : string;
    FPrjPath,
    FPrjName    : string;

    FLogList    : TStringList;
    FOutHexFile : TStringList;
    FOutTty     : TStringList;

    FCmdParams  : TCmdParams;

    FReadlnLock : TRtlCriticalSection;
    FWriteLock  : TRtlCriticalSection;
    FWritelnBuf : string;
    FReadlnList : TStringList;
    FReadlnBuf  : string;
    FWriteList,
    FWriteListA : TStringList;
    FWriteNeeded: boolean;
    FSignal     : Cardinal;

    FProgressName   : string;
    FProgressPos    : Integer;

    FExecState      : char;
    FCoreList       : string;
    FErrorCount     : Integer;

    Procedure WriteA ( Const AStr : string );
    Procedure WritelnA ( Const AStr : string );
    Procedure WritelnB ( Const AStr : string );

    Procedure WriteHelp;
    Procedure WriteVersion;
    Procedure AppendReadln;
    Function InvokeReadln : string;
    Procedure PollKbd ( Var AAction : boolean );
    Procedure ViewAny ( Const AMessage : string );
    Procedure ViewAnyM ( Var AAction : boolean );
    Procedure MbProgress ( Const AData : string );

    Procedure ExecCmd ( Const ACmd : string );
    Procedure RecvMemSegData ( Const ADataR : string );
    Procedure LogAll;

    Procedure ProcessConsole ( Const ACmdList : string; Var AExitAll : boolean );
    Function PaintRegsSD ( Const ARegsThis : string ) : string;
    Function PaintRegsRV ( Const ARegsThis : string ) : string;
    Procedure ReportCpuRegs ( Const ARecvS : string );
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function Process : Integer;
    Procedure ProcessSignal ( ASignal : Cardinal );
  end;

Const
  CPrjName  = 'Mirabelle Debugger';

implementation

Uses
  ConComL, ConComS;

Constructor TSdMain.Create;
Begin
 Inherited;
 InitCriticalSection(FReadlnLock);
 InitCriticalSection(FWriteLock);
 FProcAnyEvt:=RtlEventCreate;
 FPrjParams:=TStringList.Create;
 FCmdParams:=TCmdParams.Create;
 FReadlnList:=TStringList.Create;
 FWriteList:=TStringList.Create;
 FWriteListA:=TStringList.Create;
 FLogList:=TStringList.Create;
 FOutHexFile:=TStringList.Create;
 FOutTty:=TStringList.Create;
 FReadlnBuf:='';
 FSignal:=0;
 FProgressName:=''; FProgressPos:=0;
End;

Destructor TSdMain.Destroy;
Begin
 FOutTty.Free;
 FOutHexFile.Free;
 FLogList.Free;
 FWriteListA.Free;
 FWriteList.Free;
 FReadlnList.Free;
 FCmdParams.Free;
 FPrjParams.Free;
 RtlEventDestroy(FProcAnyEvt);
 DoneCriticalSection(FWriteLock);
 DoneCriticalSection(FReadlnLock);
 Inherited;
End;

Procedure TSdMain.WriteA ( Const AStr : string );
Begin
 FWritelnBuf:=FWritelnBuf+AStr;
 if FCmdParams.IsSilent=FALSE then Write(AStr);
End;

Procedure TSdMain.WritelnA ( Const AStr : string );
Begin
 FWritelnBuf:=FWritelnBuf+AStr;
 FLogList.Append(FWritelnBuf);
 FWritelnBuf:='';
 if FCmdParams.IsSilent=FALSE then Writeln(AStr);
End;

Procedure TSdMain.WritelnB ( Const AStr : string );
Var
  BStr  : string;
  BCmd  : Char;
Begin
 BStr:=AStr;
 repeat
 if BStr='' then break;
 BCmd:=BStr[1]; Delete(BStr,1,1);
 WritelnA('# '+BCmd+' '+BStr+CForceEol);
 until TRUE;
End;

Procedure TSdMain.WriteHelp;
Var
  BDateS,
  BTimeS    : string;
Begin
 WritelnA(CPrjName);
 BDateS:={$I %DATE%}; BTimeS:={$I %TIME%};
 WritelnA('Build date: '+BDateS+' '+BTimeS);
 WritelnA('');

 WritelnA('Usage md_cli [parameters]');
 WritelnA('Parameter list is case insensitive.');
 WritelnA('File names and path can be case-sensitive in some operating systems');
 WritelnA('Parameter list:');
 WritelnA('-h or -help or --help = Write this message');
 WritelnA('--version = Write version and change log');
 WritelnA('-s = silent mode');
 WritelnA('--projectfile <path/name> = project file (mandatory parameter)');
 WritelnA('--hexf <path/name> = additional HEX file');
 WritelnA('--gdbf <path/name> = GDB index');
 WritelnA('--flash <addr> <size> <path/name> = FPGA flash section');
 WritelnA('--startup <run/step/exit/stln> = startup mode (use "exit" together with "--flash" option if only FPGA reflash is needed)');
End;

Procedure TSdMain.WriteVersion;
Var
  BDateS,
  BTimeS    : string;
  BReadS,
  BParamS   : string;
Begin
 WritelnA(CPrjName);
 BDateS:={$I %DATE%}; BTimeS:={$I %TIME%};
 WritelnA('Build date: '+BDateS+' '+BTimeS);

 BReadS:=CChangeLog;
 repeat
 BParamS:=ReadTillC(BReadS,#13);
 if BParamS='' then break;
 WritelnA(BParamS);
 until FALSE;

End;

Procedure TSdMain.ProcessSignal ( ASignal : Cardinal );
Begin
 FSignal:=ASignal;
End;

Procedure TSdMain.AppendReadln;
Begin
 repeat
 if FReadlnBuf='' then break;
 EnterCriticalSection(FReadlnLock);
 FReadlnList.Append(FReadlnBuf);
 FReadlnBuf:='';
 LeaveCriticalSection(FReadlnLock);
 until TRUE;
End;

Function TSdMain.InvokeReadln : string;
Begin
 Result:='';
 EnterCriticalSection(FReadlnLock);
 if FReadlnList.Count<>0 then
  begin
  Result:=FReadlnList.Strings[0];
  FReadlnList.Delete(0);
  end;
 LeaveCriticalSection(FReadlnLock);
End;

Procedure TSdMain.PollKbd ( Var AAction : boolean );
Var
  BKey      : TKeyEvent;
  BKeyCh    : char;
Begin
 repeat
 BKey:=0;
 BKey:=PollKeyEvent;
 if BKey=0 then break;
 BKey:=GetKeyEvent; BKeyCh:=#0;
 AAction:=TRUE;
 BKey:=TranslateKeyEvent(BKey);
 case GetKeyEventFlags(BKey) of
   kbASCII:
     begin
     BKeyCh:=GetKeyEventChar(BKey);
     end;
   kbUniCode:
     begin
     end;
   kbFnKey:
     begin
     end;
   kbPhys:
     begin
     end;
 end;

 case BKeyCh of
  #27,
   #3: begin
       FReadlnBuf:=BKeyCh; AppendReadln;
       end;
  #32..#126:
       begin
       WriteA(BKeyCh);
       FReadlnBuf:=FReadlnBuf+BKeyCh;
       end;
  #13: begin
       AppendReadln;
       WritelnA('');
       end;
  else AppendReadln;
 end;
 until FALSE;
End;

Const
  CCmdLineItems = 'SrcF IncF OutF LnkF';

Function TSdMain.Process : Integer;
Var
  BParamList    : TStringList;
  BDummyName    : string;
  BPrjFilename  : string;
  BKbdCloseReq  : boolean;
  BSrcItems     : string;
  BDebugActions : string;
  BWrPrompt,
  BOutLog       : boolean;
  BAction,
  BExitAll      : boolean;
  BCmdList      : string;
  BTickStart,
  BTickThis     : QWord;
  BPlayerS      : string;
Begin
 Result:=-1;
 BKbdCloseReq:=FALSE;

 FMsState:=$0000;
 FMsProcess:=TMsProcess.Create(TRUE); FMsProcess.OnViewAny:=@ViewAny; FMsProcess.Start;

 BAction:=FALSE;
 while FMsState=0 do begin ViewAnyM(BAction); Sleep(10); end;
 FMsState:=$0000;

 FPathThis:=IncludeTrailingPathDelimiter(GetCurrentDir);
 FPrjPath:=''; FPrjName:='';
 BParamList:=TStringList.Create;

 repeat
 if FCmdParams.Parse(CCmdLineItems)=FALSE then begin WritelnA(FCmdParams.LastError); break; end;

 if FCmdParams.IsSilent then
 else begin InitKeyboard; BKbdCloseReq:=TRUE; end;

 WritelnA('MD command line debugger');

 if FCmdParams.WrHelp then begin WriteHelp; break; end;
 if FCmdParams.WrVersion then WriteVersion;

 if FCmdParams.PrjFilename='' then begin WritelnA('Project file is not specified'); break; end;
 BDummyName:=FCmdParams.PrjFilename; BPrjFilename:=ExpandFilename(BDummyName);
 WritelnA('Project '+BPrjFilename);
 if FileExists(BPrjFilename)=FALSE then begin WritelnA('Project file does not exist'); break; end;

 try
   FPrjParams.LoadFromFile(BPrjFilename);
 except
   WritelnA('Cannot read file '+BPrjFilename);
   break;
 end;

 FCmdParams.ExpandPrjParams(FPrjParams);

 FCoreList:=FPrjParams.Values['CpuF'];
 FOutTty.Clear;

 BSrcItems:=BuildParamsToStr(BPrjFilename,FPrjParams,BPlayerS);
 if BPlayerS='' then begin WritelnA('Player is not specified in the project file'); break; end;
 FMsProcess.AppendCmd('y'+BPlayerS);
 // Wait connection to be established
 BAction:=FALSE;
 while (FMsState and $0070)=0 do begin ViewAnyM(BAction); Sleep(10); end;
 if (FMsState and $0010)<>0 then begin WritelnA('FPGA connection error'); break; end;

 if FErrorCount<>0 then break;

 if FCmdParams.FlashList<>'' then
  begin
  if (FMsState and $0020)=0 then begin WritelnA('Cannot reflash FPGA in simulator mode'); break; end;
  FMsProcess.AppendCmd('Fw'+FCmdParams.FlashList);
  BAction:=FALSE; while FMsProcess.HasPendingCmd do begin ViewAnyM(BAction); Sleep(50); end;
  if FErrorCount<>0 then break;
  FMsProcess.AppendCmd('Fn');
  BAction:=FALSE; while FMsProcess.HasPendingCmd do begin ViewAnyM(BAction); Sleep(50); end;
  if FErrorCount<>0 then break;
  // Wait connection to be reestablished after the reaset
  FMsState:=$0000;
  FMsProcess.AppendCmd('y'+BPlayerS);
  BAction:=FALSE;
  while (FMsState and $0070)=0 do begin ViewAnyM(BAction); Sleep(10); end;
  if (FMsState and $0010)<>0 then begin WritelnA('FPGA connection error'); break; end;
  if FErrorCount<>0 then break;
  end;

 FMsState:=$0000;
 BDebugActions:='';
 if FCmdParams.StartupMode in [smRun, smStln] then begin BDebugActions:='RIMRnib.S'; FExecState:='e'; end
 else if FCmdParams.StartupMode=smStep then begin BDebugActions:='RIMRnib.'; FExecState:='s'; end;
 if BDebugActions<>'' then
  begin
  FMsProcess.AppendCmd('B'+BDebugActions+#13+BSrcItems);
  BAction:=FALSE;
  while (FMsState and $0001)=0 do begin ViewAnyM(BAction); Sleep(10); end;
  if (FMsState and $0004)=0 then begin WritelnA('Terminated by error'); break; end;
  end;

 if (FCmdParams.StartupMode=smRun) and (FCmdParams.MaxTime=0) then begin WritelnA('In continuous run mode, MaxTime is mandatory'); break; end;

 BTickStart:=GetTickCount64;

 BWrPrompt:=TRUE; BOutLog:=FALSE; BExitAll:=FALSE;

 if FCmdParams.StartupMode in [smExit, smStln] then
  begin
  BAction:=FALSE;
  while FMsProcess.HasPendingCmd do
   begin
   ViewAnyM(BAction);
   Sleep(50);
   end;
  BExitAll:=TRUE;
  end;

 while BExitAll=FALSE do
  begin
  BAction:=FALSE;
  if FSignal<>0 then
   begin
   FSignal:=0;
   WritelnB('iTerminated');
   BExitAll:=TRUE;
   end;

  if BWrPrompt then
   begin
   WritelnA('');
   if FCmdParams.StartupMode=smRun then WritelnA('** Continuous run mode (type "q" to exit, "pause" to interactive mode)')
   else WritelnA('** Running in interactive mode (type "q" to exit)');
   WritelnA('');
   //WriteA('>');
   BWrPrompt:=FALSE;
   end;
  PollKbd(BAction);
  BCmdList:=InvokeReadln;
  if BCmdList<>'' then
   begin
   ProcessConsole(BCmdList,BExitAll);
   //if BExitAll=FALSE then WriteA('>');
   end;
  if BExitAll then break;
  ViewAnyM(BAction);
  if FCmdParams.StartupMode=smRun then
   begin
   BTickThis:=GetTickCount64-BTickStart;
   if ((FMsState and $0008)<>0) or (BTickThis>=(FCmdParams.MaxTime*1000)) then
    begin
    WritelnB('iExecuted in '+FormatDateTime('HH:NN:SS.zzz',BTickThis/1000));
    WritelnB('iTerminated, output files will be created');
    BOutLog:=TRUE;
    break;
    end;
   end;
  if BAction=FALSE then RtlEventWaitFor(FProcAnyEvt,100);
  end;

 if BOutLog then LogAll;
 if FCmdParams.OutTty<>'' then
  begin
  try
    FOutTty.SaveToFile(FCmdParams.OutTty);
  except
    WritelnB('eCannot save resulting TTY file '+FCmdParams.OutTty);
  end;
  end;

 ViewAnyM(BAction);

 Result:=0;
 until TRUE;

 if BKbdCloseReq then DoneKeyboard;

 FMsProcess.Terminate; BAction:=FALSE;
 while FMsProcess.IsEnded=FALSE do
  begin
  ViewAnyM(BAction);
  Sleep(10);
  end;
 FMsProcess.Free; FMsProcess:=nil;
 BParamList.Free;
End;

Procedure TSdMain.ExecCmd ( Const ACmd : string );
Begin
 if FMsProcess<>nil then FMsProcess.AppendCmd(ACmd);
End;

Procedure TSdMain.ProcessConsole ( Const ACmdList : string; Var AExitAll : boolean );
Var
  BCmdList  : string;
  BCmd,
  BCmdL     : string;
Begin
 BCmdList:=ACmdList;

 repeat
 BCmd:=ReadParamStr(BCmdList);
 if BCmd='' then break;
 BCmdL:=LowerCase(BCmd);
 if BCmdL=#3 then
  begin
  FExecState:='s';
  WritelnA('Terminated. Output files will be created if possible');
  AExitAll:=TRUE;
  break;
  end;
 if BCmdL=#27 then
  begin
  WritelnA('Type "q" to exit');
  break;
  end;
 if BCmdL='q' then
  begin
  WritelnA('Execution ended. Output files will be created');
  AExitAll:=TRUE;
  break;
  end;
 if BCmdL='s' then
  begin
  FExecState:='s';
  ExecCmd('7');
  end
 else if BCmdL='step_into' then
  begin
  FExecState:='s';
  ExecCmd('7');
  end
 else if BCmdL='step_over' then
  begin
  FExecState:='e';
  ExecCmd('8');
  end
 else if BCmdL='pause' then
  begin
  FExecState:='s';
  ExecCmd('Ti');
  end
 else if BCmdL='reset' then
  begin
  FExecState:='s';
  ExecCmd('RIni');
  end
 else if BCmdL='run' then
  begin
  FExecState:='e';
  ExecCmd('S');
  end
 else if BCmdL='set_brkp' then
  begin
  ExecCmd('b '+BCmdList+'.');
  WritelnA('OK');
  BCmdList:='';
  end
 else WritelnA('Unknown command: '+BCmd);
 until FALSE;
End;

Procedure TSdMain.MbProgress ( Const AData : string );
Var
  BDataR    : string;
  BPrName   : string;
  BPrPosS   : string;
  BPrPosF   : Double;
  BPrPosI   : Integer;
Begin
 BDataR:=AData;
 repeat
 BPrPosS:=ReadParamStr(BDataR); DelFirstSpace(BDataR);
 BPrName:=BDataR;
 if BPrName='' then
  begin
  WritelnA(''+CForceEol);
  FProgressName:='';
  FProgressPos:=0;
  break;
  end;
 if BPrName<>FProgressName then
  begin
  FProgressName:=BPrName;
  WriteA('# p '+FProgressName+': ');
  FProgressPos:=0;
  end;
 TryStrToFloat(BPrPosS,BPrPosF,HParsFormat);
 BPrPosI:=Round(BPrPosF*20);
 EnterCriticalSection(FWriteLock);
 while FProgressPos<BPrPosI do
  begin
  WriteA('.');
  inc(FProgressPos);
  end;
 LeaveCriticalSection(FWriteLock);
 until TRUE;
End;

Procedure TSdMain.ViewAny ( Const AMessage : string );
Begin
 repeat
 EnterCriticalSection(FWriteLock);
 FWriteList.Append(AMessage);
 FWriteNeeded:=TRUE;
 LeaveCriticalSection(FWriteLock);
 RtlEventSetEvent(FProcAnyEvt);
 until TRUE;
End;

Function RemoveCR ( Const AReadS : string ) : string;
Var
  BPos  : Integer;
Begin
 Result:=AReadS;
 repeat
 BPos:=Pos(#13,Result);
 if BPos=0 then break;
 Result[BPos]:=' ';
 until FALSE;
End;

Procedure TSdMain.ViewAnyM ( Var AAction : boolean );
Var
  BCmd      : char;
  BDataR    : string;
  BDataI    : Integer;
  BActive   : string;
Begin
 repeat
 if FWriteNeeded=FALSE then break;
 EnterCriticalSection(FWriteLock);
 FWriteNeeded:=FALSE;
 FWriteListA.Assign(FWriteList);
 FWriteList.Clear;
 LeaveCriticalSection(FWriteLock);

 while FWriteListA.Count<>0 do
  begin
  AAction:=TRUE;
  BDataR:=FWriteListA.Strings[0]; FWriteListA.Delete(0);
  repeat
  if BDataR='' then break;
  BCmd:=BDataR[1]; Delete(BDataR,1,1);
  case BCmd of
    's': begin
         Delete(BDataR,1,1);
         WritelnA('# '+BCmd+' '+RemoveCR(BDataR)+CForceEol);
         end;
    'i',
    'w': WritelnA('# '+BCmd+' '+BDataR+CForceEol);
    'e': begin inc(FErrorCount); WritelnA('# '+BCmd+' '+BDataR+CForceEol); end;
    'p': MbProgress(BDataR);
    'r': begin
         WritelnA('');
         BActive:=ReadTillC(BDataR,#13);
         if BActive='1' then FExecState:='e'
         else FExecState:='s';
         ReportCpuRegs(BDataR);
         //WriteA('>');
         end;
    'u': begin
         if TryStrToInt(BDataR,BDataI) then FMsState:=FMsState or (1 shl BDataI);
         end;
    'g': RecvMemSegData(BDataR);
    't': begin
         FOutTty.Append(BDataR);
         end;
  end;
  until TRUE;
  end;

 until TRUE;
End;

// *** SD ***

Const                  // from iq to gq
  COffsetRegG : array [0..7] of integer = (0, 16, 32, 48, 64, 80, 96, 112);
  CLabelRegG  : array [0..7] of string = ('QIP', 'QAX', 'QBX', 'QCX', 'QDX', 'QEX', 'QFX', 'QGX');

Function TSdMain.PaintRegsSD ( Const ARegsThis : string ) : string;
Var
  BRegThis      : string;
  BIndex        : Integer;
Begin
 Result:='';
 for BIndex:=0 to 7 do
  begin
  BRegThis:=Copy(ARegsThis,1+COffsetRegG[BIndex],16);
  if BIndex=0 then
   begin
   Result:='EIP:'+Copy(BRegThis,1+8+2,6);
   Result:=Result+' ESP:'+Copy(BRegThis,1,8);
   end
  else Result:=Result+' '+CLabelRegG[BIndex]+':'+BRegThis;
  end;
 Result:=Result+' M:'+Copy(ARegsThis,1+8,2);
End;

// *** Risc-V ***

Const                  // from x1 to eip
  COffsetRegRV : array [0..15] of integer = (8, 24, 40, 56, 72, 88, 104, 120, 0, 16, 32, 48, 64, 80, 96, 112);
  //CLabelRegRV  : array [0..15] of string = ('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x14', 'x15', 'eip');
  CLabelRegRV  : array [0..15] of string = ('PC', 'ra', 'sp', 'gp', 'tp', 't0', 't1', 't2', 's0', 's1', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5');

Function TSdMain.PaintRegsRV ( Const ARegsThis : string ) : string;
Var
  BIndex        : Integer;
Begin
 Result:='';
 for BIndex:=0 to 15 do
  begin
  if BIndex=0 then Result:=CLabelRegRV[BIndex]+':'+Copy(ARegsThis,1+COffsetRegRV[BIndex]+2,6)
  else Result:=Result+' '+CLabelRegRV[BIndex]+':'+Copy(ARegsThis,1+COffsetRegRV[BIndex],8);
  end;
 Result:=Result+' M:'+Copy(ARegsThis,1+8,2);
End;

Procedure TSdMain.ReportCpuRegs ( Const ARecvS : string );
Var
  BRecvS    : string;
  BCoreList : string;
  BRegsThis : string;
  BRegsA    : string;
  BWriteS   : string;
  BCoreIdx  : Integer;
Begin
 BCoreList:=FCoreList;
 BRecvS:=ARecvS;
 ReadTillC(BRecvS,#13);
 BRegsThis:=ReadTillC(BRecvS,#13);
 BCoreIdx:=0;
 while BCoreList<>'' do
  begin
  BRegsA:=ReadParamStr(BRegsThis);
  BWriteS:='Core['+IntToStr(BCoreIdx)+']_'+BCoreList[1]+' ';
  case BCoreList[1] of
    'r': BWriteS:=BWriteS+PaintRegsRV(BRegsA);
    's': BWriteS:=BWriteS+PaintRegsSD(BRegsA);
  end;
  WritelnA(BWriteS);
  inc(BCoreIdx);
  Delete(BCoreList,1,1);
  end;
End;

Procedure TSdMain.RecvMemSegData ( Const ADataR : string );
Var
  BDataR    : string;
  BParamA   : string;
  BBaseAddr : Cardinal;
  BDataBin  : TMsBinPtr;
Begin
 BDataR:=ADataR;
 repeat
 BParamA:=ReadParamStr(BDataR); if BParamA='' then break; // Seg name, just skip
 BParamA:=ReadParamStr(BDataR); if BParamA='' then break;
 if HexToDWordCheck(BParamA,BBaseAddr)=FALSE then break;
 BParamA:=ReadParamStr(BDataR); if BParamA='' then break;
 BDataBin:=MsBinImport(BParamA);
 if BDataBin=nil then break;
 HexFileAddData(BBaseAddr,BDataBin^,FOutHexFile);
 until TRUE;
End;

Procedure TSdMain.LogAll;
Var
  BAction       : boolean;
  BSegIdx       : Integer;
  BProcModel    : TProcModel;
  BSeg          : TMemSeg;
Begin
 repeat
 BAction:=FALSE;
 FMsProcess.AppendCmd('u8');
 while (FMsState and $0100)=0 do begin RtlEventWaitFor(FProcAnyEvt,100); ViewAnyM(BAction); end;
 FMsState:=$0000;
 FOutHexFile.Clear;
 BProcModel:=FMsProcess.ProcModel;
 BProcModel.SegListOrder;
 BSegIdx:=0;
 while BSegIdx<Length(BProcModel.MemSegList) do
  begin
  BSeg:=BProcModel.MemSegList[BSegIdx];
  FMsProcess.AppendCmd('g'+BSeg.SegName);
  FMsProcess.AppendCmd('u8');
  while (FMsState and $0100)=0 do begin RtlEventWaitFor(FProcAnyEvt,100); ViewAnyM(BAction); end;
  FMsState:=$0000;
  inc(BSegIdx);
  end;
 FOutHexFile.Append(':00000001FF');

 if FCmdParams.OutMemDump='' then break;

 try
   FOutHexFile.SaveToFile(FCmdParams.OutMemDump);
 except
   WritelnB('eCannot save file '+FCmdParams.OutMemDump);
   break;
 end;

 until TRUE;
End;

end.

