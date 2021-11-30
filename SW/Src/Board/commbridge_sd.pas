unit CommBridge_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser, LCLIntf, LCLType, AsmTypes, ProcModel;

Type
  TCommState = (csNone, csClosed, csConnecting, csConnected, csActive, csSimulator);

  TCommBridge = class(TThread)
  private
    FComStream      : TBlockSerial;
    FIsEnded        : boolean;
    FCmdList,
    FCmdListA       : TStringList;
    FCmdListLock    : TRtlCriticalSection;
    FPlayerParams   : string;
    FSetPlayer      : boolean;
    FCommState      : TCommState;
    FWait           : Cardinal;
    FAttReq         : boolean;
    FHsRepeatToFail : Integer;

    FOnViewAny      : TOnViewAny;
    FProcModel      : TProcModel;

    Procedure ViewAny ( Const AMessage : string );
    Procedure CloseAll;
    Procedure OpenAny;
    Function FormatCommError ( Const AError : string ) : string;
    Function FormatCommState : string;
    Procedure SetCommState ( ANewState : TCommState );

    Function CanReadAny : boolean;
    Function RecvByteAny ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
    Function RecvByteAny ( Out AData : byte ) : boolean;
    Procedure SendByteAny ( AData : byte );
    Procedure SendStringAny ( Const AData : string );
    Function RecvByteA ( ASenseAtt : boolean; Out AData : byte ) : boolean;

    Procedure ProcessCmdList;

  protected
    Procedure Execute; Override;
  public
    Constructor Create ( CreateSuspended : boolean );
    Destructor Destroy; Override;

    Procedure AppendCmd ( Const ACmdS : string );

    property OnViewAny : TOnViewAny read FOnViewAny write FOnViewAny;
    property IsEnded : boolean read FIsEnded;
  end;

Const
  CCommStateS   : array [TCommState] of string = ('None', 'Closed', 'Connecting', 'Connected', 'Active', 'Simulator');

implementation

Uses
  ConComL;

Constructor TCommBridge.Create ( CreateSuspended : boolean );
Begin
 Inherited Create(CreateSuspended);
 InitCriticalSection(FCmdListLock);
 FCmdList:=TStringList.Create;
 FCmdListA:=TStringList.Create;
 FProcModel:=TProcModel.Create; FProcModel.OnViewAny:=@ViewAny;
 FIsEnded:=FALSE;
 FWait:=100;
End;

Destructor TCommBridge.Destroy;
Begin
 FOnViewAny:=nil;
 CloseAll;
 FProcModel.Free;
 FCmdListA.Free;
 FCmdList.Free;
 DoneCriticalSection(FCmdListLock);
 Inherited;
End;

Procedure TCommBridge.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TCommBridge.CloseAll;
Begin
 if FComStream<>nil then
  begin
  FComStream.CloseSocket;
  FComStream.Free;
  FComStream:=nil;
  end;
 SetCommState(csClosed);
End;

Procedure TCommBridge.AppendCmd ( Const ACmdS : string );
Begin
 EnterCriticalSection(FCmdListLock);
 FCmdList.Append(ACmdS);
 LeaveCriticalSection(FCmdListLock);
End;

Procedure TCommBridge.ProcessCmdList;
Var
  BLineIdx  : Integer;
  BReadS    : string;
  BCmd      : char;
Begin
 BLineIdx:=0;
 while BLineIdx<FCmdListA.Count do
  begin
  BReadS:=FCmdListA.Strings[BLineIdx];
  BCmd:=BReadS[1]; Delete(BReadS,1,1);
  case BCmd of
   'y': begin
        FPlayerParams:=BReadS;
        FSetPlayer:=TRUE;
        end;
   'x': begin
        FProcModel.SetParams(BReadS);
        ViewAny('r'+FProcModel.RegsAsStr);
        end;
  end;
  inc(BLineIdx);
  end;
 FCmdListA.Clear;
End;

Function TCommBridge.FormatCommError ( Const AError : string ) : string;
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

Function TCommBridge.FormatCommState : string;
Var
  BPlayerParams     : string;
Begin
 BPlayerParams:=FPlayerParams;
 Delete(BPlayerParams,1,1);
 Result:=ReadParamStr(BPlayerParams)+' ['+ReadParamStr(BPlayerParams)+'] '+CCommStateS[FCommState];
End;

Procedure TCommBridge.SetCommState ( ANewState : TCommState );
Begin
 if FCommState<>ANewState then
  begin
  FCommState:=ANewState;
  if FCommState<>csClosed then ViewAny('s'+Chr(Byte(ANewState)+Byte('0'))+FormatCommState);
  end;
End;

Procedure TCommBridge.OpenAny;
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
  'f': begin
       FComStream:=TBlockSerial.Create;
       SetCommState(csConnecting);
       FComStream.Connect(ReadParamStr(BPlayerParams));
       if FComStream.LastError<>0 then begin ViewAny('se'+FormatCommState+': '+#13+FormatCommError(FComStream.LastErrorDesc)); break; end;
       if TryStrToInt(ReadParamStr(BPlayerParams),BBaud)=FALSE then begin ViewAny('se'+FormatCommState+': '+#13+'Invalid baud rate'); break; end;
       FComStream.Config(BBaud,8,'N',2,FALSE,FALSE);
       if FComStream.LastError<>0 then begin ViewAny('se'+FormatCommState+': '+#13+FormatCommError(FComStream.LastErrorDesc)); break; end;
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

Function TCommBridge.CanReadAny : boolean;
Begin
 if FComStream<>nil then Result:=FComStream.CanRead(0)
 else Result:=FALSE;
End;

Function TCommBridge.RecvByteAny ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if FComStream<>nil then
  begin
  AData:=FComStream.RecvByte(ATimeOut);
  if FComStream.LastError<>sOK then break;
  end
 else break;
 Result:=TRUE;
 until TRUE;
End;

Function TCommBridge.RecvByteAny ( Out AData : byte ) : boolean;
Begin
 Result:=RecvByteAny(AData,FWait);
End;

Procedure TCommBridge.SendByteAny ( AData : byte );
Begin
 if FComStream<>nil then FComStream.SendByte(AData);
End;

Procedure TCommBridge.SendStringAny ( Const AData : string );
Begin
 if FComStream<>nil then FComStream.SendString(AData);
End;

Function TCommBridge.RecvByteA ( ASenseAtt : boolean; Out AData : byte ) : boolean;
Var
  BByte         : byte;
Begin
 Result:=FALSE;
 AData:=0;
 repeat
 if RecvByteAny(BByte)=FALSE then break;
 if ASenseAtt and (BByte=$AA) then FAttReq:=TRUE
 else begin AData:=BByte; Result:=TRUE; break; end;
 until FALSE;
End;

Procedure TCommBridge.Execute;
Var
  BTickHs,
  BTickThis     : QWord;
  BByte         : byte;
Begin
 BTickHs:=GetTickCount64;
 repeat
 BTickThis:=GetTickCount64;
 EnterCriticalSection(FCmdListLock);
 FCmdListA.Assign(FCmdList); FCmdList.Clear;
 LeaveCriticalSection(FCmdListLock);

 ProcessCmdList;

 if FSetPlayer then begin FSetPlayer:=FALSE; CloseAll; OpenAny; FHsRepeatToFail:=5; end;

 if BTickHs<BTickThis then
  begin
  inc(BTickHs,500);
  repeat
  if (FPlayerParams='') and (FComStream<>nil) then begin CloseAll; break; end;
  if FPlayerParams='' then break;
  case FPlayerParams[1] of
   'f': begin
        if (FPlayerParams<>'') and (FComStream=nil) then begin OpenAny; FHsRepeatToFail:=5; end;
        if FComStream=nil then begin inc(BTickHs,500); break; end;
        SendByteAny($55);
        if RecvByteA(TRUE,BByte) and (BByte=$55) then begin SetCommState(csActive); FHsRepeatToFail:=5; break; end;
        if FHsRepeatToFail>0 then begin Dec(FHsRepeatToFail); break; end;
        CloseAll;
        end;
   'i': begin
        SetCommState(csSimulator);
        end;
   end; // case
  until TRUE;
  if BTickHs<BTickThis then BTickHs:=BTickThis;
  end;

 Sleep(50);
 until Terminated;

 CloseAll;
 FIsEnded:=TRUE;
 ViewAny('mClose');
End;

end.


