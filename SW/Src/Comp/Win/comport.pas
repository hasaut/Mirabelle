unit ComPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, ComPortBase;

Type
  TComPort = class(TComPortBase)
  private
    FHandle     : THandle;
    FActive     : boolean;

    FFullName   : string;

    FRecvBuf    : array [0..4095] of byte;
    FRecvIdx,
    FRecvLen    : Cardinal;

    FRecvBufLen : Integer;
    FDcb        : TDcb;

    FDeadlockTimeOut    : Cardinal;
    Procedure DecodeCommError ( AError : DWord );
    Procedure Purge;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Connect ( Const AComName : string ) : boolean;
    Function Config ( ABaud : integer ) : boolean;
    Procedure Close;

    Function SendData ( Const ADataBin : string ) : boolean;
    Function RecvByte ( Out AData : byte ) : boolean;
    Function RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean;

    property Active : boolean read FActive;
  end;

implementation

Constructor TComPort.Create;
Begin
 Inherited;
 FDeadLockTimeOut:=1000;
End;

Destructor TComPort.Destroy;
Begin
 if FActive then Close;
 Inherited;
End;

Function TComPort.Connect ( Const AComName : string ) : boolean;
Var
  BCommTimeOuts : TCommTimeOuts;
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 FFullName:='\\.\'+UpperCase(AComName);
 FHandle:=CreateFile(PChar(FFullName),GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0);
 if FHandle=INVALID_HANDLE_VALUE then break;
 if SetCommMask(FHandle, 0)=FALSE then break;;
 if SetupComm(FHandle,FRecvBufLen,0)=FALSE then break;
 BCommTimeOuts.ReadIntervalTimeout:=MAXWORD;
 BCommTimeOuts.ReadTotalTimeoutMultiplier:=0;
 BCommTimeOuts.ReadTotalTimeoutConstant:=0;
 BCommTimeOuts.WriteTotalTimeoutMultiplier:=0;
 BCommTimeOuts.WriteTotalTimeoutConstant:=0;
 if SetCommTimeOuts(FHandle, BCommTimeOuts)=FALSE then break;
 Result:=TRUE; FActive:=TRUE;
 until TRUE;
 if Result=FALSE then FLastError:=GetLastError;
End;

Function TComPort.Config ( ABaud : integer ) : boolean;
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 FillChar(FDcb,SizeOf(FDcb),0);
 if GetCommState(FHandle,FDcb)=FALSE then break;
 FDcb.DCBlength:=SizeOf(FDcb);
 FDcb.BaudRate:=ABaud;
 FDcb.ByteSize:=8;
 FDcb.Parity:=0;
 FDcb.StopBits:=0;
 FDcb.XonChar:=#17;
 FDcb.XoffChar:=#19;
 FDcb.XonLim:=FRecvBufLen div 4;
 FDcb.XoffLim:=FRecvBufLen div 4;
 FDcb.Flags:=dcb_Binary;
 if SetCommState(FHandle,FDcb)=FALSE then break;
 Result:=TRUE;
 until TRUE;
 if Result=FALSE then FLastError:=GetLastError;
end;

Procedure TComPort.Close;
Begin
 repeat
 if FActive=FALSE then break;
 Purge;
 FileClose(FHandle);
 FActive:=FALSE;
 until TRUE;
End;

Procedure TComPort.DecodeCommError ( AError : DWord );
Begin
 repeat
 if (AError and DWord(CE_FRAME)) <> 0 then begin FLastError:=CComErrFrame; break; end;
 if (AError and DWord(CE_OVERRUN))<>0 then begin FLastError:=CComErrOverrun; break; end;
 if (AError and DWord(CE_RXOVER))<>0 then  begin FLastError:=CComErrRxOver; break; end;
 if (AError and DWord(CE_RXPARITY))<>0 then begin FLastError:=CComErrRxParity; break; end;
 if (AError and DWord(CE_TXFULL))<>0 then begin FLastError:=CComErrTxFull; break; end;
 FLastError:=0;
 until TRUE;
End;

Procedure TComPort.Purge;
begin
 PurgeComm(FHandle,PURGE_TXABORT or PURGE_TXCLEAR or PURGE_RXABORT or PURGE_RXCLEAR);
end;

Function TComPort.SendData ( Const ADataBin : string ) : boolean;
Var
  BOverlapped   : TOverlapped;
  BLen,
  BBytesSent    : DWord;
  BResultB      : DWord;
  BError        : DWord;
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 if ADataBin='' then begin Result:=TRUE; break; end;
 FillChar(BOverlapped,Sizeof(BOverlapped),0);
 BLen:=Length(ADataBin); BBytesSent:=0;
 FLastError:=0;
 if WriteFile(FHandle,ADataBin[1],BLen,BBytesSent,@BOverlapped) then begin Result:=BLen=BBytesSent; break; end;
 FLastError:=GetLastError;
 if FLastError=ERROR_IO_PENDING then
  begin
  BResultB:=WaitForSingleObject(FHandle,FDeadlockTimeout);
  if BResultB=WAIT_TIMEOUT then
   begin
   PurgeComm(FHandle,PURGE_TXABORT);
   FLastError:=CComErrTimeout;
   break;
   end;
  if GetOverlappedResult(FHandle,BOverlapped,BBytesSent,FALSE)=FALSE then begin FLastError:=GetLastError; break; end;
  FLastError:=0;
  Result:=BLen=BBytesSent;
  break;
  end;
 until TRUE;

 if FLastError<>0 then
  begin
  BError:=0;
  ClearCommError(FHandle,BError,nil);
  if BError<>0 then DecodeCommError(BError);
  end;
End;

Function TComPort.RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Var
  BOverlapped   : TOverlapped;
  BBytesRecv    : DWord;
  BResultB      : DWord;
  BError        : DWord;
Begin
 Result:=FALSE;
 FLastError:=0; AData:=0;
 repeat
 if FRecvIdx<FRecvLen then
  begin
  AData:=FRecvBuf[FRecvIdx]; inc(FRecvIdx);
  Result:=TRUE;
  break;
  end;

 FLastError:=0;
 FRecvLen:=0; FRecvIdx:=0; BBytesRecv:=0;
 FillChar(BOverlapped,Sizeof(BOverlapped),0);
 if ReadFile(FHandle,FRecvBuf[0],1,BBytesRecv,@BOverlapped) then
  begin
  FRecvLen:=BBytesRecv;
  if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; end;
  break;
  end;

 FLastError:=GetLastError;
 if FLastError=ERROR_IO_PENDING then
  begin
  BResultB:=WaitForSingleObject(FHandle,ATimeOut);
  if BResultB=WAIT_TIMEOUT then
    begin
    PurgeComm(FHandle,PURGE_RXABORT);
    FLastError:=CComErrTimeout;
    break;
    end;
  if GetOverlappedResult(FHandle,BOverlapped,BBytesRecv,FALSE)=FALSE then begin FLastError:=GetLastError; break; end;
  FRecvLen:=BBytesRecv;
  if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; end;
  end;

 until TRUE;

 if FLastError<>0 then
  begin
  BError:=0; ClearCommError(FHandle,BError,nil);
  if BError<>0 then DecodeCommError(BError);
  end;
End;

Function TComPort.RecvByte ( Out AData : byte ) : boolean;
Begin
 Result:=RecvByte(AData,FDeadLockTimeOut);
End;

end.

