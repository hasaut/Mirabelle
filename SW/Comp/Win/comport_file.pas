unit ComPort_File;

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

    FDcb        : TDcb;
    //FRxOvl      : TOverlapped;
    //FRxPend     : boolean;

    FDebug      : boolean;

    Procedure DecodeCommError ( AError : DWord );
    Procedure Purge;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Connect ( Const AComName : string ) : boolean;
    Function Config ( ABaud : integer ) : boolean;
    Procedure Close;

    Function SendData ( Const ADataBin : string ) : boolean; Override;
    Function RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean; Override;
    Function RecvData ( ADataBuf : Pointer; ASize : Cardinal; ATimeOut : Cardinal ) : boolean;

    property Active : boolean read FActive;
    property Debug : boolean read FDebug write FDebug;
  end;

implementation

Const
  CBufLen = 32768;

Constructor TComPort.Create;
Begin
 Inherited;
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
 FHandle:=CreateFile(PChar(FFullName),GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if FHandle=INVALID_HANDLE_VALUE then break;
 if SetCommMask(FHandle, 0)=FALSE then break;;
 if SetupComm(FHandle,CBufLen,CBufLen)=FALSE then break;
 {BCommTimeOuts.ReadIntervalTimeout:=1;
 BCommTimeOuts.ReadTotalTimeoutMultiplier:=0;
 BCommTimeOuts.ReadTotalTimeoutConstant:=1;
 BCommTimeOuts.WriteTotalTimeoutMultiplier:=1;
 BCommTimeOuts.WriteTotalTimeoutConstant:=1;}
 BCommTimeOuts.ReadIntervalTimeout:=MAXDWORD;
 BCommTimeOuts.ReadTotalTimeoutMultiplier:=0;
 BCommTimeOuts.ReadTotalTimeoutConstant:=0;
 BCommTimeOuts.WriteTotalTimeoutMultiplier:=0;
 BCommTimeOuts.WriteTotalTimeoutConstant:=0;
 if SetCommTimeOuts(FHandle, BCommTimeOuts)=FALSE then break;
 //FillChar(FRxOvl,Sizeof(FRxOvl),0);
 //FRxOvl.hEvent:=CreateEvent(nil,TRUE,FALSE,nil);
 //FRxPend:=FALSE;
 FActive:=TRUE;
 Result:=TRUE;
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
 FDcb.XonLim:=CBufLen div 4;
 FDcb.XoffLim:=CBufLen div 4;
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
 //CloseHandle(FRxOvl.hEvent);
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
  BLen,
  BBytesSent    : DWord;
  BError        : DWord;
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 if ADataBin='' then begin Result:=TRUE; break; end;
 BLen:=Length(ADataBin); BBytesSent:=0;
 FLastError:=0;
 if WriteFile(FHandle,ADataBin[1],BLen,BBytesSent,nil) then begin Result:=BLen=BBytesSent; break; end;
 FLastError:=GetLastError;
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
  BData         : byte;
  BBytesRecv    : DWord;
  BError        : DWord;
  BTimeOut      : Cardinal;
  BIsTimeOut    : boolean;
Begin
 Result:=FALSE;
 FLastError:=0; AData:=0;

 BTimeOut:=ATimeOut;
 repeat
 if FRecvIdx<FRecvLen then
  begin
  AData:=FRecvBuf[FRecvIdx]; inc(FRecvIdx);
  Result:=TRUE;
  break;
  end;

 FLastError:=0;
 FRecvLen:=0; FRecvIdx:=0; BBytesRecv:=0;
 BBytesRecv:=0;
 BIsTimeOut:=FALSE; BData:=0;
 if ReadFile(FHandle,FRecvBuf[0],Length(FRecvBuf),BBytesRecv,nil) then
  begin
  FRecvLen:=BBytesRecv;
  if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; break; end;
  BIsTimeOut:=TRUE;
  end;

 if BIsTimeOut then
  begin
  if BTimeOut=0 then break;
  if BTimeOut<10 then BTimeOut:=0
  else Dec(BTimeOut,10);
  Sleep(10);
  end
 else
  begin
  FLastError:=GetLastError;
  break;
  end;

 until FALSE;

 if Result then
 else if BTimeOut=0 then
 else if FLastError<>0 then
  begin
  BError:=0; ClearCommError(FHandle,BError,nil);
  if BError<>0 then DecodeCommError(BError);
  end;
End;

Function TComPort.RecvData ( ADataBuf : Pointer; ASize : Cardinal; ATimeOut : Cardinal ) : boolean;
Var
  BBytesRecv    : DWord;
  BIsTimeOut    : boolean;
Begin
 Result:=FALSE;
 FLastError:=0;

 repeat
 FLastError:=0;
 BBytesRecv:=0;
 BIsTimeOut:=FALSE;
 if ReadFile(FHandle,ADataBuf^,ASize,BBytesRecv,nil) then
  begin
  if BBytesRecv=ASize then begin Result:=TRUE; break; end;
  BIsTimeOut:=TRUE;
  end;

 if BIsTimeOut then break;
 until FALSE;
End;


{ This is an "official" MSDE routine, which for some reason does not work
Function TComPort.RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Var
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

 if FRxPend then
  begin
  BResultB:=WaitForSingleObject(FRxOvl.hEvent,ATimeOut);
  if BResultB=ERROR_IO_PENDING then break;
  FRxPend:=FALSE;
  if BResultB=WAIT_OBJECT_0 then
   begin
   if GetOverlappedResult(FHandle,FRxOvl,BBytesRecv,FALSE)=FALSE then begin FLastError:=GetLastError; break; end;
   FRecvLen:=BBytesRecv;
   if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; end;
   break;
   end;
  break; // other return values are errors
  end;

 FLastError:=0;
 FRecvLen:=0; FRecvIdx:=0; BBytesRecv:=0;
 FRxOvl.Offset:=0;
 if ReadFile(FHandle,FRecvBuf[0],1,BBytesRecv,@FRxOvl) then
  begin
  FRecvLen:=BBytesRecv;
  if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; break; end;
  end;

 FLastError:=GetLastError;
 if FLastError=ERROR_IO_PENDING then
  begin
  BResultB:=WaitForSingleObject(FRxOvl.hEvent,ATimeOut);
  if BResultB=WAIT_TIMEOUT then begin FRxPend:=TRUE; break; end;
  if BResultB=WAIT_OBJECT_0 then
   begin
   if GetOverlappedResult(FHandle,FRxOvl,BBytesRecv,FALSE)=FALSE then begin FLastError:=GetLastError; break; end;
   FRecvLen:=BBytesRecv;
   if FRecvLen<>0 then begin AData:=FRecvBuf[0]; FRecvIdx:=1; Result:=TRUE; end;
   break;
   end;
  end;

 until TRUE;

 if FLastError<>0 then
  begin
  BError:=0; ClearCommError(FHandle,BError,nil);
  if BError<>0 then DecodeCommError(BError);
  end;
End;
}
end.

