unit ComPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  termio, baseunix, unix,
  ComPortBase;

Type
  TComPort = class(TComPortBase)
  private
    FHandle     : THandle;
    FActive     : boolean;

    FFullName   : string;

    FRecvBuf    : array [0..4095] of byte;
    FRecvIdx,
    FRecvLen    : Cardinal;

    FTermiosStruc   : Termios;

    Procedure Purge;

    Function GetRecvSize : Integer;
    Function RecvByteOpti ( ABytesRecv : Integer; Out AData : byte ) : boolean;
    Function WaitRecv ( ATimeOut : integer ) : boolean;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Connect ( Const AComName : string ) : boolean;
    Function Config ( ABaud : integer ) : boolean;
    Procedure Close;

    Function SendData ( Const ADataBin : string ) : boolean; Override;
    Function RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean; Override;

    property Active : boolean read FActive;
  end;

implementation

Const
  INVALID_HANDLE_VALUE = THandle(-1);

Function GetBaud ( ABaud : Cardinal ) : Cardinal;
Begin
 Result:=B230400;

 case ABaud of
  9600:    Result:=B9600;
  19200:   Result:=B19200;
  38400:   Result:=B38400;
  57600:   Result:=B57600;
  115200:  Result:=B115200;
  230400:  Result:=B230400;
  460800:  Result:=B460800;
  500000:  Result:=$0001005;
  576000:  Result:=$0001006;
  921600:  Result:=$0001007;
  1000000: Result:=$0001008;
  1152000: Result:=$0001009;
  1500000: Result:=$000100A;
  2000000: Result:=$000100B;
  2500000: Result:=$000100C;
  3000000: Result:=$000100D;
  3500000: Result:=$000100E;
  4000000: Result:=$000100F;
 end;
End;

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
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 FFullName:=AComName;
 FHandle:=fpOpen(FFullName, O_RDWR or O_SYNC);
 if FHandle=INVALID_HANDLE_VALUE then break;
 Result:=TRUE; FActive:=TRUE;
 until TRUE;
 if Result=FALSE then FLastError:=fpGetErrno;
End;

Function TComPort.Config ( ABaud : integer ) : boolean;
Var
  BBaud     : Cardinal;
Begin
 Result:=FALSE;
 FLastError:=0;
 repeat
 if TCGetAttr(FHandle,FTermiosStruc)=-1 then break;
 CFMakeRaw(FTermiosStruc);
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag or CREAD;
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag or CLOCAL;
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag or HUPCL;
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag and (not CRTSCTS);
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag or CS8;
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag and (not PARENB);
 FTermiosStruc.c_cflag:=FTermiosStruc.c_cflag and (not CSTOPB);
 BBaud:=GetBaud(ABaud);
 CFSetOSpeed(FTermiosStruc,BBaud);
 CFSetISpeed(FTermiosStruc,BBaud);
 if TCSetAttr(FHandle,TCSANOW,FTermiosStruc)=-1 then break;
 Result:=TRUE;
 until TRUE;
 if Result=FALSE then FLastError:=fpGetErrno;;
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

Procedure TComPort.Purge;
Begin
 FpIOCtl(FHandle, TCFLSH, Pointer(PtrInt(TCIOFLUSH)));
End;

Function TComPort.SendData ( Const ADataBin : string ) : boolean;
Var
  BLen          : DWord;
Begin
 Result:=FALSE;
 FLastError:=0;

 repeat
 if ADataBin='' then begin Result:=TRUE; break; end;
 BLen:=Length(ADataBin);
 if FileWrite(FHandle,ADataBin[1],BLen)=-1 then break;
 Result:=TRUE;
 until TRUE;

 if Result=FALSE then FLastError:=fpGetErrno;
End;

Function TComPort.GetRecvSize : Integer;
Var
  BBytesRecv    : Integer;
Begin
 BBytesRecv:=0;
 if FpIOCtl(FHandle,FIONREAD,@BBytesRecv)=-1 then BBytesRecv:=0;
 Result:=BBytesRecv;
End;

Function TComPort.RecvByteOpti ( ABytesRecv : Integer; Out AData : byte ) : boolean;
Var
  BBytesRecv    : Integer;
Begin
 Result:=FALSE; AData:=0;
 repeat
 BBytesRecv:=ABytesRecv;
 if BBytesRecv>4096 then BBytesRecv:=4096;
 if FileRead(FHandle,FRecvBuf[0],BBytesRecv)=-1 then break;
 FRecvLen:=BBytesRecv;
 AData:=FRecvBuf[FRecvIdx]; inc(FRecvIdx);
 Result:=TRUE;
 until TRUE;
End;

Function TComPort.WaitRecv ( ATimeOut : integer ) : boolean;
Var
  BFDSet    : TFDSet;
  BTimeVal  : PTimeVal;
  BTimeV    : TTimeVal;
begin
 Result:=FALSE;
 repeat
 BTimeV.tv_usec:=(ATimeOut mod 1000)*1000;
 BTimeV.tv_sec:=ATimeOut div 1000;
 BTimeVal:=@BTimeV;
 fpFD_ZERO(BFDSet);
 fpFD_SET(FHandle,BFDSet);
 if fpSelect(FHandle+1,@BFDSet,nil,nil,BTimeVal)=-1 then break;
 Result:=TRUE;
 until TRUE;
 if Result=FALSE then FLastError:=fpGetErrno;
end;


Function TComPort.RecvByte ( Out AData : byte; ATimeOut : Cardinal ) : boolean;
Var
  BBytesRecv    : Integer;
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

 FRecvLen:=0; FRecvIdx:=0;
 BBytesRecv:=GetRecvSize;
 if BBytesRecv>0 then begin Result:=RecvByteOpti(BBytesRecv,AData); break; end;

 if WaitRecv(ATimeOut)=FALSE then begin FLastError:=CComErrTimeout; break; end;
 BBytesRecv:=GetRecvSize;
 if BBytesRecv=0 then begin FLastError:=CComErrTimeout; break; end;
 Result:=RecvByteOpti(BBytesRecv,AData);
 until TRUE;

 if Result=FALSE then
  begin
  if FLastError=0 then FLastError:=fpGetErrno;
  end;
End;

end.

