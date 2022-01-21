unit ComUsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

Type
  TComUsb = class(TObject)
  private
    FComName    : string;
    FComBaud    : Integer;
    FInvalidHandleValue : Integer;
    FHandle     : Integer;
    FRecvBuf    : array [0..1023] of byte;
    FRecvSize   : Cardinal;
    FActive     : boolean;

    FErrorStr   : string;

    Procedure RecvDelete ( AByteCount : Integer );
    Function HaveFullPack ( Var ADataR : array of byte ) : boolean;
    Function HaveFullPack ( Var ADataR : string ) : boolean;
    Function RecvOpti : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
    Procedure ComClose;
    Procedure ClearRecv;
    Function SendRecv ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
    Function SendRecv ( Const ADataS : string; Var ADataR : string; AWait : Integer ) : boolean;
    Function CheckRecv ( Var ADataR : array of byte ) : boolean; // Checks if full packet is already received
    Function WaitRecv ( Var ADataR : array of byte; AWait : Integer ) : boolean;

    property ComName : string read FComName;
    property ComBaud : Integer read FComBaud;
    property ErrorS : string read FErrorStr;
    property Active : boolean read FActive;
    property RecvSize : Cardinal read FRecvSize;
  end;

implementation

Constructor TComUsb.Create;
Begin
 Inherited;
 FInvalidHandleValue:=INVALID_HANDLE_VALUE;
 FHandle:=FInvalidHandleValue;
 FRecvSize:=0;
End;

Destructor TComUsb.Destroy;
Begin
 Inherited;
End;

Function TComUsb.ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
Var
  BError        : Integer;
  BErrorS       : array [0..1023] of char;
  BIndex        : Integer;
  BPortParams   : DCB;
  BTimeOuts     : COMMTIMEOUTS;

Begin
 Result:=FALSE;

 FComName:=AComName; FComBaud:=ABaud;

 FActive:=FALSE;
 repeat
 FErrorStr:='';
 if FHandle<>FInvalidHandleValue then CloseHandle(FHandle);
 FHandle:=CreateFile(PChar(AComName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if FHandle=FInvalidHandleValue then
  begin
  BError:=GetLastError;
  if BError=ERROR_FILE_NOT_FOUND then FErrorStr:='Com '+AComName+' does not exist'
  else
   begin
   FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, BError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), BErrorS, 1024, nil);
   BIndex:=0; FErrorStr:='';
   while BIndex<1024 do
    begin
    if BErrorS[BIndex]=#0 then break;
    FErrorStr:=FErrorStr+BErrorS[BIndex];
    inc(BIndex);
    end;
   end;
  break;
  end;

 FillChar(BPortParams,0,SizeOf(DCB));
 BPortParams.DCBlength:=SizeOf(DCB);
 if GetCommState(FHandle, BPortParams)=FALSE then
  begin
  FErrorStr:='Cannot set baud rate';
  break;
  end;

 BPortParams.BaudRate:=ABaud;
 BPortParams.ByteSize:=8;
 BPortParams.StopBits:=ONESTOPBIT;
 BPortParams.Parity:=NOPARITY;
 if SetCommState(FHandle, BPortParams)=FALSE then
  begin
  FErrorStr:='Cannot set baud rate';
  break;
  end;

 BTimeOuts.ReadIntervalTimeout:=1;
 BTimeOuts.ReadTotalTimeoutConstant:=1;
 BTimeOuts.ReadTotalTimeoutMultiplier:=0;
 BTimeOuts.WriteTotalTimeoutConstant:=1;
 BTimeOuts.WriteTotalTimeoutMultiplier:=1;
 if SetCommTimeouts(FHandle, BTimeOuts)=FALSE then
  begin
  FErrorStr:='Cannot set timeouts';
  break;
  end;

 FActive:=TRUE;
 Result:=TRUE;
 until TRUE;
End;

Procedure TComUsb.ComClose;
Begin
 if FHandle<>FInvalidHandleValue then CloseHandle(FHandle);
 FHandle:=FInvalidHandleValue;
 FActive:=FALSE;
End;

Procedure TComUsb.RecvDelete ( AByteCount : Integer );
Var
  BIndexW,
  BIndexR       : Cardinal;
Begin
 BIndexW:=0; BIndexR:=AByteCount;
 while BIndexR<FRecvSize do
  begin
  FRecvBuf[BIndexW]:=FRecvBuf[BIndexR];
  inc(BIndexW);
  inc(BIndexR);
  end;
 FRecvSize:=BIndexW;
End;

Function TComUsb.HaveFullPack ( Var ADataR : array of byte ) : boolean;
Var
  BIndex        : Integer;
Begin
 Result:=FALSE;
 repeat
 if FRecvSize=0 then break;
 if FRecvBuf[0]=$55 then
  begin
  ADataR[0]:=$55;
  RecvDelete(1);
  Result:=TRUE;
  break;
  end;
 if FRecvSize<2 then break;
 if FRecvSize<Cardinal(FRecvBuf[1]+2) then break;
 BIndex:=0;
 while BIndex<(FRecvBuf[1]+2) do
  begin
  ADataR[BIndex]:=FRecvBuf[BIndex];
  inc(BIndex);
  end;
 RecvDelete(FRecvBuf[1]+2);
 Result:=TRUE;
 until TRUE;
End;

Function TComUsb.HaveFullPack ( Var ADataR : string ) : boolean;
Var
  BIndex        : Integer;
  BDummyS       : string;
  BDataC        : char;
Begin
 Result:=FALSE;
 BDummyS:='';
 repeat
 if FRecvSize=0 then break;
 BIndex:=0;
 while BIndex<FRecvSize do
  begin
  BDataC:=Chr(FRecvBuf[BIndex]);
  if BDataC=#13 then begin ADataR:=BDummyS; Result:=TRUE; break; end;
  BDummyS:=BDummyS+BDataC;
  inc(BIndex);
  end;
 if Result then RecvDelete(BIndex+1);
 until TRUE;
End;

Function TComUsb.RecvOpti : boolean;
Var
  BSizeA        : Cardinal;
Begin
 Result:=FALSE;

 repeat
 BSizeA:=0;
 if ReadFile(FHandle,FRecvBuf[FRecvSize],1024-FRecvSize,BSizeA,nil)=FALSE then break;
 FRecvSize:=FRecvSize+BSizeA;
 Result:=TRUE;
 until TRUE;
End;

Procedure TComUsb.ClearRecv;
Begin
 FRecvSize:=0;
End;

Function TComUsb.SendRecv ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
Var
  BSizeA        : Cardinal;
  BTimeout      : Integer;
Begin
 Result:=FALSE;
 repeat

 // Delete all stuff from buffer
 repeat
 if HaveFullPack(ADataR)=FALSE then break;
 until FALSE;

 BSizeA:=0;
 if WriteFile(FHandle,ADataS[0],ASendSize,BSizeA,nil)=FALSE then break;

 BTimeout:=0;
 while BTimeout<AWait do
  begin
  if RecvOpti=FALSE then break;
  if HaveFullPack(ADataR) then begin Result:=TRUE; break; end;
  Sleep(1);
  inc(BTimeout);
  end;

 until TRUE;
End;

Function TComUsb.SendRecv ( Const ADataS : string; Var ADataR : string; AWait : Integer ) : boolean;
Var
  BSizeA        : Cardinal;
  BTimeout      : Integer;
  BDataS        : string;
Begin
 Result:=FALSE;
 BDataS:=ADataS+#13;

 repeat
 BSizeA:=0;
 if WriteFile(FHandle,BDataS[1],Length(BDataS),BSizeA,nil)=FALSE then break;

 BTimeout:=0;
 while BTimeout<AWait do
  begin
  if RecvOpti=FALSE then break;
  if HaveFullPack(ADataR) then begin Result:=TRUE; break; end;
  Sleep(1);
  inc(BTimeout);
  end;

 until TRUE;
End;

Function TComUsb.CheckRecv ( Var ADataR : array of byte ) : boolean;
Begin
 Result:=HaveFullPack(ADataR);
 if Result=FALSE then
  begin
  RecvOpti;
  Result:=HaveFullPack(ADataR);
  end;
End;

Function TComUsb.WaitRecv ( Var ADataR : array of byte; AWait : Integer ) : boolean;
Var
  BTimeout      : Integer;
Begin
 Result:=HaveFullPack(ADataR);
 if Result=FALSE then
  begin
  RecvOpti;
  Result:=HaveFullPack(ADataR);
  end;

 repeat
 if Result=FALSE then break;
 BTimeout:=0;
 while BTimeout<AWait do
  begin
  if RecvOpti=FALSE then break;
  if HaveFullPack(ADataR) then begin Result:=TRUE; break; end;
  Sleep(1);
  inc(BTimeout);
  end;
 until TRUE;
End;

end.

