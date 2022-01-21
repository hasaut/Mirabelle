unit ComS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynAser;

Type
  TComS = class (TObject)
  private
   FCom         : TBlockSerial;
   FComName     : string;
   FComBaud     : Integer;

   FRecvBuf     : array [0..65535] of byte;
   FRecvSize    : Cardinal;

   FActive      : boolean;
   FLastError   : string;

   Procedure RecvDelete ( AByteCount : Integer );
   Function HaveFullPackBin ( Var ADataR : array of byte ) : boolean;
   Function HaveFullPack ( Var ADataR : string ) : boolean;
   Function RecvOpti : boolean;

  public
   Constructor Create; Virtual;
   Destructor Destroy; Override;

   Function ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
   Procedure ComClose;
   Procedure ClearRecv;
   Function SendRecvBin ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
   Function SendRecv ( Const ADataS : string; Var ADataR : string; AWait : Integer ) : boolean;
   Function CheckRecv ( Var ADataR : array of byte ) : boolean; // Checks if full packet is already received
   Function WaitRecv ( Var ADataR : array of byte; AWait : Integer ) : boolean;

   property ComName : string read FComName;
   property ComBaud : Integer read FComBaud;
   property ErrorS : string read FLastError;
   property Active : boolean read FActive;
   property RecvSize : Cardinal read FRecvSize;
  end;

implementation

Constructor TComS.Create;
Begin
 Inherited;
 FCom:=TBlockSerial.Create;
End;

Destructor TComS.Destroy;
Begin
 ComClose;
 FCom.Free;
 Inherited;
End;

Procedure TComS.ComClose;
Begin
 if FActive then FCom.CloseSocket;
 FActive:=FALSE;
End;

Function ProcessError ( Const AError : string ) : string;
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

Function TComS.ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
Begin
 Result:=FALSE;
 ComClose;

 FComName:=AComName;
 FComBaud:=ABaud;
 FLastError:='';

 repeat
 FCom.Connect(FComName);
 if FCom.LastError<>0 then begin FLastError:=ProcessError(FCom.LastErrorDesc); break; end;
 FCom.Config(FComBaud,8,'N',2,FALSE,FALSE);
 if FCom.LastError<>0 then begin FLastError:=ProcessError(FCom.LastErrorDesc); break; end;
 FActive:=TRUE;
 Result:=TRUE;
 until TRUE;
End;

Procedure TComS.RecvDelete ( AByteCount : Integer );
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

Function TComS.HaveFullPackBin ( Var ADataR : array of byte ) : boolean;
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

Function TComS.HaveFullPack ( Var ADataR : string ) : boolean;
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

Function TComS.RecvOpti : boolean;
Var
  BByte         : byte;
Begin
 Result:=FALSE;

 repeat
 BByte:=FCom.RecvByte(0);
 if FCom.LastError<>sOK then break;
 FRecvBuf[FRecvSize]:=BByte;
 if (FRecvSize+1)<Length(FRecvBuf) then Inc(FRecvSize);
 Result:=TRUE;
 until FALSE;
End;

Procedure TComS.ClearRecv;
Begin
 FRecvSize:=0;
End;

Function TComS.SendRecvBin ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
Var
  BSizeA        : Cardinal;
  BTimeout      : Integer;
Begin
 Result:=FALSE;
 repeat

 // Delete all stuff from buffer
 //repeat
 //if HaveFullPack(ADataR)=FALSE then break;
 //until FALSE;
 //FRecvSize:=0;

 BSizeA:=FCom.SendBuffer(@ADataS[0],ASendSize);

 if BSizeA<>ASendSize then
  begin
  break;
  end;

 AWait:=(AWait div 10)+1;
 BTimeout:=0;
 while BTimeout<AWait do
  begin
  RecvOpti;
  if HaveFullPackBin(ADataR) then begin Result:=TRUE; break; end;
  Sleep(10);
  inc(BTimeout);
  end;

 until TRUE;
End;

Function TComS.SendRecv ( Const ADataS : string; Var ADataR : string; AWait : Integer ) : boolean;
Var
  BTimeout      : Integer;
  BDataS        : string;
Begin
 Result:=FALSE;
 BDataS:=ADataS+#13;

 repeat
 FCom.SendString(BDataS);

 AWait:=(AWait div 10)+1;
 BTimeout:=0;
 while BTimeout<AWait do
  begin
  RecvOpti;
  if HaveFullPack(ADataR) then begin Result:=TRUE; break; end;
  Sleep(10);
  inc(BTimeout);
  end;

 until TRUE;
End;

Function TComS.CheckRecv ( Var ADataR : array of byte ) : boolean;
Begin
 Result:=HaveFullPackBin(ADataR);
 if Result=FALSE then
  begin
  RecvOpti;
  Result:=HaveFullPackBin(ADataR);
  end;
End;

Function TComS.WaitRecv ( Var ADataR : array of byte; AWait : Integer ) : boolean;
Var
  BTimeout      : Integer;
Begin
 repeat
 Result:=HaveFullPackBin(ADataR);
 if Result then break;
 AWait:=(AWait div 10)+1;
 BTimeout:=0;
 while BTimeout<AWait do
  begin
  RecvOpti;
  if HaveFullPackBin(ADataR) then begin Result:=TRUE; break; end;
  Sleep(10);
  inc(BTimeout);
  end;
 until TRUE;
End;

end.

