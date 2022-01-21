unit ComUsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TComUsb = class(TObject)
  private
    FHandle     : Integer;
    FRecvBuf    : array [0..1023] of byte;
    FRecvSize   : Integer;
    FActive     : boolean;

    FErrorStr   : string;

    Procedure RecvDelete ( AByteCount : Integer );
    Function HaveFullPack ( Var ADataR : array of byte ) : boolean;
    Function RecvOpti : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
    Procedure ComClose;
    Function SendRecv ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
    Function CheckRecv ( Var ADataR : array of byte ) : boolean; // Checks if full packet is already received

    property ErrorS : string read FErrorStr;
    property Active : boolean read FActive;
    property RecvSize : Integer read FRecvSize;
  end;

implementation

Constructor TComUsb.Create;
Begin
 Inherited;
 FHandle:=-1;
 FRecvSize:=0;
End;

Destructor TComUsb.Destroy;
Begin
 Inherited;
End;

Function TComUsb.ComOpen ( Const AComName : string; ABaud : Integer ) : boolean;
Begin
 Result:=FALSE;
 FActive:=FALSE;
End;

Procedure TComUsb.ComClose;
Begin
 FActive:=FALSE;
End;

Procedure TComUsb.RecvDelete ( AByteCount : Integer );
Var
  BIndexW,
  BIndexR       : Integer;
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
 if FRecvSize<(FRecvBuf[1]+2) then break;
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

Function TComUsb.RecvOpti : boolean;
Begin
 Result:=FALSE;
End;

Function TComUsb.SendRecv ( Const ADataS : array of byte; ASendSize : Cardinal; Var ADataR : array of byte; AWait : Integer ) : boolean;
Var
  BSizeA        : Cardinal;
  BTimeout      : Integer;
Begin
 Result:=FALSE;
 repeat

 repeat
 if HaveFullPack(ADataR)=FALSE then break;
 until FALSE;

 //if WriteFile(FHandle,ADataS[0],ASendSize,BSizeA,nil)=FALSE then break;

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

end.

