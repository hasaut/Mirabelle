unit DbgBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd;

Function RecvSynLabel ( Const AMessage : string; Const ABin : array of byte ) : string;
Procedure SplitLineLst ( Const AReadS : string; Out ATypeS, AAddrS, ADataS, AOrigS : string; Out ASegType : char );


implementation

Uses
  ConComL;

Function RecvSynLabel ( Const AMessage : string; Const ABin : array of byte ) : string;
Var
  BLen          : Integer;
  BSynTime      : Int64;
  BIpThis       : Cardinal;
  BDataIdx      : Cardinal;
  BDataSrc      : Char;
  BLabelLen     : byte;
  BDataB        : byte;
Begin
 Result:='';
 repeat
 BLen:=Length(AMessage);
 if BLen<>14 then break;
 BDataSrc:=AMessage[1];
 BDataIdx:=(Ord(AMessage[5]) shl 24)+(Ord(AMessage[4]) shl 16)+(Ord(AMessage[3]) shl 8)+Ord(AMessage[2]);
 BIpThis :=(Ord(AMessage[9]) shl 24)+(Ord(AMessage[8]) shl 16)+(Ord(AMessage[7]) shl 8)+Ord(AMessage[6]);
 BSynTime:=(Ord(AMessage[14]) shl 32)+(Ord(AMessage[13]) shl 24)+(Ord(AMessage[12]) shl 16)+(Ord(AMessage[11]) shl 8)+Ord(AMessage[10]);
 Result:=IntToStr(BSynTime)+' '+BDataSrc+' '+IntToHex(BIpThis,8)+' ';
 if BDataIdx<Length(ABin) then
  begin
  if BDataSrc='A' then
   begin
   BLabelLen:=ABin[BDataIdx]; inc(BDataIdx);
   while BLabelLen>0 do
    begin
    if BDataIdx>=Length(ABin) then break;
    BDataB:=ABin[BDataIdx]; inc(BDataIdx);
    if BDataB in [32..126] then
    else break;
    Result:=Result+Chr(BDataB);
    dec(BLabelLen);
    end;
   end
  else if BDataSrc='B' then
   begin
   while BDataIdx<Length(ABin) do
    begin
    BDataB:=ABin[BDataIdx]; inc(BDataIdx);
    if BDataB in [32..126] then
    else break;
    Result:=Result+Chr(BDataB);
    end;
   end
  else Result:=Result+'Invalid SRC';
  end;
 until TRUE;
End;

Procedure SplitLineLst ( Const AReadS : string; Out ATypeS, AAddrS, ADataS, AOrigS : string; Out ASegType : char );
Var
  BReadS        : string;
  BPos          : Integer;
Begin
 BReadS:=AReadS;
 BPos:=Pos('|',BReadS);
 if BPos=0 then begin AOrigS:=BReadS; BReadS:=''; end
 else begin AOrigS:=Copy(BReadS,BPos+1,Length(BReadS)-BPos); Delete(BReadS,BPos,Length(BReadS)-BPos+1); end;
 ATypeS:=ReadParamStr(BReadS); while Length(ATypeS)<3 do ATypeS:=ATypeS+'.';
 AAddrS:=ReadParamStr(BReadS);
 ADataS:=ReadParamStr(BReadS);

 ASegType:=#0;
 if ATypeS<>'' then ASegType:=ATypeS[1];
End;

end.

