unit DataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Function ProcessHexLine ( AFile : TStringList; ALineIdx : Integer; Var ABaseAddr, AHexOffset : Cardinal; Var ADataBin : string ) : string;
Function HexFileParse ( AFile : TStringList; Out ABaseAddr : Cardinal; Out ADataBin : string ) : string;
Function HexFileParse ( AFile : TStringList; Var ALineIdx : Integer; Var AHexOffset : Cardinal; Out ABaseAddr : Cardinal; Out ADataBin : string ) : string;
Procedure HexFileCreate ( ABaseAddr : Cardinal; Const AData : string; AHex : TStringList );
Procedure HexFileAddData ( ABaseAddr : Cardinal; Const AData : string; AHex : TStringList );
Procedure MemFileCreate ( AFullSize : Cardinal; AHwWidth : byte; Const AData : string; AMem : TStringList );

Function TryParseEfinixHex ( AFile : TStringList; Out ADataBin : string; Out AErrorS : string ) : boolean;

implementation

Uses
  ConComL;

Function ProcessHexLine ( AFile : TStringList; ALineIdx : Integer; Var ABaseAddr, AHexOffset : Cardinal; Var ADataBin : string ) : string;
Var
  BData         : string;
  BLenA         : Integer;
  BSizeS,
  BAddrS,
  BTypeS,
  BDataS        : string;
  BAddr,
  BSize         : Cardinal;
  BDataBin      : string;
  BOffset       : Cardinal;
  BIndex        : Integer;
  BDummyS       : string;
  BWrAddr       : Cardinal;
Begin
 Result:='';
 BData:=AFile.Strings[ALineIdx];

 repeat
 DelFirstSpace(BData);

 BAddr:=0;
 BLenA:=Length(BData);
 if BLenA<11 then begin Result:='Hex line is too short'; break; end;
 if BData[1]<>':' then begin Result:='Semicolon expected at the begin of line'; break; end;
 BSizeS:=Copy(BData,2,2);
 BAddrS:=Copy(BData,4,4);
 BTypeS:=Copy(BData,8,2);
 BDataS:=Copy(BData,10,BLenA-11);
 if HexToDWordCheck(BAddrS,BAddr)=FALSE then begin Result:='Cannot convert address part to integer'; break; end;
 if HexToDWordCheck(BSizeS,BSize)=FALSE then begin Result:='Cannot convert size part to integer'; break; end;
 BDataBin:=StrHexToBin(BDataS);
 if Length(BDataBin)<>BSize then begin Result:='Invalid data size'; break; end;
 if BTypeS='00' then
  begin
  BWrAddr:=AHexOffset+BAddr;
  if ADataBin='' then ABaseAddr:=BWrAddr
  else if BWrAddr<ABaseAddr then
   begin
   SetLength(BDummyS,ABaseAddr-BWrAddr); for BIndex:=1 to Length(BDummyS) do BDummyS[BIndex]:=#0;
   ADataBin:=BDummyS+ADataBin;
   ABaseAddr:=BWrAddr;
   end;
  if Length(ADataBin)<(BWrAddr-ABaseAddr+Length(BDataBin)) then
   begin
   SetLength(BDummyS,BWrAddr-ABaseAddr+Length(BDataBin)-Length(ADataBin)); for BIndex:=1 to Length(BDummyS) do BDummyS[BIndex]:=#0;
   ADataBin:=ADataBin+BDummyS;
   end;
  //SetLength(ADataBin,BWrAddr-ABaseAddr+Length(BDataBin));
  BIndex:=0; while BIndex<Length(BDataBin) do begin ADataBin[1+BWrAddr-ABaseAddr+BIndex]:=BDataBin[1+BIndex]; inc(BIndex); end;
  end
 else if BTypeS='02' then // Extended segment address
  begin
  BOffset:=0;
  for BIndex:=1 to BSize do BOffset:=(BOffset shl 8)+Ord(BDataBin[BIndex]);
  AHexOffset:=BOffset shl 4;
  end
 else if BTypeS='04' then // Extended linear address
  begin
  BOffset:=0;
  for BIndex:=1 to BSize do BOffset:=(BOffset shl 8)+Ord(BDataBin[BIndex]);
  AHexOffset:=BOffset shl 16;
  end
 else if BTypeS='03' then // Start segment address (to be loaded to IP)
  begin
  end
 else if BTypeS='01' then // End of file
  begin
  end
 else begin Result:='Unknown record type '+BTypeS+' at line '+IntToStr(ALineIdx); break; end;
 until TRUE;
End;

Function ProcessHexLineNoPad ( AFile : TStringList; ALineIdx : Integer; Var ABaseAddr, AHexOffset : Cardinal; Var ADataBin : string; Var AError : string ) : boolean;
Var
  BData         : string;
  BLenA         : Integer;
  BSizeS,
  BAddrS,
  BTypeS,
  BDataS        : string;
  BAddr,
  BSize         : Cardinal;
  BDataBin      : string;
  BOffset       : Cardinal;
  BIndex        : Integer;
  BWrAddr       : Cardinal;
Begin
 Result:=FALSE;
 BData:=AFile.Strings[ALineIdx];

 repeat
 DelFirstSpace(BData);
 if BData='' then begin Result:=TRUE; break; end;

 BAddr:=0;
 BLenA:=Length(BData);
 if BLenA<11 then begin AError:='Hex line is too short'; break; end;
 if BData[1]<>':' then begin AError:='Semicolon expected at the begin of line'; break; end;
 BSizeS:=Copy(BData,2,2);
 BAddrS:=Copy(BData,4,4);
 BTypeS:=Copy(BData,8,2);
 BDataS:=Copy(BData,10,BLenA-11);
 if HexToDWordCheck(BAddrS,BAddr)=FALSE then begin AError:='Cannot convert address part to integer'; break; end;
 if HexToDWordCheck(BSizeS,BSize)=FALSE then begin AError:='Cannot convert size part to integer'; break; end;
 BDataBin:=StrHexToBin(BDataS);
 if Length(BDataBin)<>BSize then begin AError:='Invalid data size'; break;
  end;
 if BTypeS='00' then
  begin
  BWrAddr:=AHexOffset+BAddr;
  if ADataBin='' then ABaseAddr:=BWrAddr
  else if BWrAddr<ABaseAddr then break;
  if Length(ADataBin)<(BWrAddr-ABaseAddr) then break;
  SetLength(ADataBin,BWrAddr-ABaseAddr+Length(BDataBin));
  BIndex:=0; while BIndex<Length(BDataBin) do begin ADataBin[1+BWrAddr-ABaseAddr+BIndex]:=BDataBin[1+BIndex]; inc(BIndex); end;
  end
 else if BTypeS='02' then
  begin
  BOffset:=0;
  for BIndex:=1 to BSize do BOffset:=(BOffset shl 8)+Ord(BDataBin[BIndex]);
  AHexOffset:=BOffset shl 4;
  end
 else if BTypeS='03' then // Seems to be a starting point (just ignore this)
  begin
  end
 else if BTypeS='01' then
  begin
  end
 else begin AError:='Unknown record type '+BTypeS+' at line '+IntToStr(ALineIdx); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function HexFileParse ( AFile : TStringList; Out ABaseAddr : Cardinal; Out ADataBin : string ) : string;
Var
  BLineIdx      : Integer;
  BHexOffset    : Cardinal;
Begin
 Result:='';
 ABaseAddr:=0; ADataBin:='';
 BHexOffset:=0;
 BLineIdx:=0;
 while BLineIdx<AFile.Count do
  begin
  Result:=ProcessHexLine(AFile,BLineIdx,ABaseAddr,BHexOffset,ADataBin);
  if Result<>'' then begin Result:=IntToStr(BLineIdx+1)+' '+Result; break; end;
  inc(BLineIdx);
  end;
End;

Function HexFileParse ( AFile : TStringList; Var ALineIdx : Integer; Var AHexOffset : Cardinal; Out ABaseAddr : Cardinal; Out ADataBin : string ) : string;
Begin
 Result:='';
 ABaseAddr:=0; ADataBin:='';
 AHexOffset:=0;
 while ALineIdx<AFile.Count do
  begin
  if ProcessHexLineNoPad(AFile,ALineIdx,ABaseAddr,AHexOffset,ADataBin,Result)=FALSE then break;
  inc(ALineIdx);
  end;
End;

Procedure HexFileAddData ( ABaseAddr : Cardinal; Const AData : string; AHex : TStringList );
Var
  BAddrThis,
  BAddrPrev : Cardinal;
  BDataIdx  : Cardinal;
  BChecksum : byte;
  BData     : byte;
  BByteCnt  : Cardinal;
  BOffset   : Cardinal;
  BDummyS   : string;
  BWrOffset : boolean;
Begin
 BAddrThis:=ABaseAddr; BAddrPrev:=BAddrThis;
 BWrOffset:=(BAddrThis and $FFFF0000)<>0;
 BDataIdx:=0; BByteCnt:=0;
 BCheckSum:=$00;
 BDummyS:='';
 while BDataIdx<Length(AData) do
  begin
  BData:=Byte(AData[1+BDataIdx]);
  inc(BCheckSum,BData);
  BDummyS:=BDummyS+IntToHex(BData,2);
  inc(BDataIdx);
  inc(BByteCnt);
  inc(BAddrThis);
  if (BByteCnt>=16) or ((BAddrThis mod 16)=0) then
   begin
   if BWrOffset then
    begin
    BOffset:=(BAddrPrev and $FFFF0000) shr 4;
    AHex.Append(':03000002'+IntToHex(BOffset,6)+'00');
    end;
   BCheckSum:=BCheckSum+BByteCnt+((BAddrPrev shr 8) and $FF)+(BAddrPrev and $FF);
   BCheckSum:=(BCheckSum xor $FF)+$01;
   AHex.Append(':'+IntToHex(BByteCnt,2)+IntToHex(BAddrPrev and $FFFF,4)+'00'+BDummyS+IntToHex(BCheckSum,2));
   BWrOffset:=(BAddrThis and $FFFF0000)<>(BAddrPrev and $FFFF0000); // Write offset (code 02)
   BCheckSum:=$00; BDummyS:=''; BAddrPrev:=BAddrThis;
   BByteCnt:=0;
   end;
  end;
 if BByteCnt<>0 then
  begin
  while BByteCnt<16 do
   begin
   BData:=$FF;
   inc(BCheckSum,BData);
   BDummyS:=BDummyS+IntToHex(BData,2);
   inc(BByteCnt);
   end;
  BCheckSum:=BCheckSum+BByteCnt+(BAddrPrev shr 8)+(BAddrPrev and $FF);
  BCheckSum:=(BCheckSum xor $FF)+$01;
  AHex.Append(':'+IntToHex(BByteCnt,2)+IntToHex(BAddrPrev,4)+'00'+BDummyS+IntToHex(BCheckSum,2));
  end;
End;

Procedure HexFileCreate ( ABaseAddr : Cardinal; Const AData : string; AHex : TStringList );
Begin
 AHex.Clear;
 HexFileAddData(ABaseAddr,AData,AHex);
 AHex.Append(':00000001FF');
End;

Procedure MemFileCreate ( AFullSize : Cardinal; AHwWidth : byte; Const AData : string; AMem : TStringList );
Var
  BRowCnt   : Cardinal;
  BDataAddr : Cardinal;
  BData     : byte;
  BByteIdx,
  BColIdx,
  BRowIdx   : Cardinal;
  BDataS    : string;
Begin
 AMem.Clear;
 BRowCnt:=AFullSize div (8*AHwWidth);
 BRowIdx:=0;
 while BRowIdx<BRowCnt do
  begin
  BDataS:='';
  BColIdx:=0;
  while BColIdx<8 do
   begin
   if BDataS<>'' then BDataS:=BDataS+' ';
   BByteIdx:=0;
   while BByteIdx<AHwWidth do
    begin
    BDataAddr:=BRowIdx*AHwWidth*8+BColIdx*AHwWidth+AHwWidth-1-BByteIdx;
    if BDataAddr<Length(AData) then BData:=Ord(AData[1+BDataAddr]) else BData:=0;
    BDataS:=BDataS+IntToHex(BData,2);
    inc(BByteIdx);
    end;
   inc(BColIdx);
   end;
  AMem.Append(BDataS);
  inc(BRowIdx);
  end;
End;

// Returns FALSE if not Efinix HEX. Otherwise see the error code
Function TryParseEfinixHex ( AFile : TStringList; Out ADataBin : string; Out AErrorS : string ) : boolean;
Var
  BLineIdx  : Integer;
  BDataS    : string;
  BDataB    : byte;
Begin
 Result:=FALSE;
 repeat
 ADataBin:=''; AErrorS:='';
 BLineIdx:=0;
 while BLineIdx<AFile.Count do
  begin
  BDataS:=AFile.Strings[BLineIdx]; DelFirstLastSpace(BDataS);
  if BDataS<>'' then
   begin
   if Result=FALSE then // First non-empty line
    begin
    if Length(BDataS)<>2 then break;
    Result:=TRUE;
    end;
   if HexToByteCheck(BDataS,BDataB)=FALSE then begin AErrorS:='Conversion error in line '+IntToStr(BLineIdx+1)+' of Efinix HEX file'; break; end;
   ADataBin:=ADataBin+Chr(BDataB);
   end;
  inc(BLineIdx);
  end;
 until TRUE;
End;

end.

