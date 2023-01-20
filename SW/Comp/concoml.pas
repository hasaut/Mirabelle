unit ConComL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Function StringToFloat ( Const ADataS : string; Out ADataF : Double ) : boolean;
Function StringToInteger ( Const ADataS : string; Out ADataI : Integer ) : boolean;
Function StringToInt64 ( Const ADataS : string; Var ADataI : Int64 ) : boolean;
Procedure DelFirstSpace ( Var S : string );
Procedure DelLastSpace ( Var S : string );
Procedure DelFirstLastSpace ( Var ADataS : string );
Procedure DelInnerSpaces ( Var ADataS : string );
Procedure DelLastCrlf ( Var S : string );
Procedure DelFirstSlash ( Var S : string );
Procedure DelLastSlash ( Var S : string );
Function AddSpacesResL ( Const AStr : string; ALen : Integer ) : string;
Function AddSpacesResR ( Const AStr : string; ALen : Integer ) : string;
Procedure AddSpacesVarL ( Var AStr : string; ALen : Integer );
Procedure AddSpacesVarR ( Var AStr : string; ALen : Integer );
Function StrPadResL ( Const AStr : string; ASym : char; ALen : Integer ) : string;

Function ReadParamStr ( Var ASrc : string ) : string;
Function ReadParamStrInv ( Var ASrc : string ) : string;
Function ReadParamStr ( Var ASrc : string; Const ASeparator : string ) : string;
Function InvertParamsOrder ( Const AOrig : string ) : string;
Function ReadParamStr1Space ( Var ASrc : string ) : string;
Function ReadTillS ( Var ASrc, ADst : string; Const ASeparator : string ) : boolean; Overload;
Function ReadTillS ( Var ASrc : string; Const ASeparator : string ) : string; Overload;
Function ReadTillSNoDel ( Var ASrc : string; Const ASeparator : string ) : string;
Function ReadTillC ( Var ASrc : string; Out ADst : string; ASeparator : char ) : boolean; Overload;
Function ReadTillC ( Var ASrc : string; ASeparator : char ) : string; Overload;
Function ReadTillC_NoSp ( Var ASrc : string; ASeparator : char ) : string; Overload;
Function ReadTillCNoDel ( Var ASrc : string; ASeparator : char ) : string;
Function ReadTillCDel ( Var ASrc : string; Out ADst : string; ASeparator : char ) : boolean; Overload;
Function ReadTillCB ( Var ASrc : string; ASeparator : char ) : string; // Same as ReadTillC, but ignores brackets like "{", "}"
Function ReadTillCQ ( Var ASrc : string; ASeparator : char ) : string; // Same as ReadTillC, but ignores quotes
Function ReadTillCX ( Var ASrc : string; ASeparator : char; Const ABrackets : string ) : string; // Same as ReadTillS, but ignores brackets
Function ReadTillClosing ( Var ASrc : string; Out ADst : string ) : boolean;
Function ReadInside ( Var ASrc : string; APos : Integer; Out ADst : string ) : boolean;
Function ReadTillA ( Var ASrc : string; Const ASeparator : string ) : string;
Function ReadTokenS ( Const ASrc : string; Var APosS, APosE : Integer; Const ASeparator : string ) : string;
Procedure TrimA ( Var AData : string; Const AMask : string );
Function PosSkipQ ( ACmp : char; Const AReadS : string ) : Integer; // Like Pos but skip quotes

Function Bit4ToHex ( AData : byte ) : char;
Function HexToBit4 ( AData : char ) : Byte;
Function Hex32ToInt ( Const AStr : string ) : LongWord; Overload;
Function Hex32ToInt ( Const AStr : string; Out AData : Cardinal ) : boolean; Overload;
Function StrBit32ToInt ( Const AStr : string ) : LongWord;

Function StrBinToHex ( Const ADataBin : string ) : string;
Function StrHexToBin ( Const ADataHex : string ) : string;

Function ByteToHex ( AData : byte ) : string;
Function WordToHex ( AData : word ) : string;
Function HexToByteCheck ( Const AStr : string; Out AData : Byte ) : boolean;
Function HexToWordCheck ( Const AStr : string; Out AData : Word ) : boolean;
Function HexToDWordCheck ( Const AStr : string; Out AData : Cardinal ) : boolean;
Function HexToQWordCheck ( Const AStr : string; Out AData : QWord ) : boolean;
Function Bin2ToByteCheck ( Const AStr : string; Out AData : Byte ) : boolean;
Function StringToCardinal ( Const AStr : string; Var AData : Cardinal ) : boolean;
Function CardinalToString ( AData : Cardinal ) : string;
Function BinToInt ( Const AStr : string; Out AData : Cardinal ) : boolean;
Procedure StrReplaceC ( Var ADataS : string; ACharToSearch, ACharToWrite : char );

Procedure SplitFilename ( Const AFullName : string; Out APath, AName, AExt : String );
Function ExtractFilePathA ( Const AFullName : string ) : string;

Function AverageColor1 ( AColorA, AColorB : Cardinal; ALen, APos : Integer ) : Cardinal;

Function StrSkipSpace ( Const AOrigin : string; Var ADataC : char; Var AIndex : integer; ALen : Integer ) : boolean;
Function StrReadTill ( Const AOrigin : string; Var AResult : string; Const ATerminator : string; Var ADataC : char; Var AIndex : integer; ALen : Integer ) : boolean;
Function StrInList ( Const AStr : string; Const AList : string ) : boolean;
Function GetStrIndex ( Const AStrS : string; Const AList : string ) : Integer;
Function CheckTag ( Const ATag : string; Var ALine : string ) : string;
Function ReadForceParam ( AList : TStringList; Const AValueName : string; Const ADefault : string ) : string;

Function WordToDouble ( ADataW : word ) : Double;

Function PosNext ( Const ASubstr, AStr : string; AStartPos : Integer ) : Integer;
Function PosBack ( Const ASubstr, AStr : string ) : Integer;
Function PosBackC ( ACmp : char; Const AStr : string; AStartPos : Integer ) : Integer;
Function TryStrToInt0x ( Const ADataS : string; Out ADataV : Int64 ) : boolean;
Function TryStrToInt0x ( Const ADataS : string; Out ADataV : Cardinal ) : boolean;
Function FirstMultiPos ( Const ASubstrList : string; Const AStr : string ) : Integer;

Procedure XchgStrings ( Var AStrA, AStrB : string );

Procedure WDReset;

Function StrToData ( Const ADataS : string ) : Cardinal;
Function DataDToStr ( AData : Cardinal ) : string;
Function DataWToStr ( AData : Cardinal ) : string;

Function CheckTabs ( AList : TStringList ) : boolean;
Procedure CorrectTabs ( AList : TStringList; AStep : Integer );
Procedure DelLastSpace ( AList : TStringList );
Procedure ListReplace ( AList : TStringList; Const ASrc, ADst : string );

Function DirList ( Const APath : string; Const AMask : string; AList : TStringList ) : boolean;
Procedure FileList ( Const APath : string; AList : TStringList );
Procedure FileList ( Const APath, AMask : string; AList : TStringList );

Function ResolveCMod ( Const AOrig : string ) : string;

Procedure CopyPrjParams ( ADst, ASrc : TStringList; Const AName : string; Const AInsIdx : string );
Procedure ReplaceParam ( Var AData : string; Const AOldParam, ANewParam : string );

Function CheckValue ( Const ANameS : string; Const AReadS : string; Out AValue : string ) : boolean;
Function CheckValue ( Const ANameS : string; Const AReadS : string; Var AValue : Cardinal ) : boolean;

Function DWordAsStr ( AData : Cardinal ) : string;
Function StrAsDWord ( Const ADataS : string; AIndex : Integer ) : Cardinal;
Function StrAsDWord ( Const ADataS : string; Var AIndex : Integer; Out AData : Cardinal ) : boolean; // Index from 0

Procedure CorrectAscii ( Var ADataS : string );

implementation

Procedure WDReset;
Begin
End;

Function StringToFloat ( Const ADataS : string; Out ADataF : Double ) : boolean;
Var
  BDataS        : string;
  BDataF        : Double;
  BDataC        : char;
  BMul          : Double;
Begin
 Result:=FALSE;
 BDataS:=ADataS;
 ADataF:=0; BDataF:=0;

 repeat
 if BDataS='' then break;
 while BDataS<>'' do
  begin
  BDataC:=BDataS[1];
  if BDataC in ['0'..'9'] then BDataF:=(BDataF*10)+(Ord(BDataC)-Ord('0'))
  else break;
  Delete(BDataS,1,1);
  end;
 if BDataS='' then begin ADataF:=BDataF; Result:=TRUE; break; end;
 if BDataS[1]<>'.' then break;
 Delete(BDataS,1,1);
 BMul:=0.1;
 while BDataS<>'' do
  begin
  BDataC:=BDataS[1];
  if BDataC in ['0'..'9'] then BDataF:=BDataF+BMul*(Ord(BDataC)-Ord('0'))
  else break;
  BMul:=BMul*0.1;
  Delete(BDataS,1,1);
  end;
 if BDataS<>'' then break;
 ADataF:=BDataF;
 Result:=TRUE;
 until TRUE;
End;

Function StringToInteger ( Const ADataS : string; Out ADataI : Integer ) : boolean;
Var
  BDataS : string;
  BDataI : Integer;
  BDataC : char;
Begin
 Result:=FALSE;
 BDataS:=ADataS;
 ADataI:=0; BDataI:=0;

 repeat
 if BDataS='' then break;
 while BDataS<>'' do
  begin
  if BDataI>=($7FFFFFFF-10) then break;
  BDataC:=BDataS[1];
  if BDataC in ['0'..'9'] then BDataI:=(BDataI*10)+(Ord(BDataC)-Ord('0'))
  else break;
  Delete(BDataS,1,1);
  end;
 if BDataS<>'' then break;
 ADataI:=BDataI;
 Result:=TRUE;
 until TRUE;
End;

Function StringToInt64 ( Const ADataS : string; Var ADataI : Int64 ) : boolean;
Var
  BDataS : string;
  BDataI : Int64;
  BDataC : char;
Begin
 Result:=FALSE;
 BDataS:=ADataS;
 BDataI:=0;

 repeat
 if BDataS='' then break;
 while BDataS<>'' do
  begin
  if BDataI>=($7FFFFFFFFFFFFFFF-10) then break;
  BDataC:=BDataS[1];
  if BDataC in ['0'..'9'] then BDataI:=(BDataI*10)+(Ord(BDataC)-Ord('0'))
  else break;
  Delete(BDataS,1,1);
  end;
 if BDataS<>'' then break;
 ADataI:=BDataI;
 Result:=TRUE;
 until TRUE;
End;

Procedure DelFirstSpace ( Var S : string );
Begin
 while S<>'' do
  begin
  if S[1]=' ' then
  else break;
  Delete(S,1,1);
  end;
End;

Procedure DelLastSpace ( Var S : string );
Var
	i	: Integer;
Begin
 while S<>'' do
  begin
  i:=Length(S);
  if S[i]=' ' then
  else break;
  delete(S,i,1);
  end;
End;

Procedure DelFirstLastSpace ( Var ADataS : string );
Begin
 DelFirstSpace(ADataS);
 DelLastSpace(ADataS);
End;

Procedure DelInnerSpaces ( Var ADataS : string );
Var
  BPos  : Integer;
Begin
 repeat
 BPos:=Pos(#32,ADataS);
 if BPos=0 then break;
 Delete(ADataS,BPos,1);
 until FALSE;
End;

Procedure DelLastCrlf ( Var S : string );
Var
	i	: Integer;
Begin
 while S<>'' do
  begin
  i:=Length(S);
  if S[i]<>#10 then break;
  delete(S,i,1);
  end;
 while S<>'' do
  begin
  i:=Length(S);
  if S[i]<>#13 then break;
  delete(S,i,1);
  end;
End;


Procedure DelFirstSlash ( Var S : string );
Begin
 while S<>'' do
  begin
  if S[1]='/' then
  else if S[1]='\' then
  else break;
  Delete(S,1,1);
  end;
End;

Procedure DelLastSlash ( Var S : string );
Var
	i	: Integer;
Begin
 while S<>'' do
  begin
  i:=Length(S);
  if S[i]='/' then
  else if S[i]='\' then
  else break;
  delete(S,i,1);
  end;
End;

Function AddSpacesResL ( Const AStr : string; ALen : Integer ) : string;
Begin
 Result:=AStr;
 while Length(Result)<ALen do Result:=' '+Result;
End;

Function AddSpacesResR ( Const AStr : string; ALen : Integer ) : string;
Begin
 Result:=AStr;
 while Length(Result)<ALen do Result:=Result+' ';
End;

Procedure AddSpacesVarL ( Var AStr : string; ALen : Integer );
Begin
 while Length(AStr)<ALen do AStr:=' '+AStr;
End;

Procedure AddSpacesVarR ( Var AStr : string; ALen : Integer );
Begin
 while Length(AStr)<ALen do AStr:=AStr+' ';
End;

Function StrPadResL ( Const AStr : string; ASym : char; ALen : Integer ) : string;
Begin
 Result:=AStr;
 while Length(Result)<ALen do Result:=ASym+Result;
End;

Function ReadParamStr ( Var ASrc : string ) : string;
Var
  BDataC : char;
Begin
 Result:='';
 DelFirstSpace(ASrc); // Do not add DelLastSpace
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1]; Delete(ASrc,1,1);
  if BDataC=' ' then break;
  Result:=Result+BDataC;
  end;
 DelFirstSpace(ASrc);
End;

Function ReadParamStrInv ( Var ASrc : string ) : string;
Var
  BDataC        : char;
  BLen          : Integer;
Begin
 Result:='';
 DelLastSpace(ASrc);
 BLen:=Length(ASrc);
 while ASrc<>'' do
  begin
  BDataC:=ASrc[BLen]; Delete(ASrc,BLen,1);
  if BDataC=' ' then break;
  Result:=BDataC+Result;
  dec(BLen);
  end;
 DelLastSpace(ASrc);
End;

Function ReadParamStr ( Var ASrc : string; Const ASeparator : string ) : string;
Var
  BDataC : char;
Begin
 Result:='';
 DelFirstSpace(ASrc); // Do not add DelLastSpace
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1]; Delete(ASrc,1,1);
  if Pos(BDataC,ASeparator)<>0 then break;
  Result:=Result+BDataC;
  end;
 //if ASrc<>'' then Delete(ASrc,1,1);
 DelFirstSpace(ASrc);
End;

Function InvertParamsOrder ( Const AOrig : string ) : string;
Var
  BReadS,
  BParamS   : string;
Begin
 BReadS:=AOrig;
 Result:='';
 repeat
 BParamS:=ReadParamStr(BReadS);
 if BParamS='' then break;
 Result:=BParamS+' '+Result;
 until FALSE;
End;

Function ReadParamStr1Space ( Var ASrc : string ) : string;
Var
  BDataC : char;
Begin
 Result:='';
 DelFirstSpace(ASrc); // Do not add DelLastSpace
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1]; Delete(ASrc,1,1);
  if BDataC=' ' then break;
  Result:=Result+BDataC;
  end;
End;

// APos from 0 (not from 1)
Function ReadTokenS ( Const ASrc : string; Var APosS, APosE : Integer; Const ASeparator : string ) : string;
Var
  BDataC        : char;
Begin
 Result:='';
 repeat
 while APosS<Length(ASrc) do
  begin
  if ASrc[APosS+1]<>#32 then break;
  inc(APosS);
  end;
 APosE:=APosS;
 if APosE>=Length(ASrc) then break;
 BDataC:=ASrc[APosE+1]; Inc(APosE);
 Result:=BDataC;
 if Pos(BDataC,ASeparator)<>0 then break;
 while APosE<Length(ASrc) do
  begin
  BDataC:=ASrc[APosE+1];
  if BDataC=#32 then break;
  if Pos(BDataC,ASeparator)<>0 then break;
  Result:=Result+BDataC;
  inc(APosE);
  end;
 until TRUE;
End;

Procedure TrimA ( Var AData : string; Const AMask : string );
Var
  BLen      : Integer;
Begin
 repeat
 if AData='' then break;
 if Pos(AData[1],AMask)=0 then break;
 Delete(AData,1,1);
 until FALSE;

 repeat
 if AData='' then break;
 BLen:=Length(AData);
 if Pos(AData[BLen],AMask)=0 then break;
 Delete(AData,BLen,1);
 until FALSE;
End;

Function ReadTillS ( Var ASrc, ADst : string; Const ASeparator : string ) : boolean;
Var
  BPos  : Integer;
Begin
 Result:=FALSE;
 ADst:='';

 BPos:=pos(ASeparator,ASrc);
 if BPos=0 then
  begin
  ADst:=ASrc;
  ASrc:='';
  end
 else
  begin
  ADst:=Copy(ASrc,1,BPos-1);
  Delete(ASrc,1,BPos+Length(ASeparator)-1);
  Result:=TRUE;
  end;
End;

Function ReadTillS ( Var ASrc : string; Const ASeparator : string ) : string;
Var
  BPos   : Integer;
Begin
 Result:='';
 repeat
 if ASrc='' then break;
 BPos:=Pos(ASeparator,ASrc);
 if BPos=0 then begin Result:=ASrc; ASrc:=''; break; end;
 Result:=Copy(ASrc,1,BPos-1);
 Delete(ASrc,1,BPos+Length(ASeparator)-1);
 until TRUE;
End;

Function ReadTillSNoDel ( Var ASrc : string; Const ASeparator : string ) : string;
Var
  BPos   : Integer;
Begin
 Result:='';
 repeat
 if ASrc='' then break;
 BPos:=Pos(ASeparator,ASrc);
 if BPos=0 then begin Result:=ASrc; ASrc:=''; break; end;
 Result:=Copy(ASrc,1,BPos-1);
 Delete(ASrc,1,BPos-1);
 until TRUE;
End;

Function ReadTillC ( Var ASrc : string; Out ADst : string; ASeparator : char ) : boolean;
Var
  BData : char;

Begin
 Result:=FALSE;
 ADst:='';

 while ASrc<>'' do
  begin
  BData:=ASrc[1];
  if BData=ASeparator then
   begin
   Result:=TRUE;
   break;
   end;
  ADst:=ADst+BData;
  Delete(ASrc,1,1);
  end;
End;

Function ReadTillCDel ( Var ASrc : string; Out ADst : string; ASeparator : char ) : boolean;
Var
  BPos  : Integer;
Begin
 Result:=FALSE;
 ADst:='';

 repeat
 if ASrc='' then break;
 BPos:=Pos(ASeparator,ASrc);
 if BPos=0 then begin ADst:=ASrc; ASrc:=''; Result:=TRUE; break; end;
 ADst:=Copy(ASrc,1,BPos-1);
 Delete(ASrc,1,BPos);
 Result:=TRUE;
 until TRUE;
End;

Function ReadTillC ( Var ASrc : string; ASeparator : char ) : string;
Var
  BPos   : Integer;
Begin
 Result:='';
 repeat
 if ASrc='' then break;
 BPos:=Pos(ASeparator,ASrc);
 if BPos=0 then begin Result:=ASrc; ASrc:=''; break; end;
 Result:=Copy(ASrc,1,BPos-1);
 Delete(ASrc,1,BPos);
 until TRUE;
End;

Function ReadTillC_NoSp ( Var ASrc : string; ASeparator : char ) : string;
Begin
 Result:=ReadTillC(ASrc,ASeparator);
 DelFirstSpace(Result); DelLastSpace(Result);
End;

Function ReadTillCNoDel ( Var ASrc : string; ASeparator : char ) : string;
Var
  BPos   : Integer;
Begin
 Result:='';
 repeat
 if ASrc='' then break;
 BPos:=Pos(ASeparator,ASrc);
 if BPos=0 then begin Result:=ASrc; ASrc:=''; break; end;
 Result:=Copy(ASrc,1,BPos-1);
 Delete(ASrc,1,BPos-1);
 until TRUE;
End;

Function ReadTillCB ( Var ASrc : string; ASeparator : char ) : string; // Same as ReadTillS, but ignores brackets like "{", "}"
Var
  BDataC        : char;
  BLevel        : Integer;
Begin
 Result:='';
 BLevel:=0;
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1];
  Delete(ASrc,1,1);
  if BDataC='{' then Inc(BLevel)
  else if BDataC='}' then Dec(BLevel)
  else if (BDataC=ASeparator) and (BLevel=0) then break;
  Result:=Result+BDataC;
  end;
End;

Function ReadTillCQ ( Var ASrc : string; ASeparator : char ) : string; // Same as ReadTillC, but ignores quotes
Var
  BDataC        : char;
  BLevel        : char;
Begin
 Result:='';
 BLevel:=#0;
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1];
  Delete(ASrc,1,1);
  if BDataC in [#34, #39] then
   begin
   if BLevel=#0 then BLevel:=BDataC
   else if BLevel=BDataC then BLevel:=#0;
   end;
  if (BDataC=ASeparator) and (BLevel=#0) then break;
  Result:=Result+BDataC;
  end;
End;

Function ReadTillCX ( Var ASrc : string; ASeparator : char; Const ABrackets : string ) : string; // Same as ReadTillS, but ignores brackets
Var
  BDataC        : char;
  BLevel        : string;
Begin
 Result:='';
 BLevel:='';
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1];
  Delete(ASrc,1,1);
  if Pos(BDataC,ABrackets)<>0 then BLevel:=BLevel+BDataC
  else if (BDataC=']') and (BLevel<>'') and (BLevel[Length(BLevel)]='[') then Delete(BLevel,Length(BLevel),1)
  else if (BDataC='}') and (BLevel<>'') and (BLevel[Length(BLevel)]='{') then Delete(BLevel,Length(BLevel),1)
  else if (BDataC=')') and (BLevel<>'') and (BLevel[Length(BLevel)]='(') then Delete(BLevel,Length(BLevel),1)
  else if (BDataC=ASeparator) and (BLevel='') then break;
  Result:=Result+BDataC;
  end;
End;

Function ReadTillClosing ( Var ASrc : string; Out ADst : string ) : boolean;
Var
  BDataC        : char;
  BLevel        : Integer;
Begin
 Result:=FALSE;
 ADst:='';
 BLevel:=1;
 while ASrc<>'' do
  begin
  BDataC:=ASrc[1]; Delete(ASrc,1,1);
  ADst:=ADst+BDataC;
  if BDataC='(' then Inc(BLevel)
  else if BDataC=')' then
   begin
   Dec(BLevel);
   if BLevel=0 then begin Result:=TRUE; break; end;
   end;
  end;
End;

Function ReadInside ( Var ASrc : string; APos : Integer; Out ADst : string ) : boolean;
Var
  BDataC        : char;
  BLevel        : string;
Begin
 Result:=FALSE;
 BLevel:=ASrc[APos]; Delete(ASrc,APos,1);
 ADst:=BLevel;
 while APos<=Length(ASrc) do
  begin
  BDataC:=ASrc[APos]; Delete(ASrc,APos,1);
  ADst:=ADst+BDataC;
  if BDataC in ['(', '[', '{', '<'] then BLevel:=BLevel+BDataC
  else if BDataC=')' then
   begin
   if BLevel[Length(BLevel)]='(' then Delete(BLevel,Length(BLevel),1)
   else break;
   end
  else if BDataC=']' then
   begin
   if BLevel[Length(BLevel)]='[' then Delete(BLevel,Length(BLevel),1)
   else break;
   end
  else if BDataC='}' then
   begin
   if BLevel[Length(BLevel)]='{' then Delete(BLevel,Length(BLevel),1)
   else break;
   end
  else if BDataC='>' then
   begin
   if BLevel[Length(BLevel)]='<' then Delete(BLevel,Length(BLevel),1)
   else break;
   end;
  if BLevel='' then begin Result:=TRUE; break; end;
  end;
End;

Function ReadTillA ( Var ASrc : string; Const ASeparator : string ) : string;
Var
  BDataC        : char;
Begin
 Result:='';
 repeat
 if ASrc='' then break;
 BDataC:=ASrc[1];
 if Pos(BDataC,ASeparator)<>0 then break;
 Delete(ASrc,1,1);
 Result:=Result+BDataC;
 until FALSE;
End;

Function PosSkipQ ( ACmp : char; Const AReadS : string ) : Integer; // Like Pos but skip quotes
Var
  BQ,
  BReadC    : char;
  BIndex    : Integer;
Begin
 Result:=0;
 BQ:=#0;
 BIndex:=0;
 while BIndex<Length(AReadS) do
  begin
  BReadC:=AReadS[1+BIndex];
  if BQ=#0 then
   begin
   if BReadC=ACmp then begin Result:=1+BIndex; break; end;
   if (BReadC=#39) or (BReadC='"') then BQ:=BReadC;
   end
  else if BReadC=BQ then BQ:=#0;
  inc(BIndex);
  end;
End;

Function Bit4ToHex ( AData : byte ) : char;
Begin
 AData:=AData and $0F;
 if AData in [0..9] then Result:=chr(AData+ord('0'))
 else Result:=chr(AData-10+ord('A'));
End;

Function HexToBit4 ( AData : char ) : Byte;
Begin
 if AData in ['A'..'Z'] then AData:=chr(ord(AData)+(ord('a')-ord('A')));
 if AData in ['a'..'f'] then Result:=ord(AData)-ord('a')+10
 else Result:=ord(AData)-ord('0');
End;

Function Hex32ToInt ( Const AStr : string ) : LongWord;
Var
  BStr          : string;

Begin
 BStr:=AStr;
 Result:=0;
 while BStr<>'' do
  begin
  Result:=Result shl 4;
  Result:=Result+HexToBit4(BStr[1]);
  Delete(BStr,1,1);
  end;
End;

Function Hex32ToInt ( Const AStr : string; Out AData : Cardinal ) : boolean;
Var
  BStr          : string;
  BData         : Cardinal;
  BChar         : char;

Begin
 BStr:=AStr;
 AData:=0;
 Result:=BStr<>'';
 BData:=0;
 while BStr<>'' do
  begin
  BData:=BData shl 4;
  BChar:=BStr[1];
  if BChar in ['0'..'9','a'..'f','A'..'F'] then
  else
   begin
   Result:=FALSE;
   break;
   end;
  BData:=BData+HexToBit4(BStr[1]);
  Delete(BStr,1,1);
  end;
 if Result then AData:=BData;
End;

Function StrBit32ToInt ( Const AStr : string ) : LongWord;
Var
  BIndex        : Integer;
Begin
 Result:=0;
 BIndex:=0;
 while BIndex<Length(AStr) do
  begin
  if AStr[1+BIndex]=#32 then break;
  Result:=Result shl 1;
  if AStr[1+Bindex]='1' then Result:=Result or $1;
  inc(BIndex);
  end;
End;

Function StrBinToHex ( Const ADataBin : string ) : string;
Var
  BIndex        : Integer;
  BData         : byte;
Begin
 Result:='';
 SetLength(Result,Length(ADataBin)*2);
 for BIndex:=0 to Length(ADataBin)-1 do
  begin
  BData:=Ord(ADataBin[1+BIndex]);
  Result[1+BIndex*2+0]:=Bit4ToHex(BData shr 4);
  Result[1+BIndex*2+1]:=Bit4ToHex(BData);
  end;
End;

Function StrHexToBin ( Const ADataHex : string ) : string;
Var
  BIndex        : Integer;
  BData         : byte;
Begin
 Result:='';
 SetLength(Result,Length(ADataHex) div 2);
 for BIndex:=0 to Length(Result)-1 do
  begin
  BData:=(HexToBit4(ADataHex[1+BIndex*2+0]) shl 4)+HexToBit4(ADataHex[1+BIndex*2+1]);
  Result[1+BIndex]:=Chr(BData);
  end;
End;



Function ByteToHex ( AData : byte ) : string;
Begin
 Result:=Bit4ToHex(AData shr 4)+Bit4ToHex(AData);
End;

Function WordToHex ( AData : word ) : string;
Begin
 Result:=Bit4ToHex(AData shr 12)+Bit4ToHex(AData shr 8)+Bit4ToHex(AData shr 4)+Bit4ToHex(AData);
End;

Function HexToByteCheck ( Const AStr : string; Out AData : byte ) : boolean;
Var
  BStr          : string;
  i             : Integer;
  BChar         : char;
  BData         : byte;
  BLength       : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 BLength:=Length(BStr);
 if BLength>2 then break;
 while BLength<>2 do
  begin
  BStr:='0'+BStr;
  inc(BLength);
  end;

 BData:=0;
 i:=0;
 while i<BLength do
  begin
  BChar:=BStr[i+1];
  if BChar in ['0'..'9','a'..'f','A'..'F'] then
  else break;
  BData:=(BData shl 4) + HexToBit4(BChar);
  inc(i);
  end;
 if i<>BLength then break;

 AData:=BData;
 Result:=TRUE;
 until TRUE;
End;

Function HexToWordCheck ( Const AStr : string; Out AData : word ) : boolean;
Var
  BStr          : string;
  i             : Integer;
  BChar         : char;
  BData         : word;
  BLength       : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 BLength:=Length(BStr);
 if BLength>4 then break;
 while BLength<>4 do
  begin
  BStr:='0'+BStr;
  inc(BLength);
  end;

 BData:=0;
 i:=0;
 while i<BLength do
  begin
  BChar:=BStr[i+1];
  if BChar in ['0'..'9','a'..'f','A'..'F'] then
  else break;
  BData:=(BData shl 4) + HexToBit4(BChar);
  inc(i);
  end;
 if i<>BLength then break;

 AData:=BData;
 Result:=TRUE;
 until TRUE;
End;

Function HexToDWordCheck ( Const AStr : string; Out AData : Cardinal ) : boolean;
Var
  BStr          : string;
  i             : Integer;
  BChar         : char;
  BData         : Cardinal;
  BLength       : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 BLength:=Length(BStr);
 if BLength>8 then break;
 while BLength<>8 do
  begin
  BStr:='0'+BStr;
  inc(BLength);
  end;

 BData:=0;
 i:=0;
 while i<BLength do
  begin
  BChar:=BStr[i+1];
  if BChar in ['0'..'9','a'..'f','A'..'F'] then
  else break;
  BData:=(BData shl 4) + HexToBit4(BChar);
  inc(i);
  end;
 if i<>BLength then break;

 AData:=BData;
 Result:=TRUE;
 until TRUE;
End;

Function HexToQWordCheck ( Const AStr : string; Out AData : QWord ) : boolean;
Var
  BStr          : string;
  i             : Integer;
  BChar         : char;
  BData         : QWord;
  BLength       : Integer;

Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 BLength:=Length(BStr);
 if BLength>16 then break;
 while BLength<>16 do
  begin
  BStr:='0'+BStr;
  inc(BLength);
  end;

 BData:=0;
 i:=0;
 while i<BLength do
  begin
  BChar:=BStr[i+1];
  if BChar in ['0'..'9','a'..'f','A'..'F'] then
  else break;
  BData:=(BData shl 4) + HexToBit4(BChar);
  inc(i);
  end;
 if i<>BLength then break;

 AData:=BData;
 Result:=TRUE;
 until TRUE;
End;

Function Bin2ToByteCheck ( Const AStr : string; Out AData : byte ) : boolean;
Var
  BStr          : string;
  BIndex        : Integer;
  BChar         : char;
  BData         : byte;
  BLength       : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 BLength:=Length(BStr);
 if BLength>8 then break;
 while BLength<>8 do begin BStr:='0'+BStr; inc(BLength); end;

 BData:=0;
 BIndex:=0;
 while BIndex<BLength do
  begin
  BChar:=BStr[1+BIndex];
  if BChar in ['0'..'1'] then
  else break;
  BData:=(BData shl 1) + (Ord(BChar)-Ord('0'));
  inc(BIndex);
  end;
 if BIndex<>BLength then break;

 AData:=BData;
 Result:=TRUE;
 until TRUE;
End;

Procedure SplitFilename ( Const AFullName : string; Out APath, AName, AExt : String );
Var
  BLen    : Integer;
  BPosD,
  BPosS   : Integer;
Begin
 APath:=''; AName:=''; AExt:='';

 BLen:=Length(AFullName);
 BPosS:=BLen;
 while BPosS>0 do
  begin
  if (AFullName[BPosS]='/') or (AFullName[BPosS]='\') then break;
  dec(BPosS);
  end;
 BPosD:=BPosS+1;
 while BPosD<=BLen do
  begin
  if AFullName[BPosD]='.' then break;
  inc(BPosD);
  end;
 if BPosS<>0 then APath:=Copy(AFullName,1,BPosS-1);
 AName:=Copy(AFullName,BPosS+1,BPosD-BPosS-1);
 AExt:=Copy(AFullName,BPosD+1,BLen-BPosD);
End;

Function ExtractFilePathA ( Const AFullName : string ) : string;
Var
  BPath,
  BName,
  BExt   : string;
Begin
 BPath:=''; BName:=''; BExt:='';
 SplitFilename(AFullName,BPath,BName,BExt);
 Result:=BPath;
End;

Function StringToCardinal ( Const AStr : string; Var AData : Cardinal ) : boolean;
Var
  BDummyF       : Extended;

Begin
 Result:=FALSE;
 repeat
 if AStr='' then break;
 if TextToFloat(PChar(AStr),BDummyF,fvExtended)=FALSE then break;
 if Abs(BDummyF)>High(Cardinal) then break;
 if Abs(BDummyF)<>Round(BDummyF) then break;
 AData:=Round(BDummyF);
 Result:=TRUE;
 until TRUE;
End;

Function CardinalToString ( AData : Cardinal ) : string;
Begin
 Result:=IntToStr(AData mod 10);
 AData:=AData div 10;
 while AData<>0 do
  begin
  Result:=IntToStr(AData mod 10)+Result;
  AData:=AData div 10;
  end;
End;

Function BinToInt ( Const AStr : string; Out AData : Cardinal ) : boolean;
Var
  BStr  : string;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AData:=0;
 repeat
 if BStr='' then break;
 while BStr<>'' do
  begin
  case BStr[1] of
  '0': AData:=(AData shl 1)+0;
  '1': AData:=(AData shl 1)+1;
  else break;
  end; // case
  Delete(BStr,1,1);
  end;
 if BStr<>'' then break;
 Result:=TRUE;
 until TRUE;
End;

Procedure StrReplaceC ( Var ADataS : string; ACharToSearch, ACharToWrite : char );
Var
  BIndex        : Integer;
Begin
 for BIndex:=1 to Length(ADataS) do
  begin
  if ADataS[BIndex]=ACharToSearch then ADataS[BIndex]:=ACharToWrite;
  end;
End;

Function AvgByte ( ADataA, ADataB : byte; ALen, APos : Integer ) : byte;
Begin
 if ALen=0 then Result:=ADataA
 else Result:=((((ADataA and $FF)*APos)+((ADataB and $FF)*(ALen-APos))) div ALen) and $FF;
End;

Function AverageColor1 ( AColorA, AColorB : Cardinal; ALen, APos : Integer ) : Cardinal;
Begin
 Result:=(AvgByte(AColorA shr 16,AColorB shr 16,ALen,APos) shl 16) or
         (AvgByte(AColorA shr  8,AColorB shr  8,ALen,APos) shl  8) or
          AvgByte(AColorA       ,AColorB       ,ALen,APos);
End;

Function StrSkipSpace ( Const AOrigin : string; Var ADataC : char; Var AIndex : integer; ALen : Integer ) : boolean;
Var
  BDataC        : char;
Begin
 Result:=FALSE;
 while AIndex<=ALen do
  begin
  BDataC:=AOrigin[AIndex];
  if BDataC<>' ' then begin ADataC:=BDataC; Result:=TRUE; break; end;
  inc(AIndex);
  end;
End;

Function StrReadTill ( Const AOrigin : string; Var AResult : string; Const ATerminator : string; Var ADataC : char; Var AIndex : integer; ALen : Integer ) : boolean;
Var
  BDataC        : char;
Begin
 Result:=FALSE;
 AResult:='';
 while AIndex<=ALen do
  begin
  BDataC:=AOrigin[AIndex];
  if Pos(''+BDataC,ATerminator)<>0 then begin ADataC:=BDataC; Result:=TRUE; break; end;
  AResult:=AResult+BDataC;
  inc(AIndex);
  end;
End;

Function StrInList ( Const AStr : string; Const AList : string ) : boolean;
Begin
 Result:=Pos(' '+AStr+' ',' '+AList+' ')<>0;
End;

Function GetStrIndex ( Const AStrS : string; Const AList : string ) : Integer;
Var
  BList,
  BStr          : string;

Begin
 Result:=0;
 BList:=AList;

 repeat
 if BList='' then begin Result:=-1; break; end;
 BStr:=ReadParamStr(BList);
 if BStr='' then begin Result:=-1; break; end;
 if BStr=AStrS then break;
 inc(Result);
 until FALSE;
End;

Function CheckTag ( Const ATag : string; Var ALine : string ) : string;
Var
  BPos          : Integer;
  BTag          : string;
  BIndex,
  BLen          : Integer;
  BChar         : Char;
Begin
 BTag:='['+ATag+':';
 Result:='';
 BPos:=Pos(BTag,ALine);
 if BPos<>0 then
  begin
  BLen:=Length(ALine);
  BIndex:=BPos+Length(BTag);
  while BIndex<BLen do
   begin
   BChar:=ALine[BIndex];
   if BChar=']' then break;
   Result:=Result+BChar;
   inc(BIndex);
   end;
  Delete(ALine,BPos,BIndex+1-BPos)
  end;
End;

Function WordToDouble ( ADataW : word ) : Double;
Begin
 if (ADataW and $8000)<>0 then Result:=-((ADataW xor $FFFF)+1) else Result:=ADataW;
End;

Function ReadForceParam ( AList : TStringList; Const AValueName : string; Const ADefault : string ) : string;
Begin
 Result:=AList.Values[AValueName];
 if Result='' then begin Result:=ADefault; AList.Values[AValueName]:=Result; end;
End;

Function PosNext ( Const ASubstr, AStr : string; AStartPos : Integer ) : Integer;
Var
  BStr          : string;
Begin
 BStr:=Copy(AStr,AStartPos,Length(AStr)-AStartPos+1);
 Result:=Pos(ASubstr,BStr);
 if Result<>0 then Result:=AStartPos+Result-1;
End;

Function PosBack ( Const ASubstr, AStr : string ) : Integer;
Var
  BIndexA,
  BIndexB       : Integer;
  BLenSub       : Integer;
Begin
 Result:=0;
 BLenSub:=Length(ASubstr);
 BIndexA:=Length(AStr)-BLenSub;
 while BIndexA>=0 do
  begin
  BIndexB:=0;
  while BIndexB<BLenSub do
   begin
   if AStr[1+BIndexA+BIndexB]<>ASubstr[1+BIndexB] then break;
   inc(BIndexB);
   end;
  if BIndexB=BLenSub then begin Result:=1+BIndexA; break; end;
  dec(BIndexA);
  end;
End;

Function PosBackC ( ACmp : char; Const AStr : string; AStartPos : Integer ) : Integer;
Begin
 Result:=AStartPos;
 while Result>0 do
  begin
  if AStr[Result]=ACmp then break;
  dec(Result);
  end;
End;

Function TryStrToInt0x ( Const ADataS : string; Out ADataV : Int64 ) : boolean;
Var
  BDataS        : string;
  BDataA        : Cardinal;
Begin
 BDataS:=ADataS;
 if Pos('0x',BDataS)=1 then
  begin
  Delete(BDataS,1,2);
  Result:=Hex32ToInt(BDataS,BDataA);
  ADataV:=BDataA;
  end
 else
  begin
  Result:=TryStrToInt64(BDataS,ADataV);
  end;
End;

Function TryStrToInt0x ( Const ADataS : string; Out ADataV : Cardinal ) : boolean;
Var
  BDataS        : string;
Begin
 BDataS:=ADataS;
 if Pos('0x',BDataS)=1 then
  begin
  Delete(BDataS,1,2);
  Result:=HexToDwordCheck(BDataS,ADataV);
  end
 else
  begin
  Result:=TryStrToDWord(BDataS,ADataV);
  end;
End;

Function FirstMultiPos ( Const ASubstrList : string; Const AStr : string ) : Integer;
Var
  BPos          : Integer;
  BSubstrList,
  BSubstr       : String;
Begin
 Result:=0;
 BSubstrList:=ASubstrList;
 repeat
 BSubstr:=ReadParamStr(BSubstrList);
 if BSubstr='' then break;
 BPos:=Pos(' '+BSubstr+' ',AStr);
 if BPos<>0 then
  begin
  if Result=0 then Result:=BPos
  else if BPos<Result then Result:=BPos;
  end;
 until FALSE;
End;

Procedure XchgStrings ( Var AStrA, AStrB : string );
Var
  BDummyS       : string;
Begin
 BDummyS:=AStrA;
 AStrA:=AStrB;
 AStrB:=BDummyS;
End;

Function StrToData ( Const ADataS : string ) : Cardinal;
Var
  BIndex        : Integer;
Begin
 Result:=0;
 BIndex:=Length(ADataS);
 while BIndex>0 do begin Result:=(Result shl 8)+Ord(ADataS[BIndex]); Dec(BIndex); end;
End;

Function DataDToStr ( AData : Cardinal ) : string;
Begin
 Result:=Chr(AData and $FF)+Chr((AData shr 8) and $FF)+Chr((AData shr 16) and $FF)+Chr((AData shr 24) and $FF);
End;

Function DataWToStr ( AData : Cardinal ) : string;
Begin
 Result:=Chr(AData and $FF)+Chr((AData shr 8) and $FF);
End;

Function CheckTabs ( AList : TStringList ) : boolean;
Var
  BStrIdx       : Integer;
  BDummyS       : string;
Begin
 Result:=FALSE;
 for BStrIdx:=0 to AList.Count-1 do
  begin
  BDummyS:=AList.Strings[BStrIdx];
  if Pos(#9,BDummyS)<>0 then begin Result:=TRUE; break; end;
  end;
End;

Procedure CorrectTabs ( AList : TStringList; AStep : Integer );
Var
  BStrIdx       : Integer;
  BDummyS       : string;
  BPos          : Integer;
  BDirty        : boolean;
Begin
 for BStrIdx:=0 to AList.Count-1 do
  begin
  BDummyS:=AList.Strings[BStrIdx];
  BDirty:=FALSE;
  repeat
  BPos:=Pos(#9,BDummyS);
  if BPos=0 then break;
  BDirty:=TRUE;
  Delete(BDummyS,BPos,1);
  Insert(#32,BDummyS,BPos);
  inc(BPos);
  while ((BPos-1) mod AStep)<>0 do
   begin
   Insert(#32,BDummyS,BPos);
   inc(BPos);
   end;
  until FALSE;
  if BDirty then AList.Strings[BStrIdx]:=BDummyS;
  end;
End;

Procedure DelLastSpace ( AList : TStringList );
Var
  BStrIdx       : Integer;
  BDummyS       : string;
  BDirty        : boolean;
  BIndex,
  BLen          : Integer;
Begin
 for BStrIdx:=0 to AList.Count-1 do
  begin
  BDummyS:=AList.Strings[BStrIdx];
  BDirty:=FALSE;
  repeat
  if BDummyS='' then break;
  BLen:=Length(BDummyS);
  if BDummyS[BLen]<>#32 then break;
  BDirty:=TRUE;
  BIndex:=BLen-1;
  while BIndex>0 do
   begin
   if BDummyS[BIndex]<>#32 then break;
   dec(BIndex);
   end;
  if BIndex=0 then BDummyS:=''
  else Delete(BDummyS,BIndex+1,BLen-BIndex);
  until TRUE;
  if BDirty then AList.Strings[BStrIdx]:=BDummyS;
  end;
End;

Procedure ListReplace ( AList : TStringList; Const ASrc, ADst : string );
Var
  BStrIdx       : Integer;
  BDummyS       : string;
  BDirty        : boolean;
  BPos,
  BLen          : Integer;
Begin
 BLen:=Length(ASrc);
 for BStrIdx:=0 to AList.Count-1 do
  begin
  BDummyS:=AList.Strings[BStrIdx];
  BDirty:=FALSE;
  repeat
  BPos:=Pos(ASrc,BDummyS);
  if BPos=0 then break;
  BDirty:=TRUE;
  Delete(BDummyS,BPos,BLen);
  Insert(ADst,BDummyS,BPos);
  until FALSE;
  if BDirty then AList.Strings[BStrIdx]:=BDummyS;
  end;
End;

Function CheckAddFile ( Const AFullname : string; Const AMask : string; AList : TStringList ) : boolean;
Var
  BFileName,
  BFileExt      : string;
Begin
 Result:=FALSE;
 repeat
 BFileName:=ExtractFileName(AFullName);
 BFileExt:=ExtractFileExt(BFileName);
 if BFileExt='' then begin Result:=TRUE; break; end;
 if BFileExt[1]<>'.' then begin Result:=TRUE; break; end;
 Delete(BFileExt,1,1);
 if Pos(' '+BFileExt+' ',AMask)=0 then begin Result:=TRUE; break; end;
 AList.Append(AFullName);
 Result:=TRUE;
 until TRUE;
End;

Function DirList ( Const APath : string; Const AMask : string; AList : TStringList ) : boolean;
Var
  BDirList      : TStringList;
  BSearchRec    : TSearchRec;
  BResult       : LongInt;
  BIndex        : Integer;
  BDirName      : string;
Begin
 BDirList:=TStringList.Create;

 repeat
 Result:=TRUE;
 BResult:=FindFirst(APath+'*',faAnyFile,BSearchRec);
 while BResult=0 do
  begin
  if (faDirectory and BSearchRec.Attr)<>0 then
   begin
   if BSearchRec.Name='.' then
   else if BSearchRec.Name='..' then
   else BDirList.Append(BSearchRec.Name);
   end
  else if CheckAddFile(APath+BSearchRec.Name,AMask,AList)=FALSE then begin Result:=FALSE; break; end;
  BResult:=FindNext(BSearchRec);
  end;
 FindClose(BSearchRec);

 if Result=FALSE then break;

 BIndex:=0;
 while BIndex<BDirList.Count do
  begin
  BDirName:=BDirList.Strings[BIndex];
  Result:=DirList(APath+BDirName+DirectorySeparator,AMask,AList);
  if Result=FALSE then break;
  inc(BIndex);
  end;
 until TRUE;

 BDirList.Free;
End;

Procedure FileList ( Const APath : string; AList : TStringList );
Var
  BPath         : string;
  BDirList      : TStringList;
  BSearchRec    : TSearchRec;
  BResult       : LongInt;
  BIndex        : Integer;
  BDirName      : string;
Begin
 BDirList:=TStringList.Create;
 BPath:=IncludetrailingPathDelimiter(APath);

 repeat
 BResult:=FindFirst(BPath+'*',faAnyFile,BSearchRec);
 while BResult=0 do
  begin
  if (faDirectory and BSearchRec.Attr)<>0 then
   begin
   if BSearchRec.Name='.' then
   else if BSearchRec.Name='..' then
   else BDirList.Append(BSearchRec.Name);
   end
  else AList.Append(BPath+BSearchRec.Name);
  BResult:=FindNext(BSearchRec);
  end;
 FindClose(BSearchRec);

 BIndex:=0;
 while BIndex<BDirList.Count do
  begin
  BDirName:=BDirList.Strings[BIndex];
  FileList(BPath+BDirName+DirectorySeparator,AList);
  inc(BIndex);
  end;
 until TRUE;

 BDirList.Free;
End;

Procedure FileList ( Const APath, AMask : string; AList : TStringList );
Var
  BPath         : string;
  BDirList      : TStringList;
  BSearchRec    : TSearchRec;
  BResult       : LongInt;
  BIndex        : Integer;
  BDirName      : string;
  BFilename     : string;
  BPathA,
  BNameA,
  BExtA         : string;
Begin
 BDirList:=TStringList.Create;
 BPath:=IncludetrailingPathDelimiter(APath);

 repeat
 BResult:=FindFirst(BPath+'*',faAnyFile,BSearchRec);
 while BResult=0 do
  begin
  if (faDirectory and BSearchRec.Attr)<>0 then
   begin
   if BSearchRec.Name='.' then
   else if BSearchRec.Name='..' then
   else BDirList.Append(BSearchRec.Name);
   end
  else
   begin
   BFilename:=BSearchRec.Name;
   SplitFilename(BFilename,BPathA,BNameA,BExtA);
   if StrInList(BExtA,AMask) then AList.Append(BPath+BFilename);
   end;
  BResult:=FindNext(BSearchRec);
  end;
 FindClose(BSearchRec);

 BIndex:=0;
 while BIndex<BDirList.Count do
  begin
  BDirName:=BDirList.Strings[BIndex];
  FileList(BPath+BDirName+DirectorySeparator,AMask,AList);
  inc(BIndex);
  end;
 until TRUE;

 BDirList.Free;
End;

Function ResolveCMod ( Const AOrig : string ) : string;
Var
  BDataS    : string;
  BDataC    : char;
Begin
 Result:='';
 BDataS:=AOrig;
 while BDataS<>'' do
  begin
  BDataC:=BDataS[1]; Delete(BDataS,1,1);
  if (BDataC='\') and (BDataS<>'') then
   begin
   BDataC:=BDataS[1]; Delete(BDataS,1,1);
   case BDataC of
     'n': BDataC:=#10;
     'r': BDataC:=#13;
   end; // case
   end;
  Result:=Result+BDataC;
  end;
End;

Procedure CopyPrjParams ( ADst, ASrc : TStringList; Const AName : string; Const AInsIdx : string );
Var
  BReadS    : string;
  BIndex    : Integer;
  BInsIdx   : Integer;
Begin
 BIndex:=0;
 while BIndex<ADst.Count do
  begin
  BReadS:=ADst.Strings[BIndex];
  if Pos(AName+'=',BReadS)=1 then ADst.Delete(BIndex)
  else Inc(BIndex);
  end;
 BInsIdx:=ADst.IndexOfName(AInsIdx);
 if BInsIdx<>-1 then inc(BInsIdx);
 BIndex:=0;
 while BIndex<ASrc.Count do
  begin
  BReadS:=ASrc.Strings[BIndex];
  if Pos(AName+'=',BReadS)=1 then
   begin
   if BInsIdx=-1 then ADst.Append(BReadS)
   else begin ADst.Insert(BInsIdx,BReadS); inc(BInsIdx); end;
   end;
  inc(BIndex);
  end;
End;

Procedure ReplaceParam ( Var AData : string; Const AOldParam, ANewParam : string );
Var
  BPos          : Integer;
Begin
 BPos:=Pos(AOldParam,LowerCase(AData));
 if BPos<>0 then
  begin
  Delete(AData,BPos,Length(AOldParam));
  Insert(ANewParam,AData,BPos);
  end;
End;

Function CheckValue ( Const ANameS : string; Const AReadS : string; Out AValue : string ) : boolean;
Var
  BValueS   : string;
Begin
 Result:=FALSE; AValue:='';
 repeat
 if Pos(ANameS+'=',AReadS)<>1 then break;
 BValueS:=Copy(AReadS,Length(ANameS)+2,Length(AReadS)-Length(ANameS)-1);
 if BValueS='' then break;
 AValue:=BValueS;
 Result:=TRUE;
 until TRUE;
End;

Function CheckValue ( Const ANameS : string; Const AReadS : string; Var AValue : Cardinal ) : boolean;
Var
  BValueS   : string;
  BData     : Cardinal;
Begin
 Result:=FALSE;
 repeat
 if CheckValue(ANameS,AReadS,BValueS)=FALSE then break;
 if TryStrToInt0x(BValueS,BData)=FALSE then break;
 AValue:=BData;
 Result:=TRUE;
 until TRUE;
End;

Function DWordAsStr ( AData : Cardinal ) : string;
Begin
 Result:=Chr((AData shr  0) and $FF)+
         Chr((AData shr  8) and $FF)+
         Chr((AData shr 16) and $FF)+
         Chr((AData shr 24) and $FF);
End;

Function StrAsDWord ( Const ADataS : string; AIndex : Integer ) : Cardinal; // Index from 0
Var
  BIndex    : Integer;
Begin
 Result:=0;
 for BIndex:=0 to 3 do
  begin
  if (AIndex+BIndex)<Length(ADataS) then Result:=Result or (Cardinal(ADataS[1+AIndex+BIndex]) shl (8*BIndex));
  end;
End;

Function StrAsDWord ( Const ADataS : string; Var AIndex : Integer; Out AData : Cardinal ) : boolean; // Index from 0
Var
  BIndex    : Integer;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if AIndex+4>Length(ADataS) then break;
 for BIndex:=0 to 3 do
  begin
  AData:=AData or (Cardinal(ADataS[1+AIndex+BIndex]) shl (8*BIndex));
  end;
 inc(AIndex,4);
 Result:=TRUE;
 until TRUE;
End;

Procedure CorrectAscii ( Var ADataS : string );
Var
  BDataS    : string;
  BDataC    : char;
Begin
 BDataS:=ADataS;
 ADataS:='';
 while BDataS<>'' do
  begin
  BDataC:=BDataS[1]; Delete(BDataS,1,1);
  if ord(BDataC) in [32..126] then ADataS:=ADataS+BDataC
  else ADataS:=ADataS+' \x'+IntToHex(Ord(BDataC),2)+' ';
  end;
End;

end.

