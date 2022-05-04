unit ConComAsm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

{ *** Common (Conversions) *** }
Function IsFloat ( Const AStr : string; Out AConst : Double ) : boolean;
Function IsInteger ( Const AStr : string; Out AConst : Int64 ) : boolean;
Function IsCardinal ( Const AStr : string; Var AConst : Cardinal ) : boolean;
Function IsDword ( Const AStr : string; Var AConst : DWord ) : boolean;
Function IsChar ( Const AStr : string; Out AConst : Cardinal ) : boolean;

implementation

Uses
  ConComL;

{ *** Common (conversions) *** }

Function IsFloat ( Const AStr : string; Out AConst : Double ) : boolean;
Var
  BStr          : string;
  BDataF        : Double;
  BSign         : Integer;

Begin
 Result:=FALSE;
 BStr:=AStr;
 AConst:=0;

 repeat
 if AStr='+Inf' then begin AConst:=Infinity; Result:=TRUE; break; end;
 if AStr='-Inf' then begin AConst:=NegInfinity; Result:=TRUE; break; end;
 BSign:=1;
 if Pos('-',BStr)=1 then begin BSign:=-1; Delete(BStr,1,1); end;
 if TryStrToFloat(BStr,BDataF,HParsFormat)=FALSE then break;
 AConst:=BDataF*BSign;
 Result:=TRUE;
 until TRUE;
End;

Function IsInteger ( Const AStr : string; Out AConst : Int64 ) : boolean;
Var
  BStr          : string;
  BDataI        : Integer;
  BDataC        : Cardinal;
  BSign         : Integer;

Begin
 Result:=FALSE;
 BStr:=AStr;
 AConst:=0;

 repeat
 BSign:=1;
 if Pos('-',BStr)=1 then begin BSign:=-1; Delete(BStr,1,1); end;
 if pos('0x',BStr)=1 then
  begin
  Delete(BStr,1,2); DelFirstSpace(BStr);
  if BStr='' then break;
  if HexToDWordCheck(BStr,BDataC)=FALSE then break;
  AConst:=Integer(BDataC)*BSign;
  end
 else
  begin
  if StringToInteger(BStr,BDataI)=FALSE then break;
  AConst:=BDataI*BSign;
  end;
 Result:=TRUE;
 until TRUE;
End;

Function IsCardinal ( Const AStr : string; Var AConst : Cardinal ) : boolean;
Var
  BStr          : string;
  BDataI        : Integer;
  BDataC        : Cardinal;

Begin
 Result:=FALSE;
 BStr:=AStr;
 BDataI:=0; BDataC:=0;

 repeat
 if pos('0x',BStr)=1 then
  begin
  Delete(BStr,1,2); DelFirstSpace(BStr);
  if BStr='' then break;
  if HexToDWordCheck(BStr,BDataC)=FALSE then break;
  AConst:=BDataC;
  end
 else
  begin
  if StringToInteger(BStr,BDataI)=FALSE then break;
  AConst:=BDataI;
  end;
 Result:=TRUE;
 until TRUE;
End;

Function IsDword ( Const AStr : string; Var AConst : DWord ) : boolean;
Var
  BStr          : string;
  BDataI        : Integer;
  BDataIA       : DWord absolute BDataI;
  BDataF        : Single;
  BDataFA       : DWord absolute BDataF;

Begin
 Result:=FALSE;
 BStr:=AStr;
 BDataI:=0;

 repeat
 if pos('0x',BStr)=1 then
  begin
  Delete(BStr,1,2); DelFirstSpace(BStr);
  if BStr='' then break;
  if HexToDWordCheck(BStr,AConst)=FALSE then break;
  end
 else if Pos('.',BStr)=0 then
  begin
  if StringToInteger(BStr,BDataI)=FALSE then break;
  AConst:=BDataIA;
  end
 else if TryStrToFloat(BStr,BDataF,HParsFormat) then
  begin
  AConst:=BDataFA;
  end
 else break;
 Result:=TRUE;
 until TRUE;
End;

Function IsChar ( Const AStr : string; Out AConst : Cardinal ) : boolean;
Var
  BStr  : string;
  BLen  : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr;
 AConst:=0;

 repeat
 BLen:=Length(BStr);
 if BLen<>3 then break;
 if (BStr[1]=#39) and (BStr[3]=#39) then
 else break;
 AConst:=Ord(BStr[2]);
 Result:=TRUE;
 until TRUE;
End;

end.

