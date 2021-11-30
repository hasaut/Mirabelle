unit MemSegHelper_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TMsBinPtr = ^String;

Function MsBinExport ( ABinPtr : TMsBinPtr ) : string;
Function MsBinImport ( Const AHexS : string ) : TMsBinPtr;

implementation

Uses
  ConComL;

Function MsBinExport ( ABinPtr : TMsBinPtr ) : string;
Begin
 Result:=IntToHex(QWord(ABinPtr),16);
End;

Function MsBinImport ( Const AHexS : string ) : TMsBinPtr;
Var
  BData : QWord;
Begin
 Result:=nil;
 repeat
 if HexToQWordCheck(AHexS,BData)=FALSE then break;
 Result:=TMsBinPtr(BData);
 until TRUE;
End;

end.

