unit ConComF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, SynEditTypes;
  // Compiler will complain about SynEditTypes. In this case you will may need to drop
  // SynEdit to a main form

Procedure RdScrPos ( Const AParams : string; AControl : TControl );
Procedure RdScrPos ( AParams : TStringList; AControl : TControl );
Procedure RdScrPos ( AParams : TStringList; AControl : TControl; Const APrefix : string );
Function RdScrParam ( AParams : TStringList; Const AName : string ) : Integer;
Function RdScrParam ( AParams : TStringList; Const AName : string; AMinValue, AMaxValue : Integer ) : Integer;
Function WrScrPos ( AControl : TControl ) : string;
Procedure WrScrPos ( AParams : TStringList; AControl : TControl );
Procedure WrScrPos ( AParams : TStringList; AControl : TControl; Const APrefix : string );
Procedure WrScrParam ( AParams : TStringList; Const AName : string; AData : Integer );
//Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;
Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;

implementation

Uses
  ConComL;

Procedure RdScrPos ( Const AParams : string; AControl : TControl );
Var
  BPosS         : string;
  BIndex        : Integer;
  BPos          : array [0..3] of Integer;
Begin
 repeat
 BPosS:=AParams;
 if BPosS='' then break;
 BIndex:=0;
 while BIndex<4 do
  begin
  BPos[BIndex]:=0;
  StringToInteger(ReadParamStr(BPosS),BPos[BIndex]);
  inc(BIndex);
  end;
 if BPos[1]<30 then BPos[1]:=30;
 if BPos[3]<30 then BPos[3]:=30;
 AControl.SetBounds(BPos[0],BPos[2],BPos[1],BPos[3]);
 until TRUE;
End;

Procedure RdScrPos ( AParams : TStringList; AControl : TControl; Const APrefix : string );
Begin
 RdScrPos(AParams.Values[APrefix],AControl);
End;

Procedure RdScrPos ( AParams : TStringList; AControl : TControl );
Begin
 RdScrPos(AParams,AControl,'ScreenPos');
End;

Function RdScrParam ( AParams : TStringList; Const AName : string ) : Integer;
Var
  BDataS        : string;
Begin
 Result:=30;
 repeat
 BDataS:=AParams.Values[AName];
 if BDataS='' then break;
 if StringToInteger(BDataS,Result)=FALSE then begin Result:=30; break; end;
 until TRUE;
End;

Function RdScrParam ( AParams : TStringList; Const AName : string; AMinValue, AMaxValue : Integer ) : Integer;
Var
  BDataS        : string;
Begin
 Result:=30;
 repeat
 BDataS:=AParams.Values[AName];
 if BDataS='' then break;
 if StringToInteger(BDataS,Result)=FALSE then begin Result:=30; break; end;
 until TRUE;
 if Result<AMinValue then Result:=AMinValue;
 if Result>AMaxValue then Result:=AMaxValue;
End;

Function WrScrPos ( AControl : TControl ) : string;
Begin
 Result:=IntToStr(AControl.Left)+' '+
         IntToStr(AControl.Width)+' '+
         IntToStr(AControl.Top)+' '+
         IntToStr(AControl.Height);
End;

Procedure WrScrPos ( AParams : TStringList; AControl : TControl; Const APrefix : string );
Begin
 AParams.Values[APrefix]:=WrScrPos(AControl);
End;

Procedure WrScrPos ( AParams : TStringList; AControl : TControl );
Begin
 WrScrPos(AParams,AControl,'ScreenPos');
End;

Procedure WrScrParam ( AParams : TStringList; Const AName : string; AData : Integer );
Begin
 AParams.Values[AName]:=IntToStr(AData);
End;

{Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;
Begin
 Result:=ASearchOptions-[ssoEntireScope]+[ssoFindContinue];
End;}

Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;
Begin
 Result:=ASearchOptions-[ssoEntireScope]+[ssoFindContinue]; // Seems to be a bug for Linux
End;


end.

