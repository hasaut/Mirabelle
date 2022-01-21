unit ColorScheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynHighlighterAny, SynEditHighlighter;

Type
  TAttriStyle = record
    FFgColor,
    FBgColor    : TColor;
    FFontStyle  : TFontStyles;
  end;

  TColorScheme = record
    FEditorColor        : TColor;
    FAttriList          : array [0..9] of TAttriStyle;
  end;

Const
  CAttriNamesAsm : array [0..9] of string = ('Comment', 'Constant', 'Identifier', 'Key', 'Number', 'Object', 'Space', 'String', 'Symbol', 'Variable');
  CAttriNamesPas : array [0..9] of string = ('Comment', 'Constant', 'Identifier', 'Key', 'Number', 'Object', 'Space', 'String', 'Symbol', 'Variable');

  CCSAsmDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:$900000;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

  CCSPasDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clRed;     FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

  CCSCppDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clRed;     FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

  CCSVhdDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:$900000;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

  CCSPyDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:$900000;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

  CCSRustDefault : TColorScheme =
    (
     FEditorColor: clWhite;
     FAttriList: (
                  (FFgColor:clGray;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlack;   FBgColor:clNone; FFontStyle: [fsBold]),
                  (FFgColor:clGreen;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clTeal;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clOlive;   FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clBlue;    FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clRed;     FBgColor:clNone; FFontStyle: []),
                  (FFgColor:clFuchsia; FBgColor:clNone; FFontStyle: [])
                 )
    );

{Const
  CColorSchemeGreen : TColorScheme =
  // Edit comment const ident key number object space string symbol var
  (clGreen, clSilver, clLime, clYellow, clWhite, clLime, clSkyBlue, clOlive, clAqua, clWhite, clFuchsia);

  CColorSchemeWhite : TColorScheme =
  // Edit   comment const  ident   key       number   object  space    string  symbol   var
  (clWhite, clGray, clBlue, clMaroon, clBlack, clGreen, clTeal, clOlive, clBlue, clRed, clFuchsia);
}

Function ColorSchemeToStr ( Const AColorScheme : TColorScheme ) : string;
Function StrToColorScheme ( Const AStr : string; Var AColorScheme : TColorScheme ) : boolean;
Procedure SetAttri ( Const AAttriStyle : TAttriStyle; AAttri : TSynHighlighterAttributes );

implementation

Uses
  ConComL;

Function AttriStyleToStr ( Const AAttriStyle : TAttriStyle ) : string;
Begin
 Result:=IntToStr(AAttriStyle.FFgColor)+' '+IntToStr(AAttriStyle.FBgColor)+' '+IntToStr(Integer(AAttriStyle.FFontStyle));
End;

Function StrToAttriStyle ( Var AStr : string; Var AAttriStyle : TAttriStyle ) : boolean;
Var
  BDataS        : string;
  BDataI        : Integer;
Begin
 Result:=FALSE;
 BDataI:=0;
 repeat
 DelFirstSpace(AStr);
 if AStr='' then break;

 BDataS:=ReadParamStr(AStr); if BDataS='' then break;
 if StringToInteger(BDataS,BDataI)=FALSE then break;
 AAttriStyle.FFgColor:=BDataI;

 BDataS:=ReadParamStr(AStr); if BDataS='' then break;
 if StringToInteger(BDataS,BDataI)=FALSE then break;
 AAttriStyle.FBgColor:=BDataI;

 BDataS:=ReadParamStr(AStr); if BDataS='' then break;
 if StringToInteger(BDataS,BDataI)=FALSE then break;
 AAttriStyle.FFontStyle:=TFontStyles(BDataI);

 Result:=TRUE;
 until TRUE;
End;

Function ColorSchemeToStr ( Const AColorScheme : TColorScheme ) : string;
Var
  BIndex        : Integer;
Begin
 Result:=IntToStr(AColorScheme.FEditorColor);
 BIndex:=0;
 while BIndex<10 do
  begin
  Result:=Result+' '+AttriStyleToStr(AColorScheme.FAttriList[BIndex]);
  inc(BIndex);
  end;
End;

Function StrToColorScheme ( Const AStr : string; Var AColorScheme : TColorScheme ) : boolean;
Var
  BStr          : string;
  BDataS        : string;
  BDataI        : Integer;
  BIndex        : Integer;
Begin
 Result:=FALSE;
 BStr:=AStr; BDataI:=0;
 repeat
 BDataS:=ReadParamStr(BStr); if BDataS='' then break;
 if StringToInteger(BDataS,BDataI)=FALSE then break;
 AColorScheme.FEditorColor:=BDataI;
 BIndex:=0;
 while BIndex<10 do
  begin
  if StrToAttriStyle(BStr,AColorScheme.FAttriList[BIndex])=FALSE then break;
  inc(BIndex);
  end;
 Result:=BIndex=10;
 until TRUE;
End;

Procedure SetAttri ( Const AAttriStyle : TAttriStyle; AAttri : TSynHighlighterattributes );
Begin
 AAttri.Foreground:=AAttriStyle.FFgColor;
 AAttri.Background:=AAttriStyle.FBgColor;
 AAttri.Style:=AAttriStyle.FFontStyle;
End;

end.

