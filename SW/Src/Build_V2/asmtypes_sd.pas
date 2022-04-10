Unit AsmTypes_sd;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  {
   Messages:
   i = info
   w = warning
   e = error
   d = from ThBuild to MgDebug (FPrjFullName+#13+FCoreList+#13+FBuild.LstName+#13+FBuild.Lst.Text)
   g = SegData (name + sp + AddrHex)
   l = LTO report
   m = +'Close' = close gui
   p = Progress
   r = State+Registers
   s = Communication state
   t = terminal
   u = BuildState (0 = nothing, 1 = start, 2 = success). "Error" is not reported
   v = From TreeView to Editor
   a = Add file
  }
  TOnViewAny = Procedure ( Const AMessage : string ) of Object;
  TOnProcAny = Procedure ( Const ACmd : string ) of Object;
  TOnLocalizeError = Procedure ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string ) of object;

  TParsRetCode = (prcError, prcNotRecog, prcOk);
  TDefType = (dtNone, dtConst, dtMacro);

  TBreakList = array [0..7] of Cardinal;

  TParsToken = class(TObject)
  private
    FOrig       : string;
    FLine,
    FPos        : Integer;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Import ( AToken : TParsToken );

    property Line : Integer read FLine write FLine;
    property Pos : Integer read FPos write FPos;
    property Orig : string read FOrig write FOrig;
  End;

Const
  CTagS         = '~';
  CTagE         = '#';
  CTagM         = ':';
  CTagP         = ',';

  CAsmResWordsA : string = (' extern public code data align startaddr ');
  CAsmResWordsB : string = (' equ dup ');

  CVhdOneSymbol : string = (' == <= >= != // ');
  CVhdResWords  : string =
   (
    ' module endmodule input output'+
    ' if else begin end'+
    ' assign wire reg always case endcase'+
    ' or posedge negedge default parameter'+
    ' '
   );
  CBasicTypes : string = ('l c b k w m d i p f');

Function FormatError ( Const AMessage : string; Const AFilename : string; ALine, APos : Integer ) : string;
Function FormatError ( Const AMessage : string; Const AFilename : string; Const ALine : string ) : string;
Procedure ParseError ( Const AErrorCode : string; Out AMessage : string;  Out AReporter : string; Out AFilename : string; Out ALine, APos : Integer );

Implementation

Uses
  ConComL;

Function GetErrorCount ( AErrorList : TStringList; AErrorType : char ) : Integer;
Var
  BIndex        : Integer;
Begin
 Result:=0;
 BIndex:=0;
 while BIndex<AErrorList.Count do
  begin
  if Copy(AErrorList.Strings[BIndex],1,1)=AErrorType then Inc(Result);
  Inc(BIndex);
  end;
End;

Function FormatError ( Const AMessage : string; Const AFilename : string; ALine, APos : Integer ) : string;
Begin
 Result:=AMessage+'[F:'+AFilename+'|'+IntToStr(ALine)+'|'+IntToStr(APos)+']';
End;

Function FormatError ( Const AMessage : string; Const AFilename : string; Const ALine : string ) : string;
Begin
 Result:=AMessage+'[F:'+AFilename+'|'+ALine+'|0]';
End;

Procedure ParseError ( Const AErrorCode : string; Out AMessage : string;  Out AReporter : string; Out AFilename : string; Out ALine, APos : Integer );
Var
  BErrorCode    : string;
  BLocation     : string;
Begin
 BErrorCode:=AErrorCode;
 AReporter:=CheckTag('R',BErrorCode);
 BLocation:=CheckTag('F',BErrorCode); AFilename:=ReadTillC(BLocation,'|');
 TryStrToInt(ReadTillC(BLocation,'|'),ALine);
 TryStrToInt(ReadTillC(BLocation,'|'),APos);
 DelLastSpace(BErrorCode);
 AMessage:=BErrorCode;
End;

Constructor TParsToken.Create;
Begin
 Inherited;
End;

Destructor TParsToken.Destroy;
Begin
 Inherited;
End;

Procedure TParsToken.Import ( AToken : TParsToken );
Begin
 FOrig:=AToken.FOrig;
 FLine:=AToken.FLine;
 FPos:=AToken.FPos;
End;

End.

