unit DbgViewBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, StdCtrls, ExtCtrls, Forms,
  Buttons, LCLType, DbgInfo_sd, ComCtrls, AsmTypes_sd, ConComI;

Type
  TOnLineChange = Procedure ( ADbgInfoLine : TDbgInfoLine ) of object;
  TOnBreakChange = Procedure of object;
  TBreakList = array of Cardinal;

  TDbgViewBase = class(TCustomControl)
  private
  protected
    FFullName   : string;
    FDbgFile    : TDbgInfoFile;
    FBmpIp,
    FBmpBreak   : TBitmap;
    FList       : TStringList;
    FLineStart,
    FLineEnd,
    FLineThis   : Integer; // Where the own lines in LST are
    FLineStartV : Integer; // First visible line
    FLineIp     : Integer; // Current IP position (execution)
    FBreakList  : TBreakList;
    FScrollBar  : TScrollBar;
    FStatBar    : TMsStatusBar;

    FViewBitmap,
    FTextBitmap         : TBitmap;
    FLineHeight,
    FSymbolWidth        : Integer;

    FLinePosA,
    FLinePosB,
    FLinePosC           : Integer;
    FColorANorm,
    FColorAThis,
    FColorBNorm,
    FColorBThis         : TColor;
    FColorHNorm,
    FColorHThis         : TColor;
    FColorPNorm,
    FColorPThis         : TColor;

    FTabSheet           : TTabSheet;
    FHighlightFilename  : string;
    FHighlightSrcLine,
    FHighlightAsmLine   : Integer;
    FOnSetDbgLine       : TOnLineChange;

    FOnLineChange       : TOnLineChange;
    FOnBreakChange      : TOnBreakChange;

    FPaintFocused       : boolean;

    Procedure ScrollPaintOpti;
    Procedure ScrollLineUp;
    Procedure ScrollLineDn;
    Procedure ScrollPageUp;
    Procedure ScrollPageDn;
    Procedure ScrollMoved ( ASender : TObject; AScrollCode : TScrollCode; Var APosition : Integer );

    Procedure Paint; Override;
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure KeyDown ( Var AKey : Word; AShift : TShiftState ); Override;
    Function DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean; Override;

    Procedure LineChange; Virtual;
    Procedure BreakChange;

    Procedure DrawRGutter;
    Procedure ScrollUpdate;
    Function IsLineBreak ( ALineIndex : Integer ) : boolean;
    Function IsLineHighlight ( ALineIdx : Integer ) : boolean; Virtual; Abstract;
    Procedure TriangleIp ( ATop : Integer );
    Procedure BreakMark ( ATop : Integer );

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet; AOnLineChange : TOnLineChange; AOnBreakChange : TOnBreakChange; AOnSetDbgLine : TOnLineChange );
    Procedure Done;

    Procedure ClearBreakList;
    Procedure AppendBreakList ( ALineIdx : Cardinal ); // Will not call Paint
    Procedure AppendBreak ( ALineIdx : Cardinal ); // ALineIdx is original line without FLineStart correction

    Function CanFocus : boolean; Override;
    Procedure SetFocus; Override;
    Procedure PaintA; Virtual;

    property FullName : string read FFullName write FFullName;
    property LineStart : Integer read FLineStart;
    property LineEnd : Integer read FLineEnd;
    property BreakList : TBreakList read FBreakList;
    property DbgFile : TDbgInfoFile read FDbgFile;
    property TabSheet : TTabSheet read FTabSheet write FTabSheet;

  end;

implementation

{ TDbgViewBase }

Constructor TDbgViewBase.Create ( AOwner : TComponent );
Begin
 Inherited;

 FDbgFile:=TDbgInfoFile.Create;
 FStatBar:=TMsStatusBar.Create(Self); InsertControl(FStatBar); //FStatBar.Align:=alClient;
 FScrollBar:=TScrollBar.Create(Self); InsertControl(FScrollBar); FScrollBar.Kind:=sbVertical; FScrollBar.OnScroll:=@ScrollMoved;

 FViewBitmap:=TBitmap.Create;
 //FViewBitmap.PixelFormat:=pf32bit;
 FViewBitmap.Canvas.Font.Name:='Courier New';
 FViewBitmap.Canvas.Font.Size:=10;
 FViewBitmap.Canvas.Font.Color:=0;
 FLineHeight:=FViewBitmap.Canvas.TextHeight('Ayla');
 FSymbolWidth:=FViewBitmap.Canvas.TextWidth('0');

 FTextBitmap:=TBitmap.Create;

 FBmpIp:=TBitmap.Create;
 try
  FBmpIp.LoadFromLazarusResource('sdbmpip');
 except
 end;

 FBmpBreak:=TBitmap.Create;
 try
  FBmpBreak.LoadFromLazarusResource('sdbmpbreak');
 except
 end;

 FColorANorm:=$D0D8D8;
 FColorAThis:=$70C8D8;
 FColorBNorm:=$FFFFFF;
 FColorBThis:=$80E0F0;
 FColorHNorm:=$C0C8C8; // $C0C0C0;
 FColorHThis:=$70B8C8; // $A0D0D0;
 FColorPNorm:=$C0C8C8; // $E0C0C0;
 FColorPThis:=$70B8C8; // $C0D0D0;

 FHighlightSrcLine:=-1;
 FHighlightAsmLine:=-1;
End;

Destructor TDbgViewBase.Destroy;
Begin
 FBmpBreak.Free;
 FBmpIp.Free;
 FTextBitmap.Free;
 FViewBitmap.Free;

 RemoveControl(FScrollBar); FScrollBar.Free;
 RemoveControl(FStatBar); FStatBar.Free;

 FDbgFile.Free;
 Inherited;
End;

Procedure TDbgViewBase.Init ( ASheet : TTabSheet; AOnLineChange : TOnLineChange; AOnBreakChange : TOnBreakChange; AOnSetDbgLine : TOnLineChange );
Begin
 //Name:='';
 FTabSheet:=ASheet;
 if FTabSheet<>nil then FTabSheet.InsertControl(Self);
 FOnLineChange:=AOnLineChange;
 FOnBreakChange:=AOnBreakChange;
 FOnSetDbgLine:=AOnSetDbgLine;
End;

Procedure TDbgViewBase.Done;
Begin
 if FTabSheet<>nil then FTabSheet.RemoveControl(Self);
End;

Function TDbgViewBase.CanFocus : boolean;
Begin
 Inherited;
 Result:=TRUE;
End;

Procedure TDbgViewBase.SetFocus;
Begin
 Inherited;
 //LineChange;
 ScrollUpdate;
End;

Procedure TDbgViewBase.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX,AY : Integer );
Var
  BLineThis     : Integer;
  BPaint        : boolean;
Begin
 Inherited;
 SetFocus;
 BPaint:=not FPaintFocused;
 BLineThis:=FLineStartV+(AY div FLineHeight);
 if BLineThis>=FLineEnd then BLineThis:=FLineEnd-1;
 if BLineThis<>FLineThis then
  begin
  FLineThis:=BLineThis;
  LineChange;
  if Assigned(FOnSetDbgLine) then FOnSetDbgLine(FDbgFile.GetLine(FLineThis));
  BPaint:=TRUE;
  end;

 if AX<FLinePosB then begin BreakChange; BPaint:=TRUE; end;
 if BPaint then PaintA;
End;

Procedure TDbgViewBase.ScrollPaintOpti;
Begin
 //LineChange;
 ScrollUpdate;
 //if Assigned(FOnSetDbgLine) then FOnSetDbgLine(FDbgFile.GetLine(FLineThis));
 PaintA;
End;

Procedure TDbgViewBase.ScrollLineUp;
Begin
 if FLineThis>FLineStart then
  begin
  Dec(FLineThis);
  ScrollPaintOpti;
  end;
End;

Procedure TDbgViewBase.ScrollLineDn;
Begin
 if (FLineThis+1)<FLineEnd then
  begin
  Inc(FLineThis);
  ScrollPaintOpti;
  end;
End;

Procedure TDbgViewBase.ScrollPageUp;
Var
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;
Begin
 BLineCountV:=FTextBitmap.Height div FLineHeight;
 BLineThis:=FLineThis-BLineCountV;
 BLineStartV:=FLineStartV-BLineCountV;
 if BLineThis<FLineStart then BLineThis:=FLineStart;
 if BLineStartV<FLineStart then BLineStartV:=FLineStart;
 if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
  begin
  FLineThis:=BLineThis;
  FLineStartV:=BLineStartV;
  ScrollPaintOpti;
  end;
End;

Procedure TDbgViewBase.ScrollPageDn;
Var
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;
Begin
 BLineCountV:=FTextBitmap.Height div FLineHeight;
 BLineThis:=FLineThis+BLineCountV;
 BLineStartV:=FLineStartV+BLineCountV;
 if BLineThis>=FLineEnd then BLineThis:=FLineEnd-1;
 if BLineStartV>=FLineEnd then BLineStartV:=FLineEnd-1;
 if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
  begin
  FLineThis:=BLineThis;
  FLineStartV:=BLineStartV;
  ScrollPaintOpti;
  end;
End;

Procedure TDbgViewBase.KeyDown ( Var AKey : Word; AShift : TShiftState );
Begin
 Inherited;

 case AKey of
   VK_UP:    ScrollLineUp;
   VK_DOWN:  ScrollLineDn;
   VK_PRIOR: ScrollPageUp;
   VK_NEXT:  ScrollPageDn;
 end; // Case

End;

Function TDbgViewBase.DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean;
Var
  BLineStartV   : Integer;
  BWheelDelta   : Integer;
Begin
 Inherited;

 if AWheelDelta<0 then BWheelDelta:=3
 else if AWheelDelta>0 then BWheelDelta:=-3
 else BWheelDelta:=0;

 BLineStartV:=FLineStartV+BWheelDelta;
 if BLineStartV>=FLineEnd then BLineStartV:=FLineEnd-1;
 if BLineStartV<FLineStart then BLineStartV:=FLineStart;
 if BLineStartV<>FLineStartV then
  begin
  FLineStartV:=BLineStartV;
  ScrollUpdate;
  PaintA;
  end;

 Result:=TRUE;
End;

Procedure TDbgViewBase.ScrollMoved ( ASender : TObject; AScrollCode : TScrollCode; Var APosition : Integer );
Var
  BPosition : Integer;
Begin
 //BScrollBar:=ASender as TScrollBar;
 BPosition:=APosition;
 if BPosition>=FLineEnd then BPosition:=FLineEnd-1;
 if BPosition<FLineStart then BPosition:=FLineStart;
 case AScrollCode of
   scLineUp:   ScrollLineUp;
   scLineDown: ScrollLineDn;
   scPageUp:   ScrollPageUp;
   scPageDown: ScrollPageDn;
   scPosition: begin
               FLineStartV:=APosition;
               ScrollPaintOpti;
               end;
   scTrack:    begin
               FLineStartV:=APosition;
               ScrollPaintOpti;
               end;
   scTop:      begin
               end;
   scBottom:   begin
               end;
 end;
End;

Procedure TDbgViewBase.LineChange;
Begin
 FStatBar.SetParams(IntToStr(FLineThis-FLineStart),'','');
 if Assigned(FOnLineChange) then FOnLineChange(FDbgFile.GetLine(FLineThis));
End;

Procedure TDbgViewBase.BreakChange;
Var
  BIndex        : Integer;
  BBreakLine    : Cardinal;
Begin
 BBreakLine:=FLineThis;

 BIndex:=0;
 while BIndex<Length(FBreakList) do
  begin
  if FBreakList[BIndex]=BBreakLine then break;
  inc(BIndex);
  end;

 if BIndex>=Length(FBreakList) then
  begin
  SetLength(FBreakList,BIndex+1);
  FBreakList[BIndex]:=BBreakLine;
  end
 else
  begin
  inc(BIndex);
  while BIndex<Length(FBreakList) do
   begin
   FBreakList[BIndex-1]:=FBreakList[BIndex];
   inc(BIndex);
   end;
  SetLength(FBreakList,BIndex-1);
  end;

 if Assigned(FOnBreakChange) then FOnBreakChange;
End;

Procedure TDbgViewBase.ClearBreakList;
Begin
 FBreakList:=nil;
End;

Procedure TDbgViewBase.AppendBreakList ( ALineIdx : Cardinal );
Var
  BIndex    : Integer;
Begin
 BIndex:=Length(FBreakList); SetLength(FBreakList,BIndex+1);
 FBreakList[BIndex]:=ALineIdx;
End;

Procedure TDbgViewBase.AppendBreak ( ALineIdx : Cardinal );
Begin
 AppendBreakList(ALineIdx);
 PaintA;
End;

Procedure TDbgViewBase.Paint;
Begin
 Inherited;

 Canvas.Draw(0,0,FViewBitmap);
End;

Procedure TDbgViewBase.PaintA;
Begin
 FPaintFocused:=TRUE; //Focused;
 BmpBox(FViewBitmap.Canvas,0,0,FViewBitmap.Width,FViewBitmap.Height,$808080,$FFFFFF,CColorBG);
End;

Procedure TDbgViewBase.Resize;
Begin
 Inherited;

 FViewBitmap.SetSize(Width,Height-FStatBar.Height);
 FTextBitmap.SetSize(FViewBitmap.Width-4-FScrollBar.Width,FViewBitmap.Height-4);

 FScrollBar.Left:=FViewBitmap.Width-FScrollBar.Width-1; FScrollBar.Top:=1; FScrollBar.Height:=FTextBitmap.Height;

 FStatBar.SetBounds(0,Height-FStatBar.Height,Width,FStatBar.Height);

 ScrollUpdate;
 PaintA;
End;

Procedure TDbgViewBase.ScrollUpdate;
Begin
 FScrollBar.SetParams(FLineStartV,FLineStart,FLineEnd,FTextBitmap.Height div FLineHeight);
End;

Procedure TDbgViewBase.TriangleIp ( ATop : Integer );
Var
  BTop          : Integer;
  BRowIndex,
  BColIndex     : Integer;
  BMaskColor,
  BColor        : TColor;
Begin
 BTop:=ATop+(FLineHeight div 2)-(FBmpIp.Height div 2);
 BMaskColor:=FBmpIp.Canvas.Pixels[0,0];
 BRowIndex:=0;
 while BRowIndex<FBmpIp.Height do
  begin
  BColIndex:=0;
  while BColIndex<FBmpIp.Width do
   begin
   BColor:=FBmpIp.Canvas.Pixels[BColIndex,BRowIndex];
   if BColor<>BMaskColor then FTextBitmap.Canvas.Pixels[FLinePosC+BColIndex,BTop+BRowIndex]:=BColor;
   inc(BColIndex);
   end;
  inc(BRowIndex);
  end;
End;

Procedure TDbgViewBase.BreakMark ( ATop : Integer );
Var
  BTop          : Integer;
  BRowIndex,
  BColIndex     : Integer;
  BMaskColor,
  BColor        : TColor;
Begin
 BTop:=ATop+(FLineHeight div 2)-(FBmpBreak.Height div 2);
 BMaskColor:=FBmpBreak.Canvas.Pixels[0,0];
 BRowIndex:=0;
 while BRowIndex<FBmpBreak.Height do
  begin
  BColIndex:=0;
  while BColIndex<FBmpBreak.Width do
   begin
   BColor:=FBmpBreak.Canvas.Pixels[BColIndex,BRowIndex];
   if BColor<>BMaskColor then FTextBitmap.Canvas.Pixels[1+BColIndex,BTop+BRowIndex]:=BColor;
   inc(BColIndex);
   end;
  inc(BRowIndex);
  end;
End;

Function TDbgViewBase.IsLineBreak ( ALineIndex : Integer ) : boolean;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FBreakList) do
  begin
  if FBreakList[BIndex]=ALineIndex then break;
  inc(BIndex);
  end;
 Result:=BIndex<Length(FBreakList);
End;

Procedure TDbgViewBase.DrawRGutter;
Var
  BCanvas       : TCanvas;
  BPosA,
  BPosB         : Integer;
  BLineCnt,
  BLineCntV     : Integer;
  BPosYA,
  BPosYB        : Integer;
  BLineIdx      : Integer;
  BIndex        : Integer;
Begin
 BCanvas:=FTextBitmap.Canvas;

 BPosB:=FTextBitmap.Width;
 BPosA:=BPosB-13;
 BCanvas.Brush.Color:=FColorANorm;
 BCanvas.FillRect(BPosA,0,BPosB,FTextBitmap.Height);
 //BCanvas.Pen.Color:=$80FFFF; BCanvas.Line(BPosA+0,0,BPosA+0,FViewBitmap.Height);
 BCanvas.Pen.Color:=$FFFFFF; BCanvas.Line(BPosA+4,0,BPosA+4,FTextBitmap.Height);
 BCanvas.Pen.Color:=$808080; BCanvas.Line(BPosA+5,0,BPosA+5,FTextBitmap.Height);
 BCanvas.Pen.Color:=$FFFFFF; BCanvas.Line(BPosB-1,0,BPosB-1,FTextBitmap.Height);

 BCanvas.Brush.Color:=$D0D0D0;
 BCanvas.FillRect(BPosA+6,0,BPosB,FTextBitmap.Height);
 repeat
 BLineCnt:=FLineEnd-FLineStart;
 BLineCntV:=(FTextBitmap.Height+FLineHeight-1) div FLineHeight;
 if (BLineCnt<=0) or (BLineCntV<=0) then break;
 BCanvas.Brush.Color:=$E0E0E0;
 BPosYA:=(FTextBitmap.Height*(FLineStartV-FLineStart)          ) div BLineCnt; if BPosYA>=FTextBitmap.Height then BPosYA:=FTextBitmap.Height;
 BPosYB:=(FTextBitmap.Height*(FLineStartV-FLineStart+BLineCntV)) div BLineCnt; if BPosYB>=FTextBitmap.Height then BPosYB:=FTextBitmap.Height;
 BCanvas.FillRect(BPosA+6,BPosYA,BPosB,BPosYB);
 // Line highlight
 BLineIdx:=FLineStart;
 BCanvas.Brush.Color:=$0090D0;
 while BLineIdx<FLineEnd do
  begin
  if IsLineHighlight(BLineIdx) then
   begin
   BPosYA:=(FTextBitmap.Height*(BLineIdx-FLineStart)) div BLineCnt;
   BPosYB:=(FTextBitmap.Height*(BLineIdx+1-FLineStart)) div BLineCnt;
   BCanvas.FillRect(BPosA+6,BPosYA,BPosA+10,BPosYB+1);
   end;
  inc(BLineIdx);
  end;
 BLineIdx:=FLineStart;
 while BLineIdx<FLineEnd do
  begin
  if IsLineBreak(BLineIdx) then
   begin
   BPosYA:=(FTextBitmap.Height*(BLineIdx-FLineStart)) div BLineCnt;
   CircF(FTextBitmap,BPosA+2,BPosYA+0,2,$0000FF);
   end;
  if BLineIdx=FLineIp then
   begin
   BPosYA:=(FTextBitmap.Height*(BLineIdx-FLineStart)) div BLineCnt;
   BCanvas.Pen.Color:=$008000; BCanvas.Pen.Style:=psSolid;
   for BIndex:=0 to 2 do BCanvas.Line(BPosA+BIndex,BPosYA-2+BIndex,BPosA+BIndex,BPosYA+3-BIndex);
   //BCanvas.Pixels[BPosA+1,BPosYA-0]:=$80FF80;
   end;
  inc(BLineIdx);
  end;
 until TRUE;

End;


Initialization
  {$I sddbgviewbase.lrs}

end.

