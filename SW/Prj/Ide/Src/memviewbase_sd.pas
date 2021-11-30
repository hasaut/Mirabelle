unit MemViewBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, StdCtrls, ExtCtrls, Forms,
  Buttons, LCLType, ComCtrls, ConComI;

Type
  TOnMemViewReqData = Procedure ( Const ASegName : string ) of Object;

  TMemViewBase = class(TCustomControl)
  private
    Procedure ReqData;
  protected
    FParent     : TWinControl;
    FSegStart,
    FSegSize    : Cardinal;
    FSegName    : string;

    FOnReqData  : TOnMemViewReqData;


    FDataPrev,
    FDataThis   : string;
    FLineCount  : Integer; // (Length(FListThis)+15) div 16
    FChangeList : array of boolean;
    FLineThis   : Integer; // Current line
    FLineStartV : Integer; // First visible line
    FScrollBar  : TScrollBar;
    FStatBar    : TMsStatusBar;

    FViewBitmap,
    FTextBitmap         : TBitmap;
    FLineHeight,
    FSymbolWidth        : Integer;

    FLinePosA,
    FLinePosB,
    FLinePosC           : Integer;
    FWidthHex,
    FWidthChr           : Integer;
    FColorANorm,
    FColorAThis,
    FColorBNorm,
    FColorBThis         : TColor;
    FColorHNorm,
    FColorHThis         : TColor;
    FColorPNorm,
    FColorPThis         : TColor;

    FHighlightAsmLine   : Integer;
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

    Procedure DrawRGutter;
    Procedure ScrollUpdate;
    Function IsLineHighlight ( ALineIdx : Integer ) : boolean; Virtual;

    Procedure ViewLine ( ABitmap : TBitmap; APosY : Integer; Const AAddrS, ADataPrev, ADataThis : string );

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string; AOnReqData : TOnMemViewReqData );
    Procedure Done;

    Procedure SetActive ( AActive : boolean );
    Procedure GrayData;
    Procedure SetData ( Const AData : string );

    Function CanFocus : boolean; Override;
    Procedure SetFocus; Override;
    Procedure PaintA; Virtual;

    property SegName : string read FSegName;
  end;

  TMemViewList = array of TMemViewBase;

Function MemViewListAppend ( Var AList : TMemViewList; AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string; AOnReqData : TOnMemViewReqData ) : TMemViewBase;
Procedure MemViewListClear ( Var AList : TMemViewList );

Implementation

Function MemViewListAppend ( Var AList : TMemViewList; AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string; AOnReqData : TOnMemViewReqData ) : TMemViewBase;
Var
  BIndex    : Integer;
Begin
 Result:=TMemViewBase.Create(AParent); Result.Name:='';
 BIndex:=Length(AList); SetLength(AList,BIndex+1); AList[BIndex]:=Result;
 Result.Init(AParent,ASegStart,ASegSize,ASegName,AOnReqData);
End;

Procedure MemViewListClear ( Var AList : TMemViewList );
Var
  BIndex    : Integer;
  BMemView  : TMemViewBase;
Begin
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  BMemView:=AList[BIndex];
  BMemView.Done; BMemView.Free;
  inc(BIndex);
  end;
 AList:=nil;
End;

Function GetLineData ( Const ADataS : string; ALineIdx : Cardinal ) : string;
Var
  BStart,
  BLen      : Integer;
Begin
 BStart:=ALineIdx*16;
 BLen:=Length(ADataS)-BStart; if BLen<0 then BLen:=0 else if BLen>16 then BLen:=16;
 Result:=Copy(ADataS,1+BStart,BLen);
End;

{ TMemViewBase }

Const
  CColorSame    = $808080;
  CColorDiff    = $F00000;

Constructor TMemViewBase.Create ( AOwner : TComponent );
Begin
 Inherited;

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

 FColorANorm:=$D0D8D8;
 FColorAThis:=$70C8D8;
 FColorBNorm:=$FFFFFF;
 FColorBThis:=$80E0F0;
 FColorHNorm:=$C0C8C8; // $C0C0C0;
 FColorHThis:=$70B8C8; // $A0D0D0;
 FColorPNorm:=$C0C8C8; // $E0C0C0;
 FColorPThis:=$70B8C8; // $C0D0D0;

 FDataPrev:=''; FDataThis:='';
 FHighlightAsmLine:=-1;
End;

Destructor TMemViewBase.Destroy;
Begin
 RemoveControl(FScrollBar); FScrollBar.Free;
 RemoveControl(FStatBar); FStatBar.Free;

 FTextBitmap.Free;
 FViewBitmap.Free;

 FDataPrev:=''; FDataThis:='';
 Inherited;
End;

Procedure TMemViewBase.Init ( AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string; AOnReqData : TOnMemViewReqData );
Begin
 FParent:=AParent; FParent.InsertControl(Self);
 Visible:=FALSE; Align:=alClient;
 FSegStart:=ASegStart;
 FSegSize:=ASegSize;
 FSegName:=ASegName;
 FOnReqData:=AOnReqData;
End;

Procedure TMemViewBase.Done;
Begin
 if FParent<>nil then FParent.RemoveControl(Self);
End;

Function TMemViewBase.CanFocus : boolean;
Begin
 Inherited;
 Result:=TRUE;
End;

Procedure TMemViewBase.SetFocus;
Begin
 Inherited;
 //LineChange;
 ScrollUpdate;
End;

Procedure TMemViewBase.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX,AY : Integer );
Var
  BLineThis     : Integer;
  BPaint        : boolean;
Begin
 Inherited;
 SetFocus;
 BPaint:=not FPaintFocused;
 BLineThis:=FLineStartV+(AY div FLineHeight);
 if BLineThis>=FLineCount then BLineThis:=FLineCount-1;
 if BLineThis<>FLineThis then
  begin
  FLineThis:=BLineThis;
  LineChange;
  BPaint:=TRUE;
  end;

 if BPaint then PaintA;
End;

Procedure TMemViewBase.ScrollPaintOpti;
Begin
 ScrollUpdate;
 PaintA;
End;

Procedure TMemViewBase.ScrollLineUp;
Begin
 if FLineThis>0 then
  begin
  Dec(FLineThis);
  ScrollPaintOpti;
  end;
End;

Procedure TMemViewBase.ScrollLineDn;
Begin
 if (FLineThis+1)<FLineCount then
  begin
  Inc(FLineThis);
  ScrollPaintOpti;
  end;
End;

Procedure TMemViewBase.ScrollPageUp;
Var
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;
Begin
 BLineCountV:=FTextBitmap.Height div FLineHeight;
 BLineThis:=FLineThis-BLineCountV;
 BLineStartV:=FLineStartV-BLineCountV;
 if BLineThis<0 then BLineThis:=0;
 if BLineStartV<0 then BLineStartV:=0;
 if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
  begin
  FLineThis:=BLineThis;
  FLineStartV:=BLineStartV;
  ScrollPaintOpti;
  end;
End;

Procedure TMemViewBase.ScrollPageDn;
Var
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;
Begin
 BLineCountV:=FTextBitmap.Height div FLineHeight;
 BLineThis:=FLineThis+BLineCountV;
 BLineStartV:=FLineStartV+BLineCountV;
 if BLineThis>=FLineCount then BLineThis:=FLineCount-1;
 if BLineStartV>=FLineCount then BLineStartV:=FLineCount-1;
 if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
  begin
  FLineThis:=BLineThis;
  FLineStartV:=BLineStartV;
  ScrollPaintOpti;
  end;
End;

Procedure TMemViewBase.KeyDown ( Var AKey : Word; AShift : TShiftState );
Begin
 Inherited;

 case AKey of
   VK_UP:    ScrollLineUp;
   VK_DOWN:  ScrollLineDn;
   VK_PRIOR: ScrollPageUp;
   VK_NEXT:  ScrollPageDn;
 end; // Case

End;

Function TMemViewBase.DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean;
Var
  BLineStartV   : Integer;
  BWheelDelta   : Integer;
Begin
 Inherited;

 if AWheelDelta<0 then BWheelDelta:=3
 else if AWheelDelta>0 then BWheelDelta:=-3
 else BWheelDelta:=0;

 BLineStartV:=FLineStartV+BWheelDelta;
 if BLineStartV>=FLineCount then BLineStartV:=FLineCount-1;
 if BLineStartV<0 then BLineStartV:=0;
 if BLineStartV<>FLineStartV then
  begin
  FLineStartV:=BLineStartV;
  ScrollUpdate;
  PaintA;
  end;

 Result:=TRUE;
End;

Procedure TMemViewBase.ScrollMoved ( ASender : TObject; AScrollCode : TScrollCode; Var APosition : Integer );
Var
  BPosition : Integer;
Begin
 //BScrollBar:=ASender as TScrollBar;
 BPosition:=APosition;
 if BPosition>=FLineCount then BPosition:=FLineCount-1;
 if BPosition<0 then BPosition:=0;
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

Procedure TMemViewBase.SetActive ( AActive : boolean );
Begin
 Visible:=AActive;
 if AActive then begin GrayData; ReqData; end;
End;

Procedure TMemViewBase.GrayData;
Begin
End;

Procedure TMemViewBase.ReqData;
Begin
 if Assigned(FOnReqData) then FOnReqData(FSegName);
End;

Procedure TMemViewBase.SetData ( Const AData : string );
Var
  BLineIdx  : Integer;
Begin
 FDataPrev:=FDataThis;
 FDataThis:=AData; FLineCount:=(Length(FDataThis)+15) shr 4;
 if Length(FChangeList)<>FLineCount then SetLength(FChangeList,FLineCount);
 BLineIdx:=0;
 while BLineIdx<FLineCount do
  begin
  FChangeList[BLineIdx]:=GetLineData(FDataPrev,BLineIdx)<>GetLineData(FDataThis,BLineIdx);
  inc(BLineIdx);
  end;
 ScrollUpdate;
 PaintA;
End;

Procedure TMemViewBase.LineChange;
Begin
 FStatBar.SetParams(IntToStr(FLineThis),'','');
End;

Procedure TMemViewBase.Paint;
Begin
 Inherited;

 Canvas.Draw(0,0,FViewBitmap);
End;

Procedure TMemViewBase.Resize;
Begin
 Inherited;

 FViewBitmap.SetSize(Width,Height-FStatBar.Height);
 FTextBitmap.SetSize(FViewBitmap.Width-4-FScrollBar.Width,FViewBitmap.Height-4);

 FScrollBar.Left:=FViewBitmap.Width-FScrollBar.Width-1; FScrollBar.Top:=1; FScrollBar.Height:=FTextBitmap.Height;

 FStatBar.SetBounds(0,Height-FStatBar.Height,Width,FStatBar.Height);

 ScrollUpdate;
 PaintA;
End;

Procedure TMemViewBase.ScrollUpdate;
Begin
 FScrollBar.SetParams(FLineStartV,0,FLineCount,FTextBitmap.Height div FLineHeight);
End;

Function TMemViewBase.IsLineHighlight ( ALineIdx : Integer ) : boolean;
Begin
 Result:=(ALineIdx=FLineThis) and FPaintFocused;
End;

Procedure TMemViewBase.DrawRGutter;
Var
  BCanvas       : TCanvas;
  BPosA,
  BPosB         : Integer;
  BLineCnt,
  BLineCntV     : Integer;
  BPosYA,
  BPosYB        : Integer;
  BLineIdx      : Integer;
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
 BLineCnt:=FLineCount;
 BLineCntV:=(FTextBitmap.Height+FLineHeight-1) div FLineHeight;
 if (BLineCnt<=0) or (BLineCntV<=0) then break;
 BCanvas.Brush.Color:=$E0E0E0;
 BPosYA:=(FTextBitmap.Height* FLineStartV           ) div BLineCnt; if BPosYA>=FTextBitmap.Height then BPosYA:=FTextBitmap.Height;
 BPosYB:=(FTextBitmap.Height*(FLineStartV+BLineCntV)) div BLineCnt; if BPosYB>=FTextBitmap.Height then BPosYB:=FTextBitmap.Height;
 BCanvas.FillRect(BPosA+6,BPosYA,BPosB,BPosYB);
 // Line highlight
 BLineIdx:=0;
 while BLineIdx<FLineCount do
  begin
  if FChangeList[BLineIdx] then
   begin
   BPosYA:=(FTextBitmap.Height*(BLineIdx+0)) div BLineCnt;
   BPosYB:=(FTextBitmap.Height*(BLineIdx+1)) div BLineCnt;
   BCanvas.Brush.Color:=$FF6060;
   BCanvas.FillRect(BPosA+6,BPosYA,BPosA+10,BPosYB+1);
   end;
  if IsLineHighlight(BLineIdx) then
   begin
   BPosYA:=(FTextBitmap.Height*(BLineIdx+0)) div BLineCnt;
   BPosYB:=(FTextBitmap.Height*(BLineIdx+1)) div BLineCnt;
   BCanvas.Brush.Color:=$0090D0;
   BCanvas.FillRect(BPosA+6,BPosYA,BPosA+10,BPosYB+1);
   end;
  inc(BLineIdx);
  end;
 until TRUE;

End;

Procedure TMemViewBase.PaintA;
Var
  BLineCntV     : Integer; // Visible lines count
  BLineIdxScr,
  BLineIdxLst   : Cardinal;
  BPosY         : Integer;
  BWidthA,
  BWidthB       : Integer;
  BColorA,
  BColorB       : TColor;
  BHighlight    : boolean;
  BDataThis,
  BDataPrev     : string;
  BAddrS        : string;
  BIndex        : Integer;
Begin
 FPaintFocused:=TRUE; //Focused;
 BmpBox(FViewBitmap.Canvas,0,0,FViewBitmap.Width,FViewBitmap.Height,$808080,$FFFFFF,CColorBG);

 FTextBitmap.Canvas.Font.Name:='Courier New';
 FTextBitmap.Canvas.Font.Size:=10;
 FTextBitmap.Canvas.Font.Color:=0;
 FTextBitmap.Canvas.Brush.Style:=bsSolid;
 FTextBitmap.Canvas.Pen.Style:=psSolid;
 BLineCntV:=(FTextBitmap.Height+FLineHeight-1) div FLineHeight;

 BWidthA:=FTextBitmap.Canvas.TextWidth('AAAAAAAA');
 BWidthB:=FTextBitmap.Canvas.TextWidth('00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00');
 FLinePosA:=16;
 FLinePosB:=FLinePosA+BWidthA+8;
 FLinePosC:=FLinePosB+BWidthB+12;

 FWidthHex:=FTextBitmap.Canvas.TextWidth('AA_');
 FWidthChr:=FTextBitmap.Canvas.TextWidth('X');

 FTextBitmap.Canvas.Brush.Color:=FColorANorm;
 FTextBitmap.Canvas.FillRect(0,0,FLinePosB,FTextBitmap.Height);
 FTextBitmap.Canvas.Brush.Color:=FColorBNorm;
 FTextBitmap.Canvas.FillRect(FLinePosB,0,FTextBitmap.Width,FTextBitmap.Height);

 FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosC,0,FLinePosC,FTextBitmap.Height);

 BPosY:=0;
 BLineIdxScr:=0;
 while BLineIdxScr<BLineCntV do
  begin
  BLineIdxLst:=BLineIdxScr+FLineStartV;
  if BLineIdxLst>=FLineCount then break;
  BDataPrev:=GetLineData(FDataPrev,BLineIdxLst);
  BDataThis:=GetLineData(FDataThis,BLineIdxLst);
  BAddrS:=IntToHex(FSegStart+16*BLineIdxLst,8);
  for BIndex:=1 to Length(BAddrS)-1 do
   begin
   if BAddrS[BIndex]<>'0' then break;
   BAddrS[BIndex]:=' ';
   end;
  BHighlight:=IsLineHighlight(BLineIdxLst);
  FTextBitmap.Canvas.Brush.Style:=bsSolid;
  if BHighlight then begin BColorA:=FColorAThis; BColorB:=FColorBThis; end
  else begin BColorA:=FColorANorm; BColorB:=FColorBNorm; end;
  FTextBitmap.Canvas.Brush.Color:=BColorA; FTextBitmap.Canvas.FillRect(0,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
  FTextBitmap.Canvas.Brush.Color:=BColorB; FTextBitmap.Canvas.FillRect(FLinePosB,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
  FTextBitmap.Canvas.Brush.Style:=bsClear;
  FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosC,BPosY,FLinePosC,BPosY+FLineHeight);
  ViewLine(FTextBitmap,BPosY,BAddrS,BDataPrev,BDataThis);
  inc(BPosY,FLineHeight);
  inc(BLineIdxScr);
  end;

 DrawRGutter;
 FViewBitmap.Canvas.Draw(2,2,FTextBitmap);

 Canvas.Draw(0,0,FViewBitmap);
End;

Procedure TMemViewBase.ViewLine ( ABitmap : TBitmap; APosY : Integer; Const AAddrS, ADataPrev, ADataThis : string );
Var
  BCharIdx  : Integer;
  BDataC    : char;
  BSame     : boolean;
Begin
 ABitmap.Canvas.Font.Color:=$000000;
 ABitmap.Canvas.TextOut(FLinePosA,APosY,AAddrS);
 BCharIdx:=0;
 while BCharIdx<Length(ADataThis) do
  begin
  BDataC:=ADataThis[1+BCharIdx];
  BSame:=(BCharIdx<Length(ADataPrev)) and (ADataPrev[1+BCharIdx]=BDataC);
  if BSame then ABitmap.Canvas.Font.Color:=CColorSame
  else ABitmap.Canvas.Font.Color:=CColorDiff;
  //if BSame then ABitmap.Canvas.Font.Style:=[]
  //else ABitmap.Canvas.Font.Style:=[fsBold];
  ABitmap.Canvas.TextOut(4+FLinePosB+FWidthHex*BCharIdx,APosY,IntToHex(Ord(BDataC),2));
  ABitmap.Canvas.TextOut(8+FLinePosC+FWidthChr*BCharIdx,APosY,BDataC);
  inc(BCharIdx);
  end;
End;

end.

