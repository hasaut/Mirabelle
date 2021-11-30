unit DbgViewStack_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, AsmTypes_sd, DbgViewBase_sd, ConComI;

Type
  TDbgViewStack = class(TCustomControl)
  private
  protected
    FDataList   : TStringList;
    FLineThis   : Integer; // Where the own lines in LST are
    FLineStartB : Integer; // First visible line
    FScrollU,
    FScrollD    : TScrollBtn;
    FScrollB    : TScrollBarV;

    FEspThis    : Cardinal;

    FViewBitmap         : TBitmap;
    FLineHeight,
    FSymbolWidth        : Integer;
    FColorANorm,
    FColorAThis,
    FColorBNorm,
    FColorBThis         : TColor;
    FColorHNorm,
    FColorHThis         : TColor;
    FColorPNorm,
    FColorPThis         : TColor;

    FOffsetWidth        : Integer;

    Procedure ScrollMoved ( AStart : Integer );
    Procedure ViewLine ( ATop : Integer; Const ADataS : string; AColorIdx : Integer; AThis : boolean );

  protected
    Procedure Paint; Override;
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure KeyDown ( Var AKey : Word; AShift : TShiftState ); Override;
    Function DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean; Override;

    Procedure ScrollUpdate;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure ClearData;
    Procedure AppendData ( Const AEspS, ADataSH, ADataSI : string );
    Procedure InvalidateData ( AEspThis : Cardinal );

    Function CanFocus : boolean; Override;
    Procedure SetFocus; Override;
    Procedure PaintA; Virtual;
  end;

implementation

Uses
  ConComL;

{ TDbgViewStack }

Constructor TDbgViewStack.Create ( AOwner : TComponent );
Begin
 Inherited;
 FDataList:=TStringList.Create;

 FScrollU:=TScrollBtn.Create(Self); InsertControl(FScrollU); FScrollU.BtnKind:=bkUp;
 FScrollD:=TScrollBtn.Create(Self); InsertControl(FScrollD); FScrollD.BtnKind:=bkDn;

 FScrollB:=TScrollBarV.Create(Self); InsertControl(FScrollB);
 FScrollB.OnMoved:=@ScrollMoved;

 FViewBitmap:=TBitmap.Create;
 //FViewBitmap.PixelFormat:=pf32bit;
 FViewBitmap.Canvas.Font.Name:='Courier New';
 FViewBitmap.Canvas.Font.Size:=10;
 FViewBitmap.Canvas.Font.Color:=0;
 FLineHeight:=FViewBitmap.Canvas.TextHeight('Ayla');
 FSymbolWidth:=FViewBitmap.Canvas.TextWidth('0');
 FOffsetWidth:=FViewBitmap.Canvas.TextWidth('00000000')+15;


 FColorANorm:=$D0D8D8;
 FColorAThis:=$70C8D8;
 FColorBNorm:=$FFFFFF;
 FColorBThis:=$80E0F0;
 FColorHNorm:=$C0C8C8; // $C0C0C0;
 FColorHThis:=$70B8C8; // $A0D0D0;
 FColorPNorm:=$C0C8C8; // $E0C0C0;
 FColorPThis:=$70B8C8; // $C0D0D0;
End;

Destructor TDbgViewStack.Destroy;
Begin
 FViewBitmap.Free;

 RemoveControl(FScrollB); FScrollB.Free;
 RemoveControl(FScrollD); FScrollD.Free;
 RemoveControl(FScrollU); FScrollU.Free;

 FDataList.Free;
 Inherited;
End;

Function TDbgViewStack.CanFocus : boolean;
Begin
 Inherited;
 Result:=TRUE;
End;

Procedure TDbgViewStack.SetFocus;
Begin
 Inherited;
 ScrollUpdate;
End;

Procedure TDbgViewStack.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX,AY : Integer );
Var
  BLineThis     : Integer;
  BPaint        : boolean;
Begin
 Inherited;
 SetFocus;
 BPaint:=FALSE;
 BLineThis:=FLineStartB-((Height-AY-4) div FLineHeight);
 if (BLineThis>0) and (BLineThis<FDataList.Count) and (BLineThis<>FLineThis) then
  begin
  FLineThis:=BLineThis;
  BPaint:=TRUE;
  end;

 if BPaint then PaintA;
End;

Procedure TDbgViewStack.KeyDown ( Var AKey : Word; AShift : TShiftState );
Var
  BPaint        : boolean;
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;

Begin
 Inherited;

 BPaint:=FALSE;

 BLineCountV:=FViewBitmap.Height div FLineHeight;
 case AKey of
   VK_UP:
     begin
     if FLineThis>0 then
      begin
      Dec(FLineThis);
      BPaint:=TRUE;
      end;
     end;

   VK_DOWN:
     begin
     if (FLineThis+1)<FDataList.Count then
      begin
      Inc(FLineThis);
      BPaint:=TRUE;
      end;
     end;

   VK_PRIOR:
     begin
     BLineThis:=FLineThis-BLineCountV;
     BLineStartV:=FLineStartB-BLineCountV;
     if BLineThis<0 then BLineThis:=0;
     if BLineStartV<0 then BLineStartV:=0;
     if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartB) then
      begin
      FLineThis:=BLineThis;
      FLineStartB:=BLineStartV;
      BPaint:=TRUE;
      end;
     end;

   VK_NEXT:
     begin
     BLineThis:=FLineThis+BLineCountV;
     BLineStartV:=FLineStartB+BLineCountV;
     if BLineThis>=FDataList.Count then BLineThis:=FDataList.Count-1;
     if BLineStartV>=FDataList.Count then BLineStartV:=FDataList.Count-1;
     if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartB) then
      begin
      FLineThis:=BLineThis;
      FLineStartB:=BLineStartV;
      BPaint:=TRUE;
      end;
     end;


 end; // Case

 if BPaint then
  begin
  if FLineThis<=(FLineStartB-BLineCountV) then FLineStartB:=(FLineThis+BLineCountV-1);
  if FLineThis>FLineStartB then FLineStartB:=FLineThis;
  if FLineStartB<0 then FLineStartB:=0;
  if FLineStartB>=FDataList.Count then FLineStartB:=FDataList.Count-1;
  ScrollUpdate;
  PaintA;
  end;

End;

Function TDbgViewStack.DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean;
Var
  BLineStartB   : Integer;
  BWheelDelta   : Integer;
  BVisibleCnt   : Integer;
Begin
 Inherited;

 if AWheelDelta<0 then BWheelDelta:=3
 else if AWheelDelta>0 then BWheelDelta:=-3
 else BWheelDelta:=0;

 repeat
 if FLineHeight=0 then break;
 BVisibleCnt:=FViewBitmap.Height div FLineHeight;

 BLineStartB:=FLineStartB+BWheelDelta;
 if BLineStartB>=FDataList.Count then BLineStartB:=FDataList.Count-1;
 if BLineStartB<BVisibleCnt then BLineStartB:=BVisibleCnt;
 if BLineStartB<>FLineStartB then
  begin
  FLineStartB:=BLineStartB;
  ScrollUpdate;
  PaintA;
  end;

 until TRUE;
 Result:=TRUE;
End;

Procedure TDbgViewStack.ScrollMoved ( AStart : Integer );
Begin
 FLineStartB:=0+FScrollB.LineTop+FScrollB.LineVisible;
 PaintA;
End;

Const
  CHeaderHeight = 17;

Procedure TDbgViewStack.Paint;
Begin
 Inherited;

 Canvas.Brush.Color:=clBtnFace;
 Canvas.Brush.Style:=bsSolid;
 Canvas.FillRect(1,2,100,2+CHeaderHeight-1);
 Canvas.FillRect(102,2,Width-1,2+CHeaderHeight-1);

 Canvas.Font.Name:='Arial';
 Canvas.Font.Size:=8;
 Canvas.Font.Style:=[];
 Canvas.Font.Color:=$000000;

 BmpBox(Canvas,0,0,Width,Height,$808080,$FFFFFF,$404040,$D0D0D0,CColorBg);
 BmpBox(Canvas,2,2,FOffsetWidth,CHeaderHeight,$FFFFFF,$808080,CColorBg);
 BmpBox(Canvas,FOffsetWidth+2,2,Width-FOffsetWidth-2-FScrollU.Width-2,CHeaderHeight,$FFFFFF,$808080,CColorBg);

 Canvas.Brush.Style:=bsClear;
 Canvas.TextOut(5,4,'Esp+'); Canvas.TextOut(FOffsetWidth+5,4,'Data');

 PaintA;
End;

Procedure TDbgViewStack.PaintA;
Var
  BLineCnt,
  BLineIdx      : Integer;
  BStrIdx       : Integer;
  BTop          : Integer;
Begin
 Canvas.Font.Name:='Courier New';
 Canvas.Font.Size:=8;
 Canvas.Font.Style:=[];
 Canvas.Font.Color:=$000000;

 FViewBitmap.Canvas.Brush.Style:=bsSolid;
 FViewBitmap.Canvas.Brush.Color:=CColorBg;
 FViewBitmap.Canvas.FillRect(0,0,FViewBitmap.Width,FViewBitmap.Height);

 if FLineStartB>=FDataList.Count then FLineStartB:=FDataList.Count-1;

 BLineCnt:=(FViewBitmap.Height+FLineHeight-1) div FLineHeight;
 BLineIdx:=0;
 while BLineIdx<BLineCnt do
  begin
  BTop:=FViewBitmap.Height-BLineIdx*FLineHeight-FLineHeight;
  BStrIdx:=FLineStartB-BLineIdx;
  if BStrIdx<0 then break;
  ViewLine(BTop,FDataList.Strings[BStrIdx],(FEspThis div 4)+(FDataList.Count-FLineStartB-1)+BLineIdx,BStrIdx=FLineThis);
  inc(BLineIdx);
  end;
 Canvas.Draw(2,2+CHeaderHeight,FViewBitmap);
End;

Const
  CBgColorList : array [0..7] of Cardinal =
   (
    $FFFFFF,
    $F0F0F0,
    $E0E0E0,
    $D0D0D0,
    $C0C0C0,
    $D0D0D0,
    $E0E0E0,
    $F0F0F0
   );

Procedure TDbgViewStack.ViewLine ( ATop : Integer; Const ADataS : string; AColorIdx : Integer; AThis : boolean );
Var
  BDataS        : string;
  BEspS,
  BDataSH       : string;
Begin
 FViewBitmap.Canvas.Brush.Style:=bsSolid;
 FViewBitmap.Canvas.Brush.Color:=CBgColorList[AColorIdx mod Length(CBgColorList)];
 FViewBitmap.Canvas.FillRect(0,ATop,FViewBitmap.Width,ATop+FLineHeight);
 BDataS:=ADataS;
 BEspS:=ReadParamStr(BDataS);
 BDataSH:=ReadParamStr(BDataS);
 FViewBitmap.Canvas.Brush.Style:=bsClear;
 FViewBitmap.Canvas.TextOut(10,ATop,BEspS);
 FViewBitmap.Canvas.TextOut(6+FOffsetWidth,ATop,BDataSH);

 FViewBitmap.Canvas.Pen.Style:=psSolid;
 FViewBitmap.Canvas.Brush.Style:=bsSolid;

 if AThis then
  begin
  FViewBitmap.Canvas.Brush.Color:=$FF00FF;
  FViewBitmap.Canvas.FillRect(0,ATop,2,ATop+FLineHeight);
  FViewBitmap.Canvas.FillRect(FOffsetWidth-3,ATop,FOffsetWidth+2,ATop+FLineHeight);
  FViewBitmap.Canvas.FillRect(FViewBitmap.Width-2,ATop,FViewBitmap.Width,ATop+FLineHeight);
  FViewBitmap.Canvas.Pen.Color:=$800080; FViewBitmap.Canvas.Line(FOffsetWidth-1,ATop,FOffsetWidth-1,ATop+FLineHeight);
  FViewBitmap.Canvas.Pen.Color:=$FF20FF; FViewBitmap.Canvas.Line(FOffsetWidth-0,ATop,FOffsetWidth-0,ATop+FLineHeight);
  end
 else
  begin
  FViewBitmap.Canvas.Pen.Color:=$808080; FViewBitmap.Canvas.Line(FOffsetWidth-1,ATop,FOffsetWidth-1,ATop+FLineHeight);
  FViewBitmap.Canvas.Pen.Color:=$FFFFFF; FViewBitmap.Canvas.Line(FOffsetWidth-0,ATop,FOffsetWidth-0,ATop+FLineHeight);
  end;

End;

Procedure TDbgViewStack.Resize;
Begin
 Inherited;

 FViewBitmap.Width:=Width-4-FScrollU.Width;
 FViewBitmap.Height:=Height-CHeaderHeight-4;

 FScrollU.Top:=2; FScrollU.Left:=2+FViewBitmap.Width;
 FScrollD.Top:=2+Height-FScrollD.Height-3; FScrollD.Left:=FScrollU.Left;
 FScrollB.Left:=FScrollU.Left; FScrollB.Top:=FScrollU.Top+FScrollU.Height; FScrollB.Height:=FScrollD.Top-FScrollB.Top;;

 ScrollUpdate;
End;

Procedure TDbgViewStack.ScrollUpdate;
Var
  BVisibleCnt   : Integer;
Begin
 repeat
 if FLineHeight=0 then break;
 BVisibleCnt:=FViewBitmap.Height div FLineHeight;
 FScrollB.SetPos(FLineStartB-BVisibleCnt,BVisibleCnt,FDataList.Count);
 until TRUE;
End;

Procedure TDbgViewStack.ClearData;
Begin
 FDataList.Clear;
End;

Procedure TDbgViewStack.AppendData ( Const AEspS, ADataSH, ADataSI : string );
Begin
 FDataList.Append(AEspS+' '+ADataSH+' '+ADataSI);
End;

Procedure TDbgViewStack.InvalidateData ( AEspThis : Cardinal );
Begin
 FEspThis:=AEspThis;
 FLineStartB:=FDataList.Count-1;
 FLineThis:=FLineStartB;
End;

end.

