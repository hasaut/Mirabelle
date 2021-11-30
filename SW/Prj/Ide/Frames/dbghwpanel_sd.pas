unit DbgHwPanel_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics;

Type
  TDbgHwPanel = class(TCustomControl)
  private
    FViewBitmap         : TBitmap;
    FLineHeight,
    FSymbolWidth        : Integer;
    FText               : string;
    FColor              : Cardinal;

    Procedure DrawFrame ( APosL, APosT, APosR, APosB : Integer; AColorA, AColorB, AColorG : Cardinal );
  protected
    Procedure Paint; Override;
    Procedure Resize; Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA;
    Procedure SetText ( Const AText : string );
    Procedure SetColorA ( AColor : Cardinal );
  end;

implementation

Uses
  ConComL;

Constructor TDbgHwPanel.Create ( AOwner : TComponent );
Begin
 Inherited;
 FViewBitmap:=TBitmap.Create;
 FViewBitmap.Canvas.Font.Name:='Arial';
 FViewBitmap.Canvas.Font.Size:=8;
 FViewBitmap.Canvas.Font.Color:=0;
 FLineHeight:=FViewBitmap.Canvas.TextHeight('Ayla');
 FSymbolWidth:=FViewBitmap.Canvas.TextWidth('0');
 Height:=67; Width:=10;
End;

Destructor TDbgHwPanel.Destroy;
Begin
 FViewBitmap.Free;
 Inherited;
End;

Procedure TDbgHwPanel.Resize;
Begin
 Inherited;

 FViewBitmap.Width:=Width;
 FViewBitmap.Height:=Height;
End;

Procedure TDbgHwPanel.Paint;
Begin
 PaintA;
End;

Procedure TDbgHwPanel.SetText ( Const AText : string );
Begin
 FText:=AText;
 PaintA;
End;

Procedure TDbgHwPanel.SetColorA ( AColor : Cardinal );
Begin
 FColor:=AColor;
 PaintA;
End;

Procedure TDbgHwPanel.DrawFrame ( APosL, APosT, APosR, APosB : Integer; AColorA, AColorB, AColorG : Cardinal );
Begin
 FViewBitmap.Canvas.Brush.Style:=bsSolid;
 FViewBitmap.Canvas.Brush.Color:=AColorG;
 FViewBitmap.Canvas.FillRect(APosL+1,APosT+1,APosR-1,APosB-1);
 FViewBitmap.Canvas.Pen.Style:=psSolid;
 FViewBitmap.Canvas.Pen.Color:=AColorA;
 FViewBitmap.Canvas.Line(APosL,APosT,APosR,APosT);
 FViewBitmap.Canvas.Line(APosL,APosT,APosL,APosB);
 FViewBitmap.Canvas.Pen.Color:=AColorB;
 FViewBitmap.Canvas.Line(APosR-1,APosT,APosR-1,APosB);
 FViewBitmap.Canvas.Line(APosL,APosB-1,APosR,APosB-1);
End;

Procedure TDbgHwPanel.PaintA;
Var
  BHeight,
  BWidth        : Integer;
  BReadS        : string;
  BParamA       : string;
  BPosY         : Integer;
  BColorB       : Cardinal;
Begin
 BColorB:=clForm; // $C8D0D4;
 BHeight:=FViewBitmap.Canvas.TextHeight('Hardware');
 DrawFrame(0,0,FViewBitmap.Width,FViewBitmap.Height,$808080,$FFFFFF,BColorB);
 DrawFrame(1,1,FViewBitmap.Width-1,BHeight+4,$FFFFFF,$808080,BColorB);
 FViewBitmap.Canvas.Font.Color:=$000000;
 FViewBitmap.Canvas.Brush.Style:=bsClear;
 BWidth:=FViewBitmap.Canvas.TextWidth('Hardware');
 FViewBitmap.Canvas.TextOut((FViewBitmap.Width-BWidth) div 2,2,'Hardware');
 BReadS:=FText;
 FViewBitmap.Canvas.Font.Color:=FColor;
 BPosY:=BHeight+4;
 BParamA:=ReadTillC(BReadS,#13);
 FViewBitmap.Canvas.TextOut(2,BPosY,BParamA); Inc(BPosY,FLineHeight);
 FViewBitmap.Canvas.TextOut(2,BPosY,BReadS);

 Canvas.Draw(0,0,FViewBitmap);
End;

end.

