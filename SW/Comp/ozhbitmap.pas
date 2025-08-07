unit OzhBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics,
  AviTypes;

Type
  TObColor = Cardinal;
  TObColors = array of TObColor;

  POzhBitmap = ^TOzhBitmap;

  TOzhBitmap = class(TObject)
  private
    FWidth,
    FHeight     : Integer;
    FColors     : TObColors;

    Procedure LineH ( AXA, AXB, AY : Integer; AColor : TObColor );
    Procedure LineV ( AX, AYA, AYB : Integer; AColor : TObColor );
    Procedure LineFH ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
    Procedure LineFV ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure SetSize ( AWidth, AHeight : Integer );

    Procedure Line ( AXA, AYA, AXB, AYB : Integer; AColor : TObColor );
    Procedure LineF ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
    Procedure LineF ( Const APointA, APointB : TVect2s; AColor : TObColor );

    Procedure FillRect ( AXA, AYA, AXB, AYB : Integer; AColor : TObColor );
    Procedure BmpBox ( AColorF : TColor );
    Procedure BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB : TColor );
    Procedure BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD : TColor );
    Procedure BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorF : TColor );
    Procedure BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorF : TColor );
    Procedure BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );

    Procedure Draw ( AX, AY : Integer; ASrc : TOzhBitmap );

    Procedure ExportBmp ( ABmp : TBitmap );
    Procedure ExportBmp ( AX, AY : Integer; ABmp : TBitmap );
    Procedure DrawInto ( ABmp : TBitmap; APosX, APosY : Integer );

    property Colors : TObColors read FColors write FColors;
    property Width : Integer read FWidth;
    property Height : Integer read FHeight;
  end;

implementation

Constructor TOzhBitmap.Create;
Begin
 Inherited;
End;

Destructor TOzhBitmap.Destroy;
Begin
 FColors:=nil;
 Inherited;
End;

Procedure TOzhBitmap.SetSize ( AWidth, AHeight : Integer );
Begin
 repeat
 if (FWidth=AWidth) and (FHeight=AHeight) then break;
 FWidth:=AWidth; FHeight:=AHeight;
 if FWidth<0 then FWidth:=0; if FHeight<0 then FHeight:=0;
 SetLength(FColors,FHeight*FWidth);
 until TRUE;
End;

Procedure TOzhBitmap.LineH ( AXA, AXB, AY : Integer; AColor : TObColor );
Var
  BXS,
  BXE           : Integer;
  BIndex        : Integer;
Begin
 repeat
 if AY<0 then break;
 if AY>=FHeight then break;
 if AXA<AXB then begin BXS:=AXA; BXE:=AXB; end
 else begin BXS:=AXB; BXE:=AXA; end;
 BIndex:=BXS; if BIndex<0 then BIndex:=0;
 if BXE>FWidth then BXE:=FWidth;
 while BIndex<BXE do
  begin
  FColors[AY*FWidth+BIndex]:=AColor;
  inc(BIndex);
  end;
 until TRUE;
End;

Procedure TOzhBitmap.LineV ( AX, AYA, AYB : Integer; AColor : TObColor );
Var
  BYS,
  BYE           : Integer;
  BIndex        : Integer;
Begin
 repeat
 if AX<0 then break;
 if AX>=FWidth then break;
 if AYA<AYB then begin BYS:=AYA; BYE:=AYB; end
 else begin BYS:=AYB; BYE:=AYA; end;
 BIndex:=BYS; if BIndex<0 then BIndex:=0;
 if BYE>FHeight then BYE:=FHeight;
 while BIndex<BYE do
  begin
  FColors[BIndex*FWidth+AX]:=AColor;
  inc(BIndex);
  end;
 until TRUE;
End;

Procedure TOzhBitmap.LineFH ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
Var
  BXS,
  BXE           : Integer;
  BYS,
  BYD           : Single;
  BIndex        : Integer;
  BInc          : Integer;
  BYThis        : Single;
  BIdxH,
  BIdxL         : Integer;
  BMulH,
  BMulL         : Single;
  BColor,
  BColorH,
  BColorL       : Cardinal;
  BColorI       : array [0..2] of byte absolute BColor;
  BColorHI      : array [0..2] of byte absolute BColorH;
  BColorLI      : array [0..2] of byte absolute BColorL;
Begin
 BColor:=AColor;

 repeat
 BXS:=Round(AXA+0.499);
 BXE:=Round(AXB+0.499);
 if BXS=BXE then break;
 BYD:=(AYB-AYA)/(AXB-AXA);
 BYS:=BYD*0.5;

 BIndex:=BXS;
 if BXE>BXS then BInc:=1 else BInc:=-1;
 while BIndex<>BXE do
  begin
  BYThis:=AYA+BYS+BYD*(BIndex-BXS);
  BIdxL:=Trunc(BYThis); BIdxH:=BIdxL+1;
  BMulH:=Frac(BYThis); BMulL:=1-BMulH;
  if (BIdxL>0) and (BIdxL<FHeight) and
     (BIdxH>0) and (BIdxH<FHeight) and
     (BIndex>0) and (BIndex<FWidth) then
   begin
   BColorH:=FColors[BIdxH*FWidth+BIndex];
   BColorL:=FColors[BIdxL*FWidth+BIndex];
   BColorHI[0]:=Round(BColorHI[0]*BMulL + BColorI[0]*BMulH);
   BColorHI[1]:=Round(BColorHI[1]*BMulL + BColorI[1]*BMulH);
   BColorHI[2]:=Round(BColorHI[2]*BMulL + BColorI[2]*BMulH);
   BColorLI[0]:=Round(BColorLI[0]*BMulH + BColorI[0]*BMulL);
   BColorLI[1]:=Round(BColorLI[1]*BMulH + BColorI[1]*BMulL);
   BColorLI[2]:=Round(BColorLI[2]*BMulH + BColorI[2]*BMulL);
   FColors[BIdxH*FWidth+BIndex]:=BColorH;
   FColors[BIdxL*FWidth+BIndex]:=BColorL;
   end;
  BIndex:=BIndex+BInc;
  end;
 until TRUE;
End;

Procedure TOzhBitmap.LineFV ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
Var
  BYS,
  BYE           : Integer;
  BXS,
  BXD           : Single;
  BIndex        : Integer;
  BInc          : Integer;
  BXThis        : Single;
  BIdxR,
  BIdxL         : Integer;
  BMulR,
  BMulL         : Single;
  BColor,
  BColorR,
  BColorL       : Cardinal;
  BColorI       : array [0..2] of byte absolute BColor;
  BColorRI      : array [0..2] of byte absolute BColorR;
  BColorLI      : array [0..2] of byte absolute BColorL;
Begin
 BColor:=AColor;

 repeat
 BYS:=Round(AYA+0.499);
 BYE:=Round(AYB+0.499);
 if BYS=BYE then break;
 BXD:=(AXB-AXA)/(AYB-AYA);
 BXS:=BXD*0.5;

 BIndex:=BYS;
 if BYE>BYS then BInc:=1 else BInc:=-1;
 while BIndex<>BYE do
  begin
  BXThis:=AXA+BXS+BXD*(BIndex-BYS);
  BIdxL:=Trunc(BXThis); BIdxR:=BIdxL+1;
  BMulR:=Frac(BXThis); BMulL:=1-BMulR;
  if (BIdxL>0) and (BIdxL<FWidth) and
     (BIdxR>0) and (BIdxR<FWidth) and
     (BIndex>0) and (BIndex<FHeight) then
   begin
   BColorR:=FColors[BIndex*FWidth+BIdxR];
   BColorL:=FColors[BIndex*FWidth+BIdxL];
   BColorRI[0]:=Round(BColorRI[0]*BMulL + BColorI[0]*BMulR);
   BColorRI[1]:=Round(BColorRI[1]*BMulL + BColorI[1]*BMulR);
   BColorRI[2]:=Round(BColorRI[2]*BMulL + BColorI[2]*BMulR);
   BColorLI[0]:=Round(BColorLI[0]*BMulR + BColorI[0]*BMulL);
   BColorLI[1]:=Round(BColorLI[1]*BMulR + BColorI[1]*BMulL);
   BColorLI[2]:=Round(BColorLI[2]*BMulR + BColorI[2]*BMulL);
   FColors[BIndex*FWidth+BIdxR]:=BColorR;
   FColors[BIndex*FWidth+BIdxL]:=BColorL;
   end;
  BIndex:=BIndex+BInc;
  end;
 until TRUE;
End;

Procedure TOzhBitmap.Line ( AXA, AYA, AXB, AYB : Integer; AColor : TObColor );
Begin
 if AXA=AXB then LineV(AXA,AYA,AYB,AColor)
 else if AYA=AYB then LineH(AXA,AXB,AYA,AColor)
 else LineF(AXA,AYA,AXB,AYB,AColor);
End;

Procedure TOzhBitmap.LineF ( AXA, AYA, AXB, AYB : Single; AColor : TObColor );
Begin
 if Abs(AXB-AXA)>Abs(AYB-AYA) then LineFH(AXA,AYA,AXB,AYB,AColor)
 else LineFV(AXA,AYA,AXB,AYB,AColor);
End;

Procedure TOzhBitmap.LineF ( Const APointA, APointB : TVect2s; AColor : TObColor );
Begin
 LineF(APointA.FX,APointA.FY,APointB.FX,APointB.FY,AColor);
End;

Procedure TOzhBitmap.FillRect ( AXA, AYA, AXB, AYB : Integer; AColor : TObColor );
Var
  BXS,
  BXE           : Integer;
  BYS,
  BYE           : Integer;
  BIndexX,
  BIndexY       : Integer;
Begin
 if AXA<AXB then begin BXS:=AXA; BXE:=AXB; end
 else begin BXS:=AXB; BXE:=AXA; end;
 if AYA<AYB then begin BYS:=AYA; BYE:=AYB; end
 else begin BYS:=AYB; BYE:=AYA; end;
 if BXS<0 then BXS:=0; if BXE>FWidth then BXE:=FWidth;
 if BYS<0 then BYS:=0; if BYE>FHeight then BYE:=FHeight;
 BIndexY:=BYS;
 while BIndexY<BYE do
  begin
  BIndexX:=BXS;
  while BIndexX<BXE do
   begin
   FColors[BIndexY*FWidth+BIndexX]:=AColor;
   inc(BIndexX);
   end;
  inc(BIndexY);
  end;
End;

Procedure TOzhBitmap.BmpBox ( AColorF : TColor );
Begin
 FillRect(0,0,FWidth,FHeight,AColorF);
End;

Procedure TOzhBitmap.BmpBox (ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB : TColor );
Begin
 Line(ALeft,ATop,ALeft+AWidth,ATop,AColorA);
 Line(ALeft,ATop,ALeft,ATop+AHeight,AColorA);
 Line(ALeft+AWidth-1,ATop,ALeft+AWidth-1,ATop+AHeight,AColorB);
 Line(ALeft,ATop+AHeight-1,ALeft+AWidth,ATop+AHeight-1,AColorB);
End;

Procedure TOzhBitmap.BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD : TColor );
Begin
 BmpBox(ALeft,ATop,AWidth,AHeight,AColorA,AColorB);
 BmpBox(ALeft+1,ATop+1,AWidth-2,AHeight-2,AColorC,AColorD);
End;

Procedure TOzhBitmap.BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorF : TColor );
Begin
 FillRect(ALeft,ATop,ALeft+AWidth,ATop+AHeight,AColorF);
End;

Procedure TOzhBitmap.BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorF : TColor );
Begin
 BmpBox(ALeft,ATop,AWidth,AHeight,AColorA,AColorB);
 FillRect(ALeft+1,ATop+1,ALeft+AWidth-1,ATop+AHeight-1,AColorF);
End;

Procedure TOzhBitmap.BmpBox ( ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );
Begin
 BmpBox(ALeft,ATop,AWidth,AHeight,AColorA,AColorB,AColorC,AColorD);
 FillRect(ALeft+2,ATop+2,ALeft+AWidth-2,ATop+AHeight-2,AColorF);
End;

Procedure TOzhBitmap.ExportBmp ( ABmp : TBitmap );
Var
  BRowIdx,
  BColIdx       : Integer;
  BImage        : TLazIntfImage;
Begin
 ABmp.SetSize(FWidth,FHeight);
 BImage:=ABmp.CreateIntfImage;
 for BRowIdx:=0 to FHeight-1 do
  begin
  for BColIdx:=0 to FWidth-1 do
   begin
   //BImage.Colors[BColIdx,BRowIdx]:=TColorToFPColor(FColors[FHeight-1-BRowIdx,BColIdx]);
   BImage.Colors[BColIdx,BRowIdx]:=TColorToFPColor(FColors[BRowIdx*FWidth+BColIdx]);
   end;
  end;
 ABmp.LoadFromIntfImage(BImage);
 BImage.Free;
End;

Procedure TOzhBitmap.ExportBmp ( AX, AY : Integer; ABmp : TBitmap );
Var
  BImage        : TLazIntfImage;
  BXSrc,
  BYSrc         : Integer;
  BXDst,
  BYDst         : Integer;
Begin
 BImage:=ABmp.CreateIntfImage;
 for BYDst:=0 to BImage.Height-1 do
  begin
  BYSrc:=AY+BYDst;
  if (BYSrc>=0) and (BYSrc<FHeight) then
   begin
   for BXDst:=0 to BImage.Width-1 do
    begin
    BXSrc:=AX+BXDst;
    if (BXSrc>=0) and (BXSrc<FWidth) then BImage.Colors[BXDst,BYDst]:=TColorToFPColor(FColors[BYSrc*FWidth+BXSrc])
    else BImage.Colors[BXDst,BYDst]:=TColorToFPColor(0);
    end;
   end
  else
   begin
   for BXDst:=0 to BImage.Width-1 do
    begin
    BImage.Colors[BXDst,BYDst]:=TColorToFPColor(0);
    end;
   end;
  end;
 ABmp.LoadFromIntfImage(BImage);
 BImage.Free;
End;

Procedure TOzhBitmap.DrawInto ( ABmp : TBitmap; APosX, APosY : Integer );
Var
  BImage        : TLazIntfImage;
  BXSrc,
  BYSrc         : Integer;
  BXDst,
  BYDst         : Integer;
Begin
 BImage:=ABmp.CreateIntfImage;
 BYSrc:=0;
 while BYSrc<FHeight do
  begin
  BYDst:=APosY+BYSrc;
  if (BYDst>=0) and (BYDst<ABmp.Height) then
   begin
   BXSrc:=0;
   while BXSrc<FWidth do
    begin
    BXDst:=APosX+BXSrc;
    if (BXDst>=0) and (BXDst<ABmp.Width) then BImage.Colors[BXDst,BYDst]:=TColorToFPColor(FColors[BYSrc*FWidth+BXSrc]);
    inc(BXSrc);
    end;
   end;
  inc(BYSrc);
  end;
 ABmp.LoadFromIntfImage(BImage);
 BImage.Free;
End;

Procedure TOzhBitmap.Draw ( AX, AY : Integer; ASrc : TOzhBitmap );
Var
  BXSrc,
  BYSrc         : Integer;
  BXDst,
  BYDst         : Integer;
Begin
 for BYSrc:=0 to ASrc.Height-1 do
  begin
  BYDst:=AY+BYSrc;
  if (BYDst>=0) and (BYDst<FHeight) then
   begin
   for BXSrc:=0 to ASrc.Width-1 do
    begin
    BXDst:=AX+BXSrc;
    if BXDst>=FWidth then break;
    if BXDst>=0 then FColors[BYDst*FWidth+BXDst]:=ASrc.Colors[BYSrc*FWidth+BXSrc];
    end;
   end;
  end;
End;

end.

