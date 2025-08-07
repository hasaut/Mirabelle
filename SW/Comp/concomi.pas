unit ConComI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, {AsmTypes, }OzhBitmap, BGRABitmap, BGRABitmapTypes, BGRAFreeType;

Const
  CColorBg      = $D6DEE2; // $C6CED2; // $C0C8CC;

  CColorANorm = $D0D8D8;
  CColorAThis = $70C8D8;
  CColorBNorm = $FFFFFF;
  CColorBThis = $80E0F0;
  CColorHNorm = $C0C8C8;
  CColorHThis = $70B8C8;
  CColorPNorm = $C0C8C8;
  CColorPThis = $70B8C8;



Type
  TBtnKind = (bkUp, bkDn, bkLeft, bkRight);
  TOnScrollBarMoved = Procedure ( AStartPos : Integer ) of Object;
  TOnPaintA = Procedure of Object;

  TScrollBtn = class(TCustomControl)
  private
    FBmpNorm    : TBitmap;
    FBmpPres    : TBitmap;
    FPressed    : boolean;
    FOnClick    : TNotifyEvent;
    FBtnKind    : TBtnKind;

    Procedure SetBtnKind ( AKind : TBtnKind );
  protected
    Procedure Paint; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property BtnKind : TBtnKind read FBtnKind write SetBtnKind;
    property BmpNorm : TBitmap read FBmpNorm;
    property BmpPres : TBitmap read FBmpPres;
  end;

  TScrollBarAny = class(TCustomControl)
  private
  protected
    FConstructed        : boolean;
    FViewBitmap : TBitmap;
    FBmpE,
    FBmpO,
    FBmpU,
    FBmpM,
    FBmpD       : TBitmap;
    FPressed    : boolean;
    FOnMoved    : TOnScrollBarMoved;

    Procedure Paint; Override;
    Procedure Resize; Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Virtual; Abstract;

    property OnMoved : TOnScrollBarMoved read FOnMoved write FOnMoved;
  end;

  TScrollBarV = class(TScrollBarAny)
  private
    FLineTop,
    FVisible,
    FTotal      : Integer;
    FBarTop,
    FBarHeight  : Integer;
    FDownY,
    FDownBarTop : Integer;

    Procedure FileToBar;
    Procedure BarToFile;

  protected
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SetPos ( ATop, AVisible, ATotal : Integer );
    Procedure SetPos ( ATop : Integer );

    Procedure PaintA; Override;

    property LineTop : Integer read FLineTop;
    property LineVisible : Integer read FVisible;
  end;

  TScrollBarH = class(TScrollBarAny)
  private
    FPosLeft,
    FVisible,
    FTotal      : Integer;
    FBarLeft,
    FBarWidth   : Integer;
    FDownX,
    FDownBarLeft : Integer;

    Procedure FileToBar;
    Procedure BarToFile;

  protected
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SetPos ( ALeft, AVisible, ATotal : Integer );

    Procedure PaintA; Override;

    property PosLeft : Integer read FPosLeft;
    property PosVisible : Integer read FVisible;
  end;

  TOzhScrollAny = class(TCustomControl)
  private
  protected
    FConstructed        : boolean;
    FViewBitmap         : TOzhBitmap;
    FCopyBitmap         : TBitmap;
    FPressed            : boolean;

    FVisiStart,
    FVisiSize,
    FTotal      : Integer;
    FBarStart,
    FBarSize    : Integer;
    FDownX,
    FDownY,
    FDownBarStart       : Integer;
    FTrackSize          : Integer;
    FGutterWidth        : Integer;

    FColorBtnA,
    FColorBtnB,
    FColorBtnC,
    FColorBtnD,
    FColorBtnE          : Cardinal;
    FColorTrack         : array [0..1] of Cardinal;
    FColorGutterBG      : Cardinal;

    FOnMoved            : TOnScrollBarMoved;
    FOnPaintA           : TOnPaintA;

    Procedure FileToBar;
    Procedure BarToFile;

    Procedure BtnTriangle ( AX, AY : Integer; ADirection : char );

    Procedure Paint; Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Virtual; Abstract;
    Procedure GutterPaintA; Virtual; Abstract;
    Procedure ViewOpti;

    Procedure SetPos ( AVisiStart, AVisiSize, ATotal : Integer );
    Procedure SetPos ( AVisiStart : Integer );

    property VisiStart : Integer read FVisiStart;
    property VisiSize : Integer read FVisiSize;
    property ViewBitmap : TOzhBitmap read FViewBitmap;
    property OnMoved : TOnScrollBarMoved read FOnMoved write FOnMoved;
    property OnPaintA : TOnPaintA read FOnPaintA write FOnPaintA;
  end;

  TOzhScrollV = class(TOzhScrollAny)
  private
  protected
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Override;
    Procedure GutterPaintA; Override;
  end;

  TOzhScrollH = class(TOzhScrollAny)
  private
  protected
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Override;
    Procedure GutterPaintA; Override;
  end;

  TOzhProgrBar = class(TCustomControl)
  private
    FBitmap     : TBitmap;
    FProgress   : Double;
    FBarColor   : Cardinal;
    FBarCaption : string;

    Procedure PaintA;
    //Procedure HandleProgr ( Var AMessage : TLMessage ); message CProgrMsg;
  protected
    Procedure Paint; Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetProgr ( AProgr : Double; ABarColor : Cardinal; Const ABarCaption : string );
    Procedure SetProgr ( Const AParams : string );
  end;

  TMsStatusBar = class(TCustomControl)
  private
    FBitmap     : TBitmap;
    FCaretPos,
    FModified,
    FIsInsert,
    FFilename   : string;

    Procedure PaintA;
    //Procedure HandleProgr ( Var AMessage : TLMessage ); message CProgrMsg;
  protected
    Procedure Resize; Override;
    Procedure Paint; Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetParams ( Const ACaretPos, AModified, AIsInsert : string );
    Procedure SetFilename ( Const AFilename : string );
  end;



Function AvgColor ( AColorA, AColorB : Cardinal; APos : Single ) : Cardinal;
Function BgraColorA ( AColor : Cardinal ) : TBGRAPixel;
Function BgraColorExA ( AColor : Cardinal ) : TExpandedPixel;

Procedure BmpBox ( ABitmap : TBitmap; AColor : TColor );
Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB : TColor );
Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD : TColor );
Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorF : TColor );
Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorF : TColor );
Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );
Procedure BmpBox ( ABitmap : TBitmap; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );
Procedure BmpBox ( ABitmap : TBitmap; AColorA, AColorB, AColorF : TColor );
Procedure FrameRaisedV ( ACanvas : TCanvas; ATop, AHeight : Integer );
Procedure FrameRaisedH ( ACanvas : TCanvas; ALeft, AWidth : Integer );

Procedure LineF ( ABmp : TBitmap; AXA, AYA, AXB, AYB : Single; AColor : Cardinal );
Procedure CircF ( ABmp : TBitmap; ACenterX, ACenterY, AR : Single; AColor : Cardinal );

Procedure BmpBox ( ABitmap : TBGRABitmap; AColor : TColor );
Procedure BmpBox ( ABitmap : TBGRABitmap; AColorA, AColorB, AColorF : TColor );

implementation

Uses
  ConComL;

Function AvgColor ( AColorA, AColorB : Cardinal; APos : Single ) : Cardinal;
Var
  BColorAI      : array [0..3] of byte absolute AColorA;
  BColorBI      : array [0..3] of byte absolute AColorB;
  BColor        : Cardinal;
  BColorI       : array [0..3] of byte absolute BColor;
  BPos          : Single;
Begin
 BColor:=0;
 if APos<0 then APos:=0;
 if APos>1 then APos:=1;
 BPos:=1-APos;
 BColorI[0]:=Round((BColorAI[0]*BPos)+(BColorBI[0]*APos));
 BColorI[1]:=Round((BColorAI[1]*BPos)+(BColorBI[1]*APos));
 BColorI[2]:=Round((BColorAI[2]*BPos)+(BColorBI[2]*APos));
 Result:=BColor;
End;

Procedure BmpBox ( ABitmap : TBitmap; AColor : TColor );
Begin
 ABitmap.Canvas.Brush.Style:=bsSolid;
 ABitmap.Canvas.Brush.Color:=AColor;
 ABitmap.Canvas.FillRect(0,0,ABitmap.Width,ABitmap.Height);
End;

Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB : TColor );
Begin
 ACanvas.Pen.Style:=psSolid;
 ACanvas.Pen.Color:=AColorA;
 ACanvas.Line(ALeft,ATop,ALeft+AWidth,ATop);
 ACanvas.Line(ALeft,ATop,ALeft,ATop+AHeight);
 ACanvas.Pen.Color:=AColorB;
 ACanvas.Line(ALeft+AWidth-1,ATop,ALeft+AWidth-1,ATop+AHeight);
 ACanvas.Line(ALeft,ATop+AHeight-1,ALeft+AWidth,ATop+AHeight-1);
End;

Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD : TColor );
Begin
 BmpBox(ACanvas,ALeft,ATop,AWidth,AHeight,AColorA,AColorB);
 BmpBox(ACanvas,ALeft+1,ATop+1,AWidth-2,AHeight-2,AColorC,AColorD);
End;

Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorF : TColor );
Begin
 ACanvas.Brush.Style:=bsSolid;
 ACanvas.Brush.Color:=AColorF;
 ACanvas.FillRect(ALeft,ATop,ALeft+AWidth,ATop+AHeight);
End;

Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorF : TColor );
Begin
 BmpBox(ACanvas,ALeft,ATop,AWidth,AHeight,AColorA,AColorB);
 ACanvas.Brush.Style:=bsSolid;
 ACanvas.Brush.Color:=AColorF;
 ACanvas.FillRect(ALeft+1,ATop+1,ALeft+AWidth-1,ATop+AHeight-1);
End;

Procedure BmpBox ( ACanvas : TCanvas; ALeft, ATop, AWidth, AHeight : Integer; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );
Begin
 BmpBox(ACanvas,ALeft,ATop,AWidth,AHeight,AColorA,AColorB,AColorC,AColorD);
 ACanvas.Brush.Style:=bsSolid;
 ACanvas.Brush.Color:=AColorF;
 ACanvas.FillRect(ALeft+2,ATop+2,ALeft+AWidth-2,ATop+AHeight-2);
End;

Procedure BmpBox ( ABitmap : TBitmap; AColorA, AColorB, AColorC, AColorD, AColorF : TColor );
Begin
 BmpBox(ABitmap.Canvas,0,0,ABitmap.Width-1,ABitmap.Height-1,AColorA,AColorB,AColorC,AColorD,AColorF);
End;

Procedure BmpBox ( ABitmap : TBitmap; AColorA, AColorB, AColorF : TColor );
Begin
 BmpBox(ABitmap.Canvas,0,0,ABitmap.Width,ABitmap.Height,AColorA,AColorB,AColorF);
End;

Function BgraColorA ( AColor : Cardinal ) : TBGRAPixel;
Begin
 Result.red:=(AColor and $0000FF) shr 0;
 Result.green:=(AColor and $00FF00) shr 8;
 Result.blue:=(AColor and $FF0000) shr 16;
 Result.alpha:=$FF;
End;

Function BgraColorExA ( AColor : Cardinal ) : TExpandedPixel;
Begin
 Result.red:=(AColor and $0000FF) shl 8;
 Result.green:=(AColor and $00FF00) shl 0;
 Result.blue:=(AColor and $FF0000) shr 8;
 Result.alpha:=$FFFF;
End;

Procedure BmpBox ( ABitmap : TBGRABitmap; AColor : TColor );
Begin
 ABitmap.FillRect(0,0,ABitmap.Width,ABitmap.Height,BgraColorA(AColor),dmSet);
End;

Procedure BmpBox ( ABitmap : TBGRABitmap; AColorA, AColorB, AColorF : TColor );
Begin
 BmpBox(ABitmap,AColorF);
 ABitmap.DrawHorizLine(0,0,ABitmap.Width,BgraColorExA(AColorA));
 ABitmap.DrawVertLine(0,0,ABitmap.Height,BgraColorExA(AColorA));
 ABitmap.DrawHorizLine(0,ABitmap.Height-1,ABitmap.Width,BgraColorExA(AColorB));
 ABitmap.DrawVertLine(ABitmap.Width-1,0,ABitmap.Height,BgraColorExA(AColorB));
End;

Procedure FrameRaisedV ( ACanvas : TCanvas; ATop, AHeight : Integer );
Begin
 ACanvas.Pen.Style:=psSolid;
 ACanvas.Pen.Color:=$C0C0C0;
 ACanvas.Line(0,ATop,0,ATop+AHeight);
 ACanvas.Line(0,ATop,14,ATop);
 ACanvas.Pen.Color:=$404040;
 ACanvas.Line(14,ATop,14,ATop+AHeight);
 ACanvas.Line(0,ATop+AHeight-1,14,ATop+AHeight-1);
 ACanvas.Pen.Color:=$FFFFFF;
 ACanvas.Line(1,ATop+1,1,ATop+AHeight-1);
 ACanvas.Line(1,ATop+1,14,ATop+1);
 ACanvas.Pen.Color:=$808080;
 ACanvas.Line(13,ATop,13,ATop+AHeight-1);
 ACanvas.Line(1,ATop+AHeight-2,14,ATop+AHeight-2);
 ACanvas.Brush.Style:=bsSolid;
 ACanvas.Brush.Color:=clBtnFace;
 ACanvas.FillRect(2,ATop+2,13,ATop+AHeight-2);
End;

Procedure FrameRaisedH ( ACanvas : TCanvas; ALeft, AWidth : Integer );
Begin
 BmpBox(ACanvas,ALeft,0,AWidth,15,$C0C0C0,$404040,$FFFFFF,$808080,clBtnFace);
End;

Procedure LineFH ( ABmp : TBitmap; AXA, AYA, AXB, AYB : Single; AColor : Cardinal );
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
  if (BIdxL>0) and (BIdxL<ABmp.Height) and
     (BIdxH>0) and (BIdxH<ABmp.Height) and
     (BIndex>0) and (BIndex<ABmp.Width) then
   begin
   BColorH:=ABmp.Canvas.Pixels[BIndex,BIdxH];
   BColorL:=ABmp.Canvas.Pixels[BIndex,BIdxL];
   BColorHI[0]:=Round(BColorHI[0]*BMulL + BColorI[0]*BMulH);
   BColorHI[1]:=Round(BColorHI[1]*BMulL + BColorI[1]*BMulH);
   BColorHI[2]:=Round(BColorHI[2]*BMulL + BColorI[2]*BMulH);
   BColorLI[0]:=Round(BColorLI[0]*BMulH + BColorI[0]*BMulL);
   BColorLI[1]:=Round(BColorLI[1]*BMulH + BColorI[1]*BMulL);
   BColorLI[2]:=Round(BColorLI[2]*BMulH + BColorI[2]*BMulL);
   ABmp.Canvas.Pixels[BIndex,BIdxH]:=BColorH;
   ABmp.Canvas.Pixels[BIndex,BIdxL]:=BColorL;
   end;
  BIndex:=BIndex+BInc;
  end;
 until TRUE;
End;

Procedure LineFV ( ABmp : TBitmap; AXA, AYA, AXB, AYB : Single; AColor : Cardinal );
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
  if (BIdxL>0) and (BIdxL<ABmp.Width) and
     (BIdxR>0) and (BIdxR<ABmp.Width) and
     (BIndex>0) and (BIndex<ABmp.Height) then
   begin
   BColorR:=ABmp.Canvas.Pixels[BIdxR,BIndex];
   BColorL:=ABmp.Canvas.Pixels[BIdxL,BIndex];
   BColorRI[0]:=Round(BColorRI[0]*BMulL + BColorI[0]*BMulR);
   BColorRI[1]:=Round(BColorRI[1]*BMulL + BColorI[1]*BMulR);
   BColorRI[2]:=Round(BColorRI[2]*BMulL + BColorI[2]*BMulR);
   BColorLI[0]:=Round(BColorLI[0]*BMulR + BColorI[0]*BMulL);
   BColorLI[1]:=Round(BColorLI[1]*BMulR + BColorI[1]*BMulL);
   BColorLI[2]:=Round(BColorLI[2]*BMulR + BColorI[2]*BMulL);
   ABmp.Canvas.Pixels[BIdxR,BIndex]:=BColorR;
   ABmp.Canvas.Pixels[BIdxL,BIndex]:=BColorL;
   end;
  BIndex:=BIndex+BInc;
  end;
 until TRUE;
End;

Procedure LineF ( ABmp : TBitmap; AXA, AYA, AXB, AYB : Single; AColor : Cardinal );
Begin
 if Abs(AXB-AXA)>Abs(AYB-AYA) then LineFH(ABmp,AXA,AYA,AXB,AYB,AColor)
 else LineFV(ABmp,AXA,AYA,AXB,AYB,AColor);
End;

Procedure CircF ( ABmp : TBitmap; ACenterX, ACenterY, AR : Single; AColor : Cardinal );
Var
  BPosL, BPosT,
  BPosR, BPosB  : Integer;
  BIndexX,
  BIndexY       : Integer;
  BColor        : Cardinal;
  BDist         : Single;
Begin
 BPosL:=Round(ACenterX-AR-1); BPosT:=Round(ACenterY-AR-1);
 BPosR:=Round(ACenterX+AR+2); BPosB:=Round(ACenterY+AR+2);
 BIndexY:=BPosT;
 while BIndexY<BPosB do
  begin
  BIndexX:=BPosL;
  while BIndexX<BPosR do
   begin
   if (BIndexY>=0) and (BIndexY<ABmp.Height) and (BIndexX>=0) and (BIndexX<ABmp.Width) then
    begin
    BDist:=Sqrt(Sqr(BIndexX-ACenterX)+Sqr(BIndexY-ACenterY));
    if BDist<AR then
     begin
     BDist:=BDist+1-AR;
     if BDist<0 then BColor:=AColor
     else BColor:=AvgColor(ABmp.Canvas.Pixels[BIndexX,BIndexY],AColor,BDist);
     ABmp.Canvas.Pixels[BIndexX,BIndexY]:=BColor;
     end;
    end;
   inc(BIndexX);
   end;
  inc(BIndexY);
  end;
End;

{ TScrollBtn }

Constructor TScrollBtn.Create ( AOwner : TComponent );
Begin
 Inherited;
 Width:=15; Height:=15;
 FBmpNorm:=TBitmap.Create;
 FBmpPres:=TBitmap.Create;
End;

Destructor TScrollBtn.Destroy;
Begin
 FBmpPres.Free;
 FBmpNorm.Free;
 Inherited;
End;

Procedure TScrollBtn.SetBtnKind ( AKind : TBtnKind );
Begin
 FBtnKind:=AKind;
 try
 case FBtnKind of
   bkUp:
     begin
     FBmpNorm.LoadFromLazarusResource('btn_u_n');
     FBmpPres.LoadFromLazarusResource('btn_u_p');
     end;
   bkDn:
     begin
     FBmpNorm.LoadFromLazarusResource('btn_d_n');
     FBmpPres.LoadFromLazarusResource('btn_d_p');
     end;
   bkLeft:
     begin
     FBmpNorm.LoadFromLazarusResource('btn_l_n');
     FBmpPres.LoadFromLazarusResource('btn_l_p');
     end;
   bkRight:
     begin
     FBmpNorm.LoadFromLazarusResource('btn_r_n');
     FBmpPres.LoadFromLazarusResource('btn_r_p');
     end;
   end; // case
 except
 end;
End;

Procedure TScrollBtn.Paint;
Begin
 if FPressed=FALSE then Canvas.Draw(0,0,FBmpNorm)
 else Canvas.Draw(0,0,FBmpPres);
End;

Procedure TScrollBtn.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if AButton=mbLeft then
  begin
  FPressed:=TRUE;
  Invalidate;
  if Assigned(FOnClick) then FOnClick(Self);
  end;
End;

Procedure TScrollBtn.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if AButton=mbLeft then
  begin
  FPressed:=FALSE;
  Invalidate;
  end;
End;

{ TScrollBarAny }

Constructor TScrollBarAny.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 FViewBitmap:=TBitmap.Create;
 FBmpE:=TBitmap.Create;
 FBmpO:=TBitmap.Create;
 FBmpU:=TBitmap.Create;
 FBmpM:=TBitmap.Create;
 FBmpD:=TBitmap.Create;
End;

Destructor TScrollBarAny.Destroy;
Begin
 FBmpD.Free;
 FBmpM.Free;
 FBmpU.Free;
 FBmpO.Free;
 FBmpE.Free;
 FViewBitmap.Free;
 Inherited;
End;

Procedure TScrollBarAny.Resize;
Begin
 Inherited;
 if FConstructed then
  begin
  FViewBitmap.SetSize(Width,Height);
  PaintA;
  end;
End;

Procedure TScrollBarAny.Paint;
Begin
 Inherited;
 PaintA;
End;

{ TScrollBarV }

Constructor TScrollBarV.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 Width:=15; Height:=20;
 FViewBitmap.SetSize(15,20);

 try
  FBmpE.LoadFromLazarusResource('barv_bg_even');
  FBmpO.LoadFromLazarusResource('barv_bg_odd');
  FBmpU.LoadFromLazarusResource('barv_btn_u');
  FBmpM.LoadFromLazarusResource('barv_btn_m');
  FBmpD.LoadFromLazarusResource('barv_btn_d');
 except
 end;
 FConstructed:=TRUE;
End;

Destructor TScrollBarV.Destroy;
Begin
 Inherited;
End;

Procedure TScrollBarV.FileToBar;
Begin
 if (FTotal=0) or (FVisible>=FTotal) then
  begin
  FBarTop:=0;
  FBarHeight:=FViewBitmap.Height;
  end
 else
  begin
  FBarHeight:=(FVisible * FViewBitmap.Height) div FTotal;
  FBarTop:=(FLineTop * FViewBitmap.Height) div FTotal;
  if FBarHeight<5 then FBarHeight:=5;
  end;
End;

Procedure TScrollBarV.BarToFile;
Begin
 FLineTop:=(FBarTop * FTotal) div FViewBitmap.Height;
End;

Procedure TScrollBarV.SetPos ( ATop, AVisible, ATotal : Integer );
Begin
 FLineTop:=ATop;
 FVisible:=AVisible;
 FTotal:=ATotal;
 FileToBar;
 Refresh;
End;

Procedure TScrollBarV.SetPos ( ATop : Integer );
Begin
 FLineTop:=ATop;
 FileToBar;
 Refresh;
End;

Procedure TScrollBarV.PaintA;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<FBarTop do
  begin
  if (BIndex and $1)=0 then FViewBitmap.Canvas.Draw(0,BIndex,FBmpE)
  else FViewBitmap.Canvas.Draw(0,BIndex,FBmpO);
  inc(BIndex);
  end;

 {FViewBitmap.Canvas.Draw(0,FBarTop,FBmpU);

 BIndex:=FBarTop+2;
 while BIndex<(FBarTop+FBarHeight-2) do
  begin
  FViewBitmap.Canvas.Draw(0,BIndex,FBmpM);
  inc(BIndex);
  end;

 FViewBitmap.Canvas.Draw(0,FBarTop+FBarHeight-2,FBmpD);}

 BmpBox(FViewBitmap.Canvas,0,FBarTop,15,FBarHeight,$808080,$404040,$FFFFFF,$808080,$DADADA);

 BIndex:=FBarTop+FBarHeight;
 while BIndex<Height do
  begin
  if (BIndex and $1)=0 then FViewBitmap.Canvas.Draw(0,BIndex,FBmpE)
  else FViewBitmap.Canvas.Draw(0,BIndex,FBmpO);
  inc(BIndex);
  end;

 Canvas.Draw(0,0,FViewBitmap);
End;

Procedure TScrollBarV.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if (AButton=mbLeft) and (AY>FBarTop) and (AY<(FBarTop+FBarHeight)) then
  begin
  FDownY:=AY;
  FDownBarTop:=FBarTop;
  FPressed:=TRUE;
  end;
End;

Procedure TScrollBarV.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if AButton=mbLeft then FPressed:=FALSE;
End;

Procedure TScrollBarV.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Var
  BBarTop       : Integer;
Begin
 if FPressed then
  begin
  BBarTop:=FDownBarTop+(AY-FDownY);
  if BBarTop<0 then BBarTop:=0
  else if (BBarTop+FBarHeight)>=Height then BBarTop:=Height-FBarHeight;
  if FBarTop<>BBarTop then
   begin
   FBarTop:=BBarTop;
   BarToFile;
   Refresh;
   if Assigned(FOnMoved) then FOnMoved(FLineTop);
   end;
  end;
End;

{ TScrollBarH }

Constructor TScrollBarH.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 Width:=20; Height:=15;
 FViewBitmap.SetSize(20,15);

 try
  FBmpE.LoadFromLazarusResource('barh_bg_even');
  FBmpO.LoadFromLazarusResource('barh_bg_odd');
  FBmpU.LoadFromLazarusResource('barh_btn_l');
  FBmpM.LoadFromLazarusResource('barh_btn_m');
  FBmpD.LoadFromLazarusResource('barh_btn_r');
 except
 end;
 FConstructed:=TRUE;
End;

Destructor TScrollBarH.Destroy;
Begin
 Inherited;
End;

Procedure TScrollBarH.FileToBar;
Begin
 if (FTotal=0) or (FVisible>=FTotal) then
  begin
  FBarLeft:=0;
  FBarWidth:=FViewBitmap.Width;
  end
 else
  begin
  FBarWidth:=(FVisible * FViewBitmap.Width) div FTotal;
  FBarLeft:=(FPosLeft * FViewBitmap.Width) div FTotal;
  end;
End;

Procedure TScrollBarH.BarToFile;
Begin
 FPosLeft:=(FBarLeft * FTotal) div FViewBitmap.Width;
End;

Procedure TScrollBarH.SetPos ( ALeft, AVisible, ATotal : Integer );
Begin
 FPosLeft:=ALeft;
 FVisible:=AVisible;
 FTotal:=ATotal;
 FileToBar;
 Refresh;
End;

Procedure TScrollBarH.PaintA;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<FBarLeft do
  begin
  if (BIndex and $1)=0 then FViewBitmap.Canvas.Draw(BIndex,0,FBmpE)
  else FViewBitmap.Canvas.Draw(BIndex,0,FBmpO);
  inc(BIndex);
  end;

 BmpBox(FViewBitmap.Canvas,FBarLeft,0,FBarWidth,15,$808080,$404040,$FFFFFF,$808080,$DADADA);

 BIndex:=FBarLeft+FBarWidth;
 while BIndex<Width do
  begin
  if (BIndex and $1)=0 then FViewBitmap.Canvas.Draw(BIndex,0,FBmpE)
  else FViewBitmap.Canvas.Draw(BIndex,0,FBmpO);
  inc(BIndex);
  end;

 Canvas.Draw(0,0,FViewBitmap);
End;

Procedure TScrollBarH.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if (AButton=mbLeft) and (AX>FBarLeft) and (AX<(FBarLeft+FBarWidth)) then
  begin
  FDownX:=AX;
  FDownBarLeft:=FBarLeft;
  FPressed:=TRUE;
  end;
End;

Procedure TScrollBarH.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if AButton=mbLeft then FPressed:=FALSE;
End;

Procedure TScrollBarH.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Var
  BBarLeft      : Integer;
Begin
 if FPressed then
  begin
  BBarLeft:=FDownBarLeft+(AX-FDownX);
  if BBarLeft<0 then BBarLeft:=0
  else if (BBarLeft+FBarWidth)>=Width then BBarLeft:=Width-FBarWidth;
  if FBarLeft<>BBarLeft then
   begin
   FBarLeft:=BBarLeft;
   BarToFile;
   Refresh;
   if Assigned(FOnMoved) then FOnMoved(FPosLeft);
   end;
  end;
End;

{ TScrollBarAny }

Constructor TOzhScrollAny.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 FGutterWidth:=13;
 FViewBitmap:=TOzhBitmap.Create;
 FCopyBitmap:=TBitmap.Create;
 FColorBtnA:=$808080;
 FColorBtnB:=$404040;
 FColorBtnC:=$FFFFFF;
 FColorBtnD:=$808080;
 FColorBtnE:=$DADADA;
 FColorTrack[0]:=$FFFFFF; FColorTrack[1]:=$C0C0C0;
 FColorGutterBG:=$D0D0D0;
End;

Destructor TOzhScrollAny.Destroy;
Begin
 FCopyBitmap.Free;
 FViewBitmap.Free;
 Inherited;
End;

Procedure TOzhScrollAny.Paint;
Begin
 Inherited;
 PaintA;
End;

Procedure TOzhScrollAny.FileToBar;
Begin
 if (FTotal=0) or (FVisiSize>=FTotal) then
  begin
  FBarStart:=0;
  FBarSize:=FTrackSize;
  end
 else
  begin
  FBarSize:=(FVisiSize * FTrackSize) div FTotal;
  FBarStart:=(FVisiStart * FTrackSize) div FTotal;
  end;
End;

Procedure TOzhScrollAny.BarToFile;
Begin
 FVisiStart:=(FBarStart * FTotal) div FTrackSize;
End;

Procedure TOzhScrollAny.BtnTriangle ( AX, AY : Integer; ADirection : char );
Var
  BIndex        : Integer;
Begin
 case ADirection of
   'L': begin
        for BIndex:=0 to 4 do FViewBitmap.Line(AX-2+BIndex,AY-BIndex,AX-2+BIndex,AY+1+BIndex,$404040);
        end;
   'R': begin
        for BIndex:=0 to 4 do FViewBitmap.Line(AX-2+BIndex,AY-4+BIndex,AX-2+BIndex,AY+5-BIndex,$404040);
        end;
   'U': begin
        for BIndex:=0 to 4 do FViewBitmap.Line(AX-BIndex,AY-2+BIndex,AX+1+BIndex,AY-2+BIndex,$404040);
        end;
   'D': begin
        for BIndex:=0 to 4 do FViewBitmap.Line(AX-4+BIndex,AY-2+BIndex,AX+5-BIndex,AY-2+BIndex,$404040);
        end;
 end;
End;

Procedure TOzhScrollAny.SetPos ( AVisiStart, AVisiSize, ATotal : Integer );
Begin
 FVisiStart:=AVisiStart;
 FVisiSize:=AVisiSize;
 FTotal:=ATotal;
 FileToBar;
 Refresh;
End;

Procedure TOzhScrollAny.SetPos ( AVisiStart : Integer );
Begin
 FVisiStart:=AVisiStart;
 FileToBar;
 Refresh;
End;

Procedure TOzhScrollAny.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if AButton=mbLeft then FPressed:=FALSE;
End;

Procedure TOzhScrollAny.ViewOpti;
Begin
 // Copy
 FViewBitmap.ExportBmp(FCopyBitmap);
 // View
 Canvas.Draw(0,0,FCopyBitmap);
End;

{ TOzhScrollV }

Constructor TOzhScrollV.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 Width:=15+FGutterWidth; Height:=100;
 FViewBitmap.SetSize(Width,Height);
 FConstructed:=TRUE;
End;

Destructor TOzhScrollV.Destroy;
Begin
 Inherited;
End;

Procedure TOzhScrollV.Resize;
Begin
 Inherited;
 if FConstructed then
  begin
  FGutterWidth:=Width-15;
  FTrackSize:=Height-2*15;
  FViewBitmap.SetSize(Width,Height);
  FCopyBitmap.SetSize(Width,Height);
  PaintA;
  end;
End;

Procedure TOzhScrollV.GutterPaintA;
Var
  BWndTop,
  BWndLen       : Integer;
Begin
 // GutterBG
 FViewBitmap.BmpBox(1,0,FGutterWidth-1,FViewBitmap.Height,FColorGutterBG);
 FViewBitmap.Line(0,0,0,FViewBitmap.Height,$FFFFFF);
 FViewBitmap.Line(4,0,4,FViewBitmap.Height,$FFFFFF);
 FViewBitmap.Line(5,0,5,FViewBitmap.Height,$808080);
 if FTrackSize=0 then begin BWndTop:=0; BWndLen:=FViewBitmap.Height; end
 else
  begin
  BWndTop:=FBarStart*FViewBitmap.Height div FTrackSize;
  BWndLen:=FBarSize*FViewBitmap.Height div FTrackSize;
  end;
 if BWndLen<1 then BWndLen:=1;
 FViewBitmap.BmpBox(6,BWndTop,FGutterWidth-6,BWndLen,$E0E0E0);

 // Ext PaintA
 if Assigned(FOnPaintA) then FOnPaintA;
End;

Procedure TOzhScrollV.PaintA;
Var
  BColIdx,
  BRowIdx       : Integer;
  BBarSize      : Integer;
Begin
 // Buttons
 FViewBitmap.BmpBox(FViewBitmap.Width-15,0,15,15,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 FViewBitmap.BmpBox(FViewBitmap.Width-15,FViewBitmap.Height-15,15,15,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 BtnTriangle(FViewBitmap.Width-8,7,'U');
 BtnTriangle(FViewBitmap.Width-8,FViewBitmap.Height-8,'D');
 // Bar
 BBarSize:=FBarSize;
 if BBarSize<1 then BBarSize:=1;
 if (FBarStart+BBarSize)>FTrackSize then BBarSize:=FTrackSize-FBarStart;
 if BBarSize>0 then FViewBitmap.BmpBox(FViewBitmap.Width-15,15+FBarStart,15,BBarSize,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 // TrackH
 BRowIdx:=0;
 while BRowIdx<FBarStart do
  begin
  for BColIdx:=0 to 14 do FViewBitmap.Colors[(15+BRowIdx)*FViewBitmap.Width+FGutterWidth+BColIdx]:=FColorTrack[(BColIdx+BRowIdx) and $1];
  inc(BRowIdx);
  end;
 // TrackL
 BRowIdx:=FBarStart+BBarSize;
 while BRowIdx<FTrackSize do
  begin
  for BColIdx:=0 to 14 do FViewBitmap.Colors[(15+BRowIdx)*FViewBitmap.Width+FGutterWidth+BColIdx]:=FColorTrack[(BColIdx+BRowIdx) and $1];
  inc(BRowIdx);
  end;

 // Gutter
 GutterPaintA;
 // Copy
 FViewBitmap.ExportBmp(FCopyBitmap);

 Canvas.Draw(0,0,FCopyBitmap);
End;

Procedure TOzhScrollV.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if (AButton=mbLeft) and (AY>FBarStart) and (AY<(FBarStart+FBarSize)) then
  begin
  FDownY:=AY;
  FDownBarStart:=FBarStart;
  FPressed:=TRUE;
  end;
End;

Procedure TOzhScrollV.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Var
  BBarTop       : Integer;
Begin
 if FPressed then
  begin
  BBarTop:=FDownBarStart+(AY-FDownY);
  if BBarTop<0 then BBarTop:=0
  else if (BBarTop+FBarSize)>=FTrackSize then BBarTop:=FTrackSize-FBarSize;
  if FBarStart<>BBarTop then
   begin
   FBarStart:=BBarTop;
   BarToFile;
   Refresh;
   if Assigned(FOnMoved) then FOnMoved(FVisiStart);
   end;
  end;
End;

{ TOzhScrollH }

Constructor TOzhScrollH.Create ( AOwner : TComponent );
Begin
 Inherited;
 FConstructed:=FALSE;
 Width:=100; Height:=15+FGutterWidth;
 FViewBitmap.SetSize(Width,Height);
 FConstructed:=TRUE;
End;

Destructor TOzhScrollH.Destroy;
Begin
 Inherited;
End;

Procedure TOzhScrollH.Resize;
Begin
 Inherited;
 if FConstructed then
  begin
  FGutterWidth:=Height-15;
  FTrackSize:=Width-2*15;
  FViewBitmap.SetSize(Width,Height);
  FCopyBitmap.SetSize(Width,Height);
  PaintA;
  end;
End;

Procedure TOzhScrollH.GutterPaintA;
Var
  BWndLeft,
  BWndLen       : Integer;
Begin
 // GutterBG
 FViewBitmap.BmpBox(0,1,FViewBitmap.Width,FGutterWidth-1,FColorGutterBG);
 FViewBitmap.Line(0,0,FViewBitmap.Width,0,$FFFFFF);
 FViewBitmap.Line(0,4,FViewBitmap.Width,4,$FFFFFF);
 FViewBitmap.Line(0,5,FViewBitmap.Width,5,$808080);
 if FTrackSize=0 then begin BWndLeft:=0; BWndLen:=FViewBitmap.Width; end
 else
  begin
  BWndLeft:=FBarStart*FViewBitmap.Width div FTrackSize;
  BWndLen:=FBarSize*FViewBitmap.Width div FTrackSize;
  end;
 if BWndLen<1 then BWndLen:=1;
 FViewBitmap.BmpBox(BWndLeft,6,BWndLen,FGutterWidth-6,$E0E0E0);

 // Ext PaintA
 if Assigned(FOnPaintA) then FOnPaintA;
End;

Procedure TOzhScrollH.PaintA;
Var
  BColIdx,
  BRowIdx       : Integer;
  BBarSize      : Integer;
Begin
 // Buttons
 FViewBitmap.BmpBox(0,FViewBitmap.Height-15,15,15,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 FViewBitmap.BmpBox(FViewBitmap.Width-15,FViewBitmap.Height-15,15,15,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 BtnTriangle(7,FViewBitmap.Height-8,'L');
 BtnTriangle(FViewBitmap.Width-8,FViewBitmap.Height-8,'R');
 // Bar
 BBarSize:=FBarSize;
 if BBarSize<1 then BBarSize:=1;
 if (FBarStart+BBarSize)>FTrackSize then BBarSize:=FTrackSize-FBarStart;
 if BBarSize>0 then FViewBitmap.BmpBox(15+FBarStart,FViewBitmap.Height-15,BBarSize,15,FColorBtnA,FColorBtnB,FColorBtnC,FColorBtnD,FColorBtnE);
 // TrackL
 BColIdx:=0;
 while BColIdx<FBarStart do
  begin
  for BRowIdx:=0 to 14 do FViewBitmap.Colors[(FGutterWidth+BRowIdx)*FViewBitmap.Width+15+BColIdx]:=FColorTrack[(BColIdx+BRowIdx) and $1];
  inc(BColIdx);
  end;
 // TrackR
 BColIdx:=FBarStart+BBarSize;
 while BColIdx<FTrackSize do
  begin
  for BRowIdx:=0 to 14 do FViewBitmap.Colors[(FGutterWidth+BRowIdx)*FViewBitmap.Width+15+BColIdx]:=FColorTrack[(BColIdx+BRowIdx) and $1];
  inc(BColIdx);
  end;

 // Gutter
 GutterPaintA;
 // Copy
 FViewBitmap.ExportBmp(FCopyBitmap);
 // View
 Canvas.Draw(0,0,FCopyBitmap);
End;

Procedure TOzhScrollH.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 if (AButton=mbLeft) and (AX>(15+FBarStart)) and (AX<(15+FBarStart+FBarSize)) then
  begin
  FDownX:=AX;
  FDownBarStart:=FBarStart;
  FPressed:=TRUE;
  end;
End;

Procedure TOzhScrollH.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Var
  BBarLeft      : Integer;
Begin
 if FPressed then
  begin
  BBarLeft:=FDownBarStart+(AX-FDownX);
  if BBarLeft<0 then BBarLeft:=0
  else if (BBarLeft+FBarSize)>=FTrackSize then BBarLeft:=FTrackSize-FBarSize;
  if FBarStart<>BBarLeft then
   begin
   FBarStart:=BBarLeft;
   BarToFile;
   Refresh;
   if Assigned(FOnMoved) then FOnMoved(FVisiStart);
   end;
  end;
End;

{ TOzhProgrBar }

Constructor TOzhProgrBar.Create ( AOwner : TComponent );
Begin
 Inherited;
 FBitmap:=TBitmap.Create;
 Height:=21;
 Width:=100;
End;

Destructor TOzhProgrBar.Destroy;
Begin
 FBitmap.Free;
 Inherited;
End;

Procedure TOzhProgrBar.Paint;
Begin
 Inherited;
 PaintA;
End;

Procedure TOzhProgrBar.PaintA;
Var
  BBarWidth     : Integer;
  BTextWidth,
  BTextHeight   : Integer;
Begin
 FBitmap.SetSize(Width,Height);
 FBitmap.Canvas.Font.Name:='Arial';
 FBitmap.Canvas.Font.Size:=8;
 FBitmap.Canvas.Font.Color:=$404040;
 FBitmap.Canvas.Brush.Style:=bsSolid;
 FBitmap.Canvas.Brush.Color:=$C0D0D0;
 FBitmap.Canvas.FillRect(0,0,FBitmap.Width,FBitmap.Height);
 BmpBox(FBitmap,$808080,$FFFFFF,$C0D0D0);
 FBitmap.Canvas.Brush.Color:=FBarColor;
 BBarWidth:=Round((FBitmap.Width-2)*FProgress);
 if BBarWidth<>0 then FBitmap.Canvas.FillRect(1,1,1+BBarWidth,FBitmap.Height-2);

 if FBarCaption<>'' then
  begin
  BTextWidth:=FBitmap.Canvas.TextWidth(FBarCaption);
  BTextHeight:=FBitmap.Canvas.TextHeight('Ayla');
  FBitmap.Canvas.Brush.Style:=bsClear;
  FBitmap.Canvas.Font.Color:=$FFFFFF;
  FBitmap.Canvas.TextOut(((FBitmap.Width-BTextWidth) div 2)+1,((FBitmap.Height-BTextHeight) div 2)+1,FBarCaption);
  FBitmap.Canvas.Font.Color:=$000000;
  FBitmap.Canvas.TextOut((FBitmap.Width-BTextWidth) div 2,(FBitmap.Height-BTextHeight) div 2,FBarCaption);
  end;

 Canvas.Draw(0,0,FBitmap);
End;

Procedure TOzhProgrBar.SetProgr ( AProgr : Double; ABarColor : Cardinal; Const ABarCaption : string );
Begin
 FProgress:=AProgr; FBarColor:=ABarColor; FBarCaption:=ABarCaption;
 PaintA;
End;

Procedure TOzhProgrBar.SetProgr ( Const AParams : string );
Var
  BDataS    : string;
  BProgr    : Double;
  BBarColor : Cardinal;
  BBarCapt  : string;
Begin
 BDataS:=AParams;
 TryStrToFloat(ReadParamStr(BDataS),BProgr);
 HexToDWordCheck(ReadParamStr(BDataS),BBarColor);
 BBarCapt:=BDataS; DelFirstSpace(BBarCapt);
 SetProgr(BProgr,BBarColor,BBarCapt);
End;

Constructor TMsStatusBar.Create ( AOwner : TComponent );
Begin
 Inherited;
 FBitmap:=TBitmap.Create;
End;

Destructor TMsStatusBar.Destroy;
Begin
 FBitmap.Free;
 Inherited;
End;

Procedure TMsStatusBar.Resize;
Var
  BTextHeight    : Integer;
Begin
 Inherited;
 FBitmap.Canvas.Font.Assign(Font);
 BTextHeight:=FBitmap.Canvas.TextHeight('Ayla');
 Height:=BTextHeight+4;
 PaintA;
End;

Procedure TMsStatusBar.PaintA;
Var
  BTextHeight   : Integer;
  BHeight       : Integer;
Begin
 FBitmap.Canvas.Font.Assign(Font);
 BTextHeight:=FBitmap.Canvas.TextHeight('Ayla');
 BHeight:=BTextHeight+4;
 FBitmap.SetSize(Width,BHeight);
 BmpBox(FBitmap,clForm);
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.TextOut(4,2,FCaretPos); BmpBox(FBitmap.Canvas,0,0,100,BHeight,$808080,$FFFFFF);
 FBitmap.Canvas.TextOut(104,2,FModified); BmpBox(FBitmap.Canvas,100,0,100,BHeight,$808080,$FFFFFF);
 FBitmap.Canvas.TextOut(204,2,FIsInsert); BmpBox(FBitmap.Canvas,200,0,50,BHeight,$808080,$FFFFFF);
 FBitmap.Canvas.TextOut(254,2,FFilename); BmpBox(FBitmap.Canvas,250,0,Width-250,BHeight,$808080,$FFFFFF);
 Canvas.Draw(0,0,FBitmap);
End;

Procedure TMsStatusBar.Paint;
Begin
 PaintA;
End;

Procedure TMsStatusBar.SetFilename ( Const AFilename : string );
Begin
 FFilename:=AFilename;
 PaintA;
End;

Procedure TMsStatusBar.SetParams ( Const ACaretPos, AModified, AIsInsert : string );
Var
  BDirty    : boolean;
Begin
 BDirty:=FALSE;
 if FCaretPos<>ACaretPos then begin FCaretPos:=ACaretPos; BDirty:=TRUE; end;
 if FModified<>AModified then begin FModified:=AModified; BDirty:=TRUE; end;
 if FIsInsert<>AIsInsert then begin FIsInsert:=AIsInsert; BDirty:=TRUE; end;
 if BDirty then PaintA;
End;

Initialization
  {$I concomi.lrs}

end.

