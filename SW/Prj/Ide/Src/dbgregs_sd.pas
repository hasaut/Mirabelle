unit DbgRegs_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ComCtrls, ExtCtrls, Forms,
  Buttons, LCLType;

Type
  TDbgRegs = class(TCustomControl)
  private
    FParent     : TTabSheet;
    FBitmap     : TBitmap;
    FActive     : boolean;
    FMcxPrev,
    FMcxThis,
    FMcxComb    : string;

    FCoreType   : char;
    FCoreIdx    : Integer;

    FRegLeftR,
    FRegLeftD,
    FRegLeftW,
    FRegLeftL   : Integer;

    Procedure WriteRegMcgGpr ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
    Procedure WriteRegMcg32b ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
    Procedure WriteRegMcg8b ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
    Procedure WriteQue ( ALeft, ATop : Integer; Const ALabel : string; Const APrev, AThis : string );
    Procedure PaintRegsMcg;

    Procedure WriteRegRV ( ATop, ALeft : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
    Procedure PaintRegsRV;

    Procedure SetCpuImageOpti;
    Procedure PaintA;

  protected
    Procedure Paint; Override;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TTabSheet; ACoreType : char; ACoreIdx : Cardinal );
    Procedure Done;
    Procedure SetState ( AActive : boolean; Const AMcxPrev, AMcxThis, AMcxComb : string );

    Function GetViewDeltaX : Integer;
    Function GetViewDeltaY : Integer;

    property Active : boolean read FActive;
    property CoreType : char read FCoreType;
  end;

implementation

Uses
  ConComL, ConComI;

Const
  CColorBGRegs = $F0F0F0;

Constructor TDbgRegs.Create ( AOwner : TComponent );
Var
  BWidth,
  BHeight       : Integer;
Begin
  Inherited;
  Color:=CColorBGRegs;
  FBitmap:=TBitmap.Create;
  FBitmap.Canvas.Font.Name:='Courier New';
  FBitmap.Canvas.Font.Size:=10;
  FBitmap.Canvas.Font.Style:=[];
  BWidth:=FBitmap.Canvas.TextWidth(' EIP XXXXXXXX reg XXXXXXXX ');
  BHeight:=160;
  FBitmap.SetSize(BWidth,BHeight);
  Width:=FBitmap.Width; Height:=FBitmap.Height;
End;

Destructor TDbgRegs.Destroy;
Begin
  FBitmap.Free;
  Inherited;
End;

Procedure TDbgRegs.Init ( AParent : TTabSheet; ACoreType : char; ACoreIdx : Cardinal );
Begin
 FParent:=AParent; FCoreType:=ACoreType; FCoreIdx:=ACoreIdx;
 FParent.InsertControl(Self);
 FParent.Caption:='C'+IntToStr(ACoreIdx);
 SetCpuImageOpti;
 Align:=alClient;
End;

Procedure TDbgRegs.Done;
Begin
 FParent.RemoveControl(Self);
End;

Procedure TDbgRegs.SetCpuImageOpti;
Begin
 case FCoreType of
   'e': FParent.ImageIndex:=0;
   's': FParent.ImageIndex:=1;
   else FParent.ImageIndex:=-1;
 end; // case
End;

Procedure TDbgRegs.SetState ( AActive : boolean; Const AMcxPrev, AMcxThis, AMcxComb : string );
Begin
 FActive:=AActive;
 FMcxPrev:=AMcxPrev;
 FMcxThis:=AMcxThis;
 FMcxComb:=AMcxComb;
 PaintA;
End;

Procedure TDbgRegs.Paint;
Begin
 Inherited;
 FBitmap.SetSize(Width,Height);
 PaintA;
End;

Const
  CColorSame    = $808080;
  CColorChan    = $FF0000;
  CColorDiff    = $0000FF;
  CCombDelta    = 4;

Const
  CTextOffsetA  = 6;

// *** Mcg ***

Procedure TDbgRegs.WriteRegMcgGpr ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
Var
  BPrev,
  BThis,
  BComb     : string;
Begin
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.Font.Style:=[fsBold];
 FBitmap.Canvas.TextOut(FRegLeftR-FBitmap.Canvas.TextWidth(ALabel)-CTextOffsetA,ATop,ALabel);

 FBitmap.Canvas.Font.Style:=[fsBold];
 if (AThis='') or FActive then
  begin
  FBitmap.Canvas.Font.Color:=$808080;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,'XXXXXXXXXXXXXXXX');
  end
 else
  begin
  BPrev:=Copy(APrev,1,8); BThis:=Copy(AThis,1,8); BComb:=Copy(AComb,1,8);
  if (BComb<>'') and (BComb<>BThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftR,ATop,BComb);
   end;
  if BPrev=BThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,BThis);

  BPrev:=Copy(APrev,9,4); BThis:=Copy(AThis,9,4); BComb:=Copy(AComb,9,4);
  if (BComb<>'') and (BComb<>BThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftD,ATop,BComb);
   end;
  if BPrev=BThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftD,ATop,BThis);

  BPrev:=Copy(APrev,13,2); BThis:=Copy(AThis,13,2); BComb:=Copy(AComb,13,2);
  if (BComb<>'') and (BComb<>BThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftW,ATop,BComb);
   end;
  if BPrev=BThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftW,ATop,BThis);

  BPrev:=Copy(APrev,15,2); BThis:=Copy(AThis,15,2); BComb:=Copy(AComb,15,2);
  if (BComb<>'') and (BComb<>BThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftL,ATop,BComb);
   end;
  if BPrev=BThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftL,ATop,BThis);
  end;
End;

Procedure TDbgRegs.WriteRegMcg32b ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
Begin
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.Font.Style:=[fsBold];
 FBitmap.Canvas.TextOut(FRegLeftR-FBitmap.Canvas.TextWidth(ALabel)-CTextOffsetA,ATop,ALabel);

 FBitmap.Canvas.Font.Style:=[fsBold];
 if (AThis='') or FActive then
  begin
  FBitmap.Canvas.Font.Color:=$808080;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,'XXXXXXXX');
  end
 else
  begin
  if (AComb<>'') and (AComb<>AThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftR,ATop,AComb);
   end;
  if APrev=AThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,AThis);
  end;
End;

Procedure TDbgRegs.WriteRegMcg8b ( ATop : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
Begin
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.Font.Style:=[fsBold];
 FBitmap.Canvas.TextOut(FRegLeftR-FBitmap.Canvas.TextWidth(ALabel)-CTextOffsetA,ATop,ALabel);

 FBitmap.Canvas.Font.Style:=[fsBold];
 if (AThis='') or FActive then
  begin
  FBitmap.Canvas.Font.Color:=$808080;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,'XX');
  end
 else
  begin
  if (AComb<>'') and (AComb<>AThis) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(FRegLeftR,ATop,AComb);
   end;
  if APrev=AThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(FRegLeftR,ATop,AThis);
  end;
End;

Procedure TDbgRegs.WriteQue ( ALeft, ATop : Integer; Const ALabel : string; Const APrev, AThis : string );
Var
  BLeft         : Integer;
  BIndex        : Integer;
  BDummyS       : string;
  BLenThis,
  BQueThis      : string;
Begin
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.Font.Style:=[fsBold];
 FBitmap.Canvas.TextOut(ALeft,ATop,ALabel);

 FBitmap.Canvas.Font.Style:=[];
 if (AThis='') or FActive then
  begin
  FBitmap.Canvas.Font.Color:=$808080;
  FBitmap.Canvas.TextOut(ALeft+FBitmap.Canvas.TextWidth(ALabel+' '),ATop,' X');
  end
 else
  begin
  FBitmap.Canvas.Font.Color:=CColorSame;
  BQueThis:=Copy(AThis,1,12); BLenThis:=Copy(AThis,13,1);
  BDummyS:=BLenThis;
  if Length(BDummyS)<2 then BDummyS:=' '+BDummyS;
  FBitmap.Canvas.TextOut(ALeft+FBitmap.Canvas.TextWidth(ALabel+' '),ATop,BDummyS);

  BLeft:=ALeft;
  BIndex:=0;
  while BIndex<3 do
   begin
   FBitmap.Canvas.Font.Color:=CColorSame;
   FBitmap.Canvas.TextOut(BLeft,ATop+13,Copy(BQueThis,1+BIndex*4,4));
   inc(BLeft,FBitmap.Canvas.TextWidth('0000')+3);
   inc(BIndex);
   end;
  end;
End;

Const                  // from iq to gq
  COffsetRegG : array [0..7] of integer = (0, 16, 32, 48, 64, 80, 96, 112);
  CLabelRegG  : array [0..7] of string = ('QIP', 'QAX', 'QBX', 'QCX', 'QDX', 'QEX', 'QFX', 'QGX');


Procedure TDbgRegs.PaintRegsMcg;
Var
  BTop          : Integer;
  BTextHeight   : Integer;
  BRegThis,
  BRegPrev,
  BRegComb      : string;
  BIndex        : Integer;
Begin
 FRegLeftR:=1+16+FBitmap.Canvas.TextWidth('RRR');
 FRegLeftD:=FRegLeftR+FBitmap.Canvas.TextWidth('00000000')+2;
 FRegLeftW:=FRegLeftD+FBitmap.Canvas.TextWidth('0000')+2;
 FRegLeftL:=FRegLeftW+FBitmap.Canvas.TextWidth('00');
 BTextHeight:=15; //FBitmap.Canvas.TextHeight('Ayla');

 FBitmap.Canvas.Brush.Color:=CColorBG;
 FBitmap.Canvas.Brush.Style:=bsSolid;
 FBitmap.Canvas.FillRect(1,1,FRegLeftR-5,FBitmap.Height-2);
 FBitmap.Canvas.Pen.Style:=psSolid;
 FBitmap.Canvas.Pen.Color:=$808080;
 FBitmap.Canvas.Line(FRegLeftR-4,1,FRegLeftR-4,FBitmap.Height-2);

 FBitmap.Canvas.Brush.Style:=bsClear;

 BTop:=4+BTextHeight*9;
 for BIndex:=0 to 7 do
  begin
  BRegPrev:=Copy(FMcxPrev,1+COffsetRegG[BIndex],16);
  BRegThis:=Copy(FMcxThis,1+COffsetRegG[BIndex],16);
  BRegComb:=Copy(FMcxComb,1+COffsetRegG[BIndex],16);
  if BIndex=0 then
   begin
   WriteRegMcg32b(BTop,'EIP','  '+Copy(BRegPrev,1+8+2,6),'  '+Copy(BRegThis,1+8+2,6),'  '+Copy(BRegComb,1+8+2,6)); Dec(BTop,BTextHeight);
   WriteRegMcg32b(BTop,'ESP',Copy(BRegPrev,1+0,8),Copy(BRegThis,1,8),Copy(BRegComb,1,8)); Dec(BTop,2);
   end
  else WriteRegMcgGpr(BTop,CLabelRegG[BIndex],BRegPrev,BRegThis,BRegComb);
  Dec(BTop,BTextHeight);
  end;
 BRegPrev:=Copy(FMcxPrev,1+8,2);
 BRegThis:=Copy(FMcxThis,1+8,2);
 BRegComb:=Copy(FMcxComb,1+8,2);
 WriteRegMcg8b(BTop,'M',BRegPrev,BRegThis,BRegComb);
End;


{
 +00 000084800000005E
 +16 41E8000041E80000
 +32 0000000042B40000
 +48 0000000000000000
 +64 0000000000000000
 +80 0000000000000000
 +96 0000000000000000
+112 0000000000000000
+128 0000000000000000
+144 0000000000000200
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
}

// *** Risc-V ***

Procedure TDbgRegs.WriteRegRV ( ATop, ALeft : Integer; Const ALabel : string; Const APrev, AThis, AComb : string );
Begin
 FBitmap.Canvas.Font.Color:=$000000;
 FBitmap.Canvas.Font.Style:=[fsBold];
 FBitmap.Canvas.TextOut(ALeft+FRegLeftR-FBitmap.Canvas.TextWidth(ALabel)-CTextOffsetA,ATop,ALabel);

 if (AThis='') or FActive then
  begin
  FBitmap.Canvas.Font.Color:=$808080;
  FBitmap.Canvas.TextOut(ALeft+FRegLeftR,ATop,'XXXXXXXX');
  end
 else
  begin
  if (AComb<>'') and (AThis<>AComb) then
   begin
   FBitmap.Canvas.Font.Color:=CColorDiff;
   FBitmap.Canvas.TextOut(ALeft+FRegLeftR+CCombDelta,ATop-CCombDelta,AComb);
   end;
  if APrev=AThis then FBitmap.Canvas.Font.Color:=CColorSame
  else FBitmap.Canvas.Font.Color:=CColorChan;
  FBitmap.Canvas.TextOut(ALeft+FRegLeftR,ATop,AThis);
  end;
End;

Const                  // from x1 to eip
  COffsetRegRV : array [0..15] of integer = (8, 24, 40, 56, 72, 88, 104, 120, 0, 16, 32, 48, 64, 80, 96, 112);
  //CLabelRegRV  : array [0..15] of string = ('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x14', 'x15', 'eip');
  CLabelRegRV  : array [0..15] of string = ('PC', 'ra', 'sp', 'gp', 'tp', 't0', 't1', 't2', 's0', 's1', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5');

Procedure TDbgRegs.PaintRegsRV;
Var
  BTop          : Integer;
  BTextHeight   : Integer;
  BIndex        : Integer;
  BLeft         : Integer;
Begin
 FRegLeftR:=1+5+FBitmap.Canvas.TextWidth('RRR');
 FRegLeftD:=FRegLeftR+FBitmap.Canvas.TextWidth('00000000')+2;
 BTextHeight:=15; //FBitmap.Canvas.TextHeight('Ayla');

 FBitmap.Canvas.Brush.Color:=CColorBG;
 FBitmap.Canvas.Brush.Style:=bsSolid;
 FBitmap.Canvas.Pen.Style:=psSolid;
 FBitmap.Canvas.Pen.Color:=$808080;
 BLeft:=0;
 for BIndex:=0 to 1 do
  begin
  FBitmap.Canvas.FillRect(BLeft+1,1,BLeft+FRegLeftR-5,FBitmap.Height-2);
  FBitmap.Canvas.Line(BLeft+FRegLeftR-4,1,BLeft+FRegLeftR-4,FBitmap.Height-2);
  BLeft:=BLeft+(FBitmap.Width div 2);
  end;
 BLeft:=(FBitmap.Width div 2)-1;
 FBitmap.Canvas.Pen.Color:=$FFFFFF;
 FBitmap.Canvas.Line(BLeft,1,BLeft,FBitmap.Height-2);

 FBitmap.Canvas.Brush.Style:=bsClear;

 BTop:=4+BTextHeight*8;
 for BIndex:=0 to 7 do
  begin
  WriteRegRV(BTop,                  0,CLabelRegRV[8+BIndex],Copy(FMcxPrev,1+COffsetRegRV[8+BIndex],8),Copy(FMcxThis,1+COffsetRegRV[8+BIndex],8),Copy(FMcxComb,1+COffsetRegRV[8+BIndex],8));
  if BIndex=0 then WriteRegRV(BTop,FBitmap.Width div 2,CLabelRegRV[0+BIndex],Copy(FMcxPrev,1+COffsetRegRV[0+BIndex]+2,6),Copy(FMcxThis,1+COffsetRegRV[0+BIndex]+2,6),Copy(FMcxComb,1+COffsetRegRV[0+BIndex]+2,6))
  else WriteRegRV(BTop,FBitmap.Width div 2,CLabelRegRV[0+BIndex],Copy(FMcxPrev,1+COffsetRegRV[0+BIndex],8),Copy(FMcxThis,1+COffsetRegRV[0+BIndex],8),Copy(FMcxComb,1+COffsetRegRV[0+BIndex],8));
  Dec(BTop,BTextHeight);
  end;
End;

Procedure TDbgRegs.PaintA;
Var
  BFlags    : byte;
  BCoreType : char;
Begin
 BmpBox(FBitmap,$808080,$FFFFFF,CColorBGRegs);

 FBitmap.Canvas.Brush.Color:=CColorBGRegs;
 FBitmap.Canvas.Brush.Style:=bsClear;

 BCoreType:=FCoreType;
 if HexToByteCheck(Copy(FMcxThis,1+8,2),BFlags)=FALSE then
 else
  begin
  case ((BFlags shr 4) and $3) of
    0: BCoreType:='e';
    1: BCoreType:='s';
  end;
  end;
 if BCoreType<>FCoreType then
  begin
  FCoreType:=BCoreType;
  SetCpuImageOpti;
  end;

 case FCoreType of
  's': PaintRegsMcg;
  'e': PaintRegsRV;
  else PaintRegsMcg;
 end; // case

 Canvas.Draw(0,0,FBitmap);
End;

Function TDbgRegs.GetViewDeltaX : Integer;
Begin
 Result:=FBitmap.Width-Width;
End;

Function TDbgRegs.GetViewDeltaY : Integer;
Begin
 Result:=FBitmap.Height-Height;
End;

end.

