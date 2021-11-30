unit InfoViewBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, ConComI, AsmTypes_sd;

Type
  TOnErrorClick = Procedure ( Const AErrorCode : string ) of object;
  TIconIdx = (iiI, iiE, iiW, iiH, iiTS, iiTSK, iiTSE, iiTSU);

  TInfoViewBase = class(TCustomControl)
  private
  protected
    FIconBmpList        : array [TIconIdx] of TBitmap;
    FInfoList           : TStringList;
    FLineStartV         : Integer; // First visible line
    FLineThis           : Integer;
    FScrollU,
    FScrollD            : TScrollBtn;
    FScrollB            : TScrollBarV;
    FConstructed        : boolean;

    FViewBitmap         : TBitmap;
    FLineHeight,
    FHdrTextHeight      : Integer;
    FHdrHeight          : Integer;
    FLeftR              : Integer;

    FPrjPath            : string;
    FOnErrorClick       : TOnErrorClick;

    Procedure ScrollMoved ( AStart : Integer );
    Procedure ViewLine ( ATop : Integer; Const AInfoCode : string; AActive : boolean );

  protected
    Procedure Paint; Override;
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure DblClick; Override;
    Procedure KeyDown ( Var AKey : Word; AShift : TShiftState ); Override;
    Function DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean; Override;

    Procedure ScrollUpdate;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Function CanFocus : boolean; Override;
    Procedure SetFocus; Override;
    Procedure PaintA;

    Procedure Clear;
    Procedure AppendAny ( Const AMessage : string );
    Function GetMessageCount : Integer;

    property OnErrorClick : TOnErrorClick read FOnErrorClick write FOnErrorClick;
    property PrjPath : string read FPrjPath write FPrjPath;
  end;

implementation

Uses
  ConComS;

{ TInfoViewBase }

Const
  CBmpNames : array [TIconIdx] of string =
   (
    'SdInfo',
    'SdError',
    'SdWarning',
    'SdHint',
    'SdTestSuite',
    'SdTestSuite_K',
    'SdTestSuite_E',
    'SdTestSuite_U'
   );

Constructor TInfoViewBase.Create ( AOwner : TComponent );
Var
  BIconIdx      : TIconIdx;
Begin
 Inherited;
 FConstructed:=FALSE;
 FInfoList:=TStringList.Create;

 FScrollU:=TScrollBtn.Create(Self); InsertControl(FScrollU); FScrollU.BtnKind:=bkUp;
 FScrollD:=TScrollBtn.Create(Self); InsertControl(FScrollD); FScrollD.BtnKind:=bkDn;

 FScrollB:=TScrollBarV.Create(Self); InsertControl(FScrollB);
 FScrollB.OnMoved:=@ScrollMoved;

 for BIconIdx in TIconIdx do
  begin
  FIconBmpList[BIconIdx]:=TBitmap.Create;
  try
   FIconBmpList[BIconIdx].LoadFromLazarusResource(CBmpNames[BIconIdx]);
  except
  end;
  end;

 FViewBitmap:=TBitmap.Create;
 FViewBitmap.Canvas.Font.Name:='Arial';
 FViewBitmap.Canvas.Font.Size:=8;
 FHdrTextHeight:=FViewBitmap.Canvas.TextHeight('Ayla');
 FLineHeight:=FHdrTextHeight; if FLineHeight<FIconBmpList[iiI].Height then FLineHeight:=FIconBmpList[iiI].Height;
 FHdrHeight:=FLineHeight; if FHdrHeight<15 then FHdrHeight:=15;

 Width:=200; Height:=100;
 FViewBitmap.SetSize(Width-4,Height-4);

 FConstructed:=TRUE;
End;

Destructor TInfoViewBase.Destroy;
Var
  BIconIdx      : TIconIdx;
Begin
 for BIconIdx in TIconIdx do FIconBmpList[BIconIdx].Free;

 RemoveControl(FScrollB); FScrollB.Free;
 RemoveControl(FScrollD); FScrollD.Free;
 RemoveControl(FScrollU); FScrollU.Free;

 FInfoList.Free;
 FViewBitmap.Free;
 Inherited;
End;

Function TInfoViewBase.CanFocus : boolean;
Begin
 Inherited;
 Result:=TRUE;
End;

Procedure TInfoViewBase.SetFocus;
Begin
 Inherited;
 ScrollUpdate;
End;

Procedure TInfoViewBase.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX,AY : Integer );
Var
  BLineThis     : Integer;
  BPaint        : boolean;
Begin
 Inherited;
 SetFocus;
 BPaint:=FALSE;
 BLineThis:=FLineStartV+((AY - FHdrHeight) div FLineHeight);
 if BLineThis<>FLineThis then
  begin
  FLineThis:=BLineThis;
  BPaint:=TRUE;
  end;

 if BPaint then PaintA;
End;

Procedure TInfoViewBase.DblClick;
Begin
 Inherited;

 repeat
 if FLineThis<0 then break;
 if FInfoList.Count=0 then break;
 if FLineThis>=FInfoList.Count then break;
 if Assigned(FOnErrorClick) then FOnErrorClick(FInfoList.Strings[FLineThis]);
 until TRUE;
End;

Procedure TInfoViewBase.KeyDown ( Var AKey : Word; AShift : TShiftState );
Var
  BLineCntV     : Integer;
  BPaint        : boolean;
  BLineThis,
  BLineStartV   : Integer;
  BLineCountV   : Integer;

Begin
 Inherited;

 BPaint:=FALSE;

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
     if (FLineThis+1)<FInfoList.Count then
      begin
      Inc(FLineThis);
      BPaint:=TRUE;
      end;
     end;

   VK_PRIOR:
     begin
     BLineCountV:=FViewBitmap.Height div FLineHeight;
     BLineThis:=FLineThis-BLineCountV;
     BLineStartV:=FLineStartV-BLineCountV;
     if BLineThis<0 then BLineThis:=0;
     if BLineStartV<0 then BLineStartV:=0;
     if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
      begin
      FLineThis:=BLineThis;
      FLineStartV:=BLineStartV;
      BPaint:=TRUE;
      end;
     end;

   VK_NEXT:
     begin
     BLineCountV:=FViewBitmap.Height div FLineHeight;
     BLineThis:=FLineThis+BLineCountV;
     BLineStartV:=FLineStartV+BLineCountV;
     if BLineThis>=FInfoList.Count then BLineThis:=FInfoList.Count-1;
     if BLineStartV>=FInfoList.Count then BLineStartV:=FInfoList.Count-1;
     if (BLineThis<>FLineThis) or (BLineStartV<>FLineStartV) then
      begin
      FLineThis:=BLineThis;
      FLineStartV:=BLineStartV;
      BPaint:=TRUE;
      end;
     end;


 end; // Case

 if BPaint then
  begin
  if FLineThis<FLineStartV then FLineStartV:=FLineThis;
  BLineCntV:=FViewBitmap.Height div FLineHeight;
  if FLineThis>=FLineStartV+BLineCntV then FLineStartV:=FLineThis-BLineCntV+1;
  ScrollUpdate;
  PaintA;
  end;

End;

Function TInfoViewBase.DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean;
Var
  BLineStartV   : Integer;
  BWheelDelta   : Integer;
Begin
 Inherited;

 if AWheelDelta<0 then BWheelDelta:=3
 else if AWheelDelta>0 then BWheelDelta:=-3
 else BWheelDelta:=0;

 BLineStartV:=FLineStartV+BWheelDelta;
 if BLineStartV>=FInfoList.Count then BLineStartV:=FInfoList.Count-1;
 if BLineStartV<0 then BLineStartV:=0;
 if BLineStartV<>FLineStartV then
  begin
  FLineStartV:=BLineStartV;
  ScrollUpdate;
  PaintA;
  end;

 Result:=TRUE;
End;

Procedure TInfoViewBase.ScrollMoved ( AStart : Integer );
Begin
 FLineStartV:=0+FScrollB.LineTop;
 PaintA;
End;

Procedure TInfoViewBase.Clear;
Begin
 FInfoList.Clear;
 FLineStartV:=0;
 FLineThis:=-1;
 ScrollUpdate;
 PaintA;
End;

Procedure TInfoViewBase.AppendAny ( Const AMessage : string );
Begin
 FInfoList.Append(AMessage);
 ScrollUpdate;
 PaintA;
End;

Function TInfoViewBase.GetMessageCount : Integer;
Begin
 Result:=FInfoList.Count;
End;

Procedure TInfoViewBase.Resize;
Var
  BLeft,
  BTopU,
  BTopD,
  BTopB : Integer;
Begin
 Inherited;

 repeat
 if FConstructed=FALSE then break;

 FViewBitmap.Width:=Width-4;
 FViewBitmap.Height:=Height-4;

 BLeft:=2+FViewBitmap.Width-FScrollU.Width;
 BTopU:=2+FHdrHeight;
 BTopD:=FViewBitmap.Height-FScrollD.Height+2;
 BTopB:=BTopU+FScrollU.Height;
 FScrollU.Top:=BTopU; FScrollU.Left:=BLeft;
 FScrollD.Top:=BTopD; FScrollD.Left:=BLeft;
 FScrollB.Left:=BLeft; FScrollB.Top:=BTopB; FScrollB.Height:=BTopD-BTopB;

 FLeftR:=(FViewBitmap.Width*3) div 4;

 ScrollUpdate;
 until TRUE;
End;

Procedure TInfoViewBase.ScrollUpdate;
Begin
 FScrollB.SetPos(FLineStartV,FViewBitmap.Height div FLineHeight,FInfoList.Count);
End;

Procedure TInfoViewBase.Paint;
Begin
 Inherited;
 BmpBox(Canvas,0,0,Width,Height,$808080,$FFFFFF,$404040,$808080);
 PaintA;
End;

Procedure TInfoViewBase.PaintA;
Var
  BLineIdx      : Integer;
  BTop          : Integer;
Begin
 BmpBox(FViewBitmap,$FFFFFF);

 BmpBox(FViewBitmap.Canvas,0,0,FLeftR,FHdrHeight,$808080,$404040,$FFFFFF,$808080,clBtnFace);
 BmpBox(FViewBitmap.Canvas,FLeftR,0,FViewBitmap.Width-FLeftR-FScrollU.Width,FHdrHeight,$808080,$404040,$FFFFFF,$808080,clBtnFace);
 BmpBox(FViewBitmap.Canvas,FViewBitmap.Width-FScrollU.Width,0,FScrollU.Width,FHdrHeight,clBtnFace);

 FViewBitmap.Canvas.Brush.Style:=bsClear;
 FViewBitmap.Canvas.Font.Color:=$000000;
 BTop:=(FHdrHeight-FHdrTextHeight) div 2;
 FViewBitmap.Canvas.TextOut(4,BTop,'Build flow and messages');
 FViewBitmap.Canvas.TextOut(FLeftR+4,BTop,'Reporter');

 BTop:=FHdrHeight;
 BLineIdx:=FLineStartV;
 while BLineIdx<FInfoList.Count do
  begin
  if BTop>=FViewBitmap.Height then break;
  ViewLine(BTop,FInfoList.Strings[BLineIdx],BLineIdx=FLineThis);
  BTop:=BTop+FLineHeight;
  inc(BLineIdx);
  end;
 Canvas.Draw(2,2,FViewBitmap);
End;

{
Procedure TInfoViewBase.ViewLine ( ATop : Integer; Const AInfoCode : string; AActive : boolean );
Var
  BInfoType     : char;
  BComment,
  BReporter     : string;
  BImage        : TBitmap;
  BTextTop,
  BLeft         : Integer;
  BDummyS       : string;
  BTextColor    : Cardinal;
Begin
 if AActive then
  begin
  BLeft:=FIconBmpList[iiI].Width+4;
  BmpBox(FViewBitmap.Canvas,BLeft,ATop,FViewBitmap.Width-BLeft,FLineHeight,$80FF80);
  end;


 BComment:=AInfoCode; BReporter:=CheckTag('R',BComment);
 DelLastSpace(BComment); if BComment='' then BInfoType:='*' else begin BInfoType:=BComment[1]; Delete(BComment,1,1); end;

 BImage:=nil; BTextColor:=$000000;
 case BInfoType of
   'i': begin BImage:=FIconBmpList[iiI]; BTextColor:=$000000; end;
   'e': begin BImage:=FIconBmpList[iiE]; BTextColor:=$000040; end;
   'w': begin BImage:=FIconBmpList[iiW]; BTextColor:=$004040; end;
   'h': begin BImage:=FIconBmpList[iiH]; BTextColor:=$404040; end;
   'u': begin BImage:=FIconBmpList[iiTS]; BTextColor:=$000000; end;
   'v': begin BImage:=FIconBmpList[iiTSK]; BTextColor:=$000000; end;
   'V': begin BImage:=FIconBmpList[iiTSE]; BTextColor:=$000000; end;
   'U': begin BImage:=FIconBmpList[iiTSU]; BTextColor:=$000000; end;
   'g': BImage:=nil;
 end;

 BLeft:=2;
 if BImage<>nil then FViewBitmap.Canvas.Draw(BLeft,ATop,BImage);
 inc(BLeft,FIconBmpList[iiI].Width+4);

 BTextTop:=ATop+((FIconBmpList[iiI].Height-FLineHeight) div 2);

 FViewBitmap.Canvas.Brush.Style:=bsClear;
 BDummyS:=BComment;
 FViewBitmap.Canvas.Font.Color:=BTextColor;
 FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS);

 if BReporter<>'' then
  begin
  BDummyS:=BReporter;
  FViewBitmap.Canvas.Font.Color:=$000000;
  FViewBitmap.Canvas.TextOut(FLeftR,BTextTop,BDummyS);
  end;
End;
}

Procedure TInfoViewBase.ViewLine ( ATop : Integer; Const AInfoCode : string; AActive : boolean );
Var
  BErrorType    : char;
  BComment,
  BReporter,
  BFilename     : string;
  BLine,
  BPos          : Integer;
  BImage        : TBitmap;
  BTextTop,
  BLeft         : Integer;
  BDummyS       : string;
  BTextColor    : Cardinal;
Begin

 if AActive then
  begin
  BLeft:=FIconBmpList[iiI].Width+4;
  BmpBox(FViewBitmap.Canvas,BLeft,ATop,FViewBitmap.Width-BLeft,FLineHeight,$80FF80);
  end;

 ParseError(AInfoCode,BComment,BReporter,BFilename,BLine,BPos);
 if BComment='' then BErrorType:=#0
 else begin BErrorType:=BComment[1]; Delete(BComment,1,1); end;
 BImage:=nil; BTextColor:=$000000;
 case BErrorType of
   'i': begin BImage:=FIconBmpList[iiI]; BTextColor:=$000000; end;
   'e': begin BImage:=FIconBmpList[iiE]; BTextColor:=$0000F0; end;
   'w': begin BImage:=FIconBmpList[iiW]; BTextColor:=$0080F0; end;
   'h': begin BImage:=FIconBmpList[iiH]; BTextColor:=$404040; end;
   'j': begin BImage:=FIconBmpList[iiTS]; BTextColor:=$C00000; end;  // Blue arrow
   'k': begin BImage:=FIconBmpList[iiTSK]; BTextColor:=$F00000; end; // Sheet with "V" mark
   'K': begin BImage:=FIconBmpList[iiTSE]; BTextColor:=$0000C0; end; // Orange arrow
   'J': begin BImage:=FIconBmpList[iiTSU]; BTextColor:=$00C000; end; // Green arrow
   '-': begin BImage:=nil; BTextColor:=$606060; end;
   'G': BImage:=nil;
 end;

 BLeft:=2;
 if BImage<>nil then FViewBitmap.Canvas.Draw(BLeft,ATop,BImage);
 inc(BLeft,FIconBmpList[iiI].Width+4);

 BTextTop:=ATop+((FIconBmpList[iiI].Height-FLineHeight) div 2);

 FViewBitmap.Canvas.Brush.Style:=bsClear;
 if BFilename='' then
  begin
  BDummyS:=BComment;
  FViewBitmap.Canvas.Font.Color:=BTextColor;
  FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS);
  end
 else if BErrorType in ['u', 'v', 'V', 'U'] then
  begin
  BDummyS:=RelFilename(FPrjPath,BFilename)+' -> ';
  FViewBitmap.Canvas.Font.Color:=$606060;
  FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS); Inc(BLeft,FViewBitmap.Canvas.TextWidth(BDummyS));
  BDummyS:=BComment;
  FViewBitmap.Canvas.Font.Color:=BTextColor;
  FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS);
  end
 else
  begin
  BDummyS:=RelFilename(FPrjPath,BFilename)+' ['+IntToStr(BLine)+','+IntToStr(BPos)+']: ';
  FViewBitmap.Canvas.Font.Color:=$606060;
  FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS); Inc(BLeft,FViewBitmap.Canvas.TextWidth(BDummyS));
  BDummyS:=BComment;
  FViewBitmap.Canvas.Font.Color:=BTextColor;
  FViewBitmap.Canvas.TextOut(BLeft,BTextTop,BDummyS);
  end;

 if BReporter<>'' then
  begin
  BDummyS:=BReporter;
  FViewBitmap.Canvas.Font.Color:=$000000;
  FViewBitmap.Canvas.TextOut(FLeftR,BTextTop,BDummyS);
  end;
End;



Initialization
  {$I sdinfoviewbase.lrs}

end.

