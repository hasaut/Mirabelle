unit HMenu;

interface

Uses
  Classes, Types, Windows, Messages, Controls, Graphics, Forms,
  StdCtrls, SysUtils, ExtCtrls, LCLType;

Type
  TItemStyle = ( isComment, isItem, isSeparator );
  
  TVMenu = class;

  THItem = class(TObject)
  private
    FNext       : THItem;
    FChild      : THItem;
    FBitmap     : TBitmap;

    FCaption    : string;
    FStart,
    FSize       : Integer;
    FStyle      : TItemStyle;

    FOnClick    : TNotifyEvent;
    FData       : TObject;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure ClearChildList;
    Function AppendChild ( AImage : TBitmap; Const ACaption : string; AStyle : TItemStyle ) : THItem; Overload;
    Function AppendChild ( Const AImageName : string; Const ACaption : string; AStyle : TItemStyle ) : THItem; Overload;
    Function AppendChild ( AImage : TBitmap; Const ACaption : string; AStyle : TItemStyle; AOnClick : TNotifyEvent ) : THItem; Overload;
    Function AppendChild ( Const AImageName : string; Const ACaption : string; AStyle : TItemStyle; AOnClick : TNotifyEvent ) : THItem; Overload;

    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property Data : TObject read FData write FData;

  end; (* THMenuItem *)

  THMenu = class(TCustomControl)
  private
    FBitmap,
    FBitmapA    : TBitmap;
    FItems      : THItem;
    FBkColorA,
    FBkColorB   : TColor;
    FSelItem    : THItem;
    FVMenu      : TVMenu;

    FVAutoOpen  : TTimer;
    FInsideTimer: TTimer;

    FInsideH,
    FInsideV    : boolean;

    Procedure PaintB;
    Procedure SetSizeA;

    Procedure DestroyV;

    Procedure FVAutoOpenTimer ( Sender : TObject );
    Procedure FInsideTimerTimer ( Sender : TObject );
    Procedure ReenableInsideTimer;
    Procedure ReopenVMenu ( AItem : THItem );
    Procedure GetSelIndex ( AX, AY : Integer );

  protected
    Procedure WndProc ( Var AMessage : TMessage ); override;
    Procedure Paint; Override;
    Procedure AdjustSize; Override;

    Procedure MouseEnter; Override;
    Procedure MouseLeave; Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetBkColor ( AColorA, AColorB : TColor );
    Procedure ClearItems;
    Function AppendItem ( Const ACaption : string ) : THItem;

  end; (* THMenu *)

  TVMenu = class(TCustomControl)
  private
    FBitmap     : TBitmap;
    FCtrlItem   : THItem;
    FHMenu      : THMenu;
    FMsgTrace   : boolean;

    FBkColor,
    FBkComColor,
    FFrameColor,
    FIconColor  : TColor;
    FSelItem    : THItem;

    Procedure SelfDestroy;

    Procedure PaintB;

    Procedure GetSelIndex ( AX, AY : Integer );

  protected
    Procedure CreateParams ( Var AParams : TCreateParams ); Override;
    Procedure WndProc ( Var AMessage : TMessage ); override;
    Procedure Paint; Override;
    Procedure MouseMove ( AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Prepare ( AHMenu : THMenu );
    Procedure SetCtrlItem ( AItem : THItem );
  end; (* TVMenu *)

implementation

Uses
  ConComL;

Procedure TranspBitmap ( ABitmap : TBitmap; AColorB : TColor );
Var
  BColIndex,
  BRowIndex     : Integer;
  BColorA,
  BColorB       : TColor;
  BDummyB       : byte;

Begin
 repeat
 if ABitmap.Height=0 then break;
 ABitmap.PixelFormat:=pf32bit;
 BColorB:=AColorB;
 BColorB:=((AColorB and $0000FF) shl 16) or (AColorB and $00FF00) or ((AColorB and $FF0000) shr 16);

 BColorA:=ABitmap.Canvas.Pixels[0,0];
 BRowIndex:=0;
 while BRowIndex<ABitmap.Height do
  begin
  BColIndex:=0;
  while BColIndex<ABitmap.Width do
   begin
   if ABitmap.Canvas.Pixels[BColIndex,BRowIndex]=BColorA then ABitmap.Canvas.Pixels[BColIndex,BRowIndex]:=BColorB;
   inc(BColIndex);
   end;
  inc(BRowIndex);
  end;
 until TRUE;
End;

{*** TMenuItem ***}

Constructor THItem.Create;
Begin
 Inherited;
 FBitmap:=TBitmap.Create;
End;

Destructor THItem.Destroy;
Begin
 ClearChildList;
 FBitmap.Free;
 Inherited;
End;

Procedure THItem.ClearChildList;   
Var
  BItem1,
  BItem2        : THItem;

Begin
 BItem1:=FChild;
 FChild:=nil;
 while BItem1<>nil do
  begin
  BItem2:=BItem1.FNext;
  BItem1.Free;
  BItem1:=BItem2;
  end;
End;

Function THItem.AppendChild ( AImage : TBitmap; Const ACaption : string; AStyle : TItemStyle ) : THItem;
Var
  BItem         : THItem;

Begin
 Result:=THItem.Create;
 if AImage<>nil then Result.FBitmap.Assign(AImage);
 Result.FCaption:=ACaption;
 Result.FStyle:=AStyle;
 if FChild=nil then FChild:=Result
 else
  begin
  BItem:=FChild;
  while BItem.FNext<>nil do BItem:=BItem.FNext;
  BItem.FNext:=Result;
  end;
End;

Function THItem.AppendChild ( Const AImageName : string; Const ACaption : string; AStyle : TItemStyle ) : THItem;
Var
  BBitmap       : TBitmap;

Begin
 BBitmap:=TBitmap.Create;

 try
  BBitmap.LoadFromResourceName(hInstance,AImageName);
  Result:=AppendChild(BBitmap,ACaption,AStyle);
 except
  Result:=AppendChild(nil,ACaption,AStyle);
 end;

 BBitmap.Free;
End;

Function THItem.AppendChild ( AImage : TBitmap; Const ACaption : string; AStyle : TItemStyle; AOnClick : TNotifyEvent ) : THItem;
Begin
 Result:=AppendChild(AImage,ACaption,AStyle);
 Result.FOnClick:=AOnClick;
End;

Function THItem.AppendChild ( Const AImageName : string; Const ACaption : string; AStyle : TItemStyle; AOnClick : TNotifyEvent ) : THItem;
Begin
 Result:=AppendChild(AImageName,ACaption,AStyle);
 Result.FOnClick:=AOnClick;
End;

{*** THMenu ***}

Constructor THMenu.Create ( AOwner : TComponent );
Begin
 Inherited;
 FBitmap:=TBitmap.Create;
 FBitmapA:=TBitmap.Create;

 FVAutoOpen:=TTimer.Create(Self);
 FVAutoOpen.Interval:=1000;
 FVAutoOpen.OnTimer:=@FVAutoOpenTimer;

 FInsideTimer:=TTimer.Create(Self);
 FInsideTimer.Interval:=250;
 FInsideTimer.OnTimer:=@FInsideTimerTimer;

 FBkColorA:=$D8D0D0;
 FBkColorB:=$E8E0D0;
 FSelItem:=nil;
End;

Destructor THMenu.Destroy;
Begin
 if FVMenu<>nil then
  begin
  FVMenu.Free;
  FVMenu:=nil;
  end;
 ClearItems;

 FVAutoOpen.Free;
 FInsideTimer.Free;

 FBitmapA.Free;
 FBitmap.Free;
 Inherited;
End;

Procedure THMenu.ClearItems;
Var
  BItem1,
  BItem2        : THItem;

Begin
 BItem1:=FItems;
 FItems:=nil;
 FSelItem:=nil;
 while BItem1<>nil do
  begin
  BItem2:=BItem1.FNext;
  BItem1.Free;
  BItem1:=BItem2;
  end;
End;

Function THMenu.AppendItem ( Const ACaption : string ) : THItem;
Var
  BItem : THItem;

Begin
 Result:=THItem.Create;
 Result.FCaption:=ACaption;
 Result.FSize:=FBitmapA.Canvas.TextWidth(Result.FCaption)+8;
 if FItems=nil then
  begin
  FItems:=Result;
  Result.FStart:=4;
  end
 else
  begin
  BItem:=FItems;
  while BItem.FNext<>nil do BItem:=BItem.FNext;
  BItem.FNext:=Result;
  Result.FStart:=BItem.FStart+BItem.FSize+4;
  end;
End;

Procedure THMenu.SetBkColor ( AColorA, AColorB : TColor );
Begin
 FBkColorA:=AColorA;
 FBkColorB:=AColorB;
 PaintB;
End;

Procedure THMenu.AdjustSize;
Begin
 Inherited;
 SetSizeA;
 PaintB;
End;

Procedure THMenu.SetSizeA;
Var
  BIndex        : Integer;
  BLen          : Integer;

Begin
 Inherited;
 FBitmap.PixelFormat:=pf32bit;
 FBitmap.Height:=Height-2;
 FBitmap.Width:=Width;
 FBitmap.Canvas.Pen.Style:=psSolid;

 BIndex:=0; BLen:=(FBitmap.Height+1) div 2;
 while BIndex<BLen do
  begin
  FBitmap.Canvas.Pen.Color:=AverageColor1(FBkColorA,FBkColorB,BLen,BIndex);
  FBitmap.Canvas.MoveTo(0,BIndex); FBitmap.Canvas.LineTo(FBitmap.Width-1,BIndex);
  FBitmap.Canvas.MoveTo(0,FBitmap.Height-BIndex-1); FBitmap.Canvas.LineTo(FBitmap.Width-1,FBitmap.Height-BIndex-1);
  inc(BIndex);
  end;
End;

Procedure THMenu.Paint;
Begin
 PaintB;
End;

Procedure THMenu.PaintB;
Var
  BItem         : THItem;
  BTop          : Integer;
  BRect         : TRect;

Begin
 repeat
 FBitmapA.Assign(FBitmap);

 BTop:=FBitmapA.Height-FBitmapA.Canvas.TextHeight('Ayla');
 if BTop<0 then BTop:=0;
 BTop:=BTop div 2;

 FBitmapA.Canvas.Font.Color:=clBlack;
 BItem:=FItems;
 while BItem<>nil do
  begin
  if BItem=FSelItem then
   begin
   FBitmapA.Canvas.Brush.Style:=bsSolid;
   BRect.Left:=BItem.FStart; BRect.Right:=BItem.FStart+BItem.FSize+1;
   BRect.Top:=1; BRect.Bottom:=FBitmapA.Height-1;
   FBitmapA.Canvas.Brush.Color:=$F0E0E0;
   FBitmapA.Canvas.FillRect(BRect);
   FBitmapA.Canvas.Brush.Color:=$F06060;
   FBitmapA.Canvas.FrameRect(BRect);
   FBitmapA.Canvas.Brush.Style:=bsClear;
   FBitmapA.Canvas.TextOut(BItem.FStart+4,BTop,BItem.FCaption);
   end
  else
   begin
   FBitmapA.Canvas.Brush.Style:=bsClear;
   FBitmapA.Canvas.TextOut(BItem.FStart+4,BTop+1,BItem.FCaption);
   end;
  BItem:=BItem.FNext;
  end;

 repeat
 if FVMenu=nil then break;
 if FVMenu.FCtrlItem=nil then break;

 with FVMenu.FCtrlItem do
  begin
  BRect.Left:=FStart; BRect.Right:=FStart+FSize+1;
  BRect.Top:=1; BRect.Bottom:=FBitmapA.Height;
  end;

 with FBitmapA.Canvas do
  begin
  Brush.Style:=bsSolid;
  Brush.Color:=FVMenu.FBkColor;
  FillRect(BRect);
  Pen.Color:=FVMenu.FFrameColor;
  Pen.Style:=psSolid;
  MoveTo(BRect.Left,BRect.Bottom);
  LineTo(BRect.Left,BRect.Top);
  LineTo(BRect.Right-1,BRect.Top);
  LineTo(BRect.Right-1,BRect.Bottom);
  with FVMenu.FCtrlItem do TextOut(FStart+4,BTop,FCaption);
  end;

 until TRUE;

 Canvas.Draw(0,1,FBitmapA);
 until TRUE;
End;

Procedure THMenu.FVAutoOpenTimer ( Sender : TObject );
Begin
 FVAutoOpen.Enabled:=FALSE;
End;

Procedure THMenu.FInsideTimerTimer ( Sender : TObject );
Begin
 FInsideTimer.Enabled:=FALSE;
 if (FInsideH=FALSE) and (FInsideV=FALSE) then
  begin
  if FVMenu<>nil then
   begin
   FVMenu.Free;
   FVMenu:=nil;
   PaintB;
   end;
  end;
End;

Procedure THMenu.ReenableInsideTimer;
Begin
 FInsideTimer.Enabled:=FALSE;
 FInsideTimer.Enabled:=TRUE;
End;

Procedure THMenu.MouseEnter;
Begin
 Inherited;
End;

Procedure THMenu.MouseLeave;
Begin
 Inherited;
 FSelItem:=nil;
 PaintB;
End;

Procedure THMenu.WndProc ( Var AMessage : TMessage );
Begin
 inherited WndProc(AMessage);
 case AMessage.Msg of
   $B013:
     begin
     FInsideH:=TRUE;
     ReenableInsideTimer;
     end;

   $B014:
     begin
     FInsideH:=FALSE;
     ReenableInsideTimer;
     end;
 end; (* case *)
End;

Procedure THMenu.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 GetSelIndex(AX,AY);
End;

Procedure THMenu.GetSelIndex ( AX, AY : Integer );
Var
  BItem         : THItem;

Begin
 BItem:=FItems;
 while BItem<>nil do
  begin
  if (BItem.FStart<AX) and (AX<(BItem.FStart+BItem.FSize)) then break;
  BItem:=BItem.FNext;
  end;

 if FSelItem<>BItem then
  begin
  FSelItem:=BItem;
  if (BItem<>nil) and ((FVMenu<>nil) or (FVAutoOpen.Enabled)) then
   begin
   ReopenVMenu(BItem);
   end;
  PaintB;
  end;

End;

Procedure THMenu.DestroyV;
Begin
 FVMenu:=nil;
 FVAutoOpen.Enabled:=TRUE;
 PaintB;
End;

Procedure THMenu.ReopenVMenu ( AItem : THItem );
Var
  BMenu         : TVMenu;
  BPos          : TPoint;

Begin
 if FVMenu<>nil then
  begin
  FVMenu.Free;
  FVMenu:=nil;
  end;

 BMenu:=TVMenu.Create(Self);
 BMenu.SetCtrlItem(AItem);
 BPos.X:=AItem.FStart; BPos.Y:=1+FBitmapA.Height;
 BPos:=ClientToScreen(BPos);
 BMenu.Left:=BPos.X;
 BMenu.Top:=BPos.Y;
// BMenu.SetFocus;
 BMenu.Show;
 BMenu.Prepare(Self);

 FVMenu:=BMenu;
End;

Procedure THMenu.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
 repeat
 if FVMenu<>nil then
  begin
  FVMenu.Free;
  FVMenu:=nil;
  PaintB;
  break;
  end;

 if FSelItem=nil then break;
 if FSelItem.FChild=nil then break;

 ReopenVMenu(FSelItem);
 PaintB;
 until TRUE;
End;

Procedure THMenu.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
End;

{*** TVMenu ***}

Constructor TVMenu.Create ( AOwner : TComponent );
Begin
 Inherited;
 FBitmap:=TBitmap.Create;
 Hide;
 ParentWindow:=Application.MainForm.Handle;
 FSelItem:=nil;

{ FForm:=TForm.Create(Self);
 FList:=TListBox.Create(FForm);
 FForm.InsertControl(FList);
 FList.Align:=alClient;
 FForm.Show;}

 FBkColor:=$D0E8F0;
 FBkComColor:=$E0F8F8;
 FFrameColor:=$508090;
 FIconColor:=$D0D0D0;
End;

Destructor TVMenu.Destroy;
Begin
{ FList.Free; FList:=nil;
 FForm.Free; FForm:=nil;}

 FBitmap.Free;
 Inherited;
End;

Procedure TVMenu.Prepare ( AHMenu : THMenu );
Begin
 FHMenu:=AHMenu;
End;

Procedure TVMenu.CreateParams ( Var AParams : TCreateParams );
Begin
 Inherited CreateParams(AParams);
 with AParams do
  begin
  if ParentWindow<>0 then Style:=Style and not WS_CHILD or WS_POPUP or WS_OVERLAPPED;
  WindowClass.Style:=CS_SAVEBITS or CS_DBLCLKS or not (CS_HREDRAW or not CS_VREDRAW);
  ExStyle:=ExStyle or WS_EX_TOPMOST;
  end;
end;

Procedure TVMenu.WndProc ( Var AMessage : TMessage );
Begin
 inherited WndProc(AMessage);
 case AMessage.Msg of
   $B013:
     begin
     if FHMenu<>nil then
      begin
      FHMenu.FInsideH:=TRUE;
      FHMenu.ReenableInsideTimer;
      end;
     end;

   $B014:
     begin
     if FHMenu<>nil then
      begin
      FHMenu.FInsideV:=FALSE;
      FHMenu.ReenableInsideTimer;
      end;
     SelfDestroy;
     end;
 end; (* case *)
// if (FList<>nil) and (AMessage.Msg<>32) and (AMessage.Msg<>132) and (AMessage.Msg<>512) then FList.Items.Append(IntToStr(AMessage.Msg));
End;

Procedure TVMenu.SelfDestroy;
Begin
 FHMenu.DestroyV;
 Free;
End;

Procedure TVMenu.Paint;
Begin
 PaintB;
End;

Procedure TVMenu.MouseMove ( AShift : TShiftState; AX, AY : Integer );
Begin
 Inherited;
 GetSelIndex(AX,AY);
End;

Procedure TVMenu.GetSelIndex ( AX, AY : Integer );
Var
  BItem         : THItem;

Begin
 BItem:=FCtrlItem.FChild;
 while BItem<>nil do
  begin
  if (BItem.FStart<AY) and (AY<(BItem.FStart+BItem.FSize)) then break;
  BItem:=BItem.FNext;
  end;

 if FSelItem<>BItem then
  begin
  FSelItem:=BItem;
  PaintB;
  end;

End;

Procedure TVMenu.SetCtrlItem ( AItem : THItem );
Var
  BItem         : THItem;
  BTop,
  BWidth,
  BWidthA,
  BHeightA      : Integer;

Begin
 FSelItem:=nil;
 FCtrlItem:=AItem;

 BTop:=2; BWidth:=0;

 BItem:=FCtrlItem.FChild;
 while BItem<>nil do
  begin
  BWidthA:=0; BHeightA:=0;
  case BItem.FStyle of
    isComment:
      begin
      BWidthA:=FBitmap.Canvas.TextWidth(BItem.FCaption);
      BHeightA:=14;
      end;
    isItem:
      begin
      BWidthA:=FBitmap.Canvas.TextWidth(BItem.FCaption);
      BHeightA:=18;
      end;
    isSeparator:
      begin
      BWidthA:=0;
      BHeightA:=3;
      end;
  end; (* case *)
  BItem.FSize:=BHeightA;
  BItem.FStart:=BTop;
  BTop:=BTop+BHeightA;
  if BWidthA>BWidth then BWidth:=BWidthA;

  BItem:=BItem.FNext;
  end;

 FBitmap.PixelFormat:=pf32bit;
 FBitmap.Width:=BWidth+20+8;
 FBitmap.Height:=BTop+2;

 Width:=FBitmap.Width;
 Height:=FBitmap.Height;

End;

Procedure TVMenu.PaintB;
Var
  BItem         : THItem;
  BRect         : TRect;
  BTop          : Integer;
  BTextHeight   : Integer;
  BBitmap       : TBitmap;
  BColorB       : TColor;

Begin
 BBitmap:=TBitmap.Create;

 repeat
 if FCtrlItem=nil then break;
 
 BRect.Top:=0;
 BRect.Left:=0;
 BRect.Right:=FBitmap.Width;
 BRect.Bottom:=FBitmap.Height;
 with FBitmap.Canvas do
  begin
  Brush.Style:=bsSolid;
  Brush.Color:=FBkColor;
  FillRect(BRect);
  Brush.Color:=FFrameColor;
  FrameRect(BRect);
  Pen.Color:=FBkColor;
  Pen.Style:=psSolid;
  MoveTo(1,0); LineTo(FCtrlItem.FSize,0);
  BRect.Top:=1;
  BRect.Left:=1;
  BRect.Right:=FBitmap.Width-1;
  BRect.Bottom:=FBitmap.Height-1;
  Brush.Color:=FBkComColor;
  FrameRect(BRect);
  end;

 BTextHeight:=FBitmap.Canvas.TextHeight('Ayla');

 BTop:=2;
 BItem:=FCtrlItem.FChild;
 while BItem<>nil do
  begin
  BRect.Top:=BTop;
  BRect.Bottom:=BRect.Top+BItem.FSize;

  with FBitmap.Canvas do
   begin
   case BItem.FStyle of
    isComment:
      begin
      BRect.Left:=2;
      BRect.Right:=FBitmap.Width-2;
      Brush.Style:=bsSolid;
      Brush.Color:=FBkComColor;
      FillRect(BRect);
      Brush.Style:=bsClear;
      Font.Color:=clBlack;
      TextOut(22,BTop+((BItem.FSize-BTextHeight) div 2),BItem.FCaption);
      end;

    isItem:
      begin
      if BItem=FSelItem then
       begin
       BRect.Left:=2;
       BRect.Right:=FBitmap.Width-2;
       Brush.Style:=bsSolid;
       BColorB:=$F0E0E0;
       Brush.Color:=BColorB;
       FillRect(BRect);
       Brush.Color:=$F06060;
       FrameRect(BRect);
       end
      else
       begin
       BRect.Left:=2;
       BRect.Right:=20;
       BColorB:=FIconColor;
       Brush.Color:=BColorB;
       Brush.Style:=bsSolid;
       FillRect(BRect);
       end;
      if (BItem.FBitmap.Width=0) or (BItem.FBitmap.Height=0) then
      else
       begin
       BBitmap.Assign(BItem.FBitmap);
       TranspBitmap(BBitmap,BColorB);
       Draw(3,BRect.Top+1,BBitmap);
       end;
      Brush.Style:=bsClear;
      Font.Color:=clBlack;
      TextOut(22,BTop+((BItem.FSize-BTextHeight) div 2),BItem.FCaption);
      end;

    isSeparator:
      begin
      BRect.Left:=2;
      BRect.Right:=20;
      Brush.Color:=FIconColor;
      Brush.Style:=bsSolid;
      FillRect(BRect);

      Pen.Color:=$808080;
      Pen.Style:=psSolid;
      MoveTo(23,BRect.Top+1); LineTo(FBitmap.Width-3,BRect.Top+1);
      end;

   end; (* case *)
  end; (* with *)

  BTop:=BTop+BItem.FSize;
  BItem:=BItem.FNext;
  end;

 Canvas.Draw(0,0,FBitmap);
 until TRUE;
 
 BBitmap.Free;
End;

Procedure TVMenu.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Var
  BOnClick      : TNotifyEvent;
  BData         : TObject;

Begin
 repeat
 if AButton<>mbLeft then break;
 if FSelItem=nil then break;

 BOnClick:=FSelItem.FOnClick;
 BData:=FSelItem.FData;
 SelfDestroy;
 if Assigned(BOnClick) then BOnClick(BData);
 until TRUE;
End;

Procedure TVMenu.MouseUp ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer );
Begin
End;

end.

