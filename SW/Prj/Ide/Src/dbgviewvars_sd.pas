unit DbgViewVars_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, DbgInfo_sd, ComCtrls, AsmTypes_sd, ParsHelper_sd, ConComI;

Type
  TDrawPos = record
    FPosX,
    FPosY   : Integer;
  end;

  TWndVarsSd = class(TCustomControl)
  private
    FPrjParams  : TStringList;
    FProcName   : string;
    FListAll,
    FListRes,
    FListPar,
    FListVar    : TStringList;
    FViewBmp,
    FListBmp    : TBitmap;
    FLineThis   : Integer;
    FLineStartV : Integer; // First visible line
    FLeftPosH   : Integer;
    FScrollU,
    FScrollD    : TScrollBtn;
    FScrollV    : TScrollBarV;
    FScrollL,
    FScrollR    : TScrollBtn;
    FScrollH    : TScrollBarH;

    FTextHeight,
    FSymbolWidth        : Integer;
    FIndent             : Integer;
    FLineCnt,
    FMaxLen             : Integer;

    FParent           : TTabSheet;

    FIconList    : array [0..15] of TBitmap;

    Procedure ClearA;

    Procedure ScrollMovedV ( AStart : Integer );
    Procedure ScrollMovedH ( AStart : Integer );
    Procedure ScrollUpdate;

    Function GetLineBmpWidth : Integer;

    Procedure DrawText ( Var ADrawPos : TDrawPos; Const AText : string; AColor : Cardinal; AFontStyle : TFontStyles );
    Procedure DrawIcon ( Var ADrawPos : TDrawPos; AIconIdx : Integer );
    Procedure DrawProc ( Var ADrawPos : TDrawPos );
    Procedure DrawList ( Var ADrawPos : TDrawPos; AListType : char );

  protected
    Procedure Paint; Override;
    Procedure Resize; Override;
    Procedure MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX, AY : Integer ); Override;
    Procedure KeyDown ( Var AKey : Word; AShift : TShiftState ); Override;
    Function DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean; Override;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Function CanFocus : boolean; Override;
    Procedure SetFocus; Override;

    Procedure Init ( AParent : TTabSheet; APrjParams : TStringList );
    Procedure Done;
    Procedure Clear;
    Procedure SetData ( Const AProc : TDbgInfoProc; AList : TStringList );
    Procedure PaintA;
  end;

implementation

Uses
  ConComL;

Function InterpretTypeA ( Const ATypeS : string ) : Integer;
Begin
 Result:=-1;
 if ATypeS='l' then Result:=0
 else if ATypeS='c' then Result:=1
 else if ATypeS='b' then Result:=2
 else if ATypeS='w' then Result:=3
 else if ATypeS='d' then Result:=4
 else if ATypeS='k' then Result:=5
 else if ATypeS='m' then Result:=6
 else if ATypeS='i' then Result:=7
 else if ATypeS='f' then Result:=8
 else if ParsIsTypeArray(ATypeS) then Result:=9
 else if ParsIsTypeRecord(ATypeS) then Result:=10
 else if ParsIsTypeStringP(ATypeS) then Result:=11;
End;

{ TWndVarsSd }

Const
  CIconNames : array [0..15] of string =
   (
    'Gear',
    'Res',
    'Par',
    'Var',
    'Bool',
    'Char',
    'u8',
    'u16',
    'u32',
    'i8',
    'i16',
    'i32',
    'Float',
    'Array',
    'Record',
    'Str'
   );

Constructor TWndVarsSd.Create ( AOwner : TComponent );
Var
  BIconIdx  : Integer;
Begin
 Inherited;

 FListAll:=TStringList.Create;
 FListRes:=TStringList.Create;
 FListPar:=TStringList.Create;
 FListVar:=TStringList.Create;

 FScrollU:=TScrollBtn.Create(Self); InsertControl(FScrollU); FScrollU.BtnKind:=bkUp;
 FScrollD:=TScrollBtn.Create(Self); InsertControl(FScrollD); FScrollD.BtnKind:=bkDn;

 FScrollV:=TScrollBarV.Create(Self); InsertControl(FScrollV);
 FScrollV.OnMoved:=@ScrollMovedV;

 FScrollL:=TScrollBtn.Create(Self); InsertControl(FScrollL); FScrollL.BtnKind:=bkLeft;
 FScrollR:=TScrollBtn.Create(Self); InsertControl(FScrollR); FScrollR.BtnKind:=bkRight;

 FScrollH:=TScrollBarH.Create(Self); InsertControl(FScrollH);
 FScrollH.OnMoved:=@ScrollMovedH;

 FViewBmp:=TBitmap.Create;
 FListBmp:=TBitmap.Create;

 //FViewBmp.PixelFormat:=pf32bit;
 FViewBmp.Canvas.Font.Name:='Courier New';
 FViewBmp.Canvas.Font.Size:=10;
 FViewBmp.Canvas.Font.Color:=0;
 FTextHeight:=FViewBmp.Canvas.TextHeight('Ayla');
 FSymbolWidth:=FViewBmp.Canvas.TextWidth('0');
 FIndent:=40;

 FListBmp.Canvas.Font.Assign(FViewBmp.Canvas.Font);

 for BIconIdx:=0 to Length(FIconList)-1 do
  begin
  FIconList[BIconIdx]:=TBitmap.Create;
  FIconList[BIconIdx].LoadFromLazarusResource(CIconNames[BIconIdx]);
  end;

End;

Destructor TWndVarsSd.Destroy;
Var
  BIconIdx  : Integer;
Begin
 for BIconIdx:=0 to Length(FIconList)-1 do
  begin
  FIconList[BIconIdx].Free;
  end;

 FListBmp.Free;
 FViewBmp.Free;

 RemoveControl(FScrollH); FScrollH.Free;
 RemoveControl(FScrollR); FScrollR.Free;
 RemoveControl(FScrollL); FScrollL.Free;

 RemoveControl(FScrollV); FScrollV.Free;
 RemoveControl(FScrollD); FScrollD.Free;
 RemoveControl(FScrollU); FScrollU.Free;

 FListVar.Free;
 FListPar.Free;
 FListRes.Free;
 FListAll.Free;
 Inherited;
End;

Procedure TWndVarsSd.Init ( AParent : TTabSheet; APrjParams : TStringList );
Begin
 FParent:=AParent;
 FParent.InsertControl(Self);
 FPrjParams:=APrjParams;
 Align:=alClient; Visible:=TRUE;
 Clear;
End;

Procedure TWndVarsSd.Done;
Begin
 FParent.RemoveControl(Self);
End;

Procedure TWndVarsSd.ClearA;
Begin
 FProcName:='';
 FLineStartV:=0; FLeftPosH:=0;
 FListAll.Clear;
 FListRes.Clear; FListPar.Clear; FListVar.Clear;
 FMaxLen:=0; FLineCnt:=0;
End;

Procedure TWndVarsSd.Clear;
Begin
 ClearA;
 if FParent<>nil then FParent.Caption:='Local vars (0)';
End;

Function TWndVarsSd.CanFocus : boolean;
Begin
 Inherited;
 Result:=TRUE;
End;

Procedure TWndVarsSd.SetFocus;
Begin
 Inherited;
 ScrollUpdate;
End;

Procedure TWndVarsSd.MouseDown ( AButton : TMouseButton; AShift : TShiftState; AX,AY : Integer );
Var
  BLineThis     : Integer;
  BPaint        : boolean;
Begin
 Inherited;
 SetFocus;
 BPaint:=FALSE;
 BLineThis:=FLineStartV+(AY div FTextHeight);
 if BLineThis<>FLineThis then
  begin
  FLineThis:=BLineThis;
  BPaint:=TRUE;
  end;

 if BPaint then PaintA;
End;

Procedure TWndVarsSd.KeyDown ( Var AKey : Word; AShift : TShiftState );
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
     if (FLineThis+1)<FLineCnt then
      begin
      Inc(FLineThis);
      BPaint:=TRUE;
      end;
     end;

   VK_PRIOR:
     begin
     BLineCountV:=FListBmp.Height div FTextHeight;
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
     BLineCountV:=FListBmp.Height div FTextHeight;
     BLineThis:=FLineThis+BLineCountV;
     BLineStartV:=FLineStartV+BLineCountV;
     if BLineThis>=FLineCnt then BLineThis:=FLineCnt-1;
     if BLineStartV>=FLineCnt then BLineStartV:=FLineCnt-1;
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
  BLineCntV:=FListBmp.Height div FTextHeight;
  if FLineThis>=FLineStartV+BLineCntV then FLineStartV:=FLineThis-BLineCntV+1;
  ScrollUpdate;
  PaintA;
  end;

End;

Function TWndVarsSd.DoMouseWheel ( AShift : TShiftState; AWheelDelta : Integer; AMousePos : TPoint ) : Boolean;
Var
  BLineStartV   : Integer;
  BWheelDelta   : Integer;
Begin
 Inherited;

 if AWheelDelta<0 then BWheelDelta:=1
 else if AWheelDelta>0 then BWheelDelta:=-1
 else BWheelDelta:=0;

 BLineStartV:=FLineStartV+BWheelDelta;
 if BLineStartV>=FLineCnt then BLineStartV:=FLineCnt-1;
 if BLineStartV<0 then BLineStartV:=0;
 if BLineStartV<>FLineStartV then
  begin
  FLineStartV:=BLineStartV;
  ScrollUpdate;
  PaintA;
  end;

 Result:=TRUE;
End;

Procedure TWndVarsSd.ScrollMovedV ( AStart : Integer );
Begin
 FLineStartV:=FScrollV.LineTop;
 PaintA;
End;

Procedure TWndVarsSd.ScrollMovedH ( AStart : Integer );
Begin
 FLeftPosH:=FScrollH.PosLeft;
 PaintA;
End;

Procedure TWndVarsSd.Paint;
Begin
 Inherited;
 BmpBox(Canvas,0,0,Width,Height,$808080,$FFFFFF,$404040,$808080);

 Canvas.Brush.Style:=bsSolid;
 Canvas.Brush.Color:=CColorBg;
 Canvas.FillRect(Width-2-FScrollU.Left,Height-2-FScrollL.Height,Width-2,Height-2);

 PaintA;
End;

Procedure TWndVarsSd.DrawText ( Var ADrawPos : TDrawPos; Const AText : string; AColor : Cardinal; AFontStyle : TFontStyles );
Begin
 FListBmp.Canvas.Brush.Style:=bsClear;
 FListBmp.Canvas.Font.Color:=AColor;
 FListBmp.Canvas.Font.Style:=AFontStyle;
 FListBmp.Canvas.TextOut(ADrawPos.FPosX,ADrawPos.FPosY,AText);
 ADrawPos.FPosX:=ADrawPos.FPosX+FSymbolWidth*Length(AText);
End;

Procedure TWndVarsSd.DrawIcon ( Var ADrawPos : TDrawPos; AIconIdx : Integer );
Var
  BIcon     : TBitmap;
Begin
 BIcon:=FIconList[AIconIdx];
 FListBmp.Canvas.Draw(ADrawPos.FPosX,ADrawPos.FPosY+((FTextHeight-BIcon.Height) div 2),BIcon);
 ADrawPos.FPosX:=ADrawPos.FPosX+BIcon.Width+4;
End;

Procedure TWndVarsSd.DrawProc ( Var ADrawPos : TDrawPos );
Var
  BReadS    : string;
  BItem     : string;
  BItemL    : string;
Begin
 DrawIcon(ADrawPos,0);
 BReadS:=FProcName;
 repeat
 // Proc/Function
 BItem:=ReadParamStr(BReadS);
 if BItem='' then break;
 DrawText(ADrawPos,BItem,$000000,[fsBold]); Inc(ADrawPos.FPosX,FSymbolWidth);
 // Name
 BItem:=ReadParamStr(BReadS);
 if BItem='' then break;
 DrawText(ADrawPos,BItem,$000000,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
 // Params
 BItem:=ReadParamStr(BReadS);
 if BItem='(' then
  begin
  DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  repeat
  BItem:=ReadParamStr(BReadS);
  if BItem='' then break;
  if BItem=')' then begin DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth); break; end;
  BItemL:=LowerCase(BItem);
  if (BItemL='const') or (BItemL='var') then begin DrawText(ADrawPos,BItem,$000000,[fsBold]); Inc(ADrawPos.FPosX,FSymbolWidth); BItem:=ReadParamStr(BReadS); end;
  if BItem='' then break;
  DrawText(ADrawPos,BItem,$000000,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  BItem:=ReadParamStr(BReadS);
  if BItem='' then break;
  DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  BItem:=ReadParamStr(BReadS);
  if BItem='' then break;
  if BItem[Length(BItem)]=';' then
   begin
   Delete(BItem,Length(BItem),1);
   DrawText(ADrawPos,BItem,$000000,[]); DrawText(ADrawPos,';',$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
   end
  else
   begin
   DrawText(ADrawPos,BItem,$000000,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
   end;
  until FALSE;
  end
 else
  begin
  DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  end;
 BItem:=ReadParamStr(BReadS);
 if BItem='' then break;
 if BItem=';' then begin DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth); break; end;
 if BItem=':' then
  begin
  DrawText(ADrawPos,BItem,$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  BItem:=ReadParamStr(BReadS);
  if BItem='' then break;
  end;
 if BItem[Length(BItem)]=';' then
  begin
  Delete(BItem,Length(BItem),1);
  DrawText(ADrawPos,BItem,$000000,[]); DrawText(ADrawPos,';',$0000FF,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  end
 else
  begin
  DrawText(ADrawPos,BItem,$000000,[]); Inc(ADrawPos.FPosX,FSymbolWidth);
  end;
 until TRUE;
 ADrawPos.FPosY:=ADrawPos.FPosY+FTextHeight;
End;

Procedure TWndVarsSd.DrawList ( Var ADrawPos : TDrawPos; AListType : char );
Var
  BList     : TStringList;
  BIconIdx  : Integer;
  BText     : string;
  BDrawPos  : TDrawPos;
  BLineIdx  : Integer;
  BReadS    : string;
  BTarg,
  BRdblLoc,
  BRdblVal  : string;
  BTypeS,
  BNameS    : string;
  BTypeIdx  : Integer;
Begin
 repeat
 case AListType of
   'r': begin BList:=FListRes; BIconIdx:=1; BText:='Return value'; end;
   'p': begin BList:=FListPar; BIconIdx:=2; BText:='Parameters'; end;
   'v': begin BList:=FListVar; BIconIdx:=3; BText:='Local variables'; end;
   else break;
 end;
 BDrawPos:=ADrawPos;
 DrawIcon(BDrawPos,BIconIdx);
 DrawText(BDrawPos,BText,$F00000,[]);
 ADrawPos.FPosY:=ADrawPos.FPosY+FTextHeight;
 BLineIdx:=0;
 while BLineIdx<BList.Count do
  begin
  BDrawPos.FPosY:=ADrawPos.FPosY; BDrawPos.FPosX:=ADrawPos.FPosX+FIndent;
  BReadS:=BList.Strings[BLineIdx];
  BTarg:=ReadParamStr(BReadS);
  BRdblLoc:=ReadTillC(BReadS,'#');
  BRdblVal:=BReadS;
  repeat
  if BTarg='' then break;
  BTypeS:=ParsExtractType(BTarg);
  BNameS:=ParsExtractName(BTarg);
  // Type icon
  BTypeIdx:=InterpretTypeA(BTypeS);
  if BTypeIdx>=0 then DrawIcon(BDrawPos,4+BTypeIdx);
  // Name
  DrawText(BDrawPos,BNameS,$000000,[]);
  inc(BDrawPos.FPosX,FSymbolWidth);
  // Location
  if BRdblLoc='' then break;
  if BRdblLoc[1]='[' then DrawText(BDrawPos,BRdblLoc,$FF8080,[])
  else DrawText(BDrawPos,UpperCase(BRdblLoc),$6000C0,[fsBold]);
  inc(BDrawPos.FPosX,FSymbolWidth);
  // Value
  if BRdblVal='' then break;
  DrawText(BDrawPos,BRdblVal,$808080,[]);
  until TRUE;
  ADrawPos.FPosY:=ADrawPos.FPosY+FTextHeight;
  inc(BLineIdx);
  end;
 until TRUE;
End;

Function TWndVarsSd.GetLineBmpWidth : Integer;
Begin
 Result:=25+FMaxLen*FSymbolWidth;
End;

Procedure TWndVarsSd.PaintA;
Var
  BDrawPos  : TDrawPos;
Begin
 FViewBmp.SetSize(Width-4-FScrollU.Width,Height-4-FScrollL.Height);
 BmpBox(FViewBmp,$FFFFFF);
 FListBmp.SetSize(GetLineBmpWidth,FLineCnt*FTextHeight);
 BmpBox(FListBmp,$FFFFFF);

 BDrawPos.FPosY:=0; BDrawPos.FPosX:=0;
 DrawProc(BDrawPos);

 BDrawPos.FPosX:=FIndent;
 DrawList(BDrawPos,'r');
 BDrawPos.FPosX:=FIndent;
 DrawList(BDrawPos,'p');
 BDrawPos.FPosX:=FIndent;
 DrawList(BDrawPos,'v');

 FViewBmp.Canvas.Draw(-FLeftPosH,FTextHeight*(0-FLineStartV),FListBmp);

 Canvas.Draw(2,2,FViewBmp);
End;

Procedure TWndVarsSd.Resize;
Begin
 Inherited;

 FScrollU.Top:=2; FScrollU.Left:=Width-FScrollU.Width-2;
 FScrollD.Top:=Height-FScrollL.Height-FScrollD.Height-2; FScrollD.Left:=FScrollU.Left;
 FScrollV.Left:=FScrollU.Left; FScrollV.Top:=FScrollU.Top+FScrollU.Height; FScrollV.Height:=FScrollD.Top-FScrollV.Top;;

 FScrollL.Top:=Height-FScrollU.Height-2; FScrollL.Left:=2;
 FScrollR.Top:=FScrollL.Top; FScrollR.Left:=Width-FScrollR.Width-FScrollU.Width-2;
 FScrollH.Top:=FScrollL.Top; FScrollH.Left:=FScrollL.Left+FScrollL.Width; FScrollH.Width:=FScrollR.Left-FScrollH.Left;

 ScrollUpdate;
 PaintA;
End;

Procedure TWndVarsSd.ScrollUpdate;
Begin
 FScrollV.SetPos(FLineStartV,(Height-FScrollL.Height-4) div FTextHeight,FLineCnt);
 FScrollH.SetPos(FLeftPosH,Width-FScrollU.Width-4,GetLineBmpWidth);
End;

Procedure TWndVarsSd.SetData ( Const AProc : TDbgInfoProc; AList : TStringList );
Var
  BIdxAll   : Integer;
  BOrigS,
  BReadS    : string;
  BTarg     : string;
  BSpec     : string;
  BLenThis  : Integer;
  BItemCnt  : Integer;
Begin
 ClearA;

 FProcName:='';
 if AProc<>nil then FProcName:=AProc.ReadableHdr;

 FListAll.Assign(AList);

 BItemCnt:=0;

 repeat
 if FProcname='' then break;
 FMaxLen:=Length(FProcName);
 if FMaxLen<16 then FMaxLen:=20; // Length('Local variables') + some icon sizes
 BIdxAll:=0;
 while BIdxAll<FListAll.Count do
  begin
  BOrigS:=FListAll.Strings[BIdxAll];
  BReadS:=BOrigS;
  BTarg:=ReadParamStr(BReadS);
  repeat
  BSpec:=ParsExtractSpec(BTarg);
  if Length(BSpec)<2 then break;
  if ParsIsSpecResult(BSpec) then FListRes.Append(BOrigS)
  else if ParsIsSpecParam(BSpec) then FListPar.Append(BOrigS)
  else if ParsIsSpecLocal(BSpec) then FListVar.Append(BOrigS)
  else break;
  BLenThis:=Length(BOrigS)+2;
  if BLenThis>FMaxLen then FMaxLen:=BLenThis;
  until TRUE;
  Inc(BIdxAll);
  end;

 BItemCnt:=FListRes.Count+FListPar.Count+FListVar.Count;

 FLineCnt:=4+BItemCnt;
 until TRUE;

 if FParent<>nil then FParent.Caption:='Local vars ('+IntToStr(BItemCnt)+')';

 PaintA;
End;

Initialization
  {$I dbgviewvars.lrs}

end.

