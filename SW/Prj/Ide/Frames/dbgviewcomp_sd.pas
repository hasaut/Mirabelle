unit DbgViewComp_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, DbgInfo_sd, AsmTypes_sd, DbgViewBase_sd;

Type
  TOnLineChange = Procedure ( ADbgInfoLine : TDbgInfoLine ) of object;
  TOnBreakChange = Procedure of object;
  TBreakList = array of Cardinal;

  TDbgViewComp = class(TDbgViewBase)
  private
    FCompLayer          : TCompLayer;
    FHlSrcIdxL          : Integer;
    FHlProcNameL        : string;
    FHlLayerIdxL        : Integer;

    Procedure ViewLine ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );
    Procedure ViewLinePars ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );
    Procedure ViewLineFlow ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );

  protected
    Procedure LineChange; Override;
    Function IsLineHighlight ( ALineIdx : Integer ) : boolean; Override;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Override;

    Procedure SetList ( AList : TStringList );
    Function CheckLineIp ( ALineIndex : Integer ) : boolean;
    Function SetLineIp ( ALineIndex : Integer ) : boolean;

    Procedure SetHlSrc ( ALineIdx : Integer );
    Procedure SetHlCust ( Const AProcNameL : string; ALineIdx : Integer );

    property CompLayer : TCompLayer read FCompLayer write FCompLayer;
  end;

implementation

Uses
  ConComL;

{ TDbgViewComp }

Constructor TDbgViewComp.Create ( AOwner : TComponent );
Begin
 Inherited;
 FLinePosA:=12;
 FLinePosB:=24;
 FLinePosC:=32;
 FHlSrcIdxL:=-1;
 FHlLayerIdxL:=-1;
End;

Destructor TDbgViewComp.Destroy;
Begin
 Inherited;
End;

Procedure TDbgViewComp.SetList ( AList : TStringList );
Begin
 FList:=AList;
 FFullName:='';
 FLineStart:=0;
 FLineEnd:=FList.Count;
 FLineThis:=FLineStart;
 FLineStartV:=FLineStart;
 FLineIp:=-1;
 FBreakList:=nil;

 FDbgFile.IsLst:=FALSE;
 FDbgFile.ParseDbg(FList);

 ScrollUpdate;
End;

Function TDbgViewComp.CheckLineIp ( ALineIndex : Integer ) : boolean;
Begin
 Result:=(FLineStart<=ALineIndex) and (ALineIndex<FLineEnd);
End;

Function TDbgViewComp.SetLineIp ( ALineIndex : Integer ) : boolean;
Var
  BLineCntV     : Integer;
Begin
 Result:=(FLineStart<=ALineIndex) and (ALineIndex<FLineEnd);
 if Result then
  begin
  FLineThis:=ALineIndex;
  FLineIp:=ALineIndex;
  if FLineThis<FLineStartV then FLineStartV:=FLineThis;
  BLineCntV:=FViewBitmap.Height div FLineHeight;
  if FLineThis>=FLineStartV+BLineCntV then FLineStartV:=FLineThis-BLineCntV+1;
  LineChange;
  PaintA;
  end
 else
  begin
  if FLineIp<>-1 then
   begin
   FLineIp:=-1;
   PaintA;
   end;
  end;
End;

Procedure TDbgViewComp.LineChange;
Begin
 FHlSrcIdxL:=-1;
 FHlProcNameL:='';
 FHlLayerIdxL:=-1;
 Inherited;
End;

Procedure TDbgViewComp.SetHlCust ( Const AProcNameL : string; ALineIdx : Integer );
Begin
 repeat
 if (FHlSrcIdxL=-1) and (FHlLayerIdxL=ALineIdx) and (FHlProcNameL=AProcNameL) then break;
 FHlSrcIdxL:=-1;
 FHlProcNameL:=AProcNameL;
 FHlLayerIdxL:=ALineIdx;
 PaintA;
 until TRUE;
End;

Procedure TDbgViewComp.SetHlSrc ( ALineIdx : Integer );
Var
  BResult       : boolean;
Begin
 BResult:=FALSE;
 repeat
 if FHlSrcIdxL<>ALineIdx then begin BResult:=TRUE; break; end;
 if FHlLayerIdxL<>-1 then begin BResult:=TRUE; break; end;
 if FHlProcNameL<>'' then begin BResult:=TRUE; break; end;
 until TRUE;

 if BResult then
  begin
  FHlSrcIdxL:=ALineIdx;
  FHlProcNameL:='';
  FHlLayerIdxL:=-1;
  PaintA;
  end;
End;

Function TDbgViewComp.IsLineHighlight ( ALineIdx : Integer ) : boolean;
Var
  BDbgLine      : TDbgInfoLine;
Begin
 BDbgLine:=FDbgFile.GetLine(ALineIdx);
 Result:=FALSE;
 repeat
 if FHlSrcIdxL<>-1 then begin Result:=BDbgLine.SrcIdxL=FHlSrcIdxL; break; end;
 if FHlLayerIdxL<>-1 then begin Result:=(BDbgLine.LayerIdxL[FCompLayer]=FHlLayerIdxL) and (BDbgLine.ProcNameL=FHlProcNameL); break; end;
 Result:=ALineIdx=FLineThis;
 until TRUE;
End;

Procedure TDbgViewComp.ViewLine ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );
Begin
 if FCompLayer=clPars then ViewLinePars(ARect,ATop,ALine)
 else ViewLineFlow(ARect,ATop,ALine);
End;

Procedure TDbgViewComp.ViewLinePars ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );
Var
  BCanvas       : TCanvas;
  BCmd,
  BTail         : string;
  BLeft         : Integer;
Begin
 BCanvas:=FTextBitmap.Canvas;
 BCanvas.Font.Color:=$000000;
 BTail:=ALine.Readable;
 BCmd:=ReadTillC(BTail,';');
 DelFirstSpace(BCmd); DelLastSpace(BCmd);
 if (BCmd<>'') and (BCmd[Length(BCmd)]=':') then
  begin
  BLeft:=5;
  BCanvas.TextOut(FLinePosC+BLeft*FSymbolWidth,ATop,BCmd);
  end
 else
  begin
  BLeft:=8;
  BCanvas.TextOut(FLinePosC+BLeft*FSymbolWidth,ATop,BCmd);
  end;
End;

Procedure TDbgViewComp.ViewLineFlow ( Const ARect : TRect; ATop : Integer; Const ALine : TDbgInfoLine );
Var
  BCanvas       : TCanvas;
  BCmd,
  BTail         : string;
  BParamA       : string;
  BTokenPosS,
  BTokenPosE    : Integer;
  BTokenS       : string;
  BLeft         : Integer;
  BUseMatr      : string;
  BRect         : TRect;
Begin
 BRect:=ARect;
 BUseMatr:=ALine.UseMatr;
 if BUseMatr<>'' then BRect.Right:=FLinePosC+120*FSymbolWidth;
 BCanvas:=FTextBitmap.Canvas;
 BCanvas.Font.Color:=$000000;
 BTail:=ALine.Readable;
 BCmd:=ReadTillC(BTail,';');
 DelFirstSpace(BCmd); DelLastSpace(BCmd);
 if (BCmd<>'') and (BCmd[Length(BCmd)]=':') then
  begin
  BLeft:=5;
  BCanvas.TextOut(FLinePosC+BLeft*FSymbolWidth,ATop,BCmd);
  end
 else
  begin
  BLeft:=8;
  BParamA:=ReadParamStr(BCmd); DelFirstSpace(BCmd);
  BCanvas.TextOut(FLinePosC+BLeft*FSymbolWidth,ATop,BParamA);
  BTokenPosS:=0; BTokenPosE:=0;
  repeat
  BTokenS:=ReadTokenS(BCmd,BTokenPosS,BTokenPosE,' ');
  if BTokenS='' then break;
  BCanvas.TextOut(FLinePosC+(16+BTokenPosS)*FSymbolWidth,ATop,BTokenS);
  BTokenPosS:=BTokenPosE;
  until FALSE;
  end;
 BCanvas.Font.Color:=$808080;
 //BCanvas.TextRect(BRect,FLinePosC+64*FSymbolWidth,ATop,'; '+BTail);
 if BUseMatr<>'' then
  begin
  BCanvas.Brush.Style:=bsSolid;
  BCanvas.Brush.Color:=FColorBNorm;
  BCanvas.FillRect(BRect.Right-2,ATop,BRect.Right+10,ATop+FLineHeight);
  BCanvas.Brush.Color:=FColorANorm;
  BCanvas.FillRect(BRect.Right,ATop,BRect.Right+8,ATop+FLineHeight);
  BCanvas.Pen.Style:=psSolid;
  BCanvas.Pen.Color:=$FFFFFF; BCanvas.Line(BRect.Right+3,ATop,BRect.Right+3,ATop+FLineHeight);
  BCanvas.Pen.Color:=$808080; BCanvas.Line(BRect.Right+4,ATop,BRect.Right+4,ATop+FLineHeight);
  BCanvas.Brush.Style:=bsClear;
  BCanvas.TextOut(BRect.Right+16,ATop,BUseMatr);
  end;
End;

Procedure TDbgViewComp.PaintA;
Var
  BLineCntV     : Integer; // Visible lines count
  BIndex,
  BIndexA       : Integer;
  BPosY         : Integer;
  BColorA,
  BColorB       : TColor;
  BDbgLine      : TDbgInfoLine;
  BLineNrS      : string;
  BHighlight    : boolean;
  BLineHeight   : Integer;
  BRect         : TRect;
Begin
 Inherited;

 FTextBitmap.Canvas.Font.Name:='Courier New';
 FTextBitmap.Canvas.Font.Size:=10;
 FTextBitmap.Canvas.Font.Color:=0;
 FTextBitmap.Canvas.Brush.Style:=bsSolid;
 FTextBitmap.Canvas.Pen.Style:=psSolid;
 BLineCntV:=(FTextBitmap.Height+FLineHeight-1) div FLineHeight;

 FTextBitmap.Canvas.Brush.Color:=FColorANorm;
 FTextBitmap.Canvas.FillRect(0,0,FLinePosC,FTextBitmap.Height);
 FTextBitmap.Canvas.Brush.Color:=FColorBNorm;
 FTextBitmap.Canvas.FillRect(FLinePosC,0,FTextBitmap.Width,FTextBitmap.Height);

 FTextBitmap.Canvas.Pen.Color:=$FFFFFF;
 FTextBitmap.Canvas.Line(FLinePosA,0,FLinePosA,FTextBitmap.Height);
 FTextBitmap.Canvas.Pen.Color:=$808080;
 FTextBitmap.Canvas.Line(FLinePosA+1,0,FLinePosA+1,FTextBitmap.Height);
 //FTextBitmap.Canvas.Pen.Color:=$808080;
 //FTextBitmap.Canvas.Line(FLinePosC,0,FLinePosC,FTextBitmap.Height);

 BRect.Left:=0; BRect.Top:=0;
 BRect.Right:=FTextBitmap.Width-10; BRect.Bottom:=FTextBitmap.Height;

 BPosY:=0;
 BIndex:=0;
 while BIndex<BLineCntV do
  begin
  BIndexA:=BIndex+FLineStartV;
  if BIndexA>=FLineEnd then break;
  //BDataS:=FList.Strings[BIndexA];
  BDbgLine:=FDbgFile.GetLine(BIndexA);
  if BDbgLine=nil then
  else
   begin
   BHighlight:=IsLineHighlight(BIndexA);
   case BDbgLine.LineType of
      dltHeader:
         begin
         FTextBitmap.Canvas.Brush.Style:=bsSolid;
         if BHighlight then FTextBitmap.Canvas.Brush.Color:=FColorHThis
         else FTextBitmap.Canvas.Brush.Color:=FColorHNorm;
         FTextBitmap.Canvas.FillRect(0,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Style:=bsClear;
         FTextBitmap.Canvas.Font.Color:=$404040;
         FTextBitmap.Canvas.TextOut(3,BPosY,BDbgLine.Readable);
         end;
      dltProc:
         begin
         FTextBitmap.Canvas.Brush.Style:=bsSolid;
         if BHighlight then FTextBitmap.Canvas.Brush.Color:=FColorPThis
         else FTextBitmap.Canvas.Brush.Color:=FColorPNorm;
         FTextBitmap.Canvas.FillRect(0,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Style:=bsClear;
         FTextBitmap.Canvas.Font.Color:=$000000;
         FTextBitmap.Canvas.TextOut(3,BPosY,BDbgLine.Readable);
         end;
      dltAsmLine:
         begin
         FTextBitmap.Canvas.Brush.Style:=bsSolid;
         if BHighlight then begin BColorA:=FColorAThis; BColorB:=FColorBThis; end
         else begin BColorA:=FColorANorm; BColorB:=FColorBNorm; end;
         FTextBitmap.Canvas.Brush.Color:=BColorA; FTextBitmap.Canvas.FillRect(0,BPosY,FLinePosC,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Color:=BColorB; FTextBitmap.Canvas.FillRect(FLinePosC,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
         FTextBitmap.Canvas.Pen.Color:=$FFFFFF; FTextBitmap.Canvas.Line(FLinePosA,BPosY,FLinePosA,BPosY+FLineHeight);
         FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosA+1,BPosY,FLinePosA+1,BPosY+FLineHeight);
         //FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosC,BPosY,FLinePosC,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Style:=bsClear;
         ViewLine(BRect,BPosY,BDbgLine);
         end;
    end; // case
   end; // else
  if BIndexA=FLineIp then TriangleIp(BPosY);
  if IsLineBreak(BIndexA-FLineStart) then BreakMark(BPosY);
  inc(BPosY,FLineHeight);
  inc(BIndex);
  end;

 DrawRGutter;
 FViewBitmap.Canvas.Draw(2,2,FTextBitmap);

 BLineNrS:=IntToStr(FLineThis-FLineStart);
 while Length(BLineNrS)<8 do BLineNrS:=' '+BLineNrS;
 FViewBitmap.Canvas.Font.Name:='Courier New'; FViewBitmap.Canvas.Font.Size:=8; FViewBitmap.Canvas.Font.Color:=0;
 FViewBitmap.Canvas.Brush.Style:=bsClear;
 BLineHeight:=FViewBitmap.Canvas.TextHeight('Ayla');
 FViewBitmap.Canvas.TextOut(3,FViewBitmap.Height{-FBotPartHeight+((FBotPartHeight-BLineHeight) div 2)},BLineNrS);

 Canvas.Draw(0,0,FViewBitmap);
End;

end.

