unit DbgViewText_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, ConComL, DbgInfo_sd, AsmTypes_sd, DbgViewBase_sd;

Type
  TDbgViewText = class(TDbgViewBase)
  protected
    FCpuType    : char;

    Procedure LineChange; Override;
    Function IsLineHighlight ( ALineIdx : Integer ) : boolean; Override;

    Procedure ViewBinAsCmd ( Const ADataS : string; ATop : Integer; AColor : Cardinal );
    Procedure ViewBinAsData ( Const ADataS : string; ATop : Integer; AColor : Cardinal );
    Procedure ViewLine ( ABitmap : TBitmap; ATop : Integer; Const ALine : string ); Virtual; Abstract;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure PaintA; Override;

    Procedure SetList ( Const AFullName : string; AList : TStringList; ALineStart, ALineEnd : Integer; Const ABreakListS : string; AIsLst : boolean );
    Function IsInside ( ALineIndex : Integer ) : boolean;
    Function SetLineIp ( ALineIndex : Integer ) : boolean;
    Function SetLineHighlight ( ALineIndex : Integer ) : boolean;

    Procedure SetDbgHighlight ( Const AFilename : string; ASrcLineIdx, AAsmLineIdx : Integer );

    property CpuType : char read FCpuType;
  end;

implementation

Procedure InvertCmd ( Var ACmd : string );
Var
  BCmd          : string;
  BIndex        : Integer;
Begin
 BCmd:='';
 BIndex:=0;
 while BIndex<Length(ACmd) do
  begin
  BCmd:=Copy(ACmd,BIndex+1,2)+BCmd;
  inc(BIndex,2);
  end;
 while BIndex<12 do begin BCmd:=' '+BCmd; inc(BIndex); end;
 ACmd:=BCmd;
End;

{ TDbgViewText }

Constructor TDbgViewText.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TDbgViewText.Destroy;
Begin
 Inherited;
End;

Procedure TDbgViewText.LineChange;
Begin
 FHighlightFilename:='';
 FHighlightAsmLine:=-1;
 FHighlightSrcLine:=-1;
 Inherited;
End;

Procedure TDbgViewText.SetList ( Const AFullName : string; AList : TStringList; ALineStart, ALineEnd : Integer; Const ABreakListS : string; AIsLst : boolean );
Var
  BBreakListS   : string;
  BIndex        : Integer;
  BBreakIdxS    : string;
  BBreakIdx     : Cardinal;
Begin
 FList:=AList;
 FFullName:=AFullName;
 FLineStart:=ALineStart;
 FLineEnd:=ALineEnd;
 FLineThis:=FLineStart;
 FLineStartV:=FLineStart;
 FLineIp:=-1;
 FBreakList:=nil;

 BBreakIdx:=0; // Compiler satisfaction
 BBreakListS:=ABreakListS;
 BIndex:=0;
 while BBreakListS<>'' do
  begin
  BBreakIdxS:=ReadParamStr(BBreakListS);
  if BBreakIdxS='' then break;
  if StringToCardinal(BBreakIdxS,BBreakIdx)=FALSE then break;
  SetLength(FBreakList,BIndex+1); FBreakList[BIndex]:=BBreakIdx;
  inc(BIndex);
  end;

 FDbgFile.IsLst:=AIsLst;
 FDbgFile.ParseLst(FFullName,FList,FLineStart,FLineEnd);

 ScrollUpdate;
 PaintA;
End;

Function TDbgViewText.IsInside ( ALineIndex : Integer ) : boolean;
Begin
 Result:=(FLineStart<=ALineIndex) and (ALineIndex<FLineEnd);
End;

Function TDbgViewText.SetLineIp ( ALineIndex : Integer ) : boolean;
Var
  BLineCntV     : Integer;
Begin
 Result:=(FLineStart<=ALineIndex) and (ALineIndex<FLineEnd);
 if Result then
  begin
  FLineThis:=ALineIndex;
  FLineIp:=ALineIndex;
  if FLineThis<FLineStartV then FLineStartV:=FLineThis;
  BLineCntV:=FTextBitmap.Height div FLineHeight;
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

Function TDbgViewText.SetLineHighlight ( ALineIndex : Integer ) : boolean;
Var
  BLineCntV     : Integer;
Begin
 Result:=(FLineStart<=ALineIndex) and (ALineIndex<FLineEnd);
 if Result then
  begin
  FLineThis:=ALineIndex;
  if FLineThis<FLineStartV then FLineStartV:=FLineThis;
  BLineCntV:=FTextBitmap.Height div FLineHeight;
  if FLineThis>=FLineStartV+BLineCntV then FLineStartV:=FLineThis-BLineCntV+1;
  //LineChange;
  PaintA;
  end
 else
  begin
  if FLineIp<>-1 then begin FLineIp:=-1; PaintA; end;
  end;
End;

Procedure TDbgViewText.SetDbgHighlight ( Const AFilename : string; ASrcLineIdx, AAsmLineIdx : Integer );
Var
  BDirty        : boolean;
Begin
 BDirty:=FALSE;
 if FHighlightFilename<>AFilename then begin FHighlightFilename:=AFilename; BDirty:=TRUE; end;
 if FHighlightSrcLine<>ASrcLineIdx then begin FHighlightSrcLine:=ASrcLineIdx; BDirty:=TRUE; end;
 if FHighlightAsmLine<>AAsmLineIdx then begin FHighlightAsmLine:=AAsmLineIdx; BDirty:=TRUE; end;
 if (BDirty=FALSE) and (FPaintFocused<>Focused) then BDirty:=TRUE;
 if BDirty then PaintA;
End;

Function TDbgViewText.IsLineHighlight ( ALineIdx : Integer ) : boolean;
Var
  BDbgLine      : TDbgInfoLine;
Begin
 Result:=FALSE;
 repeat
 BDbgLine:=FDbgFile.GetLine(ALineIdx); if BDbgLine=nil then break;
 if FHighlightAsmLine<>-1 then
  begin
  Result:=(BDbgLine.AsmIdxL=FHighlightAsmLine) and (BDbgLine.AsmFile=FHighlightFilename);
  end
 else if FHighlightSrcLine<>-1 then
  begin
  Result:=(BDbgLine.SrcIdxL=FHighlightSrcLine) and (BDbgLine.SrcFile=FHighlightFilename);
  end
 else
  begin
  Result:=(ALineIdx=FLineThis) and FPaintFocused;
  end;
 until TRUE;
End;

Function IsLinePlayed ( Const ATypeS : string; AColor : Cardinal ) : Cardinal;
Begin
 if ATypeS[3]='*' then Result:=AColor
 else Result:=$808080+((AColor and $FCFCFC) shr 2);
End;

Procedure TDbgViewText.ViewBinAsCmd ( Const ADataS : string; ATop : Integer; AColor : Cardinal );
Var
  BDataS        : string;
Begin
 BDataS:=ADataS;
 InvertCmd(BDataS);
 if Length(BDataS)>12 then BDataS:=Copy(BDataS,1,11)+'>'
 else AddSpacesVarR(BDataS,12);
 FTextBitmap.Canvas.Font.Style:=[];
 FTextBitmap.Canvas.Font.Color:=AColor;
 FTextBitmap.Canvas.TextOut(FLinePosB+FSymbolWidth*8+2,ATop,Copy(BDataS,9,4));
 FTextBitmap.Canvas.Font.Color:=$C06000;
 FTextBitmap.Canvas.TextOut(FLinePosB,ATop,Copy(BDataS,1,8));
End;

Procedure TDbgViewText.ViewBinAsData ( Const ADataS : string; ATop : Integer; AColor : Cardinal );
Var
  BDataS        : string;
Begin
 BDataS:=ADataS;
 if Length(BDataS)>12 then BDataS:=Copy(BDataS,1,11)+'>'
 else AddSpacesVarR(BDataS,12);
 FTextBitmap.Canvas.Font.Style:=[];
 FTextBitmap.Canvas.Font.Color:=AColor;
 FTextBitmap.Canvas.TextOut(FLinePosB,ATop,BDataS);
End;

Procedure TDbgViewText.PaintA;
Var
  BLineCntV     : Integer; // Visible lines count
  BIndex,
  BIndexA       : Integer;
  BPosY         : Integer;
  BWidthA,
  BWidthB       : Integer;
  BColorA,
  BColorB       : TColor;
  BDbgLine      : TDbgInfoLine;
  BHighlight    : boolean;
Begin
 Inherited;

 FTextBitmap.Canvas.Font.Name:='Courier New';
 FTextBitmap.Canvas.Font.Size:=10;
 FTextBitmap.Canvas.Font.Color:=0;
 FTextBitmap.Canvas.Brush.Style:=bsSolid;
 FTextBitmap.Canvas.Pen.Style:=psSolid;
 BLineCntV:=(FTextBitmap.Height+FLineHeight-1) div FLineHeight;

 BWidthA:=FTextBitmap.Canvas.TextWidth('AAAAAA');
 BWidthB:=FTextBitmap.Canvas.TextWidth('DDDDDDDDDDDD');
 FLinePosA:=16;
 FLinePosB:=FLinePosA+BWidthA+4;
 FLinePosC:=FLinePosB+BWidthB+8;

 FTextBitmap.Canvas.Brush.Color:=FColorANorm;
 FTextBitmap.Canvas.FillRect(0,0,FLinePosC,FTextBitmap.Height);
 FTextBitmap.Canvas.Brush.Color:=FColorBNorm;
 FTextBitmap.Canvas.FillRect(FLinePosC,0,FTextBitmap.Width,FTextBitmap.Height);

 FTextBitmap.Canvas.Pen.Color:=$FFFFFF; FTextBitmap.Canvas.Line(FLinePosB,0,FLinePosB,FTextBitmap.Height);
 FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosB+1,0,FLinePosB+1,FTextBitmap.Height);

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
         FTextBitmap.Canvas.Brush.Style:=bsClear; FTextBitmap.Canvas.Font.Color:=$404040;
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
         FTextBitmap.Canvas.Brush.Color:=BColorA; FTextBitmap.Canvas.FillRect(0,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Color:=BColorB; FTextBitmap.Canvas.FillRect(FLinePosC,BPosY,FTextBitmap.Width,BPosY+FLineHeight);
         FTextBitmap.Canvas.Pen.Color:=$FFFFFF; FTextBitmap.Canvas.Line(FLinePosB,BPosY,FLinePosB,BPosY+FLineHeight);
         FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(FLinePosB+1,BPosY,FLinePosB+1,BPosY+FLineHeight);
         //FTextBitmap.Canvas.Pen.Color:=$808080; FTextBitmap.Canvas.Line(BWidthA+BWidthB,BPosY,BWidthA+BWidthB,BPosY+FLineHeight);
         FTextBitmap.Canvas.Brush.Style:=bsClear;
         ViewLine(FTextBitmap,BPosY,BDbgLine.Readable);
         end;
    end; // case
   end; // else
  if BIndexA=FLineIp then TriangleIp(BPosY);
  if IsLineBreak(BIndexA) then BreakMark(BPosY);
  inc(BPosY,FLineHeight);
  inc(BIndex);
  end;

 DrawRGutter;
 FViewBitmap.Canvas.Draw(2,2,FTextBitmap);

 Canvas.Draw(0,0,FViewBitmap);
End;

end.

