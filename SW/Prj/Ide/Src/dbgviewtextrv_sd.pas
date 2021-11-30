unit DbgViewTextRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources, Controls, ExtCtrls, Forms,
  Buttons, LCLType, ConComL, AsmTypes_sd, DbgBase_sd, DbgViewBase_sd, DbgViewText_sd, AsmHelper_sd;

Type
  TDbgViewTextRv = class(TDbgViewText)
  protected
    Procedure ViewLine ( ABitmap : TBitmap; ATop : Integer; Const ALine : string ); Override;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

//Const
//  CCmdNamesRV = 'swt ecall eret add sub sll slt sltu xor srl sra or and addi slti sltiu xori ori andi slli srli srai jal jalr lui beq bne blt bge bltu bgeu mul mulh mulhsu mulhu div divu rem remu amoswap amoadd amoxor amoand amoor';
//  CCmdPseudoRV = 'ret lb lh lw lbu lhu sb sh sw jr call li la mv j beqz bnez bltz bgez blez bgtu ble trap nop snez sgtz sltz seqz';

implementation

Uses
  AsmMcuRV_sd;

{ TDbgViewTextRv }

Constructor TDbgViewTextRv.Create ( AOwner : TComponent );
Begin
 Inherited;
 FCpuType:='e';
End;

Destructor TDbgViewTextRv.Destroy;
Begin
 Inherited;
End;

Const
  CSymbols = '[]{}()<>",.?/;:+-='+#39;
  //CResWords = CRegNamesRVA+' '+CRegNamesRVB+' '+CCmdNamesRV+' '+CCmdPseudoRV;

Function IsLinePlayed ( Const ATypeS : string; AColor : Cardinal ) : Cardinal;
Begin
 if ATypeS[3]='*' then Result:=AColor
 else Result:=$808080+((AColor and $FCFCFC) shr 2);
End;

Procedure TDbgViewTextRv.ViewLine ( ABitmap : TBitmap; ATop : Integer; Const ALine : String );
Var
  BCanvas       : TCanvas;
  BTypeS,
  BAddrS,
  BDataS,
  BOrigS,
  BCommentS     : string;
  BPos          : Integer;
  BSegType      : char;
  BReadS        : string;
  BTokenPosS,
  BTokenPosE    : Integer;
  BTokenS       : string;
  BColor        : Cardinal;
Begin
 BCanvas:=ABitmap.Canvas;

 SplitLineLst(ALine,BTypeS,BAddrS,BDataS,BOrigS,BSegType);

 case BSegType of
   'c': BColor:=$000000;
   'd': BColor:=$C00000;
   else BColor:=$808080;
 end; // case

 if BAddrS<>'' then
  begin
  if BTypeS[3]<>'*' then BAddrS:='XXXX';
  while Length(BAddrS)<6 do BAddrS:=' '+BAddrS;
  BCanvas.Font.Color:=IsLinePlayed(BTypeS,BColor);
  BCanvas.TextOut(FLinePosA,ATop,BAddrS);
  end;

 BCommentS:='';
 BPos:=Pos(';',BOrigS);
 if BPos<>0 then
  begin
  BCommentS:=Copy(BOrigS,BPos,Length(BOrigS)-BPos+1);
  BOrigS:=Copy(BOrigS,1,BPos-1);
  end;

 repeat
 BReadS:=BOrigS;
 if Length(BTypeS)<3 then begin BCanvas.Font.Color:=$000000; BCanvas.TextOut(FLinePosC,ATop,BOrigS); break; end;
 // isoadlpe=
 case BTypeS[2] of
   'i': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if LowerCase(BTokenS)='#include' then begin BCanvas.Font.Style:=[]; BColor:=$C060C0; end
        else begin BCanvas.Font.Style:=[]; BColor:=$C04000; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'j': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if LowerCase(BTokenS)='#IncData' then begin BCanvas.Font.Style:=[]; BColor:=$C060C0; end
        else begin BCanvas.Font.Style:=[]; BColor:=$C04000; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   's': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if StrInList(LowerCase(BTokenS),'.seg') then begin BCanvas.Font.Style:=[]; BColor:=$C06000; end
        else begin BCanvas.Font.Style:=[]; BColor:=$404000; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'o': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if LowerCase(BTokenS)='.org' then begin BCanvas.Font.Style:=[]; BColor:=$C06000; end
        else begin BCanvas.Font.Style:=[]; BColor:=$604000; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   '=': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if LowerCase(BTokenS)='equ' then begin BCanvas.Font.Style:=[]; BColor:=$006040; end
        else begin BCanvas.Font.Style:=[]; BColor:=$404040; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'a': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if LowerCase(BTokenS)='align' then begin BCanvas.Font.Style:=[]; BColor:=$C06000; end
        else begin BCanvas.Font.Style:=[]; BColor:=$404000; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'p', 'e':
        begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,' ');
        if BTokenS='' then break;
        if StrInList(LowerCase(BTokenS),'public extern') then begin BCanvas.Font.Style:=[]; BColor:=$0000000; end
        else begin BCanvas.Font.Style:=[]; BColor:=$404040; end;
        BCanvas.Font.Color:=BColor;
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'l': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,CSymbols);
        if BTokenS='' then break;
        if (Length(BTokenS)=1) and (Pos(BTokenS[1],CSymbols)<>0) then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$0000F0;
         end
        else
         begin
         BCanvas.Font.Style:=[];
         BColor:=$604000;
         end;
        BCanvas.Font.Color:=IsLinePlayed(BTypeS,BColor);
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'd': begin
        ViewBinAsData(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,CSymbols);
        if BTokenS<>'' then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$0000F0;
         end;
        BCanvas.Font.Color:=IsLinePlayed(BTypeS,BColor);
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,CSymbols);
        if BTokenS='' then break;
        if (Length(BTokenS)=1) and (Pos(BTokenS[1],CSymbols)<>0) then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$0000F0;
         end
        else if BTokenS[1] in ['0'..'9'] then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$008000;
         end
        else
         begin
         BCanvas.Font.Style:=[];
         BColor:=$404040;
         end;
        BCanvas.Font.Color:=IsLinePlayed(BTypeS,BColor);
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   'x': begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BTokenPosS:=0; BTokenPosE:=0;
        repeat
        BTokenS:=ReadTokenS(BReadS,BTokenPosS,BTokenPosE,CSymbols);
        if BTokenS='' then break;
        if (Length(BTokenS)=1) and (Pos(BTokenS[1],CSymbols)<>0) then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$0000F0;
         end
        else if BTokenS[1] in ['0'..'9'] then
         begin
         BCanvas.Font.Style:=[];
         BColor:=$008000;
         end
        else if StrInList(BTokenS,CRegNamesRVA+' '+CRegNamesRVB) then // CResWords = CRegNamesRVA+' '+CRegNamesRVB+' '+CCmdNamesRV+' '+CCmdPseudoRV;
         begin
         BCanvas.Font.Style:=[];
         BColor:=$000000;
         end
        else if StrInList(BTokenS,CCmdNamesRV+' '+CCmdPseudoRV) then // CResWords = CRegNamesRVA+' '+CRegNamesRVB+' '+CCmdNamesRV+' '+CCmdPseudoRV;
         begin
         BCanvas.Font.Style:=[fsBold];
         BColor:=$000000;
         end
        else
         begin
         BCanvas.Font.Style:=[];
         BColor:=$404040;
         end;
        BCanvas.Font.Color:=IsLinePlayed(BTypeS,BColor);
        BCanvas.TextOut(FLinePosC+BTokenPosS*FSymbolWidth,ATop,BTokenS);
        BTokenPosS:=BTokenPosE;
        until FALSE;
        end;
   else begin
        ViewBinAsCmd(BDataS,ATop,BColor);
        BCanvas.Font.Style:=[];
        BCanvas.Font.Color:=$000000;
        BCanvas.TextOut(FLinePosC,ATop,BOrigS);
        end;
 end;
 until TRUE;

 BCanvas.Font.Style:=[];
 if BCommentS<>'' then
  begin
  BCanvas.Font.Color:=$808080;
  BCanvas.TextOut(FLinePosC+BCanvas.TextWidth(BOrigS),ATop,BCommentS);
  end;

End;

end.

