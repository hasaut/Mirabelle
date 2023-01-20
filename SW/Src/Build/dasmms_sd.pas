unit DasmMS_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DasmBase_sd;

Type
  TMsCmdCode =
   (
    msccInvalid,
    msccARUC,  // |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
    msccBM,    // |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
    msccARRS,  // |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
    msccBC,    // |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
    msccBA,    // |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
    msccMAID,  // |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
    msccMARC,  // |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
    msccONL,   // |10110   ||     4cc|uuuuffff|#2/4             | jmp call 2xRFU  (either REG or # but not both, call REG is not possible)
    msccIDE,   // |10111   ||     cww|0iiirrrr|                 | inc dec
    msccPEX,   // |10111   ||     cww|1000rrrr|                 | pushzx pushsx
    msccAF,    // |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
    msccREM,   // |10111   ||     cww|1010rrrr|                 | rem RFU
    msccPRCS,  // |10111   ||     4cc|10110000|#2/4             | push_# ret_# swt RFU
    msccSys,   // |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # CPush
    msccNTRE,  // |10111   ||     ccc|10110010|                 | nop trap retz # rdml wrml # #
    msccPPL,   // |10111   ||     ccc|10110011|#2               | pushl popl 6xRFU
    msccRFU,   // |10111   ||        |        |
    msccONS,   // |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
    msccFRUC,  // |11101   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
    msccFRRS,  // |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
    msccBTR,   // |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
    msccBTM,   // |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
    msccARUI,  // |11111100||        |uuuurrrr|cccceeww|wwiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd i)
    msccARUS   // |11111101||        |uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)
   );

  TMsTargIdx = (rtA, rtR, rtU, rtS); // R := U + S
  TMsTarg = record
    FReg    : byte;
    FWw     : byte;
  end;

  TMsSubdec = record
    FCmdCode    : TMsCmdCode;
    FCccc       : byte;
    FE          : byte;
    FTarg       : array [TMsTargIdx] of TMsTarg;
    FFfff       : byte;
    FConst      : Cardinal;
    FIiix       : byte; // BT commands only
    FMioSignExt : boolean;
  end;

  TExecLineSD = class(TExecLineBase)
  private
    FBase       : word;
    FExt        : Cardinal;

    FSubdec     : TMsSubdec;

    Function MsProcessExt ( ACmdCode : TMsCmdCode ) : boolean;
    Function ConstDecBx : Cardinal;

    Procedure MsSubdecARUC;
    Procedure MsSubdecBM;
    Procedure MsSubdecARRS;
    Procedure MsSubdecBC;
    Procedure MsSubdecBA;
    Procedure MsSubdecMAID;
    Procedure MsSubdecMARC;
    Procedure MsSubdecONL;
    Procedure MsSubdecIDE;
    Procedure MsSubdecPEX;
    Procedure MsSubdecAF;
    Procedure MsSubdecREM;
    Procedure MsSubdecPRCS;
    Procedure MsSubdecSys;
    Procedure MsSubdecNTRE;
    Procedure MsSubdecPPL;
    Procedure MsSubdecRFU;
    Procedure MsSubdecONS;
    Procedure MsSubdecFRUC;
    Procedure MsSubdecFRRS;
    Procedure MsSubdecBTR;
    Procedure MsSubdecBTM;
    Procedure MsSubdecARUI;
    Procedure MsSubdecARUS;

    Function MsRegName ( ATargIdx : TMsTargIdx ) : string;
    Procedure GenCode ( Const AFormat : string );

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure GetRowCol ( Const ASubdec : TMsSubdec; ATargIdx : TMsTargIdx; Out ARow, ACol : byte );
    Procedure GetRowCol ( ATargIdx : TMsTargIdx; Out ARow, ACol : byte );
    Function CmdDec ( AVirtAddr : Cardinal; Const ACodeBin : string ) : boolean; Override;
    Procedure CheckFixDstLabel; Override;

    Function IsJmp : boolean; Override;
    Function IsJxx : boolean; Override;
    Function IsRet : boolean; Override;
    Function IsCall : boolean; Override;
    Function IsDecStop : boolean; Override;
    Function CallOrJmp : TCallOrJmp; Override;

    property Base : word read FBase;
    property Subdec : TMsSubdec read FSubdec;
  end;

Const
  ZMsSubdec : TMsSubdec =
   (
    FCmdCode:msccInvalid;
    FCccc:0;
    FE: 0;
    FTarg:
     (
      (FReg:0;FWw:0;),
      (FReg:0;FWw:0;),
      (FReg:0;FWw:0;),
      (FReg:0;FWw:0;)
     );
    FFfff:0;
    FConst:0;
    FIiix:0;
    FMioSignExt:FALSE;
   );

implementation

Const
  CExtLenList : array [TMsCmdCode] of byte =
   (
      0, // msccInvalid,
    $24, // msccARUC,  // |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
      0, // msccBM,    // |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
      0, // msccARRS,  // |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
      0, // msccBC,    // |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
      0, // msccBA,    // |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
      0, // msccMAID,  // |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
    $24, // msccMARC,  // |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
    $24, // msccONL,   // |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU  (either REG or # but not both, call REG is not possible)
      0, // msccIDE,   // |10111   ||     cww|0iiirrrr|                 | inc dec
      0, // msccPEX,   // |10111   ||     cww|1000rrrr|                 | pushzx pushsx
      0, // msccAF,    // |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
      0, // msccREM,   // |10111   ||     cww|1010rrrr|                 | rem RFU
    $24, // msccPRCS,  // |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
      0, // msccSys,   // |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
      0, // msccNTRE,  // |10111   ||     ccc|10110010|                 | nop trap 6xRFU
    $24, // msccPPL,   // |10111   ||     ccc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU
      0, // msccRFU,   // |10111   ||        |        |
      0, // msccONS,   // |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
      4, // msccFRUC,  // |11101   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
      0, // msccFRRS,  // |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
      0, // msccBTR,   // |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
    $25, // msccBTM,   // |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
      2, // msccARUI,  // |11111100||        |uuuurrrr|cccceeww|wwiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd i)
      2  // msccARUS   // |11111101||        |uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)
   );

{ *** MsExec *** }

Constructor TExecLineSD.Create;
Begin
 Inherited;
End;

Destructor TExecLineSD.Destroy;
Begin
 Inherited;
End;

Procedure ReplaceKey ( Const ANewS : string; Var ALineS : string; APos : Integer );
Begin
 Delete(ALineS,APos,2);
 Insert(ANewS,ALineS,APos);
End;

{
 al  0 1
 ah  4 2
 ax  1 3
 aw  5 4
 awx 2 7
 ar  6 8
}

Const
  CEncodeCol : array[0..7] of byte = (1, 3, 7, 0, 2, 4, 8, 0);

Procedure TExecLineSD.GetRowCol ( Const ASubdec : TMsSubdec; ATargIdx : TMsTargIdx; Out ARow, ACol : byte );
Begin
 ARow:=ASubdec.FTarg[ATargIdx].FReg and $7;
 ACol:=CEncodeCol[(ASubdec.FTarg[ATargIdx].FWw and $3) or ((ASubdec.FTarg[ATargIdx].FReg shr 1) and $4)];
End;

Procedure TExecLineSD.GetRowCol ( ATargIdx : TMsTargIdx; Out ARow, ACol : byte );
Begin
 GetRowCol(FSubdec,ATargIdx,ARow,ACol);
End;

Const
  CMsRegRowNames    : array [0..7] of string = ('z', 'a', 'b', 'c', 'd', 'e', 'f', 'g');
  CMsRegColNames    : array [0..7] of string = ('l', 'x', 'wx', '?', 'h', 'w', 'r', '?');

Function TExecLineSD.MsRegName ( ATargIdx : TMsTargIdx ) : string;
Var
  BRow,
  BCol      : byte;
Begin
 Result:='Invalid';
 repeat
 GetRowCol(ATargIdx,BRow,BCol);
 if BRow=0 then
  begin
  if BCol=$6 then begin Result:='esp'; break; end;
  if BCol=$4 then begin Result:='mh'; break; end;
  end;
 Result:=CMsRegRowNames[BRow]+CMsRegColNames[BCol];
 until TRUE;
End;

Procedure TExecLineSD.GenCode ( Const AFormat : string );
Var
  BAsmLineS : string;
  BPos      : Integer;
Begin
 BAsmLineS:=AFormat;
 BPos:=0;
 while BPos<Length(BAsmLineS) do
  begin
  repeat
  if BAsmLineS[1+BPos]<>'%' then break;
  case BAsmLineS[1+BPos+1] of
   'r': ReplaceKey(MsRegName(rtR),BAsmLineS,BPos+1);
   'u': ReplaceKey(MsRegName(rtU),BAsmLineS,BPos+1);
   's': ReplaceKey(MsRegName(rtS),BAsmLineS,BPos+1);
   'd': ReplaceKey(IntToHex(FSubdec.FConst,8),BAsmLineS,BPos+1);
  end; // Case
  until TRUE;
  inc(BPos);
  end;
 FAsmLineS:=BAsmLineS;
End;

Procedure TExecLineSD.CheckFixDstLabel;
Var
  BPos      : Integer;
Begin
 BPos:=0;
 while BPos<Length(FAsmLineS) do
  begin
  repeat
  if FAsmLineS[1+BPos]<>'%' then break;
  case FAsmLineS[1+BPos+1] of
   'm': ReplaceKey(FDstLabel,FAsmLineS,BPos+1);
  end; // Case
  until TRUE;
  inc(BPos);
  end;
End;

Function TExecLineSD.MsProcessExt ( ACmdCode : TMsCmdCode ) : boolean;
Var
  BExtLen   : byte;
Begin
 Result:=FALSE;
 repeat
 BExtLen:=CExtLenList[ACmdCode];
 if BExtLen=$24 then
  begin
  if (FBase and $0400)=0 then BExtLen:=2 else BExtLen:=4;
  end
 else if BExtLen=$25 then
  begin
  if (FBase and $0008)=0 then BExtLen:=2 else BExtLen:=4;
  end;
 if BExtLen=0 then begin Result:=TRUE; break; end;
 if Length(FCodeBin)<(2+BExtLen) then begin FLastError:='eBinary data is too short (Address: '+IntToHex(FVirtAddr,8)+') [R:TExecLineSD.MsGetExt]'; break; end;
 FExt:=(Cardinal(FCodeBin[1+2]) shl 0)+
       (Cardinal(FCodeBin[1+3]) shl 8);
 if BExtLen=4 then
  begin
  FExt:=FExt+
        (Cardinal(FCodeBin[1+4]) shl 16)+
        (Cardinal(FCodeBin[1+5]) shl 24);
  end;
 Result:=TRUE;
 until TRUE;
 if Result then SetLength(FCodeBin,2+BExtLen);
End;

Function TExecLineSD.ConstDecBx : Cardinal;
Begin
 Result:=((FBase shr 4) and $1F) or
         ((FBase shr 6) and $E0);
 if (Result and $80)<>0 then Result:=Result or $FFFFFF00;
End;

Procedure TExecLineSD.MsSubdecARUC;
// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
Begin
 FSubdec.FCccc:=(FBase shr 11) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtU].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtU].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtS].FWw:=FSubdec.FTarg[rtU].FWw;
 FSubdec.FConst:=FExt;
 if ((FBase and $0400)=0) and ((FSubdec.FConst and $8000)<>0) then FSubdec.FConst:=FSubdec.FConst or $FFFF0000;
 if FSubdec.FCccc in [0, 1] then FSubdec.FE:=1;
 if FSubdec.FCccc=7 then begin FSubdec.FCccc:=6; FSubdec.FE:=1; end;
End;

Procedure TExecLineSD.MsSubdecBM;
// BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
Begin
 FSubdec.FCccc:=0;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$0;
 FSubdec.FConst:=ConstDecBx;
End;

Procedure TExecLineSD.MsSubdecARRS;
// ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor | (r := r cmd s) of same size (Unused codes of ARUC, because FPU cannot use )
Begin
 FSubdec.FCccc:=(FBase shr 11) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtU]:=FSubdec.FTarg[rtR];
 FSubdec.FTarg[rtS].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtS].FWw:=(FBase shr 8) and $3;
 if FSubdec.FCccc in [0, 1, 3] then FSubdec.FE:=1;
End;

Procedure TExecLineSD.MsSubdecBC;
// BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
Begin
 FSubdec.FCccc:=0;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$0;
 FSubdec.FTarg[rtU]:=FSubdec.FTarg[rtR];
 FSubdec.FConst:=ConstDecBx;
 FSubdec.FE:=1;
End;

Procedure TExecLineSD.MsSubdecBA;
// BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
Begin
 FSubdec.FCccc:=0;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$0;
 FSubdec.FTarg[rtU]:=FSubdec.FTarg[rtR];
 FSubdec.FConst:=ConstDecBx;
 FSubdec.FE:=1;
End;

Procedure TExecLineSD.MsSubdecMAID;
// MAID   |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
Begin
 FSubdec.FCccc:=(FBase shr 10) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtA].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtA].FWw:=$2;
End;

Procedure TExecLineSD.MsSubdecMARC;
// MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
Begin
 FSubdec.FCccc:=(FBase shr 11) and $1;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtA].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtA].FWw:=$2;
 FSubdec.FConst:=FExt;
End;

Procedure TExecLineSD.MsSubdecONL;
// ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
Begin
 FSubdec.FCccc:=(FBase shr 8) and $3;
 FSubdec.FFfff:=FBase and $F;
 FSubdec.FTarg[rtU].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtU].FWw:=$2;
 FSubdec.FConst:=FExt;
 if (FBase and $0400)=0 then
  begin
  FSubdec.FConst:=FSubdec.FConst shl 1;
  if (FSubdec.FConst and $10000)<>0 then FSubdec.FConst:=FSubdec.FConst or $FFFF0000;
  end;
 FDstAddr:=FVirtAddr+FSubdec.FConst;
End;

Procedure TExecLineSD.MsSubdecIDE;
// IDE    |10111   ||     cww|0iiirrrr|                 | inc dec
Begin
 FSubdec.FCccc:=(FBase shr 10) and $1;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtU]:=FSubdec.FTarg[rtR];
 FSubdec.FConst:=1+((FBase shr 4) and $7);
 FSubdec.FE:=1;
End;

Procedure TExecLineSD.MsSubdecPEX;
// PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
Begin
 FSubdec.FCccc:=$0; FSubdec.FMioSignExt:=((FBase shr 10) and $1)<>0;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
End;

Procedure TExecLineSD.MsSubdecAF;
// AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$2;
End;

Procedure TExecLineSD.MsSubdecREM;
// REM    |10111   ||     cww|1010rrrr|                 | rem RFU
Begin
 FSubdec.FCccc:=(FBase shr 10) and $1;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FBase shr 8) and $3;
End;

Procedure TExecLineSD.MsSubdecPRCS;
// PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
Begin
 FSubdec.FCccc:=(FBase shr 8) and $3;
 FSubdec.FConst:=FExt;
 if ((FBase and $0400)=0) and ((FSubdec.FConst and $8000)<>0) then FSubdec.FConst:=FSubdec.FConst or $FFFF0000;
 if FSubdec.FCccc=0 then FSubdec.FConst:=FSubdec.FConst shl 1;
End;

Procedure TExecLineSD.MsSubdecSys;
// Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
End;

Procedure TExecLineSD.MsSubdecNTRE;
// NTRE   |10111   ||     ccc|10110010|                 | nop trap # # # # # #
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
End;

Procedure TExecLineSD.MsSubdecPPL;
// PPL    |10111   ||     4cc|10110011|#2/4               | popl pushl # # leave enter # #
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
 FSubdec.FConst:=FExt;
End;

Procedure TExecLineSD.MsSubdecRFU;
// RFU    |10111   ||        |        |
Begin
End;

Procedure TExecLineSD.MsSubdecONS;
// ONS    |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
Begin
 FSubdec.FFfff:=FBase and $F;
 FSubdec.FConst:=(FBase shr 3) and $1FE;
 if (FBase and $1000)<>0 then FSubdec.FConst:=FSubdec.FConst or $FFFFFE00;
 FDstAddr:=FVirtAddr+FSubdec.FConst;
End;

Procedure TExecLineSD.MsSubdecFRUC;
// FRUC   |11101   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$2;
 FSubdec.FTarg[rtU].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtU].FWw:=$2;
 FSubdec.FTarg[rtS].FWw:=$2;
 FSubdec.FConst:=FExt;
End;

Procedure TExecLineSD.MsSubdecFRRS;
// FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
Begin
 FSubdec.FCccc:=(FBase shr 8) and $7;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$2;
 FSubdec.FTarg[rtU]:=FSubdec.FTarg[rtR];
 FSubdec.FTarg[rtS].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtS].FWw:=$2;
End;

Procedure TExecLineSD.MsSubdecBTR;
// BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
Begin
 FSubdec.FCccc:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=$2;
 FSubdec.FIiix:=((FBase shr 4) and $0F) or
                ((FBase shr 6) and $10);
End;

Procedure TExecLineSD.MsSubdecBTM;
// BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
Begin
 FSubdec.FCccc:=(FBase shr 8) and $3;
 FSubdec.FTarg[rtA].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtA].FWw:=$2;
 FSubdec.FConst:=FExt;
 FSubdec.FIiix:=FBase and $7;
End;

Procedure TExecLineSD.MsSubdecARUI;
// ARUI   |1111110 ||       e|uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd i)
Begin
 FSubdec.FCccc:=(FExt shr 12) and $F;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FExt shr  8) and $3;
 FSubdec.FTarg[rtU].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtU].FWw:=(FExt shr 10) and $3;
 FSubdec.FConst:=FExt and $FF;
 FSubdec.FE:=(FBase shr 8) and $1;
End;

Procedure TExecLineSD.MsSubdecARUS;
// ARUS   |1111111 ||       e|uuuurrrr|ccccWWww|00wwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)
Begin
 FSubdec.FCccc:=(FExt shr 12) and $F;
 FSubdec.FTarg[rtR].FReg:=(FBase shr 0) and $F; FSubdec.FTarg[rtR].FWw:=(FExt shr  8) and $3;
 FSubdec.FTarg[rtU].FReg:=(FBase shr 4) and $F; FSubdec.FTarg[rtU].FWw:=(FExt shr 10) and $3;
 FSubdec.FTarg[rtS].FReg:=(FExt  shr 0) and $F; FSubdec.FTarg[rtS].FWw:=(FExt shr  4) and $3;
 FSubdec.FE:=(FBase shr 8) and $1;
End;

Procedure ProcSubdecARUC ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecARUC;
End;

Procedure ProcSubdecBM ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecBM;
End;

Procedure ProcSubdecARRS ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecARRS;
End;

Procedure ProcSubdecBC ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecBC;
End;

Procedure ProcSubdecBA ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecBA;
End;

Procedure ProcSubdecMAID ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecMAID;
End;

Procedure ProcSubdecMARC ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecMARC;
End;

Procedure ProcSubdecONL ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecONL;
End;

Procedure ProcSubdecIDE ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecIDE;
End;

Procedure ProcSubdecPEX ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecPEX;
End;

Procedure ProcSubdecAF ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecAF;
End;

Procedure ProcSubdecREM ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecREM;
End;

Procedure ProcSubdecPRCS ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecPRCS;
End;

Procedure ProcSubdecSys ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecSys;
End;

Procedure ProcSubdecNTRE ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecNTRE;
End;

Procedure ProcSubdecPPL ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecPPL;
End;

Procedure ProcSubdecRFU ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecRFU;
End;

Procedure ProcSubdecONS ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecONS;
End;

Procedure ProcSubdecFRUC ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecFRUC;
End;

Procedure ProcSubdecFRRS ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecFRRS;
End;

Procedure ProcSubdecBTR ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecBTR;
End;

Procedure ProcSubdecBTM ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecBTM;
End;

Procedure ProcSubdecARUI ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecARUI;
End;

Procedure ProcSubdecARUS ( ALine : TExecLineBase );
Begin
 TExecLineSD(ALine).MsSubdecARUS;
End;


Const
  CSubdecProcList : array [TMsCmdCode] of TSubdecProc =
   (
     nil, // msccInvalid,
     @ProcSubdecARUC, // ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
     @ProcSubdecBM,   // BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
     @ProcSubdecARRS, // ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor | (r := r cmd s) of same size (Unused codes of ARUC, because FPU cannot use )
     @ProcSubdecBC,   // BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
     @ProcSubdecBA,   // BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
     @ProcSubdecMAID, // MAID   |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
     @ProcSubdecMARC, // MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
     @ProcSubdecONL,  // ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
     @ProcSubdecIDE,  // IDE    |10111   ||     cww|0iiirrrr|                 | inc dec
     @ProcSubdecPEX,  // PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
     @ProcSubdecAF,   // AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
     @ProcSubdecREM,  // REM    |10111   ||     cww|1010rrrr|                 | rem RFU
     @ProcSubdecPRCS, // PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
     @ProcSubdecSys,  // Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
     @ProcSubdecNTRE, // NTRE   |10111   ||     ccc|10110010|                 | nop trap # # # # # #
     @ProcSubdecPPL,  // PPL    |10111   ||     4cc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU (4 is set by natural way)
     @ProcSubdecRFU,  // RFU    |10111   ||        |        |
     @ProcSubdecONS,  // ONS    |110     ||   djjjj|jjjjffff|                 | bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv
     @ProcSubdecFRUC, // FRUC   |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
     @ProcSubdecFRRS, // FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
     @ProcSubdecBTR,  // BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
     @ProcSubdecBTM,  // BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
     @ProcSubdecARUI, // ARUI   |11111100||        |uuuurrrr|cccceeww|wwiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr rol asr | (r = u cmd i)
     @ProcSubdecARUS  // ARUS   |11111101||        |uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr rol asr | (r = u cmd s)
   );

  // ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
  // BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
  // ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor | (r := r cmd s) of same size (Unused codes of ARUC, because FPU cannot use )
  // BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
  // BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
  // MAID   |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
  // MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
  // ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
  // IDE    |10111   ||     cww|0iiirrrr|                 | inc dec
  // PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
  // AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
  // REM    |10111   ||     cww|1010rrrr|                 | rem RFU
  // PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
  // Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
  // NTRE   |10111   ||     ccc|10110010|                 | nop trap # # # # # #
  // PPL    |10111   ||     4cc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU (4 is set by natural way)
  // RFU    |10111   ||        |        |
  // ONS    |110     ||   djjjj|jjjjffff|                 | bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv
  // FRUC   |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
  // FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
  // BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
  // BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
  // ARUI   |11111100||        |uuuurrrr|cccceeww|wwiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr rol asr | (r = u cmd i)
  // ARUS   |11111101||        |uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr rol asr | (r = u cmd s)


Function TExecLineSD.CmdDec ( AVirtAddr : Cardinal; Const ACodeBin : string ) : boolean;
Var
  BBase     : word;
  BCmdCode  : TMsCmdCode;
Begin
 Result:=FALSE; FLastError:='';
 FVirtAddr:=AVirtAddr; FBaseAddr:=AVirtAddr;
 FCodeBin:=ACodeBin;
 FSubdec:=ZMsSubdec;

 BCmdCode:=msccInvalid;

 repeat
 if Length(FCodeBin)<2 then begin FLastError:='eBinary data is too small [R:TExecLineSD.CmdDec]'; break; end;
 BBase:=(Word(FCodeBin[2]) shl 8)+Word(FCodeBin[1]);
 FBase:=BBase; FExt:=0;

 case (BBase and $C000) of
  $0000: begin
         if (BBase and $0600)=$0400 then BCmdCode:=msccBM
         else BCmdCode:=msccARUC;
         end;
  $4000: begin
         if (BBase and $0400)=$0000 then BCmdCode:=msccARRS
         else if (BBase and $0200)=$0000 then BCmdCode:=msccBC
         else BCmdCode:=msccBA;
         end;
  $8000: begin
         if (BBase and $2000)=$0000 then BCmdCode:=msccMAID
         else if (BBase and $1000)=$0000 then BCmdCode:=msccMARC
         else if (BBase and $0800)=$0000 then BCmdCode:=msccONL
         else if (BBase and $0080)=$0000 then BCmdCode:=msccIDE
         else
          begin
          case (BBase and $0070) of
           $0000: BCmdCode:=msccPEX;
           $0010: BCmdCode:=msccAF;
           $0020: BCmdCode:=msccREM;
            else  begin
                  case (BBase and $000F) of
                   $0000: BCmdCode:=msccPRCS;
                   $0001: BCmdCode:=msccSys;
                   $0002: BCmdCode:=msccNTRE;
                   $0003: BCmdCode:=msccPPL;
                  end; // case PRCS_Sys_NTRE_PPL
                  end;
          end; // case PEX_AF_REM_+
          end;
         end;
  $C000: begin
         if (BBase and $2000)=$0000 then BCmdCode:=msccONS
         else
          begin
          case (BBase and $1800) of
           $0000: BCmdCode:=msccFRUC;
           $0800: BCmdCode:=msccFRRS;
           $1000: BCmdCode:=msccBTR;
            else  begin
                  if (BBase and $0400)=$0000 then BCmdCode:=msccBTM
                  else
                   begin
                   case (BBase and $0200) of
                    $0000: BCmdCode:=msccARUI;
                    $0200: BCmdCode:=msccARUS;
                   end; // case PRCS_Sys_NTRE_PPL
                   end;
                  end;
          end; // case PEX_AF_REM_+
          end;
         end;
 end; // case top

 if BCmdCode=msccInvalid then begin FLastError:='eCommand code is not recognized (Address: '+IntToHex(FVirtAddr,8)+') [R:TExecLineSD.CmdDec]'; break; end;
 if MsProcessExt(BCmdCode)=FALSE then break;
 FSubdec.FCmdCode:=BCmdCode;
 CSubdecProcList[BCmdCode](Self);
 Result:=FLastError='';
 until TRUE;
End;

// ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU  (either REG or # but not both, call REG is not possible)
// ONS    |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
// PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
Function TExecLineSD.IsJmp : boolean;
Begin
 Result:=((FSubdec.FCmdCode=msccONS) and ((FSubdec.FFfff and $7)=$0)) or
         ((FSubdec.FCmdCode=msccONL) and ((FSubdec.FFfff and $7)=$0)) or
         ((FSubdec.FCmdCode=msccPRCS) and (FSubdec.FCccc=$0));
End;

Function TExecLineSD.IsJxx : boolean;
Begin
 Result:=(FSubdec.FCmdCode=msccONS) or
         (FSubdec.FCmdCode=msccONL) or
         ((FSubdec.FCmdCode=msccPRCS) and (FSubdec.FCccc=$0));
End;

// PPL    |10111   ||     4cc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU (4 is set by natural way)
Function TExecLineSD.IsRet : boolean;
Begin
 Result:=((FSubdec.FCmdCode=msccPPL) and (FSubdec.FCccc in [0, 4]) and ((FSubdec.FConst and $1)<>0));
End;

Function TExecLineSD.IsCall : boolean;
Begin
 Result:=(FSubdec.FCmdCode=msccONL) and (FSubdec.FCccc=$1);
End;

Function TExecLineSD.IsDecStop : boolean;
Begin
 Result:=FSubdec.FCmdCode in [msccInvalid];
End;

Function TExecLineSD.CallOrJmp : TCallOrJmp;
Begin
 Result:=cjUnknown;
 repeat
 if IsJmp then begin Result:=cjJmp; break; end;
 if IsJxx then begin Result:=cjJmp; break; end;
 if IsRet then begin Result:=cjCall; break; end;
 if (FSubdec.FCmdCode=msccPPL) and (FSubdec.FCccc in [1, 5]) and ((FSubdec.FConst and $1)<>0) then begin Result:=cjCall; break; end;
 until TRUE;
End;

end.

