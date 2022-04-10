unit DasmRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DasmBase_sd, AsmBase_sd, AsmHelperRV_sd, AsmMcuRV_sd;

Type
  TRvCmdCode =
   (
    rvccInvalid,
    rvccLui, rvccAuipc, rvccJ, rvccJal, rvccJalr, rvccSwt,
    rvccBeq, rvccBne, rvccBlt, rvccBge, rvccBltu, rvccBgeu,
    rvccLb, rvccLh, rvccLw, rvccLbu, rvccLhu,
    rvccSb, rvccSh, rvccSw,
    rvccAddi, rvccSlti, rvccSltiu, rvccXori, rvccOri, rvccAndi, rvccSlli, rvccSrli, rvccSrai,
    rvccAdd, rvccSub, rvccSll, rvccSlt, rvccSltu, rvccXor, rvccSrl, rvccSra, rvccOr, rvccAnd,
    rvccMul, rvccMulh, rvccMulhsu, rvccMulhu, rvccDiv, rvccDivu, rvccRem, rvccRemu,
    rvccFAdd, rvccFSub, rvccFMul, rvccFDiv,
    rvccFence, rvccFenceI, rvccEcall, rvccEbreak,
    rvccCsrRW, rvccCsrRS, rvccCsrRC
   );

  TRvSubdec = record
    FOpcode     : byte;
    FRd,
    FRs1,
    FRs2        : byte;
    FFn3,
    FFn7        : byte;
    FImm        : Integer;
  end;

  TExecLineRV = class(TExecLineBase)
  private
    FCmdCode    : TRvCmdCode;

    FSubdec     : TRvSubdec;

    FAsmLine    : TAsmFlowLine;
    FAsmBase    : TAsmBase;

    Procedure RvSubdecR ( ACode : Cardinal );
    Procedure RvSubdecI ( ACode : Cardinal );
    Procedure RvSubdecS ( ACode : Cardinal );
    Procedure RvSubdecB ( ACode : Cardinal );
    Procedure RvSubdecU ( ACode : Cardinal );
    Procedure RvSubdecJ ( ACode : Cardinal );
    Procedure RvSubdecCI ( ACode : word; AMaskReg : byte );

    Procedure SignExtImm ( AMask : Integer );
    Procedure GenCode ( Const AFormat : string );
    Procedure Reassembly;

    Function CmdDecC0 ( ACode : word ) : boolean;
    Function CmdDecC1 ( ACode : word ) : boolean;
    Function CmdDecC2 ( ACode : word ) : boolean;
    Function CmdDecC : boolean;

    Function RvCmdDecARx ( ACode73 : word ) : TRvCmdCode;
    Function RvCmdDecFRx ( ACode7 : word ) : TRvCmdCode;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function CmdDec ( AAddr : Cardinal; Const ACodeBin : string ) : boolean; Override;
    Procedure CheckFixDstLabel; Override;

    Function IsJmp : boolean; Override;
    Function IsJxx : boolean; Override;
    Function IsRet : boolean; Override;
    Function IsCall : boolean; Override;
    Function IsDecStop : boolean; Override;
    Function CallOrJmp : TCallOrJmp; Override;

    property CmdCode : TRvCmdCode read FCmdCode;
    property Subdec : TRvSubdec read FSubdec;
  end;

implementation

Uses
  ConComL;

Const
  ZRvSubdec : TRvSubdec =
   (
     FOpcode:0;
     FRd:0;
     FRs1:0;
     FRs2:0;
     FFn3:0;
     FFn7:0;
     FImm:0;
   );

  CRvCmdNames : array [TRvCmdCode] of string =
   (
    'Invalid',
    'lui', 'auipc', 'j', 'jal', 'jalr', 'swt',
    'beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu',
    'lb', 'lh', 'lw', 'lbu', 'lhu',
    'sb', 'sh', 'sw',
    'addi', 'slti', 'sltiu', 'xori', 'ori', 'andi', 'slli', 'srli', 'srai',
    'add', 'sub', 'sll', 'slt', 'sltu', 'xor', 'srl', 'sra', 'or', 'and',
    'mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu',
    'fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s',
    'fence', 'fencei',
    'ecall', 'ebreak',
    'csrrw', 'csrrs', 'csrrc'
   );

  CRvRegNames : array [0..31] of string =
   (
    'zero', 'ra', 'sp', 'gp', 'tp', 't0', 't1', 't2',
    's0', 's1', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5',
    '?', '?', '?', '?', '?', '?', '?', '?',
    '?', '?', '?', '?', '?', '?', '?', '?'
   );

  CRvBxList : array [0..7] of TRvCmdCode = (rvccBeq, rvccBne, rvccInvalid, rvccInvalid, rvccBlt, rvccBge, rvccBltu, rvccBgeu );
  CRvLxList : array [0..7] of TRvCmdCode = (rvccLb, rvccLh, rvccLw, rvccInvalid, rvccLbu, rvccLhu, rvccInvalid, rvccInvalid );
  CRvSxList : array [0..7] of TRvCmdCode = (rvccSb, rvccSh, rvccSw, rvccInvalid, rvccInvalid, rvccInvalid, rvccInvalid, rvccInvalid );
  CRvAIxList : array [0..7] of TRvCmdCode = (rvccAddi, rvccInvalid, rvccSlti, rvccSltiu, rvccXori, rvccInvalid, rvccOri, rvccAndi );

{ *** RvExec *** }

Constructor TExecLineRV.Create;
Begin
 Inherited;
 FAsmBase:=TAsmMcuRV.Create;
 FAsmLine:=TAsmFlowLineRiscV.Create; FAsmLine.AsmBase:=FAsmBase;
End;

Destructor TExecLineRV.Destroy;
Begin
 FAsmBase.Free;
 FAsmLine.Free;
 Inherited;
End;

Procedure TExecLineRV.SignExtImm ( AMask : Integer );
Var
  BImm      : Integer;
  BMask     : Integer;
Begin
 BImm:=FSubdec.FImm;
 if (AMask and BImm)<>0 then
  begin
  BMask:=AMask shl 1;
  while BMask<>0 do
   begin
   BImm:=BImm or BMask;
   BMask:=BMask shl 1;
   end;
  FSubdec.FImm:=BImm;
  end;
End;

Procedure ReplaceKey ( Const ANewS : string; Var ALineS : string; APos : Integer );
Begin
 Delete(ALineS,APos,2);
 Insert(ANewS,ALineS,APos);
End;

Procedure TExecLineRV.GenCode ( Const AFormat : string );
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
   'c': ReplaceKey(CRvCmdNames[FCmdCode],BAsmLineS,BPos+1);
   'd': ReplaceKey(CRvRegNames[FSubdec.FRd],BAsmLineS,BPos+1);
   'u': ReplaceKey(CRvRegNames[FSubdec.FRs1],BAsmLineS,BPos+1);
   's': ReplaceKey(CRvRegNames[FSubdec.FRs2],BAsmLineS,BPos+1);
   'h': ReplaceKey('0x'+IntToHex(FSubdec.FRs2,2),BAsmLineS,BPos+1);
   'i': ReplaceKey(IntToStr(FSubdec.FImm),BAsmLineS,BPos+1);
   'x': ReplaceKey('0x'+IntToHex(FSubdec.FImm,8),BAsmLineS,BPos+1);
   'r': ReplaceKey('0x'+IntToHex(FAddr+FSubdec.FImm,8),BAsmLineS,BPos+1);
  end; // Case
  until TRUE;
  inc(BPos);
  end;
 FAsmLineS:=BAsmLineS;
 Reassembly;
End;

Procedure TExecLineRV.CheckFixDstLabel;
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

Procedure TExecLineRV.RvSubdecR ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=(ACode shr 7) and $1F;
 FSubdec.FRs1:=(ACode shr 15) and $1F;
 FSubdec.FRs2:=(ACode shr 20) and $1F;
 FSubdec.FFn3:=(ACode shr 12) and $07;
 FSubdec.FFn7:=(ACode shr 25) and $7F;
 FSubdec.FImm:=0;
End;

Procedure TExecLineRV.RvSubdecI ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=(ACode shr 7) and $1F;
 FSubdec.FRs1:=(ACode shr 15) and $1F;
 FSubdec.FRs2:=0;
 FSubdec.FFn3:=(ACode shr 12) and $07;
 FSubdec.FFn7:=0;
 FSubdec.FImm:=(ACode shr 20) and $FFF;
End;

Procedure TExecLineRV.RvSubdecS ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=$0;
 FSubdec.FRs1:=(ACode shr 15) and $1F;
 FSubdec.FRs2:=(ACode shr 20) and $1F;
 FSubdec.FFn3:=(ACode shr 12) and $07;
 FSubdec.FFn7:=0;
 FSubdec.FImm:=((ACode shr 20) and $FE0) or
               ((ACode shr  7) and $01F);
End;

Procedure TExecLineRV.RvSubdecB ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=$0;
 FSubdec.FRs1:=(ACode shr 15) and $1F;
 FSubdec.FRs2:=(ACode shr 20) and $1F;
 FSubdec.FFn3:=(ACode shr 12) and $07;
 FSubdec.FFn7:=0;
 FSubdec.FImm:=((ACode shr 20) and $07E0) or
               ((ACode shr  7) and $001E) or
               ((ACode shl  4) and $0800) or
               ((ACode shr 19) and $1000);
End;

Procedure TExecLineRV.RvSubdecU ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=(ACode shr 7) and $1F;
 FSubdec.FRs1:=0;
 FSubdec.FRs2:=0;
 FSubdec.FFn3:=0;
 FSubdec.FFn7:=0;
 FSubdec.FImm:=ACode and $FFFFF000;
End;

Procedure TExecLineRV.RvSubdecJ ( ACode : Cardinal );
Begin
 FSubdec.FOpcode:=ACode and $7F;
 FSubdec.FRd:=(ACode shr 7) and $1F;
 FSubdec.FRs1:=0;
 FSubdec.FRs2:=0;    // 31 30:21 20 19:12
 FSubdec.FFn3:=0;    // 20|10: 1|11|19:12
 FSubdec.FFn7:=0;
 FSubdec.FImm:=((ACode shr 20) and $000007FE) or
               ((ACode shr  9) and $00000800) or
               ( ACode         and $000FF000) or
               ((ACode shr 11) and $00100000);
End;

Function TExecLineRV.RvCmdDecARx ( ACode73 : word ) : TRvCmdCode;
Begin
 Result:=rvccInvalid;
 case ACode73 of
  $000: Result:=rvccAdd;
  $200: Result:=rvccSub;
  $001: Result:=rvccSll;
  $002: Result:=rvccSlt;
  $003: Result:=rvccSltu;
  $004: Result:=rvccXor;
  $005: Result:=rvccSrl;
  $205: Result:=rvccSra;
  $006: Result:=rvccOr;
  $007: Result:=rvccAnd;
  $010: Result:=rvccMul;
  $011: Result:=rvccMulh;
  $012: Result:=rvccMulhsu;
  $013: Result:=rvccMulhu;
  $014: Result:=rvccDiv;
  $015: Result:=rvccDivu;
  $016: Result:=rvccRem;
  $017: Result:=rvccRemu;
 end;
End;

Function TExecLineRV.RvCmdDecFRx ( ACode7 : word ) : TRvCmdCode;
Begin
 Result:=rvccInvalid;
 case ACode7 of
  $00: Result:=rvccFAdd;
  $04: Result:=rvccFSub;
  $08: Result:=rvccFMul;
  $0C: Result:=rvccFDiv;
 end;
End;

Function TExecLineRV.CmdDecC0 ( ACode : word ) : boolean;
Begin
 Result:=FALSE;
 repeat
 case FSubdec.FFn3 of
    $0: begin // C.ADDI4SPN
        FSubdec.FImm:=((ACode shr 2) and $0008) or
                      ((ACode shr 4) and $0004) or
                      ((ACode shr 1) and $03C0) or
                      ((ACode shr 7) and $0030);
        FSubdec.FRd:=(ACode shr 2) and $07 or $08;
        FSubdec.FRs1:=$2;
        FCmdCode:=rvccAddi;
        GenCode('%c %d,%u,%x');
        end;
    $2: begin // LW
        FSubdec.FImm:=((ACode shl 1) and $0040) or
                      ((ACode shr 4) and $0004) or
                      ((ACode shr 7) and $0038);
        FSubdec.FRd:=(ACode shr 2) and $07 or $08;
        FSubdec.FRs1:=(ACode shr 7) and $07 or $08;
        FCmdCode:=rvccLw;
        GenCode('%c %d,%x(%u)');
        end;
    $6: begin // SW
        FSubdec.FImm:=((ACode shl 1) and $0040) or
                      ((ACode shr 4) and $0004) or
                      ((ACode shr 7) and $0038);
        FSubdec.FRs2:=(ACode shr 2) and $07 or $08;
        FSubdec.FRs1:=(ACode shr 7) and $07 or $08;
        FCmdCode:=rvccSw;
        GenCode('%c %s,%x(%u)');
        end;
   else begin
        FLastError:='eInvalid Fn3 opcode for compressed command '+IntToHex(ACode,4)+' [R:TExecLineRV.CmdDecC0]';
        break;
        end;
 end;
 Result:=TRUE;
 until TRUE;
End;

Procedure TExecLineRV.RvSubdecCI ( ACode : word; AMaskReg : byte );
Begin
 FSubdec.FImm:=((ACode shr 2) and $001F) or
               ((ACode shr 7) and $0020);
 SignExtImm($20);
 FSubdec.FRd:=(ACode shr 7) and AmaskReg;
End;

Function TExecLineRV.CmdDecC1 ( ACode : word ) : boolean;
Begin
 Result:=FALSE;
 repeat
 case FSubdec.FFn3 of
    $0: begin // ADDI
        RvSubdecCI(ACode,$1F);
        FSubdec.FRs1:=FSubdec.FRd;
        FCmdCode:=rvccAddi;
        if ACode=$0001 then GenCode('nop')
        else GenCode('%c %d,%u,%x');
        end;
    $1,
    $5: begin // Jal/J
        // 12 11   9  8 7 6    3 2
        // 11  4 9:8 10 6 7  3:1 5
        FSubdec.FImm:=((ACode shl 3) and $0020) or
                      ((ACode shr 2) and $000E) or
                      ((ACode shl 1) and $0080) or
                      ((ACode shr 1) and $0040) or
                      ((ACode shl 2) and $0400) or
                      ((ACode shr 1) and $0300) or
                      ((ACode shr 7) and $0010) or
                      ((ACode shr 1) and $0800);
        SignExtImm($0800);
        if FSubdec.FFn3=$1 then
         begin
         FSubdec.FRd:=$1;
         FCmdCode:=rvccJal;
         GenCode('%c %d,%m');
         end
        else
         begin
         FSubdec.FRd:=$0;
         FCmdCode:=rvccJ;
         GenCode('%c %m');
         end;
        end;
    $2: begin // LI
        RvSubdecCI(ACode,$1F);
        FSubdec.FRs1:=$0;
        FCmdCode:=rvccAddi;
        GenCode('li %d,%x');
        end;
    $3: begin // LUI/ADDI16SP
        FSubdec.FRd:=(ACode shr 7) and $1F;
        if FSubdec.FRd=$2 then // ADDI16SP
         begin
         FSubdec.FImm:=((ACode shl 3) and $0020) or
                       ((ACode shl 4) and $0180) or
                       ((ACode shl 1) and $0040) or
                       ((ACode shr 2) and $0010) or
                       ((ACode shr 3) and $0200);
         SignExtImm($200);
         FSubdec.FRs1:=$2;
         FCmdCode:=rvccAddi;
         GenCode('%c %d,%u,%x');
         end
        else
         begin
         FSubdec.FImm:=((ACode shl 10) and $1F000) or
                       ((ACode shl  5) and $20000);
         SignExtImm($20000);
         FSubdec.FRs1:=$0;
         FCmdCode:=rvccAddi;
         GenCode('lui %d,%x');
         end;
        end;
    $4: begin
        FSubdec.FRd:=(ACode shr 7) and $07 or $08;
        FSubdec.FRs1:=FSubdec.FRd;
        if (ACode and $0C00)<>$0C00 then
         begin
         FSubdec.FImm:=((ACode shr 2) and $001F) or
                       ((ACode shr 7) and $0020);
         SignExtImm($20);
         case ((ACode shr 10) and $3) of
            $0: FCmdCode:=rvccSrli;
            $1: FCmdCode:=rvccSrai;
            $2: FCmdCode:=rvccAndi;
         end; // Case
         GenCode('%c %d,%u,%x');
         end
        else
         begin
         FSubdec.FRs2:=(ACode shr 2) and $07 or $08;
         case ((ACode shr 5) and $3) of
            $0: FCmdCode:=rvccSub;
            $1: FCmdCode:=rvccXor;
            $2: FCmdCode:=rvccOr;
            $3: FCmdCode:=rvccAnd;
         end; // Case
         GenCode('%c %d,%u,%s');
         end;
        end;
    $6,
    $7: begin // BEQZ/BNEZ
        FSubdec.FImm:=((ACode shl 3) and $0020) or
                      ((ACode shr 2) and $0006) or
                      ((ACode shl 1) and $00C0) or
                      ((ACode shr 7) and $0018) or
                      ((ACode shr 4) and $0100);
        SignExtImm($0100);
        FSubdec.FRs1:=(ACode shr 7) and $07 or $08;
        FSubdec.FRs2:=$0;
        if FSubdec.FFn3=$6 then
         begin
         FCmdCode:=rvccBeq;
         GenCode('%c %u,%s,%m');
         end
        else
         begin
         FCmdCode:=rvccBne;
         GenCode('%c %u,%s,%m');
         end;
        end;
   else begin
        FLastError:='eInvalid Fn3 opcode for compressed command '+IntToHex(ACode,4)+' [R:TExecLineRV.CmdDecC0]';
        break;
        end;
 end;
 Result:=TRUE;
 until TRUE;
End;

Function TExecLineRV.CmdDecC2 ( ACode : word ) : boolean;
Var
  BFldC,
  BFldB,
  BFldA     : byte;
Begin
 Result:=FALSE;
 repeat
 case FSubdec.FFn3 of
    $0: begin // Slli
        RvSubdecCI(ACode,$1F);
        FSubdec.FRs1:=FSubdec.FRd;
        FCmdCode:=rvccSlli;
        GenCode('%c %d,%u,%x');
        end;
    $2: begin // Lwsp
        FSubdec.FImm:=((ACode shl 4) and $00C0) or
                      ((ACode shr 2) and $001C) or
                      ((ACode shr 7) and $0020);
        FSubdec.FRd:=(ACode shr 7) and $0F;
        if FSubdec.FRd=0 then begin FLastError:='eReserved command '+IntToHex(ACode,4)+' [R:TExecLineRV.CmdDecC2]'; break; end;
        FSubdec.FRs1:=$2;
        FCmdCode:=rvccLw;
        GenCode('%c %d,%x(%u)');
        end;
    $4: begin
        BFldC:=(ACode shr 12) and $01;
        BFldB:=(ACode shr  7) and $1F;
        BFldA:=(ACode shr  2) and $1F;
        if (BFldC=0) and (BFldB<>0) and (BFldA=0) then
         begin
         FSubdec.FRd:=$0;
         FSubdec.FRs1:=BFldB;
         FCmdCode:=rvccJalr;
         GenCode('jr %x(%u)');
         end
        else if (BFldC=0) and (BFldB<>0) and (BFldA<>0) then
         begin
         FSubdec.FRd:=BFldB;
         FSubdec.FRs2:=BFldA;
         FCmdCode:=rvccAdd;
         GenCode('mv %d,%s');
         end
        else if (BFldC=1) and (BFldB<>0) and (BFldA=0) then
         begin
         FSubdec.FRd:=$1;
         FSubdec.FRs1:=BFldB;
         FCmdCode:=rvccJalr;
         GenCode('%c %d,%x(%u)');
         end
        else if (BFldC=1) and (BFldB<>0) and (BFldA<>0) then
         begin
         FSubdec.FRd:=BFldB;
         FSubdec.FRs1:=BFldB;
         FSubdec.FRs2:=BFldA;
         FCmdCode:=rvccAdd;
         GenCode('%c %d,%u,%s');
         end
        else
         begin
         FLastError:='eInvalid opcode for compressed command '+IntToHex(ACode,4)+' [R:TExecLineRV.CmdDecC2]';
         break;
         end;
        end;
    $6: begin
        FSubdec.FImm:=((ACode shr 1) and $00C0) or
                      ((ACode shr 7) and $003C);
        FSubdec.FRs1:=$2;
        FSubdec.FRs2:=(ACode shr 2) and $1F;
        FCmdCode:=rvccSw;
        GenCode('%c %s,%x(%u)');
        end;
   else begin
        FLastError:='eInvalid Fn3 opcode for compressed command '+IntToHex(ACode,4)+' [R:TExecLineRV.CmdDecC0]';
        break;
        end;
 end;
 Result:=TRUE;
 until TRUE;
End;

Function TExecLineRV.CmdDecC : boolean;
Var
  BCode     : word;
Begin
 Result:=FALSE;
 repeat
 if Length(FCodeBin)<2 then begin FLastError:='eBinary data is smaller than required instruction code [R:TExecLineRV.CmdDecC]'; break; end;
 if Length(FCodeBin)>2 then SetLength(FCodeBin,2);
 BCode:=(Cardinal(FCodeBin[2]) shl  8) or
        (Cardinal(FCodeBin[1]) shl  0);
 FSubdec.FFn3:=(BCode shr 13) and $7;
 case (BCode and $3) of
   $0: Result:=CmdDecC0(BCode);
   $1: Result:=CmdDecC1(BCode);
   $2: Result:=CmdDecC2(BCode);
 end;
 until TRUE;
End;

Function TExecLineRV.CmdDec ( AAddr : Cardinal; Const ACodeBin : string ) : boolean;
Var
  BCode     : Cardinal;
  BSubFn    : Cardinal;
Begin
 Result:=FALSE; FLastError:='';
 FAddr:=AAddr;
 FCodeBin:=ACodeBin;
 FSubdec:=ZRvSubdec;
 repeat
 if (FCodeBin[1]=#0) and (FCodeBin[2]=#0) then begin FCmdCode:=rvccInvalid; Result:=TRUE; break; end;
 if (Byte(FCodeBin[1]) and $03)<>$03 then begin Result:=CmdDecC; break; end;
 if Length(FCodeBin)<4 then begin FLastError:='eBinary data is smaller than required instruction code [R:TExecLineRV.CmdDec]'; break; end;
 if Length(FCodeBin)>4 then SetLength(FCodeBin,4);
 BCode:=(Cardinal(FCodeBin[4]) shl 24) or
        (Cardinal(FCodeBin[3]) shl 16) or
        (Cardinal(FCodeBin[2]) shl  8) or
        (Cardinal(FCodeBin[1]) shl  0);
 if BCode=$C0001073 then begin FCmdCode:=rvccInvalid; Result:=TRUE; break; end;
 case (BCode and $7F) of
   $37: begin // LUI
        RvSubdecU(BCode);
        FCmdCode:=rvccLui;
        GenCode('%c %d,%x');
        end;
   $17: begin // AUIPC
        RvSubdecU(BCode);
        FCmdCode:=rvccAuipc;
        GenCode('%c %d,%x');
        end;
   $6F: begin // JAL
        RvSubdecJ(BCode); SignExtImm($100000);
        FCmdCode:=rvccJal;
        GenCode('%c %d,%r');
        end;
   $67: begin // JALR
        RvSubdecI(BCode); SignExtImm($800);
        if (BCode and $1000)<>0 then
         begin
         FSubdec.FImm:=FSubdec.FImm shl 1;
         FCmdCode:=rvccSwt;
         GenCode('%c %r');
         end
        else
         begin
         FCmdCode:=rvccJalr;
         GenCode('%c %d,%x(%u)');
         end;
        end;
   $63: begin // Bx
        RvSubdecB(BCode); SignExtImm($1000);
        FCmdCode:=CRvBxList[FSubdec.FFn3];
        GenCode('%c %u,%s,%r');
        end;
   $03: begin // Lx
        RvSubdecI(BCode); SignExtImm($800);
        FCmdCode:=CRvLxList[FSubdec.FFn3];
        GenCode('%c %d,%x(%u)');
        end;
   $23: begin // Sx
        RvSubdecS(BCode); SignExtImm($800);
        FCmdCode:=CRvSxList[FSubdec.FFn3];
        GenCode('%c %s,%x(%u)');
        end;
   $13: begin // AIx
        RvSubdecI(BCode);
        FCmdCode:=CRvAIxList[FSubdec.FFn3];
        if FSubdec.FFn3 in [0, 2, 3, 4, 6, 7] then
         begin
         SignExtImm($800);
         if BCode=$00000013 then GenCode('nop')
         else GenCode('%c %d,%u,%x');
         end
        else
         begin
         BSubFn:=(FSubdec.FImm shr 5) and $7F; FSubdec.FImm:=FSubdec.FImm and $1F;
         if (FSubdec.FFn3=$1) and (BSubFn=$00) then FCmdCode:=rvccSlli
         else if (FSubdec.FFn3=$5) and (BSubFn=$00) then FCmdCode:=rvccSrli
         else if (FSubdec.FFn3=$5) and (BSubFn=$20) then FCmdCode:=rvccSrai
         else begin FLastError:='eOpcode AIx is not recognized [R:TExecLineRV.CmdDec]'; break; end;
         GenCode('%c %d,%u,%x');
         end;
        end;
   $33: begin // ARx
        RvSubdecR(BCode);
        FCmdCode:=RvCmdDecARx((FSubdec.FFn7 shl 4)+FSubdec.FFn3);
        if FCmdCode=rvccInvalid then begin FLastError:='eInvalid Fn3/Fn7 code combination for ARx code [R:TExecLineRV.CmdDec]'; break; end;
        GenCode('%c %d,%u,%s');
        end;
   $0F: begin
        RvSubdecI(BCode);
        case FSubdec.FFn3 of
          0: FCmdCode:=rvccFence;
          1: FCmdCode:=rvccFenceI;
        else begin FLastError:='eInvalid Fn3 code for Fence command [R:TExecLineRV.CmdDec]'; break; end;
        end;
        if FSubdec.FRd<>0 then begin FLastError:='eInvalid RD field for Fence command [R:TExecLineRV.CmdDec]'; break; end;
        if FSubdec.FRs1<>0 then begin FLastError:='eInvalid RS1 field for Fence command [R:TExecLineRV.CmdDec]'; break; end;
        GenCode('%c 0x'+IntToHex((FSubdec.FImm shr 4) and $F,4)+',0x'+IntToHex((FSubdec.FImm shr 0) and $F,4));
        end;
   $53: begin // FP
        RvSubdecR(BCode);
        if FSubdec.FFn3<>0 then begin FLastError:='eOnly RM=000 (i.e. "rne") option is supported for this core [R:TExecLineRV.CmdDec]'; break; end;
        FCmdCode:=RvCmdDecFRx(FSubdec.FFn7);
        if FCmdCode=rvccInvalid then begin FLastError:='eInvalid Fn7 code for ARx code [R:TExecLineRV.CmdDec]'; break; end;
        GenCode('%c %d,%u,%s,rne');
        end;
   $73: begin
        RvSubdecI(BCode);
        if FSubdec.FFn3=0 then
         begin
         case FSubdec.FImm of
           0: FCmdCode:=rvccEcall;
           1: FCmdCode:=rvccEbreak;
         else begin
              FLastError:='eInvalid Imm field for "E" command [R:TExecLineRV.CmdDec]';
              break;
              end;
         end;
         GenCode('%c');
         end
        else
         begin
         case FSubdec.FFn3 of
           1: FCmdCode:=rvccCsrRW;
           2: FCmdCode:=rvccCsrRS;
           3: FCmdCode:=rvccCsrRC;
         else begin FLastError:='eInvalid Fn3 code for CSR group [R:TExecLineRV.CmdDec]'; break; end;
         end;
         GenCode('%c %d,%x,%u');
         end;
        end;
  // $2F: begin // AMOx
  //      end;
   else begin
        FLastError:='eCommand code is not recognized '+IntToHex(BCode,8)+' [R:TExecLineRV.CmdDec]';
        break;
        end;
 end; // case
 if FSubdec.FRd>$F then begin FLastError:='eUnsupported Rd for this type of CPU [R:TExecLineRV.CmdDec]'; break; end;
 if FSubdec.FRs1>$F then begin FLastError:='eUnsupported Rs1 for this type of CPU [R:TExecLineRV.CmdDec]'; break; end;
 if FSubdec.FRs2>$F then begin FLastError:='eUnsupported Rs2 for this type of CPU [R:TExecLineRV.CmdDec]'; break; end;
 Result:=FLastError='';
 until TRUE;
End;

Function TExecLineRV.IsJmp : boolean;
Begin
 Result:= (FCmdCode=rvccJ) or
         ((FCmdCode=rvccJal) and (FSubdec.FRd=$0));
End;

{
rvccLui, rvccAuipc, rvccJ, rvccJal, rvccJalr,
rvccBeq, rvccBne, rvccBlt, rvccBge, rvccBltu, rvccBgeu,
rvccLb, rvccLh, rvccLw, rvccLbu, rvccLhu,
rvccSb, rvccSh, rvccSw,
rvccAddi, rvccSlti, rvccSltiu, rvccXori, rvccOri, rvccAndi, rvccSlli, rvccSrli, rvccSrai,
rvccAdd, rvccSub, rvccSll, rvccSlt, rvccSltu, rvccXor, rvccSrl, rvccSra, rvccOr, rvccAnd
}
Function TExecLineRV.IsJxx : boolean;
Begin
 Result:=FCmdCode in [rvccBeq, rvccBne, rvccBlt, rvccBge, rvccBltu, rvccBgeu, rvccJal];
End;

Function TExecLineRV.IsRet : boolean;
Begin
 Result:=(FCmdCode=rvccJalr) and (FSubdec.FRd=$0) and (FSubdec.FRs1=$1);
End;

Function TExecLineRV.IsCall : boolean;
Begin
 Result:=FALSE;
End;

Function TExecLineRV.IsDecStop : boolean;
Begin
 Result:=FCmdCode in [rvccInvalid];
End;

Function TExecLineRV.CallOrJmp : TCallOrJmp;
Begin
 Result:=cjUnknown;
 repeat
 if IsJmp then begin Result:=cjJmp; break; end;
 if IsJxx then begin Result:=cjJmp; break; end;
 if IsRet then begin Result:=cjCall; break; end;
 until TRUE;
End;

Procedure TExecLineRV.Reassembly;
Begin
 repeat
 if FLastError<>'' then break;
 FAsmLine.Clear; FAsmLine.Addr:=FAddr;
 FAsmLine.Parse(FAsmLineS,'',0);
 if FAsmLine.LastError<>'' then begin FLastError:='Cannot reassemble line "'+FAsmLineS+'": ['+FAsmLine.LastError+']'; break; end;
 FAsmBase.CodeGenCmd(FAsmLine);
 if FAsmLine.LastError<>'' then begin FLastError:='Cannot reassemble line "'+FAsmLineS+'": ['+FAsmLine.LastError+']'; break; end;
 if FAsmLine.CodeBin<>CodeBin then begin FLastError:='Reassembly produces different code '+StrBinToHex(FAsmLine.CodeBin)+' vs '+StrBinToHex(CodeBin); break; end;
 until TRUE;
End;

end.

