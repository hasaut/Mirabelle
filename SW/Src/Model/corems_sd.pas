unit CoreMS_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, AsmTypes_sd, CoreBase_sd, DasmMS_sd;

Type
  TExecMS = Function : boolean of object;
  TExecMsList = array [TMsCmdCode] of TExecMS;

  TCoreMS = class(TCoreBase)
  private
    FRegList        : array [0..15] of Cardinal; // 0 not used
    FSubdecPrev     : TMsSubdec;
    FExec           : TExecLineSD;
    FIpBeforeJmp    : Cardinal;

    FExecProcList   : TExecMsList;

    Function TargSize ( ATargIdx : TMsTargIdx ) : byte;
    Function JmpEn ( ACond : byte ) : boolean;
    Procedure CropData ( Var AData : Cardinal; ATargIdx : TMsTargIdx );
    Procedure SetFlagVN_A ( ADataR, ADataU, ADataS : Cardinal; Var AFlags : byte; AMask : byte );
    Procedure SetFlagVN_S ( ADataR, ADataU : Cardinal; Var AFlags : byte );
    Procedure SignExtA ( Var AData : Cardinal; AWw : byte; ASignExt : boolean );
    Procedure SignExt ( Var ADataU, ADataS : Cardinal; ASignExt : boolean );
    Function AluA ( Out ADataR : Cardinal; ADataU, ADataS : Cardinal; ACmd : byte; ASignExt : byte ) : boolean;
    Function AluT (  Out ADataR : Cardinal; ADataU, ADataS : Cardinal; ACmd : byte ) : boolean;
    Function AluF1 ( Var ADataR : Cardinal; ACmd : byte ) : boolean;

    Function RdReg ( ARow, ACol : byte ) : Cardinal;
    Function RdReg ( ATargIdx : TMsTargIdx ) : Cardinal;
    Procedure WrReg ( ARow, ACol : byte; AData : Cardinal );
    Procedure WrReg ( ATargIdx : TMsTargIdx; AData : Cardinal );
    Procedure WrReg ( Const ASubdec : TMsSubdec; ATargIdx : TMsTargIdx; AData : Cardinal );
    Function GetSp : Cardinal;
    Procedure SetSp ( AData : Cardinal );
    Function PushD ( AData : Cardinal ) : boolean;
    Function PopD ( Out AData : Cardinal ) : boolean;
    Function GetIp : Cardinal;
    Procedure SetIp ( AData : Cardinal );
    Procedure UpdateIp;

    Function ExecInvalid : boolean;
    Function ExecARUC : boolean;
    Function ExecBM   : boolean;
    Function ExecARRS : boolean;
    Function ExecBC   : boolean;
    Function ExecBA   : boolean;
    Function ExecMAID : boolean;
    Function ExecMARC : boolean;
    Function ExecONL  : boolean;
    Function ExecIDE  : boolean;
    Function ExecPEX  : boolean;
    Function ExecAF   : boolean;
    Function ExecREM  : boolean;
    Function ExecPRCS : boolean;
    Function ExecSys  : boolean;
    Function ExecNTRE : boolean;
    Function ExecPPL  : boolean;
    Function ExecRFU  : boolean;
    Function ExecONS  : boolean;
    Function ExecFRUC : boolean;
    Function ExecFRRS : boolean;
    Function ExecBTR  : boolean;
    Function ExecBTM  : boolean;
    Function ExecARUI : boolean;
    Function ExecARUS : boolean;

  protected
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function RegsAsStr : string; Override;
    Function RdRegs : string; Override;
    Procedure WrRegs ( Const ARegs : string ); Override;

    Procedure Reset; Override;
    Procedure StepInto; Override;
  end;

implementation

Uses
  ConComL;

// *** Old one ***
// AC     |0       || 4wwcccc|uuuurrrr|#2/4             | add sub and or xor mul udiv rem sdiv fadd fsub fmul fdiv 3xRFU | (r = u cmd Const16)
// MIO    |10      ||  4wwccc|aaaarrrr|#2/4             | r/w use_inc_dec ++/-- (x01=IO. If use_inc_dec=1, then there is no #)
//        |010     ||   4wwcc|aaaarrrr|#2/4             | Unused codes of AC (because i=1(32 bit) cannot be used with byte/word constant
// J      |110     ||   diiii|iiiiffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
// JCS    |1110    ||    04cc|uuuuffff|#2/4             | jmp call swt Ejal (cond)  (either REG or # but not both, call REG is not possible)
// BBr    |1110    ||    1000|uuuurrrr|cccceeww|wwwwssss| shl shr asl asr rol ror rcl rcr 8xRFU
// BBi    |1110    ||    1001|uuuurrrr|cccceeww|wwiiiiii| shl shr asl asr rol ror rcl rcr 8xRFU
// AR     |1110    ||    1010|uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex rem sdivex fadd fsub fmul fdiv itf trunc round | (r = u cmd s)
// IDE    |1110    ||    1011|0cwwrrrr|                 | inc dec
// PEX    |1110    ||    1011|1cwwrrrr|                 | pushzx pushsx
// PRC    |1110    ||    1111|0400000c|#2/4             | push_# ret_#
// Sys    |1110    ||    1111|10000ccc|                 | info siend silock siunlock siconf # # CPush
// NTRE   |1110    ||    1111|10001ccc|                 | nop trap retz # rdml wrml # #
// BTx    |1111    ||    00cc|aaaa4iii|#2/4             | bt btr bts btx

// *** New one ***
// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
// BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
// ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
// BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
// BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
// MAID   |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
// MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
// ONL    |10110   ||     4cc|uuuuffff|#2/4             | jmp call 2xRFU  (either REG or # but not both, call REG is not possible)
// IDE    |10111   ||     cww|0iiirrrr|                 | inc dec
// PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
// AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
// REM    |10111   ||     cww|1010rrrr|                 | rem RFU
// PRCS   |10111   ||     4cc|10110000|#2/4             | push_# ret_# swt RFU
// Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # CPush
// NTRE   |10111   ||     ccc|10110010|                 | nop trap retz # rdml wrml # #
// PPL    |10111   ||     ccc|10110011|#2               | pushl popl 6xRFU
// RFU    |10111   ||        |        |
// ONS    |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
// FRUC   |11101   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
// FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
// BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
// BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
// ARUI   |11111100||        |uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd i)
// ARUS   |11111101||        |uuuurrrr|ccccwwww|00wwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)

Constructor TCoreMS.Create;
Begin
 Inherited;
 FExec:=TExecLineSD.Create;
 FCoreType:=$1; FRegMIp.FFlags:=FCoreType shl 4;
 FExecProcList[msccInvalid]:=@ExecInvalid;
 FExecProcList[msccARUC]:=@ExecARUC;
 FExecProcList[msccBM  ]:=@ExecBM  ;
 FExecProcList[msccARRS]:=@ExecARRS;
 FExecProcList[msccBC  ]:=@ExecBC  ;
 FExecProcList[msccBA  ]:=@ExecBA  ;
 FExecProcList[msccMAID]:=@ExecMAID;
 FExecProcList[msccMARC]:=@ExecMARC;
 FExecProcList[msccONL ]:=@ExecONL ;
 FExecProcList[msccIDE ]:=@ExecIDE ;
 FExecProcList[msccPEX ]:=@ExecPEX ;
 FExecProcList[msccAF  ]:=@ExecAF  ;
 FExecProcList[msccREM ]:=@ExecREM ;
 FExecProcList[msccPRCS]:=@ExecPRCS;
 FExecProcList[msccSys ]:=@ExecSys ;
 FExecProcList[msccNTRE]:=@ExecNTRE;
 FExecProcList[msccPPL ]:=@ExecPPL ;
 FExecProcList[msccRFU ]:=@ExecRFU ;
 FExecProcList[msccONS ]:=@ExecONS ;
 FExecProcList[msccFRUC]:=@ExecFRUC;
 FExecProcList[msccFRRS]:=@ExecFRRS;
 FExecProcList[msccBTR ]:=@ExecBTR ;
 FExecProcList[msccBTM ]:=@ExecBTM ;
 FExecProcList[msccARUI]:=@ExecARUI;
 FExecProcList[msccARUS]:=@ExecARUS;
End;

Destructor TCoreMS.Destroy;
Begin
 FExec.Free;
 Inherited;
End;

Function TCoreMS.RegsAsStr : string;
Var
  BRegIdx   : Integer;
Begin
 Result:=IntToHex(FRegList[8],8)+IntToHex(ExportMIp,8);
 for BRegIdx:=1 to 7 do Result:=Result+IntToHex(FRegList[8+BRegIdx],8)+IntToHex(FRegList[0+BRegIdx],8);
 Result:=Result+IntToHex(FExtReg[1],8)+IntToHex(FExtReg[0],8);
End;

Function TCoreMs.RdRegs : string;
Var
  BRegIdx   : Integer;
Begin
 Result:=DWordAsStr(ExportMIp)+DWordAsStr(FRegList[8]);
 for BRegIdx:=1 to 7 do Result:=Result+DWordAsStr(FRegList[0+BRegIdx])+DWordAsStr(FRegList[8+BRegIdx]);
End;

Procedure TCoreMs.WrRegs ( Const ARegs : string );
Var
  BRegIdx   : Integer;
Begin
 ImportMIp(StrAsDWord(ARegs,0)); FRegList[8]:=StrAsDWord(ARegs,4);
 for BRegIdx:=1 to 7 do
  begin
  FRegList[0+BRegIdx]:=StrAsDWord(ARegs,8*BRegIdx+0);
  FRegList[8+BRegIdx]:=StrAsDWord(ARegs,8*BRegIdx+4);
  end;
End;

Function TCoreMs.TargSize ( ATargIdx : TMsTargIdx ) : byte;
Begin
 Result:=1 shl FExec.Subdec.FTarg[ATargIdx].FWw;
End;

Procedure TCoreMs.CropData ( Var AData : Cardinal; ATargIdx : TMsTargIdx );
Var
  BSize     : byte;
Begin
 BSize:=TargSize(ATargIdx);
 case BSize of
   1: AData:=AData and $FF;
   2: AData:=AData and $FFFF;
   4: AData:=AData and $FFFFFFFF;
 end;
End;

Function TCoreMS.RdReg ( ARow, ACol : byte ) : Cardinal;
Begin
 Result:=0;
 repeat
 if ARow=0 then
  begin
  if ACol=$8 then begin Result:=FRegList[8]; break; end;
  end;
 case ACol of
   $1: begin
       Result:=FRegList[0+ARow] and $FF;
       end;
   $2: begin
       Result:=(FRegList[0+ARow] shr 8) and $FF;
       end;
   $3: begin
       Result:=FRegList[0+ARow] and $FFFF;
       end;
   $4: begin
       Result:=(FRegList[0+ARow] shr 16) and $FFFF;
       end;
   $7: begin
       Result:=FRegList[0+ARow];
       end;
   $8: begin
       Result:=FRegList[8+ARow];
       end;
 end;
 until TRUE;
End;

Function TCoreMS.RdReg ( ATargIdx : TMsTargIdx ) : Cardinal;
Var
  BRow,
  BCol      : byte;
Begin
 Result:=0;
 FExec.GetRowCol(ATargIdx,BRow,BCol);
 repeat
 if BRow=0 then
  begin
  if BCol=$8 then begin Result:=FRegList[8]; break; end;
  end;
 case BCol of
   $1: begin
       Result:=FRegList[0+BRow] and $FF;
       end;
   $2: begin
       Result:=(FRegList[0+BRow] shr 8) and $FF;
       end;
   $3: begin
       Result:=FRegList[0+BRow] and $FFFF;
       end;
   $4: begin
       Result:=(FRegList[0+BRow] shr 16) and $FFFF;
       end;
   $7: begin
       Result:=FRegList[0+BRow];
       end;
   $8: begin
       Result:=FRegList[8+BRow];
       end;
 end;
 until TRUE;
End;

Procedure TCoreMS.WrReg ( ARow, ACol : byte; AData : Cardinal );
Begin
 repeat
 if ARow=0 then
  begin
  if ACol=$8 then begin FRegList[8]:=AData; break; end;
  end;
 case ACol of
   $1: begin
       FRegList[0+ARow]:=(FRegList[ARow] and $FFFFFF00) or (AData and $FF);
       end;
   $2: begin
       FRegList[0+ARow]:=(FRegList[ARow] and $FFFF00FF) or ((AData shl 8) and $FF00);
       end;
   $3: begin
       FRegList[0+ARow]:=(FRegList[ARow] and $FFFF0000) or (AData and $FFFF);
       end;
   $4: begin
       FRegList[0+ARow]:=(FRegList[ARow] and $0000FFFF) or ((AData shl 16) and $FFFF0000);
       end;
   $7: begin
       FRegList[0+ARow]:=AData;
       end;
   $8: begin
       FRegList[8+ARow]:=AData;
       end;
 end;
 until TRUE;
End;

Procedure TCoreMS.WrReg ( ATargIdx : TMsTargIdx; AData : Cardinal );
Var
  BRow,
  BCol      : byte;
Begin
 FExec.GetRowCol(ATargIdx,BRow,BCol);
 WrReg(BRow,BCol,AData);
End;

Procedure TCoreMS.WrReg ( Const ASubdec : TMsSubdec; ATargIdx : TMsTargIdx; AData : Cardinal );
Var
  BRow,
  BCol      : byte;
Begin
 FExec.GetRowCol(ASubdec,ATargIdx,BRow,BCol);
 WrReg(BRow,BCol,AData);
End;

Function TCoreMS.GetSp : Cardinal;
Begin
 Result:=FRegList[8];
End;

Procedure TCoreMS.SetSp ( AData : Cardinal );
Begin
 FRegList[8]:=AData;
End;

Function TCoreMS.PushD ( AData : Cardinal ) : boolean;
Var
  BAddr     : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BAddr:=GetSp-4;
 SetSp(BAddr);
 if MioWrX(BAddr,4,AData)=FALSE then begin ViewAny('eCannot push data at 0x'+IntToHex(BAddr,8)+' [R:TCoreMS.PushD]'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TCoreMs.PopD ( Out AData : Cardinal ) : boolean;
Var
  BAddr     : Cardinal;
Begin
 Result:=FALSE;
 AData:=0;
 repeat
 BAddr:=GetSp;
 if MioRdX(BAddr,4,AData)=FALSE then begin ViewAny('eCannot pop data at 0x'+IntToHex(BAddr,8)+' [R:TCoreMS.PopD]'); break; end;
 BAddr:=BAddr+4;
 SetSp(BAddr);
 Result:=TRUE;
 until TRUE;
End;

Function TCoreMs.GetIp : Cardinal;
Begin
 Result:=FRegMIp.FIp;
End;

Procedure TCoreMS.SetIp ( AData : Cardinal );
Begin
 FRegMIp.FIp:=AData;
End;

Procedure TCoreMS.UpdateIp;
Begin
 SetIp(FRegMIp.FIp+Length(FExec.CodeBin));
End;

Function TCoreMS.ExecInvalid : boolean;
Begin
 ViewAny('eCmd code is invalid [R:TCoreMS.ExecInvalid]');
 Result:=FALSE;
End;

Function TCoreMS.JmpEn ( ACond : byte ) : boolean;
Var
  BSmaller  : byte;
  BFlags    : byte;
Begin
 {
  wire BSmaller = AFlags[IFlagN];

 ((ACond==4'h0) ?  1'b1                             : 1'b0) | // sjmp
 ((ACond==4'h1) ?  AFlags[IFlagZ] |  AFlags[IFlagC] : 1'b0) | // jbe
 ((ACond==4'h2) ?                    AFlags[IFlagC] : 1'b0) | // jc
 ((ACond==4'h3) ?                   ~AFlags[IFlagC] : 1'b0) | // jnc
 ((ACond==4'h4) ?  AFlags[IFlagZ]                   : 1'b0) | // jz
 ((ACond==4'h5) ? ~AFlags[IFlagZ]                   : 1'b0) | // jnz
 ((ACond==4'h6) ? ~AFlags[IFlagZ] & ~AFlags[IFlagC] : 1'b0) | // ja
 //((ACond==4'h7) ? ~BMuxDAE                          : 1'b0) | // djnz
 ((ACond==4'h8) ? ~AFlags[IFlagZ] & ~BSmaller       : 1'b0) | // jg
 ((ACond==4'h9) ?  AFlags[IFlagZ] | ~BSmaller       : 1'b0) | // jge
 ((ACond==4'hA) ?                    BSmaller       : 1'b0) | // js
 ((ACond==4'hB) ?  AFlags[IFlagZ] |  BSmaller       : 1'b0) | // jse
 ((ACond==4'hC) ?  AFlags[IFlagN]                   : 1'b0) | // jn
 ((ACond==4'hD) ?  AFlags[IFlagV]                   : 1'b0) | // jv
 ((ACond==4'hE) ? ~AFlags[IFlagN]                   : 1'b0) | // jnn
 ((ACond==4'hF) ? ~AFlags[IFlagV]                   : 1'b0);  // jnv
 }
 Result:=FALSE;
 BFlags:=FRegMIp.FFlags and $F;
 BSmaller:=BFlags and $4;
 case ACond of
  $0: Result:=TRUE;               // sjmp
  $1: Result:=(BFlags and $3)<>0; // jbe
  $2: Result:=(BFlags and $1)<>0; // jc
  $3: Result:=(BFlags and $1)= 0; // jnc
  $4: Result:=(BFlags and $2)<>0; // jz
  $5: Result:=(BFlags and $2)= 0; // jnz
  $6: Result:=(BFlags and $3)= 0; // ja
  $7: Result:=FALSE;              // djnz
  $8: Result:=((BFlags and $2)= 0) and (BSmaller=0); // jg
  $9: Result:=((BFlags and $2)<>0) or (BSmaller=0); // jge
  $A: Result:=BSmaller<>0; // js
  $B: Result:=((BFlags and $2)<>0) or (BSmaller<>0); // jse
  $C: Result:=(BFlags and $4)<>0; // jn
  $D: Result:=(BFlags and $8)<>0; // jv
  $E: Result:=(BFlags and $4)=0; // jnn
  $F: Result:=(BFlags and $8)=0; // jnv
 end;
End;

Procedure TCoreMS.SetFlagVN_A ( ADataR, ADataU, ADataS : Cardinal; Var AFlags : byte; AMask : byte );
Var
  BFlagV    : array [0..2] of byte;
Begin
 BFlagV[0]:=(ADataR shr (8*TargSize(rtR)-1)) and $1;
 BFlagV[1]:=(ADataU shr (8*TargSize(rtU)-1)) and $1;
 BFlagV[2]:=(ADataS shr (8*TargSize(rtS)-1)) and $1;
 if (((BFlagV[1] xor BFlagV[2]) xor $1) and (BFlagV[1] xor BFlagV[0]))<>0 then AFlags:=AFlags or $8;
 if BFlagV[0]<>0 then AFlags:=AFlags or $4;
 AFlags:=AFlags and AMask;
End;

Procedure TCoreMS.SetFlagVN_S ( ADataR, ADataU : Cardinal; Var AFlags : byte );
Var
  BFlagV    : array [0..1] of byte;
Begin
 BFlagV[0]:=(ADataR shr (8*TargSize(rtR)-1)) and $1;
 //BFlagV[1]:=(ADataU shr (8*TargSize(rtU)-1)) and $1;
 //if (BFlagV[1] xor BFlagV[0])<>0 then AFlags:=AFlags or $8;
 if BFlagV[0]<>0 then AFlags:=AFlags or $4;
End;

Procedure TCoreMs.SignExtA ( Var AData : Cardinal; AWw : byte; ASignExt : boolean );
Var
  BMaskOr   : Cardinal;
  BMaskAnd  : Cardinal;
Begin
 BMaskOr:=$00000000;
 BMaskAnd:=$FFFFFFFF;
 case AWw of
   $0: begin
       if ASignExt and ((AData and $80)<>0) then BMaskOr:=$FFFFFF00;
       BMaskAnd:=$000000FF;
       end;
   $1: begin
       if ASignExt and ((AData and $8000)<>0) then BMaskOr:=$FFFF0000;
       BMaskAnd:=$0000FFFF;
       end;
   end; // case
 AData:=(AData and BMaskAnd) or BMaskOr;
End;

Procedure TCoreMs.SignExt ( Var ADataU, ADataS : Cardinal; ASignExt : boolean );
Begin
 SignExtA(ADataU,FExec.Subdec.FTarg[rtU].FWw,ASignExt);
 SignExtA(ADataS,FExec.Subdec.FTarg[rtS].FWw,ASignExt);
End;

Function TCoreMS.AluA ( Out ADataR : Cardinal; ADataU, ADataS : Cardinal; ACmd : byte; ASignExt : byte ) : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
  BDataSF   : Single absolute BDataS;
  BDataUF   : Single absolute BDataU;
  BDataRF   : Single absolute BDataR;
  BDataRQ   : QWord;
  BFlags    : byte;
  BUpdateF  : boolean;
Begin
 Result:=FALSE; BDataRQ:=0; BDataR:=0; BFlags:=0;
 repeat
 BDataU:=ADataU; BDataS:=ADataS; BUpdateF:=FALSE;
 //SignExt(BDataU,BDataS,ACmd in [0, 1, 7]);
 SignExt(BDataU,BDataS,ASignExt<>0);
 case ACmd of
   0: begin // add
      BDataRQ:=BDataU+BDataS; if (BDataRQ and $100000000)<>0 then BFlags:=BFlags or $1;
      BDataR:=BDataRQ;
      BUpdateF:=TRUE; SetFlagVN_A(BDataR,BDataU,BDataS,BFlags,$F);
      end;
   1: begin // sub
      BDataS:=BDataS xor $FFFFFFFF;
      BDataRQ:=BDataU+BDataS+1; if (BDataRQ and $100000000)=0 then BFlags:=BFlags or $1;
      BDataR:=BDataRQ;
      BUpdateF:=TRUE; SetFlagVN_A(BDataR,BDataU,BDataS,BFlags,$F);
      end;
   2: begin // and
      BDataR:=BDataU and BDataS;
      BUpdateF:=TRUE; SetFlagVN_A(BDataR,BDataU,BDataS,BFlags,$7);
      end;
   3: begin // or
      BDataR:=BDataU or BDataS;
      BUpdateF:=TRUE; SetFlagVN_A(BDataR,BDataU,BDataS,BFlags,$7);
      end;
   4: begin // xor
      BDataR:=BDataU xor BDataS;
      BUpdateF:=TRUE; SetFlagVN_A(BDataR,BDataU,BDataS,BFlags,$7);
      end;
   5: begin // mul
      BDataR:=BDataU*BDataS;
      end;
   6: begin // xdiv
      if ASignExt=0 then
       begin // udiv
       CropData(BDataS,rtS); CropData(BDataU,rtU);
       if BDataS=0 then BDataR:=$FFFFFFFF
       else BDataR:=BDataU div BDataS;
       end
      else
       begin // sdiv
       if BDataS=0 then BDataR:=$FFFFFFFF
       else BDataR:=Cardinal(Integer(BDataU) div Integer(BDataS));
       end;
      end;
   7: begin // xrem
      if ASignExt=0 then
       begin // urem
       CropData(BDataS,rtS); CropData(BDataU,rtU);
       if BDataS=0 then BDataR:=$00000000
       else BDataR:=BDataU mod BDataS;
       end
      else
       begin // srem
       if BDataS=0 then BDataR:=$00000000
       else BDataR:=Cardinal(Integer(BDataU) mod Integer(BDataS));
       end;
      end;
   8: begin // shl
      BDataS:=BDataS and $1F;
      if BDataS=0 then
       begin
       BDataR:=BDataU;
       end
      else
       begin
       BDataR:=BDataU shl (BDataS-1);
       case TargSize(rtR) of
         1: if (BDataR and $80)<>0 then BFlags:=BFlags or $1;
         2: if (BDataR and $8000)<>0 then BFlags:=BFlags or $1;
         4: if (BDataR and $80000000)<>0 then BFlags:=BFlags or $1;
       end;
       BDataR:=BDataR shl 1;
       end;
      BUpdateF:=TRUE; SetFlagVN_S(BDataR,BDataU,BFlags);
      end;
   9: begin // shr
      BDataS:=BDataS and $1F;
      if BDataS=0 then
       begin
       BDataR:=BDataU;
       end
      else
       begin
       BDataR:=BDataU shr (BDataS-1);
       if (BDataR and $1)<>0 then BFlags:=BFlags or $1;
       BDataR:=BDataR shr 1;
       end;
      BUpdateF:=TRUE; SetFlagVN_S(BDataR,BDataU,BFlags);
      end;
  10: begin // rol
      BDataS:=BDataS and $1F;
      if BDataS=0 then
       begin
       BDataR:=BDataU;
       end
      else
       begin
       BDataR:=BDataU shl (BDataS-1);
       case TargSize(rtR) of
         1: if (BDataR and $80)<>0 then BFlags:=BFlags or $1;
         2: if (BDataR and $8000)<>0 then BFlags:=BFlags or $1;
         4: if (BDataR and $80000000)<>0 then BFlags:=BFlags or $1;
       end;
       BDataR:=(BDataU shl (BDataS and $1F)) or (BDataU shr (8*TargSize(rtR)-(BDataS and $1F)));
       end;
      BUpdateF:=TRUE; SetFlagVN_S(BDataR,BDataU,BFlags);
      end;
  11: begin // asr
      BDataS:=BDataS and $1F;
      if BDataS=0 then
       begin
       BDataR:=BDataU;
       end
      else
       begin
       BDataR:=BDataU shr (BDataS-1);
       if (BDataR and $1)<>0 then BFlags:=BFlags or $1;
       BDataR:=BDataR shr 1;
       end;
      if (BDataU and (1 shl (8*TargSize(rtR)-1)))<>0 then
       begin
       BDataR:=BDataR or (($FFFFFFFF shr BDataS) xor $FFFFFFFF);
       end;
      BUpdateF:=TRUE; SetFlagVN_S(BDataR,BDataU,BFlags);
      end;
  12: begin // fadd
      BDataRF:=BDataUF+BDataSF;
      end;
  13: begin // fsub
      BDataRF:=BDataUF-BDataSF;
      end;
  14: begin // fmul
      BDataRF:=BDataUF*BDataSF;
      end;
  15: begin // fdiv
      if IsZero(BDataSF) then
       begin
       if Sign(BDataUF)=Sign(BDataSF) then BDataRF:=Infinity
       else BDataRF:=NegInfinity;
       end
      else BDataRF:=BDataUF/BDataSF;
      end;
 else begin
      ViewAny('eInvalid AC command code [R:TCoreMS.AluA]');
      break;
      end;
 end; // case

 if BUpdateF then
  begin
  if BDataR=0 then BFlags:=BFlags or $2;
  SetFlagsA(BFlags);
  end;
 Result:=TRUE;
 until TRUE;
 ADataR:=BDataR;
End;

Function TCoreMS.AluF1 ( Var ADataR : Cardinal; ACmd : byte ) : boolean;
Var
  BDataR    : Cardinal;
  BDataRF   : Single absolute BDataR;
Begin
 Result:=FALSE; BDataR:=ADataR;
 repeat
 case ACmd of
   0: begin // itf
      BDataRF:=LongInt(BDataR);
      end;
   1: begin // trunc
      BDataR:=Trunc(BDataRF);
      end;
   2: begin // round
      BDataR:=Round(BDataRF);
      end;
 else begin
      ViewAny('eInvalid AF command code [R:TCoreMS.AluF1]');
      break;
      end;
 end; // case
 Result:=TRUE;
 until TRUE;
 ADataR:=BDataR;
End;

Function TCoreMS.AluT ( Out ADataR : Cardinal; ADataU, ADataS : Cardinal; ACmd : byte ) : boolean;
Var
  BMask     : Cardinal;
  BFlags    : byte;
Begin
 Result:=FALSE; ADataR:=0; BFlags:=0;
 repeat
 BMask:=$1 shl ADataS;
 case ACmd of
   0: begin // bt
      ADataR:=ADataU;
      end;
   1: begin // btr
      ADataR:=ADataU and (BMask xor $FFFFFFFF);
      end;
   2: begin // bts
      ADataR:=ADataU or BMask;
      end;
 else begin
      ViewAny('eInvalid BT command code [R:TCoreMS.AluT]');
      break;
      end;
 end; // case

 if (ADataU and BMask)<>0 then BFlags:=BFlags or $1;
 //if ADataR=0 then BFlags:=BFlags or $2;
 SetFlagsA(BFlags);
 Result:=TRUE;
 until TRUE;
End;

// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
Function TCoreMS.ExecARUC : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,FExec.Subdec.FCccc and $7,FExec.Subdec.FE);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// BM   |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
Function TCoreMS.ExecBM  : boolean;
Begin
 WrReg(rtR,FExec.Subdec.FConst);
 Result:=TRUE;
 UpdateIp;
End;

// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
// ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
Const
  CCmdIrisToMrb : array [0..7] of byte = (0, 1, 3, 1, 2, 3, 2, 4);

// ARRS |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
Function TCoreMS.ExecARRS : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RdReg(rtS); if FExec.Subdec.FCccc=2 then BDataU:=0 else BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,CCmdIrisToMrb[FExec.Subdec.FCccc and $7],FExec.Subdec.FE);
 if Result=FALSE then break;
 if FExec.Subdec.FCccc in [3,6] then
 else WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// BC   |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
Function TCoreMS.ExecBC : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,1,FExec.Subdec.FE);
 if Result=FALSE then break;
 UpdateIp;
 until TRUE;
End;

// BA   |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
Function TCoreMS.ExecBA : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,0,FExec.Subdec.FE);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// MAID |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
Function TCoreMS.ExecMAID : boolean;
Var
  BAddr     : Cardinal;
  BDataA,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 BDataR:=0;
 repeat
 BDataA:=RdReg(rtA);
 if FExec.Subdec.FCccc in [5,7] then
  begin
  BDataA:=BDataA-TargSize(rtR);
  WrReg(rtA,BDataA);
  end;
 BAddr:=BDataA;

 if (FExec.Subdec.FCccc and $2)=0 then // read
  begin
  if MioRdX(BAddr,TargSize(rtR),BDataR)=FALSE then break;
  WrReg(rtR,BDataR);
  end
 else // write
  begin
  BDataR:=RdReg(rtR);
  if MioWrX(BAddr,TargSize(rtR),BDataR)=FALSE then break;
  end;

 if FExec.Subdec.FCccc in [4, 6] then
  begin
  BDataA:=BDataA+TargSize(rtR);
  WrReg(rtA,BDataA);
  end;

 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// MARC |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
Function TCoreMS.ExecMARC : boolean;
Var
  BAddr     : Cardinal;
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 BDataR:=0;
 repeat
 BAddr:=FExec.Subdec.FConst+RdReg(rtA);

 if FExec.Subdec.FCccc=0 then // read
  begin
  if MioRdX(BAddr,TargSize(rtR),BDataR)=FALSE then begin ViewAny('eError reading data from address 0x'+IntToHex(BAddr,4)+' [R:TCoreMS.ExecMARC]'); break; end;
  WrReg(rtR,BDataR);
  end
 else // write
  begin
  BDataR:=RdReg(rtR);
  if MioWrX(BAddr,TargSize(rtR),BDataR)=FALSE then begin ViewAny('eError writing data to address 0x'+IntToHex(BAddr,4)+' [R:TCoreMS.ExecMARC]'); break; end;
  end;

 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// ONL  |10110   ||     4cc|uuuuffff|#2/4             | jmp call 2xRFU  (either REG or # but not both, call REG is not possible)
Function TCoreMS.ExecONL : boolean;
Var
  BNewIp        : Cardinal;
Begin
 Result:=FALSE;
 repeat
 case FExec.Subdec.FCccc of
   0: begin // bra
      if FExec.Subdec.FTarg[rtU].FReg=0 then BNewIp:=GetIp+FExec.Subdec.FConst
      else BNewIp:=RdReg(rtU)+FExec.Subdec.FConst;
      UpdateIp;
      FIpBeforeJmp:=GetIp; if JmpEn(FExec.Subdec.FFfff) then SetIp(BNewIp);
      end;
   1: begin // swt
      if FExec.Subdec.FTarg[rtU].FReg=0 then BNewIp:=GetIp+FExec.Subdec.FConst
      else BNewIp:=RdReg(rtU)+FExec.Subdec.FConst;
      UpdateIp;
      FIpBeforeJmp:=GetIp;
      if JmpEn(FExec.Subdec.FFfff) then
       begin
       SetIp(BNewIp);
       if SysCmd(scSwt)=FALSE then break;
       end;
      end;
   else
     begin
     ViewAny('eInvalid ONL command code [R:TCoreMS.ExecONL]');
     break;
     end;
 end; // case
 Result:=TRUE;
 until TRUE;
End;

// IDE  |10111   ||     cww|0iiirrrr|                 | inc dec
Function TCoreMS.ExecIDE : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,FExec.Subdec.FCccc and $1,FExec.Subdec.FE);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// PEX  |10111   ||     cww|1000rrrr|                 | pushzx pushsx
Function TCoreMS.ExecPEX : boolean;
Var
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataR:=RdReg(rtR); SignExtA(BDataR,FExec.Subdec.FTarg[rtR].FWw,FExec.Subdec.FMioSignExt);
 if PushD(BDataR)=FALSE then break;
 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// AF   |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
Function TCoreMS.ExecAF : boolean;
Var
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataR:=RdReg(rtR);
 Result:=AluF1(BDataR,FExec.Subdec.FCccc and $7);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// REM  |10111   ||     cww|1010rrrr|                 | rem RFU
Function TCoreMS.ExecREM : boolean;
Begin
 Result:=FALSE;
 repeat
 if FExec.Subdec.FCccc=0 then
 else begin ViewAny('eInvalid REM command code [R:TCoreMS.ExecREM]'); break; end;
 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// PRCS |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
Function TCoreMS.ExecPRCS : boolean;
Begin
 Result:=FALSE;
 repeat
 case FExec.Subdec.FCccc of
    1: begin
       if PushD(FExec.Subdec.FConst)=FALSE then break;
       UpdateIp;
       end;
  else begin
       ViewAny('eInvalid PRCS command code [R:TCoreMS.ExecREM]');
       break;
       end;
 end; // case
 Result:=TRUE;
 until TRUE;
End;

// Sys  |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
Function TCoreMS.ExecSys : boolean;
Begin
 Result:=FALSE;
 repeat
 case FExec.Subdec.FCccc of
    0: begin // info
       FRegList[1]:=$00080000+
                    (Cardinal(FCoreCnt) shl 8)+
                    (Cardinal(FCoreIdx) shl 0);
       UpdateIp;
       Result:=TRUE;
       end;
    1: begin // siend
       Result:=SysCmd(scSiEnd);
       end;
    2: begin // silock
       Result:=SysCmd(scSiLock);
       UpdateIp;
       end;
    3: begin // siunlock
       Result:=SysCmd(scSiUnlock);
       UpdateIp;
       end;
    4: begin // siconf
       Result:=SysCmd(scSiConf);
       UpdateIp;
       end;
  else begin
       ViewAny('eInvalid SYS command code [R:TCoreMS.ExecNTRE]');
       break;
       end;
 end; // case
 until TRUE;
End;

// NTRE |10111   ||     ccc|10110010|                 | nop trap # # # # # #
Function TCoreMS.ExecNTRE : boolean;
Begin
 Result:=FALSE;
 repeat
 case FExec.Subdec.FCccc of
    0: begin // nop
       UpdateIp;
       end;
    1: begin // trap
       UpdateIp;
       FIsTrapHit:=TRUE;
       end;
    2: begin // test_end
       UpdateIp;
       FIsTrapHit:=TRUE;
       FIsTestEnd:=TRUE;
       end;
    6: begin // getfl
       UpdateIp;
       WrReg(1,1,FRegMIp.FFlags);
       end;
    7: begin // setfl
       UpdateIp;
       FRegMIp.FFlags:=RdReg(1,1);
       end;
  else begin
       ViewAny('eInvalid NTRE command code [R:TCoreMS.ExecNTRE]');
       break;
       end;
 end; // case
 Result:=TRUE;
 until TRUE;
End;

// PPL  |10111   ||     ccc|10110011|#2               | pushl popl 6xRFU
Function TCoreMS.ExecPPL : boolean;
Var
  BRegIdx   : Integer;
  BData     : Cardinal;
  BUpdateIp : boolean;
Begin
 Result:=FALSE;
 repeat
 BUpdateIp:=TRUE;
 case FExec.Subdec.FCccc of
    0,
    4: begin // popl/leave
       if FExec.Subdec.FCccc=4 then FRegList[8]:=FRegList[8]+((FExec.Subdec.FConst shr 14) and $3FC);
       BRegIdx:=15;
       while BRegIdx>=0 do
        begin
        if ((1 shl BRegIdx) and FExec.Subdec.FConst)<>0 then
         begin
         if PopD(BData)=FALSE then break;
         if BRegIdx=0 then begin SetIp(BData); BUpdateIp:=FALSE; end
         else FRegList[(BRegIdx shr 1)+((BRegIdx shl 3) and $8)]:=BData;
         end;
        dec(BRegIdx);
        end;
       if BRegIdx<>-1 then break;
       if FExec.Subdec.FCccc=4 then FRegList[8]:=FRegList[8]+((FExec.Subdec.FConst shr 22) and $3FC);
       end;
    1,
    5: begin // pushl/enter
       BRegIdx:=0;
       while BRegIdx<16 do
        begin
        if ((1 shl BRegIdx) and FExec.Subdec.FConst)<>0 then
         begin
         if BRegIdx=0 then BData:=FIpBeforeJmp
         else BData:=FRegList[(BRegIdx shr 1)+((BRegIdx shl 3) and $8)];
         if PushD(BData)=FALSE then break;
         end;
        inc(BRegIdx);
        end;
       if BRegIdx<>16 then break;
       if FExec.Subdec.FCccc=5 then FRegList[8]:=FRegList[8]-((FExec.Subdec.FConst shr 14) and $3FC);
       end;
  else begin
       ViewAny('eInvalid PPL command code [R:TCoreMS.ExecPPL]');
       break;
       end;
 end; // case
 if BUpdateIp then UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// RFU  |10111   ||        |        |
Function TCoreMS.ExecRFU : boolean;
Begin
 Result:=FALSE;
 ViewAny('eTrying to execute RFU command [R:TCoreMS.ExecRFU]');
End;

// ONS  |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
Function TCoreMS.ExecONS : boolean;
Var
  BNewIp    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BNewIp:=GetIp+FExec.Subdec.FConst;
 UpdateIp;
 FIpBeforeJmp:=GetIp; if JmpEn(FExec.Subdec.FFfff) then SetIp(BNewIp);
 Result:=TRUE;
 until TRUE;
End;

// FRUC |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
Function TCoreMS.ExecFRUC : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,12+(FExec.Subdec.FCccc and $7),0);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// FRRS |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
Function TCoreMS.ExecFRRS : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RdReg(rtS); BDataU:=RdReg(rtR);
 Result:=AluA(BDataR,BDataU,BDataS,12+(FExec.Subdec.FCccc and $7),0);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// BTR  |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
Function TCoreMS.ExecBTR : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FIiix; BDataU:=RdReg(rtR);
 if AluT(BDataR,BDataU,BDataS,FExec.Subdec.FCccc)=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// BTM  |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
Function TCoreMS.ExecBTM : boolean;
Var
  BAddr     : Cardinal;
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BAddr:=RdReg(rtA)+FExec.Subdec.FConst;
 if MioRdX(BAddr,1,BDataU)=FALSE then break;
 if AluT(BDataR,BDataU,FExec.Subdec.FIiix,FExec.Subdec.FCccc)=FALSE then break;
 if MioWrX(BAddr,1,BDataR)=FALSE then break;
 UpdateIp;
 Result:=TRUE;
 until TRUE;
End;

// ARUI   |11111100||        |uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd i)
Function TCoreMS.ExecARUI : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=FExec.Subdec.FConst; BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,FExec.Subdec.FCccc,FExec.Subdec.FE);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

// ARUS |11111101||        |uuuurrrr|cccceeww|wwwwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)
Function TCoreMS.ExecARUS : boolean;
Var
  BDataS,
  BDataU,
  BDataR    : Cardinal;
Begin
 Result:=FALSE;
 repeat
 BDataS:=RdReg(rtS); BDataU:=RdReg(rtU);
 Result:=AluA(BDataR,BDataU,BDataS,FExec.Subdec.FCccc,FExec.Subdec.FE);
 if Result=FALSE then break;
 WrReg(rtR,BDataR);
 UpdateIp;
 until TRUE;
End;

Procedure TCoreMS.Reset;
Var
  BRegIdx   : Integer;
Begin
 Inherited;
 FSubdecPrev:=ZMsSubdec; FIpBeforeJmp:=0;
 for BRegIdx:=0 to 15 do FRegList[BRegIdx]:=0;
 FRegList[8]:=$08000000 or (Cardinal(FCoreCnt) shl 8) or FCoreIdx;
 FExec.ResetErrors;
End;

Procedure TCoreMS.StepInto;
Var
  BCodeBin  : string;
Begin
 Inherited;
 repeat
 if FStuckAtError then break;
 MemRd(FRegMIp.FIp,6,BCodeBin);
 if BCodeBin='' then begin ViewAny('eCannot read data at address 0x'+IntToHex(FRegMIp.FIp,8)+'[R:TCoreRV.StepInto]'); FStuckAtError:=TRUE; break; end;
 if FExec.CmdDec(FRegMIp.FIp,BCodeBin)=FALSE then begin ViewAny(FExec.LastError); FStuckAtError:=TRUE; break; end;
 if FExecProcList[FExec.Subdec.FCmdCode]()=FALSE then begin FStuckAtError:=TRUE; break; end;
 FSubdecPrev:=FExec.Subdec;
 inc(FCycleCnt,2); inc(FInstRet);
 until TRUE;
End;

end.


