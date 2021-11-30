unit CoreRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, CoreBase_sd, DasmRV_sd, Math;

Type
  TCoreRV = class(TCoreBase)
  private
    FRegList    : array [0..15] of Cardinal; // 0 not used
    FExec       : TExecLineRV;

    Function RdReg ( ARegIdx : byte ) : Cardinal;
    Function RdRegI ( ARegIdx : byte ) : Integer;
    Procedure WrReg ( ARegIdx : byte; AData : Cardinal );
    Procedure UpdateIp ( ACmdLen : Cardinal );
    Procedure UpdateFlags ( ADataU, ADataS : Cardinal );
    Function JmpEn ( ADataU, ADataS : Cardinal; ACond : byte ) : boolean;
    Function VerboseExec : string;
    Function ExecCmd ( Const ASubdec : TRvSubdec ) : boolean;

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

// 'zero ra sp gp tp t0 t1 t2 s0 s1 a0  a1  a2  a3  a4  a5'

implementation

Uses
  ConComL;

Constructor TCoreRV.Create;
Begin
 Inherited;
 FExec:=TExecLineRV.Create;
 FCoreType:=$0; FRegMIp.FFlags:=FCoreType shl 4;
End;

Destructor TCoreRV.Destroy;
Begin
 FExec.Free;
 Inherited;
End;

Function TCoreRV.RegsAsStr : string;
Var
  BRegIdx   : Integer;
Begin
 Result:=IntToHex(FRegList[8],8)+IntToHex(ExportMIp,8);
 for BRegIdx:=1 to 7 do Result:=Result+IntToHex(FRegList[8+BRegIdx],8)+IntToHex(FRegList[0+BRegIdx],8);
 Result:=Result+IntToHex(FExtReg[1],8)+IntToHex(FExtReg[0],8);
End;

Function TCoreRV.RdRegs : string;
Var
  BRegIdx   : Integer;
Begin
 Result:=DWordAsStr(ExportMIp)+DWordAsStr(FRegList[8]);
 for BRegIdx:=1 to 7 do Result:=Result+DWordAsStr(FRegList[0+BRegIdx])+DWordAsStr(FRegList[8+BRegIdx]);
End;

Procedure TCoreRV.WrRegs ( Const ARegs : string );
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

Function TCoreRV.RdReg ( ARegIdx : byte ) : Cardinal;
Begin
 if ARegIdx=0 then Result:=0
 else Result:=FRegList[ARegIdx];
End;

Function TCoreRV.RdRegI ( ARegIdx : byte ) : Integer;
Begin
 if ARegIdx=0 then Result:=0
 else Result:=Integer(FRegList[ARegIdx]);
End;

Procedure TCoreRV.WrReg ( ARegIdx : byte; AData : Cardinal );
Begin
 if ARegIdx=0 then
 else FRegList[ARegIdx]:=AData;
End;

Procedure TCoreRV.UpdateIp ( ACmdLen : Cardinal );
Begin
 FRegMIp.FIp:=FRegMIp.FIp+ACmdLen;
End;

Procedure TCoreRV.UpdateFlags ( ADataU, ADataS : Cardinal );
Var
  BDataS,
  BDataU    : QWord;
  BDataR    : Cardinal;
  BDataRQ   : QWord;
  BFlagV    : array [0..2] of byte;
  BFlags    : byte;
Begin
 BDataRQ:=0; BDataR:=0; BFlags:=0;
 BDataU:=ADataU; BDataS:=ADataS xor $FFFFFFFF;
 BDataRQ:=BDataU+BDataS+1;
 BDataR:=BDataRQ;
 BFlagV[0]:=(BDataR shr 31) and $1;
 BFlagV[1]:=(BDataU shr 31) and $1;
 BFlagV[2]:=(BDataS shr 31) and $1;
 if (BDataRQ and $100000000)=0 then BFlags:=BFlags or $1;
 if BDataR=0 then BFlags:=BFlags or $2;
 if (BDataR and $80000000)<>0 then BFlags:=BFlags or $4;
 if (((BFlagV[1] xor BFlagV[2]) xor $1) and (BFlagV[1] xor BFlagV[0]))<>0 then BFlags:=BFlags or $8;
 SetFlagsA(BFlags);
End;

Function TCoreRV.JmpEn ( ADataU, ADataS : Cardinal; ACond : byte ) : boolean;
Var
  BSmaller  : byte;
  BFlags    : byte;
Begin
 UpdateFlags(ADataU,ADataS);
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
 BSmaller:=((BFlags shr 2) xor (BFlags shr 3)) and $1;
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

Function TCoreRV.VerboseExec : string;
Begin
 Result:='[CmdAddr: 0x'+IntToHex(FExec.Addr,8)+' | AsmLine: '+FExec.AsmLineS+']';
End;

Function TCoreRV.ExecCmd ( Const ASubdec : TRvSubdec ) : boolean;
Var
  BIpPrev   : Cardinal;
  BDataS,
  BDataU,
  BDataR    : Cardinal;
  BDataSF   : Single absolute BDataS;
  BDataUF   : Single absolute BDataU;
  BDataRF   : Single absolute BDataR;
  BAddr     : Cardinal;
  BIndex    : Integer;
  BDataSI,
  BDataUI,
  BDataRI   : Int64;
  BDataQ    : QWord;
Begin
 Result:=FALSE;
 FIsTrapHit:=FALSE; FIsTestEnd:=FALSE;
 repeat
 case FExec.CmdCode of
   rvccInvalid:
     begin
     ViewAny('eCmd code is invalid [R:TCoreRV.ExecCmd]');
     break;
     end;
   rvccAuipc:
     begin
     BDataU:=FRegMIp.FIp;
     BDataR:=Cardinal(Integer(BDataU)+ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLui:
     begin
     BDataR:=Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccAddi:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=Cardinal(Integer(BDataU)+ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccAndi:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU and Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccOri:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU or Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccXori:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU xor Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSlli:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU shl Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSrli:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU shr Cardinal(ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSrai:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU;
     BIndex:=ASubdec.FImm;
     while BIndex>0 do
      begin
      BDataR:=BDataR shr 1;
      if (BDataR and $40000000)<>0 then BDataR:=BDataR or $80000000;
      dec(BIndex);
      end;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSlti:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     if Integer(BDataU)<ASubdec.FImm then BDataR:=1 else BDataR:=0;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSltiu:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     if BDataU<Cardinal(ASubdec.FImm) then BDataR:=1 else BDataR:=0;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccAdd:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=Cardinal(Integer(BDataU)+Integer(BDataS));
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSub:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=Cardinal(Integer(BDataU)-Integer(BDataS));
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccAnd:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU and BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccOr:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU or BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccXor:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU xor BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSll:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU shl BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSrl:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU shr BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSra:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=BDataU;
     BIndex:=BDataS;
     while BIndex>0 do
      begin
      BDataR:=BDataR shr 1;
      if (BDataR and $40000000)<>0 then BDataR:=BDataR or $80000000;
      dec(BIndex);
      end;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSlt:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if Integer(BDataU)<Integer(BDataS) then BDataR:=1 else BDataR:=0;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSltu:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if BDataU<BDataS then BDataR:=1 else BDataR:=0;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccMul:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2);
     BDataUI:=RdRegI(ASubdec.FRs1);
     BDataRI:=BDataUI*BDataSI;
     WrReg(ASubdec.FRd,BDataRI);
     UpdateIp(Length(FExec.CodeBin));
     end;
   //rvccMul, rvccMulh, rvccMulhsu, rvccMulhu, rvccDiv, rvccDivu, rvccRem, rvccRemu,
   rvccMulh:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2);
     BDataUI:=RdRegI(ASubdec.FRs1);
     BDataRI:=BDataUI*BDataSI;
     WrReg(ASubdec.FRd,BDataRI shr 32);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccMulhsu:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2); BDataSI:=BDataSI and $FFFFFFFF;
     BDataUI:=RdRegI(ASubdec.FRs1);
     BDataRI:=BDataUI*BDataSI;
     WrReg(ASubdec.FRd,BDataRI shr 32);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccMulhu:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2); BDataSI:=BDataSI and $FFFFFFFF;
     BDataUI:=RdRegI(ASubdec.FRs1); BDataUI:=BDataUI and $FFFFFFFF;
     BDataRI:=BDataUI*BDataSI;
     WrReg(ASubdec.FRd,BDataRI shr 32);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccDiv:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2);
     BDataUI:=RdRegI(ASubdec.FRs1);
     if BDataSI=0 then BDataRI:=$FFFFFFFF
     else BDataRI:=BDataUI div BDataSI;
     WrReg(ASubdec.FRd,BDataRI);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccDivu:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if BDataS=0 then BDataR:=$FFFFFFFF
     else BDataR:=BDataU div BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccRem:
     begin
     BDataSI:=RdRegI(ASubdec.FRs2);
     BDataUI:=RdRegI(ASubdec.FRs1);
     if BDataSI=0 then BDataRI:=BDataUI
     else BDataRI:=BDataUI mod BDataSI;
     WrReg(ASubdec.FRd,BDataRI);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccRemu:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if BDataS=0 then BDataR:=BDataU
     else BDataR:=BDataU mod BDataS;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSb:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     BDataR:=RdReg(ASubdec.FRs2);
     if MioWrX(BAddr,1,BDataR)=FALSE then begin ViewAny('eError writing memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSh:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     BDataR:=RdReg(ASubdec.FRs2);
     if MioWrX(BAddr,2,BDataR)=FALSE then begin ViewAny('eError writing memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccSw:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     BDataR:=RdReg(ASubdec.FRs2);
     if MioWrX(BAddr,4,BDataR)=FALSE then begin ViewAny('eError writing memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLb:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     if MioRdX(BAddr,1,BDataR)=FALSE then begin ViewAny('eError reading memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     if (BDataR and $80)<>0 then BDataR:=BDataR or $FFFFFF00;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLh:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     if MioRdX(BAddr,2,BDataR)=FALSE then begin ViewAny('eError reading memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     if (BDataR and $8000)<>0 then BDataR:=BDataR or $FFFF0000;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLw:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     if MioRdX(BAddr,4,BDataR)=FALSE then begin ViewAny('eError reading memory at address 0x'+IntToHex(BAddr,8)+' '+VerboseExec+' [R:TCoreRV.ExecCmd]'); break; end;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLbu:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     if MioRdX(BAddr,1,BDataR)=FALSE then begin ViewAny('eError reading memory at address 0x'+IntToHex(BAddr,8)+' [R:TCoreRV.ExecCmd]'); break; end;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccLhu:
     begin
     BDataU:=RdReg(ASubdec.FRs1);
     BAddr:=Cardinal(BDataU+ASubdec.FImm);
     if MioRdX(BAddr,2,BDataR)=FALSE then begin ViewAny('eError reading memory at address 0x'+IntToHex(BAddr,8)+' [R:TCoreRV.ExecCmd]'); break; end;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccJ:
     begin
     UpdateIp(Cardinal(ASubdec.FImm));
     end;
   rvccJal:
     begin
     WrReg(ASubdec.FRd,FRegMIp.FIp+Length(FExec.CodeBin));
     UpdateIp(Cardinal(ASubdec.FImm));
     end;
   rvccJalr:
     begin
     BIpPrev:=FRegMIp.FIp;
     BDataS:=FRegMIp.FIp+Length(FExec.CodeBin);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataR:=Cardinal(Integer(BDataU)+ASubdec.FImm);
     WrReg(ASubdec.FRd,BDataS);
     FRegMIp.FIp:=BDataR;
     if Assigned(FOnDasmMissing) then
      begin
      if FOnDasmMissing(BIpPrev,FRegMIp.FIp)=FALSE then break;
      end;
     end;

   //CBraListA = 'bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv';
   //CBraListB = 'bra bbe bc bae be bne ba bany bg bge bs bse bn bv bnn bnv';
   rvccBeq:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if JmpEn(BDataU,BDataS,4) then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccBne:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if JmpEn(BDataU,BDataS,5) then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccBge:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     //JmpEn(BDataU,BDataS,9);
     if JmpEn(BDataU,BDataS,9) {Integer(BDataU)>=Integer(BDataS)} then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccBgeu:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if JmpEn(BDataU,BDataS,3) then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccBlt:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if JmpEn(BDataU,BDataS,10) then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccBltu:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if JmpEn(BDataU,BDataS,2) then UpdateIp(Cardinal(ASubdec.FImm))
     else UpdateIp(Length(FExec.CodeBin));
     end;
   rvccFence:
     begin
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccEcall:
     begin
     FIsTrapHit:=TRUE; FIsTestEnd:=TRUE;
     UpdateIp(Length(FExec.CodeBin));
     end;

   // FP group
   rvccFAdd:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataRF:=BDataUF+BDataSF;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccFSub:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataRF:=BDataUF-BDataSF;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccFMul:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     BDataRF:=BDataUF*BDataSF;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;
   rvccFDiv:
     begin
     BDataS:=RdReg(ASubdec.FRs2);
     BDataU:=RdReg(ASubdec.FRs1);
     if (BDataS and $7FFFFFFF)=0 then BDataRF:=Infinity
     else BDataRF:=BDataUF/BDataSF;
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;

   // CSR group
   rvccCsrRS:
     begin
     case ASubdec.FImm of
       $C00: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataR:=FCycleCnt and $FFFFFFFF;
             end;
       $C01: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataQ:=GetTickCount64; BDataR:=BDataQ and $FFFFFFFF;
             end;
       $C02: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataR:=FInstRet and $FFFFFFFF;
             end;
       $C80: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataR:=FCycleCnt shr 32;
             end;
       $C81: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataQ:=GetTickCount64; BDataR:=BDataQ shr 32;
             end;
       $C82: begin
             if ASubdec.FRs1<>0 then begin ViewAny('eCounter is read only [R:TCoreRV.ExecCmd]'); break; end;
             BDataR:=FInstRet shr 32;
             end;
       else  begin
             ViewAny('eUnsupported register: 0x'+IntToHex(ASubdec.FImm,3)+' [R:TCoreRV.ExecCmd]');
             break;
             end;
     end; // case
     WrReg(ASubdec.FRd,BDataR);
     UpdateIp(Length(FExec.CodeBin));
     end;

   else
     begin
     ViewAny('eUnknown cmd code (perhaps not implemented) [R:TCoreRV.ExecCmd]');
     break;
     end;
 end;// case
 Result:=TRUE;
 until TRUE;
End;

Procedure TCoreRV.Reset;
Var
  BRegIdx   : Integer;
Begin
 Inherited;
 for BRegIdx:=0 to 15 do FRegList[BRegIdx]:=0;
 FRegList[8]:=$08000000 or (Cardinal(FCoreCnt) shl 8) or FCoreIdx;
 FExec.ResetErrors;
End;

Procedure TCoreRV.StepInto;
Var
  BCodeBin  : string;
Begin
 Inherited;
 repeat
 if FStuckAtError then break;
 MemRd(FRegMIp.FIp,6,BCodeBin);
 if BCodeBin='' then begin ViewAny('eCannot read data at address 0x'+IntToHex(FRegMIp.FIp,8)+'[R:TCoreRV.StepInto]'); FStuckAtError:=TRUE; break; end;
 if FExec.CmdDec(FRegMIp.FIp,BCodeBin)=FALSE then begin ViewAny(FExec.LastError); FStuckAtError:=TRUE; break; end;
 if ExecCmd(FExec.Subdec)=FALSE then begin FStuckAtError:=TRUE; break; end;
 inc(FCycleCnt,2); inc(FInstRet);
 until TRUE;
End;

end.

