unit AsmMcuRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, AsmHelperRV_sd, ParsRV_sd, BackEndRV_sd;

Const
  CRegCount = 16;

Type
  TAsmMcuRV = class(TAsmHelperRiscV)
  private
    FIsCSupp    : boolean; // C-type supported

    FRegNamesA  : array [0..CRegCount-1] of string [4];
    FRegNamesB  : array [0..CRegCount-1] of string [4];
    FRegNamesS  : string;

    Function GetXMem ( AParam : TAsmLineParam; Out ARegIdx : Integer; Out AConst : Integer; Out ARefCode : char; Out ARef : string; Out AFieldAddr : Cardinal ) : boolean;
    Function GetRegIndex ( Const ARegName : string ) : Integer;
    Function CmdGenR ( AFunc7 : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
    Function CmdGenI ( AImm : Cardinal; ARS1 : Cardinal; AFunc3 : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
    Function CmdGenS ( AImm : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; AOpcode : Cardinal ) : Cardinal;
    Function CmdGenB ( AImm : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; AOpcode : Cardinal ) : Cardinal;
    Function CmdGenU ( AImm : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
    Function CmdGenJ ( AImm : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;

    Function CodeGenAluRR ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAluRI ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenSwtEjal ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenJalJalr ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenPseudo ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenLx ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenSx ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenLui ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAuipc ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenBx ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAluU ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenFpuRR ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAmo ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenFence ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenCsr ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenOther ( ALine : TAsmFlowLine ) : TCmdCompError;

  protected
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure CodeGenCmd ( ALine : TAsmFlowLine ); Override;
  end;

  TCppMcuRV = class(TAsmMcuRV)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

Const
  CRegNamesRVA = 'x0   x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15';
  CRegNamesRVB = 'zero ra sp gp tp t0 t1 t2 s0 s1 a0  a1  a2  a3  a4  a5';

Const
  CRvRegZ   = 0;
  CRvRegRA  = 1;

implementation

Uses
  ConComL;

// Common

Procedure FixConst64 ( Var AConst64 : Int64 );
Begin
 if ((AConst64 and $80000000)<>0) and ((AConst64 and $FFFFFFFF00000000)=0) then AConst64:=AConst64 or $FFFFFFFF00000000;
End;

// CPP

Constructor TCppMcuRV.Create;
Begin
 Inherited;
 FModule:=TModuleRV.Create;
 FParser:=TParsRV.Create;
End;

Destructor TCppMcuRV.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;

// ASM

Constructor TAsmMcuRV.Create;
Var
  BRegNames     : string;
  BIndex        : Integer;
Begin
 Inherited;
 FRegNamesS:=CRegNamesRVA+' '+CRegNamesRVB;
 BRegNames:=CRegNamesRVA; for BIndex:=0 to (CRegCount-1) do FRegNamesA[BIndex]:=ReadParamStr(BRegNames);
 BRegNames:=CRegNamesRVB; for BIndex:=0 to (CRegCount-1) do FRegNamesB[BIndex]:=ReadParamStr(BRegNames);
 FAddressParamType:='d';
 FIsCSupp:=TRUE;
End;

Destructor TAsmMcuRV.Destroy;
Begin
 Inherited;
End;

{ *** TAsmMcuRV *** }

Function TAsmMcuRV.GetXMem ( AParam : TAsmLineParam; Out ARegIdx : Integer; Out AConst : Integer; Out ARefCode : char; Out ARef : string; Out AFieldAddr : Cardinal ) : boolean;
Var
  BLabel        : string;
  BConstS       : string;
  BRegNameS     : string;
  BRef          : string;
  BPos          : Integer;
Begin
 Result:=FALSE;
 ARegIdx:=-1; AConst:=0; ARefCode:=#0; ARef:=''; AFieldAddr:=0;

 repeat
 BLabel:=AParam.Name;
 if Pos('%lo',BLabel)=1 then ARefCode:='L'
 else if Pos('%hi',BLabel)=1 then ARefCode:='H';
 if ARefCode<>#0 then
  begin
  Delete(BLabel,1,3);
  if Copy(BLabel,1,1)<>'(' then begin AParam.AppendError('e','"(" is expected after a part directive [R:TAsmMcuRV.GetXMem]'); break; end;
  Delete(BLabel,1,1);
  BRef:=ReadTillCNoDel(BLabel,')');
  if BRef='' then begin AParam.AppendError('e','Reference is absent [R:TAsmMcuRV.GetXMem]'); break; end;
  if Copy(BLabel,1,1)<>')' then begin AParam.AppendError('e','")" is expected after a reference [R:TAsmMcuRV.GetXMem]'); break; end;
  Delete(BLabel,1,1);
  BPos:=Pos('+',BRef);
  if BPos=0 then ARef:=BRef
  else
   begin
   ARef:=Copy(BRef,1,BPos-1);
   Delete(BRef,1,BPos);
   DelFirstLastSpace(BRef); BConstS:=BRef;
   if TryStrToDWord(BConstS,AFieldAddr)=FALSE then begin AParam.AppendError('e','Cannot convert constant to integer [R:TAsmMcuRV.GetXMem]'); break; end;
   end;
  end
 else
  begin
  BConstS:=ReadTillCNoDel(BLabel,'(');
  if BConstS='' then begin AParam.AppendError('e','Constant is expected [R:TAsmMcuRV.GetXMem]'); break; end;
  if TryStrToInt(BConstS,AConst)=FALSE then begin AParam.AppendError('e','Cannot convert constant to integer [R:TAsmMcuRV.GetXMem]'); break; end;
  end;
 DelFirstSpace(BLabel);
 if BLabel='' then begin Result:=TRUE; break; end;
 if BLabel[1]<>'(' then begin AParam.AppendError('e','"(" is expected to define a register [R:TAsmMcuRV.GetXMem]'); break; end;
 Delete(BLabel,1,1);
 BRegNameS:=ReadTillCNoDel(BLabel,')');
 if BRegNameS='' then begin AParam.AppendError('e','Register name is absent [R:TAsmMcuRV.GetXMem]'); break; end;
 if Copy(BLabel,1,1)<>')' then begin AParam.AppendError('e','")" is expected after a register name [R:TAsmMcuRV.GetXMem]'); break; end;
 Delete(BLabel,1,1);
 DelFirstSpace(BLabel);
 if BLabel<>'' then begin AParam.AppendError('e','Extra parameter in line: "'+BLabel+'" [R:TAsmMcuRV.GetXMem]'); break; end;
 ARegIdx:=GetRegIndex(BRegNameS);
 if ARegIdx=-1 then begin AParam.AppendError('e','Invalid register name [R:TAsmMcuRV.GetXMem]'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TAsmMcuRV.GetRegIndex ( Const ARegName : string ) : Integer;
Begin
 Result:=CRegCount-1;
 while Result>=0 do
  begin
  if FRegNamesA[Result]=ARegName then break;
  if FRegNamesB[Result]=ARegName then break;
  dec(Result);
  end;
End;

Function TAsmMcuRV.CmdGenR ( AFunc7 : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=(AFunc7 shl 25) or
         (ARS2 shl 20) or
         (ARS1 shl 15) or
         (AFunc3 shl 12) or
         (ARD shl 7) or
         AOpcode;
End;

Function TAsmMcuRV.CmdGenI ( AImm : Cardinal; ARS1 : Cardinal; AFunc3 : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=((AImm and $FFF) shl 20) or
         (ARS1 shl 15) or
         (AFunc3 shl 12) or
         (ARD shl 7) or
         AOpcode;
End;

Function TAsmMcuRV.CmdGenS ( AImm : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=((AImm and $FE0) shl 20) or
         (ARS2 shl 20) or
         (ARS1 shl 15) or
         (AFunc3 shl 12) or
         ((AImm and $01F) shl 7) or
         AOpcode;
End;

Function TAsmMcuRV.CmdGenB ( AImm : Cardinal; ARS2, ARS1 : Cardinal; AFunc3 : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=((AImm and $1000) shl (31-12)) or
         ((AImm and $07E0) shl (25-5)) or
         (ARS2 shl 20) or
         (ARS1 shl 15) or
         (AFunc3 shl 12) or
         ((AImm and $001E) shl (8-1)) or
         ((AImm and $0800) shr (11-7)) or
         AOpcode;
End;

Function TAsmMcuRV.CmdGenU ( AImm : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=(AImm and $FFFFF000) or
         (ARD shl 7) or
         AOpcode;
End;

Function TAsmMcuRV.CmdGenJ ( AImm : Cardinal; ARD : Cardinal; AOpcode : Cardinal ) : Cardinal;
Begin
 Result:=((AImm and $0100000) shl (31-20)) or
         ((AImm and $00007FE) shl (21-1)) or
         ((AImm and $0000800) shl (20-11)) or
         ((AImm and $00FF000) shl (12-12)) or
         (ARD shl 7) or
         AOpcode;
End;

Procedure TAsmMcuRV.CodeGenCmd ( ALine : TAsmFlowLine );
Var
  BResult       : TCmdCompError;

Begin
 ALine.ClearDataBin; ALine.ClearRef;

 repeat
 //if Length(ALine.Params)=0 then break;

 BResult:=CodeGenAluRR(ALine);           if BResult<>cceCheckNext then break;
 BResult:=CodeGenAluRI(ALine);           if BResult<>cceCheckNext then break;
 BResult:=CodeGenSwtEjal(ALine);         if BResult<>cceCheckNext then break;
 BResult:=CodeGenJalJalr(ALine);         if BResult<>cceCheckNext then break;
 BResult:=CodeGenPseudo(ALine);          if BResult<>cceCheckNext then break;
 BResult:=CodeGenLx(ALine);              if BResult<>cceCheckNext then break;
 BResult:=CodeGenSx(ALine);              if BResult<>cceCheckNext then break;
 BResult:=CodeGenLui(ALine);             if BResult<>cceCheckNext then break;
 BResult:=CodeGenAuipc(ALine);           if BResult<>cceCheckNext then break;
 BResult:=CodeGenBx(ALine);              if BResult<>cceCheckNext then break;
 BResult:=CodeGenAluU(ALine);            if BResult<>cceCheckNext then break;
 BResult:=CodeGenFpuRR(ALine);           if BResult<>cceCheckNext then break;
 BResult:=CodeGenAmo(ALine);             if BResult<>cceCheckNext then break;
 BResult:=CodeGenFence(ALine);           if BResult<>cceCheckNext then break;
 BResult:=CodeGenCsr(ALine);             if BResult<>cceCheckNext then break;
 BResult:=CodeGenOther(ALine);           if BResult<>cceCheckNext then break;

 ALine.AppendError('e','Invalid command code "'+ALine.Params[0].Name+'" or command does not exist with this list of parameters "'+ALine.Exec+'" [R:TAsmMcuRV.CodeGenCmd]');
 until TRUE;
End;

Function TAsmMcuRV.CodeGenAluRR ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3,
  BFunc7        : byte;
  BRdIdx,
  BRs1Idx,
  BRs2Idx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BFunc7:=0;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'add sll slt sltu xor srl or and');
 if BCmdIdx=-1 then
  begin
  BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'sub # # # # sra # #');
  BFunc7:=$20;
  end;

 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  BRs2Idx:=GetRegIndex(ALine.Params[3].Name); if BRs2Idx<0 then break;
  ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));
  Result:=cceCompiled;
  break;
  end;

 // sltz rd,rs1 = slt rd,rs,x0
 BFunc7:=0;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # sltz # # # # #');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  BRs2Idx:=CRvRegZ;
  ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));
  Result:=cceCompiled;
  break;
  end;

 // snez rd,rs1 = sltu rd,x0,rs
 // sgtz rd,rs1	= slt rd,x0,rs
 BFunc7:=0;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # sgtz snez # # # #');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=CRvRegZ;
  BRs2Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;

Function TAsmMcuRV.CodeGenAluRI ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3        : byte;
  BRdIdx,
  BRs1Idx       : Integer;
  BConst64      : Int64;
  BConst        : Cardinal;
  BConstI       : Integer;
  BRegIdx       : Integer;
  BRefCode      : char;
  BRef          : string;
  BFieldAddr    : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'addi # slti sltiu xori # ori andi');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  Result:=cceError;
  if TryIntegerEqu(ALine.Params[3],BConst64) then
   begin
   FixConst64(BConst64);
   if (BConst64>2047) or (BConst64<-2048) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
   BConst:=Cardinal(BConst64) and $FFF;
   ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,BFunc3,BRdIdx,$13));
   Result:=cceCompiled;
   break;
   end;
  if GetXMem(ALine.Params[3],BRegIdx,BConstI,BRefCode,BRef,BFieldAddr)=FALSE then break;
  if BRegIdx<>-1 then begin ALine.Params[3].AppendError('e','Immediate should not include register [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
  if (BConstI>2047) or (BConstI<-2048) then begin ALine.Params[3].AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
  BConst:=BConstI;
  if BRefCode='H' then begin ALine.Params[3].AppendError('e','Invalid reference type [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
  ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,BFunc3,BRdIdx,$13));
  if BRef<>'' then ALine.AppendRef(ALine.Params[3],BRef,'I',0,BFieldAddr);
  Result:=cceCompiled;
  break;
  end;

 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# slli # # # srli # #');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  Result:=cceError;
  if TryIntegerEqu(ALine.Params[3],BConst64) then
   begin
   if (BConst64>31) or (BConst64<-31) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
   BConst:=Cardinal(BConst64) and $FFF;
   ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,BFunc3,BRdIdx,$13));
   Result:=cceCompiled;
   break;
   end;
  break;
  end;

 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # # # srai # #');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  Result:=cceError;
  if TryIntegerEqu(ALine.Params[3],BConst64) then
   begin
   if (BConst64>31) or (BConst64<-31) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
   BConst:=Cardinal(BConst64) and $FFF;
   ALine.AppendDataBinD(CmdGenI(BConst or $400,BRs1Idx,BFunc3,BRdIdx,$13));
   Result:=cceCompiled;
   break;
   end;
  break;
  end;

 // seqz rd,rs1 = sltiu rd,rs,1
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # seqz # # # #');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  BConst:=1;
  ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,BFunc3,BRdIdx,$13));
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;

Function TAsmMcuRV.CodeGenSwtEjal ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BRef      : string;
  BRdIdx,
  BRs1Idx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name='swt' then
  begin
  if Length(ALine.Params)<>2 then break;
  ALine.AppendDataBinD(CmdGenI(0,CRvRegZ,$01,CRvRegZ,$67)); // RS1 is always Z
  BRef:=ALine.Params[1].Name;
  ALine.AppendRef(ALine.Params[1],BRef,'T',0,0);
  ALine.SetDstLabel(BRef);
  ALine.IsJmp:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='ecall' then
  begin
  // call offset	auipc x6, offset >> 12; jalr x1,x6,offset & 0xfff
  // call offset	lui x6, offset >> 12; jalr x1,x6,offset & 0xfff <- Replaced by absolute address
  {if Length(ALine.Params)<>2 then break;
  ALine.AppendDataBinD(CmdGenU(0,$6,$37));      ALine.AppendRef(ALine.Params[1],'U',0);
  ALine.AppendDataBinD(CmdGenI(0,6,$02,1,$67)); ALine.AppendRef(ALine.Params[1],'I',4);}
  if Length(ALine.Params)<>1 then break;
  ALine.AppendDataBinD(CmdGenU(0,$0,$73));
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='eret' then
  begin
  if Length(ALine.Params)=2 then // Only register
   begin
   Result:=cceError;
   BRdIdx:=CRvRegRA;
   BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then begin ALine.Params[1].AppendError('e','Invalid source register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   ALine.AppendDataBinD(CmdGenI(0,BRs1Idx,$02,BRdIdx,$67));
   ALine.IsIpLoad:=TRUE;
   Result:=cceCompiled;
   break;
   end;

  ALine.AppendError('e','Invalid target register [R:TAsmMcuRV.CodeGenJalJalr]');
  Result:=cceError;
  break;
  end;


 until TRUE;
End;

Function TAsmMcuRV.CodeGenJalJalr ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BRdIdx,
  BRs1Idx   : Integer;
  BConst64  : Int64;
  BConst    : Cardinal;
  BRef      : string;
  BParamCR  : string;
  BConstS,
  BRegSrcS  : string;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name='j' then
  begin
  if Length(ALine.Params)<>2 then break;
  BRef:=ALine.Params[1].Name;
  if IsIntegerEqu(BRef,BConst64) then
   begin
   ALine.AppendDataBinD(CmdGenJ(Cardinal(BConst64)-ALine.Addr,CRvRegZ,$6F));
   end
  else if FIsCSupp then // c.j
   begin
   ALine.AppendDataBinW($A001);
   ALine.AppendRef(ALine.Params[1],BRef,'j',0,0);
   ALine.SetDstLabel(BRef);
   end
  else
   begin
   ALine.AppendDataBinD(CmdGenJ(0,CRvRegZ,$6F));
   ALine.AppendRef(ALine.Params[1],BRef,'J',0,0);
   ALine.SetDstLabel(BRef);
   end;
  ALine.IsJmp:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='jal' then
  begin
  if Length(ALine.Params)<>3 then break;
  Result:=cceError;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then begin ALine.AppendError('e','Invalid target register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
  BRef:=ALine.Params[2].Name;
  if IsIntegerEqu(BRef,BConst64) then
   begin
   ALine.AppendDataBinD(CmdGenJ(Cardinal(BConst64)-ALine.Addr,BRdIdx,$6F));
   end
  else
   begin
   ALine.AppendDataBinD(CmdGenJ(0,BRdIdx,$6F));
   ALine.AppendRef(ALine.Params[2],BRef,'J',0,0);
   end;
  ALine.SetDstLabel(BRef);
  ALine.IsJmp:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='jalr' then
  begin
  if Length(ALine.Params)=2 then // Only register
   begin
   Result:=cceError;
   BRdIdx:=CRvRegRA;
   BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then begin ALine.Params[1].AppendError('e','Invalid source register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   ALine.AppendDataBinD(CmdGenI(0,BRs1Idx,$00,BRdIdx,$67));
   ALine.IsJmp:=TRUE;
   Result:=cceCompiled;
   break;
   end;

  if Length(ALine.Params)=3 then // Register and constant
   begin
   Result:=cceError;
   BRdIdx:=GetRegIndex(ALine.Params[1].Name); if BRdIdx<0 then begin ALine.Params[1].AppendError('e','Invalid dst register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   BParamCR:=ALine.Params[2].Name;
   if ReadTillC(BParamCR,BConstS,'(')=FALSE then BRegSrcS:=''
   else
    begin
    Delete(BParamCR,1,1);
    if ReadTillC(BParamCR,BRegSrcS,')')=FALSE then begin ALine.Params[2].AppendError('e','")" is missing [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
    Delete(BParamCR,1,1);
    end;
   if BRegSrcS='' then BRs1Idx:=0
   else
    begin
    BRs1Idx:=GetRegIndex(BRegSrcS);
    if BRs1Idx<0 then begin ALine.Params[1].AppendError('e','Invalid source register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
    end;
   if BConstS='' then BConst64:=0
   else
    begin
    if IsIntegerEqu(BConstS,BConst64)=FALSE then begin ALine.Params[2].AppendError('e','Invalid constant [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
    end;
   if (BConst64>2047) or (BConst64<-2048) then
    begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
   BConst:=Cardinal(BConst64) and $FFF;
   ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,$00,BRdIdx,$67));
   ALine.IsJmp:=TRUE;
   Result:=cceCompiled;
   break;
   end;

  if Length(ALine.Params)=4 then // 2 Registers and constant
   begin
   Result:=cceError;
   BRdIdx:=GetRegIndex(ALine.Params[1].Name); if BRdIdx<0 then begin ALine.Params[1].AppendError('e','Invalid dst register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then begin ALine.Params[2].AppendError('e','Invalid src register [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   if TryIntegerEqu(ALine.Params[3],BConst64)=FALSE then begin ALine.Params[3].AppendError('e','Invalid Constant [R:TAsmMcuRV.CodeGenJalJalr]'); break; end;
   if (BConst64>2047) or (BConst64<-2048) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
   BConst:=Cardinal(BConst64) and $FFF;
   ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,$00,BRdIdx,$67));
   ALine.IsJmp:=TRUE;
   Result:=cceCompiled;
   break;
   end;

  ALine.AppendError('e','Invalid target register [R:TAsmMcuRV.CodeGenJalJalr]');
  Result:=cceError;
  break;
  end;


 until TRUE;
End;

{
Pseudo-instruction	Translation
nop	        addi x0,x0,0
mv dest,src	addi dest,src,0
not dest,src	xor dest,src,-1
seqz dest,src	sltiu dest,src,1
snez dest,src	sltu dest,x0,src
j J	        jal x0,J
call offset	auipc x6, offset >> 12; jalr x1,x6,offset & 0xfff
li dest,value	(possibly several instructions to load arbitrary value)
la dest,symbol	auipc dest, symbol >> 12; addi dest,dest,offset & 0xfff
l[d|w|h|b] dest,symbol	auipc dest, symbol >> 12; lx dest,offset & 0xfff(dest)
s[d|w|h|b] src,symbol	auipc dest, symbol >> 12; sx dest,offset & 0xfff(dest)
}

Function TAsmMcuRV.CodeGenPseudo ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BRegIdx       : Integer;
  BConst64      : Int64;
  BConst        : Cardinal;
  BCmdIdx       : Integer;
  BFunc3,
  BFunc7        : byte;
  BRdIdx,
  BRs1Idx,
  BRs2Idx       : Integer;
  BEval         : TVarData;
  BProcessedA   : boolean;
  BRef          : string;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name='ret' then
  begin
  // ret = jalr x0,x1,0
  if Length(ALine.Params)<>1 then break;
  if FIsCSupp then ALine.AppendDataBinW($8082) // c.jr
  else ALine.AppendDataBinD(CmdGenI(0,1,0,0,$67));
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='jr' then
  begin
  // jr R = jalr x0,R,0
  if Length(ALine.Params)<>2 then break;
  BRegIdx:=GetRegIndex(ALine.Params[1].Name);
  if BRegIdx=-1 then begin ALine.Params[1].AppendError('e','Invalid register [R:TAsmMcuRV.CodeGenPseudo]'); Result:=cceError; break; end;
  ALine.AppendDataBinD(CmdGenI(0,BRegIdx,0,0,$67));
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='li' then
  begin
  if Length(ALine.Params)<>3 then break;
  BRegIdx:=GetRegIndex(ALine.Params[1].Name);
  if BRegIdx=-1 then begin ALine.Params[1].AppendError('e','Invalid register [R:TAsmMcuRV.CodeGenPseudo]'); Result:=cceError; break; end;
  if AtomizeEval(ALine.Params[2].Name,BEval,BProcessedA)=FALSE then begin ALine.Params[2].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenPseudo]'); Result:=cceError; break; end;
  if BEval.FType<>'i' then begin ALine.Params[2].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenPseudo]'); Result:=cceError; break; end;
  if BProcessedA then ALine.Params[2].NameA:='0x'+IntToHex(Cardinal(BEval.FDataI),8);
  BConst64:=BEval.FDataI;
  BConst:=Cardinal(BConst64);
  if (BConst and $FFFFF800)=0 then
   begin
   ALine.AppendDataBinD(CmdGenI(BConst and $FFF,CRvRegZ,$0,BRegIdx,$13)); // addi
   end
  else if ((BConst and $800)<>0) and ((BConst and $FFFFF000)=$FFFFF000) then
   begin
   ALine.AppendDataBinD(CmdGenI(BConst and $FFF,CRvRegZ,$0,BRegIdx,$13)); // addi
   end
  else if (BConst and $FFF)=0 then
   begin
   ALine.AppendDataBinD(CmdGenU(RiscV_CorrectLui(BConst),BRegIdx,$37)); // lui
   end
  else
   begin
   ALine.AppendDataBinD(CmdGenU(RiscV_CorrectLui(BConst),BRegIdx,$37)); // LUI
   ALine.AppendDataBinD(CmdGenI(BConst,BRegIdx,$0,BRegIdx,$13)); // addi
   end;
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='la' then
  begin
  if Length(ALine.Params)<>3 then break;
  BRegIdx:=GetRegIndex(ALine.Params[1].Name);
  ALine.AppendDataBinD(CmdGenU(0,BRegIdx,$37)); ALine.AppendRef(ALine.Params[2],'U',0); // LUI
  ALine.AppendDataBinD(CmdGenI(0,BRegIdx,$0,BRegIdx,$13)); ALine.AppendRef(ALine.Params[2],'I',4); // Addi
  Result:=cceCompiled;
  break;
  end;

 {if ALine.Params[0].Name='call' then
  begin
  // call offset	auipc x6, offset >> 12; jalr x1,x6,offset & 0xfff
  // call offset	lui x6, offset >> 12; jalr x1,x6,offset & 0xfff <- Replaced by absolute address
  if Length(ALine.Params)<>2 then break;
  ALine.AppendDataBinD(CmdGenU(0,$6,$37));     ALine.AppendRef(ALine.Params[1],'U',0);
  ALine.AppendDataBinD(CmdGenI(0,6,$0,1,$67)); ALine.AppendRef(ALine.Params[1],'I',4);
  Result:=cceCompiled;
  break;
  end;}
 if ALine.Params[0].Name='call' then
  begin
  if Length(ALine.Params)<>2 then break;
  Result:=cceError;
  BRdIdx:=1;
  BRef:=ALine.Params[1].Name;
  ALine.AppendDataBinD(CmdGenJ(0,BRdIdx,$6F));
  ALine.AppendRef(ALine.Params[1],BRef,'J',0,0);
  ALine.SetDstLabel(BRef);
  ALine.IsCall:=TRUE;
  Result:=cceCompiled;
  break;
  end;


 if ALine.Params[0].Name='mv' then
  begin
  if Length(ALine.Params)<>3 then break;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
  ALine.AppendDataBinD(CmdGenR(0,0,BRs1Idx,6,BRdIdx,$33));
  Result:=cceCompiled;
  break;
  end;

 // neg rd, rs == sub rd, x0, rs
 if ALine.Params[0].Name='neg' then
  begin
  BCmdIdx:=0; BFunc7:=$20;
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
  BRs1Idx:=0;
  BRs2Idx:=GetRegIndex(ALine.Params[2].Name); if BRs2Idx<0 then break;
  ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;

// lw      a0,%lo(FDataY)(s0)
Function TAsmMcuRV.CodeGenLx ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3        : byte;
  BRegIdx       : Integer;
  BAddrReg      : Integer;
  BConstI       : Integer;
  BConst        : Cardinal;
  BRefCode      : char;
  BRef          : string;
  BFieldAddr    : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'lb lh lw # lbu lhu');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>3 then break;
 BFunc3:=BCmdIdx;

 Result:=cceError;
 BRegIdx:=GetRegIndex(ALine.Params[1].Name); if BRegIdx<0 then begin ALine.Params[1].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenLx]'); break; end;
 if GetXMem(ALine.Params[2],BAddrReg,BConstI,BRefCode,BRef,BFieldAddr)=FALSE then break;
 if BAddrReg=-1 then begin ALine.Params[2].AppendError('e','Invalid address register index (or address register is absent) [R:TAsmMcuRV.CodeGenLx]'); break; end;

 if (BConstI>2047) or (BConstI<-2048) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
 BConst:=Cardinal(BConstI) and $FFF;

 ALine.AppendDataBinD(CmdGenI(BConst,BAddrReg,BFunc3,BRegIdx,$03));
 if BRefCode='L' then BRefCode:='I'
 else if BRefCode='H' then begin ALine.Params[2].AppendError('e','Invalid reference type [R:TAsmMcuRV.CodeGenAluRI]'); break; end;
 if BRef<>'' then ALine.AppendRef(ALine.Params[2],BRef,BRefCode,0,BFieldAddr);

 Result:=cceCompiled;
 until TRUE;
End;

// sw      s0,4(sp)
Function TAsmMcuRV.CodeGenSx ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3        : byte;
  BRegIdx       : Integer;
  BAddrReg      : Integer;
  BConstI       : Integer;
  BConst        : Cardinal;
  BRefCode      : char;
  BRef          : string;
  BFieldAddr    : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'sb sh sw');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>3 then break;
 BFunc3:=BCmdIdx;

 Result:=cceError;
 BRegIdx:=GetRegIndex(ALine.Params[1].Name); if BRegIdx<0 then begin ALine.Params[1].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenSx]'); break; end;
 if GetXMem(ALine.Params[2],BAddrReg,BConstI,BRefCode,BRef,BFieldAddr)=FALSE then break;
 if BAddrReg=-1 then begin ALine.Params[2].AppendError('e','Invalid address register index (or address register is absent) [R:TAsmMcuRV.CodeGenSx]'); break; end;

 if (BConstI>2047) or (BConstI<-2048) then begin ALine.AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenSx]'); break; end;
 BConst:=Cardinal(BConstI) and $FFF;

 ALine.AppendDataBinD(CmdGenS(BConst,BRegIdx,BAddrReg,BFunc3,$23));
 if BRefCode='L' then BRefCode:='S'
 else if BRefCode='H' then begin ALine.Params[2].AppendError('e','Invalid reference type [R:TAsmMcuRV.CodeGenSx]'); break; end;
 if BRef<>'' then ALine.AppendRef(ALine.Params[2],BRef,BRefCode,0,BFieldAddr);

 Result:=cceCompiled;
 until TRUE;
End;

// sw      s0,4(sp)
Function TAsmMcuRV.CodeGenLui ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BRegIdx       : Integer;
  BAddrReg      : Integer;
  BConstI       : Integer;
  BConst        : Cardinal;
  BRefCode      : char;
  BRef          : string;
  BFieldAddr    : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name<>'lui' then break;
 if Length(ALine.Params)<>3 then break;

 Result:=cceError;
 BRegIdx:=GetRegIndex(ALine.Params[1].Name); if BRegIdx<0 then begin ALine.Params[1].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenLui]'); break; end;
 if GetXMem(ALine.Params[2],BAddrReg,BConstI,BRefCode,BRef,BFieldAddr)=FALSE then break;
 if BAddrReg<>-1 then begin ALine.Params[2].AppendError('e','Address register must be absent [R:TAsmMcuRV.CodeGenLui]'); break; end;

 BConst:=BConstI;

 ALine.AppendDataBinD(CmdGenU(BConst,BRegIdx,$37));
 if BRefCode='L' then begin ALine.Params[2].AppendError('e','Invalid reference type [R:TAsmMcuRV.CodeGenLui]'); break; end;
 if BRefCode='H' then BRefCode:='U';
 if BRef<>'' then ALine.AppendRef(ALine.Params[2],BRef,BRefCode,0,BFieldAddr);

 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuRV.CodeGenAuipc ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BRegIdx       : Integer;
  BAddrReg      : Integer;
  BConstI       : Integer;
  BConst        : Cardinal;
  BRefCode      : char;
  BRef          : string;
  BFieldAddr    : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name<>'auipc' then break;
 if Length(ALine.Params)<>3 then break;

 Result:=cceError;
 BRegIdx:=GetRegIndex(ALine.Params[1].Name); if BRegIdx<0 then begin ALine.Params[1].AppendError('e','Invalid immediate [R:TAsmMcuRV.CodeGenLui]'); break; end;
 if GetXMem(ALine.Params[2],BAddrReg,BConstI,BRefCode,BRef,BFieldAddr)=FALSE then break;
 if BAddrReg<>-1 then begin ALine.Params[2].AppendError('e','Address register must be absent [R:TAsmMcuRV.CodeGenLui]'); break; end;

 BConst:=BConstI;

 ALine.AppendDataBinD(CmdGenU(BConst,BRegIdx,$17));
 if BRefCode='L' then begin ALine.Params[2].AppendError('e','Invalid reference type [R:TAsmMcuRV.CodeGenLui]'); break; end;
 if BRefCode='H' then BRefCode:='U';
 if BRef<>'' then ALine.AppendRef(ALine.Params[2],BRef,BRefCode,0,BFieldAddr);

 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuRV.CodeGenBx ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3        : byte;
  BRs1Idx,
  BRs2Idx       : Integer;
  BRef          : string;
  BConst64      : Int64;
  BInvParams    : boolean;
  BDummyIdx     : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BInvParams:=FALSE;
 // BGT, BGTU, BLE, and BLEU can be synthesized by reversing the operands to
 // BLT, BLTU, BGE, and BGEU, respectively
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'beq bne # # blt bge bltu bgeu');
 if BCmdIdx<0 then begin BInvParams:=TRUE; BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # # bgt ble bgtu bleu'); end;
 if BCmdIdx>=0 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then break;
  BRs2Idx:=GetRegIndex(ALine.Params[2].Name); if BRs2Idx<0 then break;
  if BInvParams then begin BDummyIdx:=BRs1Idx; BRs1Idx:=BRs2Idx; BRs2Idx:=BDummyIdx; end;
  BRef:=ALine.Params[3].Name;
  if IsIntegerEqu(BRef,BConst64) then
   begin
   ALine.AppendDataBinD(CmdGenB(Cardinal(BConst64)-ALine.Addr,BRs2Idx,BRs1Idx,BFunc3,$63));
   end
  else
   begin
   ALine.AppendDataBinD(CmdGenB(0,BRs2Idx,BRs1Idx,BFunc3,$63));
   ALine.AppendRef(ALine.Params[3],BRef,'B',0,0);
   end;
  ALine.SetDstLabel(BRef);
  ALine.IsJxx:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'beqz bnez # # bltz bgez # #');
 if BCmdIdx>=0 then
  begin
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then break;
  BRs2Idx:=CRvRegZ;
  ALine.AppendDataBinD(CmdGenB(0,BRs2Idx,BRs1Idx,BFunc3,$63));
  BRef:=ALine.Params[2].Name;
  ALine.AppendRef(ALine.Params[2],BRef,'B',0,0);
  ALine.SetDstLabel(BRef);
  ALine.IsJxx:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # # # blez # #');
 if BCmdIdx>=0 then
  begin
  if Length(ALine.Params)<>3 then break;
  BFunc3:=BCmdIdx;
  BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then break;
  BRs2Idx:=CRvRegZ;
  ALine.AppendDataBinD(CmdGenB(0,BRs1Idx,BRs2Idx,BFunc3,$63)); // Note the order of BRs1Idx/BRs2Idx changed
  BRef:=ALine.Params[2].Name;
  ALine.AppendRef(ALine.Params[2],BRef,'B',0,0);
  ALine.SetDstLabel(BRef);
  ALine.IsJxx:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # # # # bgtu #');
 if BCmdIdx>=0 then
  begin
  if Length(ALine.Params)<>4 then break;
  BFunc3:=BCmdIdx;
  BRs1Idx:=GetRegIndex(ALine.Params[1].Name); if BRs1Idx<0 then break;
  BRs2Idx:=GetRegIndex(ALine.Params[2].Name); if BRs2Idx<0 then break;
  ALine.AppendDataBinD(CmdGenB(0,BRs1Idx,BRs2Idx,BFunc3,$63)); // Note the order change
  BRef:=ALine.Params[3].Name;
  ALine.AppendRef(ALine.Params[3],BRef,'B',0,0);
  ALine.SetDstLabel(BRef);
  ALine.IsJxx:=TRUE;
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;


Function TAsmMcuRV.CodeGenAluU ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3,
  BFunc7        : byte;
  BRdIdx,
  BRs1Idx,
  BRs2Idx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BFunc7:=$01;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'mul mulh mulhsu mulhu div divu rem remu');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>4 then break;
 BFunc3:=BCmdIdx;

 BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
 BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
 BRs2Idx:=GetRegIndex(ALine.Params[3].Name); if BRs2Idx<0 then break;

 ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));

 Result:=cceCompiled;
 until TRUE;
End;

Const
  CFpuEncFn7 : array [0..3] of Word = ($0, $4, $8, $C);

Function TAsmMcuRV.CodeGenFpuRR ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3,
  BFunc7        : byte;
  BRdIdx,
  BRs1Idx,
  BRs2Idx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BFunc7:=0;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'fadd.s fsub.s fmul.s fdiv.s');
 if BCmdIdx=-1 then break;

 if Length(ALine.Params)<>5 then break;
 if LowerCase(ALine.Params[4].Name)<>'rne' then begin ALine.Params[4].AppendError('e','Only "rne" option is supported [R:TAsmMcuRV.CodeGenFpuRR]'); break; end;
 BFunc3:=$0; BFunc7:=CFpuEncFn7[BCmdIdx];
 BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then begin ALine.Params[1].AppendError('e','Invalid RD register [R:TAsmMcuRV.CodeGenFpuRR]'); break; end;
 BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then begin ALine.Params[2].AppendError('e','Invalid RS1 register [R:TAsmMcuRV.CodeGenFpuRR]'); break; end;
 BRs2Idx:=GetRegIndex(ALine.Params[3].Name); if BRs2Idx<0 then  begin ALine.Params[3].AppendError('e','Invalid RS2 register [R:TAsmMcuRV.CodeGenFpuRR]'); break; end;
 ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$53));
 Result:=cceCompiled;

 until TRUE;
End;

Const
  CAmoEnc   : array [0..4] of Cardinal = ($07, $03, $13, $33, $23);

Function TAsmMcuRV.CodeGenAmo ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3,
  BFunc7        : byte;
  BRdIdx,
  BRs1Idx,
  BRs2Idx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'amoswap amoadd amoxor amoand amoor');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>4 then break;
 BFunc3:=$2;
 BFunc7:=CAmoEnc[BCmdIdx];

 BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
 BRs1Idx:=GetRegIndex(ALine.Params[2].Name); if BRs1Idx<0 then break;
 BRs2Idx:=GetRegIndex(ALine.Params[3].Name); if BRs2Idx<0 then break;

 ALine.AppendDataBinD(CmdGenR(BFunc7,BRs2Idx,BRs1Idx,BFunc3,BRdIdx,$33));

 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuRV.CodeGenFence ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BConstA,
  BConstB       : Int64;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name<>'fence' then break;
 if Length(ALine.Params)=1 then
  begin
  ALine.AppendDataBinD(CmdGenU((Cardinal($0F) shl 24) or (Cardinal($0F) shl 20),$0,$0F));
  Result:=cceCompiled;
  break;
  end;

 if Length(ALine.Params)<>3 then break;

 Result:=cceError;
 if IsIntegerEqu(ALine.Params[1].Name,BConstA)=FALSE then begin ALine.Params[1].AppendError('e','Constant expected [R:TAsmMcuRV.CodeGenFence]'); break; end;
 if (BConstA>15) or (BConstA<0) then begin ALine.Params[1].AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenFence]'); break; end;
 if IsIntegerEqu(ALine.Params[2].Name,BConstB)=FALSE then begin ALine.Params[2].AppendError('e','Constant expected [R:TAsmMcuRV.CodeGenFence]'); break; end;
 if (BConstB>15) or (BConstB<0) then begin ALine.Params[2].AppendError('e','Constant is too big or too small [R:TAsmMcuRV.CodeGenFence]'); break; end;

 ALine.AppendDataBinD(CmdGenU((Cardinal(BConstA) shl 24) or (Cardinal(BConstB) shl 20),$0,$0F));
 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuRV.CodeGenCsr ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BFunc3        : byte;
  BRdIdx,
  BRs1Idx,
  BConst64      : Int64;
  BConst        : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# csrrw csrrs csrrc');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>4 then break;
 BFunc3:=BCmdIdx;
 BRdIdx:=GetRegIndex(ALine.Params[1].Name);  if BRdIdx<0 then break;
 BRs1Idx:=GetRegIndex(ALine.Params[3].Name); if BRs1Idx<0 then break;
 Result:=cceError;
 if TryIntegerEqu(ALine.Params[2],BConst64)=FALSE then begin ALine.Params[3].AppendError('e','Cannot convert register index to integer [R:TAsmMcuRV.CodeGenCsr]'); break; end;
 BConst:=Cardinal(BConst64) and $FFF;
 if BConst64>$FFF then begin ALine.AppendError('e','CSR index is too big [R:TAsmMcuRV.CodeGenCsr]'); break; end;
 ALine.AppendDataBinD(CmdGenI(BConst,BRs1Idx,BFunc3,BRdIdx,$73));
 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuRV.CodeGenOther ( ALine : TAsmFlowLine ) : TCmdCompError;
Begin
 Result:=cceCheckNext;

 repeat
 if ALine.Params[0].Name='nop' then
  begin
  // ADDI x0, x0, 0
  ALine.AppendDataBinD(CmdGenI(0,CRvRegZ,$00,CRvRegZ,$13));
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='trap' then
  begin
  // EBREAK
  ALine.AppendDataBinD(CmdGenI($1,0,$00,0,$73));
  Result:=cceCompiled;
  break;
  end;

 if ALine.Params[0].Name='unimp' then
  begin
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;


end.

