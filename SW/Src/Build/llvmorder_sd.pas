unit LlvmOrder_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, LlvmDbg_sd, LlvmLine_sd, LlvmStrRec_sd,
  ParsHelper_sd;

Type
  TProcOrder = class(TProcStrRec)
  private
    FTextOrdr   : TStringList;

  protected
    Procedure MoveCmdUp ( Const ACmd : string );
    Procedure MoveCmdDn ( Const ACmd : string );
    Procedure OptimizeLdStLea;
    Procedure DeleteLdLeaStpNil;
    Procedure DeleteLamPush;
    Procedure DeleteLamAddPush;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;
  end;

implementation

Constructor TProcOrder.Create;
Begin
 Inherited;
 FTextOrdr:=TStringList.Create;
End;

Destructor TProcOrder.Destroy;
Begin
 FTextOrdr.Free;
 Inherited;
End;

Procedure TProcOrder.Compile;
Begin
 Inherited;

 repeat
 if FFatalError then break;
 OptimizeLdStLea;
 DeleteLdLeaStpNil;
 DeleteLamPush;
 DeleteLamAddPush;
 until TRUE;

 DbgMarkFlow('Ordr');
 FlowListToStringList(FFlowlist,FFlow);
 FTextOrdr.Assign(FFlowList);

 TimeStamp('Order');
 if FModule.DbgPath<>'' then DbgSave(Self,'E_Order',FFlowList,TRUE);
End;

Procedure TProcOrder.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T Ordr');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextOrdr.Count-1 do ADbg.Append(FTextOrdr.Strings[BLineIdx]);
End;

Procedure TProcOrder.MoveCmdUp ( Const ACmd : string );
Var
  BTextIdx      : Integer;
  BLine,
  BLineI        : TFlowLine;
  BSplit        : string;
  BVisibleDn,
  BMask         : string;
  BTargChain    : string;
  BDeleteOld    : boolean;
Begin
 repeat
 BTextIdx:=0;
 while BTextIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BTextIdx];
  if (BLine.Processed=FALSE) and (BLine.Cmd=ACmd) then break;
  inc(BTextIdx);
  end;
 if BTextIdx>=Length(FFlow) then break;

 BLine.Processed:=TRUE;

 BTargChain:=GetInitMask;
 FillChainA(BLine.Param[0].Targ,'s',BTextIdx,BTargChain);

 BSplit:=GetInitMask;
 BVisibleDn:=GetInitMask; CheckSetMask(BVisibleDn,BTextIdx,'s');
 BMask:=GetInitMask;
 MoveIdxUp(BTextIdx-1,BTargChain,BVisibleDn,BSplit,BMask);

 BDeleteOld:=FALSE;
 BTextIdx:=Length(FFlow)-1;
 while BTextIdx>=0 do
  begin
  if GetMask(BSplit,BTextIdx)='u' then
   begin
   BDeleteOld:=TRUE;
   BLineI:=BLine.Clone; BLineI.Processed:=TRUE;
   InsertFlowLine(FFlow,BTextIdx+1,BLineI);
   end;
  Dec(BTextIdx);
  end;
 if BDeleteOld then DeleteFlowLine(FFlow,BLine);
 until FALSE;
End;

Procedure TProcOrder.MoveCmdDn ( Const ACmd : string );
Var
  BTextIdx      : Integer;
  BLine,
  BLineI        : TFlowLine;
  BSplit        : string;
  BVisibleUp,
  BMask         : string;
  BTargChain    : string;
  BDeleteOld    : boolean;
Begin
 repeat
 BTextIdx:=0;
 while BTextIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BTextIdx];
  if (BLine.Processed=FALSE) and (BLine.Cmd=ACmd) then break;
  inc(BTextIdx);
  end;
 if BTextIdx>=Length(FFlow) then break;

 BLine.Processed:=TRUE;

 BTargChain:=GetInitMask;
 FillChainA(BLine.Param[0].Targ,'d',BTextIdx,BTargChain);

 BSplit:=GetInitMask;
 BVisibleUp:=GetInitMask; CheckSetMask(BVisibleUp,BTextIdx,'d');
 BMask:=GetInitMask;
 MoveIdxDn(BTextIdx+1,BTargChain,BVisibleUp,BSplit,BMask);

 BDeleteOld:=FALSE;
 BTextIdx:=Length(FFlow)-1;
 while BTextIdx>=0 do
  begin
  if GetMask(BSplit,BTextIdx)='o' then
   begin
   BDeleteOld:=TRUE;
   BLineI:=BLine.Clone; BLineI.Processed:=TRUE;
   InsertFlowLine(FFlow,BTextIdx,BLineI);
   end;
  Dec(BTextIdx);
  end;
 if BDeleteOld then DeleteFlowLine(FFlow,BLine);
 until FALSE;
End;

// The following procedure moves ST up and LD down because this is more optimal
Procedure TProcOrder.OptimizeLdStLea;
Begin
 FlowClearProcessed;   //DbgSave(Self,'E_Order',FFlow,TRUE);
 MoveCmdUp('st_lea');
 MoveCmdDn('ld_lea');
 MoveCmdDn('lea');     //DbgSave(Self,'E_Order',FFlow,TRUE);
 MoveCmdDn('lam');
End;

{
 After call with VAR, and if VAR is not used, there will be a following pair
 ld_lea   ~b_d:BDataA#                     ;~rm:26,27#       . . . . d . . . . |
 stp_nil  ~b_d:BDataA#                     ;~rm:26,27#       . . . . s . . . . |
}
Procedure TProcOrder.DeleteLdLeaStpNil;
Var
  BTextIdx      : Integer;
  BLine         : TFlowLine;
  BTargChain    : string;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BTextIdx:=0;
 while BTextIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BTextIdx];
  if BLine.Cmd='ld_lea' then
   begin
   if (BTextIdx+1)=Length(FFlow) then begin DeleteFlowLine(FFlow,BTextIdx); break; end;
   BTargChain:=GetInitMask;
   FillChainA(BLine.Param[0].Targ,'d',BTextIdx,BTargChain);
   if GetMask(BTargChain,BTextIdx+1)='.' then DeleteFlowLine(FFlow,BTextIdx)
   else inc(BTextIdx);
   end
  else
   begin
   inc(BTextIdx);
   end;
  end;
 until TRUE;
End;

{
  lam ~deq:BTmp40# ~c_a0s19ew:ConstArray22#
  push_15 dap ~deq:BTmp40#
}
Procedure TProcOrder.DeleteLamPush;
Var
  BTextIdx      : Integer;
  BLineA,
  BLineB        : TFlowLine;
  BTargChain    : string;
  BTargA,
  BTargB        : string;
  BFound        : boolean;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-1 do
  begin
  BLineA:=FFlow[BTextIdx];
  BFound:=FALSE;
  repeat
  if BLineA.IsLam=FALSE then break;
  if Length(BLineA.Param)<2 then break;
  BLineB:=FFlow[BTextIdx+1];
  if Length(BLineB.Param)<2 then break;
  if BLineB.IsPush=FALSE then break;
  if BLineB.FixedCorr<>'' then break;
  if ParsIsConst(BLineA.Param[1].Targ) or ParsIsGlobalOrExtern(BLineA.Param[1].Targ) then
  else break;
  if ParsIsGlobalOrExtern(BLineA.Param[0].Targ) then break;
  if BLineB.Param[1].Targ<>BLineA.Param[0].Targ then break;
  BTargChain:=GetInitMask;
  FillChainA(BLineA.Param[0].Targ,'d',BTextIdx,BTargChain);
  if ((BTextIdx+2)>=Length(FFlow)) or (GetMask(BTargChain,BTextIdx+2)='.') then
  else break;
  BTargA:=BLineA.Param[0].Targ; BTargB:=BLineA.Param[1].Targ;
  BLineB.ChangeTargs(BTargA,BTargB);
  BLineB.FixedCorr:=BLineA.FixedCorr;
  BFound:=TRUE;
  until TRUE;
  if BFound then DeleteFlowLine(FFlow,BTextIdx)
  else inc(BTextIdx);
  end;
 until TRUE;
End;

//  lam ~deq:BTmp2# ~dhr8e{io0_FDataA,io4_FDataB,}:FRec#
//  add ~deq:BTmp2# ~c_i:4#
//  push_0 dvi ~deq:BTmp2#
Procedure TProcOrder.DeleteLamAddPush;
Var
  BTextIdx      : Integer;
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BTargChain    : string;
  BTargA,
  BTargB,
  BTargC        : string;
  BFound        : boolean;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-2 do
  begin
  BLineA:=FFlow[BTextIdx];
  BFound:=FALSE;
  repeat
  if BLineA.IsLam=FALSE then break;
  if Length(BLineA.Param)<2 then break;
  BLineB:=FFlow[BTextIdx+1];
  if Length(BLineB.Param)<2 then break;
  if BLineB.Cmd<>'add' then break;
  BLineC:=FFlow[BTextIdx+2];
  if Length(BLineC.Param)<2 then break;
  if BLineC.IsPush=FALSE then break;
  if ParsIsConst(BLineA.Param[1].Targ) or ParsIsGlobalOrExtern(BLineA.Param[1].Targ) then
  else break;
  if ParsIsGlobalOrExtern(BLineA.Param[0].Targ) then break;
  if BLineB.Param[0].Targ<>BLineA.Param[0].Targ then break;
  if BLineC.Param[1].Targ<>BLineA.Param[0].Targ then break;
  if ParsIsConst(BLineB.Param[1].Targ)=FALSE then break;
  BTargChain:=GetInitMask;
  FillChainA(BLineA.Param[0].Targ,'d',BTextIdx,BTargChain);
  if ((BTextIdx+3)>=Length(FFlow)) or (GetMask(BTargChain,BTextIdx+3)='.') then
  else break;
  if BLineC.FixedCorr<>'' then break;
  BTargA:=BLineA.Param[0].Targ; BTargB:=BLineA.Param[1].Targ; BTargC:=BLineB.Param[1].Targ;
  BLineC.ChangeTargs(BTargA,BTargB);
  BLineC.FixedCorr:='add '+ParsExtractName(BTargC);
  BFound:=TRUE;
  until TRUE;
  if BFound then begin DeleteFlowLine(FFlow,BTextIdx); DeleteFlowLine(FFlow,BTextIdx); end
  else inc(BTextIdx);
  end;
 until TRUE;
End;

end.

