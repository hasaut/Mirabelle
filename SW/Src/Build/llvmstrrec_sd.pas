unit LlvmStrRec_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsBase_sd, ParsHelper_sd, LlvmLine_sd,
  LlvmDbg_sd, LlvmVars_sd;

Type
  TProcStrRec = class(TProcVars)
  private
    FTextSRec        : TStringList;

    Function TargSizeForPush ( AParam : TFlowLineParam; ADelta : Integer ) : string;

    Procedure ProcessRecMov ( Var ADirty : boolean );
    Procedure ProcessArrMov ( Var ADirty : boolean );

    //Function StrPointerOpti ( Const ATarg : string ) : string;

    Procedure ProcessStrMov ( Var ADirty : boolean ); // Copy
    Procedure ProcessStrAdd ( Var ADirty : boolean );
    Procedure ProcessStrCmp ( Var ADirty : boolean );

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;

    property TextStrings : TStringList read FTextSRec;
  end;

Const
  CProcStrMovS = CTagS+'mf_'+CTagM+'Sys@_StrMovS'+CTagP+'dap'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;
  CProcStrAddS = CTagS+'mf_'+CTagM+'Sys@_StrAddS'+CTagP+'dap'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;
  CProcStrCmpS = CTagS+'mf_'+CTagM+'Sys@_StrCmpS'+CTagP+'dap'+CTagP+'dap'+CTagP+CTagE;
  CProcStrMovC = CTagS+'mf_'+CTagM+'Sys@_StrMovC'+CTagP+'dac'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;
  CProcStrAddC = CTagS+'mf_'+CTagM+'Sys@_StrAddC'+CTagP+'dac'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;
  CProcStrCmpC = CTagS+'mf_'+CTagM+'Sys@_StrCmpC'+CTagP+'dac'+CTagP+'dap'+CTagP+CTagE;
  CProcMemCopy = CTagS+'mf_'+CTagM+'Sys@_MemCopy'+CTagP+'dap'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;

implementation

Constructor TProcStrRec.Create;
Begin
 Inherited;
 FTextSRec:=TStringList.Create;
End;

Destructor TProcStrRec.Destroy;
Begin
 FTextSRec.Free;
 Inherited;
End;

Procedure TProcStrRec.Compile;
Var
  BDirty        : boolean;
Begin
 Inherited;


 repeat
 if FFatalError then break;
 BDirty:=FALSE;              //DbgSave(Self,'SRec',FFlow,TRUE);
 ProcessRecMov(BDirty);      //DbgSave(Self,'SRec',FFlow,TRUE);
 ProcessArrMov(BDirty);      //DbgSave(Self,'SRec',FFlow,TRUE);
 ProcessStrMov(BDirty);     //DbgSave(Self,'SRec',FFlow,TRUE);
 ProcessStrAdd(BDirty);     //DbgSave(Self,'SRec',FFlow,TRUE);
 ProcessStrCmp(BDirty);     //DbgSave(Self,'SRec',FFlow,TRUE);
 if BDirty=FALSE then break;
 OptiVarsA;
 until FALSE;

 DbgMarkFlow('SRec');
 FlowListToStringList(FFlowList,FFlow);
 FTextSRec.Assign(FFlowList);

 TimeStamp('StrRec');
 if FModule.DbgPath<>'' then DbgSave(Self,'D_SRec',FFlowList,TRUE);
End;

Procedure TProcStrRec.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T SRec');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextSRec.Count-1 do ADbg.Append(FTextSRec.Strings[BLineIdx]);
End;


Function TProcStrRec.TargSizeForPush ( AParam : TFlowLineParam; ADelta : Integer ) : string;
Begin
 Result:=CTagS+'c_d'+CTagM+IntToStr(FModule.GetTypeSize(ExtractFinalType(AParam.Targ))+ADelta)+CTagE;
End;

// lea      ~deq:BTmp1# ~der10e{io0_FDataIA,io4_FDataIB,bo8_FDataBA,bo9_FDataBB,}:BTmp0# ;~rm:39,5#
// push_0   dqr10e{io0_FDataIA,io4_FDataIB,bo8_FDataBA,bo9_FDataBB,} ~deq:BTmp1# ;~rm:41,19#
// call_0   ~___:Discard# ~mhr10e{io0_FDataIA,io4_FDataIB,bo8_FDataBA,bo9_FDataBB,}:Proba1,# ;~rm:41,19#
// copy_rec ~dbr10e{io0_FDataIA,io4_FDataIB,bo8_FDataBA,bo9_FDataBB,}:BDataRecB# ~der10e{io0_FDataIA,io4_FDataIB,bo8_FDataBA,bo9_FDataBB,}:BTmp0# ;~rm:41,19#

Procedure TProcStrRec.ProcessRecMov ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB,
  BIndexC       : Integer;
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BCopyDel      : boolean;
  BTail         : string;
Begin
 repeat
 BIndexC:=0;
 while BIndexC<Length(FFlow) do
  begin
  BLineC:=FFlow[BIndexC];
  if BLineC.IsCopyRec then break;
  inc(BIndexC);
  end;
 if BIndexC>=Length(FFlow) then break;

 ADirty:=TRUE;
 // Here we need to either replace copy_rec by a true copy or to optimize the structure in the top

 BLineA:=nil;
 BCopyDel:=FALSE;
 repeat
 // copy_rec optimization case is only applicable to Tmp variables
 if ParsIsTmp(BLineC.Param[0].Targ)=FALSE then break;
 if ParsIsTmp(BLineC.Param[1].Targ)=FALSE then break;

 // Search pointer to BLineA.Param[1].Targ (LEA)
 BIndexA:=0;
 while BIndexA<BIndexC do
  begin
  BLineA:=FFlow[BIndexA];
  if BLineA.IsLea and (BLineA.Param[1].Targ=BLineC.Param[1].Targ) then break;
  inc(BIndexA);
  end;
 if BIndexA>=BIndexC then break;

 // Search push "dqr..."
 BIndexB:=BIndexA+1;
 while BIndexB<BIndexC do
  begin
  BLineB:=FFlow[BIndexB];
  if BLineB.IsPush and (BLineB.Param[1].Targ=BLineA.Param[0].Targ) and ParsIsParamRetOut(BLineB.Param[0].Targ) then break;
  inc(BIndexB);
  end;
 if BIndexB>=BIndexC then break;

 BCopyDel:=TRUE;
 until TRUE;

 if BCopyDel then
  begin
  BLineA.Param[1].Targ:=BLineC.Param[0].Targ;
  if ParsIsGlobalOrExtern(BLineC.Param[0].Targ) then BLineA.Cmd:='mov'
  else if ParsIsTargVarConstResult(BLineC.Param[0].Targ) then BLineA.Cmd:='lam';
  DeleteFlowLine(FFlow,BIndexC);
  end
 else
  begin
  FTextDst.Clear;
  AppendPush('dap',ResolveAddr(BLineC.Param[1].Targ));
  AppendPush('dap',ResolveAddr(BLineC.Param[0].Targ));
  AppendPush('dad',TargSizeForPush(BLineC.Param[0],0));
  AppendCall(CDiscardValue,CProcMemCopy,0);
  BTail:=BLineC.Tail;
  DeleteFlowLine(FFlow,BIndexC);
  ParsTailInsertTagVarA(BTail,'A','TProcStrRec.ProcessRecMov');
  InsertTextDstToFlow(BIndexC,BTail);
  ProcessPointersA(BTail);
  end;

 until TRUE;
End;

Procedure TProcStrRec.ProcessArrMov ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB,
  BIndexC       : Integer;
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BCopyDel      : boolean;
  BTail         : string;
Begin
 repeat
 BIndexC:=0;
 while BIndexC<Length(FFlow) do
  begin
  BLineC:=FFlow[BIndexC];
  if BLineC.IsCopyArr then break;
  inc(BIndexC);
  end;
 if BIndexC>=Length(FFlow) then break;

 ADirty:=TRUE;
 // Here we need to either replace copy_rec by a true copy or to optimize the structure in the top

 BLineA:=nil;
 BCopyDel:=FALSE;
 repeat
 // copy_rec optimization case is only applicable to Tmp variables
 if ParsIsTmp(BLineC.Param[0].Targ)=FALSE then break;
 if ParsIsTmp(BLineC.Param[1].Targ)=FALSE then break;

 // Search pointer to BLineA.Param[1].Targ (LEA)
 BIndexA:=0;
 while BIndexA<BIndexC do
  begin
  BLineA:=FFlow[BIndexA];
  if BLineA.IsLea and (BLineA.Param[1].Targ=BLineC.Param[1].Targ) then break;
  inc(BIndexA);
  end;
 if BIndexA>=BIndexC then break;

 // Search push "dqr..."
 BIndexB:=BIndexA+1;
 while BIndexB<BIndexC do
  begin
  BLineB:=FFlow[BIndexB];
  if BLineB.IsPush and (BLineB.Param[1].Targ=BLineA.Param[0].Targ) and ParsIsParamRetOut(BLineB.Param[0].Targ) then break;
  inc(BIndexB);
  end;
 if BIndexB>=BIndexC then break;

 BCopyDel:=TRUE;
 until TRUE;

 if BCopyDel then
  begin
  BLineA.Param[1].Targ:=BLineC.Param[0].Targ;
  if ParsIsGlobalOrExtern(BLineC.Param[0].Targ) then BLineA.Cmd:='mov'
  else if ParsIsTargVarConstResult(BLineC.Param[0].Targ) then BLineA.Cmd:='lam';
  DeleteFlowLine(FFlow,BIndexC);
  end
 else
  begin
  FTextDst.Clear;
  AppendPush('dap',ResolveAddr(BLineC.Param[1].Targ));
  AppendPush('dap',ResolveAddr(BLineC.Param[0].Targ));
  AppendPush('dad',TargSizeForPush(BLineC.Param[0],0));
  AppendCall(CDiscardValue,CProcMemCopy,0);
  BTail:=BLineC.Tail;
  DeleteFlowLine(FFlow,BIndexC);
  ParsTailInsertTagVarA(BTail,'A','TProcStrRec.ProcessArrMov');
  InsertTextDstToFlow(BIndexC,BTail);
  ProcessPointersA(BTail);
  end;

 until TRUE;
End;

// When doing Str-Mov do not forget to optimize post-str copy when destination is used as a return value of a function
// (same as for records)
//  BDataS:=ReadParamStr(BRecvS);
// Generates the following code
//  push_0 dqsp255e ~deq:BTmp1#
//  push_0 dvsp255e ~deq:BTmp2#
//  call_0 ~___:Discard# ~mhsp255e:ReadParamStr,dvsp255e,#
//  mov    ~dbsp255e:BDataS# ~desp255e:BTmp0#

Procedure TProcStrRec.ProcessStrMov ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB,
  BIndexC       : Integer;
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BCopyDel      : boolean;
  BTail         : string;
  BIsStringA,
  BIsStringB    : boolean;
  BFound        : boolean;
Begin
 BIsStringA:=FALSE; BIsStringB:=FALSE;
 repeat
 BIndexC:=0;
 while BIndexC<Length(FFlow) do
  begin
  BLineC:=FFlow[BIndexC];
  BFound:=FALSE;
  repeat
  if BLineC.IsMov=FALSE then break;
  if BLineC.Param[0].Targ='' then break;
  if BLineC.Param[1].Targ='' then break;
  BIsStringA:=ParsIsTypeStringP(ExtractFinalType(BLineC.Param[0].Targ));
  BIsStringB:=ParsIsTypeStringP(ExtractFinalType(BLineC.Param[1].Targ));
  if BIsStringA and BIsStringB then
  else if BIsStringA and (ParsExtractType(BLineC.Param[1].Targ)='c') then
  else if BIsStringA or BIsStringB then begin AppendError('Internal error: Both variables must be of string type [R:TProcStrRec.ProcessStrMov]'); break; end
  else break;
  BFound:=TRUE;
  until TRUE;
  if BFound then break;
  inc(BIndexC);
  end;
 if BIndexC>=Length(FFlow) then break;

 ADirty:=TRUE;
 // Here we need to either replace copy_rec by a true copy or to optimize the structure in the top

 BLineA:=nil;
 BCopyDel:=FALSE;
 repeat
 // copy_rec optimization case is only applicable to Tmp variables
 if BIsStringB=FALSE then break;
 if ParsIsTmp(BLineC.Param[1].Targ)=FALSE then break;

 // Search pointer to BLineA.Param[1].Targ (LEA)
 BIndexA:=0;
 while BIndexA<BIndexC do
  begin
  BLineA:=FFlow[BIndexA];
  if BLineA.IsLea and (BLineA.Param[1].Targ=BLineC.Param[1].Targ) then break;
  inc(BIndexA);
  end;
 if BIndexA>=BIndexC then break;

 // Search push "dqr..."
 BIndexB:=BIndexA+1;
 while BIndexB<BIndexC do
  begin
  BLineB:=FFlow[BIndexB];
  if BLineB.IsPush and (BLineB.Param[1].Targ=BLineA.Param[0].Targ) and ParsIsParamRetOut(BLineB.Param[0].Targ) then break;
  inc(BIndexB);
  end;
 if BIndexB>=BIndexC then break;

 BCopyDel:=TRUE;
 until TRUE;

 if BCopyDel then
  begin
  BLineA.Param[1].Targ:=BLineC.Param[0].Targ;
  if ParsIsGlobalOrExtern(BLineC.Param[0].Targ) then BLineA.Cmd:='mov'
  else if ParsIsTargVarConstResult(BLineC.Param[0].Targ) then BLineA.Cmd:='lam';
  DeleteFlowLine(FFlow,BIndexC);
  end
 else
  begin
  FTextDst.Clear;
  if BIsStringA and (ParsExtractType(BLineC.Param[1].Targ)='c') then // String := Char
   begin
   AppendPush('dac',BLineC.Param[1].Targ);
   AppendPush('dap',ResolveAddr(BLineC.Param[0].Targ));
   AppendPush('dad',TargSizeForPush(BLineC.Param[0],-1)); // -1, because we indicate the number of effective bytes without length
   AppendCall(CDiscardValue,CProcStrMovC,0);
   end
  else  // String := String
   begin
   AppendPush('dap',ResolveAddr(BLineC.Param[1].Targ));
   AppendPush('dap',ResolveAddr(BLineC.Param[0].Targ));
   AppendPush('dad',TargSizeForPush(BLineC.Param[0],-1)); // -1, because we indicate the number of effective bytes without length
   AppendCall(CDiscardValue,CProcStrMovS,0);
   end;
  BTail:=BLineC.Tail;
  DeleteFlowLine(FFlow,BIndexC);
  ParsTailInsertTagVarA(BTail,'A','TProcStrRec.ProcessRecMov');
  InsertTextDstToFlow(BIndexC,BTail);
  ProcessPointersA(BTail);
  end;

 until TRUE;
End;

Procedure TProcStrRec.ProcessStrAdd ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA        : TFlowLine;
  BIsStringA,
  BIsStringB    : boolean;
  BTail         : string;
Begin
 FTextDst.Clear;

 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLineA:=FFlow[BIndex];
  repeat
  if BLineA.Cmd<>'add' then break;
  if BLineA.Param[0].Targ='' then break;
  if BLineA.Param[1].Targ='' then break;
  BIsStringA:=ParsIsTypeStringP(ExtractFinalType(BLineA.Param[0].Targ));
  BIsStringB:=ParsIsTypeStringP(ExtractFinalType(BLineA.Param[1].Targ));
  if BIsStringA and BIsStringB then
  else if BIsStringA and (ParsExtractType(BLineA.Param[1].Targ)='c') then
  else if BIsStringA or BIsStringB then begin AppendError('Internal error: Both variables must be of string type [R:TProcStrRec.ProcessStrMov]'); break; end
  else break;

  if BIsStringA and (ParsExtractType(BLineA.Param[1].Targ)='c') then // String := Char
   begin
   AppendPush('dac',BLineA.Param[1].Targ);
   AppendPush('dap',ResolveAddr(BLineA.Param[0].Targ));
   AppendPush('dad',TargSizeForPush(BLineA.Param[0],-1));
   AppendCall(CDiscardValue,CProcStrAddC,0);
   break;
   end;
  // String := String
  AppendPush('dap',ResolveAddr(BLineA.Param[1].Targ));
  AppendPush('dap',ResolveAddr(BLineA.Param[0].Targ));
  AppendPush('dad',TargSizeForPush(BLineA.Param[0],-1));
  AppendCall(CDiscardValue,CProcStrAddS,0);
  until TRUE;
  if FTextDst.Count=0 then inc(BIndex)
  else
   begin
   ADirty:=TRUE;
   BTail:=BLineA.Tail;
   DeleteFlowline(FFlow,BIndex);
   ParsTailInsertTagVarA(BTail,'A','TProcStrRec.ProcessStrAdd');
   InsertTextDstToFlow(BIndex,BTail);
   ProcessPointersA(BTail);
   end;
  end;
End;

Procedure TProcStrRec.ProcessStrCmp ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA        : TFlowLine;
  BIsStringA,
  BIsStringB    : boolean;
  BTail         : string;
Begin
 FTextDst.Clear;

 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLineA:=FFlow[BIndex];
  repeat
  if BLineA.Cmd<>'cmp' then break;
  if BLineA.Param[0].Targ='' then break;
  if BLineA.Param[1].Targ='' then break;
  BIsStringA:=ParsIsTypeStringP(ExtractFinalType(BLineA.Param[0].Targ));
  BIsStringB:=ParsIsTypeStringP(ExtractFinalType(BLineA.Param[1].Targ));
  if BIsStringA and BIsStringB then
  else if BIsStringA and (ParsExtractType(BLineA.Param[1].Targ)='c') then
  else if BIsStringA or BIsStringB then begin AppendError('Internal error: Both variables must be of string type [R:TProcStrRec.ProcessStrMov]'); break; end
  else break;

  if BIsStringA and (ParsExtractType(BLineA.Param[1].Targ)='c') then // String := Char
   begin
   AppendPush('dac',BLineA.Param[1].Targ);
   AppendPush('dap',ResolveAddr(BLineA.Param[0].Targ));
   AppendCall(CDiscardValue,CProcStrCmpC,0);
   break;
   end;
  // String := String
  AppendPush('dap',ResolveAddr(BLineA.Param[1].Targ));
  AppendPush('dap',ResolveAddr(BLineA.Param[0].Targ));
  AppendCall(CDiscardValue,CProcStrCmpS,0);
  until TRUE;
  if FTextDst.Count=0 then inc(BIndex)
  else
   begin
   ADirty:=TRUE;
   BTail:=BLineA.Tail;
   DeleteFlowline(FFlow,BIndex);
   ParsTailInsertTagVarA(BTail,'A','TProcStrRec.ProcessStrCmp');
   InsertTextDstToFlow(BIndex,BTail);
   ProcessPointersA(BTail);
   end;
  end;
End;

end.

