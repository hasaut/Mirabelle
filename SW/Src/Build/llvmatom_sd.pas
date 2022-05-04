unit LlvmAtom_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParsBase_sd, ParsHelper_sd, AsmTypes_sd, LlvmBase_sd, LlvmCom_sd, LlvmDbg_sd;

Type
  TProcAtom = class(TProcComm)
  private
    FTextPars,
    FTextAtom   : TStringList;

    Procedure AppendMovCheckType ( Const ATargD, ATargS : string );
    Function ProcConst2 ( Const ATargA, ATargB : string; Const AOper : string; Const ATypeR : string ) : string;

    Procedure PushProcParam ( Const AParamType, AParamEval : string; Var ALeaList : string );
    Function ProcessEvalEtCalls ( Const AExecS : string ) : boolean;
    Procedure ReadEvalForIfContinue ( Var AEval : string; Var ASubEval : string );
    Procedure ProcessIfA ( Const AEval : string; Const ALabelThen, ALabelElse : string );
    Function ProcessIf ( Const AExecS : string ) : boolean;
    Procedure ExtractProc ( Var AEval : string; Out AProcNameL, AProcParams : string; Out APos : Integer );
    Function AtomizeProcCall ( Const AProcNameL, AProcParams : string ) : string;
    Function ProcessDirectives ( Const AProcNameL, AProcParams : string ) : string;
    Function AtomizeEval2 ( Const AEval : string; Const AWantedType : string ) : string;
    Procedure GetFieldOffsetA ( Const ARecType : string; Const AFieldThis : string; Out AResType : string; Out AOffset : string );
    //Function AtomizeAddr ( Const ATarg : string ) : string;
    Function AtomizeArray ( Const AArrayS : string; Const AWantedType : string ) : string;
  protected
    FPointers   : TStringList;

    Function PromoteType ( Const ATypeA, ATypeB : string ) : string;
    Function ResolveElem ( Const ATarg : string ) : string;
    Function ResolveAddr ( Const ATarg : string ) : string;
    Function ResolveAddr ( Const ATarg : string; Out AForceLoad : boolean ) : string;
    Function AtomizeTarg ( Const ATarg : string ) : string;
    Procedure AtomizeTarg ( Const ATarg : string; Out ABaseThis, AOffsThis, ATypeThis : string );
    Function AppendPointer ( Const AArrayName : string; ALoad : char ) : string; // "a" for LEA, "m" for LAM
    Function AtomizeEval ( Const AEval : string; Const AWantedType : string ) : string;
    Procedure ProcessPointers;

    Function BestTypeSuitableForPush ( Const AType : string ) : string; Virtual; Abstract;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;

    property TextPars : TStringList read FTextPars;
    property TextAtom : TStringList read FTextAtom;
  end;

implementation

Uses
  ConComL, ConComAsm;

Constructor TProcAtom.Create;
Begin
 Inherited;
 FTextPars:=TStringList.Create;
 FTextAtom:=TStringList.Create;
 FPointers:=TStringList.Create;
End;

Destructor TProcAtom.Destroy;
Begin
 FPointers.Free;
 FTextAtom.Free;
 FTextPars.Free;
 Inherited;
End;

Procedure TProcAtom.Compile;
Var
  BLineIdx      : Integer;
  BExecS        : string;
Begin
 Inherited;

 FTextPars.Assign(FFlowList);
 DbgMarkLines(FTextPars,'Pars');

 //DbgSave(Self,'A_Pars',FTextPars,TRUE);

 FPointers.Clear;

 FTextSrc.Assign(FFlowList); FTextDst.Clear;
 for BLineIdx:=0 to FTextSrc.Count-1 do
  begin
  BExecS:=ReadSrc(BLineIdx);
  //SplitLine(FTextSrc.Strings[BLineIdx],BExecS);

  repeat
  if ProcessEvalEtCalls(BExecS) then break;
  if ProcessIf(BExecS) then break;
  AppendExec(BExecS);
  until TRUE;
  end;

 FTextSrc.Assign(FTextDst);

 ProcessPointers;

 DbgMarkLines(FTextSrc,'Atom');
 FTextAtom.Assign(FTextSrc);
 FFlowList.Assign(FTextSrc);

 TimeStamp('Atom');
 if FModule.DbgPath<>'' then DbgSave(Self,'A_Atom',FTextAtom,TRUE);
End;

Procedure TProcAtom.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T Pars');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextPars.Count-1 do ADbg.Append(FTextPars.Strings[BLineIdx]);
 ADbg.Append(';@T Atom');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextAtom.Count-1 do ADbg.Append(FTextAtom.Strings[BLineIdx]);
End;

Function TProcAtom.AppendPointer ( Const AArrayName : string; ALoad : char ) : string;
Begin
 Result:=FPointers.Values[AArrayName];
 if Result<>'' then Delete(Result,1,1)
 else
  begin
  Result:=AppendTmpVar('q');
  FPointers.Values[AArrayName]:=ALoad+Result;
  end;
End;

Procedure TProcAtom.ExtractProc ( Var AEval : string; Out AProcNameL, AProcParams : string; Out APos : Integer );
Var
  BEval         : string;
  BPosMax,
  BPosThis,
  BLevelMax,
  BLevelThis    : Integer;
  BPosE         : Integer;
  BDummyS       : string;
  BPos          : Integer;
Begin
 AProcNameL:=''; AProcParams:=''; APos:=0;
 BEval:=AEval;
 repeat
 BPosMax:=Pos(CTagS+'x',BEval);
 if BPosMax=0 then break;
 BEval[BPosMax+1]:='X';
 BLevelMax:=GetBracketLevel(BEval,BPosMax);

  repeat
  BPosThis:=Pos(CTagS+'x',BEval);
  if BPosThis=0 then break;
  BEval[BPosThis+1]:='X';
  BLevelThis:=GetBracketLevel(BEval,BPosThis);
  if BLevelThis>BLevelMax then begin BPosMax:=BPosThis; BLevelMax:=BLevelThis; end;
  until FALSE;

  repeat
  BPosThis:=Pos(CTagS+'X',BEval);
  if BPosThis=0 then break;
  BEval[BPosThis+1]:='x';
  until FALSE;

 APos:=BPosMax;
 BPosE:=PosNext(CTagE,BEval,BPosMax);
 AProcNameL:=Copy(BEval,BPosMax,BPosE-BPosMax+1);
 Delete(AEval,BPosMax,BPosE-BPosMax+1);

 if (Length(AEval)>BPosMax) and (AEval[BPosMax]='(') then
  begin
  BDummyS:=Copy(AEval,BPosMax,Length(AEval)-BPosMax+1);
  BPos:=Pos('()',BDummyS);
  if BPos=1 then
   begin
   AProcParams:='';
   Delete(AEval,BPosMax,BPos+1);
   end
  else
   begin
   BPos:=Pos(CTagP+')',BDummyS);
   if BPos=0 then begin AppendError('Invalid number of parameters or number of opening and closing brakets do not match in call to procedure '+ParsExtractName(AProcNameL)+'[R:TProcAtom.ExtractProc]'); break; end;
   AProcParams:=Copy(AEval,BPosMax+1,BPos-1);
   Delete(AEval,BPosMax,BPos+1);
   end;
  end;
 until TRUE;
End;

Procedure TProcAtom.PushProcParam ( Const AParamType, AParamEval : string; Var ALeaList : string );
Var
  BTargE        : string;
  BTypeP,
  BTypeE        : string;
  BSpecE        : string;
  BTargP        : string;
  BTargCopy     : string;
  BForceLoad    : boolean;
Begin
 repeat
 if AParamType='da*' then
  begin
  AppendPush(AParamType,AParamEval);
  break;
  end;
 BTypeP:=AParamType; Delete(BTypeP,1,2);
 BTargE:=AParamEval; //AtomizeEval(AParamEval,'');
 BTypeE:=ExtractFinalType(BTargE);
 BSpecE:=ParsExtractSpec(BTargE);
 if Length(BSpecE)<2 then begin AppendError('Internal error: Invalid spec [R:TProcAtom.PushProcParam]'); break; end;
 if ParsIsStringP(BTargE) then
  begin
  if ParsIsTypeStringP(BTypeP)=FALSE then AppendError('Type of formal parameters is not a string [R:TProcAtom.PushProcParam]');
  if ParsIsParamConst(AParamType) then BTargP:=ResolveAddr(BTargE)
  else if ParsIsParamRetOut(AParamType) then
   begin
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else if ParsIsParamVar(AParamType) then
   begin
   if ParsIsSpecTmp(BSpecE) then AppendError('Cannot pass data as a VAR parameter [R:TProcAtom.PushProcParam]');
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   if BTypeP<>BTypeE then AppendError('Types of formal and actual VAR parameters must be identical [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else
   begin
   BTargCopy:=AppendTmpVar(BTypeP);
   AppendCmd2('mov',BTargCopy,BTargE);
   BTargP:=ResolveAddr(BTargCopy);
   end;
  {if ParsIsTargParamVar(BTargE) or ParsIsTargParamConst(BTargE) or ParsIsConst(BTargE) or ParsIsGlobal(BTargE) then BTargP:=AppendPointer(BTargE,'m')
  else BTargP:=AppendPointer(BTargE,'a'); // We do not store/load dirty LEAs}
  AppendPush(AParamType,BTargP);
  break;
  end;
 if ParsIsStringZ(BTargE) then
  begin
  if ParsIsTypePointer(BTypeP)=FALSE then AppendError('Type of formal parameters is not a pointer [R:TProcAtom.PushProcParam]');
  if ParsIsConst(BTargE) then BTargP:=BTargE
  else if ParsIsParamConst(AParamType) then BTargP:=ResolveAddr(BTargE)
  else if ParsIsParamRetOut(AParamType) then
   begin
   AppendError('Normally this should not be supported by C [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else if ParsIsParamVar(AParamType) then
   begin
   AppendError('Normally this should not be supported by C [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else
   begin
   BTargCopy:=AppendTmpVar(BTypeP);
   AppendCmd2('mov',BTargCopy,BTargE);
   BTargP:=ResolveAddr(BTargCopy);
   end;
  AppendPush(AParamType,BTargP);
  break;
  end;
 if ParsIsTypeRecord(BTypeE) then
  begin
  if BTypeP<>BTypeE then AppendError('Types of structural data must be identical [R:TProcAtom.PushProcParam]');
  if ParsIsParamConst(AParamType) then  BTargP:=ResolveAddr(BTargE)
  else if ParsIsParamRetOut(AParamType) then
   begin
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else if ParsIsParamVar(AParamType) then
   begin
   if ParsIsSpecTmp(BSpecE) and (ParsIsQ(BTargE)=FALSE) then AppendError('Cannot pass data as a VAR parameter [R:TProcAtom.PushProcParam]');
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else
   begin
   BTargCopy:=AppendTmpVar(BTypeP);
   AppendCmd2('mov',BTargCopy,BTargE);
   BTargP:=ResolveAddr(BTargCopy);
   end;
  {if ParsIsQ(BTargE) then BTargP:=BTargE
  else if ParsIsTargParamVar(BTargE) or ParsIsTargParamConst(BTargE) or ParsIsConst(BTargE) or ParsIsGlobal(BTargE) then BTargP:=AppendPointer(BTargE,'m')
  else BTargP:=AppendPointer(BTargE,'a'); // We do not store/load dirty LEAs }
  AppendPush(AParamType,BTargP);
  break;
  end;
 if ParsIsTypeArray(BTypeE) then
  begin
  if BTypeP<>BTypeE then AppendError('Types of arrays data must be identical [R:TProcAtom.PushProcParam]');
  if ParsIsParamConst(AParamType) then  BTargP:=ResolveAddr(BTargE)
  else if ParsIsParamRetOut(AParamType) then
   begin
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else if ParsIsParamVar(AParamType) then
   begin
   if ParsIsSpecTmp(BSpecE) and (ParsIsQ(BTargE)=FALSE) then AppendError('Cannot pass data as a VAR parameter [R:TProcAtom.PushProcParam]');
   if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
   BTargP:=ResolveAddr(BTargE);
   end
  else
   begin
   BTargCopy:=AppendTmpVar(BTypeP);
   AppendCmd2('mov',BTargCopy,BTargE);
   BTargP:=ResolveAddr(BTargCopy);
   end;
  {if ParsIsQ(BTargE) then BTargP:=BTargE
  else if ParsIsTargParamVar(BTargE) or ParsIsTargParamConst(BTargE) or ParsIsConst(BTargE) or ParsIsGlobal(BTargE) then BTargP:=AppendPointer(BTargE,'m')
  else BTargP:=AppendPointer(BTargE,'a'); // We do not store/load dirty LEAs }
  AppendPush(AParamType,BTargP);
  break;
  end;
 if ParsIsParamVar(AParamType) then
  begin
  BTargP:=ResolveAddr(BTargE,BForceLoad); if BForceLoad then ALeaList:=ALeaList+BTargE+' ';
  if ParsIsSpecTmp(BSpecE) and (ParsIsQ(BTargP)=FALSE) then AppendError('Cannot pass data as a VAR parameter [R:TProcAtom.PushProcParam]');
  if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
  if BTypeP<>BTypeE then AppendError('Types of formal and actual VAR parameters must be identical [R:TProcAtom.PushProcParam]');
  {BTargP:=AppendTmpVar('p');
  if (ParsIsTargParamVar(BTargE) or ParsIsTargParamConst(BTargE)) then AppendCmd('lam',BTargP,BTargE)
  else begin AppendCmd('st_lea',BTargE,''); ALeaList:=ALeaList+BTargE+' '; AppendCmd('lea',BTargP,BTargE); end;}
  {if ParsIsQ(BTargE) then BTargP:=BTargE
  else if ParsIsTargParamVar(BTargE) or ParsIsTargParamConst(BTargE) or ParsIsGlobal(BTargE) then BTargP:=AppendPointer(BTargE,'m')
  else begin BTargP:=AppendPointer(BTargE,'a'); AppendCmd('st_lea',BTargE,''); ALeaList:=ALeaList+BTargE+' '; end;}
  AppendPush(AParamType,BTargP);
  break;
  end;
 if ParsIsTypePointer(BTypeP) and ParsIsArray(BTargE) then
  begin
  BTargP:=ResolveAddr(BTargE,BForceLoad); if BForceLoad then ALeaList:=ALeaList+BTargE+' ';
  if ParsIsSpecTmp(BSpecE) and (ParsIsQ(BTargP)=FALSE) then AppendError('Cannot pass data as a VAR parameter [R:TProcAtom.PushProcParam]');
  if ParsIsSpecParamConst(BSpecE) then AppendError('Constant cannot be passed as a VAR parameter [R:TProcAtom.PushProcParam]');
  //if BTypeP<>BTypeE then AppendError('Types of formal and actual VAR parameters must be identical [R:TProcAtom.PushProcParam]');
  AppendPush(AParamType,BTargP);
  break;
  end;
 BTargE:=ResolveElem(BTargE);
 if ParsIsTypePointer(BTypeP) and ParsIsQ(BTargE) then
 else if (BTypeP<>'') and (BTypeP<>'_') then
  begin
  if ParsIsConst(BTargE) then ParsForceConstType(BTargE,BTypeP);
  BTargE:=ChangeTypeOpti(BTargE,BTypeP);
  end;
 AppendPush(AParamType,BTargE);
 until TRUE;
End;

Function TProcAtom.AtomizeProcCall ( Const AProcNameL, AProcParams : string ) : string;
Var
  BProcSpec,
  BNameS        : string;
  BRetType,
  BParamTypeList,
  BParamEvalList,
  BParamType,
  BParamEval,
  BParamTypeA,
  BParamEvalA   : string;
  BParamIdx     : Integer;
  BLeaList      : string;
  BLeaTarg      : string;
  BPushListA,
  BPushListB    : string;
  BDynCnt       : Integer;
Begin
 repeat
 Result:=ProcessDirectives(AProcNameL,AProcParams);
 if Result<>'' then break;

 BProcSpec:=ParsExtractType(AProcNameL);
 ParsSplitProcRetParams(BProcSpec,BRetType,BParamTypeList); if BRetType='' then BRetType:='_';
 BNameS:=ParsExtractName(AProcNameL);
 BLeaList:='';

 // Collect param list (Inverted order)
 BDynCnt:=0;
 BPushListA:='';
 BParamEvalList:=AProcParams;
 BParamIdx:=0;
 repeat
 BParamType:=ReadTillCX(BParamTypeList,CTagP,'{['); if BParamType<>'' then ResolveForwardParam(BParamType);
 if BParamType='da*' then
  begin
  BParamType:=ReadTillCX(BParamTypeList,CTagP,'{[');
  if BParamType<>'' then AppendError('Internal error: Variable param type is not the last one [R:TProcAtom.AtomizeProcCall]');
  BParamEval:=ReadTillCX(BParamEvalList,CTagP,'{[');
  if BParamEval<>CTagParamCnt then AppendError('Internal error: Incorrect name of ParamCnt [R:TProcAtom.AtomizeProcCall]');
  BPushListB:='';
  repeat
  BParamEval:=ReadTillCX(BParamEvalList,CTagP,'{[');
  if BParamEval='' then break;
  BParamEvalA:=AtomizeEval(BParamEval,'');
  BPushListB:='da'+BestTypeSuitableForPush(ExtractFinalType(BParamEvalA))+' '+BParamEvalA+' '+BPushListB;
  inc(BDynCnt);
  until FALSE;
  BPushListA:=BPushListB+' da* '+GenConstX(BDynCnt)+' '+BPushListA;
  break;
  end;
 BParamEval:=ReadTillCX(BParamEvalList,CTagP,'{[');
 if (BParamType='') and (BParamEval='') then break
 else if (BParamType='') and (BParamEval<>'') then AppendError('Extra parameter '+IntToStr(BParamIdx)+' in call of procedure '+BNameS+' [R:TProcAtom.AtomizeProcCall]')
 else if (BParamType<>'') and (BParamEval='') then AppendError('Not enough parameters, parameter '+IntToStr(BParamIdx)+' is missing [R:TProcAtom.AtomizeProcCall]')
 else
  begin
  BParamTypeA:=BParamType; Delete(BParamTypeA,1,2);
  BParamEvalA:=AtomizeEval(BParamEval,BParamTypeA);
  BPushListA:=BParamType+' '+BParamEvalA+' '+BPushListA;
  end;
 inc(BParamIdx);
 until FALSE;

 // Push in inverse order
 repeat
 BParamType:=ReadParamStr(BPushListA); BParamEval:=ReadParamStr(BPushListA); if BParamEval='' then break;
 PushProcParam(BParamType,BParamEval,BLeaList);
 until FALSE;

 // Analyse if return type is record/string and push as a parameter
 if (BRetType='_') or ParsIsTypeBasic(BRetType) then
  begin
  Result:=AppendTmpVar(BRetType);
  end
 else
  begin
  Result:=AppendTmpVar(BRetType);
  PushProcParam('dq'+BRetType,Result,BLeaList);
  end;

 // Analyse if return type is record/string
 if (BRetType='_') or ParsIsTypeBasic(BRetType) then
  begin
  AppendCall(Result,AProcNameL,BDynCnt);
  end
 else
  begin
  AppendCall(CDiscardValue,AProcNameL,BDynCnt);
  end;

 repeat
 BLeaTarg:=ReadParamStr(BLeaList);
 if BLeaTarg='' then break;
 AppendCmd2('ld_lea',BLeaTarg,'');
 until FALSE;

 until TRUE; // Filter directives
End;

Function TProcAtom.ProcessDirectives ( Const AProcNameL, AProcParams : string ) : string;
Var
  BEvalList     : string;
  BEval,
  BTarg,
  BTypeS,
  BIndexS       : string;
  BTargA        : string;
Begin
 Result:='';
 repeat
 // Inc, Dec
 if (AProcNameL=CDirectiveInc) or (AProcNameL=CDirectiveDec) then
  begin
  Result:=CDiscardValue;
  BEvalList:=AProcParams;
  BTarg:=ReadTillCX(BEvalList,CTagP,'{[');
  if BTarg='' then begin AppendError('Not enough parameters [R:TProcAtom.ProcessDirectives]'); break; end;
  if IsAssignable(BTarg)=FALSE then begin AppendError('Not assignable [R:TProcAtom.ProcessDirectives]'); break; end;
  BTypeS:=ParsExtractType(BTarg);
  if ParsIsTypePointer(BTypeS) or ParsIsTypeQ(BTypeS) then
   begin
   Delete(BTypeS,1,1);
   if BTypeS='' then BIndexS:=CConstOne
   else BIndexS:=CTagS+'c_q'+CTagM+IntToStr(FModule.GetTypeSize(BTypeS))+CTagE;
   end
  else if ParsIsTypeBasic(BTypeS) then BIndexS:=CTagS+'c_'+BTypeS+CTagM+'1'+CTagE
  else begin AppendError('Operation is not aplicable [R:TProcAtom.ProcessDirectives]'); break; end;
  BTargA:=BTarg;
  if ParsIsGlobalOrExtern(BTarg) then
   begin
   BTargA:=ResolveElem(BTarg);
   if AProcNameL=CDirectiveInc then AppendCmd2('add',BTargA,BIndexS)
   else AppendCmd2('sub',BTargA,BIndexS);
   AppendCmd2('mov_mr',BTarg,BTargA);
   end
  else
   begin
   if AProcNameL=CDirectiveInc then AppendCmd2('add',BTarg,BIndexS)
   else AppendCmd2('sub',BTarg,BIndexS);
   end;
  BTarg:=ReadTillCX(BEvalList,CTagP,'{[');
  if BTarg<>'' then begin AppendError('Extra parameter in line [R:TProcAtom.ProcessDirectives]'); break; end;
  break;
  end;
 if AProcNameL=CDirectiveChr then
  begin
  Result:=AppendTmpVar('c');
  BEvalList:=AProcParams;
  BEval:=ReadTillCX(BEvalList,CTagP,'{[');
  BTarg:=AtomizeEval(BEval,'');
  if BTarg='' then begin AppendError('Not enough parameters [R:TProcAtom.ProcessDirectives]'); break; end;
  BTypeS:=ParsExtractType(BTarg);
  if ParsIsTypeBasic(BTypeS)=FALSE then begin AppendError('Operation is not aplicable to this source type [R:TProcAtom.ProcessDirectives]'); break; end;
  AppendCmd2('mov_c'+BTypeS,Result,BTarg);
  BTarg:=ReadTillCX(BEvalList,CTagP,'{[');
  if BTarg<>'' then begin AppendError('Extra parameter in line [R:TProcAtom.ProcessDirectives]'); break; end;
  break;
  end;
 if AProcNameL=CDirectiveOrd then
  begin
  Result:=AppendTmpVar('b');
  BEvalList:=AProcParams;
  BEval:=ReadTillCX(BEvalList,CTagP,'{[');
  BTarg:=AtomizeEval(BEval,'');
  if BTarg='' then begin AppendError('Not enough parameters [R:TProcAtom.ProcessDirectives]'); break; end;
  BTarg:=ResolveElem(BTarg);
  BTypeS:=ParsExtractType(BTarg);
  if BTypeS<>'c' then begin AppendError('Operation is not aplicable: source type is not char [R:TProcAtom.ProcessDirectives]'); break; end;
  AppendCmd2('mov_bc',Result,BTarg);
  BTarg:=ReadTillCX(BEvalList,CTagP,'{[');
  if BTarg<>'' then begin AppendError('Extra parameter in line [R:TProcAtom.ProcessDirectives]'); break; end;
  break;
  end;
 until TRUE;
End;

Function TProcAtom.ProcessEvalEtCalls ( Const AExecS : string ) : boolean;
Var
  BExecS        : string;
  BPos          : Integer;
  BEvalSrc,
  BEvalDst      : string;
  BProcNameL,
  BProcParams,
  BTmpVar       : string;
  BTargS,
  BTargD        : string;
  BTypeS,
  BTypeD        : string;
  BTargST       : string;
  BTargSA       : string;
//  BTargArr      : string;
//  BTargIdx      : string;
//  BTargPS,
//  BTargPD       : string;
//  BSize         : Integer;
//  BTargSize     : string;
Begin
 Result:=FALSE;
 BExecS:=AExecS;
 repeat
 if Pos(':=',BExecS)=0 then break;
 BEvalSrc:=BExecS;
 BEvalDst:=ReadTillS(BEvalSrc,':=');
 DelFirstSpace(BEvalSrc); DelLastSpace(BEvalSrc);
 DelFirstSpace(BEvalDst); DelLastSpace(BEvalDst);

 repeat
 ExtractProc(BEvalSrc,BProcNameL,BProcParams,BPos);
 if BProcNameL='' then break;
 BTmpVar:=AtomizeProcCall(BProcNameL,BProcParams);
 Insert(BTmpVar,BEvalSrc,BPos);
 until FALSE;

 repeat
 ExtractProc(BEvalDst,BProcNameL,BProcParams,BPos);
 if BProcNameL='' then break;
 BTmpVar:=AtomizeProcCall(BProcNameL,BProcParams);
 Insert(BTmpVar,BEvalDst,BPos);
 until FALSE;

 BTargD:=AtomizeEval(BEvalDst,'');     BTypeD:=ExtractFinalType(BTargD);
 BTargS:=AtomizeEval(BEvalSrc,BTypeD); BTypeS:=ExtractFinalType(BTargS);

 if IsAssignable(BTargD)=FALSE then AppendError('Left side of expression cannot be assigned [R:TProcAtom.ProcessEvalCalls]');

 if ParsIsTypeRecord(BTypeD) then // Record assign to record
  begin
  if BTypeS<>BTypeD then AppendError('Left and right sides must be of the same type [R:TProcAtom.ProcessEvalCalls]');
  AppendCmd2('copy_rec',BTargD,BTargS);
  {AppendPush('dap',QResolveAddr(BTargS));
  AppendPush('dap',QResolveAddr(BTargD));
  BSize:=FModule.GetTypeSize(BTypeD);
  BTargSize:=AppendTmpVar('d');
  AppendCmd('mov',BTargSize,CTagS+'c_d'+CTagM+IntToStr(BSize)+CTagE);
  AppendPush('dad',BTargSize);
  AppendCall(CDiscardValue,CProcMemCopy);}
  Result:=TRUE;
  break;
  end;

 if ParsIsTypeArray(BTypeD) then // Array assign to array
  begin
  if BTypeS<>BTypeD then AppendError('Left and right sides must be of the same type [R:TProcAtom.ProcessEvalCalls]');
  AppendCmd2('copy_arr',BTargD,BTargS);
  Result:=TRUE;
  break;
  end;

 if ParsIsTypeStringP(BTypeD) then // Will be processed in LlvmStrRec
  begin
  AppendCmd2('mov',BTargD,BTargS);
  Result:=TRUE;
  break;
  end;

 {if ParsIsRecord(BTargD) then
  begin
  if (BTypeD<>'') and (BTypeD<>'_') then
   begin
   if ParsIsConst(BTargS) then
    begin
    ParsForceConstType(BTargS,BTypeD);
    BTargSA:=AppendTmpVar(BTypeD);
    AppendCmd('mov',BTargSA,BTargS);
    BTargS:=BTargSA;
    end;
   BTargST:=ChangeTypeOpti(BTargS,BTypeD);
   end
  else BTargST:=BTargS;
  AppendCmd('mov_mr',BTargD,BTargST);
  Result:=TRUE;
  break;
  end;}

 if BTypeD='?' then // Very special case: when compiling 'case' statement we do not know type in advance
  begin
  BTypeD:=ParsExtractType(BTargS);
  ParsReplaceAll(FTextSrc,BTargD,BTargS);
  BTargD:=BTargS;
  end;

 BTargS:=ResolveElem(BTargS);
 {if ParsIsGlobal(BTargS) then   // <- ResolveElem resolvs also for globals
  begin
  BTargSA:=AppendTmpVar(BTypeS);
  AppendCmd('mov_rm',BTargSA,BTargS);
  BTargS:=BTargSA;
  end; }

 if ParsIsQ(BTargD) or ParsIsGlobalOrExtern(BTargD) or ParsIsRecord(BTargD) or ((ParsIsArray(BTargD) or ParsIsPointer(BTargD)) and (Pos('[',BTargD)<>0)) then
  begin
  if (BTypeD<>'') and (BTypeD<>'_') then
   begin
   if ParsIsConst(BTargS) then
    begin
    if ParsIsStringZ(BTargS) then
     begin
     BTargSA:=AppendTmpVar(BTypeD);
     AppendCmd2('mov',BTargSA,BTargS);
     BTargS:=BTargSA;
     end
    else
     begin
     ParsForceConstType(BTargS,BTypeD);
     BTargSA:=AppendTmpVar(BTypeD);
     AppendCmd2('mov',BTargSA,BTargS);
     BTargS:=BTargSA;
     end;
    end;
   if ParsIsTypePointer(BTypeD) and ParsIsTypeQ(BTypeS) then BTargST:=BTargS
   else BTargST:=ChangeTypeOpti(BTargS,BTypeD);
   end
  else BTargST:=BTargS;
  AppendCmd2('mov_mr',BTargD,BTargST);
  Result:=TRUE;
  break;
  end;

 if ParsIsTypePointer(BTypeD) and ParsIsTypeStringZ(BTypeS) then
  begin
  AppendCmd2('mov',BTargD,BTargS);
  Result:=TRUE;
  break;
  end;

 if ParsIsTypePointer(BTypeD) and ParsIsQ(BTargS) then BTargST:=BTargS
 else if ParsIsTypePointer(BTypeD) and ParsIsTypePointer(BTypeS) then BTargST:=BTargS
 else if (BTypeD<>'') and (BTypeD<>'_') then
  begin
  if ParsIsConst(BTargS) then ParsForceConstType(BTargS,BTypeD);
  BTargST:=ChangeTypeOpti(BTargS,BTypeD);
  end
 else BTargST:=BTargS;

 if ParsIsConst(BTargST) and ParsIsGlobalOrExtern(BTargD) then
  begin
  BTargS:=AppendTmpVar(BTypeD);
  AppendCmd2('mov',BTargS,BTargST);
  AppendCmd2('mov',BTargD,BTargS);
  end
 else AppendCmd2('mov',BTargD,BTargST);
 Result:=TRUE;
 until TRUE;
End;

// Continues reading EVAL if command is not CMP (logical AND, Logical OR)
Procedure TProcAtom.ReadEvalForIfContinue ( Var AEval : string; Var ASubEval : string );
Var
  BEval         : string;
  BCmd          : string;
Begin
 BEval:=AEval;
 repeat
 BCmd:=ReadParamStr(BEval);
 if BCmd='' then break;
 if Pos('cmp',BCmd)=1 then break;
 if (BCmd='and') or (BCmd='or') then break;
 ASubEval:=ASubEval+' '+BCmd+' '+ReadEvalAtom(BEval);
 AEval:=BEval;
 until FALSE;
End;


Procedure TProcAtom.ProcessIfA ( Const AEval : string; Const ALabelThen, ALabelElse : string );
Var
  BLabelThen,
  BLabelElse    : string;
  BEval         : string;
  BEvalA,
  BEvalB,
  BOper,
  BCond         : string;
  BTargA,
  BTargB        : string;
  BValA         : string;
  BLabelMid     : string;
  BTypeA,
  BTypeB,
  BTypeR        : string;
  BTargR        : string;
  BIsConstA,
  BIsConstB     : boolean;
  BNotA,
  BNotB         : boolean;
  BNotS         : string;
Begin
 BLabelThen:=ALabelThen; BLabelElse:=ALabelElse;
 BEval:=AEval;
 DelFirstSpace(BEval); DelLastSpace(BEval);
 DelExtraBrackets(BEval);

 BNotA:=FALSE; BNotB:=FALSE;

 repeat
 BEvalA:=ReadEvalAtom(BEval);
 if BEvalA='not' then begin BNotA:=TRUE; BEvalA:=ReadEvalAtom(BEval); end;
 if BEvalA='' then begin AppendError('Internal error: No EvalA [R:TProcAtom.ProcessIfA]'); break; end;
 ReadEvalForIfContinue(BEval,BEvalA);
 BOper:=ReadParamStr(BEval);
 if BOper='' then
  begin
  if BNotA then XchgStrings(BLabelThen,BLabelElse);
  BTargA:=ResolveElem(AtomizeEval(BEvalA,''));
  if ParsIsConst(BTargA) then
   begin
   BValA:=ParsExtractName(BTargA);
   if BValA='0' then AppendJmp('jmp',BLabelElse)
   else AppendJmp('jmp',BLabelThen);
   end
  else
   begin
   AppendCmd2('cmp',BTargA,CBooleanFalse);  // Old code:  AppendCmd('cmp',BTargA,CBooleanTrue); <- Cannot use BooleanTrue here, because the comparison is then with '1' (and must be with '0')
   AppendJmp('jne',BLabelThen);            //            AppendJmp('je',ALabelThen);
   AppendJmp('jmp',BLabelElse);
   end;
  break;
  end;

 if Pos('cmp',BOper)=1 then
  begin
  BEvalB:=ReadEvalAtom(BEval); DelFirstSpace(BEval);
  if BEvalB='not' then begin BNotB:=TRUE; BEvalB:=ReadEvalAtom(BEval); DelFirstSpace(BEval); end;
  if BEvalB='' then begin AppendError('Internal error: No EvalB [R:TProcAtom.ProcessIfA]'); break; end;
  ReadEvalForIfContinue(BEval,BEvalB);
  if BEval<>'' then begin AppendError('Inconsistent IF. Append brackets in condition [R:TProcAtom.ProcessIfA]'); break; end;

  BCond:=BOper; Delete(BCond,1,3);
  BTargA:=AtomizeEval(BEvalA,'');
  BTargB:=AtomizeEval(BEvalB,'');

  BTypeA:=ExtractFinalType(BTargA);
  BTypeB:=ExtractFinalType(BTargB);
  if TypesCompatible(BTypeA,BTypeB,BTypeR)=FALSE then AppendError('Incompatible types [R:TProcAtom.ProcessIfA]');

  BIsConstA:=ParsIsConst(BTargA);
  BIsConstB:=ParsIsConst(BTargB);

  if (BIsConstA and BIsConstB) then
   begin
   BTargR:=ProcConst2(BTargA,BTargB,BOper,BTypeR);
   if ParsExtractType(BTargR)<>'l' then AppendError('Boolean value expected [R:TProcAtom.ProcessIfA]');
   if BNotA xor BNotB then XchgStrings(BLabelThen,BLabelElse);
   BValA:=ParsExtractName(BTargA);
   if BValA='0' then AppendJmp('jmp',BLabelElse)
   else AppendJmp('jmp',BLabelThen);
   break;
   end;

  if ParsIsTypeStringP(BTypeR) or ParsIsTypeRecord(BTypeR) then
  else
   begin
   BTargA:=ResolveElem(BTargA);
   BTargB:=ResolveElem(BTargB);
   end;

  if ParsIsTypeStringP(BTypeR) or ParsIsTypeRecord(BTypeR) then
  else if ParsIsTypePointer(BTypeA) and ParsIsConst(BTargB) then ParsForceConstType(BTargB,BTypeA)
  else if ParsIsTypePointer(BTypeB) and ParsIsConst(BTargA) then ParsForceConstType(BTargA,BTypeB)
  else
   begin
   if BIsConstA then ParsForceConstType(BTargA,BTypeR) else BTargA:=ChangeTypeOpti(BTargA,BTypeR);
   if BIsConstB then ParsForceConstType(BTargB,BTypeR) else BTargB:=ChangeTypeOpti(BTargB,BTypeR);
   end;

  if BTypeR='l' then
   begin
   if BNotA xor BNotB then XchgStrings(BLabelThen,BLabelElse);
   end
  else
   begin
   if BNotA then BTargA:=AtomizeEval('not '+BTargA,'');
   if BNotB then BTargB:=AtomizeEval('not '+BTargB,'');
   end;

  AppendCmd2('cmp',BTargA,BTargB);
  AppendJmp('j'+ProcSignedCond(BCond,BTypeR),BLabelThen);
  AppendJmp('jmp',BLabelElse);
  break;
  end;

 BNotS:='';
 if BNotA then BNotS:='not ';
 repeat
 BLabelMid:=NewLabelName('M');
 if BOper='and' then ProcessIfA(BNotS+BEvalA,BLabelMid,ALabelElse)
 else if BOper='or' then ProcessIfA(BNotS+BEvalA,ALabelThen,BLabelMid)
 else begin AppendError('Invalid operator "'+BOper+'", only AND and OR are supported [R:TProcAtom.ProcessIfA]'); break; end;
 AppendLabel(BLabelMid);
 BNotS:=''; BEvalA:=ReadEvalAtom(BEval);
 if BEvalA='not' then begin BNotS:='not '; BEvalA:=ReadEvalAtom(BEval); end;
 if BEvalA='' then begin AppendError('Internal error: No Eval [R:TProcAtom.ProcessIfA]'); break; end;
 BOper:=ReadParamStr(BEval);
 if BOper='' then
  begin
  ProcessIfA(BNotS+BEvalA,ALabelThen,ALabelElse);
  break;
  end;
 until FALSE;

 until TRUE;
End;

Function TProcAtom.ProcessIf ( Const AExecS : string ) : boolean;
Var
  BExecS        : string;
  BEval         : string;
  BLabelThen,
  BLabelElse    : string;
Begin
 Result:=FALSE;
 BExecS:=AExecS;
 repeat
 if ReadParamStr(BExecS)<>'if' then break;
 BLabelElse:=ReadParamStrInv(BExecS);
 BLabelThen:=ReadParamStrInv(BExecS);
 BEval:=BExecS;

 ProcessIfA(BEval,BLabelThen,BLabelElse);

 Result:=TRUE;
 until TRUE;
End;

Procedure TProcAtom.AppendMovCheckType ( Const ATargD, ATargS : string );
Var
  BTypeD,
  BTypeS        : string;
Begin
 BTypeD:=ParsExtractType(ATargD);
 BTypeS:=ParsExtractType(ATargS);
 if BTypeD=BTypeS then AppendCmd2('mov',ATargD,ATargS)
 else AppendCmd2('mov_'+BTypeD+BTypeS,ATargD,ATargS);
End;

Function TProcAtom.ProcConst2 ( Const ATargA, ATargB : string; Const AOper : string; Const ATypeR : string ) : string;
Var
  BNameA,
  BNameB,
  BNameR        : string;
  BValueA,
  BValueB,
  BValueR       : Int64;
  BValueFA,
  BValueFB,
  BValueFR      : Double;
  BValueSA,
  BValueSB,
  BValueSR      : string;
Begin
 Result:=CInvalidIdentifier;
 repeat
 BNameA:=ParsExtractName(ATargA);
 BNameB:=ParsExtractName(ATargB);
 if (ATypeR='b') or (ATypeR='w') or (ATypeR='d') or (ATypeR='i') then
  begin
  if ATypeR='i' then
   begin
   if TryStrToInt64(BNameA,BValueA)=FALSE then AppendError('Error converting string '+BNameA+' to value [R:TProcAtom.ProcConst2]');
   if TryStrToInt64(BNameB,BValueB)=FALSE then AppendError('Error converting string '+BNameB+' to value [R:TProcAtom.ProcConst2]');
   end
  else
   begin
   if TryStrToInt0x(BNameA,BValueA)=FALSE then AppendError('Error converting string '+BNameA+' to value [R:TProcAtom.ProcConst2]');
   if TryStrToInt0x(BNameB,BValueB)=FALSE then AppendError('Error converting string '+BNameB+' to value [R:TProcAtom.ProcConst2]');
   end;
  case AOper of
    'add': BValueR:=BValueA+BValueB;
    'sub': BValueR:=BValueA-BValueB;
    'and': BValueR:=BValueA and BValueB;
     'or': BValueR:=BValueA or BValueB;
    'xor': BValueR:=BValueA xor BValueB;
    'mul': BValueR:=BValueA*BValueB;
    'div': begin
           if BValueB=0 then begin BValueR:=0; AppendError('Zero division [R:TProcAtom.ProcConst2]'); end
           else BValueR:=BValueA div BValueB;
           end;
    'shl': BValueR:=BValueA shl BValueB;
    'shr': BValueR:=BValueA shr BValueB;
    'cmpe':  begin if BValueA=BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpne': begin if BValueA<>BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpa':  begin if BValueA>BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpb':  begin if BValueA<BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpae': begin if BValueA>=BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpbe': begin if BValueA<=BValueB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    else begin AppendError('Operation '+AOper+' is not supported [R:TProcAtom.ProcConst2]'); break; end;
  end; // case
  if (Pos('0x',BNameA)<>0) or (Pos('0x',BNameB)<>0) then
   begin
   if ATypeR='b' then BNameR:='0x'+IntToHex(BValueR,2)
   else if ATypeR='w' then BNameR:='0x'+IntToHex(BValueR,4)
   else BNameR:='0x'+IntToHex(BValueR,8);
   end
  else
   begin
   BNameR:=IntToStr(BValueR);
   end;
  Result:=CTagS+'c_'+ATypeR+CTagM+BNameR+CTagE;
  break;
  end;

 if ATypeR='f' then
  begin
  if TryStrToFloat(BNameA,BValueFA,HParsFormat)=FALSE then AppendError('Error converting string '+BNameA+' to value [R:TProcAtom.ProcConst2]');
  if TryStrToFloat(BNameB,BValueFB,HParsFormat)=FALSE then AppendError('Error converting string '+BNameB+' to value [R:TProcAtom.ProcConst2]');
  case AOper of
    'add': BValueFR:=BValueFA+BValueFB;
    'sub': BValueFR:=BValueFA-BValueFB;
    'mul': BValueFR:=BValueFA*BValueFB;
    'div': begin
           if BValueFB=0 then begin BValueFR:=0; AppendError('Zero division [R:TProcAtom.ProcConst2]'); end
           else BValueFR:=BValueFA / BValueFB;
           end;
    else begin AppendError('Operation '+AOper+' is not supported [R:TProcAtom.ProcConst2]'); break; end;
  end; // case
  BNameR:=FloatToStr(BValueFR);
  Result:=CTagS+'c_'+ATypeR+CTagM+BNameR+CTagE;
  break;
  end;

 if ParsIsTypeStringP(ATypeR) then
  begin
  BValueSA:=''; BValueSB:='';
  // Targ A
  if ParsIsStringP(ATargA) then BValueSA:=FModule.GetConstValue(ATargA)
  else if ParsExtractType(ATargA)='c' then
   begin
   if IsInteger(BNameA,BValueA)=FALSE then AppendError('Internal error. Cannot convert '+BNameA+' to integer [R:TProcAtom.ProcConst2]');
   if (BValueA<0) or (BValueA>255) then AppendError('Internal error. Invalid value for char constant '+BNameA+' [R:TProcAtom.ProcConst2]');
   BValueSA:=BValueSA+Chr(BValueA);
   end
  else AppendError('Constant '+BNameA+' is not a string [R:TProcAtom.ProcConst2]');
  // Targ B
  if ParsIsStringP(ATargB) then BValueSB:=FModule.GetConstValue(ATargB)
  else if ParsExtractType(ATargB)='c' then
   begin
   if IsInteger(BNameB,BValueB)=FALSE then AppendError('Internal error. Cannot convert '+BNameB+' to integer [R:TProcAtom.ProcConst2]');
   if (BValueB<0) or (BValueB>255) then AppendError('Internal error. Invalid value for char constant '+BNameB+' [R:TProcAtom.ProcConst2]');
   BValueSB:=BValueSB+Chr(BValueB);
   end
  else AppendError('Constant '+BNameB+' is not a string [R:TProcAtom.ProcConst2]');
  // Operation
  case AOper of
    'add': BValueSR:=BValueSA+BValueSB;
    'cmpe':  begin if BValueSA=BValueSB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpne': begin if BValueSA<>BValueSB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    else AppendError('Operation '+AOper+' is not supported for these operands [R:TProcAtom.ProcConst2]');
  end; // case
  Result:=FModule.AppendConst('sp',BValueSR);
  break;
  end;

 if ParsIsTypeStringZ(ATypeR) then
  begin
  BValueSA:=''; BValueSB:='';
  // Targ A
  if ParsIsStringZ(ATargA) then BValueSA:=FModule.GetConstValue(ATargA)
  else if ParsExtractType(ATargA)='c' then
   begin
   if IsInteger(BNameA,BValueA)=FALSE then AppendError('Internal error. Cannot convert '+BNameA+' to integer [R:TProcAtom.ProcConst2]');
   if (BValueA<0) or (BValueA>255) then AppendError('Internal error. Invalid value for char constant '+BNameA+' [R:TProcAtom.ProcConst2]');
   BValueSA:=BValueSA+Chr(BValueA);
   end
  else AppendError('Constant '+BNameA+' is not a string [R:TProcAtom.ProcConst2]');
  // Targ B
  if ParsIsStringZ(ATargB) then BValueSB:=FModule.GetConstValue(ATargB)
  else if ParsExtractType(ATargB)='c' then
   begin
   if IsInteger(BNameB,BValueB)=FALSE then AppendError('Internal error. Cannot convert '+BNameB+' to integer [R:TProcAtom.ProcConst2]');
   if (BValueB<0) or (BValueB>255) then AppendError('Internal error. Invalid value for char constant '+BNameB+' [R:TProcAtom.ProcConst2]');
   BValueSB:=BValueSB+Chr(BValueB);
   end
  else AppendError('Constant '+BNameB+' is not a string [R:TProcAtom.ProcConst2]');
  // Operation
  case AOper of
    'add': BValueSR:=BValueSA+BValueSB;
    'cmpe':  begin if BValueSA=BValueSB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    'cmpne': begin if BValueSA<>BValueSB then Result:=CBooleanTrue else Result:=CBooleanFalse; break; end;
    else AppendError('Operation '+AOper+' is not supported for these operands [R:TProcAtom.ProcConst2]');
  end; // case
  Result:=FModule.AppendConst('sz',BValueSR);
  break;
  end;

 AppendError('Unsupported constant types [R:TProcAtom.ProcConst2]');
 until TRUE;
End;

Function TProcAtom.AtomizeEval2 ( Const AEval : string; Const AWantedType : string ) : string;
Var
  BTargA,
  BTargB,
  BOper         : string;
  BEval         : string;
  BTypeA,
  BTypeB        : string;
  BTypeR        : string;
  BTargR        : string;
  BTargBT       : string;
  BIsConstA,
  BIsConstB     : boolean;
  BCond         : string;
  BLabelName    : string;
  BResultBool   : string;
  BPromoteTypeB : boolean;
Begin
 Result:='';
 BCond:='';
 BEval:=AEval;
 repeat
 BTargA:=AtomizeEval(ReadParamStr(BEval),'');
 BOper:=ReadParamStr(BEval);
 BTargB:=AtomizeEval(ReadParamStr(BEval),'');
 if ReadParamStr(BEval)<>'' then AppendError('Internal error or extra parameter in line [R:TProcAtom.AtomizeEval2]');
 if BTargB='' then begin AppendError('Internal error or not enough parameters in line [R:TProcAtom.AtomizeEval2]'); break; end;

 BTypeA:=ExtractFinalType(BTargA);
 BTypeB:=ExtractFinalType(BTargB);
 if BOper='type' then
  begin
  if TypesCompatible(BTypeA,BTypeB,BTypeR)=FALSE then AppendError('Incompatible types [R:TProcAtom.AtomizeEval2]');
  BTypeR:=BTypeA;
  Result:=ChangeTypeOpti(BTargB,BTypeR);
  break;
  end;

 if StrInList(BOper,'shl shr') then
  begin
  if StrInList(BTypeB,'b w d i') then
  else AppendError('Only integer type allowed for shift [R:TProcAtom.AtomizeEval2]');
  BTypeR:=BTypeA;
  BPromoteTypeB:=FALSE;
  end
 else if BTargB=CConstSizeOfType then
  begin
  BTargB:=GenConstX(FModule.GetTypeSize(ExtractFinalType(BTargA)));
  BTypeR:=BTypeA;
  BPromoteTypeB:=TRUE;
  end
 else
  begin
  if TypesCompatible(BTypeA,BTypeB,BTypeR)=FALSE then AppendError('Incompatible types [R:TProcAtom.AtomizeEval2]');
  BPromoteTypeB:=TRUE;
  end;

 BIsConstA:=ParsIsConst(BTargA);
 BIsConstB:=ParsIsConst(BTargB);

 if (BIsConstA and BIsConstB) then
  begin
  Result:=ProcConst2(BTargA,BTargB,BOper,BTypeR);
  break;
  end;

 if StrInList(BOper,'cmpe cmpne cmpb cmpa cmpbe cmpae') then
  begin
  BCond:=BOper; Delete(BCond,1,3);
  if BCond='' then AppendError('Internal error: missing condition [R:TProcAtom.AtomizeEval2]');
  BOper:='cmp';
  BResultBool:=AppendTmpVar('l');
  AppendCmd2('mov',BResultBool,CBooleanTrue);
  end;

 if ParsIsTypeStringP(BTypeR) then
  begin
  BTargR:=AppendTmpVar(BTypeR);
  AppendCmd2('mov',BTargR,BTargA);
  AppendCmd2(BOper,BTargR,BTargB);
  Result:=BTargR;
  break;
  end;

 BTargA:=ResolveElem(BTargA);
 BTargB:=ResolveElem(BTargB);

 BTypeR:=PromoteType(BTypeR,AWantedType);
 if BIsConstA then ParsForceConstType(BTargA,BTypeR);
 if BIsConstB and BPromoteTypeB then ParsForceConstType(BTargB,BTypeR);

 BTargR:=AppendTmpVar(BTypeR);
 AppendMovCheckType(BTargR,BTargA);
 if (BIsConstB=FALSE) and BPromoteTypeB then BTargBT:=ChangeTypeOpti(BTargB,BTypeR)
 else BTargBT:=BTargB;
 AppendCmd2(BOper,BTargR,BTargBT);
 Result:=BTargR;
 until TRUE;

 if BCond<>'' then
  begin
  BLabelName:=NewLabelName('Bool');
  AppendCmd2('j'+BCond,BLabelName,'');
  AppendCmd2('mov',BResultBool,CBooleanFalse);
  AppendLabel(BLabelName);
  Result:=BResultBool;
  end;
End;

Procedure TProcAtom.GetFieldOffsetA ( Const ARecType : string; Const AFieldThis : string; Out AResType : string; Out AOffset : string );
Var
  BPosA,
  BPosB         : Integer;
  BFieldList    : string;
  BType,
  BName,
  BOffset       : string;
Begin
 AOffset:='';
 if ParsIsTypeRecord(ARecType)=FALSE then AppendError('Parent of '+AFieldThis+' is not a record [R:TProcAtom.GetFieldOffsetA]');
 BPosA:=Pos('{',ARecType); BPosB:=LastDelimiter('}',ARecType);
 BFieldList:=Copy(ARecType,BPosA+1,BPosB-BPosA-1);
 BType:=''; BName:=''; BOffset:='';
 repeat
 ParsReadRecField(BFieldList,BType,BName,BOffset);
 if BName='' then break;
 if LowerCase(BName)=LowerCase(AFieldThis) then break;
 until FALSE;

 if BName='' then  AppendError('Field '+AFieldThis+' not found [R:TProcAtom.GetFieldOffset]');
 if BOffset<>'' then AOffset:=CTagS+'c_i'+CTagM+BOffset+CTagE;
 AResType:=BType;
End;

Function TProcAtom.AtomizeTarg ( Const ATarg : string ) : string;
Var
  BBaseThis,
  BOffsThis,
  BTypeThis     : string;
Begin
 AtomizeTarg(ATarg,BBaseThis,BOffsThis,BTypeThis);
 if BOffsThis='' then Result:=BBaseThis
 else Result:=BBaseThis+'['+ResolveElem(AtomizeEval(BOffsThis,''))+']_'+BTypeThis;
End;

Procedure TProcAtom.AtomizeTarg ( Const ATarg : string; Out ABaseThis, AOffsThis, ATypeThis : string );
Var
  BEval         : string;
  BTargS        : string;
  BIndexS       : string;
  BElemType     : string;
  BElemSize     : Integer;
  BTargIdx,
  BTargIdxA     : string;
  BTargFldA     : string;
  BReadS        : string;
  BIndexList,
  BTypeList     : string;
  BIndexSA,
  BElemTypeA    : string;
  BDimS,
  BDimE         : Integer;
  BSpec         : string;
Begin
 BEval:=ATarg;
 repeat
 ABaseThis:=ReadParamStr(BEval,CTagE)+CTagE;
 AOffsThis:='';
 ATypeThis:=ParsExtractType(ABaseThis); if ATypeThis='' then begin AppendError('e','Cannot extract type or unknown identifier '+ParsExtractName(ABaseThis)+' [R:TProcAtom.AtomizeTarg]'); break; end;
 repeat
 //if (BOffsThis<>'') and (Pos(#32,BOffsThis)=0) and (ParsExtractName(BOffsThis)='0') then BOffsThis:='';
 DelFirstSpace(BEval);
 if BEval='' then break;
 if BEval[1]='[' then
  begin
  ReadInside(BEval,1,BIndexS); DelFirstSpace(BEval);
  if ParsIsTypeQ(ATypeThis) or ParsIsTypePointer(ATypeThis) or ParsIsTypeStringX(ATypeThis) or ParsIsTypeArray(ATypeThis) then
  else AppendError('Identifier '+ParsExtractName(ABaseThis)+' cannot be indexed [R:TProcAtom.AtomizeTarg]');
  if ParsIsTypeRecord(ATypeThis) or ParsIsTypeStringX(ATypeThis) or ParsIsTypeArray(ATypeThis) then // Avoid conflicts with ldp_s
   begin
   if ParsIsTargParamVar(ABaseThis) or ParsIsTargParamConst(ABaseThis) or ParsIsTargResult(ABaseThis) then ABaseThis:=AppendPointer(ABaseThis,'m');
   end;
  Delete(BIndexS,1,1); if (BIndexS<>'') and (BIndexS[Length(BindexS)]=']') then Delete(BIndexS,Length(BIndexS),1);
  DelFirstSpace(BIndexS); DelLastSpace(BIndexS);
  // Index extraction: there can be multiple indexes for multi-dimensional arrays (comma separated)
  BElemType:=ATypeThis;
  BIndexList:=''; BTypeList:='';
  repeat
  BReadS:=ReadParamStr(BIndexS,','); DelFirstSpace(BIndexS);
  if BReadS='' then break;
  BIndexSA:=ResolveElem(AtomizeEval(BReadS,''));
  BElemTypeA:=ParsExtractTypeParentType(BElemType);
  if BElemTypeA='' then begin AppendError('e','Array dimension missmatch: required dimension is higher than array dimension (or type '+BElemType+') cannot be indexed [R:TProcAtom.AtomizeTarg]'); break; end;
  if ParsIsTypeArray(BElemTypeA) or ParsIsTypeStringX(BElemTypeA) or ParsIsTypePointer(BElemTypeA) or ParsIsTypeQ(BElemTypeA) then
  else AppendError('e','Element is not an array and cannot be indexed [R:TProcAtom.AtomizeTarg]');
  BIndexList:=BIndexList+BIndexSA+' ';
  BTypeList:=BTypeList+BElemTypeA+' ';
  until FALSE;
  // for ~dba0s3ea1s10ei:BData#[1,2] we will have
  // BIndexList = ~c_b:1# ~c_b:2#
  // BTypeList = a0s3e a1s10e
  // BElemType = i
  // Final index should be:
  // ((1-0)*(10-1+1)+(2-1))*4
  BIndexS:='';
  repeat
  BIndexSA:=ReadParamStr(BIndexList);
  if BIndexSA='' then break;
  BElemTypeA:=ReadParamStr(BTypeList); ParsGetTypeArrayDim(BElemTypeA,BDimS,BDimE);
  if BIndexS<>'' then BIndexS:='('+BIndexS+') mul '+CTagS+'c_i'+CTagM+IntToStr(BDimE-BDimS+1)+CTagE;
  if BIndexS<>'' then BIndexS:=BIndexS+' add ';
  BIndexS:=BIndexS+BIndexSA+' sub '+CTagS+'c_i'+CTagM+IntToStr(BDimS)+CTagE;
  until FALSE;
  BElemSize:=FModule.GetTypeSize(BElemType);
  if ParsIsTypeRecord(BElemType) then FModule.AlignSize(BElemSize);
  BIndexS:='('+BIndexS+') mul '+CTagS+'c_i'+CTagM+IntToStr(BElemSize)+CTagE;
  BTargIdx:=ResolveElem(AtomizeEval(BIndexS,''));
  // End of index computation
  if ParsIsConst(BTargIdx) then
   begin
   BTargIdxA:=BTargIdx;
   end
  else
   begin
   if ParsIsQ(ABaseThis) then
   else if (ParsIsArray(ABaseThis) or ParsIsStringX(ABaseThis) or ParsIsRecord(ABaseThis)) and ParsIsLocalOrTmp(ABaseThis) then
    begin
    ABaseThis:=AppendPointer(ABaseThis,'a');
    end;
   if CanBeUsedAsIndex(BTargIdx)=FALSE then AppendError(ParsExtractName(BTargIdx)+' cannot be used as an index [R:TProcAtom.AtomizeTarg]');
   BTargIdxA:=BTargIdx;
   {if IsValidArrayIdx(BTargIdx)=FALSE then BTargIdxA:=ChangeTypeOpti(BTargIdx,FModule.IndexType)
   else if BElemSize<>1 then
    begin
    BTargIdxA:=AppendTmpVar(FModule.IndexType);
    AppendCmd('mov',BTargIdxA,BTargIdx);
    end;
   if BElemSize>1 then AppendCmd('mul',BTargIdxA,CTagS+'c_d'+CTagM+IntToStr(BElemSize)+CTagE);}
   end;
  if ParsIsTypePointer(ATypeThis) then
   begin
   if AOffsThis='' then AOffsThis:=CConstZero
   else AOffsThis:=AtomizeEval(AOffsThis,''); // ResolveElem is not necessary, because we assemble OffsThis ourselves
   BTargS:=AppendTmpVar('q'+BElemType);
   BSpec:=ParsExtractSpec(ABaseThis);
   if ParsIsSpecParam(BSpec) or ParsIsSpecLocal(BSpec) or ParsIsSpecTmp(BSpec) or ParsIsSpecResult(BSpec) or ParsIsSpecParamVar(BSpec) then // These will conflict with 'ldp_s' added by LlvmUseMatr
    begin
    AppendCmd2('mov',BTargS,ABaseThis);
    if ParsExtractName(AOffsThis)<>'0' then AppendCmd2('add',BTargS,AOffsThis);
    end
   else AppendCmd2('mov_rm',BTargS,ABaseThis+'['+AOffsThis+']');
   ABaseThis:=BTargS; AOffsThis:=BTargIdxA;
   end
  else
   begin
   if AOffsThis='' then AOffsThis:=BTargIdxA else AOffsThis:=AtomizeEval2(AOffsThis+' add '+BTargIdxA,'');
   end;
  ATypeThis:=BElemType;
  continue;
  end;
 if BEval[1]='.' then
  begin
  Delete(BEval,1,1); DelFirstSpace(BEval);
  BTargFldA:=ReadTillA(BEval,'.[');
  if BTargFldA='' then begin  AppendError('Field name is missing [R:TProcAtom.AtomizeTarg]'); break; end;
  GetFieldOffsetA(ATypeThis,BTargFldA,BElemType,BIndexS);
  if BElemType='' then break;
  ResolveForwardType(BElemType);
  BTargIdx:=ResolveElem(AtomizeEval(BIndexS,''));
  if CanBeUsedAsIndex(BTargIdx)=FALSE then AppendError('Identifier "'+ParsExtractName(BTargIdx)+'" cannot be used as an index [R:TProcAtom.AtomizeTarg]');
  if IsValidArrayIdx(BTargIdx) then BTargIdxA:=BTargIdx
  else BTargIdxA:=ChangeTypeOpti(BTargIdx,FModule.IndexType);
  if ParsIsTargParamVar(ABaseThis) or ParsIsTargParamConst(ABaseThis) or ParsIsTargResult(ABaseThis) then
   begin
   ABaseThis:=AppendPointer(ABaseThis,'m');
   end
  else if ParsIsConst(BTargIdxA) then
  else if ParsIsQ(ABaseThis) then
  else
   begin
   if ParsIsLocalOrTmp(ABaseThis) then ABaseThis:=AppendPointer(ABaseThis,'a');
   end;
  if AOffsThis='' then AOffsThis:=BTargIdxA else AOffsThis:=AtomizeEval2(AOffsThis+' add '+BTargIdxA,'');
  ATypeThis:=BElemType;
  continue;
  end;
 AppendError('e','Internal error [R:TProcAtom.AtomizeTarg]');
 break;
 until FALSE;

 until TRUE;
End;

Const
  CPromotedTypes        = 'bkwmdif';

Function TProcAtom.PromoteType ( Const ATypeA, ATypeB : string ) : string;
Var
  BTypeIdxA,
  BTypeIdxB     : Integer;
Begin
 Result:=ATypeA;
 repeat
 if ATypeB='' then break;
 if ParsIsTypeBasic(ATypeA)=FALSE then break;
 if ParsIsTypeBasic(ATypeB)=FALSE then break;
 BTypeIdxA:=Pos(ATypeA,CPromotedTypes); if BTypeIdxA=0 then break;
 BTypeIdxB:=Pos(ATypeB,CPromotedTypes); if BTypeIdxB=0 then break;
 if BTypeIdxB>BTypeIdxA then Result:=ATypeB;
 until TRUE;
End;

Function TProcAtom.ResolveElem ( Const ATarg : string ) : string;
Begin
 // Records and arrays will always have '[' for fields and elements
 // Globals will not always have '['
 if ParsIsGlobalOrExtern(ATarg) or (Pos('[',ATarg)<>0) then
  begin
  Result:=AppendTmpVar(ExtractFinalType(ATarg));
  AppendCmd2('mov_rm',Result,ATarg);
  end
 else Result:=ATarg;
End;

{Function TProcAtom.ResolveAddr ( Const ATarg : string ) : string;
Var
  BArray,
  BIndex        : string;
Begin
 if ParsIsQ(ATarg) and (Pos('[',ATarg)<>0) then
  begin
  Result:=AppendTmpVar('q'); // (ExtractType(ATarg));
  ParsSplitArrayIdx(ATarg,BArray,BIndex);
  AppendCmd('mov',Result,BArray);
  AppendCmd('add',Result,BIndex);
  end
 else Result:=ATarg;
End;

Function TProcAtom.AtomizeAddr ( Const ATarg : string ) : string;}
Function TProcAtom.ResolveAddr ( Const ATarg : string; Out AForceLoad : boolean ) : string;
Var
  BTypeE,
  BSpecE        : string;
  BArray,
  BIndex        : string;
  BTargP        : string;
Begin
 AForceLoad:=FALSE;
 Result:=CInvalidIdentifier;
 repeat
 BTypeE:=ParsExtractType(ATarg);
 BSpecE:=ParsExtractSpec(ATarg);
 if ParsIsQ(ATarg) then
  begin
  if Pos('[',ATarg)=0 then Result:=ATarg
  else
   begin
   Result:=AppendTmpVar('q'); // (ExtractType(ATarg));
   ParsSplitArrayIdx(ATarg,BArray,BIndex);
   AppendCmd2('mov',Result,BArray);
   AppendCmd2('add',Result,BIndex);
   end;
  break;
  end;
 if ParsIsSpecLocal(BSpecE) or ParsIsSpecParam(BSpecE) or ParsIsSpecTmp(BSpecE)
    or (ParsIsSpecResult(BSpecE) and ParsIsTypeBasic(ParsExtractType(ATarg))) then
  begin
  if ParsIsTypeBasic(BTypeE) or ParsIsTypePointer(BTypeE) then begin AppendCmd2('st_lea',ATarg,''); AForceLoad:=TRUE; end;
  ParsSplitArrayIdx(ATarg,BArray,BIndex);
  BTargP:=AppendPointer(BArray,'a');
  if BIndex='' then Result:=BTargP
  else
   begin
   Result:=AppendTmpVar('q');
   AppendCmd2('mov',Result,BTargP);
   AppendCmd2('add',Result,BIndex);
   end;
  break;
  end;
 if ParsIsSpecParamVar(BSpecE) or ParsIsSpecParamConst(BSpecE) or ParsIsConst(ATarg) or ParsIsGlobalOrExtern(ATarg)
    or (ParsIsSpecResult(BSpecE) and (ParsIsRecord(ATarg) or ParsIsStringX(ATarg))) then
  begin
  ParsSplitArrayIdx(ATarg,BArray,BIndex);
  BTargP:=AppendPointer(BArray,'m');
  if BIndex='' then Result:=BTargP
  else
   begin
   Result:=AppendTmpVar('q');
   AppendCmd2('mov',Result,BTargP);
   AppendCmd2('add',Result,BIndex);
   end;
  break;
  end;
 AppendError('e','Cannot extract effective address of a construction "'+ATarg+'" [R:TProcAtom.AtomizeAddr]');
 until TRUE;
End;

Function TProcAtom.ResolveAddr ( Const ATarg : string ) : string;
Var
  BForceLoad    : boolean;
Begin
 Result:=ResolveAddr(ATarg,BForceLoad);
End;

Function TProcAtom.AtomizeArray ( Const AArrayS : string; Const AWantedType : string ) : string;
Var
  BArrayS       : string;
  BTargB        : string;
  BTypeE,
  BTypeB        : string;
  BTypeR        : string;
  BConstValue   : string;
  BArrayDim,
  BTargCount    : Integer;
  BTargPrev     : string;
Begin
 Result:=CInvalidIdentifier;
 repeat
 if ParsIsTypeArray(AWantedType)=FALSE then begin AppendError('e','Target is not an array [R:TProcAtom.AtomizeArray]'); break; end;
 BTypeE:=ParsExtractTypeElemType(AWantedType);
 if ParsIsTypeBasic(BTypeE)=FALSE then begin AppendError('e','This array element type is not supported (must be of basic type) [R:TProcAtom.AtomizeArray]'); break; end;

 BTargCount:=0;
 BTargPrev:='';
 BConstValue:='';
 BTargB:='';
 BArrayS:=AArrayS;
 while BArrayS<>'' do
  begin
  BTargB:=ReadParamStr(BArrayS,',');
  if BTargB='' then break;
  if BTargB='...' then break;
  BTypeB:=ParsExtractType(BTargB);
  if TypesCompatible(BTypeE,BTypeB,BTypeR)=FALSE then AppendError('e','Type of an element (in particular "'+ParsExtractName(BTargB)+'") is not compatible with a target [R:TProcAtom.AtomizeArray]')
  else if BTypeR<>BTypeE then AppendError('e','Type of an element (in particular "'+ParsExtractName(BTargB)+'") is too big or not compatible with a target [R:TProcAtom.AtomizeArray]')
  else if ParsIsConst(BTargB)=FALSE then AppendError('e','Element "'+ParsExtractName(BTargB)+'") is not a constant [R:TProcAtom.AtomizeArray]')
  else if BTypeB<>BTypeR then ParsForceConstType(BTargB,BTypeE);
  if BConstValue<>'' then BConstValue:=BConstValue+',';
  BConstValue:=BConstValue+ParsExtractName(BTargB);
  inc(BTargCount);
  BTargPrev:=BTargB;
  end;

 BArrayDim:=ParsGetTypeArrayDim(AWantedType);
 if BTargB='...' then
  begin
  DelFirstSpace(BArrayS);
  if BArrayS<>'' then AppendError('e','Extra parameter in line "'+BArrayS+'" [R:TProcAtom.AtomizeArray]');
  if BTargCount>BArrayDim then AppendError('e','Source array is too big to fit destination [R:TProcAtom.AtomizeArray]');
  if BTargB='' then begin AppendError('e','Default fill element is not defined [R:TProcAtom.AtomizeArray]'); BTargB:=CInvalidIdentifier; end;
  while BTargCount<BArrayDim do
   begin
   if BConstValue<>'' then BConstValue:=BConstValue+',';
   BConstValue:=BConstValue+ParsExtractName(BTargPrev);
   inc(BTargCount);
   end;
  end;

 if BTargCount<>BArrayDim then AppendError('e','Number of elements in right and left parts must be the same [R:TProcAtom.AtomizeArray]');

 Result:=FModule.AppendConst(AWantedType,BConstValue);
 until TRUE;

 {BHasErrors:=FALSE;
 repeat
 BTarg:=AProc.AtomizeEval(BEval,'');
 if ParsIsConst(BTarg)=FALSE then begin AppendError('e','Expression in a right part is not a constant [R:TParsPas.ParseConstArray]'); BHasErrors:=TRUE; end;
 BTargList.Append(BTarg);
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.ParseConstArray]'); break; end;
 if BReadS=']' then break;
 if BReadS<>',' then begin AppendError('e','"]" or "," expected, \r found [R:TParsPas.ParseConstArray]'); break; end;
 until FALSE;
 if BHasErrors then break;
 // Precheck array and dimensions
 if ParsIsTypeArray(BTypeA)=FALSE then begin AppendError('e','Left part of an assignment is not an array [R:TParsPas.ParseConstArray]'); break; end;
 if ParsGetTypeArrayDim(BTypeA)<>BTargList.Count then begin AppendError('e','Array dimension of left and right side must be the same [R:TParsPas.ParseConstArray]'); break; end;
 // Check all types
 if ParsIsTypeBasic(BTypeE)=FALSE then begin AppendError('e','Only basic element types are allowed (not record/strings/arrays) [R:TParsPas.ParseConstArray]'); break; end;
 BListIdx:=0;
 while BListIdx<BTargList.Count do
  begin
  BTargB:=BTargList.Strings[BListIdx];
  if ParsIsConst(BTargB)=FALSE then begin AppendError('e','Element "'+ParsExtractName(BTargB)+'" is not a constant [R:TParsPas.ParseConstArray]'); break; end;
  BTypeB:=ParsExtractType(BTargB);
  if BTypeE<>BTypeB then
   begin
   if AProc.TypesCompatible(BTypeE,BTypeB,BTypeR)=FALSE then begin AppendError('e','Types of element in right side (in particular "'+ParsExtractName(BTargB)+'") is not compatible with the type of the array elements [R:TParsPas.ParseConstArray]'); break; end;
   if BTypeR<>BTypeE then begin AppendError('e','Types of element in right side (in particular "'+ParsExtractName(BTargB)+'") cannot be hold by the type of the array elements [R:TParsPas.ParseConstArray]'); break; end;
   ParsForceConstType(BTargB,BTypeE);
   BTargList.Strings[BListIdx]:=BTargB;
   end;
  inc(BListIdx);
  end;
 if BListIdx<>BTargList.Count then break;
 // Create const
 BConstValue:='';
 BListIdx:=0;
 while BListIdx<BTargList.Count do
  begin
  if BConstValue<>'' then BConstValue:=BConstValue+',';
  BConstValue:=BConstValue+ParsExtractName(BTargList.Strings[BListIdx]);
  inc(BListIdx);
  end;
 Result:=FModule.AppendConst(ParsExtractType(ATarg),BConstValue);
 until TRUE;
 BTargList.Free;}
End;

Function TProcAtom.AtomizeEval ( Const AEval : string; Const AWantedType : string ) : string;
Var
  BEval,
  BSubEval,
  BSubEvalA     : string;
  BPosA,
  BPos          : Integer;
  BTarg,
  BTargA        : string;
  BProcParams,
  BProcNameL    : string;
  BTmpVar       : string;
  BForceLoad    : boolean;
  BLeaList      : string;
  BLeaTarg      : string;
  BDummyArrayS  : string;
Begin
 BEval:=AEval;
 DelExtraBrackets(BEval);
 DelFirstSpace(BEval); DelLastSpace(BEval);

 BLeaList:='';
 BPos:=Pos('@',BEval);
 while BPos<>0 do
  begin
  Delete(BEval,BPos,1);
  BSubEval:='';
  ReadEvalAtom(BEval,BSubEval,BPos,' ');
  if BSubEval='' then BTarg:=CInvalidIdentifier
  else BTarg:=AtomizeEval(BSubEval,'');
  BTargA:=ResolveAddr(BTarg,BForceLoad); if BForceLoad then BLeaList:=BLeaList+BTarg+' ';
  Insert(BTargA,BEval,BPos);
  BPos:=Pos('@',BEval);
  end;

 repeat
 BPos:=Pos('not ',BEval);
 if BPos=0 then break;
 Delete(BEval,BPos,4);
 repeat
 BSubEval:=''; ReadEvalAtom(BEval,BSubEval,BPos,' ');
 if BSubEval<>#32 then break; // there can be extra spaces after parsing
 until FALSE;
 if BSubEval='' then BTarg:=CInvalidIdentifier
 else BTarg:=AtomizeEval(BSubEval,'');
 BTargA:=AppendTmpVar(ParsExtractType(BTarg));
 AppendCmd2('mov',BTargA,BTarg);
 AppendCmd2('not',BTargA,'');
 Insert(BTargA,BEval,BPos);
 until FALSE;

 repeat
 BPos:=Pos(' neg ',BEval);
 if BPos=0 then BPos:=Pos(',not ',BEval);
 if BPos=0 then break;
 Delete(BEval,BPos,4); // delete either " neg" or ",neg" but keeping the space
 repeat
 BSubEval:=''; ReadEvalAtom(BEval,BSubEval,BPos,' ');
 if BSubEval<>#32 then break; // there can be extra spaces after parsing
 until FALSE;
 if BSubEval='' then BTarg:=CInvalidIdentifier
 else BTarg:=AtomizeEval(BSubEval,'');
 BTargA:=AppendTmpVar(ParsExtractType(BTarg));
 AppendCmd2('mov',BTargA,BTarg);
 AppendCmd2('neg',BTargA,'');
 Insert(BTargA,BEval,BPos);
 until FALSE;

 repeat
 ExtractProc(BEval,BProcNameL,BProcParams,BPos);
 if BProcNameL='' then break;
 BTmpVar:=AtomizeProcCall(BProcNameL,BProcParams);
 Insert(BTmpVar,BEval,BPos);
 until FALSE;

 // Process first () brackets
 repeat
 BPosA:=Pos('(',BEval);
 if BPosA=0 then break;
 if ReadInside(BEval,BPosA,BSubEval)=FALSE then begin AppendError('() missmatch [R:TProcAtom.AtomizeEval]'); break; end;
 //BTarg:=AtomizeEval(BSubEval,AWantedType); // <- Perhaps invalid typecast required by C (see following line)
 BTarg:=AtomizeEval(BSubEval,'');
 Insert(BTarg,BEval,BPosA);
 until FALSE;

 // Process [] brackets (indexes)
 BPosA:=1;
 while BPosA<=Length(BEval) do
  begin
  if BEval[BPosA]='[' then
   begin
   if ReadInside(BEval,BPosA,BSubEval)=FALSE then begin AppendError('[] missmatch [R:TProcAtom.AtomizeEval]'); break; end;
   if Length(BSubEval)<3 then begin AppendError('Internal error: Invalid SubEval [R:TProcAtom.AtomizeEval]'); break; end;
   if BSubEval[1]='[' then Delete(BSubEval,1,1);
   if BSubEval[Length(BSubEval)]=']' then Delete(BSubEval,Length(BSubEval),1);
   BTarg:='';
   repeat
   BSubEvalA:=ReadParamStr(BSubEval,',');
   if BSubEvalA='' then break;
   BTargA:=AtomizeEval(BSubEvalA,'');
   if BTarg<>'' then BTarg:=BTarg+',';
   BTarg:=BTarg+BTargA;
   until FALSE;
   Insert('['+BTarg+']',BEval,BPosA);
   BPosA:=BPosA+1+Length(BTarg)+1;
   end
  else inc(BPosA);
  end;

 // Process <> brackets (arrays)
 BPosA:=1;
 while BPosA<=Length(BEval) do
  begin
  if BEval[BPosA]='<' then
   begin
   if ReadInside(BEval,BPosA,BSubEval)=FALSE then begin AppendError('{} missmatch [R:TProcAtom.AtomizeEval]'); break; end;
   if Length(BSubEval)<3 then begin AppendError('Internal error: Invalid SubEval [R:TProcAtom.AtomizeEval]'); break; end;
   if BSubEval[1]='<' then Delete(BSubEval,1,1);
   if BSubEval[Length(BSubEval)]='>' then Delete(BSubEval,Length(BSubEval),1);
   BDummyArrayS:='';
   repeat
   BSubEvalA:=ReadParamStr(BSubEval,',');
   if BSubEvalA='' then break;
   if BSubEvalA='...' then BTargA:=BSubEvalA
   else BTargA:=AtomizeEval(BSubEvalA,'');
   if BDummyArrayS<>'' then BDummyArrayS:=BDummyArrayS+',';
   BDummyArrayS:=BDummyArrayS+BTargA;
   until FALSE;
   BTarg:=AtomizeArray(BDummyArrayS,AWantedType);
   Insert(BTarg,BEval,BPosA);
   BPosA:=BPosA+Length(BTarg);
   end
  else inc(BPosA);
  end;

 repeat
 if FirstMultiPos(COpcodeList,BEval)=0 then
  begin
  BEval:=AtomizeTarg(BEval);
  break;
  end;
 {if Length(BEval)<3 then begin AppendError('Internal error: Eval length <3 [R:TProcAtom.AtomizeEval]'); break; end;
 BLevel:=0;
 BLen:=Length(BEval);
 BIndex:=0;
 while BIndex<BLen do
  begin
  if BEval[BIndex+1]='(' then Inc(BLevel)
  else if BEval[BIndex+1]=')' then Dec(BLevel);
  if BLevel<0 then break;
  inc(BIndex);
  end;
 if BLevel<>0 then begin AppendError('() missmatch [R:TProcAtom.AtomizeEval]'); break; end;}

 repeat
 ExtractEvalPair(BEval,BSubEval,' type ',BPos);
 if BSubEval='' then break;
 BTarg:=AtomizeEval2(BSubEval,AWantedType);
 insert(BTarg,BEval,BPos);
 until FALSE;

 repeat
 ExtractEvalPair(BEval,BSubEval,' mul div ',BPos);
 if BSubEval='' then break;
 BTarg:=AtomizeEval2(BSubEval,AWantedType);
 insert(BTarg,BEval,BPos);
 until FALSE;

 repeat
 ExtractEvalPair(BEval,BSubEval,COpcodeList,BPos);
 if BSubEval='' then break;
 BTarg:=AtomizeEval2(BSubEval,AWantedType);
 insert(BTarg,BEval,BPos);
 until FALSE;

 until TRUE;

 repeat
 BLeaTarg:=ReadParamStr(BLeaList);
 if BLeaTarg='' then break;
 AppendCmd2('ld_lea',BLeaTarg,'');
 until FALSE;

 Result:=BEval;
End;

Procedure TProcAtom.ProcessPointers;
Var
  BList         : TStringList;
  BPtrIdx,
  BTextIdx      : Integer;
  BTargXchg,
  BTargXchgA    : string;
  BLine         : string;
  BCmdA,
  BParamA1,
  BParamA2      : string;
  BCmdB,
  BParamB1,
  BParamB2      : string;
  BDummyS,
  BArrayName,
  BPointer,
  BPtrType      : string;
  BXchgS        : string;
  BPos          : Integer;
Begin
 BList:=TStringList.Create;
 BTargXchg:='';
 FTextDst.Clear;

 repeat
 if FPointers.Count=0 then break;

 for BPtrIdx:=0 to FPointers.Count-1 do
  begin
  BDummyS:=FPointers[BPtrIdx];
  BArrayName:=ReadParamStr(BDummyS,'=');
  BPointer:=ReadParamStr(BDummyS);
  BPtrType:=Copy(BPointer,1,1); Delete(BPointer,1,1);
  if BPtrType='m' then BList.Append('lam '+BPointer+' '+BArrayName+' ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''))
  else BList.Append('lea '+BPointer+' '+BArrayName+' ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
  end;

 // Pointers introduced by a compiler can already be present in Text.
 // In this case replace their names (with apparently more readable ones)
 for BTextIdx:=0 to FTextSrc.Count-1 do
  begin
  BLine:=FTextSrc.Strings[BTextIdx];
  SplitExec(BLine,BCmdA,BParamA1,BParamA2);
  BXchgS:='';
  if StrInList(BCmdA,'lam lea') then
   begin
   BPtrIdx:=0;
   while BPtrIdx<BList.Count do
    begin
    SplitExec(BList.Strings[BPtrIdx],BCmdB,BParamB1,BParamB2);
    if (BCmdB=BCmdA) and (BParamA2=BParamB2) then begin BXchgS:=BParamA1+' '+BParamB1; break; end;
    inc(BPtrIdx);
    end;
   end;
  if BXchgS='' then FTextDst.Append(BLine)
  else BTargXchg:=BTargXchg+BXchgS+' ';
  end;

 FTextSrc.Assign(FTextDst); FTextDst.Clear;
 for BTextIdx:=0 to FTextSrc.Count-1 do
  begin
  BLine:=FTextSrc.Strings[BTextIdx];
  BTargXchgA:=BTargXchg;
  repeat
  BParamA1:=ReadParamStr(BTargXchgA); BParamB1:=ReadParamStr(BTargXchgA);
  if BParamB1='' then break;
  repeat
  BPos:=Pos(BParamA1,BLine);
  if BPos=0 then break;
  Delete(BLine,BPos,Length(BParamA1));
  Insert(BParamB1,BLine,BPos);
  until FALSE;
  until FALSE;
  FTextDst.Append(BLine);
  end;

 FTextSrc.Clear;
 for BTextIdx:=0 to BList.Count-1 do FTextSrc.Append(BList.Strings[BTextIdx]);
 for BTextIdx:=0 to FTextDst.Count-1 do FTextSrc.Append(FTextDst.Strings[BTextIdx]);
 until TRUE;

 BList.Free;

 FPointers.Clear;
End;

end.

