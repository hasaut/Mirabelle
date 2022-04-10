unit ParsPrepr_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, AsmTypes_sd, ParsBase_sd, ParsHelper_sd;

Type
  TParsPrepr = class(TParsBase)
  private
    Function DefRdTextA ( Var AStr : string ) : string;
    Function DefTypesCompatible ( Const ATypeA, ATypeB : string; Out ATypeR : string ) : boolean;
    Function DefProcConst2 ( Const ATargA, ATargB : string; Const AOper : string; Const ATypeR : string ) : string;
    Function DefAtomizeEval2 ( Const AEval : string ) : string;
    Function DefAtomizeEval ( Const AEval : string ) : string;
    Function DefParseEval ( Var ASrc : string ) : string;
    Function DefProcessExpr ( Const ADefineExpr : string ) : string;

    Procedure PreprPushState ( AStateC : char );
    Function PreprPopState : char;
  protected
    Function DefPreprocMacro ( Const ADefineA, ADefineB : string ) : boolean;
    Function Preprocess ( Const AReadS : string; Out AOutValid : boolean ) : boolean;
    Function PreprIsTextEn : boolean;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

implementation

Uses
  ConComL;

Constructor TParsPrepr.Create;
Begin
 Inherited;
End;

Destructor TParsPrepr.Destroy;
Begin
 Inherited;
End;

Function TParsPrepr.DefRdTextA ( Var AStr : string ) : string;
Var
  BDataC        : char;
Begin
 Result:='';

 repeat
 if AStr='' then break;
 BDataC:=AStr[1];

 while BDataC=#32 do
  begin
  Delete(AStr,1,1);
  if AStr='' then BDataC:=#0 else BDataC:=AStr[1];
  if BDataC=#0 then break;
  end;

 if BDataC=#0 then break;

 Result:=BDataC; Delete(AStr,1,1);

 if AStr='' then break;
 BDataC:=AStr[1];
 if BDataC=#32 then break;

 if StrInList(Result+BDataC,FOneSymbol) then
  begin
  Result:=Result+BDataC; Delete(AStr,1,1);
  repeat
  if AStr='' then BDataC:=#0 else BDataC:=AStr[1];
  if BDataC=#0 then break;
  if BDataC=#32 then break;
  if StrInList(Result+BDataC,FOneSymbol)=FALSE then break;
  Result:=Result+BDataC; Delete(AStr,1,1);
  until FALSE;
  break;
  end;

 if Result[1]=FStrQuotes then
  begin
  Result:=Result+BDataC; Delete(AStr,1,1);
  if BDataC=FStrQuotes then break;
  repeat
  if AStr='' then BDataC:=#0 else BDataC:=AStr[1];
  if BDataC=#0 then break;
  Result:=Result+BDataC; Delete(AStr,1,1);
  if BDataC=FStrQuotes then break;
  until FALSE;
  break;
  end;

 if IsSymbol(Result[1]) then break;

 repeat
 if (BDataC='.') and (Result[1] in ['0'..'9']) and (Pos('0x',Result)=0) then
 else if IsSymbol(BDataC) then break;
 Result:=Result+BDataC; Delete(AStr,1,1);
 if AStr='' then BDataC:=#0 else BDataC:=AStr[1];
 if BDataC=#0 then break;
 until FALSE;

 until TRUE;
End;

Function TParsPrepr.DefTypesCompatible ( Const ATypeA, ATypeB : string; Out ATypeR : string ) : boolean;
Var
  BTypeAB       : string;
Begin
 Result:=FALSE;
 ATypeR:='_';
 repeat
 if ATypeA=ATypeB then begin ATypeR:=ATypeA; Result:=TRUE; break; end;
 BTypeAB:=ATypeA+ATypeB;
 if (BTypeAB='bw') or (BTypeAB='wb') then begin ATypeR:='w'; Result:=TRUE; break; end;
 if (BTypeAB='bd') or (BTypeAB='db') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='wd') or (BTypeAB='dw') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='bi') or (BTypeAB='ib') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='wi') or (BTypeAB='iw') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='di') or (BTypeAB='id') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='bf') or (BTypeAB='fb') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='wf') or (BTypeAB='fw') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='df') or (BTypeAB='fd') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='if') or (BTypeAB='fi') then begin ATypeR:='f'; Result:=TRUE; break; end;
 until TRUE;
End;

Function TParsPrepr.DefParseEval ( Var ASrc : string ) : string;
Var
  BReadS        : string;
  BOpcode       : string;
  BTarg         : string;
Begin
 Result:='';

 repeat
 // Variable/Function
 BReadS:=DefRdTextA(ASrc);
 if BReadS='(' then
  begin
  Result:=Result+'(';
  BTarg:=DefParseEval(ASrc); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+BTarg;
  if RdTextS<>')' then AppendError('e',') expected, \r found [R:TParsPrepr.ParseDefEval]');
  Result:=Result+')';
  end
 else
  begin
  BTarg:=IsTargConstant(BReadS); if BTarg='' then begin AppendError('e','Cannot obtain constant value of '+BReadS+' [R:ParsCpp.ParseDefEval]'); BTarg:=CInvalidIdentifier; end;
  Result:=Result+BTarg;
  end;
 BReadS:=DefRdTextA(ASrc);
 if (BReadS='') or (BReadS=';') or (BReadS=')') or (BReadS=']') or (BReadS=',') or (BReadS=':') or IsReservedWord(BReadS) then break;
 BOpcode:=StrToOpcode(BReadS);
 if BOpcode='' then begin AppendError('e','Operation code expected, \r found [R:ParsCpp.ParseDefEval]'); BReadS:=CInvalidOpcode; end;
 Result:=Result+' '+BOpcode+' ';
 until FALSE;
End;

Function TParsPrepr.DefProcConst2 ( Const ATargA, ATargB : string; Const AOper : string; Const ATypeR : string ) : string;
Var
  BNameA,
  BNameB,
  BNameR        : string;
  BValueA,
  BValueB,
  BValueR       : Int64;
Begin
 Result:=CInvalidIdentifier;
 repeat
 BNameA:=ParsExtractName(ATargA);
 BNameB:=ParsExtractName(ATargB);
 if (ATypeR='b') or (ATypeR='w') or (ATypeR='d') or (ATypeR='i') then
  begin
  if ATypeR='i' then
   begin
   if TryStrToInt64(BNameA,BValueA)=FALSE then AppendError('e','Error converting string '+BNameA+' to value [R:TParsPrepr.DefProcConst2]');
   if TryStrToInt64(BNameB,BValueB)=FALSE then AppendError('e','Error converting string '+BNameB+' to value [R:TParsPrepr.DefProcConst2]');
   end
  else
   begin
   if TryStrToInt0x(BNameA,BValueA)=FALSE then AppendError('e','Error converting string '+BNameA+' to value [R:TParsPrepr.DefProcConst2]');
   if TryStrToInt0x(BNameB,BValueB)=FALSE then AppendError('e','Error converting string '+BNameB+' to value [R:TParsPrepr.DefProcConst2]');
   end;
  BValueR:=0;
  case AOper of
    'add': BValueR:=BValueA+BValueB;
    'sub': BValueR:=BValueA-BValueB;
    'mul': BValueR:=BValueA*BValueB;
    'div': begin
           if BValueB=0 then begin BValueR:=0; AppendError('e','Zero division [R:TParsPrepr.DefProcConst2]'); end
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
    else AppendError('e','Operation '+AOper+' is not supported [R:TParsPrepr.DefProcConst2]');
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
 AppendError('e','Unsupported constant types [R:TParsPrepr.DefProcConst2]');
 until TRUE;
End;

Function TParsPrepr.DefAtomizeEval2 ( Const AEval : string ) : string;
Var
  BTargA,
  BTargB,
  BOper         : string;
  BEval         : string;
  BTypeA,
  BTypeB        : string;
  BTypeR        : string;
  BIsConstA,
  BIsConstB     : boolean;
Begin
 Result:='';
 BEval:=AEval;
 repeat
 BTargA:=ReadParamStr(BEval);
 BOper:=ReadParamStr(BEval);
 BTargB:=ReadParamStr(BEval);
 if ReadParamStr(BEval)<>'' then AppendError('e','Internal error or extra parameter in line [R:TParsPrepr.DefAtomizeEval2]');
 if BTargB='' then begin AppendError('e','Internal error or not enough parameters in line [R:TParsPrepr.DefAtomizeEval2]'); break; end;

 BTypeA:=ParsExtractType(BTargA);
 BTypeB:=ParsExtractType(BTargB);
 if StrInList(BOper,'shl shr') then
  begin
  if StrInList(BTypeB,'b w d i') then
  else AppendError('e','Only integer type allowed for shift [R:TParsPrepr.DefAtomizeEval2]');
  BTypeR:=BTypeA;
  end
 else
  begin
  if DefTypesCompatible(BTypeA,BTypeB,BTypeR)=FALSE then AppendError('e','Incompatible types [R:TParsPrepr.DefAtomizeEval2]');
  end;

 BIsConstA:=ParsIsConst(BTargA);
 BIsConstB:=ParsIsConst(BTargB);

 if (BIsConstA and BIsConstB) then
 else begin AppendError('e','Only constants are allowed in the preprocessor expressions [R:TParsPrepr.DefAtomizeEval2]'); break; end;

 Result:=DefProcConst2(BTargA,BTargB,BOper,BTypeR);
 until TRUE;
End;

Function TParsPrepr.DefAtomizeEval ( Const AEval : string ) : string;
Var
  BEval,
  BSubEval      : string;
  BLevel        : Integer;
  BIndex        : Integer;
  BPosA,
  BPosB         : Integer;
  BLen          : Integer;
  BPos          : Integer;
  BTarg         : string;
Begin
 BEval:=AEval;
 DelExtraBrackets(BEval);

 repeat
 if FirstMultiPos(COpcodeList,BEval)=0 then break;
 if Length(BEval)<3 then begin AppendError('e','Internal error: Eval length <3 [TParsPrepr.DefAtomizeEval]'); break; end;
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
 if BLevel<>0 then begin AppendError('e','() missmatch [R:TParsPrepr.DefAtomizeEval]'); break; end;

 repeat
 BPosA:=Pos('(',BEval);
 if BPosA=0 then break;
 BLevel:=1;
 for BPosB:=BPosA+1 to Length(BEval) do
  begin
  if BEval[BPosB]='(' then Inc(BLevel)
  else if BEval[BPosB]=')' then begin Dec(BLevel); if BLevel=0 then break; end;
  end;
 BSubEval:=Copy(BEval,BPosA,BPosB-BPosA+1);
 Delete(BEval,BPosA,BPosB-BPosA+1);
 BTarg:=defAtomizeEval(BSubEval);
 Insert(BTarg,BEval,BPosA);
 until FALSE;

 repeat
 ExtractEvalPair(BEval,BSubEval,' mul div ',BPos);
 if BSubEval='' then break;
 BTarg:=DefAtomizeEval2(BSubEval);
 Insert(BTarg,BEval,BPos);
 until FALSE;

 repeat
 ExtractEvalPair(BEval,BSubEval,COpcodeList,BPos);
 if BSubEval='' then break;
 BTarg:=DefAtomizeEval2(BSubEval);
 Insert(BTarg,BEval,BPos);
 until FALSE;

 until TRUE;

 Result:=BEval;
End;

Function TParsPrepr.DefProcessExpr ( Const ADefineExpr : string ) : string;
Var
  BSrc          : string;
  BEval,
  BTarg         : string;
Begin
 Result:='';
 BSrc:=ADefineExpr;
 repeat
 BEval:=DefParseEval(BSrc);
 if BEval='' then begin AppendError('e','Error preprocessing expression [R:TParsPrepr.ProcessDefineExpr]'); break; end;
 if BSrc<>'' then begin AppendError('e','Extra characters in preprocess expression: "'+BSrc+'" [R:TParsPrepr.ProcessDefineExpr]'); break; end;
 if FModule.GetErrorCountA<>0 then break;
 BTarg:=DefAtomizeEval(BEval);
 if FModule.GetErrorCountA<>0 then break;
 if BTarg='' then begin AppendError('e','Error preprocessing expression [R:TParsPrepr.ProcessDefineExpr]'); break; end;
 Result:=BTarg;
 until TRUE;
End;

Procedure TParsPrepr.PreprPushState ( AStateC : char );
Begin
 FPreprState:=AStateC+FPreprState;
End;

Function TParsPrepr.PreprPopState : char;
Begin
 if FPreprState='' then Result:=#0
 else begin Result:=FPreprState[1]; Delete(FPreprState,1,1); end;
End;

Function TParsPrepr.PreprIsTextEn : boolean;
Var
  BIndex        : Integer;
Begin
 Result:=FALSE;
 repeat
 if FPreprState='' then begin Result:=TRUE; break; end;
 BIndex:=0;
 while BIndex<Length(FPreprState) do
  begin
  if FPreprState[BIndex+1] in ['A'..'Z'] then
  else break;
  inc(BIndex);
  end;
 Result:=BIndex=Length(FPreprState);
 until TRUE;
End;

// Preprocess returns FALSE if parsing has to be stopped due to error
// AOutValid = TRUE, if current ReadS can be processed for parsing
Function TParsPrepr.Preprocess ( Const AReadS : string; Out AOutValid : boolean ) : boolean;
Var
  BReadS        : string;
  BPreprStateC  : char;
  BDefineA,
  BDefineB      : string;
  BExprS        : string;
  BTarg         : string;
  BNameS        : string;
  BExprBool     : boolean;
  BNameI        : Integer;
Begin
 Result:=FALSE;
 AOutValid:=FALSE;

 repeat
 if AReadS='#ifdef' then
  begin
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then
   begin
   AppendError('e','Constant expression is expected after #ifdef [R:TParsPrepr.Preprocess]');
   break;
   end;
  if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)<>dtNone then PreprPushState('I') else PreprPushState('i');
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS<>'' then AppendError('w','Extra definition in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 if AReadS='#ifndef' then
  begin
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then begin AppendError('e','Constant expression is expected after #ifndef [R:TParsPrepr.Preprocess]'); break; end;
  if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)=dtNone then PreprPushState('I') else PreprPushState('i');
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS<>'' then AppendError('w','Extra definition in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 if AReadS='#else' then
  begin
  if FPreprState='' then begin AppendError('e','Misplaced #else: there is no open preprocessor statement [R:TParsPrepr.Preprocess]'); break;  end;
  BPreprStateC:=PreprPopState;
  case BPreprStateC of
    'i',
    'f': PreprPushState('E');
    'I',
    'F': PreprPushState('e');
    'q': PreprPushState('e');
    else begin AppendError('e','Misplaced #else not allowed here [R:TParsPrepr.Preprocess]'); break; end;
  end; // case
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS<>'' then AppendError('w','Extra character in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 if AReadS='#endif' then
  begin
  if FPreprState='' then begin AppendError('e','Misplaced #endif: there is no open preprocessor statement [R:TParsPrepr.Preprocess]'); break;  end;
  PreprPopState;
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS<>'' then AppendError('w','Extra character in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 if AReadS='#if' then
  begin
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then begin AppendError('e','Constant expression is expected after #if [R:TParsPrepr.Preprocess]'); break; end;
  if BReadS='defined' then
   begin
   RdTextFC(FALSE); BReadS:=FParsOrig;
   if BReadS='' then begin AppendError('e','Constant expression is expected after #if [R:TParsPrepr.Preprocess]'); break; end;
   if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)<>dtNone then PreprPushState('I') else PreprPushState('i');
   RdTextFC(FALSE); BReadS:=FParsOrig;
   if BReadS<>'' then AppendError('w','Extra character in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
   //SkipLine;
   Result:=TRUE;
   break;
   end;
  BExprS:='';
  repeat
  if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)=dtNone then BDefineB:=BReadS;
  if BDefineB='' then begin AppendError('e','Statement "'+BReadS+'" has no associated value [R:TParsPrepr.Preprocess]'); break; end;
  BExprS:=BExprS+BDefineB;
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then break;
  until FALSE;
  if FModule.GetErrorCountA<>0 then break;
  BTarg:=DefProcessExpr(BExprS);
  if BTarg='' then break;
  BNameS:=ParsExtractName(BTarg);
  if BNameS='true' then BExprBool:=TRUE
  else if BNameS='false' then BExprBool:=FALSE
  else
   begin
   if TryStrToInt(BNameS,BNameI)=FALSE then begin AppendError('e','Cannot convert expression to integer or boolean [R:TParsPrepr.Preprocess]'); break; end;
   BExprBool:=BNameI<>0;
   end;
  if BExprBool then PreprPushState('I') else PreprPushState('i');
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 if AReadS='#elif' then
  begin
  if FPreprState='' then begin AppendError('e','Misplaced #elif [R:TParsPrepr.Preprocess]'); break; end;
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then begin AppendError('e','Constant expression is expected after #if [R:TParsPrepr.Preprocess]'); break; end;
  if BReadS='defined' then
   begin
   RdTextFC(FALSE); BReadS:=FParsOrig;
   if BReadS='' then begin AppendError('e','Constant expression is expected after #if [R:TParsPrepr.Preprocess]'); break; end;
   BPreprStateC:=PreprPopState;
   case BPreprStateC of
     'I',
     'F': PreprPushState('q');
     'f',
     'i': begin if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)<>dtNone then PreprPushState('F') else PreprPushState('f'); end;
     'q': PreprPushState('q');
     else begin AppendError('e','Misplaced #elif not allowed here [R:TParsPrepr.Preprocess]'); break; end;
   end; // case
   RdTextFC(FALSE); BReadS:=FParsOrig;
   if BReadS<>'' then AppendError('w','Extra definition in preprocessor statement '+BReadS+' [R:TParsPrepr.Preprocess]');
   //SkipLine;
   Result:=TRUE;
   break;
   end;
  BExprS:='';
  repeat
  if FModule.ResolveDefine(BReadS,BDefineA,BDefineB)=dtNone then BDefineB:=BReadS;
  if BDefineB='' then begin AppendError('e','Statement "'+BReadS+'" has no associated value [R:TParsPrepr.Preprocess]'); break; end;
  BExprS:=BExprS+BDefineB;
  RdTextFC(FALSE); BReadS:=FParsOrig;
  if BReadS='' then break;
  until FALSE;
  if FModule.GetErrorCountA<>0 then break;
  BTarg:=DefProcessExpr(BExprS);
  if BTarg='' then break;
  BNameS:=ParsExtractName(BTarg);
  if BNameS='true' then BExprBool:=TRUE
  else if BNameS='false' then BExprBool:=FALSE
  else
   begin
   if TryStrToInt(BNameS,BNameI)=FALSE then begin AppendError('e','Cannot convert expression to integer or boolean [R:TParsPrepr.Preprocess]'); break; end;
   BExprBool:=BNameI<>0;
   end;
  BPreprStateC:=PreprPopState;
  case BPreprStateC of
    'I',
    'F': PreprPushState('q');
    'f',
    'i': begin if BExprBool then PreprPushState('F') else PreprPushState('f'); end;
    'q': PreprPushState('q');
    else begin AppendError('e','Misplaced #elif not allowed here [R:TParsPrepr.Preprocess]'); break; end;
  end; // case
  //SkipLine;
  Result:=TRUE;
  break;
  end;

 Result:=TRUE;
 AOutValid:=PreprIsTextEn;
 until TRUE;
End;

Function TParsPrepr.DefPreprocMacro ( Const ADefineA, ADefineB : string ) : boolean;
Var
  BDefName      : string;
  BReadS        : string;
  BDefineA,
  BDefineB      : string;
  BArgListA     : string;
  BArgListB     : TStringList;
  BArgIdx       : Integer;
  BDst          : string;
  BResult       : boolean;
  BArgName,
  BArgSep,
  BArgParam     : string;
  BLevelA       : Integer;
Begin
 BArgListB:=TStringList.Create;

 BArgListA:='';
 Result:=FALSE;
 BDefineA:=ADefineA; BDefineB:=ADefineB;

 repeat
 BDefName:=ReadTillC(BDefineA,'(');
 if BDefName='' then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'Macro has no defined name (or internal error) [R:TParsPrepr.DefPreprocMacro]'); break; end;
 RdTextA(TRUE); BReadS:=FParsOrig;
 if BReadS='' then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'Unexpected end of file [R:TParsPrepr.DefPreprocMacro]'); break; end;
 if BReadS<>'(' then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'"(" expected, "'+BReadS+'" found [R:TParsPrepr.DefPreprocMacro]'); break; end;

 // Macro part A: read and collect parameters
 BResult:=FALSE;
 repeat
 BArgName:=DefRdTextA(BDefineA);
 if (BArgListB.Count=0) and (BArgName=')') then begin BResult:=TRUE; break; end;
 if IsAllowedName(BArgName)=FALSE then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,BArgName+' cannot be used as a macro parameter name [R:TParsPrepr.DefPreprocMacro]'); break; end;
 if StrInList(BArgName,BArgListA) then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'Duplicate macro parameter: '+BArgName+' [R:TParsPrepr.DefPreprocMacro]'); break; end;
 BArgListA:=BArgListA+BArgName+' ';
 BArgSep:=DefRdTextA(BDefineA);
 if (BArgSep=',') or (BArgSep=')') then
 else begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'"," or ")" expected in macro definition, "'+BArgName+'" found [R:TParsPrepr.DefPreprocMacro]'); break; end;
 BArgParam:=''; BLevelA:=0;
 repeat
 RdTextA(TRUE); BReadS:=FParsOrig;
 if BReadS='' then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'Unexpected end of file [R:TParsPrepr.DefPreprocMacro]'); break; end;
 if ((BReadS=')') or (BReadS=',')) and (BLevelA=0) then break;
 BArgParam:=BArgParam+BReadS;
 if BReadS='(' then Inc(BLevelA)
 else if BReadS=')' then Dec(BLevelA);
 until FALSE;
 if (BReadS=')') or (BReadS=',') then
 else begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'")" or "," expected [R:TParsPrepr.DefPreprocMacro]'); break; end;
 if BReadS<>BArgSep then begin AppendErrorA('e',FParsCursor.FLine,FParsCursor.FPos,'Missmatch in number of declared and actual macro parameters (hint: "'+BArgSep+'" expected) [R:TParsPrepr.DefPreprocMacro]'); break; end;
 BArgListB.Append(BArgParam);
 if BArgSep=')' then begin BResult:=TRUE; break; end;
 until FALSE;
 if BResult=FALSE then break;

 // Macro part B: push parameters
 BDst:='';
 while BDefineB<>'' do
  begin
  BReadS:=DefRdTextA(BDefineB);
  if BReadS='' then break;
  BArgIdx:=GetStrIndex(BReadS,BArgListA);
  if BArgIdx<0 then BDst:=BDst+BReadS
  else BDst:=BDst+BArgListB.Strings[BArgIdx];
  end;
 if BDst<>'' then PushDefine(BDst);
 Result:=TRUE;
 until TRUE;
 BArgListB.Free;
End;

end.

