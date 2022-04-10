unit ParsPy_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, AsmTypes_sd, ParsBase_sd, ParsPrepr_sd, ParsHelper_sd;

Const
  CPyResWords  : string =
   (
    ' def return'+
    ' if else'+
    ' while'+
    ' for in'+
    ' '
   );

Type
  TParsPy = class(TParsPrepr)
  private
    FRetLabel   : string;

    Function GetIndent : Integer;
    Procedure ParseTypeA ( Out ATypeS : string; Const AKnownNames : string; AReportError : boolean );
    Function ParseVar ( AVisi : char; Const AFilter : string; Const AKnownTypes : string ) : string;
    Function ParseType ( AVisi : char; Const AKnownNames : string ) : string;
    Procedure ParseConst ( AVisi : char; Const AKnownNames : string );
    Function ResolveDirectives ( Const AName : string ) : string;
    Function ResolveConst ( Const AName : string ) : string;
    Function ParseProcHead ( AVisi : char; Const AFilter : string; Const AKnownTypes : string; Out AProcName, AProcParams : string ) : string;
    Function ParseProcBody ( AProc : TLlvmProc; AIndent : Integer ) : boolean;
    Procedure ParseProcImpl ( AProc : TLlvmProc; AIndent : Integer );
    Function ParseArrayOpti ( AProc : TLlvmProc; Const ATarg : string; AIndent : Integer ) : string;
    Function ParseFieldOpti ( AProc : TLlvmProc; AIndent : Integer ) : string;
    Function ParseEval ( AProc : TLlvmProc; AIndent : Integer ) : string;
    Function ParseCallParams ( AProc : TLlvmProc; Const ATarg : string; AIndent : Integer ) : string;
    Function ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const ABreakLabel : string; AIndent : Integer ) : boolean;
  protected
    Function StrToOpcode ( Const AOpcodeS : string ) : string; Override;
    Function SensCase ( Const ADataS : string ) : string; Override;
    Function IsReservedWord ( Const AName : string ) : boolean; Override;
    Function IsTargConstant ( Const ANameS : string ) : string; Override;
    Procedure RdTextFC ( AJmpNextLine : boolean ); Override;
    Procedure RdTextFD ( AJmpNextLine : boolean ); Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Parse ( AModule : TLlvmModule; AIsInclude : boolean ); Override;
  end;

implementation

Uses
  ConComL;

Constructor TParsPy.Create;
Begin
 Inherited;
 FSymbolList:=#32+';.,:-+=/*()[]<>{}!|&~?^@#';
 FOneSymbol:=' == != <= >= << >> -> ';
 FStrQuotes:=#39;
End;

Destructor TParsPy.Destroy;
Begin
 Inherited;
End;

Function TParsPy.GetIndent : Integer;
Begin
 Result:=FTokenThis.Pos-Length(FTokenThis.Orig);
End;

Function TParsPy.SensCase ( Const ADataS : string ) : string;
Begin
 Result:=LowerCase(ADataS);
End;

Function TParsPy.StrToOpcode ( Const AOpcodeS : string ) : string;
Begin
 Result:='';
 if AOpcodeS='+' then Result:='add'
 else if AOpcodeS='-'   then Result:='sub'
 else if AOpcodeS='*'   then Result:='mul'
 else if AOpcodeS='/'   then Result:='div'
 else if AOpcodeS='div' then Result:='div'
 else if AOpcodeS='and' then Result:='and'
 else if AOpcodeS='or'  then Result:='or'
 else if AOpcodeS='xor' then Result:='xor'
 else if AOpcodeS='<<'  then Result:='shl'
 else if AOpcodeS='>>'  then Result:='shr'
 else if AOpcodeS='=='  then Result:='cmpe'
 else if AOpcodeS='!='  then Result:='cmpne'
 else if AOpcodeS='<'   then Result:='cmpb'
 else if AOpcodeS='>'   then Result:='cmpa'
 else if AOpcodeS='<='  then Result:='cmpbe'
 else if AOpcodeS='>='  then Result:='cmpae';
End;

Function TParsPy.IsReservedWord ( Const AName : string ) : boolean;
Begin
 Result:=StrInList(SensCase(AName),CPyResWords);
End;

Function TParsPy.IsTargConstant ( Const ANameS : string ) : string;
Var
  BNameS        : string;
  BDataC        : Cardinal;
  BDataF        : Extended;
  BLen          : Integer;

Begin
 Result:='';

 BNameS:=SensCase(ANameS);

 repeat
 if BNameS='true' then begin Result:=CBooleanTrue; break; end;
 if BNameS='false' then begin Result:=CBooleanFalse; break; end;

 BLen:=Length(BNameS);
 if (BLen>=2) and (BNameS[1]=#39) and (BNameS[BLen]=#39) then
  begin
  BNameS:=ANameS;
  Delete(BNameS,BLen,1); Delete(BNameS,1,1);
  if Length(BNameS)=1 then Result:=CTagS+'c_c'+CTagM+IntToStr(Ord(BNameS[1]))+CTagE
  else Result:=FModule.AppendConst('sp',BNameS);
  break;
  end;

 if Pos('0x',BNameS)=1 then
  begin
  Delete(BNameS,1,2); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('0'))
   else if BNameS[1] in ['a'..'f'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('a')+10)
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if Pos('$',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('0'))
   else if BNameS[1] in ['a'..'f'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('a')+10)
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if Pos('%',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'1'] then BDataC:=(BDataC shl 1) or Cardinal(ord(BNameS[1])-ord('0'))
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if (Pos('.',BNameS)<>0) and (Pos('..',BNameS)=0) then
  begin
  if TryStrToFloat(BNameS,BDataF)=FALSE then break;
  Result:=CTagS+'c_f'+CTagM+BNameS+CTagE;
  break;
  end;

 BDataC:=0;
 while BNameS<>'' do
  begin
  if BDataC>=($FFFFFFFF-10) then break;
  if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC*10) + Cardinal(ord(BNameS[1])-ord('0'))
  else break;
  Delete(BNameS,1,1);
  end;
 if BNameS<>'' then break;
 Result:=ParsFormatConst(BDataC);
 until TRUE;
End;

Procedure TParsPy.RdTextFC ( AJmpNextLine : boolean );
Var
  BLastComment  : char;
  BReady        : boolean;
Begin
 repeat
 while FCommentLevel<>'' do
  begin
  BLastComment:=FCommentLevel[Length(FCommentLevel)];
  RdTextA(TRUE);
  if FParsOrig='' then break;
  if (FParsOrig='"""') and (BLastComment='*') then Delete(FCommentLevel,Length(FCommentLevel),1);
  end;

 RdTextA(AJmpNextLine);
 if FParsOrig='' then break;

 BReady:=FALSE;
 repeat
 if FParsOrig='"""' then begin FCommentLevel:=FCommentLevel+'*'; break; end;

 if FParsOrig='#' then
  begin
  SkipLine;
  if AJmpNextLine=FALSE then begin FParsOrig:=''; BReady:=TRUE; end;
  break;
  end;
 until TRUE;

 if BReady then break;

 until FALSE;
End;

Procedure TParsPy.RdTextFD ( AJmpNextLine : boolean );
Var
  BDefineA,
  BDefineB      : string;
  BOutValid     : boolean;
  BReady        : boolean;
  BDefType      : TDefType;
Begin
 repeat
 RdTextFC(AJmpNextLine);
 if FParsOrig='' then break;

 BReady:=FALSE;
 repeat
 BDefType:=FModule.ResolveDefine(FParsOrig,BDefineA,BDefineB);
 if BDefType=dtNone then
  begin
  ResetDefineCmp;
  BReady:=(Preprocess(FParsOrig,BOutValid)=FALSE) or BOutValid;
  break;
  end;
 if BDefType=dtConst then
  begin
  if PushDefine(FParsOrig,BDefineB)=FALSE then begin FModule.AppendError('e',FParsCursor.FLine,FParsCursor.FPos,'Define loop: '+FParsOrig+' [R:TParsPy.RdTextSL]'); BReady:=TRUE; end;
  break;
  end;
 // Macro
 if DefPreprocMacro(BDefineA,BDefineB)=FALSE then BReady:=TRUE;
 until TRUE;

 if BReady then break;

 until FALSE;
End;

Procedure TParsPy.ParseTypeA ( Out ATypeS : string; Const AKnownNames : string; AReportError : boolean );
Var
  BReadS        : string;
  BArrayS,
  BArrayE       : Integer;
  BTypeS        : string;
  BTarg         : string;
  BVisibleNames : string;
Begin
 ATypeS:='_';
 BReadS:=RdTextS;
 repeat
 if SensCase(BReadS)='bool' then begin ATypeS:='l'; break; end;
 if SensCase(BReadS)='char' then begin ATypeS:='c'; break; end;
 if SensCase(BReadS)='byte' then begin ATypeS:='b'; break; end;
 if SensCase(BReadS)='word' then begin ATypeS:='w'; break; end;
 if SensCase(BReadS)='cardinal' then begin ATypeS:='d'; break; end;
 if SensCase(BReadS)='shortint' then begin ATypeS:='k'; break; end;
 if SensCase(BReadS)='smallint' then begin ATypeS:='m'; break; end;
 if SensCase(BReadS)='int' then begin ATypeS:='i'; break; end;
 if SensCase(BReadS)='float' then begin ATypeS:='f'; break; end;
 if SensCase(BReadS)='array' then
  begin
  if RdTextS<>'[' then AppendError('e','[ expected, \r found [R:TParsPy.ParseTypeA]');
  ATypeS:='';
  repeat
  BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayS)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPy.ParseTypeA]');
  if RdTextS<>'.' then AppendError('e','. expected, \r found [R:TParsPy.ParseTypeA]');
  if RdTextS<>'.' then AppendError('e','. expected, \r found [R:TParsPy.ParseTypeA]');
  BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayE)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPy.ParseTypeA]');
  ATypeS:=ATypeS+'a'+IntToStr(BArrayS)+'s'+IntToStr(BArrayE)+'e';
  BReadS:=RdTextS;
  if BReadS=']' then break;
  if BReadS<>',' then begin AppendError('e','"," or "]" expected, \r found [R:TParsPy.ParseTypeA]'); break; end;
  until FALSE;
  if ATypeS='' then ATypeS:='_';
  if RdTextS<>'of' then AppendError('e','"of" expected, \r found [R:TParsPy.ParseTypeA]');
  ParseTypeA(BTypeS,AKnownNames,TRUE);
  ATypeS:=ATypeS+BTypeS;
  break;
  end;
 if SensCase(BReadS)='str' then
  begin
  if LuTextS='[' then
   begin
   RdTextS;
   BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayE)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPy.ParseTypeA]');
   if RdTextS<>']' then AppendError('e','] expected, \r found [R:TParsPy.ParseTypeA]');
   if BArrayE>255 then AppendError('e','String dimension is too big (max 255 allowed) [R:TParsPy.ParseTypeA]');
   if BArrayE<=0 then AppendError('e','String dimension is too small [R:TParsPy.ParseTypeA]');
   end
  else
   begin
   BArrayE:=255;
   end;
  ATypeS:='sp'+IntToStr(BArrayE)+'e';
  break;
  end;
 if SensCase(BReadS)='^' then
  begin
  ATypeS:='p';
  BReadS:=LuTextS;
  ParseTypeA(BTypeS,AKnownNames,FALSE);
  if BTypeS='_' then ATypeS:=ATypeS+'"'+BReadS+'"'
  else ATypeS:=ATypeS+BTypeS;
  break;
  end;

 BTarg:='';
 BVisibleNames:=AKnownNames;
 repeat
 BTarg:=ReadParamStr(BVisibleNames);
 if BTarg='' then break;
 if ParsIsType(BTarg) and (SensCase(ParsExtractName(BTarg))=SensCase(BReadS)) then break;
 until FALSE;
 if BTarg<>'' then
  begin
  ATypeS:=ParsExtractType(BTarg);
  break;
  end;
 if AReportError then AppendError('e','Invalid type \r [R:TParsPy.ParseTypeA]');

 until TRUE;
End;

Function TParsPy.ParseVar ( AVisi : char; Const AFilter : string; Const AKnownTypes : string ) : string;
Var
  BFilterS      : string;
  BReadS        : string;
  BTypeS        : string;
  BVarList      : string;

Begin
 Result:='';
 BFilterS:=LowerCase(AFilter);
 BVarList:='';

 repeat
 BReadS:=RdTextS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a variable name [R:TParsPy.ParseVar]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a variable name [R:TParsPy.ParseVar]');
 if Pos(' '+BReadS+' ',BVarList)<>0 then AppendError('e','Identifier \r has just been declared [R:TParsPy.ParseVar]');
 if Pos(CTagM+BReadS+CTagE+' ',BFilterS)<>0 then AppendError('e','Identifier \r is already declared [R:TParsPy.ParseVar]');
 BVarList:=BVarList+' '+BReadS+' ';
 BReadS:=RdTextS;
 if BReadS=':' then begin ParseTypeA(BTypeS,AKnownTypes,TRUE); break; end;
 if BReadS<>',' then begin AppendError('e',': or , expected, \r found [R:TParsPy.ParseVar]'); SkipText(';'); break; end;
 until FALSE;

 while BVarList<>'' do
  begin
  BReadS:=ReadParamStr(BVarList);
  if BReadS='' then break;
  Result:=Result+CTagS+'d'+AVisi+BTypeS+CTagM+BReadS+CTagE+' ';
  end;
End;

Function TParsPy.ParseType ( AVisi : char; Const AKnownNames : string ) : string;
Var
  BKnownTypesS  : string;
  BReadS        : string;
  BTypeS        : string;
  BTypeName     : string;
  BTypeThis     : string;
  BFieldList    : string;
  BFieldListA,
  BField        : string;
  BRecSize      : Integer;

Begin
 Result:='';
 BKnownTypesS:=LowerCase(AKnownNames);

 repeat
 BReadS:=RdTextS;
 BTypeName:=BReadS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a type name [R:TParsPy.ParseType]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a type name [R:TParsPy.ParseType]');
 if Pos(' '+BReadS+' ',Result)<>0 then AppendError('e','Type \r has just been declared [R:TParsPy.ParseType]');
 if Pos(CTagM+BReadS+CTagE+' ',BKnownTypesS)<>0 then AppendError('e','Type \r is already declared [R:TParsPy.ParseType]');
 if RdTextS<>'=' then begin AppendError('e','= expected, \r found [R:TParsPy.ParseType]'); SkipText(';'); break; end;
 if LuTextS='record' then
  begin
  RdTextS;
  BFieldList:='';
  repeat
  if LuTextS='end' then break;
  BFieldListA:=ParseVar('f',BFieldList,AKnownNames+Result);
  while BFieldListA<>'' do
   begin
   BField:=ReadParamStr(BFieldListA);
   if BField='' then break;
   BFieldList:=BFieldList+BField+' ';
   end;
  if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPy.ParseType]'); SkipText(';'); break; end;
  until FALSE;
  RdTextS;
  FModule.OrderRecordFields(BFieldList,BFieldListA,BRecSize);
  BTypeThis:=CTagS+'t'+AVisi+'r'+IntToStr(BRecSize)+'e{'+BFieldListA+'}'+CTagM+BTypeName+CTagE;
  end
 else
  begin
  ParseTypeA(BTypeS,AKnownNames+BFieldList+Result,TRUE);
  BTypeThis:=CTagS+'t'+AVisi+BTypeS+CTagM+BTypeName+CTagE;
  end;
 Result:=Result+BTypeThis+' ';
 if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPy.ParseType]'); SkipText(';'); break; end;
 if IsReservedWord(LuTextS) then break;
 until FALSE;
End;

Function TParsPy.ResolveDirectives ( Const AName : string ) : string;
Begin
 Result:='';
 repeat
 if LowerCase(AName)='inc' then begin Result:=CDirectiveInc; break; end;
 if LowerCase(AName)='dec' then begin Result:=CDirectiveDec; break; end;
 if LowerCase(AName)='chr' then begin Result:=CDirectiveChr; break; end;
 if LowerCase(AName)='ord' then begin Result:=CDirectiveOrd; break; end;
 until TRUE;
End;

Function TParsPy.ResolveConst ( Const AName : string ) : string;
Begin
 Result:='';
 repeat
 if LowerCase(AName)='nil' then begin Result:=CPointerNil; break; end;
 until TRUE;
End;

Procedure TParsPy.ParseConst ( AVisi : char; Const AKnownNames : string );
Var
  BKnownNamesS  : string;
  BReadS        : string;
  BConstName,
  BConstData    : string;
  BConstNameA   : string;
Begin
 BKnownNamesS:=LowerCase(AKnownNames);

 repeat
 BReadS:=RdTextS; BConstName:=BReadS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a Const name [R:TParsPy.ParseConst]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a Const name [R:TParsPy.ParseConst]');
 BConstNameA:=FModule.ResolveAlias(BConstName,TRUE);
 if BConstNameA<>BConstName then AppendError('e','Identifier \r is already declared [R:TParsPy.ParseConst]');
 if Pos(CTagM+BReadS+CTagE+' ',BKnownNamesS)<>0 then AppendError('e','Identifier \r is already declared [R:TParsPy.ParseConst]');
 if RdTextS<>'=' then begin AppendError('e','= expected, \r found [R:TParsPy.ParseConst]'); SkipText(';'); break; end;
 BReadS:=RdTextS; BConstData:=BReadS;
 repeat
 BReadS:=LuTextS;
 if BReadS=';' then break;
 if BReadS='' then break;
 BConstData:=BConstData+RdTextS;
 until FALSE;
 if AVisi='f' then FModule.PrivateAliases.Values[BConstName]:=BConstData
 else FModule.PublicAliases.Values[BConstName]:=BConstData;
 if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPy.ParseConst]'); SkipText(';'); break; end;
 if IsReservedWord(LuTextS) then break;
 until FALSE;
End;

Function TParsPy.ParseProcHead ( AVisi : char; Const AFilter : string; Const AKnownTypes : string; Out AProcName, AProcParams : string ) : string;
Var
  BFilterS      : string;
  BReadS        : string;
  BTypeS        : string;
  BProcName,
  BSearchName   : string;
  BVarList      : string;
  BVarVisi      : char;
Begin
 Result:='';
 BFilterS:=LowerCase(AFilter);
 BVarList:=''; BTypeS:='_';

 BReadS:=RdTextS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a procedure/function name [R:TParsPy.ParseProcHead]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a procedure/function name [R:TParsPy.ParseProcHead]');
 if Pos(CTagS+BReadS+CTagE+' ',BFilterS)<>0 then AppendError('e','Identifier \r is already declared [R:TParsPy.ParseProcHead]');
 BProcName:=BReadS;
 BReadS:=LuTextS;
 if BReadS='(' then
  begin
  RdTextS;
  BReadS:=LuTextS;
  if BReadS=')' then RdTextS
  else
   repeat
   BVarVisi:='a';
   BVarList:=BVarList+ParseVar(BVarVisi,BVarList,AKnownTypes);
   BReadS:=RdTextS;
   if BReadS=')' then break;
   if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.ParseProcHead]'); break; end;
   if BReadS<>',' then begin AppendError('e',', or ) expected, \r found [R:TParsPy.ParseProcHead]'); break; end;
   until FALSE;
  BReadS:=LuTextS;
  end;
 if BReadS='' then AppendError('e','Unexpected end of file [R:TParsPy.ParseProcHead]')
 else if BReadS='->' then
  begin
  RdTextS;
  ParseTypeA(BTypeS,AKnownTypes,TRUE);
  BReadS:=LuTextS;
  end
 else if BReadS=':' then
 else AppendError('e','Unexpected character \l found [R:TParsPy.ParseProcHead]');

 if BReadS<>':' then AppendError('e','":" expected, \l found [R:TParsPy.ParseProcHead]');
 RdTextS;

 AProcParams:=BVarList;

 BSearchName:=CTagM+BProcName+CTagP;
 while BVarList<>'' do
  begin
  BReadS:=ReadParamStr(BVarList);
  if BReadS='' then break;
  BSearchName:=BSearchName+ParsExtractSpec(BReadS)+CTagP;
  end;

// if Pos(BSearchName,LowerCase(FModule.PublicNames+FModule.PrivateNames))<>0 then AppendError('e','Identifier '+BProcName+' is already declared');
// if Pos(BSearchName,LowerCase(FModule.VisibleNames))<>0 then AppendError('e','Identifier '+BProcName+' is declared in one of included modules');

 AProcName:=BProcName;
 Result:=CTagS+'m'+AVisi+BTypeS+BSearchName+CTagE;
End;

Function TParsPy.ParseProcBody ( AProc : TLlvmProc; AIndent : Integer ) : boolean;
Begin
 Result:=FALSE;

 AProc.StartLine:=FRdQueue[0].Line; AProc.StartPos:=FRdQueue[0].Pos;

 repeat
 FRetLabel:=AProc.LabelName('Ret',0,0);
 if ParseBlock(AProc,0,'',AIndent)=FALSE then break;
 AProc.FlowList.Append(FRetLabel+':'+GenTail);
 AProc.EndLine:=FRdQueue[0].Line; AProc.EndPos:=FRdQueue[0].Pos;
 Result:=TRUE;
 until TRUE;
End;

Procedure TParsPy.ParseProcImpl ( AProc : TLlvmProc; AIndent : Integer );
Var
  BType         : string;
  BIndent       : Integer;
Begin
 repeat
 BType:=ParsExtractType(AProc.NameL);
 if (BType<>'') and (BType<>'_') then AProc.RetListS:=AProc.RetListS+CTagS+'dr'+BType+CTagM+'Result'+CTagE+' ';
 if LuTextS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.ParseProcImpl]'); break; end;
 BIndent:=GetIndent;
 if BIndent<=AIndent then begin AppendError('e','Invalid indent [R:TParsPy.ParseProcImpl]'); break; end;
 if ParseProcBody(AProc,BIndent)=FALSE then break;

 AProc.IsImplemented:=TRUE;
 until TRUE;
End;

Function TParsPy.ParseArrayOpti ( AProc : TLlvmProc; Const ATarg : string; AIndent : Integer ) : string;
Var
  BReadS        : string;
  BTarg         : string;
Begin
 Result:='';
 if ParsIsArray(ATarg) or ParsIsStringP(ATarg) then
 else AppendError('e','\r is not an array and cannot be indexed [R:TParsPy.ParseArrayOpti]');
 RdTextS;
 Result:=Result+'[';
 repeat
 BTarg:=ParseEval(AProc, AIndent); if BTarg='' then BTarg:=CInvalidIdentifier;
 Result:=Result+BTarg;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.ParseArrayOpti]'); break; end;
 if BReadS=']' then break;
 if BReadS<>',' then begin AppendError('e','"]" or "," expected, \r found [R:TParsPy.ParseArrayOpti]'); break; end;
 Result:=Result+',';
 until FALSE;
 Result:=Result+']';
End;

Function TParsPy.ParseFieldOpti ( AProc : TLlvmProc; AIndent : Integer ) : string;
Var
  BReadS        : string;
  BTarg         : string;
  BField        : string;
Begin
 Result:='';
 RdTextS;
 BReadS:=RdTextS;
 BField:=BReadS;
 Result:=Result+'.'+BField;
 repeat
 BReadS:=LuTextS;
 if BReadS='^' then
  begin
  RdTextS;
  Result:=Result+'['+CConstZero+']';
  BReadS:=LuTextS;
  end;
 if BReadS='.' then
  begin
  RdTextS;
  BField:=RdTextS;
  Result:=Result+'.'+BField;
  end
 else if BReadS='[' then
  begin
  RdTextS;
  BTarg:=ParseEval(AProc,AIndent); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+'['+BTarg;
  if RdTextS<>']' then AppendError('e','] expected, \r found');
  Result:=Result+']';
  end
 else
  begin
  break;
  end;
 until FALSE;
End;

Function TParsPy.ParseEval ( AProc : TLlvmProc; AIndent : Integer ) : string;
Var
  BReadS        : string;
  BOpcode       : string;
  BTarg,
  BEval         : string;
  BIsNot        : boolean;
  BLineThis     : Integer;
Begin
 Result:='';

 repeat
 // Variable/Function
 BReadS:=LuTextS;
 if BReadS='@' then
  begin
  RdTextS;
  Result:=Result+'@';
  BTarg:=ParseEval(AProc,AIndent); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+BTarg;
  end
 else if BReadS='(' then
  begin
  RdTextS;
  Result:=Result+'(';
  BTarg:=ParseEval(AProc,AIndent); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+BTarg;
  if RdTextS<>')' then AppendError('e',') expected, \r found [R:TParsPy.ParseEval]');
  Result:=Result+')';
  end
 else if BReadS='-' then
  begin
  Result:=Result+CConstZero;
  end
 else
  begin
  BIsNot:=FALSE;
  repeat
  if LowerCase(BReadS)<>'not' then break;
  BIsNot:=not BIsNot;
  RdTextS;
  BReadS:=LuTextS;
  until FALSE;
  RdTextS;
  if BIsNot then Result:=Result+' not ';
  BTarg:='';
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \r cannot be used as an identifier [R:TParsPy.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if IsSymbol(BReadS[1]) then begin AppendError('e','Misplaced symbol \r [R:TParsPy.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if BTarg='' then BTarg:=ResolveDirectives(BReadS);
  if BTarg='' then BTarg:=ResolveConst(BReadS);
  if BTarg='' then BTarg:=GetTarg(AProc,BReadS);
  if BTarg='' then begin AppendError('e','Identifier \r not found [R:TParsPy.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if ParsIsProc(BTarg) then begin BEval:=ParseCallParams(AProc,BTarg,AIndent); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')'; end;
  Result:=Result+BTarg;
  end;
 // Operation or '[' or end
 BLineThis:=FTokenThis.Line;
 BReadS:=LuTextS;
 if BReadS='^' then
  begin
  RdTextS;
  Result:=Result+'['+CConstZero+']';
  BReadS:=LuTextS;
  end;
 if BReadS='[' then
  begin
  Result:=Result+ParseArrayOpti(AProc,BTarg,AIndent);
  BReadS:=LuTextS;
  end;
 if BReadS='.' then
  begin
  Result:=Result+ParseFieldOpti(AProc,AIndent);
  BReadS:=LuTextS;
  end;
 if (FTokenThis.Pos<AIndent) or (FTokenThis.Line<>BLineThis) or (BReadS=';') or (BReadS=')') or (BReadS=']') or (BReadS=',') or (BReadS='=') or (BReadS=':') or IsReservedWord(BReadS) then break;
 RdTextS;
 BOpcode:=StrToOpcode(BReadS);
 if BOpcode='' then begin AppendError('e','Operation code expected, \r found [R:TParsPy.ParseEval]'); BReadS:=CInvalidOpcode; end;
 Result:=Result+' '+BOpcode+' ';
 until FALSE;
End;

Function TParsPy.ParseCallParams ( AProc : TLlvmProc; Const ATarg : string; AIndent : Integer ) : string;
Var
  BReadS,
  BEval         : string;
  BParams       : string;
  BTypeS        : string;
Begin
 Result:='';
 BParams:=ParsSplitParamTypes(ATarg); DelFirstSpace(BParams); DelLastSpace(BParams);
 repeat
 BReadS:=LuTextS;
 if BReadS<>'(' then
  begin
  if BParams<>'' then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsPy.ParseCallParams]');
  break;
  end;
 RdTextS;
 BReadS:=LuTextS;

  repeat
  DelFirstSpace(BParams);
  BTypeS:=ReadParamStr(BParams);
  if BReadS=')' then BEval:='' else BEval:=ParseEval(AProc,AIndent);
  if (BEval='') and (BTypeS='') then break;
  if (BEval='') and (BTypeS<>'') then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsPy.ParseCallParams]');
  if (BEval<>'') and (BTypeS='') then AppendError('e','Extra parameter in call of procedure/function [R:TParsPy.ParseCallParams]');
  if (BEval<>'') and (BTypeS<>'') then Result:=Result+BEval+CTagP;
  if BEval<>'' then
   begin
   BReadS:=LuTextS;
   if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.ParseCallParams]'); break; end;
   if BReadS=',' then RdTextS
   else if BReadS<>')' then begin RdTextS; AppendError('e',', or ) expected, \r found [R:TParsPy.ParseCallParams]'); end;
   end;
  until FALSE;

 BReadS:=LuTextS;
 if BReadS<>')' then AppendError('e',') expected, \r found [R:TParsPy.ParseCallParams]')
 else RdTextS;
 until TRUE;
End;

Function TParsPy.ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const ABreakLabel : string; AIndent : Integer ) : boolean;
Var
  BReadS        : string;
  BTarg,
  BEval         : string;
  BResult       : boolean;
  BLabelIndex   : Integer;
  BLCmp,
  BLEnd,
  BLThen,
  BLElse        : string;
  BIndent       : Integer;
  BItemListS    : string;

Begin
 Result:=FALSE;
 BLabelIndex:=0;

 repeat
 BReadS:=LuTextS;
 BIndent:=GetIndent;
 if BReadS='' then begin Result:=TRUE; break; end;
 if BIndent<AIndent then begin Result:=TRUE; break; end;
 if BIndent>AIndent then begin AppendError('e','Invalid indent [R:TParsPy.ParseBlock]'); break; end;

 BResult:=FALSE;

 repeat // Error block

 if SensCase(BReadS)='break' then
  begin
  RdTextS;
  if ABreakLabel='' then begin AppendError('e','Misplaced break [R:TParsPy.ParseBlock]'); break; end;
  AProc.FlowList.Append('jmp '+ABreakLabel+GenTail);
  Result:=TRUE;
  break;
  end;

 if SensCase(BReadS)='return' then
  begin
  RdTextS;
  BReadS:=LuTextS;
  BIndent:=GetIndent;
  if BIndent<AIndent then
   begin
   if AProc.RetListS<>'' then AppendError('e','Return value must be specified [R:TParsPy.ParseBlock]');
   end
  else
   begin
   if AProc.RetListS='' then AppendError('e','This function cannot return any value [R:TParsPy.ParseBlock]');
   BEval:=ParseEval(AProc,AIndent);
   if BEval='' then begin AppendError('e','Error parsing eval [R:TParsPy.ParseBlock]'); BEval:=CInvalidIdentifier; end;
   BTarg:=AProc.RetListS; DelLastSpace(BTarg);
   if BTarg<>'' then AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);
   end;
  AProc.FlowList.Append('jmp '+FRetLabel+GenTail);
  BResult:=TRUE;
  break;
  end;

 // ** IF **
 if SensCase(BReadS)='if' then
  begin
  RdTextS;
  BLThen:=AProc.LabelName('IfThen',ALevel,BLabelIndex);
  BLElse:=AProc.LabelName('IfElse',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('IfEnd',ALevel,BLabelIndex);
  BEval:=ParseEval(AProc,AIndent); if BEval='' then begin AppendError('e','Error parsing if condition [R:TParsPy.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLElse+GenTail);
  if LuTextS<>':' then AppendError('e','":" expected, \l found [R:TParsPy.ParseBlock]')
  else RdTextS;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  LuTextS; BIndent:=GetIndent;
  if BIndent<=AIndent then // "then" with no body
  else
   begin
   if ParseBlock(AProc,ALevel+1,ABreakLabel,BIndent)=FALSE then break;
   end;
  AProc.FlowList.Append('jmp '+BLEnd+GenTail);
  BReadS:=LuTextS; BIndent:=GetIndent;
  AProc.FlowList.Append(BLElse+':'+GenTail);
  if BIndent<AIndent then
  else if BIndent>AIndent then begin AppendError('e','Invalid indent [R:TParsPy.ParseBlock]'); break; end
  else if SensCase(BReadS)='else' then
   begin
   RdTextS;
   LuTextS; BIndent:=GetIndent;
   if BIndent<=AIndent then // "else" with no body
   else
    begin
    if ParseBlock(AProc,ALevel+1,ABreakLabel,BIndent)=FALSE then break;
    end;
   end;
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** IF **

 if SensCase(BReadS)='while' then
  begin
  RdTextS;
  BLCmp:=AProc.LabelName('WhileCmp',ALevel,BLabelIndex);
  BLThen:=AProc.LabelName('WhileThen',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('WhileEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLCmp+':'+GenTail);
  BEval:=ParseEval(AProc,AIndent); if BEval='' then begin AppendError('e','Error parsing while condition [R:TParsPy.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  if Pos(#32,BEval)<>0 then BEval:='('+BEval+')';
  AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLEnd+GenTail);
  if LuTextS<>':' then AppendError('e','":" expected, "\l" found [R:TParsPy.ParseBlock]')
  else RdTextS;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  LuTextS; BIndent:=GetIndent;
  if BIndent<=AIndent then // "while" with no body
  else
   begin
   if ParseBlock(AProc,ALevel+1,BLEnd,BIndent)=FALSE then break;
   end;
  // JMP to the beginning
  AProc.FlowList.Append('jmp '+BLCmp+GenTail);
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** WHILE **  }

 if StrInList(BReadS,'inc dec chr ord') then
 else if IsReservedWord(BReadS) then begin RdTextS; AppendError('e','Misplaced \r [R:TParsPy.ParseBlock]'); break; end;

 if LuTextS2=':' then // Variable declaration
  begin
  BItemListS:=ParseVar('b',AProc.NameL+' '+AProc.VarListS,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  AProc.AppendVarListS(BItemListS);
  BResult:=TRUE;
  break;
  end;

 BTarg:=ParseEval(AProc,AIndent);
 if BTarg='' then begin AppendError('e','Error parsing eval (see previous messages) [R:TParsPy.ParseBlock]'); BTarg:=CInvalidIdentifier; end;

 if LuTextS='=' then
  begin
  RdTextS;
  BEval:=ParseEval(AProc,AIndent); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsPy.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);
  BResult:=TRUE;
  break;
  end;

 // LastFieldIsFunction
 if ParsIsProc(BTarg) then
  begin
  //BEval:=ParseCallParams(AProc,BTarg); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')';
  AProc.Flowlist.Append(CDiscardValue+' := '+BTarg+GenTail);
  BResult:=TRUE;
  break;
  end;

 // Something else
 AppendError('e','Construction is not recognized by the parser [R:TParsPy.ParseBlock]');
 break;
 until TRUE; // Error loop

 if BResult=FALSE then break;
 until FALSE;
End;

Procedure TParsPy.Parse ( AModule : TLlvmModule; AIsInclude : boolean );
Var
  BReadS        : string;
  BResultB      : boolean;
  BVisi         : char;
  //BResult       : boolean;
  BItemListS    : string;
  BProcName,
  BProcParams   : string;
  BProc         : TLlvmProc;
  BUsesItem     : TLlvmModule;
  BErrorA       : string;
  BNow          : Double;
  BVarName      : string;
  BFilePath,
  BFileName,
  BFileExt      : string;
  BIndentThis   : Integer;
Begin
 Inherited;

 BNow:=Now;
 FParsState:=psParsStart;

 if AIsInclude=FALSE then
  begin
  SplitFilename(FFilename,BFilePath,BFileName,BFileExt);
  FModule.Name:=BFileName;
  if LowerCase(BFileExt)='h' then FModule.Name:=FModule.Name+'.h';
  end;

 // Interface/Implementation parser
 BVisi:='h';

 repeat
 BReadS:=RdTextS;
 if BReadS='' then break;

 // ** Uses section **
 if SensCase(BReadS)='uses' then
  begin
  BResultB:=FALSE;
  repeat
  BReadS:=RdTextS;
  if IsAllowedName(BReadS)=FALSE then begin AppendError('e','Invalid unit name. (\r is not an allowed unit name) [R:TParsPy.GetUsesOpti]'); break; end;
  if Assigned(FGetUses)=FALSE then begin AppendError('e','Internal error: FGetUses unassigned [R:TParsPy.GetUsesOpti]'); break; end;
  BErrorA:='';
  BUsesItem:=FGetUses(BReadS,itPyUnit,BErrorA);
  if BUsesItem=nil then
   begin
   if BErrorA='' then BErrorA:='Cannot include uses module \r [R:TParsPy.GetUsesOpti]';
   AppendError('e',BErrorA);
   break;
   end;
  if FModule.AppendUses(BUsesItem)=FALSE then begin AppendError('e','Module \r already exists in the list [R:TParsPy.GetUsesOpti]'); break; end;
  BReadS:=RdTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPy.GetUsesOpti]'); break; end;
  if BReadS=';' then begin BResultB:=TRUE; break; end;
  if BReadS<>',' then begin AppendError('e','; or , expected, \r found [R:TParsPy.GetUsesOpti]'); break; end;
  until FALSE;
  if BResultB=FALSE then break;
  end

 // ** Var section **
 else if SensCase(BReadS)='var' then
  begin
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \l cannot be used as variable name'); break; end;
  BResultB:=FALSE;
  repeat
  BItemListS:=ParseVar(BVisi,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  if RdTextS<>';' then begin AppendError('e','; expected, \r found'); break; end;
  if SensCase(LuTextS)='external' then
   begin
   RdTextS;
   //BReadS:=RdTextS;
   //if Length(BReadS)<3 then begin AppendError('e','External reference module name expected [R:TParsPy.ParseProcImpl]'); break; end;
   //if (BReadS[1]<>#39) or (BReadS[Length(BReadS)]<>#39) then begin AppendError('e','External reference module name must be in quotes [R:TParsPy.ParseProcImpl]'); end;
   BVarName:=ReadParamStr(BItemListS);
   //FModule.AppendExtLoc(BVarName);
   FModule.AppendExternName(BVarName);
   DelFirstSpace(BItemListS);
   if BItemListS<>'' then AppendError('e','Only one external variable name is allowed (just declare them one by one and not comma-separated)');
   if RdTextS<>';' then begin AppendError('e','; expected, \r found'); break; end;
   end
  else
   begin
   FModule.VarList:=FModule.VarList+BItemListS;
   if BVisi='f' then FModule.PrivateNames:=FModule.PrivateNames+BItemListS
   else FModule.PublicNames:=FModule.PublicNames+BItemListS;
   end;
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin BResultB:=TRUE; break; end;
  until FALSE;
  if BResultB=FALSE then break;
  end

 // ** Type section **
 else if SensCase(BReadS)='type' then
  begin
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \l cannot be used as a type name'); break; end;
  BResultB:=FALSE;
  repeat
  BItemListS:=ParseType(BVisi,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  if BVisi='f' then FModule.PrivateNames:=FModule.PrivateNames+BItemListS
  else FModule.PublicNames:=FModule.PublicNames+BItemListS;
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin BResultB:=TRUE; break; end;
  until FALSE;
  if BResultB=FALSE then break;
  end

 // ** Const section **
 else if SensCase(BReadS)='const' then
  begin
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \l cannot be used as a const name'); break; end;
  BResultB:=FALSE;
  repeat
  ParseConst(BVisi,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin BResultB:=TRUE; break; end;
  until FALSE;
  if BResultB=FALSE then break;
  end

 // ** Methods **
 else if StrInList(SensCase(BReadS),'def') then
  begin
  BIndentThis:=FTokenThis.Pos;
  BItemListS:=ParseProcHead(BVisi,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,BProcName,BProcParams);
  BProc:=TLlvmProc(ObjByName(FModule.ProcList,BProcName));
  if BProc=nil then
   begin
   FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
   BProc:=FModule.AppendProc;
   BProc.Name:=BProcName; BProc.NameL:=BItemListS; BProc.ParListS:=BProcParams;
   end
  else
   begin
   AppendError('e','Procedure '+BProcName+' is already declared [R:TParsPy.Parse]');
   end;
  BReadS:=LuTextS;
  if BReadS='external' then
   begin
   RdTextS;
   FModule.ExternNames:=FModule.ExternNames+BItemListS+' ';
   FModule.AppendExternName(BProc.NameL);
   end
  else ParseProcImpl(BProc,BIndentThis);
  end

 else // ** Error **
  begin
  AppendError('e','Uses, Type, Var, Const, Procedure or function declaration expected (\r found) [R:TParsPy.Parse]');
  break;
  end;
 until FALSE;

 FModule.CompStat.ParsTime:=FModule.CompStat.ParsTime+now-BNow;
 FParsState:=psParsEnd;
End;


end.

