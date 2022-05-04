unit ParsPas_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, AsmTypes_sd, ParsBase_sd, ParsHelper_sd;

Const
  CPasResWords  : string =
   (
    ' unit uses interface implementation initialization finalization'+
    ' procedure function constructor destructor var const type'+
    ' record object class'+
    ' private protected public published'+
    ' virtual override'+
    ' if then else'+
    ' while do'+
    ' for to downto do'+
    ' repeat until'+
    ' begin end'+
    ' array of'+
    ' '
   );

Type
  TParsPas = class(TParsBase)
  private
    Procedure ParseTypeA ( Out ATypeS : string; Const AKnownNames : string; AReportError : boolean );
    Function ParseVar ( AVisi : char; Const AFilter : string; Const AKnownTypes : string ) : string;
    Function ParseType ( AVisi : char; Const AKnownNames : string ) : string;
    Procedure ParseConst ( AVisi : char; Const AKnownNames : string );
    Function ResolveDirectives ( Const AName : string ) : string;
    Function ResolveConst ( Const AName : string ) : string;
    Function ParseProcHead ( AVisi, AProcType : char; Const AFilter : string; Const AKnownTypes : string; Out AProcName, AProcParams : string ) : string;
    Function ParseProcVarTypeConst ( AProc : TLlvmProc ) : boolean;
    Function ParseProcBody ( AProc : TLlvmProc ) : boolean;
    Procedure ParseProcImpl ( AProc : TLlvmProc );
    Function ParseArrayOpti ( AProc : TLlvmProc; Const ATarg : string ) : string;
    Function ParseFieldOpti ( AProc : TLlvmProc ) : string;
    Function ParseEval ( AProc : TLlvmProc ) : string;
    Function ParseCallParams ( AProc : TLlvmProc; Const AParamTypes : string ) : string;
    Function ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const ABreakLabel : string; Const AEndMarker : string ) : boolean;
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

Constructor TParsPas.Create;
Begin
 Inherited;
 FOneSymbol:=' // <> <= >= := (* *) .. ... ';
 FStrQuotes:=#39;
 FSymbolList:=#32+';.,:-+=/*()[]<>{}!|&~?^@';
End;

Destructor TParsPas.Destroy;
Begin
 Inherited;
End;

Function TParsPas.SensCase ( Const ADataS : string ) : string;
Begin
 Result:=LowerCase(ADataS);
End;

Function TParsPas.StrToOpcode ( Const AOpcodeS : string ) : string;
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
 else if AOpcodeS='shl' then Result:='shl'
 else if AOpcodeS='shr' then Result:='shr'
 else if AOpcodeS='='   then Result:='cmpe'
 else if AOpcodeS='<>'  then Result:='cmpne'
 else if AOpcodeS='<'   then Result:='cmpb'
 else if AOpcodeS='>'   then Result:='cmpa'
 else if AOpcodeS='<='  then Result:='cmpbe'
 else if AOpcodeS='>='  then Result:='cmpae';
End;

Function TParsPas.IsReservedWord ( Const AName : string ) : boolean;
Begin
 Result:=StrInList(SensCase(AName),CPasResWords);
End;

Function TParsPas.IsTargConstant ( Const ANameS : string ) : string;
Var
  BNameS,
  BNameA        : string;
  BDataC        : Cardinal;
  BDataF        : Extended;
  BLen          : Integer;
  BDataI        : Integer;

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

 if Pos('#',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  if StringToInteger(BNameS,BDataI)=FALSE then break;
  Result:=CTagS+'c_c'+CTagM+IntToStr(BDataI)+CTagE;
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
  if TryStrToFloat(BNameS,BDataF,HParsFormat)=FALSE then break;
  Result:=CTagS+'c_f'+CTagM+BNameS+CTagE;
  break;
  end;

 BDataC:=0;
 BNameA:=BNameS;
 while BNameA<>'' do
  begin
  if BDataC>=($FFFFFFFF-10) then break;
  if BNameA[1] in ['0'..'9'] then BDataC:=(BDataC*10) + Cardinal(ord(BNameA[1])-ord('0'))
  else break;
  Delete(BNameA,1,1);
  end;
 if BNameA='' then
  begin
  Result:=ParsFormatConst(BDataC);
  break;
  end;

 if (BNameS[1] in ['0'..'9']) and TryStrToFloat(BNameS,BDataF,HParsFormat) then
  begin
  Result:=CTagS+'c_f'+CTagM+BNameS+CTagE;
  break;
  end;
 until TRUE;
End;

Procedure TParsPas.RdTextFC ( AJmpNextLine : boolean );
Var
  BLastComment  : char;
Begin
 repeat
 while FCommentLevel<>'' do
  begin
  BLastComment:=FCommentLevel[Length(FCommentLevel)];
  RdTextA(TRUE);
  if FParsOrig='' then break;
  if (FParsOrig='}') and (BLastComment='{') then Delete(FCommentLevel,Length(FCommentLevel),1);
  if (FParsOrig='*)') and (BLastComment='*') then Delete(FCommentLevel,Length(FCommentLevel),1);
  end;

 RdTextA(AJmpNextLine);
 if FParsOrig='' then break;
 if FParsOrig='{' then FCommentLevel:=FCommentLevel+'{'
 else if FParsOrig='(*' then FCommentLevel:=FCommentLevel+'*'
 else if FParsOrig='//' then
  begin
  SkipLine;
  if AJmpNextLine=FALSE then begin FParsOrig:=''; break; end;
  end
 else break;

 until FALSE;
End;

Procedure TParsPas.RdTextFD ( AJmpNextLine : boolean );
Begin
 RdTextFC(AJmpNextLine);
End;

Procedure TParsPas.ParseTypeA ( Out ATypeS : string; Const AKnownNames : string; AReportError : boolean );
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
 if SensCase(BReadS)='boolean' then begin ATypeS:='l'; break; end;
 if SensCase(BReadS)='char' then begin ATypeS:='c'; break; end;
 if SensCase(BReadS)='byte' then begin ATypeS:='b'; break; end;
 if SensCase(BReadS)='word' then begin ATypeS:='w'; break; end;
 if SensCase(BReadS)='cardinal' then begin ATypeS:='d'; break; end;
 if SensCase(BReadS)='shortint' then begin ATypeS:='k'; break; end;
 if SensCase(BReadS)='smallint' then begin ATypeS:='m'; break; end;
 if SensCase(BReadS)='integer' then begin ATypeS:='i'; break; end;
 if SensCase(BReadS)='real' then begin ATypeS:='f'; break; end;
 if SensCase(BReadS)='array' then
  begin
  if RdTextS<>'[' then AppendError('e','[ expected, \r found [R:TParsPas.ParseTypeA]');
  ATypeS:='';
  repeat
  BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayS)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPas.ParseTypeA]');
  if RdTextS<>'..' then AppendError('e','.. expected, \r found [R:TParsPas.ParseTypeA]');
  BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayE)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPas.ParseTypeA]');
  ATypeS:=ATypeS+'a'+IntToStr(BArrayS)+'s'+IntToStr(BArrayE)+'e';
  BReadS:=RdTextS;
  if BReadS=']' then break;
  if BReadS<>',' then begin AppendError('e','"," or "]" expected, \r found [R:TParsPas.ParseTypeA]'); break; end;
  until FALSE;
  if ATypeS='' then ATypeS:='_';
  if RdTextS<>'of' then AppendError('e','"of" expected, \r found [R:TParsPas.ParseTypeA]');
  ParseTypeA(BTypeS,AKnownNames,TRUE);
  ATypeS:=ATypeS+BTypeS;
  break;
  end;
 if SensCase(BReadS)='string' then
  begin
  if LuTextS='[' then
   begin
   RdTextS;
   BReadS:=RdTextS; if TryStrToInt(BReadS,BArrayE)=FALSE then AppendError('e','Integer type expected, \r found [R:TParsPas.ParseTypeA]');
   if RdTextS<>']' then AppendError('e','] expected, \r found [R:TParsPas.ParseTypeA]');
   if BArrayE>255 then AppendError('e','String dimension is too big (max 255 allowed) [R:TParsPas.ParseTypeA]');
   if BArrayE<=0 then AppendError('e','String dimension is too small [R:TParsPas.ParseTypeA]');
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
 if AReportError then AppendError('e','Invalid type \r [R:TParsPas.ParseTypeA]');

 until TRUE;
End;

Function TParsPas.ParseVar ( AVisi : char; Const AFilter : string; Const AKnownTypes : string ) : string;
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
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a variable name');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a variable name');
 if Pos(' '+BReadS+' ',BVarList)<>0 then AppendError('e','Identifier \r has just been declared');
 if Pos(CTagM+BReadS+CTagE+' ',BFilterS)<>0 then AppendError('e','Identifier \r is already declared');
 BVarList:=BVarList+' '+BReadS+' ';
 BReadS:=RdTextS;
 if BReadS=':' then begin ParseTypeA(BTypeS,AKnownTypes,TRUE); break; end;
 if BReadS<>',' then begin AppendError('e',': or , expected, \r found [R:TParsPas.ParseVar]'); SkipText(';'); break; end;
 until FALSE;

 while BVarList<>'' do
  begin
  BReadS:=ReadParamStr(BVarList);
  if BReadS='' then break;
  Result:=Result+CTagS+'d'+AVisi+BTypeS+CTagM+BReadS+CTagE+' ';
  end;
End;

Function TParsPas.ParseType ( AVisi : char; Const AKnownNames : string ) : string;
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
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a type name [R:TParsPas.ParseType]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a type name [R:TParsPas.ParseType]');
 if Pos(' '+BReadS+' ',Result)<>0 then AppendError('e','Type \r has just been declared [R:TParsPas.ParseType]');
 if Pos(CTagM+BReadS+CTagE+' ',BKnownTypesS)<>0 then AppendError('e','Type \r is already declared [R:TParsPas.ParseType]');
 if RdTextS<>'=' then begin AppendError('e','= expected, \r found [R:TParsPas.ParseType]'); SkipText(';'); break; end;
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
  if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPas.ParseType]'); SkipText(';'); break; end;
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
 if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPas.ParseType]'); SkipText(';'); break; end;
 if IsReservedWord(LuTextS) then break;
 until FALSE;
End;

Function TParsPas.ResolveDirectives ( Const AName : string ) : string;
Begin
 Result:='';
 repeat
 if LowerCase(AName)='inc' then begin Result:=CDirectiveInc; break; end;
 if LowerCase(AName)='dec' then begin Result:=CDirectiveDec; break; end;
 if LowerCase(AName)='chr' then begin Result:=CDirectiveChr; break; end;
 if LowerCase(AName)='ord' then begin Result:=CDirectiveOrd; break; end;
 until TRUE;
End;

Function TParsPas.ResolveConst ( Const AName : string ) : string;
Begin
 Result:='';
 repeat
 if LowerCase(AName)='nil' then begin Result:=CPointerNil; break; end;
 until TRUE;
End;

Procedure TParsPas.ParseConst ( AVisi : char; Const AKnownNames : string );
Var
  BKnownNamesS  : string;
  BReadS        : string;
  BConstName,
  BConstData    : string;
Begin
 BKnownNamesS:=LowerCase(AKnownNames);

 repeat
 BReadS:=RdTextS; BConstName:=BReadS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a Const name [R:TParsPas.ParseConst]');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a Const name [R:TParsPas.ParseConst]');
 if FModule.AliasExists(BConstName,TRUE) then AppendError('e','Identifier \r is already declared [R:TParsPas.ParseConst]');
 if Pos(CTagM+BReadS+CTagE+' ',BKnownNamesS)<>0 then AppendError('e','Identifier \r is already declared [R:TParsPas.ParseConst]');
 if RdTextS<>'=' then begin AppendError('e','= expected, \r found [R:TParsPas.ParseConst]'); SkipText(';'); break; end;
 BReadS:=RdTextS; BConstData:=BReadS;
 repeat
 BReadS:=LuTextS;
 if BReadS=';' then break;
 if BReadS='' then break;
 BConstData:=BConstData+RdTextS;
 until FALSE;
 if AVisi='f' then FModule.PrivateAliases.Values[BConstName]:=BConstData
 else FModule.PublicAliases.Values[BConstName]:=BConstData;
 if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPas.ParseConst]'); SkipText(';'); break; end;
 if IsReservedWord(LuTextS) then break;
 until FALSE;
End;

Function TParsPas.ParseProcHead ( AVisi, AProcType : char; Const AFilter : string; Const AKnownTypes : string; Out AProcName, AProcParams : string ) : string;
Var
  BFilterS      : string;
  BReadS        : string;
  BTypeS        : string;
  BProcName     : string;
  BVarList      : string;
  BVarVisi      : char;
Begin
 Result:='';
 BFilterS:=LowerCase(AFilter);
 BVarList:=''; BTypeS:='_';

 BReadS:=RdTextS;
 if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a procedure/function name');
 if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a procedure/function name');
 if Pos(CTagS+BReadS+CTagE+' ',BFilterS)<>0 then AppendError('e','Identifier \r is already declared');
 BProcName:=BReadS;
 BReadS:=RdTextS;
 if BReadS='(' then
  begin
  BReadS:=LuTextS;
  if BReadS=')' then
  else
   repeat
   BVarVisi:='a';
   if SensCase(BReadS)='var' then begin BVarVisi:='v'; RdTextS; end
   else if SensCase(BReadS)='const' then begin BVarVisi:='c'; RdTextS; end;
   BVarList:=BVarList+ParseVar(BVarVisi,BVarList,AKnownTypes);
   BReadS:=RdTextS;
   if BReadS=')' then break;
   if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
   if BReadS<>';' then begin AppendError('e','; or ) expected, \r found'); break; end;
   BReadS:=LuTextS;
   until FALSE;
  BReadS:=RdTextS;
  end;
 if BReadS='' then AppendError('e','Unexpected end of file')
 else if BReadS=';' then
  begin
  if AProcType='f' then AppendError('e','Function must have a return type [R:TParsPas.ParseProcHead]');
  end
 else if BReadS=':' then
  begin
  if AProcType<>'f' then AppendError('e','Only a function can have a return type [R:TParsPas.ParseProcHead]');
  ParseTypeA(BTypeS,AKnownTypes,TRUE);
  if RdTextS<>';' then AppendError('e','; expected \r found [R:TParsPas.ParseProcHead]');
  end
 else
  begin
  AppendError('e','Unexpected character \r found [R:TParsPas.ParseProcHead]');
  end;

 AProcParams:=BVarList;

 AProcName:=BProcName;
 Result:=CTagS+'x'+AVisi+BTypeS+'{'+ParsCreateParamsSpec(BVarList)+'}'+CTagM+BProcName+CTagE;
End;

Function TParsPas.ParseProcVarTypeConst ( AProc : TLlvmProc ) : boolean;
Var
  BReadS        : string;
  BResultB      : boolean;
  BItemListS    : string;
Begin
 Result:=FALSE;

 repeat
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;

 if SensCase(BReadS)='begin' then begin Result:=TRUE; break; end;

 // ** Var section **
 if SensCase(BReadS)='var' then
  begin
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \l cannot be used as variable name'); break; end;
  BResultB:=FALSE;
  repeat
  BItemListS:=ParseVar('b',AProc.NameL+' '+AProc.VarListS,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  AProc.AppendVarListS(BItemListS);
  if RdTextS<>';' then begin AppendError('e','; expected, \r found'); break; end;
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  if IsReservedWord(BReadS) then begin BResultB:=TRUE; break; end;
  until FALSE;
  if BResultB=FALSE then break;
  end

 else // ** Error **
  begin
  AppendError('e','Type, Var, Const, Procedure or function declaration expected (\r found)');
  break;
  end;
 until FALSE;
End;

Function TParsPas.ParseProcBody ( AProc : TLlvmProc ) : boolean;
Var
  BReadS        : string;
Begin
 Result:=FALSE;

 AProc.StartLine:=FRdQueue[0].Line+1; AProc.StartPos:=FRdQueue[0].Pos+1;

 repeat
 FWithList:='';
 if ParseBlock(AProc,0,'',' end ')=FALSE then break;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
 if SensCase(BReadS)<>'end' then begin AppendError('e','Internal error: end expected, \r found'); break; end;
 AProc.EndLine:=FRdQueue[0].Line+1; AProc.EndPos:=FRdQueue[0].Pos+1;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
 if BReadS<>';' then begin AppendError('e','; expected at the end of procedure, \r found'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Procedure TParsPas.ParseProcImpl ( AProc : TLlvmProc );
Var
  BType         : string;
Begin
 repeat
 BType:=ParsExtractRetType(AProc.NameL);
 if BType<>'' then AProc.RetListS:=AProc.RetListS+CTagS+'dr'+BType+CTagM+'Result'+CTagE+' ';
 if SensCase(LuTextS)='external' then
  begin
  RdTextS;
  //FModule.AppendExtLoc(AProc.NameL);
  FModule.AppendExternName(AProc.NameL);
  if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsPas.ParseProcImpl]'); break; end;
  end
 else
  begin
  if ParseProcVarTypeConst(AProc)=FALSE then break;
  if ParseProcBody(AProc)=FALSE then break;
  end;

 AProc.IsImplemented:=TRUE;
 until TRUE;
End;

Function TParsPas.ParseArrayOpti ( AProc : TLlvmProc; Const ATarg : string ) : string;
Var
  BReadS        : string;
  BTarg         : string;
Begin
 Result:='';
 if ParsIsArray(ATarg) or ParsIsStringP(ATarg) then
 else AppendError('e','\r is not an array and cannot be indexed [R:TParsPas.ParseArrayOpti]');
 RdTextS;
 Result:=Result+'[';
 repeat
 BTarg:=ParseEval(AProc); if BTarg='' then BTarg:=CInvalidIdentifier;
 Result:=Result+BTarg;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.ParseArrayOpti]'); break; end;
 if BReadS=']' then break;
 if BReadS<>',' then begin AppendError('e','"]" or "," expected, \r found [R:TParsPas.ParseArrayOpti]'); break; end;
 Result:=Result+',';
 until FALSE;
 Result:=Result+']';
End;

Function TParsPas.ParseFieldOpti ( AProc : TLlvmProc ) : string;
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
  BTarg:=ParseEval(AProc); if BTarg='' then BTarg:=CInvalidIdentifier;
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

Function TParsPas.ParseEval ( AProc : TLlvmProc ) : string;
Var
  BReadS        : string;
  BOpcode       : string;
  BTarg,
  BEval         : string;
  BIsNot        : boolean;
  BSubEval      : string;
Begin
 Result:='';

 repeat
 // Variable/Function
 BReadS:=LuTextS;
 if BReadS='@' then
  begin
  RdTextS;
  Result:=Result+'@';
  BTarg:=ParseEval(AProc); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+BTarg;
  end
 else if BReadS='(' then
  begin
  RdTextS;
  Result:=Result+'(';
  BTarg:=ParseEval(AProc); if BTarg='' then BTarg:=CInvalidIdentifier;
  Result:=Result+BTarg;
  if RdTextS<>')' then AppendError('e',') expected, \r found [R:TParsPas.ParseEval]');
  Result:=Result+')';
  end
 else if BReadS='[' then
  begin
  RdTextS;
  Result:=Result+'<';
  BSubEval:='';
  repeat
  BReadS:=LuTextS;
  if BReadS='...' then begin BTarg:='...'; RdTextS; end
  else BTarg:=ParseEval(AProc); if BTarg='' then break;
  if BSubEval<>'' then BSubEval:=BSubEval+',';
  BSubEval:=BSubEval+BTarg;
  BReadS:=RdTextS;
  if BReadS=']' then break;
  if BReadS<>',' then begin AppendError('e','"," or "]" expected, \r found [R:TParsPas.ParseEval]'); break; end;
  until FALSE;
  Result:=Result+BSubEval+'>';
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
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:ParsPas.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if IsReservedWord(BReadS) then begin AppendError('e','Reserved word \r cannot be used as an identifier [R:ParsPas.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if IsSymbol(BReadS[1]) then begin AppendError('e','Misplaced symbol \r [R:ParsPas.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if BTarg='' then BTarg:=ResolveDirectives(BReadS);
  if BTarg='' then BTarg:=ResolveConst(BReadS);
  if BTarg='' then BTarg:=GetTarg(AProc,BReadS);
  if BTarg='' then begin AppendError('e','Identifier \r not found [R:ParsPas.ParseEval]'); BTarg:=CInvalidIdentifier; end;
  if ParsIsProc(BTarg) then begin BEval:=ParseCallParams(AProc,ParsExtractParamTypes(BTarg)); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')'; end;
  Result:=Result+BTarg;
  end;
 // Operation or '['
 BReadS:=LuTextS;
 if BReadS='^' then
  begin
  RdTextS;
  Result:=Result+'['+CConstZero+']';
  BReadS:=LuTextS;
  end;
 if BReadS='[' then
  begin
  Result:=Result+ParseArrayOpti(AProc,BTarg);
  BReadS:=LuTextS;
  end;
 if BReadS='.' then
  begin
  Result:=Result+ParseFieldOpti(AProc);
  BReadS:=LuTextS;
  end;
 if (BReadS=';') or (BReadS=')') or (BReadS=']') or (BReadS=',') or (BReadS=':=') or (BReadS=':') or IsReservedWord(BReadS) then break;
 RdTextS;
 BOpcode:=StrToOpcode(BReadS);
 if BOpcode='' then begin AppendError('e','Operation code expected, \r found [R:ParsPas.ParseEval]'); BReadS:=CInvalidOpcode; end;
 Result:=Result+' '+BOpcode+' ';
 until FALSE;
End;

Function TParsPas.ParseCallParams ( AProc : TLlvmProc; Const AParamTypes : string ) : string;
Var
  BReadS,
  BEval         : string;
  BParams       : string;
  BTypeS        : string;
Begin
 Result:='';
 BParams:=ParsSplitParamTypes(AParamTypes); DelFirstSpace(BParams); DelLastSpace(BParams);
 repeat
 BReadS:=LuTextS;
 if BReadS<>'(' then
  begin
  if BParams<>'' then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsPas.ParseCallParams]');
  break;
  end;
 RdTextS;
 BReadS:=LuTextS;

  repeat
  DelFirstSpace(BParams);
  BTypeS:=ReadParamStr(BParams);
  if BReadS=')' then BEval:='' else BEval:=ParseEval(AProc);
  if (BEval='') and (BTypeS='') then break;
  if (BEval='') and (BTypeS<>'') then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsPas.ParseCallParams]');
  if (BEval<>'') and (BTypeS='') then AppendError('e','Extra parameter in call of procedure/function [R:TParsPas.ParseCallParams]');
  if (BEval<>'') and (BTypeS<>'') then Result:=Result+BEval+CTagP;
  if BEval<>'' then
   begin
   BReadS:=LuTextS;
   if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.ParseCallParams]'); break; end;
   if BReadS=',' then RdTextS
   else if BReadS<>')' then begin RdTextS; AppendError('e',', or ) expected, \r found [R:TParsPas.ParseCallParams]'); end;
   end;
  until FALSE;

 BReadS:=LuTextS;
 if BReadS<>')' then AppendError('e',') expected, \r found [R:TParsPas.ParseCallParams]')
 else RdTextS;
 until TRUE;
End;

Function TParsPas.ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const ABreakLabel : string; Const AEndMarker : string ) : boolean;
Var
  BReadS        : string;
  BTarg,
  BEval         : string;
  BResult,
  BResultA      : boolean;
  BLabelIndex   : Integer;
  BLStart,
  BLCmp,
  BLEnd,
  BLThen,
  BLElse        : string;
  BWithSave,
  BWithList     : string;
  BCaseIdx      : Integer;
  BLCmpThis,
  BLCmpNext,
  BLThenThis,
  BLThenNext    : string;

Begin
 Result:=FALSE;
 BLabelIndex:=0;

 repeat
 BReadS:=LuTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.ParseBlock]'); break; end;
 if StrInList(SensCase(BReadS),AEndMarker) then begin Result:=TRUE; break; end;

 BResult:=FALSE;

 repeat // Error block

 if SensCase(BReadS)='break' then
  begin
  RdTextS;
  if ABreakLabel='' then begin AppendError('e','Misplaced break [R:TParsPas.ParseBlock]'); break; end;
  AProc.FlowList.Append('jmp '+ABreakLabel+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end;

 // ** IF **
 if SensCase(BReadS)='if' then
  begin
  RdTextS;
  BLThen:=AProc.LabelName('IfThen',ALevel,BLabelIndex);
  BLElse:=AProc.LabelName('IfElse',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('IfEnd',ALevel,BLabelIndex);
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing if condition [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLElse+GenTail);
  if LuTextS<>'then' then AppendError('e','then expected, \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='begin' then // Then with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' end ')=FALSE then break;
   if RdTextS<>'end' then begin AppendError('e','Internal error: end expected, \r found [R:TParsPas.ParseBlock]'); break; end;
   end
  else // Then without begin
   begin
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' ; else'+AEndMarker)=FALSE then break;
   end;
  AProc.FlowList.Append('jmp '+BLEnd+GenTail);
  BReadS:=LuTextS;
  if (BReadS=';') and (LuTextS2='else') then AppendError('e','\l is not allowed before else [R:TParsPas.ParseBlock]');
  AProc.FlowList.Append(BLElse+':'+GenTail);
  if SensCase(BReadS)='else' then
   begin
   RdTextS;
   if LuTextS='begin' then // else with begin
    begin
    RdTextS;
    if ParseBlock(AProc,ALevel+1,ABreakLabel,' end ')=FALSE then break;
    if RdTextS<>'end' then begin AppendError('e','Internal error: end expected, \r found [R:TParsPas.ParseBlock]'); break; end;
    end
   else // else without begin
    begin
    if ParseBlock(AProc,ALevel+1,ABreakLabel,' ;'+AEndMarker)=FALSE then break;
    end;
   BReadS:=LuTextS;
   end;
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  inc(BLabelIndex);
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end; // ** IF **

 if SensCase(BReadS)='while' then
  begin
  RdTextS;
  BLCmp:=AProc.LabelName('WhileCmp',ALevel,BLabelIndex);
  BLThen:=AProc.LabelName('WhileThen',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('WhileEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLCmp+':'+GenTail);
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing while condition [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  if Pos(#32,BEval)<>0 then BEval:='('+BEval+')';
  AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLEnd+GenTail);
  if LuTextS<>'do' then AppendError('e','do expected, \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='begin' then // while with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,BLEnd,' end ')=FALSE then break;
   if RdTextS<>'end' then AppendError('e','Internal error: end expected, \r found [R:TParsPas.ParseBlock]');
   end
  else // while without begin
   begin
   if ParseBlock(AProc,ALevel+1,BLEnd,' ;'+AEndMarker)=FALSE then break;
   end;
  // JMP to the beginning
  AProc.FlowList.Append('jmp '+BLCmp+GenTail);
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end; // ** WHILE **  }

 if SensCase(BReadS)='repeat' then
  begin
  RdTextS;
  BLStart:=AProc.LabelName('RepeatStart',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('RepeatEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLStart+':'+GenTail);
  if ParseBlock(AProc,ALevel+1,BLEnd,' until ')=FALSE then break;
  if RdTextS<>'until' then AppendError('e','Internal error: until expected, \r found [R:TParsPas.ParseBlock]');
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing repeat condition [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  if Pos(#32,BEval)<>0 then BEval:='('+BEval+')';
  AProc.FlowList.Append('if '+BEval+' '+BLEnd+' '+BLStart+GenTail);
  // End label
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end; // ** REPEAT **  }

 if SensCase(BReadS)='for' then
  begin
  RdTextS;
  BLCmp:=AProc.LabelName('ForCmp',ALevel,BLabelIndex);
  BLThen:=AProc.LabelName('ForThen',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('ForEnd',ALevel,BLabelIndex);
  BReadS:=RdTextS;
  if IsReservedWord(BReadS) then AppendError('e','Misplaced \r [R:TParsPas.ParseBlock]');
  BTarg:=GetTarg(AProc,BReadS);
  if BTarg='' then begin AppendError('e','Identifier \r not found [R:TParsPas.ParseBlock]'); BTarg:=CInvalidIdentifier; end;
  if LuTextS<>':=' then AppendError('e',':= expected, \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.Flowlist.Append(BTarg+' := '+BEval+GenTail);
  BReadS:=LuTextS;
  if BReadS<>'to' then AppendError('e','to expected \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlowList.Append(BLCmp+':'+GenTail);
  if Pos(#32,BEval)<>0 then BEval:='('+BEval+')';
  AProc.FlowList.Append('if '+BTarg+' cmpbe '+BEval+' '+BLThen+' '+BLEnd+GenTail);
  if LuTextS<>'do' then AppendError('e','do expected, \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='begin' then // while with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,BLEnd,' end ')=FALSE then break;
   if RdTextS<>'end' then AppendError('e','Internal error: end expected, \r found [R:TParsPas.ParseBlock]');
   end
  else // while without begin
   begin
   if ParseBlock(AProc,ALevel+1,BLEnd,' ;'+AEndMarker)=FALSE then break;
   end;
  // INC index
  AProc.FlowList.Append(BTarg+' := '+BTarg+' add '+CTagS+'c_'+ParsExtractType(BTarg)+CTagM+'1'+CTagE+GenTail);
  // JMP to the beginning
  AProc.FlowList.Append('jmp '+BLCmp+GenTail);
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end; // ** FOR **

 if BReadS='case' then
  begin
  RdTextS;
  BLStart:=AProc.LabelName('CaseStart',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('CaseEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLStart+':'+GenTail);
  BEval:=ParseEval(AProc);
  if LuTextS<>'of' then AppendError('e','"of" expected, \l found [R:TParsPas.ParseBlock]')
  else RdTextS;
  if BEval='' then begin AppendError('e','Error parsing case selection [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  BTarg:=AProc.AppendTmpVar('?');
  AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);

  BCaseIdx:=0;
  BLCmpNext:=AProc.LabelName('CaseCmp_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BLThenNext:=AProc.LabelName('CaseThen_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BResultA:=FALSE;
  repeat
  BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext; inc(BCaseIdx);
  BLCmpNext:=AProc.LabelName('CaseCmp_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BLThenNext:=AProc.LabelName('CaseThen_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing case selection [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  if RdTextS<>':' then begin AppendError('e','":" expected, \r found [R:TParsPas.ParseBlock]'); break; end;
  AProc.FlowList.Append(BLCmpThis+':'+GenTail);
  AProc.FlowList.Append('if '+BTarg+' cmpe ('+BEval+') '+BLThenThis+' '+BLCmpNext+GenTail);
  AProc.FlowList.Append(BLThenThis+':'+GenTail);
  if LuTextS='begin' then // case with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' end ')=FALSE then break;
   if RdTextS<>'end' then AppendError('e','Internal error: "end" expected, \r found [R:TParsPas.ParseBlock]');
   end
  else // while without begin
   begin
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' ;'+AEndMarker)=FALSE then break;
   end;
  if RdTextS<>';' then AppendError('e','Internal error: ";" expected, \r found [R:TParsPas.ParseBlock]');
  AProc.FlowList.Append('jmp '+BLEnd+GenTail);
  BReadS:=LuTextS;
  if BReadS='end' then
   begin
   RdTextS;
   BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext;
   AProc.FlowList.Append(BLCmpThis+':'+GenTail);
   BResultA:=TRUE;
   break;
   end;
  if BReadS='else' then
   begin
   RdTextS;
   BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext;
   AProc.FlowList.Append(BLCmpThis+':'+GenTail);
   AProc.FlowList.Append(BLThenThis+':'+GenTail);
   if LuTextS='begin' then // case with begin
    begin
    RdTextS;
    if ParseBlock(AProc,ALevel+1,ABreakLabel,' end ')=FALSE then break;
    if RdTextS<>'end' then AppendError('e','Internal error: "end" expected, \r found [R:TParsPas.ParseBlock]');
    end
   else // case without begin
    begin
    if ParseBlock(AProc,ALevel+1,ABreakLabel,' ;'+AEndMarker)=FALSE then break;
    end;
   if RdTextS<>';' then AppendError('e','Internal error: ";" expected, \r found [R:TParsPas.ParseBlock]');
   if RdTextS<>'end' then begin AppendError('e','"end" expected after case else statement, \r found [R:TParsPas.ParseBlock]'); break; end;
   BResultA:=TRUE;
   break;
   end;
  until FALSE;
  if BResultA=FALSE then break;
  if RdTextS<>';' then AppendError('e','Internal error: ";" expected, \r found [R:TParsPas.ParseBlock]');
  // End label
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** CASE **

 if SensCase(BReadS)='with' then
  begin
  RdTextS;
  BWithList:='';
  repeat
  BEval:=ParseEval(AProc); if BEval='' then begin AppendError('e','Error parsing Eval [R:TParsPas.ParseBlock]'); BTarg:=CInvalidIdentifier; end;
  if ParsIsTypeRecord(AProc.ExtractFinalType(BEval))=FALSE then AppendError('e','Identifier or a field is not a record [R:TParsPas.ParseBlock]');
  BWithList:=BWithList+BEval+' ';
  BReadS:=RdTextS;
  if BReadS='do' then break;
  if BReadS<>',' then break;
  until FALSE;
  if SensCase(BReadS)<>'do' then AppendError('e','"do" or "," expected, \r found [R:TParsPas.ParseBlock]');
  if LuTextS='begin' then // with with begin
   begin
   RdTextS;
   BWithSave:=FWithList; FWithList:=FWithList+BWithList;
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' end ')=FALSE then break;
   FWithList:=BWithSave;
   if RdTextS<>'end' then AppendError('e','Internal error: end expected, \r found [R:TParsPas.ParseBlock]');
   end
  else // while without begin
   begin
   BWithSave:=FWithList; FWithList:=FWithList+BWithList;
   if ParseBlock(AProc,ALevel+1,ABreakLabel,' ;'+AEndMarker)=FALSE then break;
   FWithList:=BWithSave;
   end;
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsPas.ParseBlock]');
  break;
  end; // ** WITH **  }

 if StrInList(BReadS,'inc dec chr ord') then
 else if IsReservedWord(BReadS) then begin RdTextS; AppendError('e','Misplaced \r [R:TParsPas.ParseBlock]'); break; end;

 BTarg:=ParseEval(AProc);
 if BTarg='' then begin AppendError('e','Error parsing eval (see previous messages) [R:TParsPas.ParseBlock]'); BTarg:=CInvalidIdentifier; end;

 if LuTextS=':=' then
  begin
  RdTextS;
  BEval:=ParseEval(AProc);
  if BEval='' then begin AppendError('e','Error parsing eval [R:TParsPas.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsPas.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 // LastFieldIsFunction
 if ParsIsProc(BTarg) then
  begin
  //BEval:=ParseCallParams(AProc,BTarg); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')';
  AProc.Flowlist.Append(CDiscardValue+' := '+BTarg+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected (\r found) [R:TParsPas.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 // Something else
 AppendError('e','Construction is not recognized by the parser [R:TParsPas.ParseBlock]');
 break;
 until TRUE; // Error loop

 if BResult=FALSE then break;
 until FALSE;
End;

Const
  CMetClass     = ' procedure function constructor destructor ';

Procedure TParsPas.Parse ( AModule : TLlvmModule; AIsInclude : boolean );
Var
  BReadS        : string;
  BResultA,
  BResultB      : boolean;
  BVisi         : char;
  //BResult       : boolean;
  BItemListS    : string;
  BMetClass     : char;
  BProcName,
  BProcParams   : string;
  BProc         : TLlvmProc;
  BUsesItem     : TLlvmModule;
  BErrorA       : string;
  BNameA,
  BNameB        : string;
  BNow          : Double;
  BVarName      : string;
Begin
 Inherited;

 BNow:=Now;
 FParsState:=psParsStart;

 repeat
 // Unit
 if SensCase(RdTextS)<>'unit' then begin AppendError('e','Unit name is expected at the beginning of the file, (\r found) [R:TParsPas.Parse]'); break; end;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unit name is expected, (\r found) [R:TParsPas.Parse]'); break; end;
 if IsAllowedName(BReadS)=FALSE then begin AppendError('e','Unit name error. (\r is not an allowed unit name) [R:TParsPas.Parse]'); break; end;
 if (SensCase(BReadS)+'.pas')<>LowerCase(ExtractFileName(FFileName)) then begin AppendError('e','Unit name \r must be somewhat similar to a file name [R:TParsPas.Parse]'); break; end;
 if AIsInclude=FALSE then FModule.Name:=BReadS;
 if RdTextS<>';' then begin AppendError('e','; is expected at the end of the line (\r found) [R:TParsPas.Parse]'); break; end;
 // Interface
 if SensCase(RdTextS)<>'interface' then begin AppendError('e','Interface reserved word expected (\r found) [R:TParsPas.Parse]'); break; end;

 // Interface/Implementation parser
 BVisi:='h';
 BResultA:=FALSE;

 repeat
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.Parse]'); break; end;

 if (SensCase(BReadS)='initialization') or (BReadS='finalization') or (BReadS='begin') or (BReadS='end') then
  begin
  if BVisi<>'f' then begin AppendError('e','Implementation section is missing [R:TParsPas.Parse]'); break; end;
  BResultA:=TRUE;
  break;
  end;

 if SensCase(BReadS)='implementation' then
  begin
  if BVisi<>'h' then begin AppendError('e','Misplaced \r [R:TParsPas.Parse]'); break; end;
  BVisi:='f';
  end

 // ** Uses section **
 else if SensCase(BReadS)='uses' then
  begin
  BResultB:=FALSE;
  repeat
  BReadS:=RdTextS;
  if IsAllowedName(BReadS)=FALSE then begin AppendError('e','Invalid unit name. (\r is not an allowed unit name) [R:TParsPas.GetUsesOpti]'); break; end;
  if Assigned(FGetUses)=FALSE then begin AppendError('e','Internal error (ZParsPas): FGetUses unassigned [R:TParsPas.GetUsesOpti]'); break; end;
  BErrorA:='';
  BUsesItem:=FGetUses(BReadS,itPasUnit,BErrorA);
  if BUsesItem=nil then
   begin
   if BErrorA='' then BErrorA:='Cannot include uses module \r [R:TParsPas.GetUsesOpti]';
   AppendError('e',BErrorA);
   break;
   end;
  if FModule.AppendUses(BUsesItem)=FALSE then begin AppendError('e','Module \r already exists in the list [R:TParsPas.GetUsesOpti]'); break; end;
  BReadS:=RdTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsPas.GetUsesOpti]'); break; end;
  if BReadS=';' then begin BResultB:=TRUE; break; end;
  if BReadS<>',' then begin AppendError('e','; or , expected, \r found [R:TParsPas.GetUsesOpti]'); break; end;
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
  if BVisi='h' then FModule.PublicNames:=FModule.PublicNames+BItemListS
  else FModule.PrivateNames:=FModule.PrivateNames+BItemListS;
  if SensCase(LuTextS)='external' then
   begin
   RdTextS;
   //BReadS:=RdTextS;
   //if Length(BReadS)<3 then begin AppendError('e','External reference module name expected [R:TParsPas.ParseProcImpl]'); break; end;
   //if (BReadS[1]<>#39) or (BReadS[Length(BReadS)]<>#39) then begin AppendError('e','External reference module name must be in quotes [R:TParsPas.ParseProcImpl]'); end;
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
 else if StrInList(SensCase(BReadS),CMetClass) then
  begin
  BMetClass:=LowerCase(BReadS[1]);
  if BVisi='h' then
   begin
   if StrInList(BReadS,' constructor destructor ') then begin AppendError('e','\r declaration cannot be used in the interface part'); break; end;
   BItemListS:=ParseProcHead(BVisi,BMetClass,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,BProcName,BProcParams);
   FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
   if ObjByName(FModule.ProcList,BProcName)<>nil then AppendError('e','Procedure '+BProcName+' is already declared')
   else
    begin
    BProc:=FModule.AppendProc;
    BProc.Name:=BProcName; BProc.NameL:=BItemListS; BProc.ParListS:=BProcParams;
    end;
   end
  else
   begin
   if StrInList(BReadS,' constructor destructor ') then begin AppendError('e','\r declaration cannot be used for non-class methods'); break; end;
   BItemListS:=ParseProcHead(BVisi,BMetClass,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,BProcName,BProcParams);
   BProc:=TLlvmProc(ObjByName(FModule.ProcList,BProcName));
   if BProc=nil then
    begin
    FModule.PrivateNames:=FModule.PrivateNames+BItemListS+' ';
    BProc:=FModule.AppendProc;
    BProc.Name:=BProcName; BProc.NameL:=BItemListS; BProc.ParListS:=BProcParams;
    end
   else
    begin
    BNameA:=BProc.NameL; BNameB:=BItemListS;
    Delete(BNameA,3,1); Delete(BNameB,3,1);
    if BNameA<>BNameB then AppendError('e','Procedure parameters of '+BProcName+' differ from previous declaration [R:TParsPas.Parse]');
    //if BProc.NameL<>BItemListS then AppendError('e','Procedure parameters of '+BProcName+' differ from previous declaration[R:TParsPas.Parse]');
    end;
   ParseProcImpl(BProc);
   end;
  end

 else // ** Error **
  begin
  AppendError('e','Uses, Type, Var, Const, Procedure or function declaration expected (\r found) [R:TParsPas.Parse]');
  break;
  end;
 until FALSE;

 if BResultA=FALSE then break;

 if SensCase(BReadS)='end' then
  begin
  if RdTextS<>'.' then begin AppendError('e','. expected after the final end [R:TParsPas.Parse]'); break; end;
  if RdTextS<>'' then begin AppendError('e','Extra text after the end of file [R:TParsPas.Parse]'); break; end;
  if IsAllProcImplementedOpti(FModule.ProcList)=FALSE then break;
  FModule.ResolveForwardAll;
  //BResult:=TRUE;
  break;
  end;

 AppendError('e','Unexpected feature \r or feature is not implemented [R:TParsPas.Parse]');
 until TRUE;

 FModule.CompStat.ParsTime:=FModule.CompStat.ParsTime+now-BNow;
 FParsState:=psParsEnd;
End;


end.

