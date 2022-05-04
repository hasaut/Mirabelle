unit ParsCpp_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, AsmTypes_sd, ParsBase_sd, ParsHelper_sd, ParsPrepr_sd;

Const
  CCppResWords  : string =
   (
    ' void unsigned char bool int long float'+
    ' if else'+
    ' while do'+
    ' for' +
    ' switch case'+
    ' va_list va_start va_end va_arg'+
    ' '
   );

Type
  TParsCpp = class(TParsPrepr)
  private
    FRetLabel   : string;

    Function StrToOpcodeDual ( Const AOpcodeS : string ) : string;
    Function ProcessStrConst ( Const AData : string ) : string;
    Procedure ExtractWantedTypeH ( Var ANameS : string; Out AWantedType : string );
    Procedure ExtractWantedTypeD ( Var ANameS : string; Out AWantedType : string );
    Procedure ForceWantedType ( Var ATarg : string; Const AWantedType : string );
    Function ParseStructFields ( Const AParent : string ) : string;
    Procedure TryParseTypeA ( AVisi : char; Const AParent : string; Out ATypeS : string );
    Function ParseDecl ( AProc : TLlvmProc; Const AVarListS : string; AVisi : char; Const ATypeS : string; Const AFilter : string; AEndMarker : string ) : string;
    Procedure ParseProcHead ( Out AProcParams : string );
    Function ParseProcBody ( AProc : TLlvmProc ) : boolean;
    Procedure ParseProcImpl ( AProc : TLlvmProc );
    Function ParseAny : boolean; // In C we do not know in advance if it is a function or a variable
    Function ParseArrayOpti ( AProc : TLlvmProc; Const AVarListS : string; Const ATarg : string ) : string;
    Function ParseFieldOpti ( AProc : TLlvmProc; Const AVarListS : string ) : string;
    Function ParseTargOpti ( AProc : TLlvmProc; Const AVarListS : string; Const AReadS : string ) : string;
    Function AppendCmdIncDecAny ( AList : TStringList; Const ATarg : string; Const AOpcode : string ) : boolean;
    Function ParseEvalVF ( AProc : TLlvmProc; Const AVarListS : string ) : string;
    Function ParseEval ( AProc : TLlvmProc; Const AVarListS : string; Out AIsAsgn : boolean ) : string;
    Function ParseEval ( AProc : TLlvmProc; Const AVarListS : string ) : string;
    Function ParseCallParams ( AProc : TLlvmProc; Const AVarListS : string; Const AParamTypes : string ) : string;
    Procedure ParseAssign ( AProc : TLlvmProc; Const AVarListS : string; Const ATarg : string; Const AEndMarker : string );
    Function ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const AVarListS : string; Const ABreakLabel : string; Const AEndMarker : string ) : boolean;
    Procedure DefMacro ( Var ADefineA, ADefineB : string );
    Function ParseType ( AVisi : char; Const AKnownNames : string ) : string;

    Function IsDiscard ( Const AEval : string; Out ADiscard : string ) : boolean;

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

Constructor TParsCpp.Create;
Begin
 Inherited;
 FOneSymbol:=' == != <= >= ++ -- << >> // /* */ += -= *= /= &= |= ^= <<= >>= && || -> ... ';
 FStrQuotes:='"'+#39;
 FSymbolList:=#32+';.,:-+=/*()[]<>{}!|&~?^@%';
End;

Destructor TParsCpp.Destroy;
Begin
 Inherited;
End;

Function TParsCpp.SensCase ( Const ADataS : string ) : string;
Begin
 Result:=ADataS;
End;

Procedure TParsCpp.RdTextFC ( AJmpNextLine : boolean );
Var
  BLastComment  : char;
Begin
 repeat
 while FCommentLevel<>'' do
  begin
  BLastComment:=FCommentLevel[Length(FCommentLevel)];
  RdTextA(TRUE);
  if FParsOrig='' then break;
  if (FParsOrig='*/') and (BLastComment='*') then Delete(FCommentLevel,Length(FCommentLevel),1);
  end;

 RdTextA(AJmpNextLine);
 if FParsOrig='' then break;
 if FParsOrig='/*' then FCommentLevel:=FCommentLevel+'*'
 else if FParsOrig='//' then
  begin
  SkipLine;
  if AJmpNextLine=FALSE then begin FParsOrig:=''; break; end;
  end
 else break;

 until FALSE;
End;

Procedure TParsCpp.RdTextFD ( AJmpNextLine : boolean );
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
  if PushDefine(FParsOrig,BDefineB)=FALSE then begin FModule.AppendError('e',FParsCursor.FLine,FParsCursor.FPos,'Define loop: '+FParsOrig+' [R:TParsCpp.RdTextSL]'); BReady:=TRUE; end;
  break;
  end;
 // Macro
 if DefPreprocMacro(BDefineA,BDefineB)=FALSE then BReady:=TRUE;
 until TRUE;

 if BReady then break;

 until FALSE;
End;

Function TParsCpp.IsReservedWord ( Const AName : string ) : boolean;
Begin
 Result:=StrInList(AName,CCppResWords);
End;

Function TParsCpp.StrToOpcode ( Const AOpcodeS : string ) : string;
Begin
 Result:='';
 if AOpcodeS='+' then Result:='add'
 else if AOpcodeS='-'   then Result:='sub'
 else if AOpcodeS='*'   then Result:='mul'
 else if AOpcodeS='/'   then Result:='div'
 else if AOpcodeS='%'   then Result:='mod'
 else if AOpcodeS='&'   then Result:='and'
 else if AOpcodeS='|'   then Result:='or'
 else if AOpcodeS='^'   then Result:='xor'
 else if AOpcodeS='<<'  then Result:='shl'
 else if AOpcodeS='>>'  then Result:='shr'
 else if AOpcodeS='&&'  then Result:='and'
 else if AOpcodeS='||'  then Result:='or'
 else if AOpcodeS='=='  then Result:='cmpe'
 else if AOpcodeS='!='  then Result:='cmpne'
 else if AOpcodeS='<'   then Result:='cmpb'
 else if AOpcodeS='>'   then Result:='cmpa'
 else if AOpcodeS='<='  then Result:='cmpbe'
 else if AOpcodeS='>='  then Result:='cmpae';
End;

Function TParsCpp.StrToOpcodeDual ( Const AOpcodeS : string ) : string;
Begin
 Result:='';
 if      AOpcodeS='+='  then Result:='add'
 else if AOpcodeS='-='  then Result:='sub'
 else if AOpcodeS='*='  then Result:='mul'
 else if AOpcodeS='/='  then Result:='div'
 else if AOpcodeS='&='  then Result:='and'
 else if AOpcodeS='|='  then Result:='or'
 else if AOpcodeS='^='  then Result:='xor'
 else if AOpcodeS='<<=' then Result:='shl'
 else if AOpcodeS='>>=' then Result:='shr';
End;

Function TParsCpp.ProcessStrConst ( Const AData : string ) : string;
Var
  BIndex    : Integer;
  BDataC    : char;
Begin
 Result:='';
 BIndex:=0;
 while BIndex<Length(AData) do
  begin
  BDataC:=AData[BIndex+1];
  if (BDataC='\') and ((BIndex+1)<Length(AData)) then
   begin
   inc(BIndex);
   BDataC:=AData[BIndex+1];
   case BDataC of
    'r': BDataC:=#13;
    'n': BDataC:=#10;
   end; // case
   end;
  Result:=Result+BDataC;
  inc(BIndex);
  end;
End;

Procedure TParsCpp.ExtractWantedTypeH ( Var ANameS : string; Out AWantedType : string );
Begin
 AWantedType:='';
 while ANameS<>'' do
  begin
  if ANameS[Length(ANameS)] in ['0'..'9', 'A'..'F', 'a'..'f'] then break;
  AWantedType:=ANameS[Length(ANameS)]+AWantedType;
  Delete(ANameS,Length(ANameS),1);
  end;
End;

Procedure TParsCpp.ExtractWantedTypeD ( Var ANameS : string; Out AWantedType : string );
Begin
 AWantedType:='';
 while ANameS<>'' do
  begin
  if ANameS[Length(ANameS)] in ['0'..'9'] then break;
  AWantedType:=ANameS[Length(ANameS)]+AWantedType;
  Delete(ANameS,Length(ANameS),1);
  end;
End;

Procedure TParsCpp.ForceWantedType ( Var ATarg : string; Const AWantedType : string );
Var
  BType         : string;
  BWantedType   : string;
Begin
 repeat
 if AWantedType='' then break;
 BWantedType:=LowerCase(AWantedType);
 BType:=ParsExtractType(ATarg);
 if StrInList(BType,'b w k m i d') and (BWantedType='l') then
  begin
  ATarg[4]:='i';
  break;
  end;
 if StrInList(BType,'b w k m i d f') and (BWantedType='f') then
  begin
  ATarg[4]:='f';
  break;
  end;
 AppendError('e','Impossible to force type "'+AWantedType+'" to a base constant "'+ParsExtractName(ATarg)+'" [R:TParsCpp.ForceWantedType]');
 until TRUE;
End;

Function TParsCpp.IsTargConstant ( Const ANameS : string ) : string;
Var
  BNameS        : string;
  BDataC        : Int64;
  BDataF        : Extended;
  BLen          : Integer;
  BWantedType   : string;
Begin
 Result:='';

 BNameS:=LowerCase(ANameS);

 repeat
 if BNameS='true' then begin Result:=CBooleanTrue; break; end;
 if BNameS='false' then begin Result:=CBooleanFalse; break; end;
 if BNameS='null' then begin Result:=CPointerNil; break; end;

 BLen:=Length(BNameS);
 if (BLen>=2) and (BNameS[1]='"') and (BNameS[BLen]='"') then
  begin
  BNameS:=ANameS;
  Delete(BNameS,BLen,1); Delete(BNameS,1,1);
  if Length(BNameS)=1 then Result:=CTagS+'c_C'+CTagM+IntToStr(Ord(BNameS[1]))+CTagE
  else Result:=FModule.AppendConst('sz',ProcessStrConst(BNameS));
  break;
  end;

 if (BLen>=2) and (BNameS[1]=#39) and (BNameS[BLen]=#39) then
  begin
  BNameS:=ANameS;
  Delete(BNameS,BLen,1); Delete(BNameS,1,1);
  if Length(BNameS)=1 then Result:=CTagS+'c_C'+CTagM+IntToStr(Ord(BNameS[1]))+CTagE
  else Result:=FModule.AppendConst('sz',ProcessStrConst(BNameS));
  break;
  end;

 if Pos('0x',BNameS)=1 then
  begin
  ExtractWantedTypeH(BNameS,BWantedType);
  Delete(BNameS,1,2); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$FFFFFFFF then break;
   if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('0'))
   else if BNameS[1] in ['a'..'f'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('a')+10)
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  ForceWantedType(Result,BWantedType);
  break;
  end;

 if Pos('$',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  ExtractWantedTypeH(BNameS,BWantedType);
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
  ForceWantedType(Result,BWantedType);
  break;
  end;

 if Pos('%',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  ExtractWantedTypeD(BNameS,BWantedType);
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
  ForceWantedType(Result,BWantedType);
  break;
  end;

 if (Pos('.',BNameS)<>0) and (Pos('..',BNameS)=0) then
  begin
  ExtractWantedTypeD(BNameS,BWantedType);
  if TryStrToFloat(BNameS,BDataF,HParsFormat)=FALSE then break;
  Result:=CTagS+'c_f'+CTagM+BNameS+CTagE;
  ForceWantedType(Result,BWantedType);
  break;
  end;

 if (BNameS<>'') and (BNameS[1] in ['0'..'9']) then ExtractWantedTypeD(BNameS,BWantedType);
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
 ForceWantedType(Result,BWantedType);
 until TRUE;
End;

Function TParsCpp.ParseStructFields ( Const AParent : string ) : string;
Var
  BTypeS        : string;
  BItemListS    : string;
  BField        : string;
Begin
 Result:='';
 repeat
 if RdTextS<>'{' then begin AppendError('e','{ expected, \r found [R:TParsCpp.ParseStructFields]'); SkipText('}'); break; end;
 repeat
 if LuTextS='}' then begin RdTextS; break; end;
 TryParseTypeA('i',AParent,BTypeS);
 if BTypeS='' then begin AppendError('e','Invalid type \l or type is not specified [R:TParsCpp.ParseStructFields]'); break; end;
 BItemListS:=ParseDecl(nil,'','f',BTypeS,Result,';'); if RdTextS<>';' then AppendError('e','";" expected, \r found [R:ParseStructFields.ParseType]');
 while BItemListS<>'' do
  begin
  BField:=ReadParamStr(BItemListS);
  if BField='' then break;
  Result:=Result+BField+' ';
  end;
 until FALSE;
 until TRUE;
End;

Procedure TParsCpp.TryParseTypeA ( AVisi : char; Const AParent : string; Out ATypeS : string ); // AVisi is only used for new structures
Var
  BReadS        : string;
  BTarg         : string;
  BVisibleNames : string;
  BTypeName     : string;
  BFieldList,
  BItemListS    : string;
  BRecSize      : Integer;
Begin
 ATypeS:='';
 BReadS:=LuTextS;
 repeat
 if BReadS='void' then begin RdTextS; ATypeS:='_'; break; end;

 if BReadS='unsigned' then
  begin
  RdTextS;
  BReadS:=LuTextS;
  if BReadS='char' then begin ATypeS:='b'; RdTextS; break; end;
  if BReadS='int' then begin ATypeS:=FModule.BaseTypeU; RdTextS; break; end;
  if BReadS='short' then begin ATypeS:='w'; RdTextS; break; end;
  if BReadS='long' then
   begin
   ATypeS:='d'; RdTextS;
   BReadS:=LuTextS;
   if BReadS='int' then begin ATypeS:='d'; RdTextS; end
   else if BReadS='long' then
    begin
    ATypeS:='q'; RdTextS;
    BReadS:=LuTextS; if BReadS='int' then RdTextS;
    end;
   break;
   end;
  ATypeS:=FModule.BaseTypeU;
  break;
  end;
 if BReadS='signed' then
  begin
  RdTextS;
  BReadS:=LuTextS;
  if BReadS='char' then begin ATypeS:='k'; RdTextS; break; end;
  if BReadS='int' then begin ATypeS:=FModule.BaseTypeI; RdTextS; break; end;
  if BReadS='short' then begin ATypeS:='m'; RdTextS; break; end;
  if BReadS='long' then
   begin
   ATypeS:='i'; RdTextS;
   BReadS:=LuTextS;
   if BReadS='int' then begin ATypeS:='i'; RdTextS; end
   else if BReadS='long' then
    begin
    ATypeS:='g'; RdTextS;
    BReadS:=LuTextS; if BReadS='int' then RdTextS;
    end;
   break;
   end;
  AppendError('e','Signed type must be specified [R:TParsCpp.TryParseTypeA]');
  break;
  end;
 if BReadS='char' then begin ATypeS:='k'; RdTextS; break; end;
 if BReadS='int' then begin ATypeS:=FModule.BaseTypeI; RdTextS; break; end;
 if BReadS='short' then begin ATypeS:='m'; RdTextS; break; end;
 if BReadS='long' then
  begin
  ATypeS:='i'; RdTextS;
  BReadS:=LuTextS;
  if BReadS='int' then begin ATypeS:='i'; RdTextS; end
  else if BReadS='long' then
   begin
   ATypeS:='g'; RdTextS;
   BReadS:=LuTextS; if BReadS='int' then RdTextS;
   end;
  break;
  end;
 if BReadS='bool' then begin ATypeS:='l'; RdTextS; break; end;
 if BReadS='float' then begin ATypeS:='f'; RdTextS; break; end;
 if BReadS='va_list' then begin ATypeS:='p'; RdTextS; break; end;

 if BReadS='struct' then
  begin
  RdTextS;
  BTypeName:=RdTextS;
  if IsAllowedName(BTypeName)=FALSE then AppendError('e','Invalid name for a structure \r [R:TParsCpp.TryParseTypeA]');
  BTarg:='';
  BVisibleNames:=FModule.PrivateNames+FModule.PublicNames+FModule.ExternNames+AParent;
  repeat
  BTarg:=ReadParamStr(BVisibleNames);
  if BTarg='' then break;
  if SensCase(ParsExtractName(BTarg))=SensCase(BTypeName) then break;
  until FALSE;
  if BTarg='' then // This is a new structure
   begin
   BFieldList:=ParseStructFields(AParent+CTagS+'t'+AVisi+'r'+IntToStr(0)+'e{"'+BTypeName+'"}'+CTagM+BTypeName+CTagE+' ');
   FModule.OrderRecordFields(BFieldList,BItemListS,BRecSize);
   ATypeS:=CTagS+'t'+AVisi+'r'+IntToStr(BRecSize)+'e{'+BItemListS+'}'+CTagM+BTypeName+CTagE;
   case AVisi of
    'f': FModule.PrivateNames:=FModule.PrivateNames+ATypeS+' ';
    'h': FModule.PublicNames:=FModule.PublicNames+ATypeS+' ';
   end;
   break;
   end;
  if ParsIsType(BTarg) then begin ATypeS:=ParsExtractType(BTarg); break; end;
  AppendError('e','Identifier \r is already declared [R:TParsCpp.TryParseTypeA]');
  break;
  end;

 BTarg:='';
 BVisibleNames:=FModule.PrivateNames+FModule.PublicNames+FModule.ExternNames;
 repeat
 BTarg:=ReadParamStr(BVisibleNames);
 if BTarg='' then break;
 if ParsIsType(BTarg) and (SensCase(ParsExtractName(BTarg))=SensCase(BReadS)) then break;
 until FALSE;
 if BTarg<>'' then begin RdTextS; ATypeS:=ParsExtractType(BTarg); break; end;

 until TRUE;

End;

Function TParsCpp.ParseDecl ( AProc : TLlvmProc; Const AVarListS : string; AVisi : char; Const ATypeS : string; Const AFilter : string; AEndMarker : string ) : string;
Var
  BReadS        : string;
  BVarName      : string;
  BIsPtr        : string;
  BTarg         : string;
  BArrayDim     : Integer;
Begin
 Result:='';

 repeat
 BIsPtr:='';
 while LuTextS='*' do begin RdTextS; BIsPtr:=BIsPtr+'p'; end;
 BVarName:=RdTextS;
 if IsReservedWord(BVarName) then AppendError('e','Reserved word \r cannot be used as a variable name [R:TParsCpp.ParseDecl]');
 if IsAllowedName(BVarName)=FALSE then AppendError('e','\r cannot be used as a variable name [R:TParsCpp.ParseDecl]');
 if Pos(CTagM+BVarName+CTagE,Result)<>0 then AppendError('e','Identifier \r has just been declared [R:TParsCpp.ParseDecl]');
 BReadS:=LuTextS;
 if (Result='') and (BReadS='(') then // Declaration is a procedure or function
  begin
  Result:=CTagS+'x'+AVisi+BIsPtr+ATypeS+CTagM+BVarName+CTagE+' ';
  break; // Only one declaration is allowed (comma-separated function declaration is not allowed)
  end;
 if BReadS='(' then
  begin
  AppendError('e','Declaration of function is not allowed here. Use separate line [R:TParsCpp.ParseDecl]');
  RdTextS;
  SkipText(')');
  end;

 if Pos(CTagM+BVarName+CTagE,AFilter)<>0 then AppendError('e','Identifier \r is already declared [R:TParsCpp.ParseDecl]');
 while BReadS='[' do
  begin
  RdTextS;
  BReadS:=LuTextS;
  if BReadS='' then break;
  if BReadS=']' then begin RdTextS; BIsPtr:=BIsPtr+'p'; end
  else
   begin
   RdConstI(BArrayDim);
   BIsPtr:=BIsPtr+'a0s'+IntToStr(BArrayDim-1)+'e';
   BReadS:=RdTextS;
   if BReadS<>']' then AppendError('e','"]" expected, \r found [R:TParsCpp.ParseDecl]');
   end;
  BReadS:=LuTextS;
  if BReadS='' then break;
  end;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseDecl]'); break; end;

 BTarg:=CTagS+'d'+AVisi+BIsPtr+ATypeS+CTagM+BVarName+CTagE;
 Result:=Result+BTarg+' ';
 if BReadS='=' then
  begin
  RdTextS;
  if AProc=nil then AppendError('e','Initialization is not allowed here [R:TParsCpp.ParseDecl]')
  else ParseAssign(AProc,AVarListS,BTarg,'; ,');
  end;
 BReadS:=LuTextS;
 if StrInList(BReadS,AEndMarker) then break;
 if BReadS<>',' then begin AppendError('e','";" or "," or ")" expected, \r found [R:TParsCpp.ParseDecl]'); SkipText(';'); break; end;
 RdTextS;
 until FALSE;

End;

Procedure TParsCpp.ParseProcHead ( Out AProcParams : string );
Var
  BReadS        : string;
  BTypeS        : string;
  BVarName,
  BVarList      : string;
  BVarVisi      : char;
Begin
 AProcParams:='';

 repeat
 BReadS:=RdTextS;
 if BReadS<>'(' then begin AppendError('e','"(" expected, \r found [R:TParsCpp.ParseProcHead]'); SkipText(',)'); break; end;

 if LuTextS=')' then RdTextS
 else
  begin
  BVarList:='';
  repeat
  BReadS:=LuTextS;
  if BReadS='...' then
   begin
   RdTextS;
   BVarList:=BVarList+CTagParamCnt+' ';
   BReadS:=RdTextS;
   if BReadS<>')' then begin AppendError('e','")" expected, \r found [R:TParsCpp.ParseProcHead]'); break; end;
   break;
   end;
  BVarVisi:='a';
  if SensCase(BReadS)='const' then begin BVarVisi:='c'; RdTextS; end;
  TryParseTypeA('i','',BTypeS);
  if BTypeS='' then AppendError('e','Invalid type or type is not specified [R:TParsCpp.ParseProcHead]');
  if (BVarList='') and (BTypeS='_') and (LuTextS=')') then begin RdTextS; break; end;
  BVarName:=LuTextS;
  if (BVarName=',') or (BVarName=')') then BVarName:=CNullParamName
  else BVarName:=ParseDecl(nil,'',BVarVisi,BTypeS,BVarList,'; ) ,');
  if ParsIsArray(BVarName) and (BVarVisi='a') then BVarName[3]:='v';
  BVarList:=BVarList+BVarName;
  BReadS:=RdTextS;
  if BReadS=')' then break;
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseProcHead]'); break; end;
  if BReadS<>',' then begin AppendError('e','"," or ")" expected, \r found [R:TParsCpp.ParseProcHead]'); break; end;
  until FALSE;
  end;

 AProcParams:=BVarList;
 until TRUE;
End;

Function TParsCpp.ParseProcBody ( AProc : TLlvmProc ) : boolean;
Var
  BReadS        : string;
Begin
 Result:=FALSE;

 AProc.StartLine:=FRdQueue[0].Line; AProc.StartPos:=FRdQueue[0].Pos;

 repeat
 FRetLabel:=AProc.LabelName('Ret',0,0);
 if ParseBlock(AProc,0,'','',' } ')=FALSE then break;
 AProc.FlowList.Append(FRetLabel+':'+GenTail);
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
 if BReadS<>'}' then begin AppendError('e','Internal error: end expected, \r found'); break; end;
 AProc.EndLine:=FRdQueue[0].Line; AProc.EndPos:=FRdQueue[0].Pos;
 Result:=TRUE;
 until TRUE;
End;

Procedure TParsCpp.ParseProcImpl ( AProc : TLlvmProc );
Var
  BType         : string;
  //BReadS        : string;
Begin
 repeat
 BType:=ParsExtractRetType(AProc.NameL);
 if (BType<>'') and (BType<>'_') then AProc.RetListS:=AProc.RetListS+CTagS+'dr'+BType+CTagM+'Result'+CTagE+' ';
 if LuTextS='external' then
  begin
  RdTextS;
  //BReadS:=RdTextS;
  //if Length(BReadS)<3 then begin AppendError('e','External reference module name expected [R:TParsCpp.ParseProcImpl]'); break; end;
  //if (BReadS[1]<>#39) or (BReadS[Length(BReadS)]<>#39) then begin AppendError('e','External reference module name must be in quotes [R:TParsCpp.ParseProcImpl]'); end;
  //AProc.ExtRefName:=Copy(BReadS,2,Length(BReadS)-2);
  //FModule.AppendExtLoc(AProc.NameL);
  if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsCpp.ParseProcImpl]'); break; end;
  end
 else
  begin
  if ParseProcBody(AProc)=FALSE then break;
  end;

 AProc.IsImplemented:=TRUE;
 until TRUE;
End;

Function TParsCpp.ParseArrayOpti ( AProc : TLlvmProc; Const AVarListS : string; Const ATarg : string ) : string;
Var
  BReadS        : string;
  BTarg         : string;
Begin
 Result:='';
 if ParsIsArray(ATarg) or ParsIsPointer(ATarg) or ParsIsStringZ(ATarg) then
 else AppendError('e','\r is not an array and cannot be indexed [R:TParsCpp.ParseArrayOpti]');
 RdTextS;
 Result:=Result+'[';
 repeat
 BTarg:=ParseEval(AProc,AVarListS); if BTarg='' then BTarg:=CInvalidIdentifier;
 Result:=Result+BTarg;
 BReadS:=RdTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseArrayOpti]'); break; end;
 if BReadS=',' then
 else if BReadS=']' then
  begin
  BReadS:=LuTextS;
  if BReadS<>'[' then break;
  RdTextS;
  end
 else begin AppendError('e','"]" or "," expected, \r found [R:TParsCpp.ParseArrayOpti]'); break; end;
 Result:=Result+',';
 until FALSE;
 Result:=Result+']';
End;

Function TParsCpp.ParseFieldOpti ( AProc : TLlvmProc; Const AVarListS : string ) : string;
Var
  BReadS        : string;
  BTarg         : string;
  BField        : string;
Begin
 Result:='';
 RdTextS;
 BField:=RdTextS;
 Result:=Result+'.'+BField;
 repeat
 BReadS:=LuTextS;
 if BReadS='.' then
  begin
  RdTextS;
  BField:=RdTextS;
  Result:=Result+'.'+BField;
  end
 else if BReadS='[' then
  begin
  RdTextS;
  BTarg:=ParseEval(AProc,AVarListS); if BTarg='' then BTarg:=CInvalidIdentifier;
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

Function TParsCpp.ParseTargOpti ( AProc : TLlvmProc; Const AVarListS : string; Const AReadS : string ) : string;
Begin
 Result:='';
 if AReadS='' then begin AppendError('e','Unexpected end of file [R:ParsCpp.ParseTarg]'); Result:=CInvalidIdentifier; end;
 if IsReservedWord(AReadS) then begin AppendError('e','Reserved word \r cannot be used as an identifier [R:ParsCpp.ParseTarg]'); Result:=CInvalidIdentifier; end;
 if IsSymbol(AReadS[1]) then begin AppendError('e','Misplaced symbol \r [R:ParsCpp.ParseTarg]'); Result:=CInvalidIdentifier; end;
 repeat
 Result:=ParsSearchTarg(AVarListS,AReadS); if Result<>'' then break;
 Result:=GetTarg(AProc,AReadS);            if Result<>'' then break;
 AppendError('e','Identifier \r not found [R:ParsCpp.ParseTarg]');
 Result:=CInvalidIdentifier;
 until TRUE;
End;

Function TParsCpp.AppendCmdIncDecAny ( AList : TStringList; Const ATarg : string; Const AOpcode : string ) : boolean;
Begin
 if AOpcode='++' then AList.Append(CDiscardValue+' := '+CDirectiveInc+'('+ATarg+CTagP+')'+GenTail)
 else AList.Append(CDiscardValue+' := '+CDirectiveDec+'('+ATarg+CTagP+')'+GenTail);
 Result:=TRUE;
End;

Function TParsCpp.ParseEval ( AProc : TLlvmProc; Const AVarListS : string ) : string;
Var
  BHasAssignment    : boolean;
Begin
 Result:=ParseEval(AProc,AVarListS,BHasAssignment); // In most cases caller does not care if there were final assignment or not
End;

Function TParsCpp.ParseEvalVF ( AProc : TLlvmProc; Const AVarListS : string ) : string;
Var
  BReadS        : string;
  BOpcode       : string;
  BTarg,
  BEval         : string;
  BIsNot        : boolean;
  BTypeS        : string;
  BIsPtr        : string;
  BRetType,
  BParamTypes   : string;
  BConstS       : string;
  BTargA,
  BTargB        : string;
  BSubEval      : string;
Begin
 // Parse only Variables/Functions, not operations
 Result:='';

 repeat
 BReadS:=LuTextS;
 if BReadS='*' then
  begin
  RdTextS;
  BTarg:=ParseEvalVF(AProc,AVarListS); if BTarg='' then BTarg:=CInvalidIdentifier;
  if Pos (#32,BTarg)<>0 then BTarg:='('+BTarg+')';
  BTarg:=BTarg+'['+CConstZero+']';
  Result:=Result+BTarg;
  end
 else if BReadS='&' then
  begin
  RdTextS;
  { old code. Replace with "ParseEvalVF" (below)
  BReadS:=LuTextS;
  BTarg:='';
  repeat
  BTarg:=ParsSearchTarg(AVarListS,BReadS); if BTarg<>'' then break;
  BTarg:=GetTarg(AProc,BReadS);            if BTarg<>'' then break;
  until TRUE;
  if BTarg='' then BTarg:=ParseEval(AProc,AVarListS)
  else RdTextS;}
  BTarg:=ParseEvalVF(AProc,AVarListS); if BTarg='' then BTarg:=CInvalidIdentifier;
  if Pos (#32,BTarg)<>0 then BTarg:='('+BTarg+')';
  Result:=Result+'@'+BTarg;
  end
 else if BReadS='(' then
  begin
  RdTextS;
  TryParseTypeA('i','',BTypeS);
  if BTypeS<>'' then
   begin
   BIsPtr:='';
   while LuTextS='*' do begin RdTextS; BIsPtr:=BIsPtr+'p'; end;
   BReadS:=RdTextS;
   if BReadS<>')' then AppendError('e','")" expected, \r found [R:ParsCpp.ParseEvalVF]');
   BTarg:=CTagS+'t'+'b'+BIsPtr+BTypeS+CTagM+CTagE;
   Result:=BTarg+' type '+ParseEval(AProc,AVarListS);
   end
  else
   begin
   BTarg:=ParseEval(AProc,AVarListS); if BTarg='' then BTarg:=CInvalidIdentifier;
   BReadS:=RdTextS;
   if (BReadS='++') or (BReadS='--') then //
    begin
    //AppendCmdIncDecAny(AProc.PostList,BTarg,BReadS);
    AppendError('e','Internal error: ++/-- must be processed inside child ParseEval [R:ParsCpp.ParseEvalVF]');
    BReadS:=RdTextS;
    end;
   if BReadS<>')' then AppendError('e','")" expected, \r found [R:ParsCpp.ParseEvalVF]');
   if ParsCheckCallPtr(BTarg,BRetType,BParamTypes) and (LuTextS='(') then
    begin
    BEval:=ParseCallParams(AProc,AVarListS,BParamTypes);
    Result:='('+BTarg+')('+BEval+')';
    end
   else
    begin
    Result:=Result+'('+BTarg+')';
    end;
   end;
  end
 else if BReadS='{' then
  begin
  RdTextS;
  Result:=Result+'<';
  BSubEval:='';
  repeat
  BReadS:=LuTextS;
  if BReadS='...' then begin BTarg:='...'; RdTextS; end
  else BTarg:=ParseEval(AProc,''); if BTarg='' then break;
  if BSubEval<>'' then BSubEval:=BSubEval+',';
  BSubEval:=BSubEval+BTarg;
  BReadS:=RdTextS;
  if BReadS='}' then break;
  if BReadS<>',' then begin AppendError('e','"," or "]" expected, \r found [R:TParsPas.ParseEval]'); break; end;
  until FALSE;
  Result:=Result+BSubEval+'>';
  end
 else if BReadS='-' then
  begin
  Result:=Result+CConstZero;
  end
 else if (BReadS='++') or (BReadS='--') then // Pre-inc / Pre-dec
  begin
  RdTextS;
  BOpcode:=BReadS;
  BReadS:=RdTextS;
  BTarg:=ParseTargOpti(AProc,AVarListS,BReadS);
  if ParsIsProc(BTarg) then begin AppendError('e','Procedure cannot be used here [R:ParsCpp.ParseEvalVF]'); break; end;
  AppendCmdIncDecAny(AProc.FlowList,BTarg,BOpcode);
  Result:=Result+BTarg;
  end
 else if BReadS='va_arg' then
  begin
  RdTextS;
  if RdTextS<>'(' then AppendError('e','"(" expected \r found [R:ParsCpp.ParseEvalVF]');
  BReadS:=RdTextS;
  BTarg:=ParseTargOpti(AProc,AVarListS,BReadS);
  if RdTextS<>',' then AppendError('e','"," expected \r found [R:ParsCpp.ParseEvalVF]');
  TryParseTypeA('i','',BTypeS);
  if BTypeS='' then begin AppendError('e','Invalid type [R:ParsCpp.ParseEvalVF]'); break; end;
  if RdTextS<>')' then AppendError('e','")" expected \r found [R:ParsCpp.ParseEvalVF]');
  if ParsIsPointer(BTarg)=FALSE then AppendError('e','First argument must be of pointer type [R:ParsCpp.ParseEvalVF]');
  if ParsIsTypeBasic(BTypeS) or ParsIsTypePointer(BTypeS) then
  else AppendError('e','Type must be basic [R:ParsCpp.ParseEvalVF]');
  BTargA:=AProc.AppendTmpVar('p'+BTypeS);
  AProc.Flowlist.Append(BTargA+' := '+BTarg+GenTail);
  BConstS:=CTagS+'c_p'+CTagM+IntToStr(AProc.Module.GetTypeSize(AProc.Module.StackGenericType))+CTagE;
  BTargB:=AProc.AppendTmpVar(BTypeS);
  AProc.Flowlist.Append(BTargB+' := '+BTargA+'['+CConstZero+']'+GenTail);
  AProc.Flowlist.Append(BTarg+' := '+BTarg+' add '+BConstS+GenTail);
  Result:=Result+BTargB;
  end
 else // The most common case, parse Targ
  begin
  BIsNot:=FALSE;
  repeat
  if LowerCase(BReadS)<>'~' then break;
  BIsNot:=not BIsNot;
  RdTextS;
  BReadS:=LuTextS;
  until FALSE;
  RdTextS;
  if BIsNot then Result:=Result+' not ';
  BTarg:=ParseTargOpti(AProc,AVarListS,BReadS);
  if ParsIsProc(BTarg) then begin BEval:=ParseCallParams(AProc,AVarListS,ParsExtractParamTypes(BTarg)); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')'; end;
  Result:=Result+BTarg;
  end;
  // '[' or '.'
  BReadS:=LuTextS;
  if BReadS='[' then
   begin
   Result:=Result+ParseArrayOpti(AProc,AVarListS,BTarg);
   end;
  if BReadS='.' then
   begin
   Result:=Result+ParseFieldOpti(AProc,AVarListS);
   end;
 until TRUE;
End;

Function TParsCpp.ParseEval ( AProc : TLlvmProc; Const AVarListS : string; Out AIsAsgn : boolean ) : string;
Var
  BReadS        : string;
  BOpcode       : string;
  BTarg,
  BEval         : string;
  BOpcodeS      : string;
Begin
 Result:='';
 AIsAsgn:=FALSE;

 repeat
 // Variable/Function
 BTarg:=ParseEvalVF(AProc,AVarListS);
 Result:=Result+BTarg;
 // Operation
 BReadS:=LuTextS;
 // += -= &= etc
 BOpcodeS:=StrToOpcodeDual(BReadS);
 if BOpcodeS<>'' then
  begin
  RdTextS;
  BEval:=ParseEval(AProc,AVarListS); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseEval]'); BEval:=CInvalidIdentifier; end;
  AProc.Flowlist.Append(Result+' := '+Result+' '+BOpcodeS+' '+BEval+GenTail);
  BReadS:=LuTextS;
  AIsAsgn:=TRUE;
  end
 else if (BReadS='++') or (BReadS='--') then
  begin
  RdTextS;
  AppendCmdIncDecAny(AProc.PostList,Result,BReadS);
  BReadS:=LuTextS;
  AIsAsgn:=TRUE;
  end;
 if (BReadS=';') or (BReadS=')') or (BReadS=']') or (BReadS=',') or (BReadS='=') or (BReadS=':') or IsReservedWord(BReadS) then break;
 //if ((Length(BReadS)=1) and IsSymbol(BReadS[1])) or IsReservedWord(BReadS) then break;
 RdTextS;
 BOpcode:=StrToOpcode(BReadS);
 if BOpcode='' then begin AppendError('e','Operation code expected, \r found [R:ParsCpp.ParseEval]'); BReadS:=CInvalidOpcode; end;
 Result:=Result+' '+BOpcode+' ';
 AIsAsgn:=FALSE; // Only the final (top-level) assignment is taken in account
 until FALSE;
End;

Function TParsCpp.ParseCallParams ( AProc : TLlvmProc; Const AVarListS : string; Const AParamTypes : string ) : string;
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
  if BParams<>'' then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsCpp.ParseCallParams]');
  break;
  end;
 RdTextS;
 BReadS:=LuTextS;

  repeat
  DelFirstSpace(BParams);
  BTypeS:=ReadParamStr(BParams);
  if BTypeS='da*' then // Variable param list (like printf)
   begin
   Result:=Result+CTagParamCnt+CTagP;
   repeat
   if BReadS=')' then break;
   BEval:=ParseEval(AProc,AVarListS);
   Result:=Result+BEval+CTagP;
   BReadS:=LuTextS;
   if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseCallParams]'); break; end;
   if BReadS=',' then RdTextS
   else if BReadS<>')' then begin RdTextS; AppendError('e',', or ) expected, \r found [R:TParsCpp.ParseCallParams]'); break; end;
   until FALSE;
   break;
   end;
  if BReadS=')' then BEval:='' else BEval:=ParseEval(AProc,AVarListS);
  if (BEval='') and (BTypeS='') then break;
  if (BEval='') and (BTypeS<>'') then AppendError('e','Not enough parameters passed to the procedure/function [R:TParsCpp.ParseCallParams]');
  if (BEval<>'') and (BTypeS='') then AppendError('e','Extra parameter in call of procedure/function [R:TParsCpp.ParseCallParams]');
  if (BEval<>'') and (BTypeS<>'') then Result:=Result+BEval+CTagP;
  if BEval<>'' then
   begin
   BReadS:=LuTextS;
   if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseCallParams]'); break; end;
   if BReadS=',' then RdTextS
   else if BReadS<>')' then begin RdTextS; AppendError('e',', or ) expected, \r found [R:TParsCpp.ParseCallParams]'); end;
   end;
  until FALSE;

 BReadS:=LuTextS;
 if BReadS<>')' then AppendError('e',') expected, \r found [R:TParsCpp.ParseCallParams]')
 else RdTextS;
 until TRUE;
End;

Procedure TParsCpp.ParseAssign ( AProc : TLlvmProc; Const AVarListS : string; Const ATarg : string; Const AEndMarker : string );
Var
  BEval         : string;
  BReadS        : string;
Begin
 repeat
 BEval:=ParseEval(AProc,AVarListS); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseAssign]'); BEval:=CInvalidIdentifier; end;
 if ParsIsStringZ(BEval) and ParsIsArray(ATarg) then AppendError('e','Use strcpy function to copy strings in C [R:TParsCpp.ParseAssign]');
 if ParsIsStringZ(BEval) and (ParsIsPointer(ATarg)=FALSE) then AppendError('e','Left side of assignment is not a pointer [R:TParsCpp.ParseAssign]');
 AProc.Flowlist.Append(ATarg+' := '+BEval+GenTail);
 AProc.FlushPostList;
 BReadS:=LuTextS;
 if StrInList(BReadS,AEndMarker) then break;
 RdTextS;
 if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsCpp.ParseAssign]'); break; end;
 until TRUE;
End;

Function TParsCpp.IsDiscard ( Const AEval : string; Out ADiscard : string ) : boolean;
Var
  BEval         : string;
Begin
 Result:=FALSE; ADiscard:='';
 repeat
 if Pos(CDiscardStrSearch,AEval)<>1 then break;
 BEval:=AEval; Delete(BEval,1,Length(CDiscardStrSearch));
 DelFirstSpace(BEval); DelLastSpace(BEval);
 if BEval='' then break;
 if Pos(#32,BEval)<>0 then break;
 ADiscard:=BEval;
 Result:=TRUE;
 until TRUE;
End;

Function TParsCpp.ParseBlock ( AProc : TLlvmProc; ALevel : Integer; Const AVarListS : string; Const ABreakLabel : string; Const AEndMarker : string ) : boolean;
Var
  BVarListS     : string;
  BReadS        : string;
  BTarg,
  BTargA,
  BEval         : string;
  BResult,
  BResultA      : boolean;
  BLabelIndex   : Integer;
  BLStart,
  BLCmp,
  BLEnd,
  BLThen,
  BLElse        : string;
  BCaseIdx      : Integer;
  BLCmpThis,
  BLCmpNext,
  BLThenThis,
  BLThenNext    : string;
  BTypeS        : string;
  BItemListS    : string;
  BFor3S,
  BFor3E        : Integer;
  BLine         : string;
  BDiscard      : string;
  BConstS       : string;
  BTypeSize     : Integer;
  BIsEvalAsgn   : boolean;
Begin
 Result:=FALSE;
 BLabelIndex:=0;
 BVarListS:=AVarListS;

 {if ParsExtractName(AProc.NameL)='_printn' then
  begin
  Sleep(0);
  end;}

 repeat
 BReadS:=LuTextS;
 if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseBlock]'); break; end;
 if StrInList(BReadS,AEndMarker) then begin Result:=TRUE; break; end;
 BResult:=FALSE;

 repeat // Error block
 if BReadS='const' then
  begin
  RdTextS;
  TryParseTypeA('i','',BTypeS);
  if BTypeS='' then begin AppendError('e','Identifier declaration required [R:TParsCpp.ParseBlock]'); SkipText(';'); break; end;
  BItemListS:=ParseDecl(AProc,BVarListS,'b',BTypeS,AProc.ParListS+AProc.VarListS,';');
  if RdTextS<>';' then AppendError('e','";" expected, \r found [R:TParsCpp.ParseBlock]');
  if ParsIsProc(BItemListS) then begin AppendError('e','Function declaration is not allowed here [R:TParsCpp.ParseBlock]'); SkipText(')'); end;
  ParsAppendVarListS(BVarListS,BItemListS);
  AProc.AppendVarListS(BItemListS);
  BResult:=TRUE;
  break;
  end;

 TryParseTypeA('i','',BTypeS);
 if BTypeS<>'' then
  begin
  BItemListS:=ParseDecl(AProc,BVarListS,'b',BTypeS,AProc.ParListS+AProc.VarListS,';');
  if RdTextS<>';' then AppendError('e','";" expected, \r found [R:TParsCpp.ParseBlock]');
  if ParsIsProc(BItemListS) then begin AppendError('e','Function declaration is not allowed here [R:TParsCpp.ParseBlock]'); SkipText(')'); end;
  ParsAppendVarListS(BVarListS,BItemListS);
  AProc.AppendVarListS(BItemListS);
  BResult:=TRUE;
  break;
  end;

 //BReadS:=LuTextS;

 //if IsSymbol(BReadS[1]) then begin RdTextS; AppendError('e','Misplaced symbol \r [R:TParsCpp.ParseBlock]'); break; end;

 if BReadS='{' then // Block inside another block
  begin
  RdTextS;
  if ParseBlock(AProc,ALevel+1,BVarListS,ABreakLabel,' } ')=FALSE then break;
  if RdTextS<>'}' then begin AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 if BReadS='break' then
  begin
  RdTextS;
  if ABreakLabel='' then begin AppendError('e','Misplaced break [R:TParsCpp.ParseBlock]'); break; end;
  AProc.FlowList.Append('jmp '+ABreakLabel+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsCpp.ParseBlock]');
  break;
  end;

 if BReadS='return' then
  begin
  RdTextS;
  if LuTextS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseBlock]'); break; end
  else if LuTextS=';' then
   begin
   if AProc.RetListS<>'' then AppendError('e','Return value must be specified [R:TParsCpp.ParseBlock]');
   end
  else
   begin
   if AProc.RetListS='' then AppendError('e','This function cannot return any value (or missing ";") [R:TParsCpp.ParseBlock]');
   BEval:=ParseEval(AProc,BVarListS);
   if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
   BTarg:=AProc.RetListS; DelLastSpace(BTarg);
   if BTarg<>'' then AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);
   AProc.FlushPostList;
   end;
  AProc.FlowList.Append('jmp '+FRetLabel+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsCpp.ParseBlock]');
  break;
  end;

 // ** IF **
 if BReadS='if' then
  begin
  RdTextS;
  BLThen:=AProc.LabelName('IfThen',ALevel,BLabelIndex);
  BLElse:=AProc.LabelName('IfElse',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('IfEnd',ALevel,BLabelIndex);
  if RdTextS<>'(' then begin AppendError('e','"(" expected \r fond [R:TParsCpp.ParseBlock]'); SkipText(')'); end
  else
   begin
   BEval:=ParseEval(AProc,BVarListS);
   if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
   AProc.FlushPostList;
   AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLElse+GenTail);
   if LuTextS<>')' then AppendError('e','")" expected, "\l" found [R:TParsCpp.ParseBlock]')
   else RdTextS;
   end;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='{' then // Then with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,BVarListS,ABreakLabel,' } ')=FALSE then break;
   if RdTextS<>'}' then begin AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
   end
  else // Then without begin
   begin
   if ParseBlock(AProc,ALevel+1,BVarListS,ABreakLabel,'')=FALSE then break;
   end;
  AProc.FlowList.Append('jmp '+BLEnd+GenTail);
  BReadS:=LuTextS;
  AProc.FlowList.Append(BLElse+':'+GenTail);
  if BReadS='else' then
   begin
   RdTextS;
   if LuTextS='{' then // else with begin
    begin
    RdTextS;
    if ParseBlock(AProc,ALevel+1,BVarListS,ABreakLabel,' } ')=FALSE then break;
    if RdTextS<>'}' then begin AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
    end
   else // else without begin
    begin
    if ParseBlock(AProc,ALevel+1,BVarListS,ABreakLabel,'')=FALSE then break;
    end;
   end;
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** IF **

 if BReadS='while' then
  begin
  RdTextS;
  BLCmp:=AProc.LabelName('WhileCmp',ALevel,BLabelIndex);
  BLThen:=AProc.LabelName('WhileThen',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('WhileEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLCmp+':'+GenTail);
  if RdTextS<>'(' then begin AppendError('e','"(" expected \r fond [R:TParsCpp.ParseBlock]'); SkipText(')'); end
  else
   begin
   BEval:=ParseEval(AProc,BVarListS);
   if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
   AProc.FlushPostList;
   AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLEnd+GenTail);
   if LuTextS<>')' then AppendError('e',') expected, \l found [R:TParsCpp.ParseBlock]')
   else RdTextS;
   end;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='{' then // while with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' } ')=FALSE then break;
   if RdTextS<>'}' then AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]');
   end
  else // while without begin
   begin
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,'')=FALSE then break;
   end;
  // JMP to the beginning
  AProc.FlowList.Append('jmp '+BLCmp+GenTail);
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** WHILE **  }

 if BReadS='do' then
  begin
  RdTextS;
  BLStart:=AProc.LabelName('RepeatStart',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('RepeatEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLStart+':'+GenTail);
  if RdTextS<>'{' then begin AppendError('e','{ expected \r found [R:TParsCpp.ParseBlock]'); SkipText('}'); end
  else
   begin
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' } ')=FALSE then break;
   if RdTextS<>'}' then AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]');
   end;
  if RdTextS<>'while' then AppendError('e','Internal error: while expected, \r found [R:TParsCpp.ParseBlock]');
  if RdTextS<>'(' then begin AppendError('e','"(" expected \r fond [R:TParsCpp.ParseBlock]'); SkipText(')'); end
  else
   begin
   BEval:=ParseEval(AProc,BVarListS);
   if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
   AProc.FlushPostList;
   AProc.FlowList.Append('if '+BEval+' '+BLStart+' '+BLEnd+GenTail);
   if LuTextS<>')' then AppendError('e',') expected, \l found [R:TParsCpp.ParseBlock]')
   else RdTextS;
   end;
  // End label
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS=';' then begin BResult:=TRUE; break; end;
  AppendError('e','; expected, \r found [R:TParsCpp.ParseBlock]');
  break;
  end; // ** REPEAT **  }

 if BReadS='for' then
  begin
  RdTextS;
  BLCmp:=AProc.LabelName('ForCmp',ALevel,BLabelIndex);
  BLThen:=AProc.LabelName('ForThen',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('ForEnd',ALevel,BLabelIndex);
  BFor3S:=AProc.FlowList.Count; BFor3E:=AProc.FlowList.Count;
  if RdTextS<>'(' then begin AppendError('e','"(" expected \r fond [R:TParsCpp.ParseBlock]'); SkipText(')'); end
  else
   begin
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' ; ')=FALSE then break;
   if RdTextS<>';' then AppendError('e','Internal error: ; expected, \r found [R:TParsCpp.ParseBlock]');
   AProc.FlowList.Append(BLCmp+':'+GenTail);
   if LuTextS=';' then
   else
    begin
    BEval:=ParseEval(AProc,BVarListS);
    AProc.FlushPostList;
    if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
    AProc.FlowList.Append('if '+BEval+' '+BLThen+' '+BLEnd+GenTail);
    end;
   if RdTextS<>';' then AppendError('e','; expected, \r found [R:TParsCpp.ParseBlock]');
   if LuTextS=')' then RdTextS
   else
    begin
    BFor3S:=AProc.FlowList.Count;
    if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' ; ) ')=FALSE then break;
    if RdTextS<>')' then AppendError('e',') expected, \r found [R:TParsCpp.ParseBlock]');
    BFor3E:=AProc.FlowList.Count;
    end;
   end;
  AProc.FlowList.Append(BLThen+':'+GenTail);
  if LuTextS='{' then // for with begin
   begin
   RdTextS;
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' } ')=FALSE then break;
   if RdTextS<>'}' then AppendError('e','Internal error: } expected, \r found [R:TParsCpp.ParseBlock]');
   end
  else // for without begin
   begin
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,'')=FALSE then break;
   end;
  // Move section 3 of for block here
  while BFor3E>BFor3S do
   begin
   BLine:=AProc.FlowList.Strings[BFor3S];
   AProc.FlowList.Delete(BFor3S);
   AProc.FlowList.Append(BLine);
   Dec(BFor3E);
   end;
  // JMP to the beginning
  AProc.FlowList.Append('jmp '+BLCmp+GenTail);
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** FOR **

 if BReadS='switch' then
  begin
  RdTextS;
  BLStart:=AProc.LabelName('CaseStart',ALevel,BLabelIndex);
  BLEnd:=AProc.LabelName('CaseEnd',ALevel,BLabelIndex);
  AProc.FlowList.Append(BLStart+':'+GenTail);
  if RdTextS<>'(' then begin AppendError('e','"(" expected \r fond [R:TParsCpp.ParseBlock]'); SkipText(')'); BEval:=CInvalidIdentifier; end
  else
   begin
   BEval:=ParseEval(AProc,BVarListS);
   if LuTextS<>')' then AppendError('e',') expected, \l found [R:TParsCpp.ParseBlock]')
   else RdTextS;
   end;
  if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.FlushPostList;
  if RdTextS<>'{' then begin AppendError('e','{ expected \r found [R:TParsCpp.ParseBlock]'); break; end;
  BReadS:=RdTextS; BTarg:=CInvalidIdentifier;
  if StrInList(BReadS,'case default') then // This will ensure no unassigned Tmp vars are declared
   begin
   BTarg:=AProc.AppendTmpVar('?');
   AProc.FlowList.Append(BTarg+' := '+BEval+GenTail);
   end;

  BCaseIdx:=0;
  BLCmpNext:=AProc.LabelName('CaseCmp_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BLThenNext:=AProc.LabelName('CaseThen_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BResultA:=FALSE;
  repeat
  BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext; inc(BCaseIdx);
  BLCmpNext:=AProc.LabelName('CaseCmp_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  BLThenNext:=AProc.LabelName('CaseThen_'+IntToStr(BCaseIdx)+'_',ALevel,BLabelIndex);
  if BReadS<>'case' then begin AppendError('e','case expression expected \r found [R:TParsCpp.ParseBlock]'); break; end;
  BEval:=ParseEval(AProc,BVarListS);
  if RdTextS<>':' then begin AppendError('e',': expected, \l found [R:TParsCpp.ParseBlock]'); break; end;
  AProc.FlushPostList; // Impossible to have ++/-- here
  AProc.FlowList.Append(BLCmpThis+':'+GenTail);
  AProc.FlowList.Append('if '+BTarg+' cmpe ('+BEval+') '+BLThenThis+' '+BLCmpNext+GenTail);
  AProc.FlowList.Append(BLThenThis+':'+GenTail);
  if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' } case default ')=FALSE then break;
  AProc.FlowList.Append('jmp '+BLThenNext+GenTail);
  BReadS:=RdTextS;
  if BReadS='}' then
   begin
   BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext;
   AProc.FlowList.Append(BLCmpThis+':'+GenTail);
   AProc.FlowList.Append(BLThenThis+':'+GenTail);
   BResultA:=TRUE;
   break;
   end;
  if BReadS='default' then
   begin
   if RdTextS<>':' then begin AppendError('e',': expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
   BLCmpThis:=BLCmpNext; BLThenThis:=BLThenNext;
   AProc.FlowList.Append(BLCmpThis+':'+GenTail);
   AProc.FlowList.Append(BLThenThis+':'+GenTail);
   if ParseBlock(AProc,ALevel+1,BVarListS,BLEnd,' } ')=FALSE then break;
   if RdTextS<>'}' then begin AppendError('e','} expected after default statement, \r found [R:TParsCpp.ParseBlock]'); break; end;
   BResultA:=TRUE;
   break;
   end;
  until FALSE;
  if BResultA=FALSE then break;
  // End label
  AProc.FlowList.Append(BLEnd+':'+GenTail);
  // Final analysis
  inc(BLabelIndex);
  BResult:=TRUE;
  break;
  end; // ** SWITCH **


 if BReadS='va_start' then
  begin
  RdTextS;
  if RdTextS<>'(' then begin AppendError('e','"(" expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  BTarg:=ParseTargOpti(AProc,BVarListS,RdTextS);
  if RdTextS<>',' then begin AppendError('e','"," expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  BTargA:=ParseTargOpti(AProc,BVarListS,RdTextS);
  if RdTextS<>')' then begin AppendError('e','")" expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  if ParsIsPointer(BTarg)=FALSE then AppendError('e','Variable '+ParsExtractName(BTarg)+' must be of pointer type [R:TParsCpp.ParseBlock]');
  AProc.FlowList.Append(BTarg+' := @'+BTargA+GenTail);
  BTypeSize:=AProc.Module.GetTypeSize(ParsExtractType(BTargA));
  AProc.Module.AlignSize(BTypeSize);
  BTypeSize:=BTypeSize+AProc.Module.GetTypeSize('i');
  AProc.Module.AlignSize(BTypeSize);
  BConstS:=CTagS+'c_p'+CTagM+IntToStr(BTypeSize)+CTagE;
  AProc.Flowlist.Append(BTarg+' := '+BTarg+' add '+BConstS+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 if BReadS='va_end' then
  begin
  RdTextS;
  if RdTextS<>'(' then begin AppendError('e','"(" expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  BTarg:=ParseTargOpti(AProc,BVarListS,RdTextS);
  if RdTextS<>')' then begin AppendError('e','")" expected, \r found [R:TParsCpp.ParseBlock]'); break; end;
  if ParsIsPointer(BTarg)=FALSE then AppendError('e','Variable '+ParsExtractName(BTarg)+' must be of pointer type [R:TParsCpp.ParseBlock]');
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 if IsReservedWord(BReadS) then begin RdTextS; AppendError('e','Reserved word \r is used as an identifier [R:TParsCpp.ParseBlock]'); break; end;

 BTarg:=ParseEval(AProc,BVarListS,BIsEvalAsgn);
 if BTarg='' then begin AppendError('e','Error parsing eval (see previous messages) [R:TParsCpp.ParseBlock]'); BTarg:=CInvalidIdentifier; end;

 BReadS:=LuTextS;

 {if (ParsIsProc(BTarg)=FALSE) and (BReadS=';') and (AProc.PostList.Count<>0) then // Some statements like "cnt++" will produce this
  begin
  RdTextS;
  AProc.FlushPostList;
  Result:=TRUE;
  break;
  end;}

 if BReadS='=' then
  begin
  RdTextS;
  ParseAssign(AProc,BVarListS,BTarg,AEndMarker);
  BResult:=TRUE;
  break;
  end;

 AProc.FlushPostList; // Flush output of ParseEval above

 if (BReadS=';') and BIsEvalAsgn then
  begin
  RdTextS;
  BResult:=TRUE;
  break;
  end;

 if (ParsIsProc(BTarg)=FALSE) and StrInList(BReadS,AEndMarker) and BIsEvalAsgn then
  begin
  BResult:=TRUE;
  break;
  end;

 {if (BReadS='++') or (BReadS='--') then
  begin
  RdTextS;
  AppendCmdIncDecAny(AProc.FlowList,BTarg,BReadS);
  AProc.FlushPostList;
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 // += -= &= etc
 BOpcodeS:=StrToOpcodeDual(BReadS);
 if BOpcodeS<>'' then
  begin
  RdTextS;
  BEval:=ParseEval(AProc,BVarListS); if BEval='' then begin AppendError('e','Error parsing eval [R:TParsCpp.ParseBlock]'); BEval:=CInvalidIdentifier; end;
  AProc.Flowlist.Append(BTarg+' := '+BTarg+' '+BOpcodeS+' '+BEval+GenTail);
  AProc.FlushPostList;
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected at the end of assignment statement (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end; }

 // LastFieldIsFunction
 if ParsIsProc(BTarg) then
  begin
  //BEval:=ParseCallParams(AProc,BTarg); BTarg:=ParsExtractNameL(BTarg)+'('+BEval+')';
  AProc.Flowlist.Append(CDiscardValue+' := '+BTarg+GenTail);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 if IsDiscard(BTarg,BDiscard) then
  begin
  AProc.AppendDiscardListS(BDiscard);
  BReadS:=LuTextS;
  if StrInList(BReadS,AEndMarker) then begin BResult:=TRUE; break; end;
  RdTextS;
  if BReadS<>';' then begin AppendError('e','; expected (\r found) [R:TParsCpp.ParseBlock]'); break; end;
  BResult:=TRUE;
  break;
  end;

 // Something else
 AppendError('e','Construction is not recognized by the parser [R:TParsCpp.ParseBlock]');
 break;
 until TRUE; // Error loop

 if BResult=FALSE then break;
 if AEndMarker='' then begin Result:=TRUE; break; end;
 until FALSE;
End;

Function TParsCpp.ParseAny : boolean; // Returns TRUE if no fatal error (can stay in the parsing loop)
Var
  BReadS        : string;
  BVisi         : char;
  BItemListS    : string;
  BProcName,
  BProcParams   : string;
  BProc         : TLlvmProc;
  BNameA,
  BNameB        : string;
  BTypeS        : string;
  BParamsSpec   : string;
  BDummyS       : string;
  BPos,
  BPosA         : Integer;
  BProcNameA,
  BProcNameB    : string;
  BTargA,
  BTargB        : string;
  BParListA,
  BParListB     : string;
Begin
 Result:=FALSE;
 BReadS:=LuTextS;

 repeat
 BVisi:='h';
 if BReadS='static' then begin BVisi:='f'; RdTextS; end
 else if BReadS='extern' then begin BVisi:='g'; RdTextS; end;
 BReadS:=LuTextS;
 if BReadS='volatile' then begin {BVisi:=Chr(Ord(BVisi)-$20);} RdTextS; end; // By default global vars are volatile
 TryParseTypeA(BVisi,'',BTypeS);
 if ParsIsRecord(BTypeS) and (LuTextS=';') then
  begin
  RdTextS;
  Result:=TRUE;
  break;
  end;
 if BTypeS='' then begin AppendError('e','Invalid type \l or type is not specified [R:TParsCpp.ParseAny]'); break; end;
 BItemListS:=ParseDecl(nil,'',BVisi,BTypeS,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,'; )');
 BReadS:=LuTextS;

 if BReadS=';' then // Variable
  begin
  RdTextS;
  FModule.VarList:=FModule.VarList+BItemListS;
  case BVisi of
   'h': FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
   'f': FModule.PrivateNames:=FModule.PrivateNames+BItemListS+' ';
   'g': FModule.ExternNames:=FModule.ExternNames+BItemListS+' ';
  end;
  Result:=TRUE;
  break;
  end;

 if BReadS<>'(' then begin AppendError('e','";" or "(" expected, \r found [R:TParsCpp.ParseAny]'); break; end;

 BProcName:=ParsExtractName(BItemListS);
 ParseProcHead(BProcParams); BParamsSpec:=ParsCreateParamsSpec(BProcParams);
 BItemListS:=CTagS+ParsExtractSpec(BItemListS)+'{'+BParamsSpec+'}'+CTagM+BProcName+CTagE;

 if BVisi='g' then
  begin
  if ObjByName(FModule.ProcList,BProcName)<>nil then AppendError('e','Procedure '+BProcName+' is already declared internally [R:TParsCpp.ParseAny]');
  if RdTextS<>';' then begin AppendError('e','Function '+BProcName+' is declared as external and cannot be implemented here (hint: ";" expected \r found [R:TParsCpp.ParseAny]'); break; end;
  BProc:=FModule.AppendProc;
  BProc.Name:=BProcName; BProc.NameL:=BItemListS; BProc.ParListS:=BProcParams;
  Result:=TRUE;
  break;
  end;

 BReadS:=RdTextS;

 BProc:=ObjByName(FModule.ProcList,BProcName) as TLlvmProc;
 if BProc<>nil then
  begin
  if BReadS=';' then AppendError('e','Procedure '+BProcName+' is already declared (perhaps in one of includes) [R:TParsCpp.ParseAny]');
  end
 else
  begin
  BProc:=FModule.AppendProc;
  BProc.Name:=BProcName; BProc.NameL:=BItemListS; BProc.ParListS:=BProcParams;
  case BVisi of
   'h': begin
        if BReadS=';' then FModule.ExternNames:=FModule.ExternNames+BItemListS+' ' // TEMPORARILY write as extern
        else FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
        end;
   'f': FModule.PrivateNames:=FModule.PrivateNames+BItemListS+' ';
  end;
  end;

 if BReadS=';' then
  begin
  Result:=TRUE;
  break;
  end;

 BNameA:=BProc.NameL; BNameB:=BItemListS;
 Delete(BNameA,3,1); Delete(BNameB,3,1);
 if BNameA<>BNameB then AppendError('e','Procedure parameters of '+BProcName+' differ from previous declaration[R:TParsCpp.ParseAny]');
 BParListA:=BProc.ParListS; BParListB:=BProcParams;
 repeat
 BTargA:=ReadParamStr(BParListA); BTargB:=ReadParamStr(BParListB);
 if (BTargA='') or (BTargB='') then break;
 BNameA:=ParsExtractName(BTargA); BNameB:=ParsExtractName(BTargB);
 if (BNameA=CNullParamName) or (BNameB=CNullParamName) then
 else
  begin
  if BNameA<>BNameB then AppendError('w','Parameter names are not identical "'+BNameB+'"<>"'+BNameA+'" in previous declaration [R:TParsCpp.ParseAny]');
  end;
 until FALSE;
 if Copy(BProc.NameL,3,1)<>Copy(BItemListS,3,1) then AppendError('w','Procedure "'+BProcName+'" was declared earlier with another visibility [R:TParsCpp.ParseAny]');

 BProc.ParListS:=BProcParams;
 if BReadS<>'{' then begin AppendError('e','Procedure implementation expected (hint: "{" expected \r found [R:TParsCpp.ParseAny]'); break; end;
 if BProc.IsImplemented then begin AppendError('e','Procedure '+ParsExtractName(BProc.NameL)+' is already implemented [R:TParsCpp.ParseAny]'); break; end;
 ParseProcImpl(BProc);
 BPos:=Pos(CTagM+BProcName+CTagE,FModule.ExternNames);
 if BPos<>0 then // This means the function was inserted into "Extern" list by mistake. Move it to public or private
  begin
  BDummyS:=FModule.ExternNames;
  BPosA:=BPos-1;
  while BPosA>0 do
   begin
   if BDummyS[BPosA]=CTagS then break;
   Dec(BPosA);
   end;
  if BPosA=0 then inc(BPosA);
  BProcNameA:=Copy(BDummyS,BPosA,BPos-BPosA+Length(BProcName)+2);
  Delete(BDummyS,BPosA,BPos-BPosA+Length(BProcName)+2);
  BProcNameB:=BItemListS; DelFirstSpace(BProcNameB); DelLastSpace(BProcNameB);
  if BProcNameA<>BProcNameB then AppendError('w','Procedure '+BProcName+' was already declared with another visibility [R:TParsCpp.ParseAny]');
  FModule.ExternNames:=BDummyS;
  FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
  end;
 Result:=TRUE;

 until TRUE;
End;

Procedure TParsCpp.DefMacro ( Var ADefineA, ADefineB : string );
Var
  BLevel        : Integer;
  BReadS        : string;
Begin
 ADefineA:=ADefineA+'( ';
 BLevel:=1;
 repeat
 BReadS:=RdSameLine;
 if BReadS='' then begin AppendError('e',FParsCursor.FLine,FParsCursor.FPos,'Error in macro definition: closing bracket missmatch [R:TParsCpp.DefMacro]'); break; end;
 ADefineA:=ADefineA+BReadS;
 if BReadS='(' then inc(BLevel);
 if BReadS=')' then begin dec(BLevel); if BLevel=0 then break; end;
 until FALSE;

 ADefineB:='';
 repeat
 BReadS:=RdSameLine;
 if BReadS='' then break;
 ADefineB:=ADefineB+BReadS;
 until FALSE;

 DelLastSpace(ADefineA);
 DelLastSpace(ADefineB);
End;

Function TParsCpp.ParseType ( AVisi : char; Const AKnownNames : string ) : string;
Var
  BKnownTypesS  : string;
  BReadS        : string;
  BTypeS        : string;
  BTypeName     : string;
  BFieldList    : string;
  BRecSize      : Integer;
  BItemListS    : string;
  BTarg         : string;
  BProcParams   : string;
  BPos          : Integer;
Begin
 Result:='';
 BKnownTypesS:=AKnownNames;

 repeat
 BReadS:=LuTextS;
 if BReadS='struct' then
  begin
  RdTextS;
  BReadS:=RdTextS;
  BTypeName:=BReadS;
  if BReadS='' then begin AppendError('e','Unexpected end of file [R:TParsCpp.ParseType]'); break; end;
  if IsReservedWord(BReadS) then AppendError('e','Reserved word \r cannot be used as a type name [R:TParsCpp.ParseType]');
  if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a type name [R:TParsCpp.ParseType]');
  if Pos(' '+BReadS+' ',Result)<>0 then AppendError('e','Type \r has just been declared [R:TParsCpp.ParseType]');
  if Pos(CTagM+BReadS+CTagE+' ',BKnownTypesS)<>0 then AppendError('e','Type \r is already declared [R:TParsCpp.ParseType]');
  BFieldList:=ParseStructFields('');
  if RdTextS<>';' then begin AppendError('e','; expected, \r found [R:TParsCpp.ParseType]'); SkipText(';'); break; end;
  FModule.OrderRecordFields(BFieldList,BItemListS,BRecSize);
  Result:=CTagS+'t'+AVisi+'r'+IntToStr(BRecSize)+'e{'+BItemListS+'}'+CTagM+BTypeName+CTagE;
  break;
  end;

 TryParseTypeA(AVisi,'',BTypeS);
 if BTypeS='' then begin AppendError('e','Invalid type \l or type is not specified [R:TParsCpp.ParseType]'); break; end;
 if LuTextS='(' then
  begin
  RdTextS;
  BTarg:=ParseDecl(nil,'',AVisi,'',FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,')');
  if LuTextS<>')' then  AppendError('e','")" expected \l found [R:TParsCpp.ParseType]')
  else RdTextS;
  if LuTextS='(' then
   begin
   ParseProcHead(BProcParams);
   BTypeS:=ParsExtractType(BTarg)+'x'+BTypeS+'{'+ParsCreateParamsSpec(BProcParams)+'}';
   BTarg:=CTagS+'d'+AVisi+BTypeS+CTagM+ParsExtractName(BTarg)+CTagE;
   end
  else
   begin
   BPos:=Pos(CTagM,BTarg);
   Insert(BTypeS,BTarg,BPos);
   end;
  if RdTextS<>';' then AppendError('e','";" expected, \r found [R:TParsCpp.ParseType]');
  Result:=BTarg;
  end
 else
  begin
  Result:=ParseDecl(nil,'',AVisi,BTypeS,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames,';');
  if RdTextS<>';' then AppendError('e','";" expected, \r found [R:TParsCpp.ParseType]');
  end;
 if Length(Result)>2 then Result[2]:='t';
 until TRUE;
End;

Procedure TParsCpp.Parse ( AModule : TLlvmModule; AIsInclude : boolean );
Var
  BReadS        : string;
  BIncParser    : TParsBase;
  BErrorA       : string;
  BFilePath,
  BFileName,
  BFileExt      : string;
  BLen          : Integer;
  BIncType      : TIncType;
  BDefineA,
  BDefineB      : string;
  BVisi         : char;
  BItemListS    : string;
Begin
 Inherited;

// FModule.AppendExternName(CBuiltInVaList);

 FParsState:=psParsStart;

 if AIsInclude=FALSE then
  begin
  SplitFilename(FFilename,BFilePath,BFileName,BFileExt);
  FModule.Name:=BFileName;
  if LowerCase(BFileExt)='h' then FModule.Name:=FModule.Name+'.h';
  end;

 repeat
 BReadS:=LuTextS;
 if BReadS='' then
  begin
  if FPreprState<>'' then AppendError('w','There are unclosed preprocessor statements (like #ifdef, #elif, etc...) [R:TParsCpp.Parse]');
  break;
  end;

 // ** Uses section **
 if BReadS='#include' then
  begin
  RdTextS;
  BReadS:=RdSameLine;
  if BReadS='<' then
   begin
   BReadS:=RdSameLine('>');
   if BReadS='' then begin AppendError('e','Unit name expected [R:TParsCpp.Parse]'); break; end;
   BLen:=Length(BReadS);
   if BReadS[BLen]<>'>' then begin AppendError('e','">" expected after the unit name [R:TParsCpp.Parse]'); break; end;
   BIncType:=itCppSystem;
   Delete(BReadS,BLen,1);
   end
  else
   begin
   BLen:=Length(BReadS);
   BIncType:=itNone;
   if BLen<=2 then begin AppendError('e','Unit name is too short. (\r is not an allowed unit name) [R:TParsCpp.Parse]'); break; end;
   if (BReadS[1]='<') and (BReadS[BLen]='>') then BIncType:=itCppSystem
   else if (BReadS[1]='"') and (BReadS[BLen]='"') then BIncType:=itCppUser
   else begin AppendError('e','Include file must be taken in "" or <> parenthesis. (\r is not an allowed unit name) [R:TParsCpp.Parse]'); break; end;
   Delete(BReadS,BLen,1); Delete(BReadS,1,1);
   end;
  if IsAllowedName(BReadS)=FALSE then begin AppendError('e','Invalid unit name. (\r is not an allowed unit name) [R:TParsCpp.Parse]'); break; end;
  if Assigned(FReadInc)=FALSE then begin AppendError('e','Internal error: FReadInc unassigned [R:TParsCpp.Parse]'); break; end;
  BErrorA:='';
  BIncParser:=FReadInc(BReadS,BIncType,BErrorA);
  if BIncParser=nil then
   begin
   if BErrorA='' then BErrorA:='Cannot include this module \r [R:TParsCpp.Parse]';
   AppendError('e',BErrorA);
   break;
   end;
  if RdSameLine<>'' then begin AppendError('e','Unexpected parameter in line: \r [R:TParsCpp.Parse]'); break; end;
  BIncParser.Parse(FModule,TRUE);
  end

 // ** Define section **
 else if BReadS='#define' then
  begin
  RdTextS;
  BReadS:=RdSameLine;
  if BReadS='' then begin AppendError('e','Definition is expected after #define clause [R:TParsCpp.Parse]'); break; end;
  if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as a define value [R:TParsCpp.Parse]');
  BDefineA:=BReadS;
  BDefineB:='';
  BReadS:=LuBufS(0);
  if BReadS='(' then
   begin
   RdSameLine;
   DefMacro(BDefineA,BDefineB);
   end
  else
   begin
   BReadS:=RdSameLine;
   while BReadS<>'' do
    begin
    if BDefineB<>'' then BDefineB:=BDefineB+' ';
    BDefineB:=BDefineB+BReadS;
    BReadS:=RdSameLine;
    end;
   end;
  FModule.AppendDefine(BDefineA,BDefineB,FRdQueue[0]);
  {
   #define proc(a,b) WriteReg(a,b)
   #define PROC(x,y) proc(y,x)
  }
  end

 // ** Undef section **
 else if BReadS='#undef' then
  begin
  RdTextS;
  BReadS:=RdSameLine;
  if BReadS='' then begin AppendError('e','Definition is expected after #undef clause [R:TParsCpp.Parse]'); break; end;
  if IsAllowedName(BReadS)=FALSE then AppendError('e','\r cannot be used as an undef value [R:TParsCpp.Parse]');
  BDefineA:=BReadS;
  BDefineB:='';
  BReadS:=RdSameLine;
  while BReadS<>'' do
   begin
   if BDefineB<>'' then BDefineB:=BDefineB+' ';
   BDefineB:=BDefineB+BReadS;
   BReadS:=RdSameLine;
   end;
  FModule.RemoveDefine(BDefineA);
  end

 // ** Typedef section **
 else if BReadS='typedef' then
  begin
  RdTextS;
  BReadS:=LuTextS;
  if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
  BVisi:='h';
  if BReadS='static' then
   begin
   BVisi:='f';
   RdTextS;
   BReadS:=LuTextS;
   if BReadS='' then begin AppendError('e','Unexpected end of file'); break; end;
   end;
  BItemListS:=ParseType(BVisi,FModule.PublicNames+FModule.PrivateNames+FModule.VisibleNames);
  if BVisi='f' then FModule.PrivateNames:=FModule.PrivateNames+BItemListS+' '
  else FModule.PublicNames:=FModule.PublicNames+BItemListS+' ';
  end

 // ** All types **
 else
  begin
  if ParseAny=FALSE then break;
  end; // All types
 until FALSE;

 if FModule.GetErrorCountA=0 then
  begin
  end;

 FParsState:=psParsEnd;
End;

end.

