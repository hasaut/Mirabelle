unit ParsHelper_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd;

Function ParsReadBrackets ( Const ASrc : string; ABrackS, ABrackE : char ) : string;
Function ParsReadBrackets ( Var ASrc : string; ABrackS, ABrackE : char; Out AInside : string ) : boolean;
Function ParsExtractSpec ( Const ATarg : string ) : string;
Function ParsExtractType ( Const ATarg : string ) : string;
Function ParsExtractElemType ( Const ATarg : string ) : string;
Function ParsExtractRetType ( Const ATarg : string ) : string;
Function ParsExtractTypeElemType ( Const AType : string ) : string;
Function ParsExtractName ( Const ATarg : string ) : string;
Function ParsExtractNameAbsLoc ( Const ATarg : string; Out AAbsLoc : string ) : string;
Function ParsExtractNameL ( Const ATarg : string ) : string;
Function ParsCreateParamsSpec ( Const AProcParams : string ) : string;
Function ParsReadFieldList ( Const AType : string ) : string;
Procedure ParsGetFieldType ( Var AType : string; Const AFieldThis : string );
Procedure ParsSplitArrayIdx ( Const ATarg : string; Out AArray, AIndex : string );
Procedure ParsSplitProcRetParams ( Const ATypeS : string; Out ARet, AParams : string );
Function ParsIsConst ( Const ATarg : string ) : boolean;
Function ParsIsType ( Const ATarg : string ) : boolean;
Function ParsIsGlobalOrExtern ( Const ATarg : string ) : boolean;
Function ParsIsAbsLoc ( Const ATarg : string ) : boolean;
Function ParsExtractNameLoc ( Const AName : string ) : string;
Function ParsIsExtern ( Const ATarg : string ) : boolean;
Function ParsIsLocalOrTmp ( Const ATarg : string ) : boolean;
Function ParsIsLocalOrParamOrTmp ( Const ATarg : string ) : boolean;
Function ParsIsParam ( Const ATarg : string ) : boolean;
Function ParsIsTmp ( Const ATarg : string ) : boolean;
Function ParsIsPointer ( Const ATarg : string ) : boolean;
Function ParsIsQ ( Const ATarg : string ) : boolean;
Function ParsIsProc ( Const AEval : string ) : boolean;
Function ParsIsArray ( Const ATarg : string ) : boolean;
Function ParsIsRecord ( Const ATarg : string ) : boolean;
Function ParsIsStringP ( Const ATarg : string ) : boolean;
Function ParsIsStringZ ( Const ATarg : string ) : boolean;
Function ParsIsStringX ( Const ATarg : string ) : boolean;
Function ParsIsTypeArray ( Const AType : string ) : boolean;
Function ParsIsTypePointer ( Const AType : string ) : boolean;
Function ParsIsTypeQ ( Const AType : string ) : boolean;
Function ParsIsTypeRecord ( Const AType : string ) : boolean;
Function ParsIsTypeStringP ( Const AType : string ) : boolean;
Function ParsIsTypeStringZ ( Const AType : string ) : boolean;
Function ParsIsTypeStringX ( Const AType : string ) : boolean;
Function ParsIsTypeBasic ( Const AType : string ) : boolean;
Function ParsIsTypeSigned ( Const AType : string ) : boolean;
Function ParsIsTypeFloat ( Const AType : string ) : boolean;
Procedure ParsSplitRecFld ( Const ATarg : string; Out ARec, AFld : string );
Procedure ParsReadRecField ( Var AFieldList : string; Out AType, AName, AOffset : string );
Function ParsIsSpecTmp ( Const ASpec : string ) : boolean;
Function ParsIsSpecLocal ( Const ASpec : string ) : boolean;
Function ParsIsSpecParam ( Const ASpec : string ) : boolean;
Function ParsIsSpecParamConst ( Const ASpec : string ) : boolean;
Function ParsIsSpecParamVar ( Const ASpec : string ) : boolean;
Function ParsIsSpecParamAny ( Const ASpec : string ) : boolean;
Function ParsIsSpecResult ( Const ASpec : string ) : boolean;
Function ParsIsSpecVarConstResult ( Const ASpec : string ) : boolean;
Function ParsIsParamVar ( Const AParamType : string ) : boolean; // Param type, not Targ
Function ParsIsParamRetOut ( Const AParamType : string ) : boolean; // Param type, not Targ
Function ParsIsParamConst ( Const AParamType : string ) : boolean;
Function ParsIsTargLocal ( Const ATarg : string ) : boolean;
Function ParsIsTargParam ( Const ATarg : string ) : boolean; // Targ and not ParamType
Function ParsIsTargParamVar ( Const ATarg : string ) : boolean; // Targ and not ParamType
Function ParsIsTargParamConst ( Const ATarg : string ) : boolean; // Targ and not ParamType
Function ParsIsTargParamAny ( Const ATarg : string ) : boolean; // Targ and not ParamType
Function ParsIsTargResult ( Const ATarg : string ) : boolean; // Targ and not ParamType
Function ParsIsTargVarConstResult ( Const ATarg : string ) : boolean;
Function ParsGetStrPTypeLen ( Const AType : string ) : Integer;
Function ParsGetRecordSize ( Const AType : string ) : Integer;
Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : string; Out AFinalType : string );
Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : Integer; Out AFinalType : string );
Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : Integer );
Function ParsGetTypeArrayDim ( Const AType : string ) : Integer;
Procedure ParsGetArrayDim ( Const ATarg : string; Out ADimS, ADimE : string );
Function ParsGetArrayDim ( Const ATarg : string ) : Integer;
Procedure ParsForceConstType ( Var ATarg : string; Const AType : string );
Function GetBracketLevel ( Const AEval : string; AMaxPos : Integer ) : Integer;
Function ParsCheckGetForwardPointer ( Const AType : string ) : string;

Function ParsCheckCallPtr ( Const AEval : string; Out ARetType, AParamTypes : string ) : boolean;
Function ParsExtractParamTypes ( Const ATarg : string ) : string;
Function ParsSplitParamTypes ( Const AParams : string ) : string;

Function ParsFormatConstX ( AData : Cardinal ) : string;
Function ParsFormatConst ( AData : Cardinal ) : string;

Procedure ParsForceType ( Var ATarg : string; Const ANewType : string );
Procedure ParsReplaceAll ( Var AList : string; Const AOld, ANew : string );
Procedure ParsReplaceAll ( AText : TStringList; Const AOld, ANew : string );

Function RemoveRecInside ( Const ATarg : string ) : string;
Function ParsRemoveQTypeRef ( Const ATarg : string ) : string;
Function ParsExtractTypeParentType ( Var AType : string ) : string;

Function ParsRemoveExtern ( Const AVarList : string ) : string;
Function ParsRemoveAbsLoc ( Const AVarList : string ) : string;
Function ParsAppendVarListS ( Var AVarListS : string; Const ATarg : string ) : boolean;
Function ParsSearchTarg ( Const AVarListS : string; Const AName : string ) : string;

Function ParsTailGen ( Const AFilename : string; ALine, APos : Integer; Const AComment : string ) : string;
Function ParsTailGenA ( Const AFilename : string; ALine, APos : Integer; Const ADataA : string; Const AComment : string ) : string;
Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA, ADataB : Integer ) : boolean;
Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA : Integer ) : boolean;
Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA : string ) : boolean;
Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string ) : string;
Procedure ParsTailInsertTagVarA ( Var ATail : string; Const ATagName, ATagData : string );
Procedure ParsTailInsertTagVarB ( Var ATail : string; Const ATag : string );
Function ParsTailInsertTagResA ( Const ATail : string; Const ATagName, ATagData : string ) : string;
Function ParsTailInsertTagResB ( Const ATail : string; Const ATag : string ) : string;

implementation

Uses
  ConComL;

Function ParsReadBrackets ( Const ASrc : string; ABrackS, ABrackE : char ) : string;
Var
  BPosA,
  BPosB         : Integer;
Begin
 Result:='';
 BPosA:=Pos(ABrackS,ASrc);
 BPosB:=Pos(ABrackE,ASrc);
 repeat
 if BPosA=0 then break;
 if BPosB=0 then break;
 if (BPosB-BPosA)<1 then break;
 Result:=Copy(ASrc,BPosA+1,BPosB-BPosA-1);
 until TRUE;
End;

Function ParsReadBrackets ( Var ASrc : string; ABrackS, ABrackE : char; Out AInside : string ) : boolean;
Var
  BPosA,
  BPosB         : Integer;
Begin
 Result:=FALSE; AInside:='';
 BPosA:=Pos(ABrackS,ASrc);
 BPosB:=Pos(ABrackE,ASrc);
 repeat
 if BPosA=0 then break;
 if BPosB=0 then break;
 if (BPosB-BPosA)<1 then break;
 AInside:=Copy(ASrc,BPosA+1,BPosB-BPosA-1);
 Delete(ASrc,BPosA,BPosB-BPosA+1);
 Result:=TRUE;
 until TRUE;
End;

Function ParsExtractSpec ( Const ATarg : string ) : string;
Begin
 Result:=ParsReadBrackets(ATarg,CTagS,CTagM);
End;

Function ParsExtractType ( Const ATarg : string ) : string;
Begin
 Result:=ParsExtractSpec(ATarg);
 Delete(Result,1,2);
End;

Function ParsReadFieldList ( Const AType : string ) : string;
Var
  BPosA,
  BPosB         : Integer;
Begin
 Result:='';
 repeat
 if ParsIsTypeRecord(AType)=FALSE then break;
 BPosA:=Pos('{',AType); BPosB:=LastDelimiter('}',AType);
 Result:=Copy(AType,BPosA+1,BPosB-BPosA-1);
 until TRUE;
End;

Procedure ParsGetFieldType ( Var AType : string; Const AFieldThis : string );
Var
  BFieldList    : string;
  BType,
  BName,
  BOffset       : string;
Begin
 repeat
 if ParsIsTypeRecord(AType)=FALSE then begin AType:=''; break; end;
 BFieldList:=ParsReadFieldList(AType);
 BType:=''; BName:='';
 repeat
 ParsReadRecField(BFieldList,BType,BName,BOffset);
 if BName='' then break;
 if LowerCase(BName)=LowerCase(AFieldThis) then break;
 until FALSE;

 if BName='' then AType:=''
 else AType:=BType;

 until TRUE;
End;

Function ParsExtractElemType ( Const ATarg : string ) : string;
Begin
 Result:=ParsExtractTypeElemType(ParsExtractType(ATarg));
End;

Function ParsExtractTypeElemType ( Const AType : string ) : string;
Var
  BPos          : Integer;
Begin
 Result:=AType;
 repeat
 if Result='' then break;
 if Result[1]='a' then begin BPos:=Pos('e',Result); Delete(Result,1,BPos); break; end;
 if Result[1]='p' then begin Delete(Result,1,1); break; end;
 if ParsIsTypeStringP(Result) then begin Result:='c'; break; end;
 if ParsIsTypeStringZ(Result) then begin Result:='c'; break; end;
 Result:='';
 until TRUE;
End;

Function ParsExtractRetType ( Const ATarg : string ) : string;
Var
  BTypeParams   : string;
Begin
 ParsSplitProcRetParams(ParsExtractType(ATarg),Result,BTypeParams);
End;

Function ParsExtractName ( Const ATarg : string ) : string;
Var
  BPos          : Integer;
Begin
 Result:=ParsReadBrackets(ATarg,CTagM,CTagE);
 BPos:=Pos(CTagP,Result); if BPos<>0 then Delete(Result,BPos,Length(Result)-BPos+1);
 BPos:=Pos(CTagA,Result); if BPos<>0 then Delete(Result,BPos,Length(Result)-BPos+1);
End;

Function ParsExtractNameAbsLoc ( Const ATarg : string; Out AAbsLoc : string ) : string;
Var
  BPos          : Integer;
Begin
 AAbsLoc:='';
 Result:=ParsReadBrackets(ATarg,CTagM,CTagE);
 BPos:=Pos(CTagP,Result); if BPos<>0 then Delete(Result,BPos,Length(Result)-BPos+1);
 BPos:=Pos(CTagA,Result); if BPos<>0 then
  begin
  AAbsLoc:=Copy(Result,BPos+1,Length(Result)-BPos);
  Delete(Result,BPos,Length(Result)-BPos+1);
  end;
End;


Function ParsExtractNameL ( Const ATarg : string ) : string;
Var
  BTarg         : string;
Begin
 BTarg:=ATarg;
 Result:=ReadTillS(BTarg,CTagE)+CTagE;
End;

Function ParsCreateParamsSpec ( Const AProcParams : string ) : string;
Var
  BProcParams   : string;
  BParamA       : string;
Begin
 BProcParams:=AProcParams;
 Result:='';
 repeat
 BParamA:=ReadParamStr(BProcParams);
 if BParamA='' then break;
 if Result<>'' then Result:=Result+',';
 Result:=Result+ParsExtractSpec(BParamA);
 until FALSE;
End;

Procedure ParsSplitArrayIdx ( Const ATarg : string; Out AArray, AIndex : string );
Var
  BPosA,
  BPosB         : Integer;
  BLevel        : Integer;
Begin
 AArray:=''; AIndex:='';
 repeat
 BPosA:=Pos('[',ATarg);
 if BPosA=0 then begin AArray:=ATarg; break; end;
 BPosB:=BPosA+1; BLevel:=0;
 while BPosB<=Length(ATarg) do
  begin
  if ATarg[BPosB]='[' then inc(BLevel);
  if ATarg[BPosB]=']' then
   begin
   if BLevel=0 then break;
   dec(BLevel);
   end;
  inc(BPosB);
  end;
 if (BPosA<>0) and (BPosB>BPosA) then
  begin
  AArray:=Copy(ATarg,1,BPosA-1);
  AIndex:=Copy(ATarg,BPosA+1,BPosB-BPosA-1);
  end;
 until TRUE;
End;

Procedure ParsSplitProcRetParams ( Const ATypeS : string; Out ARet, AParams : string );
Var
  BPosA,
  BPosB         : Integer;
  BLevel        : Integer;
Begin
 ARet:=''; AParams:='';
 repeat
 if ATypeS='' then break;
 BPosB:=Length(ATypeS);
 if ATypeS[BPosB]<>'}' then break;
 BPosA:=BPosB-1; BLevel:=1;
 while BPosA>0 do
  begin
  if ATypeS[BPosA]='{' then Dec(BLevel)
  else if ATypeS[BPosA]='}' then Inc(BLevel);
  if BLevel=0 then break;
  Dec(BPosA);
  end;
 if BLevel<>0 then break;
 ARet:=Copy(ATypeS,1,BPosA-1);
 AParams:=Copy(ATypeS,BPosA+1,BPosB-BPosA-1);
 until TRUE;
End;

Function ParsIsConst ( Const ATarg : string ) : boolean;
Begin
 Result:=Pos(CTagS+'c',ATarg)=1;
End;

Function ParsIsType ( Const ATarg : string ) : boolean;
Begin
 Result:=Pos(CTagS+'t',ATarg)=1;
End;

Function ParsIsGlobalOrExtern ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=(Pos('dh',BSpec)=1) or (Pos('df',BSpec)=1) or (Pos('dg',BSpec)=1);
End;

Function ParsIsAbsLoc ( Const ATarg : string ) : boolean;
Begin
 Result:=Pos(CTagA,ATarg)<>0;
End;

Function ParsExtractNameLoc ( Const AName : string ) : string;
Var
  BPos      : Integer;
Begin
 Result:='';
 BPos:=Pos(CTagA,AName);
 if BPos<>0 then Result:=Copy(AName,BPos+1,Length(AName)-BPos);
End;

Function ParsIsExtern ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=Pos('dg',BSpec)=1;
End;

Function ParsIsLocalOrTmp ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=(Pos('db',BSpec)=1) or (Pos('de',BSpec)=1);
End;

Function ParsIsLocalOrParamOrTmp ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=(Pos('db',BSpec)=1) or (Pos('de',BSpec)=1) or (Pos('da',BSpec)=1);
End;

Function ParsIsParam ( Const ATarg : string ) : boolean;
Begin
 Result:=ParsIsSpecParam(ParsExtractSpec(ATarg));
End;

Function ParsIsTmp ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=(Pos('de',BSpec)=1);
End;

Function ParsIsPointer ( Const ATarg : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=ParsExtractType(ATarg);
 Result:=(Pos('p',BType)=1) or (Pos('q',BType)=1);
End;

Function ParsIsQ ( Const ATarg : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=ParsExtractType(ATarg);
 Result:=Pos('q',BType)=1;
End;

Function ParsIsProc ( Const AEval : string ) : boolean;
Var
  BSpec         : string;
  BEval,
  BInside       : string;
  BRetType,
  BParamTypes   : string;
Begin
 Result:=FALSE;
 repeat
 if AEval='' then break;
 if AEval[1]=CTagS then
  begin
  BSpec:=ParsExtractSpec(AEval);
  Result:=Pos('x',BSpec)=1;
  break;
  end;
 if AEval[1]<>'(' then break;
 BEval:=AEval;
 ParsReadBrackets(BEval,'(',')',BInside);
 Result:=ParsCheckCallPtr(BInside,BRetType,BParamTypes);
 until TRUE;
End;

Function ParsIsArray ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
  BLen          : Integer;
Begin
 Result:=FALSE;
 repeat
 BSpec:=ParsExtractSpec(ATarg);
 BLen:=Length(BSpec);
 if (BLen<3) then break;
 if BSpec[3]='a' then begin Result:=TRUE; break; end;
 until TRUE;
End;

Function ParsIsRecord ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
  BLen          : Integer;
Begin
 Result:=FALSE;
 repeat
 BSpec:=ParsExtractSpec(ATarg);
 BLen:=Length(BSpec);
 if (BLen<3) then break;
 if BSpec[3]='r' then begin Result:=TRUE; break; end;
 until TRUE;
End;

Function ParsIsStringP ( Const ATarg : string ) : boolean;
Begin
 Result:=ParsIsTypeStringP(ParsExtractType(ATarg));
End;

Function ParsIsStringZ ( Const ATarg : string ) : boolean;
Begin
 Result:=ParsIsTypeStringZ(ParsExtractType(ATarg));
End;

Function ParsIsStringX ( Const ATarg : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=ParsExtractType(ATarg);
 Result:=ParsIsTypeStringP(BType) or ParsIsTypeStringZ(BType);
End;

Function ParsIsTypeArray ( Const AType : string ) : boolean;
Begin
 Result:=Copy(AType,1,1)='a';
End;

Function ParsIsTypePointer ( Const AType : string ) : boolean;
Begin
 Result:=pos('p',AType)=1;
End;

Function ParsIsTypeQ ( Const AType : string ) : boolean;
Begin
 Result:=pos('q',AType)=1;
End;

Function ParsIsTypeRecord ( Const AType : string ) : boolean;
Begin
 Result:=Copy(AType,1,1)='r';
End;

Function ParsIsTypeStringP ( Const AType : string ) : boolean;
Begin
 Result:=Pos('sp',AType)=1;
End;

Function ParsIsTypeStringZ ( Const AType : string ) : boolean;
Begin
 Result:=Pos('sz',AType)=1;
End;

Function ParsIsTypeStringX ( Const AType : string ) : boolean;
Begin
 Result:=ParsIsTypeStringP(AType) or ParsIsTypeStringZ(AType);
End;

Function ParsIsTypeBasic ( Const AType : string ) : boolean;
Begin
 Result:=StrInList(AType,CBasicTypes);
End;

Function ParsIsTypeSigned ( Const AType : string ) : boolean;
Begin
 Result:=(AType='k') or (AType='m') or (AType='i');
End;

Function ParsIsTypeFloat ( Const AType : string ) : boolean;
Begin
 Result:=(AType='f');
End;

Procedure ParsSplitRecFld ( Const ATarg : string; Out ARec, AFld : string );
Var
  BTarg         : string;
Begin
 BTarg:=ATarg;
 ARec:=ReadParamStr(BTarg,CTagE)+CTagE;
 AFld:=ReadParamStr(BTarg);
 if (AFld<>'') and (AFld[1]='.') then Delete(AFld,1,1);
End;

Function ParsIsSpecTmp ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='de';
End;

Function ParsIsSpecLocal ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='db';
End;

Function ParsIsSpecParam ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='da';
End;

Function ParsIsSpecParamConst ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='dc';
End;

Function ParsIsSpecParamVar ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='dv';
End;

Function ParsIsSpecResult ( Const ASpec : string ) : boolean;
Begin
 Result:=Copy(ASpec,1,2)='dr';
End;

Function ParsIsSpecParamAny ( Const ASpec : string ) : boolean;
Begin
 Result:=(ASpec[1]='d') and (ASpec[2] in ['a', 'c', 'v']);
End;

Function ParsIsSpecVarConstResult ( Const ASpec : string ) : boolean;
Begin
 Result:=(ASpec[1]='d') and (ASpec[2] in ['c', 'v', 'r']);
End;

Function ParsIsParamVar ( Const AParamType : string ) : boolean;
Begin
 Result:=AParamType[2]='v';
End;

Function ParsIsParamRetOut ( Const AParamType : string ) : boolean; // Param type, not Targ
Begin
 Result:=AParamType[2]='q';
End;

Function ParsIsParamConst ( Const AParamType : string ) : boolean;
Begin
 Result:=AParamType[2]='c';
End;

Function ParsIsTargLocal ( Const ATarg : string ) : boolean;
Begin
 Result:=ParsIsSpecLocal(ParsExtractSpec(ATarg));
End;

Function ParsIsTargParam ( Const ATarg : string ) : boolean; // Targ and not ParamType
Begin
 Result:=ParsIsSpecParam(ParsExtractSpec(ATarg));
End;

Function ParsIsTargParamVar ( Const ATarg : string ) : boolean;
Begin
 Result:=ParsIsSpecParamVar(ParsExtractSpec(ATarg));
End;

Function ParsIsTargParamConst ( Const ATarg : string ) : boolean; // Targ and not ParamType
Begin
 Result:=ParsIsSpecParamConst(ParsExtractSpec(ATarg));
End;

Function ParsIsTargParamAny ( Const ATarg : string ) : boolean; // Targ and not ParamType
Begin
 Result:=ParsIsSpecParamAny(ParsExtractSpec(ATarg));
End;

Function ParsIsTargResult ( Const ATarg : string ) : boolean; // Targ and not ParamType
Begin
 Result:=ParsIsSpecResult(ParsExtractSpec(ATarg));
End;

Function ParsIsTargVarConstResult ( Const ATarg : string ) : boolean; // Targ and not ParamType
Var
  BSpec     : string;
Begin
 BSpec:=ParsExtractSpec(ATarg);
 Result:=(Length(BSpec)>=2) and ParsIsSpecVarConstResult(BSpec);
End;

Function ParsGetStrPTypeLen ( Const AType : string ) : Integer;
Var
  BTypeS        : string;
  BSizeS        : string;
Begin
 Result:=-1;
 repeat
 BTypeS:=AType;
 Delete(BTypeS,1,2); BSizeS:=ReadParamStr(BTypeS,'e');
 if TryStrToInt(BSizeS,Result)=FALSE then break;
 until TRUE;
End;

Function ParsGetRecordSize ( Const AType : string ) : Integer;
Var
  BTypeS        : string;
  BSizeS        : string;
Begin
 Result:=0;
 BTypeS:=AType;
 repeat
 if BTypeS='' then break;
 if BTypeS[1]<>'r' then break;
 Delete(BTypeS,1,1); BSizeS:=ReadParamStr(BTypeS,'e');
 if TryStrToInt(BSizeS,Result)=FALSE then break;
 until TRUE;
End;

Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : string; Out AFinalType : string );
Var
  BTypeS        : string;
Begin
 BTypeS:=AType;
 ADimS:=''; ADimE:=''; AFinalType:='';
 repeat
 if BTypeS='' then break;
 if BTypeS[1]='a' then begin Delete(BTypeS,1,1); ADimS:=ReadParamStr(BTypeS,'s'); ADimE:=ReadParamStr(BTypeS,'e'); DelLastSpace(BTypeS); AFinalType:=BTypeS; break; end;
 if (BTypeS[1]='p') or (BTypeS[1]='q') then begin Delete(BTypeS,1,1); ADimS:='0'; ADimE:='0'; DelLastSpace(BTypeS); AFinalType:=BTypeS; break; end;
 if ParsIsTypeStringP(BTypeS) then begin ADimS:='0'; Delete(BTypeS,1,2); ADimE:=ReadParamStr(BTypeS,'e'); DelLastSpace(BTypeS); AFinalType:=BTypeS; break; end;
 if ParsIsTypeStringZ(BTypeS) then begin ADimS:='0'; Delete(BTypeS,1,2); ADimE:=ReadParamStr(BTypeS,'e'); DelLastSpace(BTypeS); AFinalType:=BTypeS; break; end;
 until TRUE;
End;

Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : Integer; Out AFinalType : string );
Var
  BDimS,
  BDimE         : string;
Begin
 ParsGetTypeArrayDim(AType,BDimS,BDimE,AFinalType);
 ADimS:=0; ADimE:=0;
 TryStrToInt(BDimS,ADimS); TryStrToInt(BDimE,ADimE);
End;

Procedure ParsGetTypeArrayDim ( Const AType : string; Out ADimS, ADimE : Integer );
Var
  BFinalType    : string;
Begin
 ParsGetTypeArrayDim(AType,ADimS,ADimE,BFinalType);
End;

Function ParsGetTypeArrayDim ( Const AType : string ) : Integer;
Var
  BDimS,
  BDimE         : Integer;
Begin
 ParsGetTypeArrayDim(AType,BDimS,BDimE);
 Result:=BDimE-BDimS+1;
End;

Procedure ParsGetArrayDim ( Const ATarg : string; Out ADimS, ADimE : string );
Var
  BTypeS        : string;
Begin
 BTypeS:=ParsExtractType(ATarg);
 Delete(BTypeS,1,1);
 ADimS:=ReadParamStr(BTypeS,'s');
 ADimE:=ReadParamStr(BTypeS,'e');
End;

Function ParsGetArrayDim ( Const ATarg : string ) : Integer;
Var
  BArraySStr,
  BArrayEStr    : string;
  BArraySInt,
  BArrayEInt    : Integer;
Begin
 ParsGetArrayDim(ATarg,BArraySStr,BArrayEStr);
 TryStrToInt(BArraySStr,BArraySInt);
 TryStrToInt(BArrayEStr,BArrayEInt);
 Result:=BArrayEInt-BArraySInt+1;
End;

Procedure ParsForceConstType ( Var ATarg : string; Const AType : string );
Begin
 Delete(ATarg,4,1);
 Insert(AType,ATarg,4);
End;

Function ParsTailGen ( Const AFilename : string; ALine, APos : Integer; Const AComment : string ) : string;
Begin
 Result:='[SrcFile:'+AFilename+'][SrcPos:'+IntToStr(ALine)+','+IntToStr(APos)+'][SrcOrig:]'+AComment;
End;

Function ParsTailGenA ( Const AFilename : string; ALine, APos : Integer; Const ADataA : string; Const AComment : string ) : string;
Begin
 Result:='[SrcFile:'+AFilename+'][SrcPos:'+IntToStr(ALine)+','+IntToStr(APos)+']'+ADataA+'[SrcOrig:]'+AComment;
End;

Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA, ADataB : Integer ) : boolean;
Var
  BLineA,
  BLineB        : string;
  BDataA,
  BDataB        : string;
  BTag          : string;
Begin
 ADataA:=0; ADataB:=0;
 BLineA:=ATail; BLineB:=ReadTillS(BLineA,'[SrcOrig:]');
 BTag:=CheckTag(ATag,BLineB);
 ATail:=BLineB+BLineA;
 BDataA:=ReadTillS(BTag,',');
 BDataB:=ReadParamStr(BTag);
 Result:=TryStrToInt(BDataA,ADataA) and TryStrToInt(BDataB,ADataB);
End;

Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA : Integer ) : boolean;
Var
  BLineA,
  BLineB        : string;
  BDataA        : string;
  BTag          : string;
Begin
 ADataA:=0;
 BLineA:=ATail; BLineB:=ReadTillS(BLineA,'[SrcOrig:]');
 BTag:=CheckTag(ATag,BLineB);
 ATail:=BLineB+BLineA;
 BDataA:=ReadParamStr(BTag);
 Result:=TryStrToInt(BDataA,ADataA);
End;

Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string ) : string;
Var
  BLineA,
  BLineB        : string;
Begin
 BLineA:=ATail; BLineB:=ReadTillS(BLineA,'[SrcOrig:]');
 Result:=CheckTag(ATag,BLineB);
 ATail:=BLineB+BLineA;
End;

Function ParsTailExtractTagValue ( Var ATail : string; Const ATag : string; Out ADataA : string ) : boolean;
Begin
 ADataA:=ParsTailExtractTagValue(ATail,ATag);
 Result:=ADataA<>'';
End;

Procedure ParsTailInsertTagVarA ( Var ATail : string; Const ATagName, ATagData : string );
Var
  BPosE         : Integer;
  BTagS         : string;
Begin
 BTagS:='['+ATagName+':'+ATagData+']';
 repeat
 BPosE:=Pos('[SrcOrig:]',ATail);
 if BPosE=0 then begin ATail:=ATail+BTagS; break; end;
 Insert(BTagS,ATail,BPosE);
 until TRUE;
End;

Procedure ParsTailInsertTagVarB ( Var ATail : string; Const ATag : string );
Var
  BPosE         : Integer;
Begin
 repeat
 BPosE:=Pos('[SrcOrig:]',ATail);
 if BPosE=0 then begin ATail:=ATail+ATag; break; end;
 Insert(ATag,ATail,BPosE);
 until TRUE;
End;

Function ParsTailInsertTagResA ( Const ATail : string; Const ATagName, ATagData : string ) : string;
Begin
 Result:=ATail;
 ParsTailInsertTagVarA(Result,ATagName,ATagData);
End;

Function ParsTailInsertTagResB ( Const ATail : string; Const ATag : string ) : string;
Begin
 Result:=ATail;
 ParsTailInsertTagVarB(Result,ATag);
End;

Function GetBracketLevel ( Const AEval : string; AMaxPos : Integer ) : Integer;
Var
  BPos          : Integer;
Begin
 Result:=0;
 BPos:=0;
 while BPos<AMaxPos do
  begin
  if AEval[BPos]='(' then inc(Result)
  else if AEval[BPos]=')' then dec(Result);
  inc(BPos);
  end;
End;

Function ParsCheckGetForwardPointer ( Const AType : string ) : string;
Begin
 Result:='';
 if (AType<>'') and (Pos('p"',AType)=1) and (AType[Length(AType)]='"') then Result:=Copy(AType,3,Length(AType)-3);
End;

Function ParsCheckCallPtr ( Const AEval : string; Out ARetType, AParamTypes : string ) : boolean;
Var
  BEval         : string;
  BTypeS        : string;
  BIndexesS     : string;
  BPos          : Integer;
  BPtrDeepness  : Integer;
  BInsideS      : string;
Begin
 Result:=FALSE; ARetType:=''; AParamTypes:='';
 BEval:=AEval;
 repeat
 DelFirstSpace(BEval); DelLastSpace(BEval);
 if Pos(#32,BEval)<>0 then break;
 BTypeS:=ParsExtractType(BEval);
 BIndexesS:=BEval;
 BPos:=Pos(CTagE,BIndexesS);
 if BPos=0 then break;
 Delete(BIndexesS,1,BPos);
 BPtrDeepness:=0; while ParsReadBrackets(BIndexesS,'[',']',BInsideS) do Inc(BPtrDeepness);
 while BPtrDeepness>0 do
  begin
  if BTypeS='' then break;
  if BTypeS[1]<>'p' then break;
  Delete(BTypeS,1,1);
  Dec(BPtrDeepness);
  end;
 if BPtrDeepness<>0 then break;
 if BTypeS='' then break;
 if BTypeS[1]<>'x' then break;
 Delete(BTypeS,1,1);
 ParsSplitProcRetParams(BTypeS,ARetType,AParamTypes);
 if AParamTypes='' then break;
 Result:=TRUE;
 until TRUE;
End;

Function ParsExtractParamTypes ( Const ATarg : string ) : string;
Var
  BRetType      : string;
Begin
 ParsSplitProcRetParams(ParsExtractType(ATarg),BRetType,Result);
End;

Function ParsSplitParamTypes ( Const AParams : string ) : string;
Var
  BIndex        : Integer;
  BLevel        : Integer;
  BChar         : char;
Begin
 Result:=AParams;
 BLevel:=0;
 for BIndex:=1 to Length(Result) do
  begin
  BChar:=Result[BIndex];
  case BChar of
   ',': if BLevel=0 then Result[BIndex]:=#32;
   '{': Inc(BLevel);
   '}': Dec(BLevel);
  end;
  end;
End;

Procedure ParsReadRecField ( Var AFieldList : string; Out AType, AName, AOffset : string );
Var
  BType,
  BName         : string;
  BDataC        : char;
  BState        : byte;
  BLevel        : Integer;
  BPos          : Integer;
Begin
 AType:=''; AName:=''; AOffset:='';
 BType:=''; BName:=''; BLevel:=0; BState:=0;
 while AFieldList<>'' do
  begin
  BDataC:=AFieldList[1]; Delete(AFieldList,1,1);
  if (BDataC=',') and (BLevel=0) then break
  else if BDataC='{' then inc(BLevel)
  else if BDataC='}' then dec(BLevel);
  if (BDataC='_') and (BLevel=0) then BState:=1;
  if BState=0 then BType:=BType+BDataC
  else BName:=BName+BDataC;
  end;
 if BType<>'' then
  begin
  Delete(BName,1,1);
  AName:=BName;
  BPos:=LastDelimiter('o',BType);
  AType:=Copy(BType,1,BPos-1);
  Delete(BType,1,BPos);
  AOffset:=BType;
  end;
End;

Function ParsFormatConstX ( AData : Cardinal ) : string;
Begin
 if AData<256 then Result:=CTagS+'c_b'+CTagM+'0x'+IntToHex(AData,2)+CTagE
 else if AData<65536 then  Result:=CTagS+'c_w'+CTagM+'0x'+IntToHex(AData,4)+CTagE
 else Result:=CTagS+'c_d'+CTagM+'0x'+IntToHex(AData,8)+CTagE;
End;

Function ParsFormatConst ( AData : Cardinal ) : string;
Begin
 if AData<256 then Result:=CTagS+'c_b'+CTagM+IntToStr(AData)+CTagE
 else if AData<65536 then Result:=CTagS+'c_w'+CTagM+IntToStr(AData)+CTagE
 else Result:=CTagS+'c_d'+CTagM+IntToStr(AData)+CTagE;
End;

Procedure ParsForceType ( Var ATarg : string; Const ANewType : string );
Var
  BName         : string;
  BSpec         : string;
Begin
 BName:=ParsExtractName(ATarg);
 BSpec:=ParsExtractSpec(ATarg);
 ATarg:=CTagS+Copy(BSpec,1,2)+ANewType+CTagM+BName+CTagE;
End;

Procedure ParsReplaceAll ( Var AList : string; Const AOld, ANew : string );
Var
  BLenOld       : Integer;
  BPos          : Integer;
Begin
 BLenOld:=Length(AOld);
 repeat
 BPos:=Pos(AOld,AList); if BPos=0 then break;
 Delete(AList,BPos,BLenOld);
 Insert(ANew,AList,BPos);
 until FALSE;
End;

Procedure ParsReplaceAll ( AText : TStringList; Const AOld, ANew : string );
Var
  BReadS        : string;
  BLenOld       : Integer;
  BPos          : Integer;
  BIndex        : Integer;
  BDirty        : boolean;
Begin
 BLenOld:=Length(AOld);
 BIndex:=0;
 while BIndex<AText.Count do
  begin
  BReadS:=AText.Strings[BIndex];
  BDirty:=FALSE;
  repeat
  BPos:=Pos(AOld,BReadS); if BPos=0 then break;
  BDirty:=TRUE;
  Delete(BReadS,BPos,BLenOld);
  Insert(ANew,BReadS,BPos);
  until FALSE;
  if BDirty then AText.Strings[BIndex]:=BReadS;
  inc(BIndex);
  end;
End;

Function RemoveRecInside ( Const ATarg : string ) : string;
Var
  BPos          : Integer;
  BDummyS       : string;
Begin
 Result:=ATarg;
 BPos:=Pos('{',Result);
 if BPos<>0 then ReadInside(Result,BPos,BDummyS);
End;

Function ParsRemoveQTypeRef ( Const ATarg : string ) : string;
Var
  BPos          : Integer;
Begin
 Result:=ATarg;
 BPos:=Pos(']_',Result);
 if BPos<>0 then Delete(Result,BPos+1,Length(Result)-BPos);
End;

Function ParsExtractTypeParentType ( Var AType : string ) : string;
Var
  BPos  : Integer;
Begin
 Result:='';
 repeat
 if AType='' then break;
 if AType[1]='a' then begin BPos:=Pos('e',AType); Result:=Copy(AType,1,BPos); Delete(AType,1,BPos); break; end;
 if AType[1]='p' then begin Result:=AType[1]; Delete(AType,1,1); break; end;
 if AType[1]='q' then begin Result:=AType[1]; Delete(AType,1,1); break; end;
 if ParsisTypeStringP(AType) then begin Result:=AType; AType:='c'; break; end;
 if ParsisTypeStringZ(AType) then begin Result:=AType; AType:='c'; break; end;

 until TRUE;
End;

Function ParsRemoveExtern ( Const AVarList : string ) : string;
Var
  BVarList      : string;
  BTarg         : string;
Begin
 BVarList:=AVarList;
 Result:='';
 while BVarList<>'' do
  begin
  BTarg:=ReadParamStr(BVarList);
  if BTarg='' then break;
  if ParsIsExtern(BTarg)=FALSE then Result:=Result+BTarg+' ';
  end;
End;

Function ParsRemoveAbsLoc ( Const AVarList : string ) : string;
Var
  BVarList      : string;
  BTarg         : string;
Begin
 BVarList:=AVarList;
 Result:='';
 while BVarList<>'' do
  begin
  BTarg:=ReadParamStr(BVarList);
  if BTarg='' then break;
  if ParsIsAbsLoc(BTarg)=FALSE then Result:=Result+BTarg+' ';
  end;
End;

Function ParsAppendVarListS ( Var AVarListS : string; Const ATarg : string ) : boolean;
Var
  BName         : string;
  BSearchS      : string;
Begin
 Result:=FALSE;
 BName:=ParsExtractName(ATarg); BSearchS:=CTagM+BName+CTagE;
 repeat
 if Pos(BSearchS,AVarListS)<>0 then break;
 AVarListS:=AVarListS+ATarg+' ';
 Result:=TRUE;
 until TRUE;
End;

Function ParsSearchTarg ( Const AVarListS : string; Const AName : string ) : string;
Var
  BVarListS     : string;
  BTarg         : string;
  BSearchS      : string;
Begin
 Result:='';
 BVarListS:=AVarListS;
 BSearchS:=CTagM+AName+CTagE;
 repeat
 BTarg:=ReadParamStr(BVarListS);
 if BTarg='' then break;
 if Pos(BSearchS,BTarg)<>0 then begin Result:=BTarg; break; end;
 until FALSE;
End;

end.

