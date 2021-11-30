unit BackEndMs_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsBase_sd, ParsHelper_sd, LlvmBase_sd, LlvmDbg_sd,
  LlvmLine_sd, LlvmStrRec_sd, LlvmBeHelper_sd;

Type
  TModuleMs = class(TModuleBeHelper)
  private
    Procedure WriteDataSeg ( AAsmSrc : TStringList );
    Procedure ExtractRecField ( Var ASrcList, ADstList : string; Var AOffset : Integer; ASizeA, ASizeMod : Integer );
    Procedure AppendConstStrP ( AAsmSrc : TStringList; Const ATarg, AValue : string );
    Procedure AppendConstStrZ ( AAsmSrc : TStringList; Const ATarg, AValue : string );
    Procedure AppendConstArray ( AAsmSrc : TStringList; Const ATarg, AValue : string );
    Procedure AppendConsts ( AAsmSrc : TStringList );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function AppendProc : TLlvmProc; Override;
    Procedure AlignSize ( Var AData : Integer ); Override;
    Function GetTypeSize ( Const ATypeS : string ) : Integer; Override;
    Procedure OrderRecordFields ( Const ASrcList : string; Out ADstList : string; Out ARecSize : Integer ); Override;

    Procedure GenAsmSrc ( AAsmSrc : TStringList ); Override;
  end;

  TExternNeeds = (enMoveCwx, enMemCopy, enStrMovS, enStrAddS, enStrCmpS, enStrMovC, enStrAddC, enStrCmpC);
  TNeedsList = set of TExternNeeds;

  TProcMs = class(TProcBeHelper)
  private
    FTextMcd            : TStringList;
    FTargRegMapList     : TTargRegMapList;
    //FSrcDstTextMap      : array of Integer; // TextDst can be bigger then TextSrc. For debug info we need a correspondence
    FDbgInfoS           : Integer; // TextDst can be bigger then TextSrc. DbgInfoStart shows where main block beging
    FRegSaveList        : word; // gr fr er dr cr br ar 0 gwx fwx ewx dwx cwx bwx awx 0

    FRegSaveCnt         : Integer;
    FVarSizeInStack,
    FParSizeInStack     : Integer;
    FNeeds              : TNeedsList;

    Function GetTargReg ( AIndex : Integer ) : string;
    Procedure InitTargRegMap;
    Function IsRegAwxImpass ( ATextIdx : Integer ) : boolean;
    Function IsRegSuitable ( ATargIdx : Integer; AWww : byte ) : boolean;
    Function TryAssignReg ( ADstTargIdx : Integer; ARow, AWww : byte; ALineToSkip, ATargToSkip : Integer; Out AConfl : TConfl ) : boolean;
    Procedure SelectBestConfl ( Const AConflList : TConflList; Out AConfl : TConfl );

    Function MapRegs ( Out AConfl : TConfl ) : boolean;

    Function HeaviestCallResult : Integer;
    Function MapCallResult ( Var AConfl : TConfl ) : boolean;
    Function TargRequiresLowerRegPart ( ATargIdx : Integer ) : boolean;
    Function TargIsLowerRegPartDst ( ATargIdx : Integer ) : boolean;
    Function HeaviestAny ( ATypeC : char ) : Integer;
    Function CheckConflA ( ATargIdxA, ATargIdxB : Integer; ARow, AWww : byte ) : boolean;
    Procedure CleanupSD;
    Procedure CleanupPart;
    Function MapAny ( Out AConfl : TConfl ) : boolean;
    Function MapAny ( Const ARegList : TRegDescrList; ATypeC : char; Var AConfl : TConfl ) : boolean;

    Function FormatZReg ( Const ADstReg : string ) : string;
    Function McdAppendCmd ( ATextIdx : Integer; ALineA, ALineB, ALineC : TFlowLine; Const ATail : string ) : Integer;

    Procedure CopyVarToStack;
    Procedure CopyVarFromStack;
    Function GetTargOffset ( ATargIdx : Integer ) : Integer;
    Function GetTargOffset ( Const ATarg : string ) : Integer;
    Function GetTargRef ( Const ATarg : string ) : Integer;
    Procedure CreateHeaderFooterA;
    Procedure CreateHeaderFooterB;
    Function GetTargIdx ( ATextIdx : Integer; Const AParam : string ) : Integer;
    Procedure CorrectCmdName ( ALine : TFlowLine );
    Procedure CorrectCmdNames;
    Procedure FinalWrRegs;

    Function DbgRegMap ( ATextIdx : Integer ) : string;

  protected
    Function CanBeUsedAsIndex ( Const ATarg : string ) : boolean; Override;
    Function IsValidArrayIdx ( Const ATarg : string ) : boolean; Override;
    Function BestTypeSuitableForPush ( Const AType : string ) : string; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure DebugInfo ( AList : TStringList ); Override;
    Procedure GenAsmSrc ( AAsmSrc : TStringList ); Override;

    property Needs : TNeedsList read FNeeds;
  end;

implementation

Uses
  ConComL;


// When adding types, change also GetTypeSize and MapAny

Function TypeCToWww ( ATypeC : char ) : byte;
Begin
 Result:=0;
 case ATypeC of
   'l': Result:=$1;
   'c': Result:=$1;
   'b': Result:=$1;
   'k': Result:=$1;
   'w': Result:=$3;
   'm': Result:=$3;
   'd': Result:=$7;
   'i': Result:=$7;
   'p': Result:=$7;
   'f': Result:=$7;
 end;
End;

Function GetDstWww ( Const ACmd : string ) : byte;
Begin
 if StrInList(ACmd,CMov_BW) then Result:=$1
 else if StrInList(ACmd,CMov_BD) then Result:=$1
 else if StrInList(ACmd,CMov_WD) then Result:=$3
 else Result:=0;
End;

Function RowWwwToReg ( ARow, AWww : byte ) : string;
Begin
 Result:='';
 repeat
 if AWww=0 then break;
 case ARow of
   1: Result:='a';
   2: Result:='b';
   3: Result:='c';
   4: Result:='d';
   5: Result:='e';
   6: Result:='f';
   7: Result:='g';
 end;
 case AWww of
   $1: Result:=Result+'l';
   $2: Result:=Result+'h';
   $3: Result:=Result+'x';
   $4: Result:=Result+'w';
   $7: Result:=Result+'wx';
   $8: Result:=Result+'r';
   $F: Result:=Result+'q';
   else Result:=Result+'?';
 end;
 until TRUE;
End;

Function RegLowerPart ( Const AReg : string; APart : char ) : string;
Begin
 Result:='';
 repeat
 if AReg='' then break;
 if Length(AReg)=2 then
  begin
  if (AReg[2]='x') and (APart='b') then begin Result:=AReg[1]+'l'; break; end;
  if (AReg[2]='r') and (APart='b') then begin Result:=AReg[1]+'l'; break; end;
  if (AReg[2]='q') and (APart='b') then begin Result:=AReg[1]+'l'; break; end;
  if (AReg[2]='r') and (APart='w') then begin Result:=AReg[1]+'x'; break; end;
  if (AReg[2]='q') and (APart='w') then begin Result:=AReg[1]+'x'; break; end;
  if (AReg[2]='r') and (APart='d') then begin Result:=AReg[1]+'wx'; break; end;
  if (AReg[2]='q') and (APart='d') then begin Result:=AReg[1]+'wx'; break; end;
  end;
 if Length(AReg)=3 then
  begin
  if (Copy(AReg,2,2)='wx') then
   begin
   if APart='b' then begin Result:=AReg[1]+'l'; break; end;
   if APart='w' then begin Result:=AReg[1]+'x'; break; end;
   end;
  end;
 until TRUE;
End;

Function ExtractConstOpti ( Const ATarg : string ) : string;
Var
  BTypeS        : string;
  BDataI        : Integer;
  BDataC        : char;
  BPos          : Integer;
Begin
 Result:=ParsExtractName(ATarg);
 BTypeS:=ParsExtractType(ATarg);
 if BTypeS='c' then
  begin
  StringToInteger(Result,BDataI);
  BDataC:=Chr(BDataI);
  if BDataC in [#33..#38, #40..#126] then Result:=#39+BDataC+#39;
  end
 else if (BTypeS='f') and (Pos('.',Result)=0) then
  begin
  BPos:=Pos('e',Result);
  if BPos=0 then BPos:=Pos('E',Result);
  if BPos=0 then Result:=Result+'.0'
  else Insert('.0',Result,BPos);
  end;
End;

Const
  CRegPplList   : string = ('gr fr er dr cr br ar 0 gwx fwx ewx dwx cwx bwx awx 0');

Function GetPushPopList ( ARegList : word ) : string;
Var
  BRegPplList   : string;
  BRegIdx       : Integer;
  BParamS       : string;
Begin
 Result:='';
 BRegPplList:=CRegPplList;
 for BRegIdx:=0 to 15 do
  begin
  BParamS:=ReadParamStr(BRegPplList);
  if (ARegList and ($8000 shr BRegIdx))<>0 then
   begin
   if Result<>'' then Result:='|'+Result;
   Result:=BParamS+Result;
   end;
  end;
End;

{Function GetPopList ( ARegList : word ) : string;
Var
  BRegPplList   : string;
  BRegIdx       : Integer;
  BParamS       : string;
Begin
 Result:='';
 BRegPplList:=CRegPplList;
 for BRegIdx:=0 to 15 do
  begin
  BParamS:=ReadParamStr(BRegPplList);
  if (ARegList and ($8000 shr BRegIdx))<>0 then
   begin
   if Result<>'' then Result:=Result+'|';
   Result:=Result+BParamS;
   end;
  end;
End;}

Constructor TModuleMs.Create;
Begin
 Inherited;
 FIndexType:='d';
 FBaseTypeI:='i';
 FBaseTypeU:='d';
 FStackGenericType:='d';
End;

Destructor TModuleMs.Destroy;
Begin
 Inherited;
End;

Function TModuleMs.AppendProc : TLlvmProc;
Begin
 Result:=TProcMs.Create;
 AppendProcA(Result);
End;

Procedure TModuleMs.AlignSize ( Var AData : Integer );
Begin
 AData:=AData+((4-(AData and $3)) and $3);
End;

Function TModuleMs.GetTypeSize ( Const ATypeS : string ) : Integer;
Var
  BSizeS        : string;
  BPos          : Integer;
  BDimSStr,
  BDimEStr      : string;
  BFinalType    : string;
  BDimS,
  BDimE         : Integer;
Begin
 Result:=0;
 if ATypeS='l' then Result:=1
 else if ATypeS='c' then Result:=1
 else if ATypeS='b' then Result:=1
 else if ATypeS='k' then Result:=1
 else if ATypeS='w' then Result:=2
 else if ATypeS='m' then Result:=2
 else if ATypeS='d' then Result:=4
 else if ATypeS='i' then Result:=4
 else if ATypeS='p' then Result:=4
 else if ATypeS='f' then Result:=4
 else if ParsIsTypePointer(ATypeS) then Result:=4
 else if ParsIsTypeArray(ATypeS) then
  begin
  ParsGetTypeArrayDim(ATypeS,BDimSStr,BDimEStr,BFinalType);
  if TryStrToInt(BDimSStr,BDimS)=FALSE then AppendError('e',1,1,'Internal error: array dimension conversion error [R:TModuleMs.GetTypeSize]');
  if TryStrToInt(BDimEStr,BDimE)=FALSE then AppendError('e',1,1,'Internal error: array dimension conversion error [R:TModuleMs.GetTypeSize]');
  if BDimE<BDimS then AppendError('e',1,1,'Internal error: negative array dimension [R:TModuleMs.GetTypeSize]');
  Result:=(BDimE-BDimS+1)*GetTypeSize(BFinalType);
  end
 else if ParsIsTypeRecord(ATypeS) then
  begin
  BPos:=Pos('e',ATypeS);
  BSizeS:=Copy(ATypeS,2,BPos-2);
  TryStrToInt(BSizeS,Result);
  end
 else if ParsIsTypeStringP(ATypeS) then
  begin
  BPos:=Pos('e',ATypeS);
  BSizeS:=Copy(ATypeS,3,BPos-3);
  TryStrToInt(BSizeS,Result);
  Result:=Result+1; // Byte 0 is length
  end;
End;

Procedure TModuleMs.WriteDataSeg ( AAsmSrc : TStringList );
Var
  BDeclVarList  : string; // variables which we need to declare, i.e. without Externals
  BVarList,
  BVarName      : string;
  BVarType      : string;
  BTypeSize     : Integer;
  BAlignSet     : boolean;
  BDummyS       : string;
Begin
 if FVarList<>'' then
  begin
  AAsmSrc.Append('');
  AAsmSrc.Append('.seg '+FDataSegName);
  // Other vars (Size>4)
  BDeclVarList:=ParsRemoveExtern(FVarList);
  BVarList:=BDeclVarList;
  repeat
  BVarName:=ReadParamStr(BVarList);
  if BVarName='' then break;
  BVarType:=ParsExtractType(BVarName);
  BTypeSize:=GetTypeSize(BVarType);
  if BTypeSize in [4, 2, 1] then
  else
   begin
   AAsmSrc.Append('        Align 4');
   BDummyS:='   '+ParsExtractName(BVarName)+':';
   AddSpacesVarR(BDummyS,25);
   BDummyS:=BDummyS+'db 0 dup('+IntToStr(BTypeSize)+')';
   AAsmSrc.Append(BDummyS);
   end;
  until FALSE;

  // Vars of size DD
  BAlignSet:=FALSE;
  BVarList:=BDeclVarList;
  repeat
  BVarName:=ReadParamStr(BVarList);
  if BVarName='' then break;
  BVarType:=ParsExtractType(BVarName);
  BTypeSize:=GetTypeSize(BVarType);
  if BTypeSize=4 then
   begin
   if BAlignSet=FALSE then begin AAsmSrc.Append('        Align 4'); BAlignSet:=TRUE; end;
   BDummyS:='   '+ParsExtractName(BVarName)+':';
   AddSpacesVarR(BDummyS,25);
   BDummyS:=BDummyS+'dd 0';
   AAsmSrc.Append(BDummyS);
   end;
  until FALSE;

  // Vars of size DW
  BAlignSet:=FALSE;
  BVarList:=BDeclVarList;
  repeat
  BVarName:=ReadParamStr(BVarList);
  if BVarName='' then break;
  BVarType:=ParsExtractType(BVarName);
  BTypeSize:=GetTypeSize(BVarType);
  if BTypeSize=2 then
   begin
   if BAlignSet=FALSE then begin AAsmSrc.Append('        Align 2'); BAlignSet:=TRUE; end;
   BDummyS:='   '+ParsExtractName(BVarName)+':';
   AddSpacesVarR(BDummyS,25);
   BDummyS:=BDummyS+'dw 0';
   AAsmSrc.Append(BDummyS);
   end;
  until FALSE;

  // Vars of size DB
  BAlignSet:=FALSE;
  BVarList:=BDeclVarList;
  repeat
  BVarName:=ReadParamStr(BVarList);
  if BVarName='' then break;
  BVarType:=ParsExtractType(BVarName);
  BTypeSize:=GetTypeSize(BVarType);
  if BTypeSize=1 then
   begin
   if BAlignSet=FALSE then begin AAsmSrc.Append('        Align 1'); BAlignSet:=TRUE; end;
   BDummyS:='   '+ParsExtractName(BVarName)+':';
   AddSpacesVarR(BDummyS,25);
   BDummyS:=BDummyS+'db 0';
   AAsmSrc.Append(BDummyS);
   end;
  until FALSE;

  end;
End;

Function FormatRecField ( Const ATarg : string; AOffset : Integer ) : string;
Var
  BType,
  BName         : string;
Begin
 BType:=ParsExtractType(ATarg);
 BName:=ParsExtractName(ATarg);
 Result:=BType+'o'+IntToStr(AOffset)+'_'+BName;
End;

Procedure TModuleMs.ExtractRecField ( Var ASrcList, ADstList : string; Var AOffset : Integer; ASizeA, ASizeMod : Integer );
Var
  BList,
  BTarg         : string;
  BType         : string;
  BElemSize,
  BTypeSize     : Integer;
  BDimSStr,
  BDimEStr      : string;
  BFinalType    : string;
Begin
 BList:='';
 repeat
 BTarg:=ReadParamStr(ASrcList); if BTarg='' then break;
 BType:=ParsExtractType(BTarg);
 BTypeSize:=GetTypeSize(BType);
 if ParsIsTypeArray(BType) then
  begin
  ParsGetTypeArrayDim(BType,BDimSStr,BDimEStr,BFinalType);
  BElemSize:=GetTypeSize(BFinalType);
  end
 else
  begin
  BElemSize:=BTypeSize;
  end;
 if BElemSize>=ASizeA then begin ADstList:=ADstList+FormatRecField(BTarg,AOffset)+','; AOffset:=AOffset+BTypeSize; while (AOffset mod ASizeMod)<>0 do inc(AOffset); end
 else BList:=BList+BTarg+' ';
 until FALSE;
 ASrcList:=BList;
End;

Procedure TModuleMs.OrderRecordFields ( Const ASrcList : string; Out ADstList : string; Out ARecSize : Integer );
Var
  BOffset       : Integer;
  BSrcList      : string;
Begin
 ADstList:='';
 BOffset:=0;
 // Size>4
 BSrcList:=ASrcList;
 ExtractRecField(BSrcList,ADstList,BOffset,5,4);
 ExtractRecField(BSrcList,ADstList,BOffset,4,4);
 ExtractRecField(BSrcList,ADstList,BOffset,3,4);
 ExtractRecField(BSrcList,ADstList,BOffset,2,2);
 ExtractRecField(BSrcList,ADstList,BOffset,1,1);
 ARecSize:=BOffset;
End;

Procedure TModuleMs.AppendConstStrP ( AAsmSrc : TStringList; Const ATarg, AValue : string );
Var
  BOrigS        : string;
  BLen          : Integer;
  BChar         : char;
  BIsReadable   : boolean;
  BValue        : string;
  BResult       : string;
Begin
 BOrigS:=AValue;
 repeat
 BResult:=ParsExtractName(ATarg)+': ';
 AddSpacesVarR(BResult,25);
 BLen:=Length(BOrigS);
 if BLen>255 then AppendError('e',0,0,'Internal error: String '+BOrigS+' is too long [R:TModuleMs.AppendConstStrP]');
 BResult:=BResult+'db ';
 BResult:=BResult+IntToStr(BLen);
 if BOrigS<>'' then
  begin
  BValue:='';  BIsReadable:=FALSE;
  while BOrigS<>'' do
   begin
   BChar:=BOrigS[1]; Delete(BOrigS,1,1);
   if (BChar<>#39) and (BChar>=#32) and (BChar<='~') then
    begin
    if BIsReadable=FALSE then
     begin
     if BValue<>'' then BValue:=BValue+', ';
     BValue:=BValue+#39;
     end;
    BValue:=BValue+BChar;
    BIsReadable:=TRUE;
    end
   else
    begin
    if BIsReadable then BValue:=BValue+#39;
    if BValue<>'' then BValue:=BValue+', ';
    BValue:=BValue+IntToStr(Ord(BChar));
    BIsReadable:=FALSE;
    end;
   end;
  if BIsReadable then BValue:=BValue+#39;
  end;
 if BValue<>'' then BResult:=BResult+', '+BValue;
 AAsmSrc.Append(BResult);
 until TRUE;

End;

Procedure TModuleMs.AppendConstStrZ ( AAsmSrc : TStringList; Const ATarg, AValue : string );
Var
  BOrigS        : string;
  BLen          : Integer;
  BChar         : char;
  BIsReadable   : boolean;
  BValue        : string;
  BResult       : string;
Begin
 BOrigS:=AValue;
 repeat
 BResult:=ParsExtractName(ATarg)+': ';
 AddSpacesVarR(BResult,25);
 BLen:=Length(BOrigS);
 if BLen>255 then AppendError('e',0,0,'Internal error: String '+BOrigS+' is too long [R:TModuleMs.AppendtConstStrZ]');
 BResult:=BResult+'db ';
 if BOrigS<>'' then
  begin
  BValue:='';  BIsReadable:=FALSE;
  while BOrigS<>'' do
   begin
   BChar:=BOrigS[1]; Delete(BOrigS,1,1);
   if (BChar<>#39) and (BChar>=#32) and (BChar<='~') then
    begin
    if BIsReadable=FALSE then
     begin
     if BValue<>'' then BValue:=BValue+', ';
     BValue:=BValue+#39;
     end;
    BValue:=BValue+BChar;
    BIsReadable:=TRUE;
    end
   else
    begin
    if BIsReadable then BValue:=BValue+#39;
    if BValue<>'' then BValue:=BValue+', ';
    BValue:=BValue+IntToStr(Ord(BChar));
    BIsReadable:=FALSE;
    end;
   end;
  if BIsReadable then BValue:=BValue+#39;
  end;
 if BValue<>'' then BResult:=BResult+BValue+', 0'
 else BResult:=BResult+'0';
 AAsmSrc.Append(BResult);
 until TRUE;
End;

Procedure TModuleMs.AppendConstArray ( AAsmSrc : TStringList; Const ATarg, AValue : string );
Var
  BOrigS,
  BValueS       : string;
  BElemType     : string;
  BElemSize     : Integer;
  BValueRes     : string;
  BResult       : string;
Begin
 BResult:='';
 repeat
 BElemType:=ParsExtractElemType(ATarg);
 BElemSize:=GetTypeSize(BElemType);
 if BElemSize>=4 then AAsmSrc.Append('        Align 4')
 else if BElemSize>=2 then AAsmSrc.Append('        Align 2');
 BResult:=ParsExtractName(ATarg)+': ';
 AddSpacesVarR(BResult,25);
 BResult:=BResult+'d'+BElemType+' ';
 BOrigS:=AValue; BValueRes:='';
 while BOrigS<>'' do
  begin
  BValueS:=ReadParamStr(BOrigS,',');
  if BValueS='' then break;
  if BValueRes<>'' then BValueRes:=BValueRes+', ';
  BValueRes:=BValueRes+BValueS;
  end;
 BResult:=BResult+BValueRes;
 AAsmSrc.Append(BResult);
 until TRUE;
End;

Procedure TModuleMs.AppendConsts ( AAsmSrc : TStringList );
Var
  BOrigS        : string;
  BTarg,
  BValueS       : string;
  BIndex        : Integer;
  BPos          : Integer;
Begin
 repeat
 if FConstList.Count=0 then break;
 for BIndex:=0 to FConstList.Count-1 do
  begin
  BOrigS:=FConstList.Strings[BIndex];
  BPos:=Pos(' ',BOrigS);
  if BPos=0 then begin AppendError('e',0,0,'Internal error: String constant has no value [R:TModuleMs.AppendConsts]'); break; end;
  BTarg:=Copy(BOrigS,1,BPos-1); Delete(BOrigS,1,BPos); BValueS:=BOrigS;
  if ParsIsStringP(BTarg) then AppendConstStrP(AAsmSrc,BTarg,BValueS)
  else if ParsIsStringZ(BTarg) then AppendConstStrZ(AAsmSrc,BTarg,BValueS)
  else if ParsIsArray(BTarg) then  AppendConstArray(AAsmSrc,BTarg,BValueS)
  else begin AppendError('e',0,0,'Constant is not recognized by the back-end [R:TModuleMs.AppendConsts]'); break; end;
  end;

 AAsmSrc.Append('');
 AAsmSrc.Append('        Align 4');
 until TRUE;
End;

Procedure TModuleMs.GenAsmSrc ( AAsmSrc : TStringList );
Var
  BProc         : TLlvmProc;
  BIndex,
  BIndexA       : Integer;
  BList         : TStringList;
  BParamList,
  BParam        : string;
  BNeeds        : TNeedsList;
Begin
 Inherited;

 BList:=TStringList.Create;

 AAsmSrc.Clear;
 AAsmSrc.Append(';@M '+FName);
 AAsmSrc.Append(';@S '+Filename);

 AAsmSrc.Append('');
 BParamList:=FPublicNames;
 repeat
 BParam:=ReadParamStr(BParamList);
 if BParam='' then break;
 if (StrInList(BParam,FExternNames)=FALSE) and (ParsIsType(BParam)=FALSE) then AAsmSrc.Append('Public '+ParsExtractName(BParam));
 until FALSE;

 BNeeds:=[];
 for BIndex:=0 to Length(FProcList)-1 do BNeeds:=BNeeds+TProcMs(FProcList[Bindex]).Needs;

 if BNeeds<>[] then AAsmSrc.Append('');
 if enMoveCwx in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcMoveCwx));
 if enMemCopy in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcMemCopy));
 if enStrMovS in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrMovS));
 if enStrAddS in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrAddS));
 if enStrCmpS in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrCmpS));
 if enStrMovC in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrMovC));
 if enStrAddC in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrAddC));
 if enStrCmpC in BNeeds then AAsmSrc.Append('Extern '+ParsExtractName(CProcStrCmpC));

 AAsmSrc.Append('');
 BParamList:=FExternNames;
 repeat
 BParam:=ReadParamStr(BParamList);
 if BParam='' then break;
 AAsmSrc.Append('Extern '+ParsExtractName(BParam));
 until FALSE;

 WriteDataSeg(AAsmSrc);

// AAsmSrc.Append('');
// AAsmSrc.Append('Module private');
// AAsmSrc.Append(FPrivateNames);

// AAsmSrc.Append('');
// AAsmSrc.Append('Module Vars');
// AAsmSrc.Append(FVarList);

 AAsmSrc.Append('');
 AAsmSrc.Append('.seg '+FCodeSegName);

 CleanupConsts;
 AppendConsts(AAsmSrc);

 for BIndex:=0 to Length(FProcList)-1 do
  begin
  BProc:=TLlvmProc(FProcList[BIndex]);
  if (StrInList(BProc.NameL,FExternNames)=FALSE) and BProc.IsImplemented then
   begin
   BProc.GenAsmSrc(BList);
   AAsmSrc.Append('');
   for BIndexA:=0 to BList.Count-1 do AAsmSrc.Append(BList.Strings[BIndexA]);
   end;
  end;

 AAsmSrc.Append('');
 BList.Free;
End;

Constructor TProcMs.Create;
Begin
 Inherited;
 FTextMcd:=TStringList.Create;
End;

Destructor TProcMs.Destroy;
Begin
 FTextMcd.Free;
 FTargRegMapList:=nil;
 //FSrcDstTextMap:=nil;
 Inherited;
End;

Function TProcMs.CanBeUsedAsIndex ( Const ATarg : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=ExtractFinalType(ATarg);
 Result:=StrInList(BType,'b w i d');
End;

Function TProcMs.IsValidArrayIdx ( Const ATarg : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=ExtractFinalType(ATarg);
 Result:=StrInList(BType,'i d');
End;

Function TProcMs.BestTypeSuitableForPush ( Const AType : string ) : string;
Begin
 if StrInList(AType,'m w k b c l') then Result:='d'
 else Result:=AType;
End;

Function TProcMs.DbgRegMap ( ATextIdx : Integer ) : string;
Var
  BTargIdx      : Integer;
  BRegName      : string;
Begin
 Result:='';
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  if FUsageMatr[ATextIdx,BTargIdx]='.' then BRegName:='.'
  else BRegName:=GetTargReg(BTargIdx);
  AddSpacesVarL(BRegName,3);
  Result:=Result+BRegName+' ';
  inc(BTargIdx);
  end;
End;

Procedure TProcMs.DebugInfo ( AList : TStringList );
Var
  BIndex        : Integer;
  BTextIdx      : Integer;
  BReadS,
  BDummyS       : string;
  BVsmItem      : TVsmItem;
  BRegMap       : string;
Begin
 Inherited;

 repeat
 AList.Append('');
 AList.Append('*** BackEnd');
 AList.Append('* Chains');
 BIndex:=0;
 while BIndex<FChTargList.Count do
  begin
  BDummyS:=AddSpacesResL(IntToStr(BIndex),2)+' '+RemoveRecInside(FChTargList.Strings[BIndex]);
  AddSpacesVarR(BDummyS,33);
  BDummyS:=BDummyS+GetTargReg(BIndex);
  AddSpacesVarR(BDummyS,39);
  AList.Append(BDummyS);
  inc(BIndex);
  end;

 AList.Append('');
 AList.Append('* Variables');
 {
 FVarName    : string[128];
 FSize       : Integer;
 FRefOffset,            // Reference in parameter list in case of VAR
 FVarOffset  : Integer; // Offset in stack of a variable or of a copy in case of VAR
 FIsA        : boolean;
 FIsAV,
 FIsAR       : boolean;
 FHasCopy    : boolean;
 }
 AList.Append('Name                   Size RefOfs VarOfs AVRP');
 BIndex:=0;
 while BIndex<Length(FVsmList) do
  begin
  BVsmItem:=FVsmList[BIndex];
  BDummyS:=RemoveRecInside(BVsmItem.VarName)+' ';
  AddSpacesVarR(BDummyS,24);
  BDummyS:=BDummyS+AddSpacesResL(IntToStr(BVsmItem.Size),3)+' ';
  AddSpacesVarR(BDummyS,30);
  if BVsmItem.IsAV or BVsmItem.IsAP then BDummyS:=BDummyS+AddSpacesResL(IntToStr(BVsmItem.RefOffset),4)+' ';
  AddSpacesVarR(BDummyS,37);
  if BVsmItem.HasCopy then BDummyS:=BDummyS+AddSpacesResL(IntToStr(BVsmItem.VarOffset),4)+' ';
  AddSpacesVarR(BDummyS,42);
  if BVsmItem.IsA then BDummyS:=BDummyS+'A' else BDummyS:=BDummyS+'_';
  if BVsmItem.IsAV then BDummyS:=BDummyS+'V' else BDummyS:=BDummyS+'_';
  if BVsmItem.IsAR then BDummyS:=BDummyS+'R' else BDummyS:=BDummyS+'_';
  if BVsmItem.IsAP then BDummyS:=BDummyS+'P' else BDummyS:=BDummyS+'_';
  AList.Append(BDummyS);
  inc(BIndex);
  end;

 //if FBeResult=FALSE then break;

 BDummyS:=''; AddSpacesVarR(BDummyS,67);
 for BIndex:=0 to FChTargList.Count-1 do BDummyS:=BDummyS+AddSpacesResL(IntToStr(BIndex),4);
 AList.Append(BDummyS);

 //if GetErrorCountA<>0 then break;

 BTextIdx:=0;
 while BTextIdx<FTextMcd.Count do
  begin
  BReadS:=FTextMcd.Strings[BTextIdx];
  BRegMap:=ParsTailExtractTagValue(BReadS,'RegMap');
  BDummyS:=DbgFormatFlow(BReadS);
  AddSpacesVarR(BDummyS,67);
  BDummyS:=BDummyS+BRegMap;
  AList.Append(BDummyS);
  inc(BTextIdx);
  end;

 AList.Append('');
 AList.Append('* Time stamps');
 for BIndex:=0 to FTimeStamp.Count-1 do
  begin
  BReadS:=FTimeStamp.Strings[BIndex];
  BDummyS:=ReadParamStr(BReadS);
  ReadParamStr(BReadS);
  BDummyS:=BDummyS+' '+ReadParamStr(BReadS);
  AList.Append(BDummyS);
  end;
 until TRUE;
End;

Function BeFormatFlow ( Const ALine : string ) : string;
Var
  BLine,
  BReadS        : string;
Begin
 Result:='';
 repeat
 BLine:=ALine;
 BReadS:=ReadParamStr(BLine);
 if IsLabel(BReadS) then
  begin
  Result:='     '+BReadS;
  Result:=AddSpacesResR(Result,33);
  Result:=Result+BLine;
  break;
  end;
 Result:='        '+BReadS;
 Result:=AddSpacesResR(Result,17);
 BReadS:=ReadParamStr(BLine);
 if BReadS='' then break;
 if BReadS='#' then
  begin
  DelFirstSpace(BLine);
  DelLastSpace(BLine);
  Result:=Result+BLine;
  end
 else
  begin
  Result:=Result+BReadS;
  repeat
  BReadS:=ReadParamStr(BLine);
  if BReadS='' then break;
  Result:=Result+','+BReadS;
  until FALSE;
  end;

 until TRUE;
End;

Procedure TProcMs.GenAsmSrc ( AAsmSrc : TStringList );
Var
  BLine         : TFlowLine;
  BTextIdx      : Integer;
  BTail         : string;
Begin
 Inherited;
 BLine:=TFlowLine.Create;

 AAsmSrc.Append(DbgProcHeader);
 AAsmSrc.Append(FName+':');
 for BTextIdx:=0 to FTextMcd.Count-1 do
  begin
  BLine.RdLine(FTextSrc.Strings[BTextIdx]);
  BTail:=BLine.Tail; CheckTag('M',BTail); // Module
  AAsmSrc.Append(AddSpacesResR(BeFormatFlow(BLine.Exec),33)+' ;'+BTail);
  end;

 BLine.Free;
End;

Procedure TProcMs.Compile;
Var
  BConfl        : TConfl;
Begin
 Inherited;

 repeat
 if FFatalError then break;

 //DbgSave(Self,'G_BEnd',FFlow,TRUE);
 repeat
 //if FModule.Name='PasMainA' then
 // begin
 // Sleep(0);
 // end;
 FlowClearRemoved;
 if MapRegs(BConfl) then begin {BResult:=TRUE;} break; end;
 //DbgSave(Self,'G_BEnd',FFlow,TRUE);
 SplitChain(BConfl.FTextIdx,BConfl.FTargIdx);
 RecompileChainsOpti;
 RecompileWeightsOpti;
 if GetErrorCountA<>0 then break;
 until FALSE;

 MarkStackDepth;
 CreateHeaderFooterA;
 CorrectCmdNames;
 FinalWrRegs;
 CreateHeaderFooterB;
 until TRUE;

 FFlowList.Assign(FTextDst);
 FTextMcd.Assign(FTextDst);

 TimeStamp('BackEnd');
 if FModule.DbgPath<>'' then DbgSave(Self,'G_BEnd',FFlowList,TRUE);
End;

Procedure TProcMs.CreateHeaderFooterA;
Var
  BRegIdx,
  BTargIdx      : Integer;
  BTypeC        : string;
  BVarIdx       : Integer;
  BOffset       : Integer;
  BVsmItem      : TVsmItem;
Begin
 FRegSaveList:=0;
 for BTargIdx:=0 to Length(FTargRegMapList)-1 do
  begin
  if (FTargRegMapList[BTargIdx].FAsgnWww and $7)<>0 then FRegSaveList:=FRegSaveList or (1 shl (FTargRegMapList[BTargIdx].FAsgnRow+0));
  if (FTargRegMapList[BTargIdx].FAsgnWww and $8)<>0 then FRegSaveList:=FRegSaveList or (1 shl (FTargRegMapList[BTargIdx].FAsgnRow+8));
  end;
 BTypeC:=ParsExtractType(FNameL);
 if (BTypeC<>'') and (BTypeC<>'_') then FRegSaveList:=FRegSaveList or $0002; // If return result, awx have to save in any case
 if BTypeC='q' then FRegSaveList:=FRegSaveList or $0200;

 VsmListInit;

 FNeeds:=[];

 // Refine register to save list
 BVarIdx:=0;
 while BVarIdx<Length(FVsmList) do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.IsAV then
   begin
   if ParsIsRecord(BVsmItem.VarName) then
   else if BVsmItem.Size<=4 then FRegSaveList:=FRegSaveList or $0006 // Save awx, bwx
   else begin FRegSaveList:=FRegSaveList or $002C; FNeeds:=FNeeds+[enMoveCwx]; end; // Save bwx, cwx, ewx
   end;
  inc(BVarIdx);
  end;

 FRegSaveList:=FRegSaveList or $0002; // Can refine this later

 // How many registers to save?
 FRegSaveCnt:=0;
 for BRegIdx:=0 to 15 do
  begin
  if (FRegSaveList and (1 shl BRegIdx))<>0 then inc(FRegSaveCnt);
  end;

 //VarStackMapListMap;
 // assign offsets
 BOffset:=0;
 // Byte map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.HasCopy and (BVsmItem.Size=1) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   end;
  dec(BVarIdx);
  end;
 while (BOffset and $1)<>0 do inc(BOffset);
 // Word map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.HasCopy and (BVsmItem.Size=2) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   end;
  dec(BVarIdx);
  end;
 while (BOffset and $3)<>0 do inc(BOffset);
 // DWord map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.HasCopy and (BVsmItem.Size=4) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   end;
  dec(BVarIdx);
  end;
 // Arrays map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if ParsIsArray(BVsmItem.VarName) and (BVsmItem.IsAV=FALSE) and (BVsmItem.IsAP=FALSE) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   while (BOffset and $3)<>0 do inc(BOffset);
   end;
  dec(BVarIdx);
  end;
 // Strings map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if ParsIsStringP(BVsmItem.VarName) and (BVsmItem.IsAV=FALSE) and (BVsmItem.IsAP=FALSE) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   while (BOffset and $3)<>0 do inc(BOffset);
   end;
  dec(BVarIdx);
  end;
 // Records map
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if ParsIsRecord(BVsmItem.VarName) and (BVsmItem.IsAV=FALSE) and (BVsmItem.IsAP=FALSE) then
   begin
   BVsmItem.VarOffset:=BOffset;
   inc(BOffset,BVsmItem.Size);
   while (BOffset and $3)<>0 do inc(BOffset);
   end;
  dec(BVarIdx);
  end;

 FVarSizeInStack:=BOffset;
 // Params map
 BOffset:=BOffset+4+FRegSaveCnt*4;
 FParSizeInStack:=BOffset;
 BVarIdx:=Length(FVsmList)-1;
 while BVarIdx>=0 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.IsA then
   begin
   if BVsmItem.IsAR then // Return value
    begin
    BVsmItem.VarOffset:=FVarSizeInStack+(FRegSaveCnt-1)*4;
    end
   else if BVsmItem.IsAV or BVsmItem.IsAP then
    begin
    BVsmItem.RefOffset:=BOffset;
    inc(BOffset,4);
    end
   else
    begin
    BVsmItem.RefOffset:=BOffset;
    BVsmItem.VarOffset:=BOffset;
    if BVsmItem.Size<=4 then inc(BOffset,4) // yes, add 4 even for words and bytes
    else inc(BOffset,BVsmItem.Size);
    while (BOffset and $3)<>0 do inc(BOffset);
    end;
   end;
  dec(BVarIdx);
  end;
 FParSizeInStack:=BOffset-FParSizeInStack;

End;

Function GenEsp ( AEsp : Integer ) : string;
Begin
 Result:='[Esp:'+IntToStr(AEsp)+']';
End;

Procedure TProcMs.CreateHeaderFooterB;
Var
  BTextIdx      : Integer;
  BEsp          : Integer;
  BTailS        : string;
Begin
 FTextDst.Clear;
 BEsp:=-FVarSizeInStack-FRegSaveCnt*4;
 BTailS:=' ; '+ParsTailGenA(FModule.Filename,FStartLine,FStartPos,GenEsp(BEsp),'');
 if FVarSizeInStack<1024 then FTextDst.Append('enter '+GetPushPopList(FRegSaveList)+','+IntToStr(FVarSizeInStack)+BTailS)
 else
  begin
  FTextDst.Append('enter '+GetPushPopList(FRegSaveList)+','+IntToStr(0)+BTailS);
  FTextDst.Append('sub esp,'+IntToStr(FVarSizeInStack)+BTailS);
  end;
 Inc(BEsp,FRegSaveCnt*4);
 Inc(BEsp,FVarSizeInStack);
 FTextDst.Append(BTailS);

 // Copy Var as parameter to their place in stack
 CopyVarToStack;

 FDbgInfoS:=FTextDst.Count;

 for BTextIdx:=0 to FTextSrc.Count-1 do FTextDst.Append(FTextSrc.Strings[BTextIdx]);

 // Return back Var as parameter to their place in stack
 CopyVarFromStack;

 BTailS:=' ; '+ParsTailGenA(FModule.Filename,FEndLine,FEndPos,GenEsp(BEsp),'');
 FTextDst.Append(BTailS);
 if FVarSizeInStack<1024 then FTextDst.Append('leave '+GetPushPopList(FRegSaveList)+','+IntToStr(FVarSizeInStack)+','+IntToStr(FParSizeInStack)+BTailS)
 else
  begin
  FTextDst.Append('add esp,'+IntToStr(FVarSizeInStack)+BTailS);
  FTextDst.Append('leave '+GetPushPopList(FRegSaveList)+','+IntToStr(0)+','+IntToStr(FParSizeInStack)+BTailS)
  end;
 Dec(BEsp,FVarSizeInStack);
 Dec(BEsp,FRegSaveCnt*4);

 FTextSrc.Assign(FTextDst);
End;

Procedure TProcMs.CopyVarToStack;
Var
  BVarIdx       : Integer;
  BVsmItem      : TVsmItem;
  BMessageWr    : boolean;
Begin
 BMessageWr:=FALSE;
 BVarIdx:=0;
 while BVarIdx<Length(FVsmList) do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  repeat
  if BVsmItem.IsAV=FALSE then break;
  if ParsIsRecord(BVsmItem.VarName) or ParsIsArray(BVsmItem.VarName) or ParsIsStringP(BVsmItem.VarName) then break;
  if BMessageWr=FALSE then
   begin
   FTextDst.Append('; Make VAR copy in stack ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   BMessageWr:=TRUE;
   end;
  FTextDst.Append('mov bwx [esp+'+IntToStr(BVsmItem.RefOffset)+'] ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
  if BVsmItem.Size=1 then
   begin
   FTextDst.Append('mov al [bwx] ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('mov [esp+'+IntToStr(BVsmItem.VarOffset)+'] al ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   end
  else if BVsmItem.Size=2 then
   begin
   FTextDst.Append('mov ax [bwx] ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('mov [esp+'+IntToStr(BVsmItem.VarOffset)+'] ax ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   end
  else if BVsmItem.Size=4 then
   begin
   FTextDst.Append('mov awx [bwx] ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('mov [esp+'+IntToStr(BVsmItem.VarOffset)+'] awx ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   end
  else
   begin
   FTextDst.Append('mov cwx '+IntToStr(BVsmItem.Size)+' ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('mov ewx esp ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('add ewx '+IntToStr(BVsmItem.VarOffset)+' ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   FTextDst.Append('call MoveCwx ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,''));
   end;
  until TRUE;
  inc(BVarIdx);
  end;
End;

Procedure TProcMs.CopyVarFromStack;
Var
  BVarIdx       : Integer;
  BVsmItem      : TVsmItem;
  BMessageWr    : boolean;
Begin
 BMessageWr:=FALSE;
 BVarIdx:=0;
 while BVarIdx<Length(FVsmList) do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  repeat
  if BVsmItem.IsAV=FALSE then break;
  if ParsIsRecord(BVsmItem.VarName) or ParsIsArray(BVsmItem.VarName) or ParsIsStringP(BVsmItem.VarName) then break;
  if BMessageWr=FALSE then
   begin
   FTextDst.Append('; Copy VAR back to memory ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   BMessageWr:=TRUE;
   end;
  FTextDst.Append('mov ewx [esp+'+IntToStr(BVsmItem.RefOffset)+'] ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
  if BVsmItem.Size=1 then
   begin
   FTextDst.Append('mov al [esp+'+IntToStr(BVsmItem.VarOffset)+'] ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('mov [ewx] al ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   end
  else if BVsmItem.Size=2 then
   begin
   FTextDst.Append('mov ax [esp+'+IntToStr(BVsmItem.VarOffset)+'] ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('mov [ewx] ax ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   end
  else if BVsmItem.Size=4 then
   begin
   FTextDst.Append('mov awx [esp+'+IntToStr(BVsmItem.VarOffset)+'] ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('mov [ewx] awx ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   end
  else
   begin
   FTextDst.Append('mov cwx '+IntToStr(BVsmItem.Size)+' ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('mov bwx esp ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('add bwx '+IntToStr(BVsmItem.VarOffset)+' ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   FTextDst.Append('call MoveCwx ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
   end;
  until TRUE;
  inc(BVarIdx);
  end;
End;

Function TProcMs.GetTargOffset ( ATargIdx : Integer ) : Integer;
Begin
 Result:=GetTargOffset(FChTargList.Strings[ATargIdx]);
End;

Function TProcMs.GetTargOffset ( Const ATarg : string ) : Integer;
Var
  BVarIdx       : Integer;
  BVsmItem      : TVsmItem;
Begin
 Result:=0;
 for BVarIdx:=0 to Length(FVsmList)-1 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.VarName=ATarg then begin Result:=BVsmItem.VarOffset; break; end;
  end;
End;

Function TProcMs.GetTargRef ( Const ATarg : string ) : Integer;
Var
  BVarIdx       : Integer;
  BVsmItem      : TVsmItem;
Begin
 Result:=0;
 for BVarIdx:=0 to Length(FVsmList)-1 do
  begin
  BVsmItem:=FVsmList[BVarIdx];
  if BVsmItem.VarName=ATarg then begin Result:=BVsmItem.RefOffset; break; end;
  end;
End;

Function TProcMs.GetTargReg ( AIndex : Integer ) : string;
Begin
 Result:=RowWwwToReg(FTargRegMapList[AIndex].FAsgnRow,FTargRegMapList[AIndex].FAsgnWww);
End;

Function TProcMs.FormatZReg ( Const ADstReg : string ) : string;
Var
  BDstReg   : string;
Begin
 BDstReg:=ADstReg;
 Delete(BDstReg,1,1);
 if BDstReg='l' then Result:='zl'
 else if BDstReg='h' then Result:='zl'
 else if BDstReg='x' then Result:='zx'
 else if BDstReg='w' then Result:='zx'
 else if BDstReg='wx' then Result:='zwx'
 else if BDstReg='r' then Result:='zwx'
 else if BDstReg='q' then Result:='zq'
 else Result:='?';
End;

Function TProcMs.McdAppendCmd ( ATextIdx : Integer; ALineA, ALineB, ALineC : TFlowLine; Const ATail : string ) : Integer;
Var
  BTargIdxA,
  BTargIdxB,
  BTargIdxC,
  BTargIdxD,
  BTargIdxI,
  BTargIdxArr   : Integer;
  BRegA,
  BRegB,
  BRegC,
  BRegD,
  BRegI,
  BRegArr       : string;
  BName         : string;
  BRegS         : string;
  BTypeC        : string;
  BRegAe,
  BRegBe        : string;
  BParamIdx,
  BParamArr     : string;
  BDummyInt,
  BOffset       : Integer;
  BCmdA,
  BCmdB         : string;
  BTypeS        : string;
  BTypeSize     : Integer;
  BFound        : boolean;
  BConstS       : string;
  BConstI       : Integer;
Begin
 BTargIdxA:=GetTargIdx(ATextIdx,ALineA.Param[0].Targ);
 BTargIdxB:=GetTargIdx(ATextIdx,ALineA.Param[1].Targ);
 BRegA:=''; if BTargIdxA<>-1 then BRegA:=GetTargReg(BTargIdxA);
 BRegB:=''; if BTargIdxB<>-1 then BRegB:=GetTargReg(BTargIdxB);
 BRegAe:=BRegA; if BRegAe='' then BRegAe:='?';
 BRegBe:=BRegB; if BRegBe='' then BRegBe:='?';
 Result:=1;
 repeat
 if ALineA.Removed then break;
 if ALineA.IsLabel then
  begin
  AppendCmd(ALineA.Cmd,'','',ATail);
  break;
  end;
 if ALineA.IsNop then
  begin
  if (ATextIdx=0) or ((ATextIdx+1)=Length(FFlow)) then
  else AppendCmd(ALineA.Cmd,'','',ATail);
  break;
  end;
 if ALineA.IsJxx then
  begin
  if ALineA.IsJmp then BCmdA:='bra'
  else begin BCmdA:=ALineA.Cmd; if (BCmdA<>'') and (BCmdA[1]='j') then BCmdA[1]:='b'; end;
  AppendCmd(BCmdA,ALineA.Param[0].Targ,'',ATail);
  break;
  end;
 if ALineA.IsCall then
  begin
  BName:=ParsExtractName(ALineA.Param[1].Targ);
  AppendCmd('call',BName,'',ATail);
  if Length(ALineA.Param)<3 then begin AppendError('e','Internal error: No stack size parameter [R:TProcMs.McdAppendCmd]'); break; end;
  BConstS:=ExtractConstOpti(ALineA.Param[2].Targ);
  TryStrToInt(BConstS,BConstI);
  if BConstI<>0 then AppendCmd('add','esp',IntToStr(BConstI*4),ATail);
  if ALineA.Param[1].Targ=CProcMemCopy then FNeeds:=FNeeds+[enMemCopy]
  else if ALineA.Param[1].Targ=CProcStrMovS then FNeeds:=FNeeds+[enStrMovS]
  else if ALineA.Param[1].Targ=CProcStrAddS then FNeeds:=FNeeds+[enStrAddS]
  else if ALineA.Param[1].Targ=CProcStrCmpS then FNeeds:=FNeeds+[enStrCmpS]
  else if ALineA.Param[1].Targ=CProcStrMovC then FNeeds:=FNeeds+[enStrMovC]
  else if ALineA.Param[1].Targ=CProcStrAddC then FNeeds:=FNeeds+[enStrAddC]
  else if ALineA.Param[1].Targ=CProcStrCmpC then FNeeds:=FNeeds+[enStrCmpC];
  break;
  end;
 if ALineA.IsPush then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BConstS:=ExtractConstOpti(ALineA.Param[1].Targ);
   if ALineA.FixedCorr<>'' then BConstS:=BConstS+' '+ALineA.FixedCorr;
   if BConstS='0' then AppendCmd('push','zwx','',ATail)
   else AppendCmd('push',BConstS,'',ATail);
   end
  else if ParsIsGlobalOrExtern(ALineA.Param[1].Targ) then
   begin
   BConstS:=ParsExtractName(ALineA.Param[1].Targ);
   if ALineA.FixedCorr<>'' then BConstS:='# '+BConstS+' '+ALineA.FixedCorr;
   AppendCmd('push',BConstS,'',ATail);
   end
  else if Pos('dv',ALineA.Param[0].Targ)=1 then // This is already processed earier
   begin
   AppendCmd('push',BRegB,'',ATail);
   end
  else
   begin
   if ParsIsQ(ALineA.Param[1].Targ) then BTypeC:='q'
   else if ParsIsPointer(ALineA.Param[1].Targ) then BTypeC:='p'
   else BTypeC:=ParsExtractType(ALineA.Param[1].Targ);
   if StrInList(BTypeC,'d i p q f') then AppendCmd('push',BRegB,'',ATail)
   else if StrInList(BTypeC,'l c b k w m') then AppendCmd('pushzx',BRegB,'',ATail)
   else AppendError('e','Internal error: Push of non-suitable reg. Line: "'+ALineA.Orig+'" [R:TProcMs.McdAppendCmd]');
   end;
  break;
  end;

 // Multi line: add awx,bwx,10
 // mov awx,bwx
 // add awx,10
 BFound:=FALSE;
 repeat
 if ALineB=nil then break;
 if ALineA.IsMov=FALSE then break;
 if (BRegA='') or (BRegB='') then break;
 BTargIdxC:=GetTargIdx(ATextIdx+1,ALineB.Param[0].Targ);
 BTargIdxD:=GetTargIdx(ATextIdx+1,ALineB.Param[1].Targ);
 BRegC:=''; if BTargIdxC<>-1 then BRegC:=GetTargReg(BTargIdxC);
 BRegD:=''; if BTargIdxD<>-1 then BRegD:=GetTargReg(BTargIdxD);
 if BRegC<>BRegA then break;
 if StrInList(ALineB.Cmd,'add sub and or xor mul udiv sdiv fadd fsub fmul fdiv')=FALSE then break;
 if BRegD<>'' then
  begin
  AppendCmd(ALineB.Cmd,BRegC,BRegB,BRegD,ATail);
  BFound:=TRUE;
  break;
  end;
 if ParsIsConst(ALineB.Param[1].Targ) then
  begin
  AppendCmd(ALineB.Cmd,BRegC,BRegB,ExtractConstOpti(ALineB.Param[1].Targ),ATail);
  BFound:=TRUE;
  break;
  end;
 until TRUE;
 if BFound then begin Result:=2; break; end;

 // Multi line: "mov bwx,2" + "add bwx,awx" = "add bwx,awx,2"
 BFound:=FALSE;
 repeat
 if ALineB=nil then break;
 if ALineA.IsMov=FALSE then break;
 BTargIdxC:=GetTargIdx(ATextIdx+1,ALineB.Param[0].Targ);
 BTargIdxD:=GetTargIdx(ATextIdx+1,ALineB.Param[1].Targ);
 BRegC:=''; if BTargIdxC<>-1 then BRegC:=GetTargReg(BTargIdxC);
 BRegD:=''; if BTargIdxD<>-1 then BRegD:=GetTargReg(BTargIdxD);
 if (BRegA='') or (BRegD='') then break;
 if BRegC<>BRegA then break;
 if StrInList(ALineB.Cmd,'add and or xor mul fadd')=FALSE then break;
 if BRegB<>'' then
  begin
  AppendCmd(ALineB.Cmd,BRegC,BRegD,BRegB,ATail);
  BFound:=TRUE;
  break;
  end;
 if ParsIsConst(ALineA.Param[1].Targ) then
  begin
  AppendCmd(ALineB.Cmd,BRegC,BRegD,ExtractConstOpti(ALineA.Param[1].Targ),ATail);
  BFound:=TRUE;
  break;
  end;
 until TRUE;
 if BFound then begin Result:=2; break; end;

 // Integer group
 if StrInList(ALineA.Cmd,'mov_wb mov_wc mov_db mov_dw mov_dc') then // To bigger unsigned from unsigned
  begin
  AppendCmd('movzx',BRegA,BRegB,ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_ib mov_iw mov_ic mov_mb mov_mc') then // To bigger signed from unsigned
  begin
  AppendCmd('movzx',BRegA,BRegB,ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_ik mov_im mov_mk') then // To bigger signed from signed
  begin
  AppendCmd('movsx',BRegA,BRegB,ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_dk mov_dm mov_wk mov_wm') then // To bigger unsigned from signed
  begin
  AppendCmd('movsx',BRegA,BRegB,ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_id mov_bc mov_cb mov_bk mov_wm mov_mw') then // Same size
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   AppendCmd('mov',BRegA,ExtractConstOpti(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd('mov',BRegA,ALineA.Param[1].Targ,ATail)
   else if BRegA=BRegB then
   else AppendCmd('mov',BRegA,BRegB,ATail);
   end;
  break;
  end;
 if StrInList(ALineA.Cmd,CMov_BW) then // To Byte from Word
  begin
  BRegS:=RegLowerPart(BRegB,'b');
  if BRegS<>'' then AppendCmd('mov',BRegA,BRegS,ATail)
  else AppendCmd('mov_bw',BRegA,BRegB,ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,CMov_BD) then // To byte from Dword
  begin
  BRegS:=RegLowerPart(BRegB,'b');
  if BRegS<>'' then AppendCmd('mov',BRegA,BRegS,ATail)
  else AppendError('e','Internal error: Mov of variable part from non-suitable reg. Line: "'+ALineA.Orig+'" [R:TProcMs.McdAppendCmd]');
  break;
  end;
 if StrInList(ALineA.Cmd,CMov_WD) then // To Word from Dword
  begin
  BRegS:=RegLowerPart(BRegB,'w');
  if BRegS<>'' then AppendCmd('mov',BRegA,BRegS,ATail)
  else AppendError('e','Internal error: Mov of variable part from non-suitable reg. Line: "'+ALineA.Orig+'" [R:TProcMs.McdAppendCmd]');
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_di mov_wm mov_bk') then
  begin
  AppendCmd('mov',BRegA,BRegB,ATail);
  break;
  end;
 // Float group
 if StrInList(ALineA.Cmd,'mov_fd mov_fi') then
  begin
  if BRegA<>BRegB then AppendCmd('mov',BRegA,BRegB,ATail);
  AppendCmd('itf',BRegA,'',ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_fb mov_fw') then
  begin
  AppendCmd('movzx',BRegA,BRegB,ATail);
  AppendCmd('itf',BRegA,'',ATail);
  break;
  end;
 if StrInList(ALineA.Cmd,'mov_fk mov_fm') then
  begin
  AppendCmd('movsx',BRegA,BRegB,ATail);
  AppendCmd('itf',BRegA,'',ATail);
  break;
  end;

 // Other
 if (ALineA.Cmd='mov') and ParsIsGlobalOrExtern(ALineA.Param[0].Targ) then
  begin
  AppendCmd(ALineA.Cmd,ParsExtractName(ALineA.Param[0].Targ),BRegBe,ATail);
  break;
  end;
 if (ALineA.Cmd='mov') and ParsIsGlobalOrExtern(ALineA.Param[1].Targ) then
  begin
  AppendCmd(ALineA.Cmd,BRegAe,ParsExtractName(ALineA.Param[1].Targ),ATail);
  break;
  end;
 if ALineA.Cmd='mov' then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='0' then AppendCmd(ALineA.Cmd,BRegA,FormatZReg(BRegA),ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BName,ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else if BRegA=BRegB then
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='cmp' then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='0' then AppendCmd(ALineA.Cmd,BRegA,FormatZReg(BRegA),ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BName,ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='add' then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='0' then
   else if BName='1' then AppendCmd('inc',BRegA,'',ATail)
   else AppendCmd(ALineA.Cmd,BRegA,ParsExtractName(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='sub' then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='0' then
   else if BName='1' then AppendCmd('dec',BRegA,'',ATail)
   else AppendCmd(ALineA.Cmd,BRegA,ParsExtractName(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if StrInList(ALineA.Cmd,'and or xor') then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   AppendCmd(ALineA.Cmd,BRegA,BName,ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='not' then
  begin
  BTypeS:=ParsExtractType(ALineA.Param[0].Targ);
  if BTypeS='l' then AppendCmd('xor',BRegA,'0x01',ATail)
  else
   begin
   BTypeSize:=FModule.GetTypeSize(BTypeS);
   case BTypeSize of
     1: AppendCmd('xor',BRegA,'0xFF',ATail);
     2: AppendCmd('xor',BRegA,'0xFFFF',ATail);
     4: AppendCmd('xor',BRegA,'0xFFFFFFFF',ATail);
   else AppendError('e','Internal error: Invalid type for negation [R:TProcMs.McdAppendCmd]');
   end; // case
   end;
  break;
  end;
 if ALineA.Cmd='shl' then
  begin
  //if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[0].Targ)) then BCmdA:='asl' else BCmdA:='shl';
  BCmdA:='shl'; // with shift left use always SHL
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='1' then AppendCmd(BCmdA,BRegA,'',ATail)
   else AppendCmd(BCmdA,BRegA,BRegA,BName,ATail);
   end
  else
   begin
   AppendCmd(BCmdA,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='shr' then
  begin
  if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[0].Targ)) then BCmdA:='asr' else BCmdA:='shr';
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='1' then AppendCmd(BCmdA,BRegA,'',ATail)
   else AppendCmd(BCmdA,BRegA,BRegA,BName,ATail);
   end
  else
   begin
   AppendCmd(BCmdA,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='mul' then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='1' then
   else if BName='2' then AppendCmd('shl',BRegA,'',ATail)
   else if BName='4' then AppendCmd('shl',BRegA,'2',ATail)
   else if BName='8' then AppendCmd('shl',BRegA,'3',ATail)
   else if BName='16' then AppendCmd('shl',BRegA,'4',ATail)
   else if BName='32' then AppendCmd('shl',BRegA,'5',ATail)
   else if BName='64' then AppendCmd('shl',BRegA,'6',ATail)
   else if BName='128' then AppendCmd('shl',BRegA,'7',ATail)
   else AppendCmd(ALineA.Cmd,BRegA,ParsExtractName(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if StrInList(ALineA.Cmd,'udiv sdiv') then
  begin
  if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[0].Targ)) then begin BCmdA:='asr'; BCmdB:='sdiv'; end
  else begin BCmdA:='shr'; BCmdB:='udiv'; end;
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   if BName='1' then
   else if BName='2' then AppendCmd(BCmdA,BRegA,'',ATail)
   else if BName='4' then AppendCmd(BCmdA,BRegA,'2',ATail)
   else if BName='8' then AppendCmd(BCmdA,BRegA,'3',ATail)
   else if BName='16' then AppendCmd(BCmdA,BRegA,'4',ATail)
   else if BName='32' then AppendCmd(BCmdA,BRegA,'5',ATail)
   else if BName='64' then AppendCmd(BCmdA,BRegA,'6',ATail)
   else if BName='128' then AppendCmd(BCmdA,BRegA,'7',ATail)
   else AppendCmd(BCmdB,BRegA,ParsExtractName(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[1].Targ)) then BCmdB:='sdiv';
   if BRegB='' then AppendCmd(BCmdB,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(BCmdB,BRegA,BRegB,ATail);
   end;
  break;
  end;
 if ALineA.Cmd='mod' then
  begin
  if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[0].Targ)) then begin BCmdB:='srem'; end
  else begin BCmdB:='urem'; end;
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   AppendCmd(BCmdB,BRegA,ParsExtractName(ALineA.Param[1].Targ),ATail);
   end
  else
   begin
   if ParsIsTypeSigned(ExtractFinalType(ALineA.Param[1].Targ)) then BCmdB:='srem';
   if BRegB='' then AppendCmd(BCmdB,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(BCmdB,BRegA,BRegB,ATail);
   end;
  break;
  end;
 // fpu
 if StrInList(ALineA.Cmd,'fadd fsub fmul fdiv') then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then
   begin
   BName:=ExtractConstOpti(ALineA.Param[1].Targ);
   AppendCmd(ALineA.Cmd,BRegA,BRegA,BName,ATail);
   end
  else
   begin
   if BRegB='' then AppendCmd(ALineA.Cmd,BRegA,BRegA,ALineA.Param[1].Targ,ATail)
   else AppendCmd(ALineA.Cmd,BRegA,BRegA,BRegB,ATail);
   end;
  break;
  end;
 // Lam, Lea
 if ALineA.IsLam then
  begin
  if ParsIsConst(ALineA.Param[1].Targ) then AppendCmd('mov',BRegAe,ExtractConstOpti(ALineA.Param[1].Targ),ATail)
  else if ParsIsGlobalOrExtern(ALineA.Param[1].Targ) then AppendCmd('mov',BRegAe,ParsExtractName(ALineA.Param[1].Targ),ATail)
  else AppendCmd('mov',BRegAe,'[esp+'+IntToStr(GetTargRef(ALineA.Param[1].Targ)+4*FStackDepth[ATextIdx])+']',ATail);
  break;
  end;
 if ALineA.IsLea then
  begin
  ParsSplitArrayIdx(ALineA.Param[1].Targ,BParamArr,BParamIdx);
  if BParamIdx='' then BDummyInt:=0
  else TryStrToInt(ParsExtractName(BParamIdx),BDummyInt);
  BOffset:=GetTargOffset(BParamArr)+4*FStackDepth[ATextIdx]+BDummyInt;
  if BOffset=0 then AppendCmd('mov',BRegAe,'esp',ATail)
  else AppendCmd('add',BRegAe,'esp',IntToStr(BOffset),ATail);
  break;
  end;
 if ALineA.Cmd='stp_nil' then break;
 if ALineA.IsLdx and (BTargIdxA<>-1) then
  begin
  if ALineA.Cmd='ldp_z' then AppendCmd('mov',BRegA,'0',ATail)
  else AppendCmd('mov',BRegA,'[esp+'+IntToStr(GetTargOffset(BTargIdxA)+4*FStackDepth[ATextIdx])+']',ATail);
  break;
  end;
 if ALineA.IsStx and (BTargIdxA<>-1) then
  begin
  AppendCmd('mov','[esp+'+IntToStr(GetTargOffset(BTargIdxA)+4*FStackDepth[ATextIdx])+']',BRegA,ATail);
  break;
  end;
 // Arrays
 if ALineA.IsMovRm then
  begin
  ParsSplitArrayIdx(ALineA.Param[1].Targ,BParamArr,BParamIdx);
  if ParsIsGlobalOrExtern(ALineA.Param[1].Targ) and (BParamIdx='') then
   begin
   AppendCmd('mov',BRegAe,'['+ParsExtractName(ALineA.Param[1].Targ)+']',ATail);
   break;
   end;
  if ParsIsGlobalOrExtern(ALineA.Param[1].Targ) and ParsIsConst(BParamIdx) then
   begin
   AppendCmd('mov',BRegAe,'['+ParsExtractName(ALineA.Param[1].Targ)+'+'+ParsExtractName(BParamIdx)+']',ATail);
   break;
   end;
  if ParsIsPointer(ALineA.Param[1].Targ) and ParsIsConst(BParamIdx) then
   begin
   BTargIdxArr:=GetTargIdx(ATextIdx,BParamArr);
   BRegArr:=''; if BTargIdxArr<>-1 then BRegArr:=GetTargReg(BTargIdxArr);
   AppendCmd('mov',BRegAe,'['+BRegArr+'+'+ParsExtractName(BParamIdx)+']',ATail);
   break;
   end;
  if ParsIsLocalOrTmp(ALineA.Param[1].Targ) and ParsIsConst(BParamIdx) then
   begin
   TryStrToInt(ParsExtractName(BParamIdx),BDummyInt);
   AppendCmd('mov',BRegAe,'[esp+'+IntToStr(GetTargOffset(BParamArr)+BDummyInt+4*FStackDepth[ATextIdx])+']',ATail);
   break;
   end;
  BTargIdxArr:=GetTargIdx(ATextIdx,BParamArr);
  BRegArr:=''; if BTargIdxArr<>-1 then BRegArr:=GetTargReg(BTargIdxArr);
  BTargIdxI:=GetTargIdx(ATextIdx,BParamIdx);
  BRegI:=''; if BTargIdxI<>-1 then BRegI:=GetTargReg(BTargIdxI);
  if (BRegArr<>'') and (BRegI<>'') then
   begin
   AppendCmd('mov',BRegAe,'['+BRegArr+'+'+BRegI+']',ATail);
   break;
   end;
  if (BRegI<>'') and ParsIsGlobalOrExtern(ALineA.Param[1].Targ) then
   begin
   AppendCmd('mov',BRegAe,'['+ParsExtractName(ALineA.Param[1].Targ)+'+'+BRegI+']',ATail);
   break;
   end;
  BTargIdxArr:=GetTargIdx(ATextIdx,BParamArr);
  BRegArr:=''; if BTargIdxArr<>-1 then BRegArr:=GetTargReg(BTargIdxArr);
  if (BRegArr<>'') and ParsIsLocalOrTmp(BParamArr) then
   begin
   AppendCmd('mov',BRegAe,'['+BRegArr+'+'+BRegI+']',ATail);
   break;
   end;
  AppendError('Internal error: Construction "'+ALineA.Orig+'" is not recognized by the back-end [R:TProcMs.McdAppendCmd]');
  break;
  end;
 if ALineA.IsMovMr then
  begin
  ParsSplitArrayIdx(ALineA.Param[0].Targ,BParamArr,BParamIdx);
  if ParsIsGlobalOrExtern(ALineA.Param[0].Targ) and (BParamIdx='') then
   begin
   AppendCmd('mov','['+ParsExtractName(ALineA.Param[0].Targ)+']',BRegBe,ATail);
   break;
   end;
  if ParsIsGlobalOrExtern(ALineA.Param[0].Targ) and ParsIsConst(BParamIdx) then
   begin
   AppendCmd('mov','['+ParsExtractName(ALineA.Param[0].Targ)+'+'+ParsExtractName(BParamIdx)+']',BRegBe,ATail);
   break;
   end;
  if ParsIsPointer(ALineA.Param[0].Targ) and ParsIsConst(BParamIdx) then
   begin
   BTargIdxArr:=GetTargIdx(ATextIdx,BParamArr);
   BRegArr:=''; if BTargIdxArr<>-1 then BRegArr:=GetTargReg(BTargIdxArr);
   AppendCmd('mov','['+BRegArr+'+'+ParsExtractName(BParamIdx)+']',BRegBe,ATail);
   break;
   end;
  if ParsIsLocalOrTmp(ALineA.Param[0].Targ) and ParsIsConst(BParamIdx) then
   begin
   TryStrToInt(ParsExtractName(BParamIdx),BDummyInt);
   AppendCmd('mov','[esp+'+IntToStr(GetTargOffset(BParamArr)+BDummyInt+4*FStackDepth[ATextIdx])+']',BRegBe,ATail);
   break;
   end;
  BTargIdxArr:=GetTargIdx(ATextIdx,BParamArr);
  BRegArr:=''; if BTargIdxArr<>-1 then BRegArr:=GetTargReg(BTargIdxArr);
  BTargIdxI:=GetTargIdx(ATextIdx,BParamIdx);
  BRegI:=''; if BTargIdxI<>-1 then BRegI:=GetTargReg(BTargIdxI);
  if (BRegArr<>'') and (BRegI<>'') then
   begin
   AppendCmd('mov','['+BRegArr+'+'+BRegI+']',BRegBe,ATail);
   break;
   end;
  if (BRegI<>'') and ParsIsGlobalOrExtern(ALineA.Param[0].Targ) then
   begin
   AppendCmd('mov','['+ParsExtractName(ALineA.Param[0].Targ)+'+'+BRegI+']',BRegBe,ATail);
   break;
   end;
  if (BRegArr<>'') and ParsIsLocalOrTmp(BParamArr) then
   begin
   AppendCmd('mov','['+BRegArr+'+'+BRegI+']',BRegBe,ATail);
   break;
   end;
  AppendError('Internal error: Construction "'+ALineA.Orig+'" is not recognized by the back-end [R:TProcMs.McdAppendCmd]');
  break;
  end;
 AppendError('Internal error: Construction "'+ALineA.Orig+'" is not recognized by the back-end [R:TProcMs.McdAppendCmd]');
 //AppendCmd(ALineA.Cmd,ALineA.Param[0].Targ,ALineA.Param[1].Targ,ATail);
 until TRUE;
End;

Function TProcMs.GetTargIdx ( ATextIdx : Integer; Const AParam : string ) : Integer;
Var
  BTargIdx      : Integer;
  BUsage        : char;
  BTarg         : string;
  BWww          : byte;
Begin
 Result:=-1;
 for BTargIdx:=0 to FChTargList.Count-1 do
  begin
  BUsage:=FUsageMatr[ATextIdx,BTargIdx];
  if BUsage in ['s', 'd', 'x'] then
   begin
   BTarg:=FChTargList.Strings[BTargIdx];
   BWww:=FTargRegMapList[BTargIdx].FAsgnWww;
   if (BWww<>0) and (AParam=BTarg) then begin Result:=BTargIdx; break; end;
   end;
  end;
End;

Procedure TProcMs.CorrectCmdName ( ALine : TFlowLine );
Var
  BTypeA,
  BTypeB        : string;
  BIsFloatA,
  BIsFloatB     : boolean;
Begin
 repeat
 if ALine.IsClassX=FALSE then break;
 if StrInList(ALine.Cmd,'add sub mul div')=FALSE then break;
 if Length(ALine.Param)<2 then break;
 BTypeA:=ParsExtractType(ALine.Param[0].Targ); BTypeB:=ParsExtractType(ALine.Param[1].Targ);
 BIsFloatA:=ParsIsTypeFloat(BTypeA); BIsFloatB:=ParsIsTypeFloat(BTypeB);
 if BIsFloatA or BIsFloatB then
  begin
  if BIsFloatA and BIsFloatB then ALine.CorrectCmd('f'+ALine.Cmd)
  else AppendError('e',ALine,'Internal error: both parameters must be of type Float [R:TProcMs.CorrectCmdName]');;
  break;
  end;
 if ALine.Cmd<>'div' then break;
 if ParsIsTypeSigned(BTypeA) or ParsIsTypeSigned(BTypeB) then ALine.CorrectCmd('s'+ALine.Cmd)
 else ALine.CorrectCmd('u'+ALine.Cmd);
 until TRUE;
End;

Procedure TProcMs.CorrectCmdNames;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlow)-1 do CorrectCmdName(FFlow[BLineIdx]);
End;

Procedure TProcMs.FinalWrRegs;
Var
  BTextIdx      : Integer;
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BTail         : string;
  BInc          : Integer;
Begin
 FTextDst.Clear;
 BTextIdx:=0;
 while BTextIdx<Length(FFlow) do
  begin
  BLineA:=FFlow[BTextIdx+0];
  if (BTextIdx+1)<Length(FFlow) then BLineB:=FFlow[BTextIdx+1] else BLineB:=nil;
  if (BTextIdx+2)<Length(FFlow) then BLineC:=FFlow[BTextIdx+2] else BLineC:=nil;
  BTail:=ParsTailInsertTagResB(BLineA.Tail,'[Esp:'+IntToStr(4*FStackDepth[BTextIdx])+'][TargRegMap:'+DbgRegMap(BTextIdx)+']');
  BInc:=McdAppendCmd(BTextIdx,BLineA,BLineB,BLineC,BTail);
  if BInc=0 then break;
  inc(BTextIdx,BInc);
  end;
 FTextSrc.Assign(FTextDst);
End;

Procedure TProcMs.InitTargRegMap;
Var
  BTarg,
  BSpec         : string;
  BTargIdx,
  BTextIdx      : Integer;
  BWeight         : Single;
  BUsage        : char;
  BAnyUsage     : Integer;
Begin
 SetLength(FTargRegMapList,FChTargList.Count);
 for BTargIdx:=0 to FChTargList.Count-1 do
  begin
  BTarg:=FChTargList.Strings[BTargIdx];
  BSpec:=ParsExtractSpec(BTarg);
  FTargRegMapList[BTargIdx]:=ZTargRegMap;
  FTargRegMapList[BTargIdx].FTypeC:=BSpec[3];
  // Set mapping priority. Each d, x, s increase priority, | just counts
  BWeight:=0; BAnyUsage:=0;
  for BTextIdx:=0 to Length(FFlow)-1 do
   begin
   BUsage:=FUsageMatr[BTextIdx,BTargIdx];
   if BUsage<>'.' then
    begin
    case BUsage of
      'd': BWeight:=BWeight+1;
      'x': BWeight:=BWeight+1;
      's': BWeight:=BWeight+1;
    end;
    inc(BAnyUsage);
    end;
   end;
  if BAnyUsage=0 then BWeight:=0.0
  else BWeight:=BWeight/BAnyUsage;
  FTargRegMapList[BTargIdx].FWeight:=BWeight;
  end;
End;

Function TProcMs.IsRegAwxImpass ( ATextIdx : Integer ) : boolean;
Var
  BLine         : TFlowLine;
  BTypeC        : string;
  BTypeS        : string;
Begin
 Result:=FALSE;
 repeat
 BLine:=FFlow[ATextIdx];
 if BLine.IsPush then
  begin
  //if ParsIsConst(BLine.Param[1].Targ) then begin Result:=TRUE; break; end;
  if ParsIsQ(BLine.Param[1].Targ) then BTypeC:='q'
  else if ParsIsPointer(BLine.Param[1].Targ) then BTypeC:='p'
  else BTypeC:=ParsExtractType(BLine.Param[1].Targ);
  Result:=StrInList(BTypeC,'d i p q f')=FALSE;
  break;
  end;
 if BLine.IsCall and (BLine.Param[0].Targ=CDiscardValue) then
  begin
  BTypeS:=ParsExtractType(BLine.Param[1].Targ);
  Result:=BTypeS<>'_';
  break;
  end;
 //if StrInList(BLine.Cmd,CMov_BW+CMov_BD+CMov_WD) then begin Result:=TRUE; break; end;

 until TRUE;
End;

Function TProcMs.IsRegSuitable ( ATargIdx : Integer; AWww : byte ) : boolean;
Begin
 Result:=TRUE;
 repeat
 if AWww in [$7, $3, $1] then break; // Always suitable
 if TargRequiresLowerRegPart(ATargIdx)=FALSE then break;
 Result:=FALSE;
 until TRUE;
End;

Function TProcMs.TryAssignReg ( ADstTargIdx : Integer; ARow, AWww : byte; ALineToSkip, ATargToSkip : Integer; Out AConfl : TConfl ) : boolean;
Var
  BTextIdx,
  BTargIdx      : Integer;
  BUsageThis,
  BUsageOther   : char;
  BRegMapPtr    : PTargRegMap;
  BWeightThis,
  BWeightOther  : Single;
Begin
 AConfl:=ZConfl;
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-2 do
  begin
  BUsageThis:=FUsageMatr[BTextIdx,ADstTargIdx];
  BWeightThis:=FChWeights[BTextIdx,ADstTargIdx];
  repeat
  if BUsageThis='.' then break;
  if BTextIdx=ALineToSkip then break;
  if (BUsageThis='s') and (FUsageMatr[BTextIdx+1,ADstTargIdx]='.') then break;
  // A* register has special restrictions
  if (ARow=1) and ((AWww and 7)<>0) and IsRegAwxImpass(BTextIdx) then
   begin
   AConfl.FTextIdx:=BTextIdx; AConfl.FTargIdx:=ADstTargIdx;
   if BUsageThis<>'|' then AConfl.FWeight:=2
   else AConfl.FWeight:=FChWeights[BTextIdx,ADstTargIdx];
   break;
   end;
  BTargIdx:=0;
  while BTargIdx<FChTargList.Count do
   begin
   repeat
   if BTargIdx=ADstTargIdx then break;
   if BTargIdx=ATargToSkip then break;
   BUsageOther:=FUsageMatr[BTextIdx,BTargIdx];
   if BUsageOther='.' then break;
   BWeightOther:=FChWeights[BTextIdx,BTargIdx];;
   BRegMapPtr:=@FTargRegMapList[BTargIdx];
   if (BRegMapPtr^.FAsgnRow=ARow) and ((BRegMapPtr^.FAsgnWww and AWww)<>0) then
    begin
    AConfl.FTextIdx:=BTextIdx;
    if (BUsageThis<>'|') and (BUsageOther<>'|') then begin AConfl.FTargIdx:=ADstTargIdx; AConfl.FWeight:=2; end
    else if BUsageThis<>'|' then begin AConfl.FTargIdx:=BTargIdx; AConfl.FWeight:=BWeightOther; end
    else if BUsageOther<>'|' then begin AConfl.FTargIdx:=ADstTargIdx; AConfl.FWeight:=BWeightThis; end
    else if BWeightOther<BWeightThis then begin AConfl.FTargIdx:=BTargIdx; AConfl.FWeight:=BWeightOther; end
    else begin AConfl.FTargIdx:=ADstTargIdx; AConfl.FWeight:=BWeightThis; end;
    break;
    end;
   until TRUE;
   if AConfl.FTextIdx<>-1 then break;
   inc(BTargIdx);
   end;
  until TRUE;
  if AConfl.FTextIdx<>-1 then break;
  inc(BTextIdx);
  end;
 Result:=AConfl.FTextIdx=-1;
End;

Procedure TProcMs.SelectBestConfl ( Const AConflList : TConflList; Out AConfl : TConfl );
Var
  BIndex        : Integer;
Begin
 AConfl:=AConflList[0];
 BIndex:=1;
 while BIndex<Length(AConflList) do
  begin
  if AConflList[BIndex].FWeight<AConfl.FWeight then AConfl:=AConflList[BIndex];
  inc(BIndex);
  end;
End;

Function TProcMs.MapRegs ( Out AConfl : TConfl ) : boolean;
Begin
 AConfl.FTextIdx:=0; AConfl.FTargIdx:=0;
 Result:=FALSE;
 InitTargRegMap;
 repeat
 if MapCallResult(AConfl)=FALSE then break;
 if MapAny(AConfl)=FALSE then break;
 Result:=TRUE;
 until TRUE;
End;

Function TProcMs.HeaviestCallResult : Integer;
Var
  BTargIdx      : Integer;
  BBestWeight   : Single;
Begin
 Result:=-1; BBestWeight:=0;
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  with FTargRegMapList[BTargIdx] do
   begin
   if (FAsgnWww=0) and IsProcCallRes(BTargIdx) then
    begin
    if Result=-1 then begin Result:=BTargIdx; BBestWeight:=FWeight; end
    else if FWeight>BBestWeight then begin Result:=BTargIdx; BBestWeight:=FWeight; end;
    end;
   end;
  inc(BTargIdx);
  end;
End;

Function TProcMs.MapCallResult ( Var AConfl : TConfl ) : boolean;
Var
  BTargIdx      : Integer;
  BRegMapPtr    : PTargRegMap;
  BWww          : Byte;
Begin
 Result:=FALSE;
 repeat
 BTargIdx:=HeaviestCallResult;
 if BTargIdx=-1 then begin Result:=TRUE; break; end;
 BRegMapPtr:=@FTargRegMapList[BTargIdx];
 BWww:=TypeCToWww(BRegMapPtr^.FTypeC);
 if BWww=0 then begin AppendError('Invalid type [r:TProcMs.MapCallResult]'); break; end;
 BRegMapPtr^.FAsgnWww:=BWww;
 BRegMapPtr^.FAsgnRow:=1; // al/ax/awx
 if TryAssignReg(BTargIdx,1,BWww,-1,-1,AConfl)=FALSE then break;
 until FALSE;
End;

Function TProcMs.TargRequiresLowerRegPart ( ATargIdx : Integer ) : boolean;
Var
  BTextIdx      : Integer;
Begin
 Result:=FALSE;
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-2 do
  begin
  if (FUsageMatr[BTextIdx,ATargIdx]='s') and StrInList(FFlow[BTextIdx].Cmd,CMov_BW+CMov_BD+CMov_WD) then
   begin
   Result:=TRUE;
   break;
   end;
  inc(BTextIdx);
  end;
End;

Function TProcMs.TargIsLowerRegPartDst ( ATargIdx : Integer ) : boolean;
Var
  BTextIdx      : Integer;
Begin
 Result:=FALSE;
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-2 do
  begin
  if (FUsageMatr[BTextIdx,ATargIdx]='d') and StrInList(FFlow[BTextIdx].Cmd,CMov_BW+CMov_BD+CMov_WD) then begin Result:=TRUE; break; end;
  inc(BTextIdx);
  end;
End;


Function TProcMs.HeaviestAny ( ATypeC : char ) : Integer;
Var
  BTargIdx      : Integer;
  BBestWeight   : Single;
Begin
 Result:=-1; BBestWeight:=0;
 repeat

 repeat
 //if Pos(ATypeC,'mwkbcl')=0 then break;
 // First loop which requires something special
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  repeat
  if TargRequiresLowerRegPart(BTargIdx)=FALSE then break;
  with FTargRegMapList[BTargIdx] do
   begin
   if (FAsgnWww=0) and (FTypeC=ATypeC) then
    begin
    if Result=-1 then begin Result:=BTargIdx; BBestWeight:=FWeight; end
    else if FWeight>BBestWeight then begin Result:=BTargIdx; BBestWeight:=FWeight; end;
    end;
   end;
  until TRUE;
  inc(BTargIdx);
  end;
 until TRUE;

 if Result<>-1 then break;

 // No special requirements
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  repeat
  with FTargRegMapList[BTargIdx] do
   begin
   if (FAsgnWww=0) and (FTypeC=ATypeC) then
    begin
    if Result=-1 then begin Result:=BTargIdx; BBestWeight:=FWeight; end
    else if FWeight>BBestWeight then begin Result:=BTargIdx; BBestWeight:=FWeight; end;
    end;
   end;
  until TRUE;
  inc(BTargIdx);
  end;
 until TRUE;
End;

Const
  CRegDCount    = 14;
  CRegDList : array [0..CRegDCount-1] of TRegDescr =
   (
    (FRow: 1; FWww:8), // ar
    (FRow: 2; FWww:8), // br
    (FRow: 3; FWww:8), // cr
    (FRow: 4; FWww:8), // dr
    (FRow: 5; FWww:8), // er
    (FRow: 6; FWww:8), // fr
    (FRow: 7; FWww:8), // gr
    (FRow: 1; FWww:7), // awx
    (FRow: 2; FWww:7), // bwx
    (FRow: 3; FWww:7), // cwx
    (FRow: 4; FWww:7), // dwx
    (FRow: 5; FWww:7), // ewx
    (FRow: 6; FWww:7), // fwx
    (FRow: 7; FWww:7)  // gwx
   );

  CRegWCount    = 14;
  CRegWList : array [0..CRegWCount-1] of TRegDescr =
   (
    (FRow: 1; FWww:4), // aw
    (FRow: 2; FWww:4), // bw
    (FRow: 3; FWww:4), // cw
    (FRow: 4; FWww:4), // dw
    (FRow: 5; FWww:4), // ew
    (FRow: 6; FWww:4), // fw
    (FRow: 7; FWww:4), // gw
    (FRow: 1; FWww:3), // ax
    (FRow: 2; FWww:3), // bx
    (FRow: 3; FWww:3), // cx
    (FRow: 4; FWww:3), // dx
    (FRow: 5; FWww:3), // ex
    (FRow: 6; FWww:3), // fx
    (FRow: 7; FWww:3)  // gx
   );

  CRegBCount    = 14;
  CRegBList : array [0..CRegBCount-1] of TRegDescr =
   (
    (FRow: 1; FWww:1), // al
    (FRow: 1; FWww:2), // ah
    (FRow: 2; FWww:1), // bl
    (FRow: 2; FWww:2), // bh
    (FRow: 3; FWww:1), // cl
    (FRow: 3; FWww:2), // ch
    (FRow: 4; FWww:1), // dl
    (FRow: 4; FWww:2), // dh
    (FRow: 5; FWww:1), // el
    (FRow: 5; FWww:2), // eh
    (FRow: 6; FWww:1), // fl
    (FRow: 6; FWww:2), // fh
    (FRow: 7; FWww:1), // gl
    (FRow: 7; FWww:2)  // gh
   );

Function TProcMs.MapAny ( Out AConfl : TConfl ) : boolean;
Begin
 Result:=FALSE;
 AConfl:=ZConfl;
 repeat
 if MapAny(CRegDList,'f',AConfl)=FALSE then break;
 if MapAny(CRegDList,'p',AConfl)=FALSE then break;
 if MapAny(CRegDList,'q',AConfl)=FALSE then break;
 if MapAny(CRegDList,'i',AConfl)=FALSE then break;
 if MapAny(CRegDList,'d',AConfl)=FALSE then break;
 if MapAny(CRegWList,'m',AConfl)=FALSE then break;
 if MapAny(CRegWList,'w',AConfl)=FALSE then break;
 if MapAny(CRegBList,'k',AConfl)=FALSE then break;
 if MapAny(CRegBList,'b',AConfl)=FALSE then break;
 if MapAny(CRegBList,'c',AConfl)=FALSE then break;
 if MapAny(CRegBList,'l',AConfl)=FALSE then break;


 Result:=TRUE;
 until TRUE;
End;

Function TProcMs.MapAny ( Const ARegList : TRegDescrList; ATypeC : char; Var AConfl : TConfl ) : boolean;
Var
  BTargIdx      : Integer;
  BRegMapPtr    : PTargRegMap;
  BWww,
  BRow          : Byte;
  BConflList    : TConflList;
  BRegIdx       : Integer;
Begin
 Result:=FALSE;
 SetLength(BConflList,Length(ARegList));
 repeat
 BTargIdx:=HeaviestAny(ATypeC);
 if BTargIdx=-1 then begin Result:=TRUE; break; end;
 BRegMapPtr:=@FTargRegMapList[BTargIdx];
 BRegIdx:=0;
 while BRegIdx<Length(ARegList) do
  begin
  BWww:=ARegList[BRegIdx].FWww; BRow:=ARegList[BRegIdx].FRow;
  if IsRegSuitable(BTargIdx,BWww) then
   begin
   BRegMapPtr^.FAsgnWww:=BWww;
   BRegMapPtr^.FAsgnRow:=BRow;
   if TryAssignReg(BTargIdx,BRow,BWww,-1,-1,BConflList[BRegIdx]) then break;
   end;
  inc(BRegIdx);
  end;
 if BRegIdx=Length(ARegList) then
  begin
  SelectBestConfl(BConflList,AConfl);
  break;
  end;
 CleanupSD;
 CleanupPart;
 until FALSE;
 BConflList:=nil;
End;

Function TProcMs.CheckConflA ( ATargIdxA, ATargIdxB : Integer; ARow, AWww : byte ) : boolean;
Var
  BLineIdx,
  BTargIdx      : Integer;
  BRegMapPtr    : PTargRegMap;
Begin
 Result:=FALSE;
 BLineIdx:=0;
 while BLineIdx<Length(FFlow) do
  begin
  if (FUsageMatr[BLineIdx,ATargIdxA]<>'.') or (FUsageMatr[BLineIdx,ATargIdxB]<>'.') then
   begin
   BTargIdx:=0;
   while BTargIdx<Length(FTargRegMapList) do
    begin
    repeat
    if (BTargIdx=ATargIdxA) or (BTargIdx=ATargIdxB) then break;
    if FUsageMatr[BLineIdx,BTargIdx]='.' then break;
    BRegMapPtr:=@FTargRegMapList[BTargIdx];
    Result:=(BRegMapPtr^.FAsgnRow=ARow) and ((BRegMapPtr^.FAsgnWww and AWww)<>0);
    until TRUE;
    if Result then break;
    inc(BTargIdx);
    end;
   if Result then break;
   end;
  inc(BLineIdx);
  end;
End;

Procedure TProcMs.CleanupSD;
Var
  BLine         : TFlowLine;
  BLineIdx,
  BTargIdxA,
  BTargIdxB     : Integer;
  BRegMapPtrA,
  BRegMapPtrB   : PTargRegMap;
  BConfl        : TConfl;
Begin
 BLineIdx:=1;
 while BLineIdx<Length(FFlow)-1 do
  begin
  BLine:=FFlow[BLineIdx];
  repeat
  if BLine.Removed then break;
  if BLine.IsMov=FALSE then break;
  for BTargIdxA:=0 to Length(FTargRegMapList)-1 do
   begin
   repeat
   BRegMapPtrA:=@FTargRegMapList[BTargIdxA];
   if (FUsageMatr[BLineIdx,BTargIdxA]='s') and (FUsageMatr[BLineIdx+1,BTargIdxA]='.') and (BRegMapPtrA^.FAsgnWww<>0) then
    begin
    BTargIdxB:=0;
    while BTargIdxB<Length(FTargRegMapList) do
     begin
     if (BTargIdxB<>BTargIdxA) and (FUsageMatr[BLineIdx,BTargIdxB]='d') then break;
     inc(BTargIdxB);
     end;
    end
   else if (FUsageMatr[BLineIdx,BTargIdxA]='d') and (BRegMapPtrA^.FAsgnWww<>0) then
    begin
    BTargIdxB:=0;
    while BTargIdxB<Length(FTargRegMapList) do
     begin
     if (BTargIdxB<>BTargIdxA) and (FUsageMatr[BLineIdx,BTargIdxB]='s') and (FUsageMatr[BLineIdx+1,BTargIdxB]='.') then break;
     inc(BTargIdxB);
     end;
    end
   else break;
   if BTargIdxB>=Length(FTargRegMapList) then break;
   BRegMapPtrB:=@FTargRegMapList[BTargIdxB];
   if BRegMapPtrB^.FAsgnWww<>0 then break;
   //if CheckConflA(BTargIdxA,BTargIdxB,BRegMapPtrA^.FAsgnRow,BRegMapPtrA^.FAsgnWww) then break;
   if TryAssignReg(BTargIdxB,BRegMapPtrA^.FAsgnRow,BRegMapPtrA^.FAsgnWww,BLineIdx,-1,BConfl)=FALSE then break;
   BRegMapPtrB^.FAsgnWww:=BRegMapPtrA^.FAsgnWww;
   BRegMapPtrB^.FAsgnRow:=BRegMapPtrA^.FAsgnRow;
   BLine.Removed:=TRUE;
   until TRUE;
   end;
  until TRUE;
  inc(BLineIdx);
  end;
End;

Procedure TProcMs.CleanupPart;
Var
  BLine         : TFlowLine;
  BLineIdx,
  BTargIdxS,
  BTargIdxD     : Integer;
  BRegMapPtrS,
  BRegMapPtrD   : PTargRegMap;
  BFound        : boolean;
  BPartWww      : byte;
  BLineIdxA     : Integer;
  BConfl        : TConfl;
Begin
 BRegMapPtrD:=nil; BRegMapPtrS:=nil;
 BLineIdx:=1;
 while BLineIdx<Length(FFlow)-1 do
  begin
  BLine:=FFlow[BLineIdx];
  repeat
  if BLine.Removed then break;
  if BLine.IsMovPart=FALSE then break;
  // Search Src
  BTargIdxS:=0;
  while BTargIdxS<Length(FTargRegMapList) do
   begin
   BFound:=FALSE;
   repeat
   //if FChTargList.Strings[BTargIdxS]<>BLine.Param[1].Targ then break;
   if FUsageMatr[BLineIdx,BTargIdxS]<>'s' then break;
   BRegMapPtrS:=@FTargRegMapList[BTargIdxS];
   if BRegMapPtrS^.FAsgnWww=0 then break;
   BFound:=TRUE;
   until TRUE;
   if BFound then break;
   inc(BTargIdxS);
   end;
  if BTargIdxS>=Length(FTargRegMapList) then break;
  // Search Dst
  BTargIdxD:=0;
  while BTargIdxD<Length(FTargRegMapList) do
   begin
   BFound:=FALSE;
   repeat
   //if FChTargList.Strings[BTargIdxD]<>BLine.Param[0].Targ then break;
   if FUsageMatr[BLineIdx,BTargIdxD]<>'d' then break;
   BRegMapPtrD:=@FTargRegMapList[BTargIdxD];
   if BRegMapPtrD^.FAsgnWww<>0 then break;
   BFound:=TRUE;
   until TRUE;
   if BFound then break;
   inc(BTargIdxD);
   end;
  if BTargIdxD>=Length(FTargRegMapList) then break;
  // Check if applicable
  BLineIdxA:=1;
  while BLineIdxA<Length(FFlow)-1 do
   begin
   if (BLineIdxA<>BLineIdx) and (FUsageMatr[BLineIdxA,BTargIdxD]<>'.') and (not (FUsageMatr[BLineIdxA,BTargIdxS] in ['.','|'])) then break;
   inc(BLineIdxA);
   end;
  if BLineIdxA<>(Length(FFlow)-1) then break;
  // Change reg
  BPartWww:=GetDstWww(BLine.Cmd);
  if BPartWww=0 then break;
  if TryAssignReg(BTargIdxD,BRegMapPtrS^.FAsgnRow,BPartWww,BLineIdx,BTargIdxS,BConfl)=FALSE then break;
  BRegMapPtrD^.FAsgnWww:=BPartWww;
  BRegMapPtrD^.FAsgnRow:=BRegMapPtrS^.FAsgnRow;
  BLine.Removed:=TRUE;

  until TRUE;
  inc(BLineIdx);
  end;
End;

end.

// 2073
