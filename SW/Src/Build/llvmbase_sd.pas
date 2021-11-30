unit LlvmBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsHelper_sd;

{
 All items are defined by a prefix.
 Kind:
  m method, d data, c const
 Visibility:
  a parameter, v var_parameter, c const_parameter, b local, f private, h public
 Type
  b byte,
  w word,
  d cardinal,
  l boolean
  k shortint (-128..127)
  m smallint (-32768..32767)
  i integer  (32-bit)
  r real
  p pointer (is always followed by PtrType)
  q special pointer, usually Tmp. Used for ex. to address records: we move pointer to a reg, then do [reg+offset]
}

Type
  TLlvmModule = class;

  TLlvmObj = class(TObject)
  private
    FDeclLine,
    FDeclPos            : Integer;

    FOnAppendError      : TOnViewAny;
  protected
    FName               : string;
    FNameL              : string; // with prefix and parameters
    FErrorList          : TStringList;
    FFatalError         : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Compile; Virtual; Abstract;

    Procedure AppendError ( Const AErrorList : TStringList );
    Procedure AppendErrorA ( Const AError : string );
    Procedure AppendErrorA ( AErrorType : char; ALine, APos : Integer; Const AFilename : string; Const AComment : string );
    Function GetErrorCountA : Integer;

    property Name : string read FName write FName;
    property NameL : string read FNameL write FNameL;
    property DeclLine : Integer read FDeclLine write FDeclLine;
    property DeclPos : Integer read FDeclPos write FDeclPos;
    property ErrorList : TStringList read FErrorList;

    property OnAppendError : TOnViewAny read FOnAppendError write FOnAppendError;
  end;

  TLlvmObjList = array of TLlvmObj;

  TLlvmProc = class(TLlvmObj)
  private
    FIsImplemented      : boolean;
    FTmpIdx             : Integer;
    //FExtRefName         : string; // If external, contains external reference name (usually in asm file)
  protected
    FModule             : TLlvmModule;
    FTypeListS          : string;
    FVarListS,
    FParListS,
    FRetListS           : string;
    FFlowList,
    FPostList           : TStringList;
    FStartLine,
    FStartPos           : Integer; // In Pascal points to the end of the 1st Begin
    FEndLine,
    FEndPos             : Integer; // In Pascal points to the end of the 1st Begin

    FDiscardListS       : string;

    Function CanBeUsedAsIndex ( Const ATarg : string ) : boolean; Virtual; Abstract;
    Function IsValidArrayIdx ( Const ATarg : string ) : boolean; Virtual; Abstract;
    Procedure ResolveForwardList ( Var AList : string );

    Function DbgProcHeader : string; Virtual;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure AppendVarListS ( Const AList : string );
    Procedure AppendDiscardListS ( Const ATarg : string );

    Function GenTmpVarName ( APrefix : char ) : string;
    Function AppendTmpVar ( Const ATypeS : string ) : string;
    Function ResolveForwardType ( Var AType : string ) : boolean;
    Function ResolveForwardTarg ( Const ATarg : string ) : string;
    Function ResolveForwardParam ( Var AParam : string ) : boolean;
    Procedure ResolveForwardAll;
    Function ExtractFinalType ( Const ATarg : string ) : string;

    Function LabelName ( Const ALabel : string; ALevel, ALabelIndex : Integer ) : string;
    Function LabelName ( ALabelIndex : Integer ) : string;

    Function IsTargUsed ( Const ATarg : string ) : boolean; Virtual; Abstract;

    Procedure FlushPostList;

    Procedure Compile; Override;
    Procedure DebugInfo ( AList : TStringList ); Virtual;
    Procedure GenAsmSrc ( AAsmSrc : TStringList ); Virtual;
    Procedure GenCompDbg ( ADbg : TStringList ); Virtual;

    property Module : TLlvmModule read FModule;
    property IsImplemented : boolean read FIsImplemented write FIsImplemented;
    //property ExtRefName : string read FExtRefName write FExtRefName;
    property VarListS : string read FVarListS;
    property ParListS : string read FParListS write FParListS;
    property RetListS : string read FRetListS write FRetListS;
    property FlowList : TStringList read FFlowlist;
    property PostList : TStringList read FPostList;
    property StartLine : Integer read FStartLine write FStartLine;
    property StartPos : Integer read FStartPos write FStartPos;
    property EndLine : Integer read FEndLine write FEndLine;
    property EndPos : Integer read FEndPos write FEndPos;
  end;

  TCompStat = class(TObject)
  private
    FParsTime,
    FAtomTime,
    FFlowTime,
    FJmpsTime,
    FVarsTime,
    FOrdrTime,
    FMatrTime,
    FSpltTime,
    FBEndTime   : Double;
  public
    Procedure Clear;

    property ParsTime : Double read FParsTime write FParsTime;
    property AtomTime : Double read FAtomTime write FAtomTime;
    property FlowTime : Double read FFlowTime write FFlowTime;
    property JmpsTime : Double read FJmpsTime write FJmpsTime;
    property VarsTime : Double read FVarsTime write FVarsTime;
    property OrdrTime : Double read FOrdrTime write FOrdrTime;
    property MatrTime : Double read FMatrTime write FMatrTime;
    property SpltTime : Double read FSpltTime write FSpltTime;
    property BEndTime : Double read FBEndTime write FBEndTime;
  end;

  TLlvmModule = class(TLlvmObj)
  private
    FFilename           : string;
    FDbgPath            : string;
  protected
    FUsesList           : TLlvmObjList;
    FProcList           : TLlvmObjList;
    FPublicNames        : string;
    FExternNames        : string;
    FPrivateNames       : string;
    FVarList            : string;
    //FExtLocNames        : string; // When procedure is declared as External in Pascal. This prevents back-end to declare it as Public
    FLabelUid           : Integer;
    FPublicAliases,
    FPrivateAliases     : TStringList;
    FDefineList         : TStringList;
    FConstList          : TStringList;

    FPrjPath,
    FDstPath            : string;
    FCodeSegName,
    FDataSegName        : string;

    FCompStat           : TCompStat;
    FPrjParams          : TStringList; // Used to pass some common project parameters (Such as Risc-V compiler string)

    // Following settings must be filed by the back-end
    FIndexType          : string;
    FBaseTypeI,
    FBaseTypeU          : char;
    FStackGenericType   : char; // Indicates how many bits are copied to the stack in one push/pop

    Procedure AppendProcA ( AProc : TLlvmProc );
    Procedure ResolveForwardList ( Var AList : string );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function AppendUses ( AModule : TLlvmModule ) : boolean;
    Function VisibleNames : string;
    Procedure Clear;
    Procedure ClearErrorList;
    Function AppendProc : TLlvmProc; Virtual; Abstract;
    //Procedure AppendExtLoc ( Const ATarg : string );
    Procedure AppendExternName ( Const ATarg : string );

    Procedure AppendError ( AErrorType : char; ALine, APos : Integer; Const AComment : string );
    Procedure AppendError ( AErrorType : char; AToken : TParsToken; Const AComment : string );
    Procedure AppendError ( AErrorList : TStringList );

    Function ResolveForwardType ( Var AType : string ) : boolean;
    Function ResolveForwardTarg ( Const ATarg : string ) : string;
    Procedure ResolveForwardAll;

    Procedure Init ( Const AFilename : string; Const APrjPath, ADstPath : string; Const ADefCodeSeg, ADefDataSeg : string );
    Procedure Compile; Override;
    Procedure GenAsmSrc ( AAsmSrc : TStringList ); Virtual;
    Procedure GenCompDbg ( ADbg : TStringList );
    Procedure AlignSize ( Var AData : Integer ); Virtual;
    Function GetTypeSize ( Const ATypeS : string ) : Integer; Virtual;
    Procedure OrderRecordFields ( Const ASrcList : string; Out ADstList : string; Out ARecSize : Integer ); Virtual;
    Function AliasExists ( Const AName : string; ASearchPrivate : boolean ) : boolean;
    Function ResolveAlias ( Const AName : string; ASearchPrivate : boolean ) : string;

    Procedure AppendDefine ( Const AFilename, ADefineA, ADefineB : string; AToken : TParsToken );
    Procedure AppendDefine ( Const ADefineA, ADefineB : string; AToken : TParsToken );
    Procedure RemoveDefine ( Const ADefineA : string );
    Procedure ImportDefine ( AModule : TLlvmModule; AToken : TParsToken );
    Function ResolveDefine ( Const ASrc : string; Out ADefineA, ADefineB : string ) : TDefType;

    Function AppendConst ( Const AType : string; Const AValue : string ) : string;
    Function GetConstValue ( Const ATarg : string ) : string;

    Function NextLabelUid : string;

    property PrjPath : string read FPrjPath;
    property Filename : string read FFilename write FFilename;
    property DbgPath : string read FDbgPath write FDbgPath;
    property PrjParams : TStringList read FPrjParams write FPrjParams;
    property ProcList : TLlvmObjList read FProcList;
    property PublicNames : string read FPublicNames write FPublicNames;
    property ExternNames : string read FExternNames write FExternNames;
    property PrivateNames : string read FPrivateNames write FPrivateNames;
    //property ExtLocList : TStringList read FExtLocList;
    property VarList : string read FVarList write FVarList;
    property PublicAliases : TStringList read FPublicAliases;
    property PrivateAliases : TStringList read FPrivateAliases;
    property CompStat : TCompStat read FCompStat write FCompStat;
    property IndexType : string read FIndexType;
    property BaseTypeI : char read FBaseTypeI;
    property BaseTypeU : char read FBaseTypeU;
    property StackGenericType : char read FStackGenericType;
  end;

Const
  COpcodeList = ' add sub mul div mod and or test xor shl shr cmpe cmpne cmpb cmpa cmpbe cmpae type ';


Procedure AppendObjList ( Var AList : TLlvmObjList; AObj : TLlvmObj );
Procedure DestroyObjListA ( Var AList : TLlvmObjList );
Function ObjByName ( Const AList : TLlvmObjList; Const AName : string ) : TLlvmObj;
Function IsLabel ( Const AName : string ) : boolean;
Function IsJxx ( Const AName : string ) : boolean;
Function IsJmp ( Const AName : string ) : boolean;
Function IsPush ( Const AName : string ) : boolean;
Function IsCall ( Const AName : string ) : boolean;
Function IsLea ( Const AName : string ) : boolean;
Function IsNop ( Const AName : string ) : boolean;
Function IsCmdClassX ( Const ACmd : string ) : boolean;
Function IsLdx ( Const AName : string ) : boolean;
Function IsStx ( Const AName : string ) : boolean;
Function IsMovMd ( Const AName : string ) : boolean;
Function IsMovDm ( Const AName : string ) : boolean;
Function IsSpecOut ( Const ASpec : string ) : boolean;
Function IsSpecParam ( Const ASpec : string ) : boolean;
Function IsNonAsgnCmd ( Const AName : string ) : boolean;

Function ExtractCallIdx ( Const ACmd : String ) : Integer;

Procedure SplitDefine ( Const AOrig : string; Out AFilename, ADefineA, ADefineB : string );

Procedure DelExtraBrackets ( Var AEval : string );
Procedure ReadEvalAtom ( Var AEval : string; Var ASubEval : string; APos : Integer; Const ATerminator : string );
Function ReadEvalAtom ( Var AEval : string ) : string;
Procedure SplitExec ( Const AExec : string; Out ACmd, AParamA, AParamB : string );
Procedure SplitExec ( Const AExec : string; Out ACmd, AParamA, AParamB, ATail : string );
Procedure ExtractEvalPair ( Var AEval : string; Out ASubEval : string; Const AOperList : string; Out APos : Integer );

implementation

Uses
  ConComL;

Procedure AppendObjList ( Var AList : TLlvmObjList; AObj : TLlvmObj );
Var
  BIndex        : Integer;
Begin
 BIndex:=Length(AList);
 SetLength(AList,BIndex+1);
 AList[BIndex]:=AObj;
End;

Procedure DestroyObjListA ( Var AList : TLlvmObjList );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(AList)-1 do AList[BIndex].Free;
 AList:=nil;
End;

Function ObjByName ( Const AList : TLlvmObjList; Const AName : string ) : TLlvmObj;
Var
  BIndex        : Integer;
  BNameS        : string;
Begin
 Result:=nil;
 BNameS:=LowerCase(AName);
 for BIndex:=0 to Length(AList)-1 do
  begin
  if LowerCase(AList[BIndex].FName)=BNameS then begin Result:=AList[BIndex]; break; end;
  end;
End;

Function IsLabel ( Const AName : string ) : boolean;
Begin
 Result:=(AName<>'') and (AName[Length(AName)]=':');
End;

Function IsJxx ( Const AName : string ) : boolean;
Begin
 Result:=Pos('j',AName)=1;
End;

Function IsJmp ( Const AName : string ) : boolean;
Begin
 Result:=AName='jmp';
End;

Function IsPush ( Const AName : string ) : boolean;
Begin
 Result:=Pos('push',AName)=1;
End;

Function IsCall ( Const AName : string ) : boolean;
Begin
 Result:=Pos('call',AName)=1;
End;

Function IsNop ( Const AName : string ) : boolean;
Begin
 Result:=Pos('nop',AName)=1;
End;

Function IsCmdClassX ( Const ACmd : string ) : boolean;
Begin
 Result:=StrInList(ACmd,'add sub and or xor mul div shl shr');
End;

Function IsLdx ( Const AName : string ) : boolean;
Begin
 Result:=Pos('ld',AName)=1;
End;

Function IsStx ( Const AName : string ) : boolean;
Begin
 Result:=Pos('st',AName)=1;
End;

Function IsLea ( Const AName : string ) : boolean;
Begin
 Result:=Pos('lea',AName)=1;
End;

Function IsMovMd ( Const AName : string ) : boolean;
Begin
 Result:=AName='mov_md';
End;

Function IsMovDm ( Const AName : string ) : boolean;
Begin
 Result:=AName='mov_dm';
End;

Function IsSpecOut ( Const ASpec : string ) : boolean;
Begin
 Result:=(Pos('dv',ASpec)=1) or (Pos('do',ASpec)=1) or (Pos('dr',ASpec)=1);
End;

Function IsSpecParam ( Const ASpec : string ) : boolean;
Begin
 Result:=StrInList(Copy(ASpec,1,2),'da dv dc'); //Pos('da',ASpec)=1;
End;

Function IsNonAsgnCmd ( Const AName : string ) : boolean;
Begin
 Result:=(AName='cmp') or (AName='test');
End;

Function ExtractCallIdx ( Const ACmd : String ) : Integer;
Var
  BCmd          : string;
  BPos          : Integer;
Begin
 Result:=-1;
 repeat
 BCmd:=ACmd;
 BPos:=Pos('_',BCmd);
 if BPos<=0 then break;
 Delete(BCmd,1,BPos);
 Result:=StrToInt(BCmd);
 until TRUE;
End;

Procedure SplitDefine ( Const AOrig : string; Out AFilename, ADefineA, ADefineB : string );
Var
  BOrig         : string;
  BDefineM      : string;
Begin
 ADefineB:='';
 repeat
 BOrig:=AOrig;
 AFilename:=ReadParamStr(BOrig);
 ADefineA:=ReadParamStr(BOrig);
 if ADefineA='' then break;
 if ADefineA[Length(ADefineA)]='(' then
  begin
  ReadTillClosing(BOrig,BDefineM);
  ADefineA:=ADefineA+BDefineM;
  end;
 DelFirstSpace(BOrig); DelLastSpace(BOrig);
 ADefineB:=BOrig;
 until TRUE;
End;

Procedure DelExtraBrackets ( Var AEval : string );
Var
  BIndex,
  BLen          : Integer;
  BLevel        : Integer;
Begin
 repeat
 if AEval='' then break;
 if (AEval[1]<>'(') or (AEval[Length(AEval)]<>')') then break;
 BLevel:=0;
 BLen:=Length(AEval);
 BIndex:=2;
 while BIndex<BLen do
  begin
  if AEval[BIndex]='(' then inc(BLevel)
  else if AEval[BIndex]=')' then begin dec(BLevel); if BLevel<0 then break; end;
  inc(BIndex);
  end;
 if BIndex<>BLen then break;
 Delete(AEval,1,1);
 Delete(AEval,Length(AEval),1);
 until FALSE;
End;

Procedure ReadEvalAtom ( Var AEval : string; Var ASubEval : string; APos : Integer; Const ATerminator : string );
Var
  BDataC        : char;
Begin
 repeat
 if APos>Length(AEval) then break;
 BDataC:=AEval[APos]; Delete(AEval,APos,1);
 ASubEval:=ASubEval+BDataC;
 //if BDataC=ATerminator then break;
 if Pos(BDataC,ATerminator)<>0 then break;
 //if BDataC='(' then begin ReadEvalAtom(AEval,ASubEval,APos,')'); break; end;
 //if BDataC='[' then begin ReadEvalAtom(AEval,ASubEval,APos,']'); break; end;
 if BDataC='(' then ReadEvalAtom(AEval,ASubEval,APos,')');
 if BDataC='[' then ReadEvalAtom(AEval,ASubEval,APos,']');
 until FALSE;
End;


Function ReadEvalAtom ( Var AEval : string ) : string;
Begin
 Result:='';
 ReadEvalAtom(AEval,Result,1,' ');
 DelLastSpace(Result);
End;

Procedure SplitExec ( Const AExec : string; Out ACmd, AParamA, AParamB : string );
Var
  BExecA,
  BExec         : string;
Begin
 BExecA:=AExec; BExec:=ReadTillC(BExecA,';');
 ACmd:=ReadParamStr(BExec);
 AParamA:=ReadParamStr(BExec);
 AParamB:=ReadParamStr(BExec);
End;

Procedure SplitExec ( Const AExec : string; Out ACmd, AParamA, AParamB, ATail : string );
Var
  BExecA,
  BExec         : string;
Begin
 BExecA:=AExec; BExec:=ReadTillC(BExecA,';');
 ACmd:=ReadParamStr(BExec);
 AParamA:=ReadParamStr(BExec);
 AParamB:=ReadParamStr(BExec);
 ATail:=BExecA;
 DelFirstSpace(ATail);
End;

Procedure SplitExec ( Const ALine : string; Out AExec, ACmd, AParamA, AParamB, ATail : string );
Var
  BExecA,
  BExec         : string;
Begin
 BExecA:=ALine; BExec:=ReadTillC(BExecA,';');
 AExec:=BExec;
 ACmd:=ReadParamStr(BExec);
 AParamA:=ReadParamStr(BExec);
 AParamB:=ReadParamStr(BExec);
 ATail:=BExecA;
 DelFirstSpace(ATail);
End;

Procedure ExtractEvalPair ( Var AEval : string; Out ASubEval : string; Const AOperList : string; Out APos : Integer );
Var
  BPosA,
  BPosB         : Integer;
  BPos          : Integer;
Begin
 ASubEval:='';
 APos:=0;
 repeat
 BPos:=FirstMultiPos(' '+AOperList+' ',AEval);
 if BPos=0 then break;
 BPosA:=BPos-1;
 if BPosA<=0 then break;
 while BPosA>1 do
  begin
  if AEval[BPosA] in [#32, '('] then break;
  Dec(BPosA);
  end;
 for BPosB:=BPos+1 to Length(AEval) do begin if AEval[BPosB]=' ' then break; end;
 for BPosB:=BPosB+1 to Length(AEval) do begin if AEval[BPosB] in [#32, ')'] then break; end;
 if AEval[BPosA] in [#32, '('] then inc(BPosA);
 if AEval[BPosB] in [#32, ')'] then dec(BPosB);
 ASubEval:=Copy(AEval,BPosA,BPosB-BPosA+1);
 Delete(AEval,BPosA,BPosB-BPosA+1);
 APos:=BPosA;
 until TRUE;
End;

// ** TLlvmObj

Constructor TLlvmObj.Create;
Begin
 Inherited;
 FErrorList:=TStringList.Create;
End;

Destructor TLlvmObj.Destroy;
Begin
 FErrorList.Free;
 Inherited;
End;

Procedure TLlvmObj.AppendErrorA ( AErrorType : char; ALine, APos : Integer; Const AFilename : string; Const AComment : string );
Begin
 AppendErrorA(FormatError(AErrorType+AComment,AFilename,ALine,APos));
End;

Procedure TLlvmObj.AppendErrorA ( Const AError : string );
Begin
 FErrorList.Append(AError);
 if Assigned(FOnAppendError) then FOnAppendError(AError);
End;

Procedure TLlvmObj.AppendError ( Const AErrorList : TStringList );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to AErrorList.Count-1 do AppendErrorA(AErrorList.Strings[BIndex]);
End;


Function TLlvmObj.GetErrorCountA : Integer;
Var
  BIndex        : Integer;
Begin
 Result:=0;
 for BIndex:=0 to FErrorList.Count-1 do
  begin
  if Copy(FErrorList.Strings[BIndex],1,1)='e' then inc(Result);
  end;
End;

// ** TLlvmProc

Constructor TLlvmProc.Create;
Begin
 Inherited;
 FFlowList:=TStringList.Create;
 FPostList:=TStringList.Create;
 FTmpIdx:=0;
End;

Destructor TLlvmProc.Destroy;
Begin
 FPostList.Free;
 FFlowList.Free;
 Inherited;
End;

Procedure TLlvmProc.AppendVarListS ( Const AList : string );
Var
  BList : string;
  BTarg : string;
Begin
 BList:=AList;
 repeat
 BTarg:=ReadParamStr(BList);
 if BTarg='' then break;
 if Pos(BTarg,FVarListS)=0 then FVarListS:=FVarListS+BTarg+' ';
 until FALSE;
End;

Procedure TLlvmProc.AppendDiscardListS ( Const ATarg : string );
Var
  BTarg : string;
Begin
 BTarg:=ATarg; DelLastSpace(BTarg);
 FDiscardListS:=FDiscardListS+BTarg+' ';
End;

Function TLlvmProc.GenTmpVarName ( APrefix : char ) : string;
Begin
 Result:=APrefix+'Tmp'+IntToStr(FTmpIdx);
 inc(FTmpIdx);
End;

Function TLlvmProc.AppendTmpVar ( Const ATypeS : string ) : string;
Begin
 Result:=CTagS+'de'+ATypeS+CTagM+GenTmpVarName('B')+CTagE;
 FVarListS:=FVarListS+Result+' ';
End;

Function TLlvmProc.LabelName ( Const ALabel : string; ALevel, ALabelIndex : Integer ) : string;
Begin
 Result:=FName+'_'+ALabel;
 if ALevel<32 then Result:=Result+Chr(ALevel+ord('A'))+IntToStr(ALabelIndex)
 else Result:=Result+'_'+IntToStr(ALevel)+'_'+IntToStr(ALabelIndex);
 Result:=Result+'_U'+FModule.NextLabelUid;
End;

Function TLlvmProc.LabelName ( ALabelIndex : Integer ) : string;
Begin
 Result:=FName+'_'+IntToStr(ALabelIndex)+'_U'+Module.NextLabelUid;
End;

Procedure TLlvmProc.FlushPostList;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<FPostList.Count do
  begin
  FFlowList.Append(FPostList.Strings[BIndex]);
  inc(BIndex);
  end;
 FPostList.Clear;
End;

Procedure TLlvmProc.Compile;
Begin
 FErrorList.Clear;
 FFatalError:=FALSE;
End;

Procedure TLlvmProc.DebugInfo ( AList : TStringList );
Begin
 AList.Clear;
End;

Procedure TLlvmProc.GenAsmSrc ( AAsmSrc : TStringList );
Begin
 AAsmSrc.Clear;
End;

Procedure TLlvmProc.GenCompDbg ( ADbg : TStringList );
Begin
 ADbg.Append('');
End;

Function TLlvmProc.ResolveForwardType ( Var AType : string ) : boolean;
Begin
 Result:=FModule.ResolveForwardType(AType);
End;

Function TLlvmProc.ResolveForwardTarg ( Const ATarg : string ) : string;
Var
  BType         : string;
Begin
 Result:=ATarg;
 BType:=ParsExtractType(Result);
 if ResolveForwardType(BType) then ParsForceType(Result,BType);
End;

Function TLlvmProc.ResolveForwardParam ( Var AParam : string ) : boolean;
Var
  BType         : string;
Begin
 BType:=AParam; Delete(BType,1,2);
 Result:=ResolveForwardType(BType);
 if Result then AParam:=Copy(AParam,1,2)+BType;
End;

Procedure TLlvmProc.ResolveForwardList ( Var AList : string );
Var
  BList         : string;
  BTarg         : string;
Begin
 BList:=AList;
 AList:='';
 while BList<>'' do
  begin
  BTarg:=ReadParamStr(BList);
  if BTarg='' then break;
  AList:=AList+ResolveForwardTarg(BTarg)+' ';
  end;
End;

Procedure TLlvmProc.ResolveForwardAll;
Begin
 ResolveForwardList(FParListS);
 ResolveForwardList(FVarListS);
 ResolveForwardList(FRetListS);
End;

Function TLlvmProc.ExtractFinalType ( Const ATarg : string ) : string;
Var
  BPos          : Integer;
  BTarg         : string;
  BTargRec,
  BTargFld,
  BTargFldA     : string;
  BIndexS,
  BDummyS       : string;
  BTypeS        : string;
Begin
 Result:='';
 BTarg:=ATarg;
 repeat
 BPos:=Pos(']_',ATarg);
 if BPos<>0 then
  begin
  Result:=Copy(ATarg,BPos+2,Length(ATarg)-BPos-1);
  break;
  end;
 if ParsIsQ(ATarg) then
  begin
  Result:=ParsExtractType(ATarg);
  if Length(Result)>1 then Delete(Result,1,1);
  break;
  end;
 BTargRec:=ReadParamStr(BTarg,CTagE)+CTagE;
 BTargFld:=ReadParamStr(BTarg);
 BTypeS:=ParsExtractType(BTargRec); if BTypeS='' then break;
 repeat
 if BTargFld='' then begin Result:=BTypeS; break; end;
 if BTargFld[1]='.' then
  begin
  Delete(BTargFld,1,1);
  BTargFldA:=ReadTillA(BTargFld,'.[');
  if BTargFldA<>'' then ParsGetFieldType(BTypeS,BTargFldA);
  ResolveForwardType(BTypeS);
  if BTypeS='' then begin Result:=BTypeS; break; end;
  DelFirstSpace(BTargFld);
  continue;
  end;
 if BTargFld[1]='[' then
  begin
  if ReadInside(BTargFld,1,BIndexS)=FALSE then break;
  if BTypeS[1]='a' then
   begin
   repeat
   BDummyS:=ReadParamStr(BIndexS,',');
   if BDummyS='' then break;
   BPos:=Pos('e',BTypeS); if BPos=0 then BTypeS:='' else Delete(BTypeS,1,BPos);
   until FALSE;
   end
  else if BTypeS[1]='p' then
   begin
   repeat
   BDummyS:=ReadParamStr(BIndexS,',');
   if BDummyS='' then break;
   Delete(BTypeS,1,1);
   until FALSE;
   end
  else if ParsIsTypeStringP(BTypeS) then BTypeS:='c'
  else if ParsIsTypeStringZ(BTypeS) then BTypeS:='c'
  else break;
  DelFirstSpace(BTargFld);
  end;
 until FALSE;

 until TRUE;
End;

Function TLlvmProc.DbgProcHeader : string;
Begin
 Result:='';
End;

// ** TCompStat
Procedure TCompStat.Clear;
Begin
 FParsTime:=0;
 FAtomTime:=0;
 FJmpsTime:=0;
 FVarsTime:=0;
 FOrdrTime:=0;
 FMatrTime:=0;
 FSpltTime:=0;
 FBEndTime:=0;
End;

// ** TLlvmModule

Constructor TLlvmModule.Create;
Begin
 Inherited;
 FPrivateAliases:=TStringList.Create;
 FPublicAliases:=TStringList.Create;
 FDefineList:=TStringList.Create;
 FConstList:=TStringList.Create;
 FCompStat:=TCompStat.Create;
 FBaseTypeI:='i';
 FBaseTypeU:='w';
 FStackGenericType:='d';
End;

Destructor TLlvmModule.Destroy;
Begin
 Clear;
 FCompStat.Free;
 FConstList.Free;
 FDefineList.Free;
 FPublicAliases.Free;
 FPrivateAliases.Free;
 Inherited;
End;

Procedure TLlvmModule.AppendError ( AErrorType : char; ALine, APos : Integer; Const AComment : string );
Begin
 AppendErrorA(AErrorType,ALine,APos,FFilename,AComment);
End;

Procedure TLlvmModule.AppendError ( AErrorType : char; AToken : TParsToken; Const AComment : string );
Begin
 AppendErrorA(AErrorType,AToken.Line,AToken.Pos,FFilename,AComment);
End;

Procedure TLlvmModule.AppendError ( AErrorList : TStringList );
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<AErrorList.Count do
  begin
  AppendErrorA(AErrorList.Strings[BIndex]);
  //FErrorList.Append(AErrorList.Strings[BIndex]);
  inc(BIndex);
  end;
End;

Function TLlvmModule.AppendUses ( AModule : TLlvmModule ) : boolean;
Var
  BObj          : TLlvmObj;
Begin
 BObj:=ObjByName(FUsesList,AModule.FName);
 if BObj=nil then AppendObjList(FUsesList,AModule);
 Result:=BObj=nil;
End;

Function TLlvmModule.VisibleNames : string;
Var
  BIndex        : Integer;
  BModule       : TLlvmModule;
Begin
 Result:='';
 for BIndex:=0 to Length(FUsesList)-1 do
  begin
  BModule:=TLlvmModule(FUsesList[BIndex]);
  Result:=Result+BModule.FPublicNames;
  end;
End;

Procedure TLlvmModule.Clear;
Begin
 DestroyObjListA(FProcList);
 FConstList.Clear;
 FDefineList.Clear;
 FPublicAliases.Clear;
 FPrivateAliases.Clear;
 FUsesList:=nil;
 ClearErrorList;
 FPublicNames:='';
 FPrivateNames:='';
 FExternNames:='';
 //FExtLocNames:='';
 FVarList:='';
 FLabelUid:=0;
 FCompStat.Clear;
End;

Procedure TLlvmModule.ClearErrorList;
Begin
 FErrorList.Clear;
End;

Procedure TLlvmModule.AppendProcA ( AProc : TLlvmProc );
Begin
 AProc.FModule:=Self;
 AppendObjList(FProcList,AProc);
End;

Procedure TLlvmModule.AppendExternName ( Const ATarg : string );
Begin
 repeat
 if StrInList(ATarg,FExternNames) then break;
 FExternNames:=FExternNames+ATarg+' ';
 until TRUE;
End;

{Procedure TLlvmModule.AppendExtLoc ( Const ATarg : string );
Begin
 repeat
 if StrInList(ATarg,FExtLocNames) then break;
 FExtLocNames:=FExtLocNames+ATarg+' ';
 until TRUE;
End;}

Function TLlvmModule.NextLabelUid : string;
Begin
 Result:=IntToStr(FLabelUid);
 inc(FLabelUid);
End;

Function TLlvmModule.ResolveForwardType ( Var AType : string ) : boolean;
Var
  BTypeS        : string;
  BForward      : string;
  BVisibleNames : string;
  BReadS        : string;
Begin
 Result:=FALSE;
 BTypeS:=AType;
 BForward:=ParsCheckGetForwardPointer(BTypeS);
 if BForward<>'' then
  begin
  Result:=TRUE;
  BVisibleNames:=FPrivateNames+FPublicNames+VisibleNames;
  BReadS:='';
  while BVisibleNames<>'' do
   begin
   BReadS:=ReadParamStr(BVisibleNames);
   if BReadS='' then break;
   if ParsIsType(BReadS) and (ParsExtractName(BReadS)=BForward) then break;
   end;
  if BReadS='' then AppendError('e',0,0,'Cannot resolve forward declaration '+BForward+' [R:TLlvmModule.ResolveForward]');
  AType:='p'+ParsExtractType(BReadS);
  end;
End;

Function TLlvmModule.ResolveForwardTarg ( Const ATarg : string ) : string;
Var
  BType         : string;
Begin
 Result:=ATarg;
 BType:=ParsExtractType(Result);
 if ResolveForwardType(BType) then ParsForceType(Result,BType);
End;

Procedure TLlvmModule.ResolveForwardList ( Var AList : string );
Var
  BList         : string;
  BTarg         : string;
Begin
 BList:=AList;
 AList:='';
 while BList<>'' do
  begin
  BTarg:=ReadParamStr(BList);
  if BTarg='' then break;
  AList:=AList+ResolveForwardTarg(BTarg)+' ';
  end;
End;

Procedure TLlvmModule.ResolveForwardAll;
Var
  BIndex        : Integer;
Begin
 ResolveForwardList(FPublicNames);
 ResolveForwardList(FPrivateNames);
 ResolveForwardList(FVarList);
 for BIndex:=0 to Length(FProcList)-1 do TLlvmProc(FProcList[BIndex]).ResolveForwardAll;
End;

Procedure TLlvmModule.Init ( Const AFilename : string; Const APrjPath, ADstPath : string; Const ADefCodeSeg, ADefDataSeg : string );
Begin
 FFilename:=AFilename;
 FPrjPath:=APrjPath;
 FDstPath:=ADstPath;
 FCodeSegName:=ADefCodeSeg;
 FDataSegName:=ADefDataSeg;
 Clear;
End;

Procedure TLlvmModule.Compile;
Var
  BIndex        : Integer;
  BProc         : TLlvmProc;
Begin
 for BIndex:=0 to Length(FProcList)-1 do
  begin
  BProc:=TLlvmProc(FProcList[BIndex]);
  if BProc.IsImplemented then BProc.Compile;
  AppendError(FProcList[BIndex].ErrorList);
  end;
End;

Procedure TLlvmModule.GenAsmSrc ( AAsmSrc : TStringList );
Begin
 AAsmSrc.Clear;
End;

Procedure TLlvmModule.GenCompDbg ( ADbg : TStringList );
Var
  BProcIdx      : Integer;
Begin
 ADbg.Append(';@M '+FName);
 ADbg.Append(';@S '+FFilename);
 for BProcIdx:=0 to Length(FProcList)-1 do TLlvmProc(FProcList[BProcIdx]).GenCompDbg(ADbg);
 ADbg.Append('');
End;

Procedure TLlvmModule.AlignSize ( Var AData : Integer );
Begin
 AData:=AData+0;
End;

Function TLlvmModule.GetTypeSize ( Const ATypeS : string ) : Integer;
Begin
 Result:=0;
 if ATypeS='b' then Result:=1
 else if ATypeS='w' then Result:=2
 else if ATypeS='d' then Result:=4;
End;

Procedure TLlvmModule.OrderRecordFields ( Const ASrcList : string; Out ADstList : string; Out ARecSize : Integer );
Begin
 ADstList:=ASrcList;
 ARecSize:=0;
End;

Function TLlvmModule.ResolveAlias ( Const AName : string; ASearchPrivate : boolean ) : string;
Var
  BIndex        : Integer;
Begin
 Result:='';
 repeat
 Result:=FPublicAliases.Values[AName]; if Result<>'' then break;
 if ASearchPrivate then
  begin
  Result:=FPrivateAliases.Values[AName]; if Result<>'' then break;
  end;
 BIndex:=0;
 while BIndex<Length(FUsesList) do
  begin
  Result:=TLlvmModule(FUsesList[BIndex]).ResolveAlias(AName,FALSE);
  if Result<>AName then break;
  inc(BIndex);
  end;
 if BIndex<Length(FUsesList) then break;
 Result:=AName;
 until TRUE;
End;

Function TLlvmModule.AliasExists ( Const AName : string; ASearchPrivate : boolean ) : boolean;
Var
  BIndex        : Integer;
Begin
 Result:=TRUE;
 repeat
 if FPublicAliases.IndexOfName(AName)<>-1 then break;
 if ASearchPrivate and (FPrivateAliases.IndexOfName(AName)<>-1) then break;
 BIndex:=0;
 while BIndex<Length(FUsesList) do
  begin
  if TLlvmModule(FUsesList[BIndex]).AliasExists(AName,FALSE) then break;
  inc(BIndex);
  end;
 Result:=BIndex<Length(FUsesList);
 until TRUE;
End;

Procedure TLlvmModule.AppendDefine ( Const AFilename, ADefineA, ADefineB : string; AToken : TParsToken );
Var
  BIndex        : Integer;
  BFilename,
  BDefineA,
  BDefineB      : string;
  BDefineSrc,
  BDefineDst    : string;
  BDirty        : boolean;
  BDummyS       : string;
Begin
 repeat
 if StrInList(ADefineA,ADefineB) then begin AppendError('e',AToken,'Define cannot define itself [R:TLlvmModule.AppendDefine]'); break; end;

 BIndex:=0;
 while BIndex<FDefineList.Count do
  begin
  SplitDefine(FDefineList.Strings[BIndex],BFilename,BDefineA,BDefineB);
  if BDefineA=ADefineA then
   begin
   if (BFilename=AFilename) and (BDefineB=ADefineB) then break;
   if BFilename<>AFilename then AppendError('e',AToken,'Define conflict: '+ADefineA+' is already defined in '+BFilename+' [R:TLlvmModule.AppendDefine]')
   else AppendError('e',AToken,'Define conflict: '+ADefineA+' is already defined as '+BDefineB+' [R:TLlvmModule.AppendDefine]');
   break;
   end;
  inc(BIndex);
  end;

 if BIndex<>FDefineList.Count then break;

 BDefineDst:=ADefineB;
 repeat
 BDefineSrc:=BDefineDst; BDefineDst:='';
 BDirty:=FALSE;
 repeat
 BDefineA:=ReadParamStr(BDefineSrc);
 if BDefineA='' then break;
 if ResolveDefine(BDefineA,BDummyS,BDefineB)<>dtNone then begin BDirty:=TRUE; BDefineDst:=BDefineDst+BDefineB+' '; end
 else BDefineDst:=BDefineDst+BDefineA+' ';
 until FALSE;
 if BDirty=FALSE then break;
 until FALSE;

 DelLastSpace(BDefineDst);
 FDefineList.Append(AFilename+' '+ADefineA+' '+BDefineDst);
 until TRUE;
End;

Procedure TLlvmModule.AppendDefine ( Const ADefineA, ADefineB : string; AToken : TParsToken );
Begin
 AppendDefine(FFilename,ADefineA,ADefineB,AToken);
End;

Procedure TLlvmModule.RemoveDefine ( Const ADefineA : string );
Var
  BIndex        : Integer;
  BFilename,
  BDefineA,
  BDefineB      : string;
Begin
 repeat
 BIndex:=0;
 while BIndex<FDefineList.Count do
  begin
  SplitDefine(FDefineList.Strings[BIndex],BFilename,BDefineA,BDefineB);
  if BDefineA=ADefineA then break;
  inc(BIndex);
  end;

 if BIndex>=FDefineList.Count then break;
 FDefineList.Delete(BIndex);
 until TRUE;
End;

Procedure TLlvmModule.ImportDefine ( AModule : TLlvmModule; AToken : TParsToken );
Var
  BIndex        : Integer;
  BFilename,
  BDefineA,
  BDefineB      : string;
Begin
 BIndex:=0;
 while BIndex<AModule.FDefineList.Count do
  begin
  SplitDefine(AModule.FDefineList.Strings[BIndex],BFilename,BDefineA,BDefineB);
  AppendDefine(BFilename,BDefineA,BDefineB,AToken);
  inc(BIndex);
  end;
End;

Function TLlvmModule.ResolveDefine ( Const ASrc : string; Out ADefineA, ADefineB : string ) : TDefType;
Var
  BIndex        : Integer;
  BFilename,
  BDefineA,
  BDefineB      : string;
  BDefineM      : string;
  BPos          : Integer;
Begin
 Result:=dtNone; ADefineB:='';
 BIndex:=0;
 while BIndex<FDefineList.Count do
  begin
  SplitDefine(FDefineList.Strings[BIndex],BFilename,BDefineA,BDefineB);
  BPos:=Pos('(',BDefineA);
  if BPos<>0 then
   begin
   BDefineM:=Copy(BDefineA,1,BPos-1);
   if BDefineM=ASrc then
    begin
    ADefineA:=BDefineA;
    ADefineB:=BDefineB;
    Result:=dtMacro;
    break;
    end;
   end
  else if BDefineA=ASrc then
   begin
   ADefineA:=BDefineA;
   ADefineB:=BDefineB;
   Result:=dtConst;
   break;
   end;
  inc(BIndex);
  end;
End;

Function TLlvmModule.AppendConst ( Const AType : string; Const AValue : string ) : string;
Begin
 Result:='';
 repeat
 if AType='sp' then
  begin
  Result:=CTagS+'c_sp'+IntToStr(Length(AValue))+'e'+CTagM+'ConstStrP'+NextLabelUid+CTagE;
  FConstList.Append(Result+' '+AValue);
  break;
  end;
 if AType='sz' then
  begin
  Result:=CTagS+'c_sz'+IntToStr(Length(AValue))+'e'+CTagM+'ConstStrZ'+NextLabelUid+CTagE;
  FConstList.Append(Result+' '+AValue);
  break;
  end;
 if ParsIsTypeArray(AType) then
  begin
  Result:=CTagS+'c_'+AType+CTagM+'ConstArray'+NextLabelUid+CTagE;
  FConstList.Append(Result+' '+AValue);
  break;
  end;
 AppendError('e',0,0,'Internal error: invalid const specifier [R:TLlvmModule.AppendConst]');
 until TRUE;
End;

Procedure SplitConstNameValue ( Const AOrig : string; Out AName, AValue : string );
Var
  BPos          : Integer;
Begin
 BPos:=Pos(' ',AOrig);
 if BPos=0 then begin AName:=AOrig; AValue:='' end
 else begin AName:=Copy(AOrig,1,BPos-1); AValue:=Copy(AOrig,BPos+1,Length(AOrig)-BPos); end;
End;

Function TLlvmModule.GetConstValue ( Const ATarg : string ) : string;
Var
  BIndex        : Integer;
  BName,
  BValue        : string;
Begin
 Result:='';
 BIndex:=0;
 while BIndex<FConstList.Count do
  begin
  SplitConstNameValue(FConstList.Strings[BIndex],BName,BValue);
  if BName=ATarg then begin Result:=BValue; break; end;
  inc(BIndex);
  end;
End;

end.

