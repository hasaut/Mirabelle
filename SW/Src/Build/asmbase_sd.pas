unit AsmBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParsBase_sd, LlvmBase_sd, LlvmDbg_sd, AsmTypes_sd, DataFiles, ElfFile, MemSeg_sd, DasmBase_sd;

Type
  TCmdIs = (acUnparsed, acSysComment, acEmpty, acInclude, acIncData, acSegName, acOrg, acAlign, acData, acStack, acLabel, acPublic, acExtern, acEqu, acCmd, acOther, acM16aGlobal, acRiscVGlobal);
  TCmdCompError = ( cceCheckNext, cceError, cceCompiled );
  PUid = ^Integer;

  TAsmBase = class;
  TAsmFlowLine = class;
  TAsmLineParam = class;

  TAsmRef = class(TObject)
  private
    FName       : string;
    FParam      : TAsmLineParam;
    FCodeBinPos : Cardinal; // Position in CodeBin
    FObjectAddr,            // Linker part, assigned by linker
    FFieldAddr  : Cardinal; // Fixed part (for ex. [FData+2] then it will be 2)
    FRefType    : char;     // j - sjmps, b - byte, w - word, d - dword, c - (dword shr 1) (used by call and Jmp)
    FUsed       : boolean;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure AppendError ( AErrorCode : char; Const AComment : string );

    property Name : string read FName write FName;
    property Param : TAsmLineParam read FParam write FParam;
    property CodeBinPos : Cardinal read FCodeBinPos write FCodeBinPos;
    property ObjectAddr : Cardinal read FObjectAddr write FObjectAddr;
    property FieldAddr : Cardinal read FFieldAddr write FFieldAddr;
    property RefType : char read FRefType write FRefType;
    property Used : boolean read FUsed write FUsed;
  end;

  TAsmRefList = array of TAsmRef;

  TAsmLineParam = class(TObject)
  private
    FLine       : TAsmFlowLine;
    FName,                // Normal name, like in the source
    FNameA      : string; // With final constants (to avoid "(-1 shl (64-1))")
    FTextPos    : Integer;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure AppendError ( AErrorCode : char; Const AComment : string );

    property Name : string read FName write FName;
    property NameA : string read FNameA write FNameA;
    property Line : TAsmFlowLine read FLine write FLine;
    property TextPos : Integer read FTextPos write FTextPos;
  end;

  TAsmLineParamList = array of TAsmLineParam;

  TAsmFlowLine = class(TObject)
  private
    FFileName   : string;
    FTextLine   : Integer;
    FReadPos    : Integer; // Used by ReadParamA for parsing
    FFlowIdx    : Integer;

    FIncName    : string;

    FIsJmp,
    FIsJxx,
    FIsCall,               // Determined and assigned by LTO. It can be difficult to distinguish between BRA as JMP and as CALL
    FIsIpLoad,
    FIsIpSave   : boolean;
    FDstLabel   : string;
    FStackDelta : Integer;
    FRecursCnt  : Integer;

    FLastError  : string;

    Procedure SetCodeBin ( Const ACodeBin : string );
  protected
    FOrig,
    FExec,
    FTail       : string;

    FAsmBase    : TAsmBase;
    FRefList    : TAsmRefList;
    FParams     : TAsmLineParamList;

    FCmdIs      : TCmdIs;
    FLabelName  : string;
    FCodeBinA   : string; // Preserved CodeBin before link loop
    FCodeBin    : string;
    FAlign      : Integer;

    FMemSeg     : TMemSeg;
    FUsed       : boolean;
    FBaseAddr,
    FVirtAddr   : Cardinal;

    FIsAddrOor  : boolean;

    Function AppendParam ( Const AName : string; ATextPos : Integer ) : TAsmLineParam;
    Procedure ReadParamA ( ASeparator : char; Out AName : string; Out ATextPos : Integer );
    Procedure ReadParamC ( ASeparator : char; Out AName : string; Out ATextPos : Integer );
    Procedure AppendTail ( Var ALstLine : string );
    Function PreFormatA ( Const ALineType : string; ASpacesCnt : Integer ) : string;
    Function InvJmpA ( Const AJmp : string ) : string; Virtual; Abstract;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Clear;

    Procedure AppendError ( AErrorCode : char; Const AComment : string );
    Procedure AppendError ( AErrorCode : char; APos : Integer; Const AComment : string );
    Procedure AppendError ( AErrorCode : char; AParam : TAsmLineParam; Const AComment : string );

    Procedure ClearParams;
    Procedure SplitExecTail ( Const AReadS : string );
    Procedure Parse ( Const AReadS : string; Const AFileName : string; ATextLine : Integer ); Virtual;
    Procedure Parse ( AMemSeg : TMemSeg; AVirtAddr, ABaseAddr : Cardinal; ACodeBin : string; Const AReadable : string; Const AFilename : string; ATextLine : Integer );

    Procedure ClearDataBin;
    Procedure AppendDataBinB ( AData : byte );
    Procedure AppendDataBinW ( AData : word );
    Procedure AppendDataBinD ( AData : Cardinal );
    Procedure AppendDataBinQ ( AData : QWord );
    Procedure AppendDataBinX ( AData : QWord; ASize : byte );
    Procedure SetDataSize ( ADataSize : Integer );
    Procedure DupData ( ADupCnt : Integer );

    Procedure ClearRef;
    Procedure AppendDataRefQ ( AParam : TAsmLineParam );
    Procedure AppendDataRefD ( AParam : TAsmLineParam );
    Procedure AppendDataRefW ( AParam : TAsmLineParam );
    Procedure AppendRef ( AParam : TAsmLineParam; AType : char; ACodeBinPos : Cardinal );
    Procedure AppendRef ( AParam : TAsmLineParam; Const AName : string; AType : char; ACodeBinPos : Cardinal; AFieldAddr : Cardinal );

    Procedure SaveCodeBin;
    Procedure LoadCodeBin;

    Procedure FixAddr ( Var ASegList : TMemSegList; Var ASeg : TMemSeg ); Virtual; Abstract;
    Procedure UpdateRefs ( Const AInternList, AExternList : TAsmRefList; Var AIsAddrOor : boolean ); Virtual; Abstract;
    Procedure InvJmp ( Const ANewLabel : string );
    Procedure WriteCodeBin;
    Procedure SetDstLabel ( Const ALabel : string );

    Procedure ResetPlay;

    Function FormatForLst : string;

    property AsmBase : TAsmBase read FAsmBase write FAsmBase;
    property TextLine : Integer read FTextLine;
    property FlowIdx : Integer read FFlowIdx write FFlowIdx;
    property CmdIs : TCmdIs read FCmdIs;
    property IncName : string read FIncName;
    property LabelName : string read FLabelName;

    property Orig : string read FOrig;
    property FileName : string read FFileName;
    property Exec : string read FExec;
    property Params : TAsmLineParamList read FParams;
    property RefList : TAsmRefList read FRefList;
    property Align : Integer read FAlign;
    property CodeBin : string read FCodeBin write SetCodeBin;
    property MemSeg : TMemSeg read FMemSeg;
    property BaseAddr : Cardinal read FBaseAddr write FBaseAddr;
    property VirtAddr : Cardinal read FVirtAddr write FVirtAddr;
    property Tail : string read FTail;

    property IsJmp : boolean read FIsJmp write FIsJmp;
    property IsJxx : boolean read FIsJxx write FIsJxx;
    property IsCall : boolean read FIsCall write FIsCall;
    property IsIpLoad : boolean read FIsIpSave write FIsIpSave;
    property IsIpSave : boolean read FIsIpLoad write FIsIpLoad;
    property DstLabel : string read FDstLabel;

    property StackDelta : Integer read FStackDelta write FStackDelta; // Shows size (i.e. positive for push, call and negative for pop, ret
    property RecursCnt : Integer read FRecursCnt write FRecursCnt;

    property Used : boolean read FUsed write FUsed;

    property IsAddrOor : boolean read FIsAddrOor;
    property LastError : string read FLastError;
  end;

  TAsmFlowList = array of TAsmFlowLine;

  TVarData = record
    FType   : char;
    FDataI  : Int64;
    FDataF  : Double;
  end;
  TVarDataList = array of TVarData;

  TAsmBase = class(TLlvmObj)
  private
    FElfFile        : TElfFile;
    FVerifList      : TStringList;
    FCodeStart      : Cardinal;

    FConstInsIdx    : Integer;
    FSkipCodeGen    : boolean; // If code generation needs to be skipped. Use case: ".debug_*" section for RV

    FCodeChunkList  : TCodeChunkList;

    //Procedure AppendError ( AErrorType : char; Const AComment : string );
    Function ResolveIncName ( ALine : TAsmFlowLine; Const AUsedIncNames : string; Const AFilename : string; ALineIdx : Integer ) : string;
    Procedure IncludeSrc ( ALine : TAsmFlowLine; Const AUsedIncNames : string; Const AFilename : string; ALineIdx : Integer );

    Function PreprocDateTimeStr ( Var AOrigin : string ) : boolean;
    Function PreprocConstStrCS ( ALineIdx : Integer; Var AOrigin : string ) : boolean;
    Function PreprocTimeHexImm ( Var AOrigin : string ) : boolean;
    Function PreprocMath ( Var AOrigin : string ) : boolean;
    Function PreprocLine ( ALineIdx : Integer; Var AOrigin : string ) : boolean;

    Function GetHexInfo ( ALine : TAsmFlowLine; Out ASegStart, ASegSize : Cardinal ) : boolean;
    Function GetHexInfo ( ALine : TAsmFlowLine; Out ASegStart, ASegSize : Cardinal; Out ALabelName : string ) : boolean;
    Function ParseHexLine ( Const AReadS : string; ASegStart, ASegSize : Cardinal; ASegWrPos : Cardinal; Const AHexFilename : string; AHexLineIdx : Integer; Out AAddr : Cardinal; Out ADataS : string ) : boolean;
    Procedure ParseHexTxt ( ALine : TasmFlowLine; AHexSrc : TStringList; Const AHexFilename : string );

  protected
    FPrjPath,
    FDstPath            : string;
    FSrcName,
    FAsmName            : string;
    FTextSrc,
    FAsmSrc             : TStringList;
    FIncSearchPath,
    FLocSearchPath      : TStringList;
    FDefCodeSeg,
    FDefDataSeg         : string;

    FUid                : PUid;

    FFlowList           : TAsmFlowList;

    FParser             : TParsBase;
    FModule             : TLlvmModule;
    FPublicList,
    FExternList,
    FLabelList          : TAsmRefList;
    FEquList            : TStringList;

    FConstStrList       : TStringList;

    FAddressParamType   : char;
    FHiddenLine         : TAsmFlowLine;

    FSymComment,
    FSymMultiline       : char;

    Procedure ClearFlowList;
    Function InsertFlowLineAfter ( AParent : TAsmFlowLine ) : TAsmFlowLine;
    Function NewFlowLine : TAsmFlowLine; Virtual; Abstract;
    Procedure ParseList ( AAsmSrc : TStringList; Const AFilename : string; Const AUsedIncNames : string );

    Function AtomizeEvalNoBr ( Const AEval : TVarDataList; Out ATarg : TVarData ) : boolean;
    Function AtomizeEval ( Const AEval : string; Out ATarg : TVarData; Out AProcessed : boolean ) : boolean;
    Function IsIntegerEqu ( Const AName : string; Out ADataI : Int64 ) : boolean;
    Function IsFloatEqu ( Const AName : string; Out ADataF : Double ) : boolean;
    Function TryIntegerEqu ( AParam : TAsmLineParam; Out ADataI : Int64 ) : boolean;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    // Only ConstFile auto generation
    Procedure ConstFileStart;
    Procedure ConstFileAppend ( AConstList : TStringList );
    Procedure ConstFileAppend ( Const AName : string; Const ADataS : string );
    Function ConstFileGetLineData ( AName : string ) : TAsmFlowLine;

    Procedure Init ( Const AFilename : string; Const APrjPath, ADstPath : string; AIncSearchPath, ALocSearchPath : TStringList; Const ADefCodeSeg, ADefDataSeg : string; AGetUses : TGetUses; AReadInc : TReadInc; AUid : PUid );
    Function LoadSrc : boolean;
    Procedure AppendSrcLine ( Const ALine : string );
    Procedure AppendFlowLine ( ALine : TAsmFlowLine );
    Function AppendFlowLineA : TAsmFlowline;
    Function AppendFlowLine ( AAddr : Cardinal ) : TAsmFlowline;
    Procedure Compile; Override;
    Procedure FixAddr ( Var ASegList : TMemSegList );
    Procedure FillPublicAddr;
    //Procedure CheckExternUsed;
    Procedure FillLabelList;
    Procedure FillExternList ( Const APublicList : TAsmRefList );
    Procedure SaveCodeBin;
    Procedure LoadCodeBin;
    Procedure UpdateTextRefs ( Var AIsAddrOor : boolean );
    Procedure CorrectAddrOor; Virtual; Abstract;
    Procedure WriteCodeBin ( Const ASegList : TMemSegList );
    Procedure WriteLst ( AList : TStringList );
    Procedure CodeGenCmd ( ALine : TAsmFlowLine ); Virtual; Abstract;

    Function FindLabel ( ALineThis : TAsmFlowLine; Const AName : string ) : TAsmFlowLine;
    Function NextEp ( Var ALineIdx : Integer ) : TAsmFlowLine;
    Procedure ResetPlay;
    Procedure MarkLinesUsed;

    // Disassembler
    Procedure ExportExecLine ( Const ASegList : TMemSegList; AExec : TExecLineBase );
    Procedure ExportExecLines ( Const ASegList : TMemSegList );

    property SrcName : string read FSrcName;
    property AsmName : string read FAsmName;
    property FlowList : TAsmFlowList read FFlowList;
    property Parser : TParsBase read FParser;
    property Module : TLlvmModule read FModule;
    property PublicList : TAsmRefList read FPublicList;
    property VerifList : TStringList read FVerifList;
    property CodeStart : Cardinal read FCodeStart;
    property ConstStrList : TStringList read FConstStrList;

    property HiddenLine : TAsmFlowLine read FHiddenLine;
    property AsmSrc : TStringList read FAsmSrc;

    property CodeChunkList : TCodeChunkList read FCodeChunkList;
    property SkipCodeGen : boolean read FSkipCodeGen write FSkipCodeGen;
  end; (* TAsmBase *)

  TAsmBaseList = array of TAsmBase;

{ *** Common (RefList) *** }
Procedure RefListClear ( Var AList : TAsmRefList );
Function RefListSearch ( Const AList : TAsmRefList; Const AName : string ) : TAsmRef;
Function RefListAppend ( Var AList : TAsmRefList; AParam : TAsmLineParam; Const AName : string ) : TAsmRef;
Procedure RefListDelItem ( Var AList : TAsmRefList; AIndex : Integer );

{ *** Common (AddRef) *** }
Procedure AddRefDD ( Var ACodeBin : string; AOffset : Integer; AData : Cardinal );
Procedure AddRefDW ( Var ACodeBin : string; AOffset : Integer; AData : word );
Procedure AddRefDB ( Var ACodeBin : string; AOffset : Integer; AData : Byte );

{ *** Common VarData ***}
Procedure VarDataDel ( Var ADataList : TVarDataList; AIndex : Integer; ACount : Integer );
Procedure VarDataAdd ( Var ADataList : TVarDataList; Const AVarData : TVarData );

implementation

Uses
  ConComL, ConComAsm;

Procedure RefListClear ( Var AList : TAsmRefList );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(AList)-1 do AList[BIndex].Free;
 AList:=nil;
End;

Function RefListSearch ( Const AList : TAsmRefList; Const AName : string ) : TAsmRef;
Var
  BIndex        : Integer;
  BName         : string;
Begin
 BName:=LowerCase(AName);
 Result:=nil;
 for BIndex:=0 to Length(AList)-1 do
  begin
  if LowerCase(AList[BIndex].Name)=BName then begin Result:=AList[BIndex]; break; end;
  end;
End;

Function RefListAppend ( Var AList : TAsmRefList; AParam : TAsmLineParam; Const AName : string ) : TAsmRef;
Var
  BIndex        : Integer;
Begin
 Result:=TAsmRef.Create;
 BIndex:=Length(AList); SetLength(AList,BIndex+1); AList[BIndex]:=Result;
 Result.Param:=AParam;
 Result.CodeBinPos:=0;
 Result.ObjectAddr:=0;
 Result.FieldAddr:=0;
 Result.Name:=AName;
End;

Procedure RefListDelItem ( Var AList : TAsmRefList; AIndex : Integer );
Var
  BIndex        : Integer;
Begin
 repeat
 if AIndex>=Length(AList) then break;
 AList[AIndex].Free;
 for BIndex:=AIndex+1 to Length(AList)-1 do AList[BIndex-1]:=AList[BIndex];
 SetLength(AList,Length(AList)-1);
 until TRUE;
End;

Procedure AddRefDD ( Var ACodeBin : string; AOffset : Integer; AData : Cardinal );
Var
  BData         : Cardinal;
Begin
 BData:=(Cardinal(Ord(ACodeBin[1+AOffset])) shl  0) +
        (Cardinal(Ord(ACodeBin[2+AOffset])) shl  8) +
        (Cardinal(Ord(ACodeBin[3+AOffset])) shl 16) +
        (Cardinal(Ord(ACodeBin[4+AOffset])) shl 24);
 BData:=BData+AData;
 ACodeBin[1+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
 ACodeBin[2+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
 ACodeBin[3+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
 ACodeBin[4+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
End;

Procedure AddRefDW ( Var ACodeBin : string; AOffset : Integer; AData : word );
Var
  BData         : word;
Begin
 BData:=(Word(Ord(ACodeBin[1+AOffset])) shl  0) +
        (Word(Ord(ACodeBin[2+AOffset])) shl  8);
 BData:=BData+AData;
 ACodeBin[1+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
 ACodeBin[2+AOffset]:=Chr(Byte(BData)); BData:=BData shr 8;
End;

Procedure AddRefDB ( Var ACodeBin : string; AOffset : Integer; AData : Byte );
Var
  BData         : Byte;
Begin
 BData:=Ord(ACodeBin[1+AOffset]);
 BData:=BData+AData;
 ACodeBin[1+AOffset]:=Chr(Byte(BData));
End;

{ *** VarData *** }
Procedure VarDataDel ( Var ADataList : TVarDataList; AIndex : Integer; ACount : Integer );
Var
  BIndexA,
  BIndexB   : Integer;
Begin
 BIndexA:=AIndex;
 BIndexB:=AIndex+ACount;
 while BIndexB<Length(ADataList) do
  begin
  ADataList[BIndexA]:=ADataList[BIndexB];
  inc(BIndexB);
  inc(BindexA);
  end;
 SetLength(ADataList,BIndexA);
End;

Procedure VarDataAdd ( Var ADataList : TVarDataList; Const AVarData : TVarData );
Var
  BIndex    : Integer;
Begin
 BIndex:=Length(ADataList);
 SetLength(ADataList,BIndex+1);
 ADataList[BIndex]:=AVarData;
End;

{ *** TAsmRef *** }

Constructor TAsmRef.Create;
Begin
 Inherited;
End;

Destructor TAsmRef.Destroy;
Begin
 Inherited;
End;

Procedure TAsmRef.AppendError ( AErrorCode : char; Const AComment : string );
Begin
 FParam.AppendError(AErrorCode,AComment);
End;

{ *** TAsmFlowLineParam *** }

Constructor TAsmLineParam.Create;
Begin
 Inherited;
End;

Destructor TAsmLineParam.Destroy;
Begin
 Inherited;
End;

Procedure TAsmLineParam.AppendError ( AErrorCode : char; Const AComment : string );
Begin
 FLine.AppendError(AErrorCode,Self,AComment);
End;

{ *** TAsmFlowLine *** }

Constructor TAsmFlowLine.Create;
Begin
 Inherited;
End;

Destructor TAsmFlowLine.Destroy;
Begin
 Clear;
 Inherited;
End;

Procedure TAsmFlowLine.Clear;
Begin
 FCodeBin:=''; FLastError:='';
 ClearParams;
 RefListClear(FRefList);
End;

Procedure TAsmFlowLine.AppendError ( AErrorCode : char; APos : Integer; Const AComment : string );
Begin
 FLastError:=AErrorCode+AComment;
 if FAsmBase<>nil then FAsmBase.AppendErrorA(FormatError(FLastError,FFilename,FTextLine+1,APos));
End;

Procedure TAsmFlowLine.AppendError ( AErrorCode : char; AParam : TAsmLineParam; Const AComment : string );
Var
  BComment      : string;
  BPos          : Integer;
Begin
 BComment:=AComment;
 BPos:=Pos('%p',BComment);
 if BPos<>0 then
  begin
  Delete(BComment,BPos,2);
  Insert(AParam.Name,BComment,BPos);
  end;
 AppendError(AErrorCode,AParam.TextPos+Length(AParam.Name),BComment);
End;

Procedure TAsmFlowLine.AppendError ( AErrorCode : char; Const AComment : string );
Begin
 AppendError(AErrorCode,Length(FExec)+1,AComment);
End;

Procedure TAsmFlowLine.SetCodeBin ( Const ACodeBin : string );
Begin
 FCodeBin:=ACodeBin;
End;

Procedure TAsmFlowLine.ClearParams;
Var
  BParamIdx     : Integer;
Begin
 for BParamIdx:=0 to Length(FParams)-1 do FParams[BParamIdx].Free;
 FParams:=nil;
End;

Function TAsmFlowLine.AppendParam ( Const AName : string; ATextPos : Integer ) : TAsmLineParam;
Var
  BParamIdx     : Integer;
Begin
 Result:=TAsmLineParam.Create;
 BParamIdx:=Length(FParams); SetLength(FParams,BParamIdx+1); FParams[BParamIdx]:=Result;
 Result.Line:=Self;
 Result.Name:=AName;
 Result.TextPos:=ATextPos;
End;

Procedure TAsmFlowLine.ReadParamA ( ASeparator : char; Out AName : string; Out ATextPos : Integer );
Var
  BDataC        : char;
  BLevel        : char;
  BName         : string;
  BSpaces       : string;
Begin
 while FReadPos<=Length(FExec) do
  begin
  if FExec[FReadPos]<>' ' then break;
  inc(FReadPos);
  end;
 BName:='';
 ATextPos:=FReadPos;
 BLevel:=#0; BSpaces:='';
 while FReadPos<=Length(FExec) do
  begin
  BDataC:=FExec[FReadPos];
  if BDataC in [#34, #39] then
   begin
   if BLevel=#0 then BLevel:=BDataC
   else if BLevel=BDataC then BLevel:=#0;
   end;
  if (BLevel=#0) and (BDataC=ASeparator) then break;
  if (BDataC=#32) and (BLevel=#0) then BSpaces:=BSpaces+#32
  else begin BName:=BName+BSpaces+BDataC; BSpaces:=''; end;
  inc(FReadPos);
  end;
 if ATextPos>Length(FExec) then AName:=''
 else begin AName:=BName; inc(FReadPos); end;
End;

Procedure TAsmFlowLine.ReadParamC ( ASeparator : char; Out AName : string; Out ATextPos : Integer );
Var
  BDataC        : char;
  BLevel        : char;
  BName         : string;
  BSpaces       : string;
Begin
 while FReadPos<=Length(FExec) do
  begin
  if FExec[FReadPos]<>' ' then break;
  inc(FReadPos);
  end;
 BName:='';
 ATextPos:=FReadPos;
 BLevel:=#0; BSpaces:='';
 while FReadPos<=Length(FExec) do
  begin
  BDataC:=FExec[FReadPos];
  if BDataC='\' then
   begin
   inc(FReadPos);
   if FReadPos>Length(FExec) then break;
   BDataC:=FExec[FReadPos];
   end
  else if BDataC in [#34, #39] then
   begin
   if BLevel=#0 then BLevel:=BDataC
   else if BLevel=BDataC then BLevel:=#0;
   end;
  if (BLevel=#0) and (BDataC=ASeparator) then break;
  if (BDataC=#32) and (BLevel=#0) then BSpaces:=BSpaces+#32
  else begin BName:=BName+BSpaces+BDataC; BSpaces:=''; end;
  inc(FReadPos);
  end;
 if ATextPos>Length(FExec) then AName:=''
 else begin AName:=BName; inc(FReadPos); end;
End;

Procedure TAsmFlowLine.SplitExecTail ( Const AReadS : string );
Var
  BReadS    : string;
Begin
 BReadS:=AReadS;
 FExec:=ReadTillCQ(BReadS,FAsmBase.FSymComment);
 DelFirstSpace(BReadS);
 if BReadS<>'' then
  begin
  if BReadS[1]=FAsmBase.FSymComment then
   begin
   Delete(BReadS,1,1);
   DelFirstSpace(BReadS);
   end;
  end;
 FTail:=BReadS;
End;

Procedure TAsmFlowLine.Parse ( Const AReadS : string; Const AFileName : string; ATextLine : Integer );
Var
  BCmdS,
  BParamA       : string;
  BTextPos      : Integer;
Begin
 FCmdIs:=acUnparsed;

 FCodeBin:='';
 ClearParams;
 RefListClear(FRefList);
 FFilename:=AFilename;
 FTextLine:=ATextLine;
 FOrig:=AReadS;
 if Pos(';@',AReadS)=1 then FCmdIs:=acSysComment
 else SplitExecTail(AReadS);
 DelLastSpace(FExec); DelLastSpace(FTail);
 FReadPos:=1;

 repeat
 if FCmdIs<>acUnparsed then break;
 if FExec='' then begin FCmdIs:=acEmpty; break; end;

 ReadParamA(' ',BCmdS,BTextPos);
 AppendParam(BCmdS,BTextPos);
 if LowerCase(BCmdS)='#include' then
  begin
  FCmdIs:=acInclude;
  repeat
  ReadParamA(' ',BParamA,BTextPos);
  AppendParam(BParamA,BTextPos);
  if BParamA='' then begin AppendError('e',BTextPos,'Include name is absent [R:TAsmFlowLine.Parse]'); break; end;
  if Length(BParamA)<3 then begin AppendError('e',BTextPos,'Invalid include name [R:TAsmFlowLine.Parse]'); break; end;
  if (BParamA[1]<>'"') or (BParamA[Length(BParamA)]<>'"') then begin AppendError('e',BTextPos,'Invalid include name (must be quoted) [R:TAsmFlowLine.Parse]'); break; end;
  Delete(BParamA,Length(BParamA),1); Delete(BParamA,1,1);
  FIncName:=BParamA;
  // Segment start and end can be specified after include name
  repeat
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  until TRUE;
  break;
  end;
 if LowerCase(BCmdS)='#incdata' then
  begin
  FCmdIs:=acIncData;
  repeat
  ReadParamA(' ',BParamA,BTextPos);
  AppendParam(BParamA,BTextPos);
  if BParamA='' then begin AppendError('e',BTextPos,'Include name is absent [R:TAsmFlowLine.Parse]'); break; end;
  if Length(BParamA)<3 then begin AppendError('e',BTextPos,'Invalid include name [R:TAsmFlowLine.Parse]'); break; end;
  if (BParamA[1]<>'"') or (BParamA[Length(BParamA)]<>'"') then begin AppendError('e',BTextPos,'Invalid include name (must be quoted) [R:TAsmFlowLine.Parse]'); break; end;
  Delete(BParamA,Length(BParamA),1); Delete(BParamA,1,1);
  FIncName:=BParamA;
  // Segment start and end can be specified after include name
  repeat
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  until TRUE;
  break;
  end;
 until TRUE;
End;

Procedure TAsmFlowLine.Parse ( AMemSeg : TMemSeg; AVirtAddr, ABaseAddr : Cardinal; ACodeBin : string; Const AReadable : string; Const AFilename : string; ATextLine : Integer );
Begin
 Parse(AReadable,AFilename,ATextLine);
 FMemSeg:=AMemSeg;
 FVirtAddr:=AVirtAddr;
 FBaseAddr:=ABaseAddr;
 FCodeBin:=ACodeBin;
 FUsed:=TRUE;
End;

Procedure TAsmFlowLine.ClearDataBin;
Begin
 FCodeBin:='';
End;

Procedure TAsmFlowLine.AppendDataBinB ( AData : byte );
Begin
 FCodeBin:=FCodeBin+Chr(AData);
End;

Procedure TAsmFlowLine.AppendDataBinW ( AData : word );
Begin
 AppendDataBinB(Byte(AData));
 AppendDataBinB(Byte(AData shr 8));
End;

Procedure TAsmFlowLine.AppendDataBinD ( AData : Cardinal );
Begin
 AppendDataBinB(Byte(AData));
 AppendDataBinB(Byte(AData shr 8));
 AppendDataBinB(Byte(AData shr 16));
 AppendDataBinB(Byte(AData shr 24));
End;

Procedure TAsmFlowLine.AppendDataBinQ ( AData : QWord );
Var
  BByteIdx      : Integer;
Begin
 for BByteIdx:=0 to 7 do
  begin
  AppendDataBinB(Byte(AData));
  AData:=AData shr 8;
  end;
End;

Procedure TAsmFlowLine.AppendDataBinX ( AData : QWord; ASize : byte );
Begin
 case ASize of
   0: begin end;
   1: AppendDataBinB(AData);
   2: AppendDataBinW(AData);
   4: AppendDataBinD(AData);
   8: AppendDataBinQ(AData);
 else AppendError('e',FParams[0],'Internal error: invalid data size '+IntToStr(ASize)+' [R:TAsmFlowLine.AppendDataBinX]');
 end;
End;

Procedure TAsmFlowLine.SetDataSize ( ADataSize : Integer );
Var
  BIndex        : Integer;
Begin
 SetLength(FCodeBin,ADataSize);
 BIndex:=0;
 while BIndex<ADataSize do
  begin
  FCodeBin[1+BIndex]:=#0;
  inc(BIndex);
  end;
End;

Procedure TAsmFlowLine.DupData ( ADupCnt : Integer );
Var
  BColCnt       : Integer;
  BColIdx,
  BRowIdx       : Integer;
Begin
 BColCnt:=Length(FCodeBin);
 SetLength(FCodeBin,BColCnt*ADupCnt);
 BRowIdx:=1;
 while BRowIdx<ADupCnt do
  begin
  BColIdx:=0;
  while BColIdx<BColCnt do
   begin
   FCodeBin[1+BRowIdx*BColCnt+BColIdx]:=FCodeBin[1+BColIdx];
   inc(BColIdx);
   end;
  inc(BRowIdx);
  end;
End;

Procedure TAsmFlowLine.ClearRef;
Begin
 RefListClear(FRefList);
End;

Procedure TAsmFlowLine.AppendDataRefQ ( AParam : TAsmLineParam );
Begin
 AppendRef(AParam,'q',Length(FCodeBin));
 AppendDataBinQ(0);
End;

Procedure TAsmFlowLine.AppendDataRefD ( AParam : TAsmLineParam );
Begin
 AppendRef(AParam,'d',Length(FCodeBin));
 AppendDataBinD(0);
End;

Procedure TAsmFlowLine.AppendDataRefW ( AParam : TAsmLineParam );
Begin
 AppendRef(AParam,'w',Length(FCodeBin));
 AppendDataBinW(0);
End;

Procedure TAsmFlowLine.AppendRef ( AParam : TAsmLineParam; Const AName : string; AType : char; ACodeBinPos : Cardinal; AFieldAddr : Cardinal );
Var
  BRef          : TAsmRef;
Begin
 BRef:=RefListAppend(FRefList,AParam,AName);
 BRef.RefType:=AType;
 BRef.CodeBinPos:=ACodeBinPos;
 BRef.FieldAddr:=AFieldAddr;
End;

Procedure TAsmFlowLine.AppendRef ( AParam : TAsmLineParam; AType : char; ACodeBinPos : Cardinal );
Var
  BRef,
  BName         : string;
  BFieldAddr    : Cardinal;
  BPos          : Integer;
  BConstS       : string;
Begin
 repeat
 BFieldAddr:=0;
 BRef:=AParam.Name;
 BPos:=Pos('+',BRef);
 if BPos=0 then BName:=BRef
 else
  begin
  BName:=Copy(BRef,1,BPos-1);
  Delete(BRef,1,BPos);
  DelFirstLastSpace(BRef); BConstS:=BRef;
  if TryStrToDWord(BConstS,BFieldAddr)=FALSE then begin AParam.AppendError('e','Cannot convert constant to integer [R:TAsmFlowLine.AppendRef]'); break; end;
  end;
 AppendRef(AParam,ReadParamStr(BName),AType,ACodeBinPos,BFieldAddr);
 until TRUE;
End;

Procedure TAsmFlowLine.SaveCodeBin;
Begin
 FCodeBinA:=FCodeBin;
End;

Procedure TAsmFlowLine.LoadCodeBin;
Begin
 FCodeBin:=FCodeBinA;
End;

Procedure TAsmFlowLine.InvJmp ( Const ANewLabel : string );
Var
  BNewJmp       : string;
  BTail         : string;
  BFileName     : string;
  BTextLine      : Integer;
Begin
 repeat
 if FIsJxx=FALSE then begin AppendError('e',0,'Cannot invert JMP because line is not a Jxx class [R:TAsmFlowLine.InvJmp]'); break; end;
 if Length(FParams)<2 then begin AppendError('e',0,'Cannot invert JMP: parameter count <2 [R:TAsmFlowLine.InvJmp]'); break; end;
 BNewJmp:=InvJmpA(FParams[0].Name);
 BTail:=FTail;
 BFileName:=FFileName;
 BTextLine:=FTextLine;
 Parse(BNewJmp+' '+ANewLabel+' ; '+BTail,BFileName,BTextLine);
 until TRUE;
End;

Procedure TAsmFlowLine.WriteCodeBin;
Var
  BLen          : Integer;
Begin
 repeat
 if FMemSeg=nil then break;
 if FUsed=FALSE then break;
 BLen:=Length(FCodeBin); if BLen=0 then break;
 if FMemSeg.IsInside(FBaseAddr,FCodeBin)=FALSE then begin AppendError('e',0,'Code or data fall out of segment '+FMemSeg.SegName+' [R:TAsmFlowLine.WriteCodeBin]'); break; end;
 FMemSeg.WrData(FBaseAddr,FCodeBin);
 until TRUE;
End;

Procedure TAsmFlowLine.SetDstLabel ( Const ALabel : string );
Var
  BLabel        : string;
Begin
 BLabel:=ALabel;
 FDstLabel:=ReadParamStr(BLabel);
End;

Procedure TAsmFlowLine.ResetPlay;
Begin
 FUsed:=FALSE;
End;

Procedure TAsmFlowLine.AppendTail ( Var ALstLine : string );
Begin
 AddSpacesVarR(ALstLine,53);
 ALstLine:=ALstLine+' ;[AsmPos:'+IntToStr(FTextLine+1)+'][AsmFile:'+FFilename+']'+FTail;
End;

Function TAsmFlowLine.PreFormatA ( Const ALineType : string; ASpacesCnt : Integer ) : string;
Var
  BAddrS        : string;
  BDataS        : string;
  BIndex        : Integer;
Begin
 BAddrS:=''; BDataS:='';
 if Length(FCodeBin)<>0 then
  begin
  BAddrS:=IntToHex(FBaseAddr,4);
  for BIndex:=1 to Length(FCodeBin) do BDataS:=BDataS+IntToHex(Ord(FCodeBin[BIndex]),2);
  end;
 if Length(BDataS)>12 then begin SetLength(BDataS,11); BDataS:=BDataS+'>'; end;
 Result:=ALineType; while Length(Result)<2 do Result:=Result+'.';
 if FUsed then Result:=Result+'*' else Result:=Result+'.';
 Result:=Result+#32;
 Result:=Result+BAddrS; AddSpacesVarR(Result,7); Result:=Result+#32;
 Result:=Result+BDataS; AddSpacesVarR(Result,20); Result:=Result+#32;
 Result:=Result+'|';
 for BIndex:=1 to ASpacesCnt do Result:=Result+#32;
End;

Function TAsmFlowLine.FormatForLst : string;
Var
  BSegType      : char;
  BParamIdx     : Integer;
  BDummyS       : string;
Begin
 Result:='';
 if FMemSeg=nil then BSegType:='?'
 else if (FMemSeg.SegFlags and $8)<>0 then BSegType:='c'
 else BSegType:='d';
 //TCmdIs = (acUnparsed, acSysComment, acEmpty, acInclude, acIncData, acSegmC, acSegmD, acOrg, acAlign, acData, acStack, acLabel, acPublic, acExtern, acEqu, acCmd);
 case FCmdIs of
   acUnparsed:
     begin
     Result:='.?'+FOrig;
     end;
   acSysComment:
     Result:=FOrig;
   acEmpty:
     begin
     Result:=PreFormatA('..',0);
     AppendTail(Result);
     end;
   acInclude:
     begin
     Result:=PreFormatA('.i',0)+'#include "'+FIncName+'"';
     AppendTail(Result);
     end;
   acIncData:
     begin
     Result:=PreFormatA('.j',0)+'#IncData "'+FIncName+'"';
     AppendTail(Result);
     end;
   acSegName:
     begin
     if FMemSeg=nil then Result:=PreFormatA('?s',0)+'.seg <invalid>'
     else Result:=PreFormatA(BSegType+'s',0)+'.seg '+FMemSeg.SegName;
     AppendTail(Result);
     end;
   acOrg:
     begin
     Result:=PreFormatA(BSegType+'o',0)+'.org '+IntToStr(FAlign);
     AppendTail(Result);
     end;
   acAlign:
     begin
     Result:=PreFormatA(BSegType+'a',0)+'Align '+IntToStr(FAlign);
     AppendTail(Result);
     end;
   acData:
     begin
     case FAlign of
       1: BDummyS:='db';
       2: BDummyS:='dw';
       4: BDummyS:='dd';
     else BDummyS:='d?';
     end; // case
     BDummyS:=BDummyS+' ';
     for BParamIdx:=1 to Length(FParams)-1 do
      begin
      if BParamIdx<>1 then BDummyS:=BDummyS+',';
      BDummyS:=BDummyS+FParams[BParamIdx].Name;
      end;
     Result:=PreFormatA(BSegType+'d',8)+BDummyS;
     AppendTail(Result);
     end;
   acStack:
     begin
     Result:=PreFormatA(BSegType+'d',8)+'stack '+IntToStr(Length(FCodeBin));
     AppendTail(Result);
     end;
   acLabel:
     begin
     Result:=PreFormatA(BSegType+'l',5)+FLabelName+':';
     AppendTail(Result);
     end;
   acPublic:
     begin
     Result:=PreFormatA('.p',0)+'Public '+FParams[1].Name;
     AppendTail(Result);
     end;
   acExtern:
     begin
     Result:=PreFormatA('.e',0)+'Extern '+FParams[1].Name;
     AppendTail(Result);
     end;
   acEqu:
     begin
     Result:=PreFormatA('.=',0)+FExec;
     AppendTail(Result);
     end;
   acCmd:
     begin
     BDummyS:=FParams[0].Name;
     AddSpacesVarR(BDummyS,8);
     for BParamIdx:=1 to Length(FParams)-1 do
      begin
      if BParamIdx<>1 then BDummyS:=BDummyS+',';
      if FParams[BParamIdx].NameA<>'' then BDummyS:=BDummyS+FParams[BParamIdx].NameA
      else BDummyS:=BDummyS+FParams[BParamIdx].Name;
      end;
     Result:=PreFormatA(BSegType+'x',8)+BDummyS;
     AppendTail(Result);
     end;
   end;
End;

{ *** TAsmBase *** }

Constructor TAsmBase.Create;
Begin
 Inherited;
 FVerifList:=TStringList.Create;
 FTextSrc:=TStringList.Create;
 FAsmSrc:=TStringList.Create;
 //FIncSearchPath:=TStringList.Create;
 //FLocSearchPath:=TStringList.Create;
 FConstStrList:=TStringList.Create;
 FEquList:=TStringList.Create;
 FHiddenLine:=NewFlowLine; FHiddenLine.AsmBase:=Self;
 FAddressParamType:='d';
 FSymComment:=';';
 FSymMultiline:=#0;
End;

Destructor TAsmBase.Destroy;
Begin
 if FElfFile<>nil then FElfFile.Free;
 FHiddenLine.Free;
 FEquList.Free;
 FConstStrList.Free;
 RefListClear(FPublicList);
 RefListClear(FExternList);
 RefListClear(FLabelList);
 ClearFlowList;
 //FLocSearchPath.Free;
 //FIncSearchPath.Free;
 FAsmSrc.Free;
 FTextSrc.Free;
 FVerifList.Free;
 Inherited;
End;

{Procedure TAsmBase.AppendError ( AErrorType : char; Const AComment : string );
Begin
 FErrorList.Append(AErrorType+' 0 0 '+AComment+' [F:'+FFilename+']');
End;}

Procedure TAsmBase.ConstFileStart;
Begin
 FTextSrc.Clear;
 FConstInsIdx:=0;
 FTextSrc.Append('');
 FTextSrc.Append('.seg code');
End;

Procedure TAsmBase.ConstFileAppend ( AConstList : TStringList );
Var
  BIndex        : Integer;
  BOrigin       : string;
  BLabelName,
  BLabelData    : string;
  BLabelDataD   : string;
Begin
 BIndex:=0;
 while BIndex<AConstList.Count do
  begin
  BOrigin:=AConstList.Strings[BIndex];
  BLabelName:=ReadParamStr1Space(BOrigin);
  BLabelData:=BOrigin;
  FTextSrc.Insert(FConstInsIdx,'Public '+BLabelName); inc(FConstInsIdx);
  BLabelDataD:='';
  while BLabelData<>'' do
   begin
   if BLabelDataD<>'' then BLabelDataD:=BLabelDataD+', ';
   BLabelDataD:=BLabelDataD+IntToStr(Ord(BLabelData[1]));
   Delete(BLabelData,1,1);
   end;
  FTextSrc.Append(BLabelName+':');
  FTextSrc.Append('     db  '+BLabelDataD);
  inc(BIndex);
  end;
End;

Procedure TAsmBase.ConstFileAppend ( Const AName : string; Const ADataS : string );
Begin
 FTextSrc.Insert(FConstInsIdx,'Public '+AName); inc(FConstInsIdx);
 FTextSrc.Append('Align 4');
 FTextSrc.Append(AName+':');
 FTextSrc.Append('     '+ADataS);
End;

Function TAsmBase.ConstFileGetLineData ( AName : string ) : TAsmFlowLine;
Var
  BLineLabel,
  BLineData     : TAsmFlowLine;
Begin
 Result:=nil;
 repeat
 BLineLabel:=FindLabel(nil,AName); if BLineLabel=nil then break;
 if Length(FFlowList)<=(BLineLabel.TextLine+1) then break;
 BLineData:=FFlowList[BLineLabel.TextLine+1]; if BLineData=nil then break;
 if BLineData.CmdIs<>acData then break;
 Result:=BLineData;
 until TRUE;
End;

Procedure TAsmBase.Init ( Const AFilename : string; Const APrjPath, ADstPath : string; AIncSearchPath, ALocSearchPath : TStringList; Const ADefCodeSeg, ADefDataSeg : string; AGetUses : TGetUses; AReadInc : TReadInc; AUid : PUid );
Begin
 if FElfFile<>nil then FElfFile.Free;
 FSrcName:=AFilename;
 FAsmName:=AFilename;
 FPrjPath:=APrjPath;
 FDstPath:=ADstPath;
 FIncSearchPath:=AIncSearchPath;
 FLocSearchPath:=ALocSearchPath;
 FDefCodeSeg:=ADefCodeSeg;
 FDefDataSeg:=ADefDataSeg;
 if FParser<>nil then
  begin
  FParser.GetUses:=AGetUses;
  FParser.ReadInc:=AReadInc;
  FParser.Init(AFilename,FDstPath,FTextSrc);
  end;
 FUid:=AUid;
End;

Procedure TAsmBase.ClearFlowList;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].Free;
 FFlowList:=nil;
End;

Procedure TAsmBase.AppendFlowLine ( ALine : TAsmFlowLine );
Var
  BLineIdx      : Integer;
Begin
 ALine.AsmBase:=Self;
 BLineIdx:=Length(FFlowList); SetLength(FFlowList,BLineIdx+1); FFlowList[BLineIdx]:=ALine;
 ALine.FlowIdx:=BLineIdx;
End;

Function TAsmBase.AppendFlowLineA : TAsmFlowline;
Begin
 Result:=NewFlowLine;
 AppendFlowLine(Result);
End;

Function TAsmBase.AppendFlowLine ( AAddr : Cardinal ) : TAsmFlowline;
Var
  BLen,
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
Begin
 Result:=nil;
 repeat
 if FFlowList=nil then begin Result:=AppendFlowLineA; break; end;
 BLine:=nil;
 BLineIdx:=Length(FFlowList)-1;
 while BLineIdx>=0 do
  begin
  BLine:=FFlowList[BLineIdx];
  if BLine.BaseAddr<=AAddr then break;
  Dec(BLineIdx);
  end;
 if BLineIdx>=0 then begin Result:=InsertFlowLineAfter(BLine); break; end;
 Result:=NewFlowLine;
 Result.AsmBase:=Self;
 BLen:=Length(FFlowList);
 SetLength(FFlowList,BLen+1);
 BLineIdx:=Length(FFlowList)-2;
 while BLineIdx>=0 do
  begin
  BLine:=FFlowList[BLineIdx];
  BLine.FlowIdx:=BLine.FlowIdx+1;
  FFlowList[BLineIdx+1]:=BLine;
  dec(BLineIdx);
  end;
 Result.FlowIdx:=0;
 FFlowList[0]:=Result;
 until TRUE;
End;

Function TAsmBase.InsertFlowLineAfter ( AParent : TAsmFlowLine ) : TAsmFlowLine;
Var
  BLen          : Integer;
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
Begin
 Result:=NewFlowLine;
 Result.AsmBase:=Self;
 BLen:=Length(FFlowList);
 SetLength(FFlowList,BLen+1);
 BLineIdx:=Length(FFlowList)-2;
 while BLineIdx>AParent.FlowIdx do
  begin
  BLine:=FFlowList[BLineIdx];
  BLine.FlowIdx:=BLine.FlowIdx+1;
  FFlowList[BLineIdx+1]:=BLine;
  dec(BLineIdx);
  end;
 BLineIdx:=AParent.FlowIdx+1;
 Result.FlowIdx:=BLineIdx;
 FFlowList[BLineIdx]:=Result;
End;

Procedure TAsmBase.ExportExecLine ( Const ASegList : TMemSegList; AExec : TExecLineBase );
Var
  BLine     : TAsmFlowLine;
  BMemSeg   : TMemSeg;
  BSrcName  : string;
  BSrcPos   : Integer;
Begin
 repeat
 BMemSeg:=MemSegSearch(ASegList,AExec.BaseAddr);
 if BMemSeg=nil then begin AppendErrorA(FormatError('eMemory segment is not found for disassembled line "'+AExec.AsmLineS+'"',FSrcName,'0')+'[R:TAsmBase.ExportExecLine]'); break; end;
 BSrcName:=FAsmName; {This is HEX name} BSrcPos:=0;
 if AExec.ElfFile<>nil then AExec.ElfFile.ResolveSrc(AExec.VirtAddr,BSrcName,BSrcPos);
 if AExec.LabelName<>'' then
  begin
  if (Pos('Proc_',AExec.LabelName)=1) or (Pos('Entry_',AExec.LabelName)=1) then
   begin
   BLine:=AppendFlowLine(AExec.BaseAddr);
   BLine.Parse(BMemSeg,AExec.VirtAddr,AExec.BaseAddr,'','',BSrcName,BSrcPos);
   end;
  BLine:=AppendFlowLine(AExec.BaseAddr);
  BLine.Parse(BMemSeg,AExec.VirtAddr,AExec.BaseAddr,'',AExec.LabelName+':',BSrcName,BSrcPos);
  end;
 AExec.CheckFixDstLabel;
 BLine:=AppendFlowLine(AExec.BaseAddr);
 BLine.Parse(BMemSeg,AExec.VirtAddr,AExec.BaseAddr,AExec.CodeBin,AExec.AsmLineS,BSrcName,BSrcPos);
 until TRUE;
End;


Procedure TAsmBase.ExportExecLines ( Const ASegList : TMemSegList );
Var
  BChainIdx : Integer;
  BChunk    : TCodeChunk;
  BExecIdx  : Integer;
  BExec     : TExecLineBase;
Begin
 BChainIdx:=0;
 while BChainIdx<Length(FCodeChunkList) do
  begin
  BChunk:=FCodeChunkList[BChainIdx];
  BExecIdx:=0;
  while BExecIdx<Length(BChunk.ExecList) do
   begin
   BExec:=BChunk.ExecList[BExecIdx];
   if BExec<>nil then ExportExecLine(ASegList,BExec);
   inc(BExecIdx);
   end;
  inc(BChainIdx);
  end;
End;

Function TAsmBase.LoadSrc : boolean;
Var
  BFileExtS     : string;
  BError        : string;
  BLineNr       : string;
  BFixBinBase   : Cardinal;
  BFixBinData   : string;
  BChunk        : TCodeChunk;
  //BLineIdx      : Integer;
  //BHexOffset    : Cardinal;
  BChunkIdx     : Integer;
  BErrorS       : string;
  BProgIdx      : SizeInt;
  BProgItem     : TElfProgItem;

Begin
 Result:=FALSE;
 FVerifList.Clear;
 if FElfFile<>nil then FElfFile.Free;
 CodeChunkListClear(FCodeChunkList);

 repeat

 BFileExtS:=LowerCase(ExtractFileExt(FSrcName));
 if BFileExtS='.hex' then
  begin
  try
   FTextSrc.LoadFromFile(FSrcName);
  except
   break;
  end;

  // Read sections of HEX
  { Parse with NoPad option
  BError:=''; BLineIdx:=0; BHexOffset:=0;
  repeat
  BError:=HexFileParse(FTextSrc,BLineIdx,BHexOffset,BFixBinBase,BFixBinData);
  if BFixBinData='' then break;
  if BError<>'' then
   begin
   BLineNr:=ReadParamStr(BError); DelFirstSpace(BError);
   AppendErrorA(FormatError('e'+BError+' | Origin: "'+FTextSrc.Strings[BLineIdx]+'"',FSrcName,BLineNr)+'[R:TAsmBase.LoadSrc]');
   break;
   end;
  BChunk:=TCodeChunk.Create;
  BChunk.Init(Self,FSrcName,BFixBinBase,BFixBinData);
  CodeChunkListAppend(FCodeChunkList,BChunk);
  until FALSE;}

  // Parse as 1 section
  BError:=HexFileParse(FTextSrc,BFixBinBase,BFixBinData);
  if BFixBinData='' then break;
  if BError<>'' then
   begin
   BLineNr:=ReadParamStr(BError); DelFirstSpace(BError);
   AppendErrorA(FormatError('e'+BError,FSrcName,BLineNr)+'[R:TAsmBase.LoadSrc]');
   break;
   end;
  BChunk:=TCodeChunk.Create;
  BChunk.Init(Self,FSrcName,BFixBinBase,BFixBinData);
  CodeChunkListAppend(FCodeChunkList,BChunk);

  if BError<>'' then break;
  // Scan sections for marker
  BChunkIdx:=0; BChunk:=nil;
  while BChunkIdx<Length(FCodeChunkList) do
   begin
   BChunk:=FCodeChunkList[BChunkIdx];
   if BChunk.FindMarker('MirabelleSD RVE') then break;
   inc(BChunkIdx);
   end;
  if BChunkIdx>=Length(FCodeChunkList) then begin Result:=TRUE; break; end;
  if Length(FCodeChunkList)<>1 then begin AppendErrorA(FormatError('eThe file is recognised as externally linked HEX, it should contain only 1 section | Origin: "'+FTextSrc.Strings[0]+'"',FSrcName,'1')+'[R:TAsmBase.LoadSrc]'); break; end;
  if BChunk.ParseRelHdr(BErrorS)=FALSE then begin AppendErrorA(FormatError('e'+BErrorS+' | Origin: "'+FTextSrc.Strings[0]+'"',FSrcName,'1')+'[R:TAsmBase.LoadSrc]');   break; end;
  Result:=TRUE;
  break;
  end;

 if BFileExtS='.elf' then
  begin
  FElfFile:=TElfFile.Create(FOnAppendError,FSrcName);
  if FElfFile.Load=FALSE then break;
  if FElfFile.ParseAll=FALSE then break;
  FElfFile.MapLocPath(FLocSearchPath);

  BProgIdx:=0;
  while BProgIdx<FElfFile.ProgList.Count do
   begin
   BProgItem:=FElfFile.ProgList.Items[BProgIdx];
   if (BProgItem.IsLoadable) and (BProgItem.AddrP=FElfFile.ProgramEntry) then
    begin
    BChunk:=TCodeChunk.Create;
    BChunk.Init(Self,FElfFile,FSrcName,BProgItem.AddrP,BProgItem.FileSZ,BProgItem.RawData);
    CodeChunkListAppend(FCodeChunkList,BChunk);
    end;
   inc(BProgIdx);
   end;

  // Scan sections for marker
  BChunkIdx:=0; BChunk:=nil;
  while BChunkIdx<Length(FCodeChunkList) do
   begin
   BChunk:=FCodeChunkList[BChunkIdx];
   if BChunk.FindMarker('MirabelleSD RVE') then break;
   inc(BChunkIdx);
   end;
  if BChunkIdx>=Length(FCodeChunkList) then begin Result:=TRUE; break; end;
  if Length(FCodeChunkList)<>1 then begin AppendErrorA(FormatError('eThe file is recognised as externally linked ELF, it should contain only 1 section | Origin: "'+FTextSrc.Strings[0]+'"',FSrcName,'1')+'[R:TAsmBase.LoadSrc]'); break; end;
  if BChunk.ParseRelHdr(BErrorS)=FALSE then begin AppendErrorA(FormatError('e'+BErrorS+' | Origin: "'+FTextSrc.Strings[0]+'"',FSrcName,'1')+'[R:TAsmBase.LoadSrc]');   break; end;
  Result:=TRUE;
  break;
  end;

 // Regular ASM file
 try
  FTextSrc.LoadFromFile(FSrcName);
 except
  break;
 end;
 CorrectTabs(FTextSrc,8);
 DbgExtractVerifInfo(FTextSrc,FVerifList);
 Result:=TRUE;
 until TRUE;
End;

Procedure TAsmBase.AppendSrcLine ( Const ALine : string );
Begin
 FTextSrc.Append(ALine);
End;

Procedure TAsmBase.Compile;
Begin
 FAsmSrc.Clear;
 ClearFlowList;
 RefListClear(FPublicList);
 RefListClear(FExternList);
 RefListClear(FLabelList);
 FEquList.Clear;
 FHiddenLine.Clear;
End;

Function ReadAsmStatement ( Var AReadS : string; ASymComment, ASymMultiline : char ) : string;
Var
  BPosC,
  BPosM     : Integer;
Begin
 Result:='';
 BPosC:=PosSkipQ(ASymComment,AReadS);
 BPosM:=PosSkipQ(ASymMultiline,AReadS);
 repeat
 if (BPosC=0) and (BPosM=0) then begin Result:=AReadS; AReadS:=''; break; end;
 if BPosC=0 then
  begin
  Result:=Copy(AReadS,1,BPosM-1);
  Delete(AReadS,1,BPosM);
  break;
  end;
 if BPosM=0 then begin Result:=AReadS; AReadS:=''; break; end;
 if BPosC<BPosM then begin Result:=AReadS; AReadS:=''; break; end;
 Result:=Copy(AReadS,1,BPosM-1);
 Delete(AReadS,1,BPosM);
 until TRUE;
End;

Procedure TAsmBase.ParseList ( AAsmSrc : TStringList; Const AFilename : string; Const AUsedIncNames : string );
Var
  BLineIdx      : Integer;
  BReadSM,                  // Multiline
  BReadS        : string;
  BLine         : TAsmFlowLine;
  BReadSA,
  BLabelSA      : string;
  BPos,
  BIndex        : Integer;
Begin
 FSkipCodeGen:=FALSE;
 BLineIdx:=0;
 while BLineIdx<AAsmSrc.Count do
  begin
  BReadSM:=AAsmSrc.Strings[BLineIdx];
  PreprocLine(BLineIdx,BReadSM);
  repeat
  if FSymMultiline=#0 then begin BReadS:=BReadSM; BReadSM:=''; end
  else if (Length(BReadSM)>=2) and (BReadSM[1]=';') and (BReadSM[2]='@') then begin BReadS:=BReadSM; BReadSM:=''; end
  else
   begin
   BReadS:=ReadAsmStatement(BReadSM,FSymComment,FSymMultiline);
   while (BReadS='') and (BReadSM<>'') do BReadS:=ReadAsmStatement(BReadSM,FSymComment,FSymMultiline);
   if BReadS='' then break;
   end;
  // Here filter cases where Label and other share the same line
  BReadSA:=BReadS;
  BLabelSA:=ReadParamStr(BReadSA);
  if (BLabelSA<>'') and (BLabelSA[Length(BLabelSA)]=':') then
   begin
   DelFirstSpace(BReadSA);
   if (BReadSA<>'') and (BReadSA[1]<>';') then
    begin
    // Split label and line
    BPos:=Pos(BLabelSA,BReadS)+Length(BLabelSA);
    BReadSA:=Copy(BReadS,1,BPos-1);
    Delete(BReadS,1,BPos-1);
    for BIndex:=1 to BPos do Insert(#32,BReadS,1);
    BLine:=AppendFlowLineA;
    BLine.Parse(BReadSA,AFilename,BLineIdx);
    end;
   end;
  BLine:=AppendFlowLineA;
  BLine.Parse(BReadS,AFilename,BLineIdx);
  if BLine.CmdIs=acInclude then IncludeSrc(BLine,AUsedIncNames,AFilename,BLineIdx);
  if FSymMultiline=#0 then break;
  until FALSE;
  inc(BLineIdx);
  end;
End;

Function TAsmBase.GetHexInfo ( ALine : TAsmFlowLine; Out ASegStart, ASegSize : Cardinal ) : boolean;
Var
  BParamCnt     : Integer;
  BDummyInt     : Int64;
Begin
 Result:=FALSE;
 ASegStart:=0; ASegSize:=$FFFFFFFF;
 repeat
 BParamCnt:=Length(ALine.Params);
 if BParamCnt=2 then begin Result:=TRUE; break; end;
 if BParamCnt<>4 then begin ALine.AppendError('e','Invalid number of parameters after include file name [R:TAsmBase.GetHexInfo]'); break; end;
 if IsInteger(ALine.Params[2].FName,BDummyInt)=FALSE then begin ALine.Params[2].AppendError('e','Cannot convert segment start to integer [R:TAsmBase.GetHexInfo]'); break; end;
 if BDummyInt<0 then begin ALine.Params[2].AppendError('e','Invalid segment start, value must be non-negative [R:TAsmBase.GetHexInfo]'); break; end;
 ASegStart:=BDummyInt;
 if IsInteger(ALine.Params[3].FName,BDummyInt)=FALSE then begin ALine.Params[3].AppendError('e','Cannot convert segment start to integer [R:TAsmBase.GetHexInfo]'); break; end;
 if BDummyInt<0 then begin ALine.Params[3].AppendError('e','Invalid segment size, value must be non-negative [R:TAsmBase.GetHexInfo]'); break; end;
 ASegSize:=BDummyInt;
 Result:=TRUE;
 until TRUE;
End;

Function TAsmBase.GetHexInfo ( ALine : TAsmFlowLine; Out ASegStart, ASegSize : Cardinal; Out ALabelName : string ) : boolean;
Var
  BParamCnt     : Integer;
  BDummyInt     : Int64;
Begin
 Result:=FALSE;
 ASegStart:=0; ASegSize:=$FFFFFFFF;
 repeat
 BParamCnt:=Length(ALine.Params);
 if BParamCnt=2 then begin Result:=TRUE; break; end;
 if BParamCnt<>6 then begin ALine.AppendError('e','Invalid number of parameters after include file name [R:TAsmBase.GetHexInfo]'); break; end;
 if IsInteger(ALine.Params[2].FName,BDummyInt)=FALSE then begin ALine.Params[2].AppendError('e','Cannot convert segment start to integer [R:TAsmBase.GetHexInfo]'); break; end;
 if BDummyInt<0 then begin ALine.Params[2].AppendError('e','Invalid segment start, value must be non-negative [R:TAsmBase.GetHexInfo]'); break; end;
 ASegStart:=BDummyInt;
 if IsInteger(ALine.Params[3].FName,BDummyInt)=FALSE then begin ALine.Params[3].AppendError('e','Cannot convert segment start to integer [R:TAsmBase.GetHexInfo]'); break; end;
 if BDummyInt<0 then begin ALine.Params[3].AppendError('e','Invalid segment size, value must be non-negative [R:TAsmBase.GetHexInfo]'); break; end;
 if ALine.Params[4].FName<>'as' then begin ALine.Params[4].AppendError('e','"as" reserved word expected [R:TAsmBase.GetHexInfo]'); break; end;
 ALabelName:=ALine.Params[5].FName;
 ASegSize:=BDummyInt;
 Result:=TRUE;
 until TRUE;
End;

Function TAsmBase.ParseHexLine ( Const AReadS : string; ASegStart, ASegSize : Cardinal; ASegWrPos : Cardinal; Const AHexFilename : string; AHexLineIdx : Integer; Out AAddr : Cardinal; Out ADataS : string ) : boolean;
Var
  BLenA         : Integer;
  BSizeS,
  BAddrS,
  BTypeS,
  BDataS        : string;
  BAddr         : Cardinal;
  BConst        : Cardinal;
  BLineData     : TAsmFlowLine;
Begin
 Result:=FALSE;
 ADataS:='';
 AAddr:=0;
 BLenA:=Length(AReadS);
 repeat
 if BLenA<11 then begin Result:=TRUE; break; end;
 if AReadS[1]<>':' then begin Result:=TRUE; break; end;
 BSizeS:=Copy(AReadS,2,2);
 BAddrS:=Copy(AReadS,4,4);
 BTypeS:=Copy(AReadS,8,2);
 BDataS:=Copy(AReadS,10,BLenA-11);
 if HexToDWordCheck(BAddrS,BAddr)=FALSE then
  begin
  BLineData:=AppendFlowLineA;
  BLineData.Parse('',AHexFilename,AHexLineIdx);
  BLineData.AppendError('e','Cannot convert address part to integer [R:TAsmBase.ParseHexLine]');
  break;
  end;
 if (BAddr<ASegStart) or (BAddr>(ASegStart+ASegSize)) then begin Result:=TRUE; break; end;
 if BTypeS<>'00' then begin Result:=TRUE; break; end;
 if HexToDWordCheck(BSizeS,BConst)=FALSE then
  begin
  BLineData:=AppendFlowLineA;
  BLineData.Parse('',AHexFilename,AHexLineIdx);
  BLineData.AppendError('e','Cannot convert size part to integer [R:TAsmBase.ParseHexLine]');
  break;
  end;
 if (BConst*2)<>Length(BDataS) then
  begin
  BLineData:=AppendFlowLineA;
  BLineData.Parse('',AHexFilename,AHexLineIdx);
  BLineData.AppendError('e','Specified data size does not correspond to actual data size [R:TAsmBase.ParseHexLine]');
  break;
  end;
 if ASegWrPos>BAddr then // Overlap
  begin
  BLineData:=AppendFlowLineA;
  BLineData.Parse('',AHexFilename,AHexLineIdx);
  BLineData.AppendError('e','Data overlap in source HEX file [R:TAsmBase.ParseHexLine]');
  break;
  end;
 AAddr:=BAddr;
 ADataS:=BDataS;
 Result:=TRUE;
 until TRUE;
End;

Procedure TAsmBase.ParseHexTxt ( ALine : TAsmFlowLine; AHexSrc : TStringList; Const AHexFilename : string );
Var
  BLineIdx      : Integer;
  BWriteS       : string;
  BLineSize,
  BLineData     : TAsmFlowLine;
  BLastLineIdx  : Integer;
  BSegStart,
  BSegSize      : Cardinal;
  BSegEnd       : Cardinal;
  BDataS        : string;
  BAddr         : Cardinal;
  BDummyS       : string;
  BPadIdx       : Integer;
Begin
 repeat
 if GetHexInfo(ALine,BSegStart,BSegSize)=FALSE then break;
 BSegEnd:=BSegStart;
 BLineSize:=AppendFlowLineA;
 BLastLineIdx:=0;
 BLineIdx:=0;
 while BLineIdx<AHexSrc.Count do
  begin
  if ParseHexLine(AHexSrc.Strings[BLineIdx],BSegStart,BSegSize,BSegEnd,AHexFilename,BLineIdx,BAddr,BDataS)=FALSE then break;
  if BSegEnd<BAddr then // Padding
   begin
   BWriteS:='    db ';
   BPadIdx:=0;
   while BSegEnd<BAddr do
    begin
    if BPadIdx<>0 then BWriteS:=BWriteS+', ';
    BWriteS:=BWriteS+'0x00';
    inc(BPadIdx);
    if BPadIdx>=16 then
     begin
     BLineData:=AppendFlowLineA;
     BLineData.Parse(BWriteS+' ; // Padding ',AHexFilename,BLineIdx);
     BWriteS:='    db ';
     BPadIdx:=0;
     end;
    inc(BSegEnd);
    end;
   if BPadIdx>0 then
    begin
    BLineData:=AppendFlowLineA;
    BLineData.Parse(BWriteS+' ; // Padding ',AHexFilename,BLineIdx);
    end;
   end;
  BWriteS:='    db ';
  while BDataS<>'' do
   begin
   BDummyS:=Copy(BDataS,1,2); Delete(BDataS,1,2);
   BWriteS:=BWriteS+'0x'+BDummyS; if BDataS<>'' then BWriteS:=BWriteS+', ';
   inc(BSegEnd);
   end;
  BLineData:=AppendFlowLineA;
  BLineData.Parse(BWriteS,AHexFilename,BLineIdx);
  BLastLineIdx:=BLineIdx;
  inc(BLineIdx);
  end;
 BLineSize.Parse('    dd '+IntToStr(BSegEnd-BSegStart),AHexFilename,BLastLineIdx+1);
 until TRUE;
End;

Function TAsmBase.ResolveIncName ( ALine : TAsmFlowLine; Const AUsedIncNames : string; Const AFilename : string; ALineIdx : Integer ) : string;
Var
  BSearchIdx    : Integer;
  BUsedIncNames : string;
  BIncFilenameA : string;
Begin
 BUsedIncNames:=AUsedIncNames+AFilename+' ';
 Result:='';
 BSearchIdx:=0;
 while BSearchIdx<FIncSearchPath.Count do
  begin
  BIncFilenameA:=IncludeTrailingPathDelimiter(FIncSearchPath.Strings[BSearchIdx])+ALine.IncName;
  if FileExists(BIncFilenameA) then
   begin
   if Result<>'' then AppendErrorA('e',ALineIdx,0,AFilename,'There is more than one location of include file with the same name: '+ALine.IncName+' ('+Result+' and '+BIncFilenameA+') [R:TAsmBase.LoadIncFile]')
   else if StrInList(BIncFilenameA,BUsedIncNames) then AppendErrorA('e',ALineIdx,0,AFilename,'Recursive include: '+ALine.IncName+' ('+BIncFilenameA+')[R:TAsmBase.LoadIncFile]')
   else Result:=BIncFilenameA;
   end;
  inc(BSearchIdx);
  end;
End;

Procedure TAsmBase.IncludeSrc ( ALine : TAsmFlowLine; Const AUsedIncNames : string; Const AFilename : string; ALineIdx : Integer );
Var
  BIncFilename  : string;
  BUsedIncNames : string;
  BTextSrc      : TStringList;
  BExt          : string;
Begin
 BTextSrc:=TStringList.Create;
 BUsedIncNames:='';
 repeat
 BIncFilename:=ResolveIncName(ALine,AUsedIncNames,AFilename,ALineIdx);
 if BIncFilename='' then begin AppendErrorA('e',ALineIdx,0,AFilename,'Include file is not found: '+ALine.IncName+'[R:TAsmBase.LoadIncFile]'); break; end;
 BExt:=LowerCase(ExtractFileExt(ALine.IncName));
 try
  BTextSrc.LoadFromFile(BIncFilename);
 except
  AppendErrorA('e',ALineIdx,0,AFilename,'Cannot load include file: '+ALine.IncName+'(Full name: '+BIncFilename+')[R:TAsmBase.LoadIncFile]');
  break;
 end;
 if BExt='.hex' then ParseHexTxt(ALine,BTextSrc,BIncFilename)
 //else if BExt='.memh' then ParseMemhTxt(ALine,BTextSrc,BIncFilename)
 else ParseList(BTextSrc,BIncFilename,BUsedIncNames);
 until TRUE;
 BTextSrc.Free;
End;

Procedure ReplaceOpcodes ( Var AEval : string; Const ACmpList, ADstList : string );
Var
  BCmpList,
  BDstList  : string;
  BCmpItem,
  BDstItem  : string;
  BPosA     : Integer;
Begin
 BCmpList:=ACmpList;
 BDstList:=ADstList;
 repeat
 BCmpItem:=ReadParamStr(BCmpList);
 BDstItem:=ReadParamStr(BDstList);
 if BCmpItem='' then break;

 repeat
 BPosA:=Pos(BCmpItem,AEval);
 if BPosA=0 then break;
 Delete(AEval,BPosA,Length(BCmpItem));
 Insert(' '+BDstItem+' ',AEval,BPosA);
 until FALSE;

 repeat
 BPosA:=Pos('  ',AEval);
 if BPosA=0 then break;
 Delete(AEval,BPosA,1);
 until FALSE;

 until FALSE;
End;

Function OpInt ( ADataA, ADataB : Int64; Const AOper : char; Out ATarg : TVarData ) : boolean;
Var
  BTarg     : Int64;
Begin
 Result:=FALSE;
 ATarg.FType:='i';
 repeat
 BTarg:=0;
 if AOper='*' then BTarg:=ADataA*ADataB
 else if (AOper='/') and (ADataB<>0) then BTarg:=ADataA div ADataB
 else if AOper='+' then BTarg:=ADataA+ADataB
 else if AOper='-' then BTarg:=ADataA-ADataB
 else if AOper='<' then BTarg:=ADataA shl ADataB
 else if AOper='>' then BTarg:=ADataA shr ADataB
 else if AOper='&' then BTarg:=ADataA and ADataB
 else if AOper='|' then BTarg:=ADataA or ADataB
 else break;
 ATarg.FDataI:=BTarg;
 Result:=TRUE;
 until TRUE;
End;

Function OpFloat ( ADataA, ADataB : Double; Const AOper : Char; Out ATarg : TVarData ) : boolean;
Var
  BTarg     : Double;
Begin
 Result:=FALSE;
 ATarg.FType:='f';
 repeat
 BTarg:=0;
 if AOper='*' then BTarg:=ADataA*ADataB
 else if (AOper='/') and (ADataB<>0) then BTarg:=ADataA/ADataB
 else if AOper='+' then BTarg:=ADataA+ADataB
 else if AOper='-' then BTarg:=ADataA-ADataB
 else break;
 ATarg.FDataF:=BTarg;
 Result:=TRUE;
 until TRUE;
End;

Function AsmBase_ReadInside ( Var ASrc : string; APos : Integer; Out ADst : string ) : boolean;
Var
  BDataC        : char;
  BLevel        : string;
Begin
 Result:=FALSE;
 BLevel:=ASrc[APos]; Delete(ASrc,APos,1);
 ADst:=BLevel;
 while APos<=Length(ASrc) do
  begin
  BDataC:=ASrc[APos]; Delete(ASrc,APos,1);
  ADst:=ADst+BDataC;
  if BDataC in ['(', '['] then BLevel:=BLevel+BDataC
  else if BDataC=')' then
   begin
   if BLevel[Length(BLevel)]='(' then Delete(BLevel,Length(BLevel),1)
   else break;
   end
  else if BDataC=']' then
   begin
   if BLevel[Length(BLevel)]='[' then Delete(BLevel,Length(BLevel),1)
   else break;
   end;
  if BLevel='' then begin Result:=TRUE; break; end;
  end;
End;

Function TAsmBase.AtomizeEvalNoBr ( Const AEval : TVarDataList; Out ATarg : TVarData ) : boolean;
Var
  BEval     : TVarDataList;
  BTarg,
  BTargA,
  BTargB    : TVarData;
  BOper     : char;
  BIndex    : Integer;
Begin
 Result:=FALSE;
 BEval:=Copy(AEval);
 repeat
 if Length(BEval)=0 then break;
 if Length(BEval)=1 then begin ATarg:=BEval[0]; Result:=TRUE; break; end;
 if BEval[0].FType='-' then
  begin
  BTarg:=BEval[1];
  if BTarg.FType='i' then begin BEval[0].FDataI:=-BTarg.FDataI; BEval[0].FType:='i'; end
  else if BTarg.FType='f' then begin BEval[0].FDataF:=-BTarg.FDataF; BEval[0].FType:='f'; end
  else break;
  VarDataDel(BEval,1,1);
  end;

 BIndex:=0;
 while BIndex<Length(BEval) do
  begin
  BTarg:=BEval[BIndex];
  if BTarg.FType in ['*', '/'] then
   begin
   BOper:=BTarg.FType;
   if (BIndex<1) or ((BIndex+1)>=Length(BEval)) then break;
   BTargA:=BEval[BIndex-1]; BTargB:=BEval[BIndex+1];
   if (BTargA.FType='i') and (BTargB.FType='i') then
    begin
    if OpInt(BTargA.FDataI,BTargB.FDataI,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='i') and (BTargB.FType='f') then
    begin
    if OpFloat(BTargA.FDataI,BTargB.FDataF,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='f') and (BTargB.FType='i') then
    begin
    if OpFloat(BTargA.FDataF,BTargB.FDataI,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='f') and (BTargB.FType='f') then
    begin
    if OpFloat(BTargA.FDataF,BTargB.FDataF,BOper,BTarg)=FALSE then break;
    end
   else break;
   VarDataDel(BEval,BIndex,2); BEval[BIndex-1]:=BTarg;
   end
  else inc(BIndex);
  end;
 if BIndex<Length(BEval) then break;

 BIndex:=0;
 while BIndex<Length(BEval) do
  begin
  BTarg:=BEval[BIndex];
  if BTarg.FType in ['+', '-', '<', '>', '&', '|'] then
   begin
   BOper:=BTarg.FType;
   if (BIndex<1) or ((BIndex+1)>=Length(BEval)) then break;
   BTargA:=BEval[BIndex-1]; BTargB:=BEval[BIndex+1];
   if (BTargA.FType='i') and (BTargB.FType='i') then
    begin
    if OpInt(BTargA.FDataI,BTargB.FDataI,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='i') and (BTargB.FType='f') then
    begin
    if OpFloat(BTargA.FDataI,BTargB.FDataF,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='f') and (BTargB.FType='i') then
    begin
    if OpFloat(BTargA.FDataF,BTargB.FDataI,BOper,BTarg)=FALSE then break;
    end
   else if (BTargA.FType='f') and (BTargB.FType='f') then
    begin
    if OpFloat(BTargA.FDataF,BTargB.FDataF,BOper,BTarg)=FALSE then break;
    end
   else break;
   VarDataDel(BEval,BIndex,2); BEval[BIndex-1]:=BTarg;
   end
  else inc(BIndex);
  end;
 if BIndex<Length(BEval) then break;

 until FALSE;
End;

Const
  CSymbols = '()*/+-<>&|';

Function TAsmBase.AtomizeEval ( Const AEval : string; Out ATarg : TVarData; Out AProcessed : boolean ) : boolean;
Var
  BEval         : TVarDataList;
  BTarg         : TVarData;
  BPosA         : Integer;
  BDataC        : Char;
  BDataS        : string;
  BIndex        : Integer;
  BIndexA,
  BIndexB       : Integer;
Begin
 BEval:=nil;
 AProcessed:=FALSE;

 repeat
 BIndex:=0;
 while BIndex<Length(AEval) do
  begin
  BDataC:=AEval[1+BIndex];
  BPosA:=Pos(BDataC,CSymbols);
  if BPosA<>0 then
   begin
   if (BDataC='<') and ((BIndex+1)<Length(AEval)) and (AEval[1+BIndex+1]='<') then Inc(BIndex)
   else if (BDataC='>') and ((BIndex+1)<Length(AEval)) and (AEval[1+BIndex+1]='>') then Inc(BIndex);
   BTarg.FType:=BDataC;
   VarDataAdd(BEval,BTarg);
   AProcessed:=TRUE;
   inc(BIndex);
   end
  else
   begin
   BDataS:=BDataC; inc(BIndex);
   while BIndex<Length(AEval) do
    begin
    BDataC:=AEval[1+BIndex];
    if Pos(BDataC,CSymbols)<>0 then break;
    BDataS:=BDataS+BDataC;
    inc(BIndex);
    end;
   DelFirstLastSpace(BDataS);
   if BDataS='' then
   else
    begin
    if IsIntegerEqu(BDataS,BTarg.FDataI) then BTarg.FType:='i'
    else if IsFloatEqu(BDataS,BTarg.FDataF) then BTarg.FType:='f'
    else break;
    VarDataAdd(BEval,BTarg);
    end;
   end;
  end;
 if BIndex<Length(AEval) then break;

 BIndexB:=0;
 while BIndexB<Length(BEval) do
  begin
  if (BEval[BIndexB].FType=')') then
   begin
   BIndexA:=BIndexB-1;
   while BIndexA>=0 do
    begin
    if (BEval[BIndexA].FType='(') then break;
    dec(BIndexA);
    end;
   if BIndexA<0 then break;
   if AtomizeEvalNoBr(Copy(BEval,BIndexA+1,BIndexB-BIndexA-1),BTarg)=FALSE then break;
   AProcessed:=TRUE;
   VarDataDel(BEval,BIndexA+1,BIndexB-BIndexA);
   BEval[BIndexA]:=BTarg;
   BIndexB:=BIndexA+1;
   end
  else inc(BIndexB);
  end;
 if BIndexB<Length(BEval) then break;

 Result:=AtomizeEvalNoBr(BEval,ATarg);
 BEval:=nil;

 until TRUE;
End;

Function TAsmBase.IsIntegerEqu ( Const AName : string; Out ADataI : Int64 ) : boolean;
Var
  BName         : string;
  BConstC       : Cardinal;
Begin
 BName:=FEquList.Values[AName];
 if BName='' then BName:=AName;
 ADataI:=0;
 Result:=FALSE;
 repeat
 if IsInteger(BName,ADataI) then begin Result:=TRUE; break; end;
 if IsChar(BName,BConstC) then begin ADataI:=BConstC; Result:=TRUE; break; end;
 until TRUE;
End;

Function TAsmBase.IsFloatEqu ( Const AName : string; Out ADataF : Double ) : boolean;
Var
  BName         : string;
Begin
 BName:=FEquList.Values[AName];
 if BName='' then BName:=AName;
 Result:=IsFloat(BName,ADataF);
End;

Function TAsmBase.TryIntegerEqu ( AParam : TAsmLineParam; Out ADataI : Int64 ) : boolean;
Var
  BProcessedA   : boolean;
  BEval         : TVarData;
Begin
 Result:=FALSE; ADataI:=0;
 repeat
 if AtomizeEval(AParam.Name,BEval,BProcessedA)=FALSE then break;
 if BEval.FType<>'i' then break;
 if BProcessedA then AParam.NameA:='0x'+IntToHex(Cardinal(BEval.FDataI),8);
 ADataI:=BEval.FDataI;
 Result:=TRUE;
 until TRUE;
End;

Function IsLabelF ( Const AName : string; Out ANameA : string ) : boolean;
Begin
 Result:=FALSE; ANameA:='';
 repeat
 if Length(AName)<2 then break;
 if AName[1] in ['1'..'9'] then
 else break;
 if AName[Length(AName)]<>'f' then break;
 ANameA:=Copy(AName,1,Length(AName)-1);
 Result:=TRUE;
 until TRUE;
End;

Function IsLabelB ( Const AName : string; Out ANameA : string ) : boolean;
Begin
 Result:=FALSE; ANameA:='';
 repeat
 if Length(AName)<2 then break;
 if AName[1] in ['1'..'9'] then
 else break;
 if AName[Length(AName)]<>'b' then break;
 ANameA:=Copy(AName,1,Length(AName)-1);
 Result:=TRUE;
 until TRUE;
End;

Function TAsmBase.FindLabel ( ALineThis : TAsmFlowLine; Const AName : string ) : TAsmFlowLine;
Var
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BNameA        : string;
Begin
 Result:=nil;
 repeat
 if (ALineThis<>nil) and IsLabelF(AName,BNameA) then
  begin
  BLineIdx:=ALineThis.FlowIdx;
  while BLineIdx<Length(FFlowList) do
   begin
   BLine:=FFlowList[BLineIdx];
   if (BLine.CmdIs=acLabel) and (BLine.LabelName=BNameA) then begin Result:=BLine; break; end;
   inc(BLineIdx);
   end;
  break;
  end;
 if (ALineThis<>nil) and IsLabelB(AName,BNameA) then
  begin
  BLineIdx:=ALineThis.FlowIdx;
  while BLineIdx>=0 do
   begin
   BLine:=FFlowList[BLineIdx];
   if (BLine.CmdIs=acLabel) and (BLine.LabelName=BNameA) then begin Result:=BLine; break; end;
   dec(BLineIdx);
   end;
  break;
  end;
 for BLineIdx:=0 to Length(FFlowList)-1 do
  begin
  BLine:=FFlowList[BLineIdx];
  if (BLine.CmdIs=acLabel) and (BLine.LabelName=AName) then begin Result:=BLine; break; end;
  end;
 until TRUE;
End;

Function TAsmBase.NextEp ( Var ALineIdx : Integer ) : TAsmFlowLine;
Var
  BLine         : TAsmFlowLine;
Begin
 Result:=nil;
 while ALineIdx<Length(FFlowList) do
  begin
  BLine:=FFlowList[ALineIdx];
  inc(ALineIdx);
  if (BLine.CmdIs=acLabel) and (Pos('_@ep',BLine.LabelName)<>0) then begin Result:=BLine; break; end;
  end;
End;

Procedure TAsmBase.FixAddr ( Var ASegList : TMemSegList );
Var
  BLineIdx      : Integer;
  BMemSeg       : TMemSeg;
Begin
 BMemSeg:=nil;
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].FixAddr(ASegList,BMemSeg);
End;

Procedure TAsmBase.FillPublicAddr;
Var
  BRefIdx       : Integer;
  BRef          : TAsmRef;
  BLabel        : TAsmFlowLine;
Begin
 for BRefIdx:=0 to Length(FPublicList)-1 do
  begin
  BRef:=FPublicList[BRefIdx];
  BLabel:=FindLabel(nil,BRef.Name);
  if BLabel=nil then BRef.AppendError('e','Public reference %p not found')
  else BRef.ObjectAddr:=BLabel.BaseAddr;
  end;
End;

{Procedure TAsmBase.CheckExternUsed;
Var
  BExternIdx    : Integer;
  BExtern       : TAsmRef;
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BRefIdx       : Integer;
  BRef          : TAsmRef;
Begin
 BExternIdx:=0;
 while BExternIdx<Length(FExternList) do
  begin
  BExtern:=FExternList[BExternIdx];
  BLineIdx:=0;
  while BLineIdx<Length(FFlowList) do
   begin
   BLine:=FFlowList[BLineIdx];
   BRefIdx:=0;
   while BRefIdx<Length(BLine.RefList) do
    begin
    BRef:=BLine.RefList[BRefIdx];
    if BRef.Name=BExtern.Name then break;
    inc(BRefIdx);
    end;
   if BRefIdx<Length(BLine.RefList) then break;
   inc(BLineIdx);
   end;
  if BLineIdx<Length(FFlowList) then inc(BExternIdx)
  else
   begin
   AppendErrorA('h',BExtern.TextLine,BExtern.TextPos,BExtern.Filename,'External reference is not used in the file and will be removed');
   RefListDelItem(FExternList,BExternIdx);
   end;
  end;
End;}

Procedure TAsmBase.FillLabelList;
Var
  BLabelIdx     : Integer;
  BLabel        : TAsmRef;
  BLine         : TAsmFlowLine;
Begin
 for BLabelIdx:=0 to Length(FLabelList)-1 do
  begin
  BLabel:=FLabelList[BLabelIdx];
  BLine:=FindLabel(nil,BLabel.Name);
  if BLine=nil then BLabel.AppendError('e','Internal error: cannot find previously declared label [R:TAsmBase.FillLabelList]')
  else BLabel.ObjectAddr:=BLine.BaseAddr;
  end;
End;

Procedure TAsmBase.FillExternList ( Const APublicList : TAsmRefList );
Var
  BPublicIdx    : Integer;
  BExternIdx    : Integer;
  BExtern       : TAsmRef;
Begin
 for BExternIdx:=0 to Length(FExternList)-1 do
  begin
  BExtern:=FExternList[BExternIdx];
  BPublicIdx:=0;
  while BPublicIdx<Length(APublicList) do
   begin
   if APublicList[BPublicIdx].Name=BExtern.Name then break;
   inc(BPublicIdx);
   end;
  if BPublicIdx>=Length(APublicList) then BExtern.AppendError('e','Linker cannot resolve external reference '+BExtern.Name+' [R:TAsmBase.FillExternList]')
  else BExtern.ObjectAddr:=APublicList[BPublicIdx].ObjectAddr;
  end;
 for BExternIdx:=0 to Length(FHiddenLine.RefList)-1 do
  begin
  BExtern:=FHiddenLine.RefList[BExternIdx];
  BPublicIdx:=0;
  while BPublicIdx<Length(APublicList) do
   begin
   if APublicList[BPublicIdx].Name=BExtern.Name then break;
   inc(BPublicIdx);
   end;
  if BPublicIdx>=Length(APublicList) then BExtern.AppendError('e','Linker cannot resolve external reference '+BExtern.Name+' [R:TAsmBase.FillExternList]')
  else BExtern.ObjectAddr:=APublicList[BPublicIdx].ObjectAddr;
  end;
End;

Procedure TAsmBase.SaveCodeBin;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].SaveCodeBin;
End;

Procedure TAsmBase.LoadCodeBin;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].LoadCodeBin;
End;

Procedure TAsmBase.UpdateTextRefs ( Var AIsAddrOor : boolean );
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].UpdateRefs(FLabelList,FExternList,AIsAddrOor);
End;

Procedure TAsmBase.WriteCodeBin ( Const ASegList : TMemSegList );
Var
  BChainIdx     : Integer;
  BChunk        : TCodeChunk;
  BLineIdx      : Integer;
  BMemSeg       : TMemSeg;
  BFillSize     : Cardinal;
  BBaseAddr     : Cardinal;
Begin
 repeat
 if FCodeChunkList=nil then
  begin
  for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].WriteCodeBin;
  end
 else
  begin
  BChainIdx:=0;
  while BChainIdx<Length(FCodeChunkList) do
   begin
   BChunk:=FCodeChunkList[BChainIdx];
   if BChunk.CanRelocate then BBaseAddr:=BChunk.RelBinBase else BBaseAddr:=BChunk.FixBinBase;
   BMemSeg:=MemSegSearch(ASegList,BBaseAddr);
   if BMemSeg=nil then begin AppendErrorA('w',0,0,FSrcName,'There is no memory segment to fit this file data [R:TAsmBase.WriteCodeBin]'); break; end;
   if BMemSeg.IsInside(BBaseAddr,BChunk.FixBinData)=FALSE then begin AppendErrorA('w',0,0,FSrcName,'Data falls out of segment dimension [R:TAsmBase.WriteCodeBin]'); break; end;
   BMemSeg.WrData(BBaseAddr,BChunk.FixBinData);
   BFillSize:=BBaseAddr+Length(BChunk.FixBinData)-BMemSeg.HwBase;
   if BMemSeg.FillSize<BFillSize then BMemSeg.FillSize:=BFillSize;
   inc(BChainIdx);
   end;
  end;
 until TRUE;
End;

Procedure TAsmBase.WriteLst ( AList : TStringList );
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do AList.Append(FFlowList[BLineIdx].FormatForLst);
End;

Procedure TAsmBase.ResetPlay;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].ResetPlay;
End;

Procedure TAsmBase.MarkLinesUsed;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlowList)-1 do FFlowList[BLineIdx].Used:=TRUE;
End;

Function TAsmBase.PreprocLine ( ALineIdx : Integer; Var AOrigin : string ) : boolean;
Begin
 Result:=FALSE;

 repeat
 if PreprocDateTimeStr(AOrigin)=FALSE then break;
 if PreprocConstStrCS(ALineIdx,AOrigin)=FALSE then break;
 if PreprocTimeHexImm(AOrigin)=FALSE then break;
 if PreprocMath(AOrigin)=FALSE then break;
 Result:=TRUE;
 until TRUE;
End;

Function TAsmBase.PreprocDateTimeStr ( Var AOrigin : string ) : boolean;
Var
  BOrigin       : string;
  BPos          : Integer;
Begin
 Result:=FALSE;

 repeat
 BPos:=Pos('#DateTimeStr',AOrigin);
 if BPos=0 then begin Result:=TRUE; break; end;

 BOrigin:=AOrigin;
 while BPos<>0 do
  begin
  Delete(BOrigin,BPos,12);
  Insert(#39+FormatDateTime('YYYY MMM DD, HH:NN.SS',Now)+#39,BOrigin,BPos);
  BPos:=Pos('#DateTimeStr',BOrigin);
  end;

 Result:=BPos=0;
 AOrigin:=BOrigin;
 until TRUE;
End;

Const
  CConstStrCSOrig = '#ConstStrCSO';
  CConstStrCSCpp  = '#ConstStrCSC';
  CConstStrCSPas  = '#ConstStrCSP';
Type
  TConstStrKind = (sckNone, sckOrig, sckCpp, sckPas);

Function TAsmBase.PreprocConstStrCS ( ALineIdx : Integer; Var AOrigin : string ) : boolean;
Var
  BPos          : Integer;
  BOrigin       : string;
  BConstStr     : string;
  BIndex,
  BLen          : Integer;
  BDataC        : char;
  BReadS,
  BDummyS       : string;
  BDummyInt     : Integer;
  BDummyCard    : Cardinal;
  BExternName   : string;
  BMarkerLen    : Integer;
  BMarkerType   : TConstStrKind;
  BParam        : TAsmLineParam;
Begin
 Result:=FALSE;
 BDummyInt:=0; BDummyCard:=0; BDataC:=#0;
 BReadS:='';
 BDummyS:='';
 BMarkerLen:=0;
 BMarkerType:=sckNone;

 repeat
 BPos:=Pos(CConstStrCSOrig,AOrigin);
 if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSOrig); BMarkerType:=sckOrig; end
 else
  begin
  BPos:=Pos(CConstStrCSCpp,AOrigin);
  if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSCpp); BMarkerType:=sckCpp; end
  else
   begin
   BPos:=Pos(CConstStrCSPas,AOrigin);
   if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSPas); BMarkerType:=sckPas; end
   else begin Result:=TRUE; break; end;
   end;
  end;

 BOrigin:=AOrigin;
 BLen:=Length(BOrigin);
 while BPos<>0 do
  begin
  BConstStr:=''; BIndex:=BPos+BMarkerLen;
  if StrSkipSpace(BOrigin,BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'"(" expected after "ConstStrCS"'); break; end;
  if BDataC<>'(' then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'"(" expected after "ConstStrCS"'); break; end;
  inc(BIndex);
  repeat
  if StrSkipSpace(BOrigin,BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'")" expected after "ConstStrCS"'); break; end;
  if BDataC=')' then begin Result:=TRUE; break; end;
  inc(BIndex);
  if BDataC=#39 then
   begin
   if StrReadTill(BOrigin,BReadS,''+#39,BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Closing ['+#39+'] is not found'); break; end;
   if BDataC<>#39 then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Closing ['+#39+'] is not found'); break; end;
   if BMarkerType=sckCpp then
    begin
    BDummyS:=ResolveCMod(BReadS);
    BReadS:=BDummyS;
    end;
   BConstStr:=BConstStr+BReadS;
   inc(BIndex);
   if StrReadTill(BOrigin,BDummyS,'+)',BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Unexpected end of line'); break; end;
   DelFirstSpace(BDummyS); DelLastSpace(BDummyS);
   if BDummyS<>'' then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Misplaces constant '+BDummyS); break; end;
   end
  else if BDataC=#34 then
   begin
   if StrReadTill(BOrigin,BReadS,''+#34,BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Closing ['+#39+'] is not found'); break; end;
   if BDataC<>#34 then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Closing ['+#34+'] is not found'); break; end;
   if BMarkerType=sckCpp then
    begin
    BDummyS:=ResolveCMod(BReadS);
    BReadS:=BDummyS;
    end;
   BConstStr:=BConstStr+BReadS;
   inc(BIndex);
   if StrReadTill(BOrigin,BDummyS,'+)',BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Unexpected end of line'); break; end;
   DelFirstSpace(BDummyS); DelLastSpace(BDummyS);
   if BDummyS<>'' then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Misplaces constant '+BDummyS); break; end;
   end
  else
   begin
   if StrReadTill(BOrigin,BDummyS,'+)',BDataC,BIndex,BLen)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Unexpected end of line'); break; end;
   DelFirstSpace(BDummyS); DelLastSpace(BDummyS);
   if BDummyS='' then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Invalid constant (empty)'); break; end;
   if Pos('#',BDummyS)=1 then
    begin
    Delete(BDummyS,1,1);
    if StringToInteger(BDummyS,BDummyInt)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Invalid constant '+BDummyS); break; end;
    if BDummyInt>255 then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Char constant '+BDummyS+' cannot exceed 255'); break; end;
    BConstStr:=BConstStr+Chr(BDummyInt);
    end
   else if Pos('0x',BDummyS)=1 then
    begin
    Delete(BDummyS,1,2);
    if BDummyS='' then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Invalid constant (format error)'); break; end;
    if HexToDWordCheck(BDummyS,BDummyCard)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Invalid constant 0x'+BDummyS); break; end;
    if BDummyCard>255 then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Char constant '+BDummyS+' cannot exceed 255'); break; end;
    BConstStr:=BConstStr+Chr(BDummyCard);
    end
   else
    begin
    if StringToInteger(BDummyS,BDummyInt)=FALSE then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Invalid constant '+BDummyS); break; end;
    if BDummyInt>255 then begin AppendErrorA('e',ALineIdx,BIndex,FAsmName,'Char constant '+BDummyS+' cannot exceed 255'); break; end;
    BConstStr:=BConstStr+Chr(BDummyInt);
    end;
   end;
  inc(BIndex);
  if BDataC=')' then begin Result:=TRUE; break; end;
  until FALSE;

  if Result=FALSE then break;

  if BMarkerType=sckCpp then BConstStr:=BConstStr+#0
  else if BMarkerType=sckPas then BConstStr:=Chr(Length(BConstStr))+BConstStr;

  // Replace original data by label
  Delete(BOrigin,BPos,BIndex-BPos);
  BExternName:='ConstStrCS_'+IntToStr(FUid^);
  Insert(BExternName,BOrigin,BPos);
  inc(FUid^);
  BParam:=FHiddenLine.AppendParam(BExternName,BPos);
  FHiddenLine.AppendRef(BParam,FAddressParamType,0);
  //RefListAppend(FExternList,Self,BExternName,ALineIdx,BIndex);
  FConstStrList.Append(BExternName+' '+BConstStr);

  //BPos:=Pos('#ConstStrCS',BOrigin);
  BPos:=Pos(CConstStrCSOrig,BOrigin);
  if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSOrig); BMarkerType:=sckOrig; end
  else
   begin
   BPos:=Pos(CConstStrCSCpp,BOrigin);
   if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSCpp); BMarkerType:=sckCpp; end
   else
    begin
    BPos:=Pos(CConstStrCSPas,BOrigin);
    if BPos<>0 then begin BMarkerLen:=Length(CConstStrCSPas); BMarkerType:=sckPas; end
    else begin Result:=TRUE; break; end;
    end;
   end;

  BLen:=Length(BOrigin);
  end;

 Result:=BPos=0;
 AOrigin:=BOrigin;
 until TRUE;
End;

Function TAsmBase.PreprocTimeHexImm ( Var AOrigin : string ) : boolean;
Var
  BOrigin       : string;
  BPos          : Integer;
Begin
 Result:=FALSE;

 repeat
 BPos:=Pos('#TimeHexImm',AOrigin);
 if BPos=0 then begin Result:=TRUE; break; end;

 BOrigin:=AOrigin;
 while BPos<>0 do
  begin
  Delete(BOrigin,BPos,11);
  Insert('0x'+FormatDateTime('HHNN',Now),BOrigin,BPos);
  BPos:=Pos('#TimeHexImm',BOrigin);
  end;

 Result:=BPos=0;
 AOrigin:=BOrigin;
 until TRUE;
End;

Function TAsmBase.PreprocMath ( Var AOrigin : string ) : boolean;
Var
  BOrigin       : string;
  BPos          : Integer;
  BIndex        : Integer;
  BMul          : Double;
  BDummyS       : string;
  BChange       : Boolean;
Begin
 BOrigin:=AOrigin;
 BChange:=FALSE;

 BPos:=Pos('#MathFactR32',BOrigin);
 while BPos<>0 do
  begin
  BChange:=TRUE;
  Delete(BOrigin,BPos,Length('#MathFactR32'));
  BIndex:=2; BDummyS:=''; BMul:=1;
  while BIndex<34 do
   begin
   BMul:=BMul*BIndex;
   if BDummyS<>'' then BDummyS:=BDummyS+', ';
   BDummyS:=BDummyS+FloatToStr(1/BMul);
   inc(BIndex);
   end;
  Insert(BDummyS,BOrigin,BPos);
  BPos:=Pos('#MathFactR32',BOrigin);
  end;

 BPos:=Pos('#MathFactR16',BOrigin);
 while BPos<>0 do
  begin
  BChange:=TRUE;
  Delete(BOrigin,BPos,Length('#MathFactR16'));
  BIndex:=2; BDummyS:=''; BMul:=1;
  while BIndex<18 do
   begin
   BMul:=BMul*BIndex;
   if BDummyS<>'' then BDummyS:=BDummyS+', ';
   BDummyS:=BDummyS+FloatToStr(1/BMul);
   inc(BIndex);
   end;
  Insert(BDummyS,BOrigin,BPos);
  BPos:=Pos('#MathFactR16',BOrigin);
  end;

 BPos:=Pos('#MathOneDivN16',BOrigin);
 while BPos<>0 do
  begin
  BChange:=TRUE;
  Delete(BOrigin,BPos,Length('#MathOneDivN16'));
  BIndex:=2; BDummyS:='';
  while BIndex<18 do
   begin
   BMul:=BMul*BIndex;
   if BDummyS<>'' then BDummyS:=BDummyS+', ';
   BDummyS:=BDummyS+FloatToStr(1/BIndex);
   inc(BIndex);
   end;
  Insert(BDummyS,BOrigin,BPos);
  BPos:=Pos('#MathOneDivN16',BOrigin);
  end;

 if BChange then AOrigin:=BOrigin;
 Result:=TRUE;
End;

end.

