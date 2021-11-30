unit AsmMcuMS_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, AsmHelperMS_sd,
  ParsPas_sd, ParsCpp_sd, ParsPy_sd, ParsRust_sd, ParsLlir_sd,
  BackEndMS_sd;

Const
  CRegCount = 56;

Type
  TAsmMcuMs = class(TAsmHelperMs)
  private
    FRegNames    : array [0..CRegCount-1] of string [4]; // al bl cl ...
    FRegNamesS   : string;

    Function GetXMem ( AParam : TAsmLineParam; Const ALabel : string; Out AAaaa : word; Out AConstSize : byte; Out AConst : Cardinal; Out ARef : string ) : boolean;
    Function GetRegIndex ( Const ARegName : string ) : Integer;
    Function GetRegIndex ( ALine : TAsmFlowLine; AParamIdx : Integer ) : Integer;
    Function GetPplRegIdx ( AParam : TAsmLineParam; AIncludeEip : boolean; Out ARegBitList : word; Out ARegCnt : Integer ) : boolean;

    Function CodeGenBM_BC_BA ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenARUC ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenARRS ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAliasAX ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenMAID_MARC ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenONL ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenIDX ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenPEX ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenAF ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenREM ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenPRCS ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenSys ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenNTRE ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenPPL ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenONS ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenFRUC ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenFRRS ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenBTR ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenBTM ( ALine : TAsmFlowLine ) : TCmdCompError;
    Function CodeGenARUI_ARUS ( ALine : TAsmFlowLine ) : TCmdCompError;
  protected
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure CodeGenCmd ( ALine : TAsmFlowLine ); Override;
  end;

  TPasMcuMs = class(TAsmMcuMs)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TCppMcuMs = class(TAsmMcuMs)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TPyMcuMs = class(TAsmMcuMs)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TRustMcuMs = class(TAsmMcuMs)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TLlirMcuMs = class(TAsmMcuMs)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

Const
  //              0  1  2  3  4  5  6  7   8  9 10 11 12 13 14 15  16 17 18 19 20 21 22 23  24 25 26 27 28 29 30 31  32  33  34  35  36  37  38  39   40 41 42 43 44 45 46 47  48 49 50 51 52 53 54 55
  CRegNamesMs = 'zl al bl cl dl el fl gl  mh ah bh ch dh eh fh gh  zx ax bx cx dx ex fx gx  mw aw bw cw dw ew fw gw  zwx awx bwx cwx dwx ewx fwx gwx esp ar br cr dr er fr gr  iq aq bq cq dq eq fq gq';

implementation

Uses
  ConComL;

Const
  CMaxConst     : array [0..3] of Int64 = ($FF, $FFFF, $FFFFFFFF, $FFFFFFFFFFFFFFFF);
  CMinConst     : array [0..3] of Int64 = (-$80, -$8000, -$80000000, -$8000000000000000);

  CRegSpx       = $8;
  CRegNil       = $0;

Constructor TAsmMcuMs.Create;
Var
  BRegNames     : string;
  BIndex        : Integer;
Begin
 Inherited;
 FRegNamesS:=CRegNamesMs;
 BRegNames:=CRegNamesMs;
 for BIndex:=0 to (CRegCount-1) do FRegNames[BIndex]:=ReadParamStr(BRegNames);
 FAddressParamType:='d';
End;

Destructor TAsmMcuMs.Destroy;
Begin
 Inherited;
End;

Constructor TPasMcuMs.Create;
Begin
 Inherited;
 FModule:=TModuleMs.Create;
 FParser:=TParsPas.Create;
End;

Destructor TPasMcuMs.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;

Constructor TCppMcuMs.Create;
Begin
 Inherited;
 FModule:=TModuleMs.Create;
 FParser:=TParsCpp.Create;
End;

Destructor TCppMcuMs.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;

Constructor TPyMcuMs.Create;
Begin
 Inherited;
 FModule:=TModuleMs.Create;
 FParser:=TParsPy.Create;
End;

Destructor TPyMcuMs.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;

Constructor TRustMcuMs.Create;
Begin
 Inherited;
 FModule:=TModuleMs.Create;
 FParser:=TParsRust.Create;
End;

Destructor TRustMcuMs.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;

Constructor TLlirMcuMs.Create;
Begin
 Inherited;
 FModule:=TModuleMs.Create;
 FParser:=TParsLlir.Create;
End;

Destructor TLlirMcuMs.Destroy;
Begin
 FParser.Free;
 FModule.Free;
 Inherited;
End;


{ *** TAsmMcuMs *** }

Procedure SplitCmd ( Const ACmd : string; Var ACmdBase, ACmdCond : string );
Var
  BLen,
  BPos  : Integer;
Begin
 BPos:=Pos('_',ACmd);
 if BPos=0 then
  begin
  ACmdBase:=ACmd;
  ACmdCond:='';
  end
 else
  begin
  BLen:=Length(ACmd);
  ACmdBase:=Copy(ACmd,1,BPos-1);
  ACmdCond:=Copy(ACmd,BPos+1,BLen-BPos);
  end;
End;

Function TAsmMcuMs.GetXMem ( AParam : TAsmLineParam; Const ALabel : string; Out AAaaa : word; Out AConstSize : byte; Out AConst : Cardinal; Out ARef : string ) : boolean;
Var
  BSumList      : TStringList;
  BLabel        : string;
  BRegIdxList   : array [0..2] of Integer;
  BRilIndex,                  // Index in BRegIdxList (attempt)
  BSumIndex,
  BRegIndex     : Integer;
  BConst,
  BConstA       : Int64;
  BRef          : string;
  BConstAssigned : boolean;
Begin
 Result:=FALSE;
 AAaaa:=0; AConstSize:=0; AConst:=0; ARef:='';

 BSumList:=TStringList.Create;
 repeat
 // Decompose BLabel
 BLabel:=ALabel;
 while BLabel<>'' do
  begin
  BSumList.Append(ReadTillC(BLabel,'+'));
  end;

 // ** Search if there is a register name in the SumList
 // Clear RegIdxList
 BRilIndex:=0;
 while BRilIndex<3 do
  begin
  BRegIdxList[BRilIndex]:=-1;
  inc(BRilIndex);
  end;
 // Make 2 attempts to find a register name in a list. If found, delete it from the list
 BRilIndex:=0;
 while BRilIndex<2 do
  begin
  BSumIndex:=0;
  while BSumIndex<BSumList.Count do
   begin
   BRegIndex:=GetRegIndex(BSumList.Strings[BSumIndex]);
   if BRegIndex>=0 then begin BRegIdxList[BRilIndex]:=BRegIndex; BSumList.Delete(BSumIndex); break; end;
   inc(BSumIndex);
   end;
  if BRegIdxList[BRilIndex]=-1 then break;
  inc(BRilIndex);
  end;
 if BRilIndex>=2 then begin AParam.AppendError('e','Too many registers are used for addressing [R:TAsmMcuMs.GetXMem]'); break; end;

 // Filter all constants from the list
 BConstAssigned:=FALSE;
 BConst:=0;
 BSumIndex:=0;
 while BSumIndex<BSumList.Count do
  begin
  BConstA:=0;
  if IsIntegerEqu(BSumList.Strings[BSumIndex],BConstA) then begin BConstAssigned:=TRUE; BConst:=BConst+BConstA; BSumList.Delete(BSumIndex); end
  else inc(BSumIndex);
  end;

 if BConst>$FFFFFFFF then AParam.AppendError('e','Constant is too wide for memory addressing [R:TAsmMcuMs.GetXMem]');
 if BConst<-$80000000 then AParam.AppendError('e','Constant is too wide for memory addressing [R:TAsmMcuMs.GetXMem]');

 if BSumList.Count>=2 then
  begin
  AParam.AppendError('e','Too many references or some identifiers are not recognized as a constant or EQU: [R:TAsmMcuMs.GetXMem]');
  BSumIndex:=0;
  while BSumIndex<BSumList.Count do
   begin
   ARef:=ARef+' '+BSumList.Strings[BSumIndex];
   inc(BSumIndex);
   end;
  end;

 // Check if there is an offset
 BRef:=''; if BSumList.Count=1 then BRef:=BSumList.Strings[0];
 if (BRef<>'') or BConstAssigned then
  begin
  if BRegIdxList[1]<>-1 then begin AParam.AppendError('e','If constant or memory reference is used, only 1 register can be taken to calculate an address [R:TAsmMcuMs.GetXMem]'); break; end;
  if BRegIdxList[0]=-1 then // Only constant is used
   begin
   if (BConstAssigned=FALSE) and (BRef<>'') then
   else if BConst>=32767 then AConstSize:=4
   else AConstSize:=2;
   ARef:=BRef; AConst:=BConst;
   AAaaa:=CRegNil;
   Result:=TRUE;
   break;
   end;
  if BRegIdxList[0] in [32..47] then AAaaa:=BRegIdxList[0]-32
  else begin AParam.AppendError('e','Only awx, bwx, cwx, dwx, ewx, fwx, gwx, esp, ar, br, cr, dr, er, fr, gr registers can be used with an offset to address memory [R:TAsmMcuMs.GetXMem]'); break; end;
  if (BConstAssigned=FALSE) and (BRef<>'') then
  else if BConst>=32767 then AConstSize:=4
  else AConstSize:=2;
  ARef:=BRef; AConst:=BConst;
  Result:=TRUE;
  break;
  end;

 // Check if there is only 1 register
 if BRegIdxList[1]=-1 then
  begin
  if BRegIdxList[0] in [32..47] then AAaaa:=BRegIdxList[0]-32
  else begin AParam.AppendError('e','Only awx, bwx, cwx, dwx, ewx, fwx, gwx, esp, ar, br, cr, dr, er, fr, gr registers can be used with an offset to address memory [R:TAsmMcuMs.GetXMem]'); break; end;
  AConstSize:=0;
  AConst:=0; ARef:='';
  Result:=TRUE;
  break;
  end;

 until TRUE;

 BSumList.Free;
End;

Function TAsmMcuMs.GetRegIndex ( Const ARegName : string ) : Integer;
Begin
 Result:=CRegCount-1;
 while Result>=0 do
  begin
  if FRegNames[Result]=ARegName then break;
  dec(Result);
  end;
End;

Function TAsmMcuMs.GetRegIndex ( ALine : TAsmFlowLine; AParamIdx : Integer ) : Integer;
Begin
 if Length(ALine.Params)<=AParamIdx then Result:=-1
 else Result:=GetRegIndex(ALine.Params[AParamIdx].Name);
End;

Procedure TAsmMcuMs.CodeGenCmd ( ALine : TAsmFlowLine );
Var
  BResult       : TCmdCompError;

Begin
 ALine.ClearDataBin; ALine.ClearRef;

 repeat
 BResult:=CodeGenBM_BC_BA(ALine);  if BResult<>cceCheckNext then break;
 BResult:=CodeGenARUC(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenARRS(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenAliasAX(ALine);   if BResult<>cceCheckNext then break;
 BResult:=CodeGenMAID_MARC(ALine); if BResult<>cceCheckNext then break;
 BResult:=CodeGenONL(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenIDX(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenPEX(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenAF(ALine);        if BResult<>cceCheckNext then break;
 BResult:=CodeGenREM(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenPRCS(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenSys(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenNTRE(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenPPL(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenONS(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenFRUC(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenFRRS(ALine);      if BResult<>cceCheckNext then break;
 BResult:=CodeGenBTR(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenBTM(ALine);       if BResult<>cceCheckNext then break;
 BResult:=CodeGenARUI_ARUS(ALine); if BResult<>cceCheckNext then break;

 ALine.AppendError('e','Invalid command code or command does not exist with this list of parameters "'+ALine.Exec+'" [R:TAsmMcuMs.CodeGenCmd]');
 until TRUE;
End;

Function TAsmMcuMs.CodeGenBM_BC_BA ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR         : Integer;
  BCmdW         : word;
  BWwR          : Word;
  BRrrr         : word;
  BConst        : Cardinal;
  BConstI       : Int64;
  BParamConst   : TAsmLineParam;
Begin
 Result:=cceCheckNext;
 BParamConst:=nil;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'mov cmp add sub');
 if BCmdIdx=-1 then break;

 if Length(ALine.Params)<>3 then break;
 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 if BWwR<>0 then break;
 BParamConst:=ALine.Params[2];
 if IsIntegerEqu(BParamConst.Name,BConstI)=FALSE then break;
 if (BConstI<-128) or (BConstI>255) then  begin ALine.AppendError('e',ALine.Params[2],'Constant is too wide for this register [R:TAsmMcuMs.CodeGenBM_BC_BA]'); Result:=cceError; break; end;
 BConst:=Cardinal(BConstI);
 // BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
 // BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
 // BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
 BCmdW:=0;
 case BCmdIdx of
   0: BCmdW:=$0400;
   1: BCmdW:=$4400;
   2: BCmdW:=$4600;
   3: begin BCmdW:=$4600; BConst:=((BConst xor $FF)+1) and $FF; end;
 end;
 BCmdW:=BCmdW or ((BConst and $1F) shl 4) or ((BConst and $E0) shl 6) or BRrrr;

 ALine.AppendDataBinW(BCmdW);
 Result:=cceCompiled;
 until TRUE;
End;

// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
Function TAsmMcuMs.CodeGenARUC ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR,
  BIdxU         : Integer;
  BCcc          : word;
  BWwR,
  BWwU          : Word;
  BIsNilR,
  BIsNilU       : boolean;
  BUuuu,
  BRrrr         : word;
  BLabel        : string;
  BConst        : Cardinal;
  BConstI       : Int64;
  BConstSize    : Integer;
  BParamConst   : TAsmLineParam;
Begin
 Result:=cceCheckNext;
 BLabel:='';
 BParamConst:=nil;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'add sub and or xor mul udiv sdiv');
 if BCmdIdx=-1 then break;
 BCcc:=Word(BCmdIdx);

 if Length(ALine.Params)=4 then
  begin
  BParamConst:=ALine.Params[3];
  BIdxR:=-1; BIdxU:=-1;
  BIsNilR:=ALine.Params[1].Name='nil';
  if BIsNilR=FALSE then
   begin
   BIdxR:=GetRegIndex(ALine.Params[1].Name);
   if BIdxR=-1 then break;
   end;
  BIsNilU:=ALine.Params[2].Name='nil';
  if BIsNilU=FALSE then
   begin
   BIdxU:=GetRegIndex(ALine.Params[2].Name);
   if BIdxU=-1 then break;
   end;
  if BIsNilR and BIsNilU then break;
  if BIsNilR then begin BWwR:=BIdxU shr 4; BRrrr:=$0; end
  else begin BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F; end;
  if BIsNilU then begin BWwU:=BIdxR shr 4; BUuuu:=$0; end
  else begin BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F; end;
  if BWwR<>BWwU then break; // perhaps it is ARUI/ARUS begin ALine.AppendError('e',ALine.Params[2],'Registers must have same size [R:TAsmMcuMs.CodeGenARUC]'); Result:=cceError; break; end;
  end
 else if Length(ALine.Params)=3 then
  begin
  BParamConst:=ALine.Params[2];
  BIdxR:=GetRegIndex(ALine.Params[1].Name);
  if BIdxR=-1 then break;
  BIdxU:=BIdxR;
  BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
  BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F;
  end
 else break;

 if IsPointer(BParamConst.Name,BLabel) then break; // This is not our problem anymore, just verify other cases
 if GetRegIndex(BParamConst.Name)>=0 then break;

 BConstSize:=0;
 BConst:=0;
 if IsIntegerEqu(BParamConst.Name,BConstI) then
  begin
  if BConstI>CMaxConst[BWwR] then begin ALine.AppendError('e',BParamConst,'Constant is too wide for this column [R:TAsmMcuMs.CodeGenAC]'); Result:=cceError; break; end;
  if BConstI<CMinConst[BWwR] then begin ALine.AppendError('e',BParamConst,'Constant is too wide for this column [R:TAsmMcuMs.CodeGenAC]'); Result:=cceError; break; end;
  BConst:=Cardinal(BConstI);
  if BRrrr=CRegSpx then
   begin
   if BCmdIdx=0 then ALine.StackDelta:=-BConst;
   if BCmdIdx=1 then ALine.StackDelta:=BConst;
   end;
  if ((BConstI>32767) or (BConstI<-32768)) and (BWwR>$1) then BConstSize:=4 else BConstSize:=2;
  //if (BConstSize=2) and ((BConst and $8000)<>0) and (BWwR>$1) then BConstSize:=4 else BConstSize:=2;
  end;

 // ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
 if BConstSize=0 then
  begin
  ALine.AppendDataBinW($0000 or (BWwR shl 8) or (BCcc shl 11) or (BUuuu shl 4) or BRrrr);
  ALine.AppendRef(BParamConst,'r',2); ALine.AppendDataBinX(0,2);
  Result:=cceCompiled;
  break;
  end;

 // Custom case IDX
 if ((BCcc=0) or (BCcc=1)) and (BConst>0) and (BConst<=8) and (BIdxU=BIdxR) then
  begin
  // IDX    |10111   ||     cww|0iiirrrr|                 | inc dec
  ALine.AppendDataBinW($B800 or (BWwR shl 8) or (BCcc shl 10) or ((BConst-1) shl 4) or BRrrr);
  Result:=cceCompiled;
  break;
  end;

 if BConstSize=2 then
  begin
  ALine.AppendDataBinW($0000 or (BWwR shl 8) or (BCcc shl 11) or (BUuuu shl 4) or BRrrr);
  ALine.AppendDataBinW(BConst);
  end
 else
  begin
  ALine.AppendDataBinW($0400 or (BWwR shl 8) or (BCcc shl 11) or (BUuuu shl 4) or BRrrr);
  ALine.AppendDataBinD(BConst);
  end;

 Result:=cceCompiled;
 until TRUE;
End;

Procedure FixMcdRrrrUuuu ( ACmdIdx : Integer; ARrrr : word; Out ARrrrU, ARrrrR : word );
Begin
 ARrrrU:=ARrrr; ARrrrR:=ARrrr;
 if ACmdIdx=2 then ARrrrU:=CRegNil // mov
 else if (ACmdIdx=3) or (ACmdIdx=6) then ARrrrR:=CRegNil;
End;

Function McdCmdAToCccc ( ACmdIdx : Integer ) : word;
Begin                 // add sub mov cmp and or test xor
 Result:=0;           // add sub  or sub and or and  xor
 case ACmdIdx of      // 0   1    3  1   2   3  2    4
   0: Result:=0;
   1: Result:=1;
   2: Result:=3;
   3: Result:=1;
   4: Result:=2;
   5: Result:=3;
   6: Result:=2;
   7: Result:=4;
 end;
End;

// ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
Function TAsmMcuMs.CodeGenARRS ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR,
  BIdxU         : Integer;
  BWwR,
  BWwU          : Word;
  BUuuu,
  BRrrr         : word;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'add sub mov cmp and or test xor');
 if BCmdIdx<0 then break;
 if Length(ALine.Params)<>3 then break;

 BIdxR:=GetRegIndex(ALine.Params[1].Name); if BIdxR<0 then break;
 BIdxU:=GetRegIndex(ALine.Params[2].Name); if BIdxU<0 then break;

 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F;
 if BWwR<>BWwU then break;

 if BRrrr=CRegSpx then ALine.AppendError('w',ALine.Params[2],'With ESP use constant, otherwise it is not possible to calculate stack use [R:TAsmMcuMs.CodeGenARS]');

 // ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor r/r of same size (Unused codes of ARUC, because FPU cannot use )
 ALine.AppendDataBinW($4000 or (BWwR shl 8) or (BCmdIdx shl 11) or (BUuuu shl 4) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuMs.CodeGenAliasAX ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR,
  BIdxU         : Integer;
  BCccc         : word;
  BWwR,
  BWwU          : Word;
  BUuuu,
  BRrrr         : word;
  BLabel        : string;
  BConst        : Cardinal;
  BConstI       : Int64;
  BConstFD      : Double;
  BConstFS      : Single;
  BConstFI      : Cardinal absolute BConstFS;
  BConstSize    : Integer;
  BRrrrU,
  BRrrrR        : word;
  BExt          : char;
  BEe           : word;
Begin
 Result:=cceCheckNext;
 BLabel:='';

 repeat
 BExt:='.';
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'add sub mov cmp and or test xor');
 if BCmdIdx>=0 then
 else
  begin
  BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'addzx subzx movzx cmpzx andzx orzx testzx xorzx');
  if BCmdIdx>0 then BExt:='z'
  else
   begin
   BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'addsx subsx movsx cmpsx andsx orsx testsx xorsx');
   if BCmdIdx>0 then BExt:='s'
   else break;
   end;
  end;
 BCccc:=McdCmdAToCccc(BCmdIdx);

 if Length(ALine.Params)<>3 then break;

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 BIdxU:=GetRegIndex(ALine.Params[2].Name);

 if BIdxR<0 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 FixMcdRrrrUuuu(BCmdIdx,BRrrr,BRrrrU,BRrrrR);

 if BIdxU>=0 then
  begin
  BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F;
  if BRrrr=CRegSpx then ALine.AppendError('w',ALine.Params[2],'With ESP use constant, otherwise it is not possible to calculate stack use [R:TAsmMcuMs.CodeGenAliasAX]');
  BEe:=$0;
  if BExt='.' then
   begin
   if BWwR<>BWwU then begin ALine.AppendError('e',ALine.Params[2],'Registers have different size. Use "'+ALine.Params[0].Name+'zx" or "'+ALine.Params[0].Name+'sx" instead [R:TAsmMcuMs.CodeGenAliasAX]'); Result:=cceError; break; end;
   if BCmdIdx in [0, 1, 3] then BEe:=$1;
   end
  else if BExt='s' then BEe:=$1;
  // ARUS   |1111110 ||       e|uuuurrrr|ccccwwww|00wwssss| addex subex andex orex xorex mulex udivex sdivex fadd fsub fmul fdiv shl shr asr rol | (r = u cmd s)
  ALine.AppendDataBinW($FE00 or (BEe shl 8) or (BRrrrU shl 4) or BRrrrR);
  ALine.AppendDataBinW((BCccc shl 12) or (BWwR shl 10) or (BWwR shl 8) or (BWwU shl 4) or BUuuu);
  Result:=cceCompiled;
  break;
  end;

 if BIdxR>=0 then // DST = Reg
  begin
  BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
  if IsPointer(ALine.Params[2].Name,BLabel) then break; // This is not our problem anymore, just verify other cases
  BConst:=0; BConstSize:=0;
  if IsIntegerEqu(ALine.Params[2].Name,BConstI) then
   begin
   if BConstI>CMaxConst[BWwR] then begin ALine.AppendError('e',ALine.Params[2],'Constant is too wide for this column [R:TAsmMcuMs.CodeGenAliasAX]'); Result:=cceError; break; end;
   if BConstI<CMinConst[BWwR] then begin ALine.AppendError('e',ALine.Params[2],'Constant is too wide for this column [R:TAsmMcuMs.CodeGenAliasAX]'); Result:=cceError; break; end;
   BConst:=Cardinal(BConstI);
   if BRrrr=CRegSpx then
    begin
    if BCmdIdx=0 then ALine.StackDelta:=-BConst;
    if BCmdIdx=1 then ALine.StackDelta:=BConst;
    end;
   if ((BConstI>32767) or (BConstI<-32768)) and (BWwR>$1) then BConstSize:=4 else BConstSize:=2;
   if (BWwR=0) and ((BConst and $80)<>0) then BConst:=BConst or $FF00;
   end
  else if IsFloatEqu(ALine.Params[2].Name,BConstFD) then
   begin
   if BRrrr=CRegSpx then begin ALine.AppendError('e',ALine.Params[2],'Floating point constants are not applicable for ESP [R:TAsmMcuMs.CodeGenAliasAX]'); Result:=cceError; break; end;
   if BWwR<>$2 then begin ALine.AppendError('e',ALine.Params[2],'Floating point constants can only be used with 32-bit columns [R:TAsmMcuMs.CodeGenAliasAX]'); Result:=cceError; break; end;
   BConstFS:=BConstFD;
   BConst:=BConstFI;
   BConstSize:=4;
   end;

  // ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
  if BConstSize=0 then
   begin
   ALine.AppendDataBinW($0000 or (BWwR shl 8) or (BCccc shl 11) or (BRrrrU shl 4) or BRrrrR);
   ALine.AppendRef(ALine.Params[2],'r',2); ALine.AppendDataBinX(0,2);
   Result:=cceCompiled;
   break;
   end;

  // ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
  if BConstSize=2 then
   begin
   ALine.AppendDataBinW($0000 or (BWwR shl 8) or (BCccc shl 11) or (BRrrrU shl 4) or BRrrrR);
   ALine.AppendDataBinW(BConst);
   end
  else
   begin
   ALine.AppendDataBinW($0400 or (BWwR shl 8) or (BCccc shl 11) or (BRrrrU shl 4) or BRrrrR);
   ALine.AppendDataBinD(BConst);
   end;
  Result:=cceCompiled;
  break;
  end;
 until TRUE;
End;

// MAID   |100     ||   cccww|aaaarrrr|                 | Mem rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg]
// MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
Function TAsmMcuMs.CodeGenMAID_MARC ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BRegIdx       : Integer;
  BAaaa         : word;
  BLabel        : string;
  BConstSize    : byte;
  BRef          : string;
  BCmd          : Word;
  BWw,
  BRrrr         : Word;
  BConst        : Cardinal;
  BPointer      : TAsmLineParam;
  BWrRd         : word;
  BIncDec       : Integer;
  BPos          : Integer;
  BMemIdx       : Integer;
  BCcc          : word;
Begin
 Result:=cceCheckNext;
 BLabel:=''; BRef:=''; BConst:=0; BConstSize:=0;
 BAaaa:=0;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'pop push');
 if BCmdIdx<>-1 then
  begin
  if Length(ALine.Params)<>2 then break;
  BRegIdx:=GetRegIndex(ALine.Params[1].Name);
  if BRegIdx=-1 then break;
  if BRegIdx in [32..47] then BRrrr:=BRegIdx-32
  else begin ALine.AppendError('e',ALine.Params[1],'Only 32-bit registers can be used [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end;
  BWrRd:=BCmdIdx;
  if BWrRd=0 then begin BCcc:=$4; ALine.StackDelta:=-4; end
  else begin BCcc:=$7; ALine.StackDelta:=4; end;
  // MAID   |100     ||   cccww|aaaarrrr|                 | Mem rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg]
  ALine.AppendDataBinW($8280 or (BCcc shl 10) or BRrrr);
  Result:=cceCompiled;
  break;
  end;

 // Common case
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'mov');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>3 then break;

 BIncDec:=-1;

 BPointer:=nil;
 BRegIdx:=GetRegIndex(ALine.Params[1].Name);
 if BRegIdx>=0 then // Potential read
  begin
  BWrRd:=0;
  BPointer:=ALine.Params[2];
  if IsPointer(BPointer.Name,BLabel)=FALSE then break;
  end
 else
  begin
  BWrRd:=1;
  BPointer:=ALine.Params[1];
  if IsPointer(BPointer.Name,BLabel)=FALSE then break;
  BRegIdx:=GetRegIndex(ALine.Params[2].Name);
  if BRegIdx=-1 then break;
  end;

 BPos:=Pos('++',BLabel);
 if BPos=0 then
  begin
  BPos:=Pos('--',BLabel);
  if BPos=0 then // Normal memory addr
   begin
   if GetXMem(BPointer,BLabel,BAaaa,BConstSize,BConst,BRef)=FALSE then begin Result:=cceError; break; end;
   end
  else if BPos<>1 then begin ALine.AppendError('e',BPointer,'Post-decrement is not allowed (-- must go before the register name, not after) [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end
  else BIncDec:=1;
  end
 else if BPos=1 then
  begin
  ALine.AppendError('e',BPointer,'Pre-increment is not allowed (++ must go after the register name, not before) [R:TAsmMcuMs.CodeGenMio]');
  Result:=cceError;
  break;
  end
 else
  begin
  BIncDec:=0;
  end;

 if BIncDec<>-1 then // Continue with ++/--
  begin
  Delete(BLabel,BPos,2);
  DelFirstSpace(BLabel); DelLastSpace(BLabel);
  if BLabel='' then begin ALine.AppendError('e',BPointer,'Invalid memory address [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end;
  BMemIdx:=GetRegIndex(BLabel);
  if BMemIdx=-1 then begin ALine.AppendError('e',BPointer,'Invalid memory address: %p [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end;
  if BMemIdx in [32..47] then BAaaa:=BMemIdx-32
  else begin ALine.AppendError('e',BPointer,'Invalid memory address: %p [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end;
  end;

 BWw:=BRegIdx shr 4; BRrrr:=BRegIdx and $F;

 BCcc:=BWrRd shl 1;
 if BIncDec<>-1 then
  begin
  BCcc:=BCcc or $4 or Byte(BIncDec);
  if BAaaa=CRegSpx then
   begin
   if BWw<>$2 then begin ALine.AppendError('e',BPointer,'Only 32-bit register is allowed with stack [R:TAsmMcuMs.CodeGenMio]'); Result:=cceError; break; end;
   if BWrRd=0 then ALine.StackDelta:=-4
   else ALine.StackDelta:=4;
   end;
  end;

 // MAID   |100     ||   cccww|aaaarrrr|                 | Mem rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg]
 if (BConstSize=0) and (BRef='') then
  begin
  ALine.AppendDataBinW($8000 or (BWw shl 8) or (BCcc shl 10) or (BAaaa shl 4) or BRrrr);
  Result:=cceCompiled;
  break;
  end;

 // MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
 BCmd:=$A000;
 if (BRef<>'') and (BConst=0) then // Try to assign 16-bit ref
 else if BConstSize>2 then BCmd:=BCmd or $0400;
 ALine.AppendDataBinW(BCmd or (BWw shl 8) or (BWrRd shl 11) or (BAaaa shl 4) or BRrrr);
 if BIncDec<>-1 then begin ALine.AppendError('e',BPointer,'Internal error [R:TAsmMcuMs.CodeGenMAID_MARC]'); Result:=cceError; break; end;
 if BConstSize=0 then BConstSize:=2;
 if BRef<>'' then begin ALine.AppendRef(BPointer,BRef,'r',2,BConst); ALine.AppendDataBinX(0,BConstSize); end
 else ALine.AppendDataBinX(BConst,BConstSize);

 Result:=cceCompiled;
 until TRUE;
End;

Const
  CBraListA = 'bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv';
  CBraListB = 'bra bbe bb bae be bne ba bany bg bge bs bse bn bv bnn bnv';
  CSwtListA = 'swt swt_be swt_c swt_nc swt_z swt_nz swt_a swt_any swt_g swt_ge swt_s swt_se swt_n swt_v swt_nn swt_nv';
  CSwtListB = 'swt swt_be swt_c swt_ae swt_e swt_ne swt_a swt_any swt_g swt_ge swt_s swt_se swt_n swt_v swt_nn swt_nv';

// ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
Function TAsmMcuMs.CodeGenONL ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIndex     : Integer;
  BAaaa         : word;
  BConstSize    : byte;
  BRef          : string;
  BCmd          : Word;
  BConst        : Cardinal;
  BCc,
  BFfff         : word;
Begin
 Result:=cceCheckNext;

 BConst:=0;
 BCc:=0; BFfff:=0;
 repeat
 if Length(ALine.Params)<>3 then break; // Only analyse branches with registers. Otherwise only short branches are assembled. Long branches are created from short ones by linker when necessary

 BCmdIndex:=-1;
 repeat
 if LowerCase(ALine.Params[0].Name)='call' then begin BCmdIndex:=0; break; end; // Call is same as bra but more readable
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CBraListA); if BCmdIndex>=0 then break;
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CBraListB);
 until TRUE;
 if BCmdIndex=-1 then break;
 BFfff:=Word(BCmdIndex);

 ALine.IsJxx:=TRUE;
 if BFfff in [0, 8] then ALine.IsJmp:=TRUE;

 if GetXMem(ALine.Params[1],ALine.Params[1].Name,BAaaa,BConstSize,BConst,BRef)=FALSE then
  begin
  ALine.AppendError('e',ALine.Params[1],'Invalid destination [R:TAsmMcuMs.CodeGenJcsW]');
  Result:=cceError;
  break;
  end;

 // ONL    |10110   ||     4cc|uuuuffff|#2/4             | jmp call 2xRFU  (either REG or # but not both, call REG is not possible)
 BCmd:=$B000;
 if BRef<>'' then
  begin
  ALine.AppendDataBinW(BCmd or $0400 or (BCc shl 8) or (BAaaa shl 4) or BFfff);
  ALine.AppendDataBinW(0);
  ALine.AppendRef(ALine.Params[1],BRef,'l',2,0);
  ALine.SetDstLabel(BRef);
  Result:=cceCompiled;
  break;
  end;
 if BConstSize>2 then BCmd:=BCmd or $0400;
 ALine.AppendDataBinW(BCmd or (BCc shl 8) or (BAaaa shl 4) or BFfff);
 ALine.AppendDataBinX(BConst,BConstSize);
 Result:=cceCompiled;
 until TRUE;
End;

// IDX    |10111   ||     cww|0iiirrrr|                 | inc dec
Function TAsmMcuMs.CodeGenIDX ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BIdxR         : Integer;
  BWwR          : Word;
  BRrrr         : Word;
  BConstI       : Int64;
  BConst        : Cardinal;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'inc dec');
 if BCmdIdx=-1 then break;

 BConst:=0;
 if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Register name required [R:TAsmMcuMs.CodeGenIDE]'); Result:=cceError; break; end;
 if Length(ALine.Params)>3 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenIDE]'); Result:=cceError; break; end;
 if Length(ALine.Params)=3 then
  begin
  if IsIntegerEqu(ALine.Params[2].Name,BConstI)=FALSE then begin ALine.AppendError('e',ALine.Params[2],'Invalid integer constant [R:TAsmMcuMs.CodeGenIDE]'); Result:=cceError; break; end;
  if (BConstI<1) or (BConstI>8) then begin ALine.AppendError('e',ALine.Params[2],'Constant out of range [R:TAsmMcuMs.CodeGenIDE]'); Result:=cceError; break; end;
  BConst:=Cardinal(BConstI)-1;
  end;

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;

 // IDX    |10111   ||     cww|0iiirrrr|                 | inc dec
 ALine.AppendDataBinW($B800 or (Word(BCmdIdx) shl 10) or (BWwR shl 8) or (BConst shl 4) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

// PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
Function TAsmMcuMs.CodeGenPEX ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BIdxR         : Integer;
  BWwR          : Word;
  BRrrr         : Word;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'pushzx pushsx');
 if BCmdIdx=-1 then break;

 if Length(ALine.Params)>2 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenPEX]'); Result:=cceError; break; end;
 if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Register name required [R:TAsmMcuMs.CodeGenPEX]'); Result:=cceError; break; end;

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;

 ALine.StackDelta:=4;

 ALine.AppendDataBinW($B880 or (Word(BCmdIdx) shl 10) or (BWwR shl 8) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

// AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
Function TAsmMcuMs.CodeGenAF ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BIdxR         : Integer;
  BWwR          : Word;
  BRrrr         : Word;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'itf trunc round');
 if BCmdIdx=-1 then break;

 if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Register name required [R:TAsmMcuMs.CodeGenAF]'); Result:=cceError; break; end;
 if Length(ALine.Params)>2 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenAF]'); Result:=cceError; break; end;

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 if BWwR<>2 then begin ALine.AppendError('e',ALine.Params[0],'Register is not appicable to this command [R:TAsmMcuMs.CodeGenAF]'); Result:=cceError; break; end;

 ALine.AppendDataBinW($B890 or (Word(BCmdIdx) shl 8) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

// REM    |10111   ||     cww|1010rrrr|#2/4             | urem srem (r := r rem Const)
Function TAsmMcuMs.CodeGenREM ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BIdxR         : Integer;
  BWwR          : Word;
  BRrrr         : Word;
  BConstI       : Int64;
  BConst        : Cardinal;
  BConstSize    : byte;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'urem srem');
 if BCmdIdx=-1 then break;

 BConst:=0;
 if Length(ALine.Params)<>3 then break;

 if IsIntegerEqu(ALine.Params[2].Name,BConstI)=FALSE then break;
 BConst:=Cardinal(BConstI);

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 if (BWwR and $2)<>0 then BConstSize:=4 else BConstSize:=2;

 // REM    |10111   ||     cww|1010rrrr|#2/4             | urem srem (r := r rem Const)
 ALine.AppendDataBinW($B8A0 or (Word(BCmdIdx) shl 10) or (BWwR shl 8) or BRrrr);
 ALine.AppendDataBinX(BConst,BConstSize);
 Result:=cceCompiled;
 until TRUE;
End;

// PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
Function TAsmMcuMs.CodeGenPRCS ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BConstI       : Int64;
  BConst        : Cardinal;
  BCc           : word;
  BConstSize    : Cardinal;
  BRef          : string;
  BCmd          : word;
  BConstFD      : Double;
  BConstFS      : Single;
  BConstFI      : Cardinal absolute BConstFS;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'# push');
 if BCmdIdx=-1 then break;

 BCc:=Word(BCmdIdx);

 BConstSize:=0; BConst:=0; BRef:='';
 if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Register name required [R:TAsmMcuMs.CodeGenPRCS]'); Result:=cceError; break; end;
 if Length(ALine.Params)>2 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenPRCS]'); Result:=cceError; break; end;

 if BCmdIdx=1 then ALine.StackDelta:=4;

 if IsIntegerEqu(ALine.Params[1].Name,BConstI) then
  begin
  if (BConstI>32767) or (BConstI<-32768) then BConstSize:=4 else BConstSize:=2;
  BConst:=Cardinal(BConstI);
  end
 else if IsFloatEqu(ALine.Params[1].Name,BConstFD) then
  begin
  BConstFS:=BConstFD;
  BConstSize:=4;
  BConst:=BConstFI;
  end
 else BRef:=ALine.Params[1].Name;

 BCmd:=$B8B0;
 if BRef<>'' then
  begin
  ALine.AppendDataBinW(BCmd or (BCc shl 8));
  ALine.AppendRef(ALine.Params[1],BRef,'r',2,0);  ALine.AppendDataBinW(0);
  ALine.SetDstLabel(BRef);
  Result:=cceCompiled;
  break;
  end;

 if BConstSize>2 then BCmd:=BCmd or $0400;
 ALine.AppendDataBinW(BCmd or (BCc shl 8));
 ALine.AppendDataBinX(BConst,BConstSize);
 Result:=cceCompiled;
 until TRUE;
End;

// Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
Function TAsmMcuMs.CodeGenSys ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'info siend silock siunlock siconf # # #');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>1 then break;

 if BCmdIdx=1 then ALine.IsIpLoad:=TRUE;

 ALine.AppendDataBinW($B8B1 or (Word(BCmdIdx) shl 8));
 Result:=cceCompiled;
 until TRUE;
End;

// NTRE   |10111   ||     ccc|10110010|                 | nop trap # # # # # #
Function TAsmMcuMs.CodeGenNTRE ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'nop trap test_end # # # getfl setfl');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>1 then break;

 ALine.AppendDataBinW($B8B2 or (Word(BCmdIdx) shl 8));
 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuMs.GetPplRegIdx ( AParam : TAsmLineParam; AIncludeEip : boolean; Out ARegBitList : word; Out ARegCnt : Integer ) : boolean;
Var
  BRegList  : string;
  BRegIdx   : Integer;
  BRegName  : string;
  BMask     : word;
Begin
 Result:=FALSE;
 ARegBitList:=0; ARegCnt:=0;
 BRegList:=AParam.Name;
 repeat
 BRegName:=ReadTillC(BRegList,'|');
 if BRegName='' then begin Result:=TRUE; break; end;
 if BRegName='#' then
 else
  begin
  BRegIdx:=GetStrIndex(LowerCase(BRegName),'eip # awx ar bwx br cwx cr dwx dr ewx er fwx fr gwx gr');
  if BRegIdx=-1 then begin AParam.AppendError('e','Invalid register name '+BRegName+' [R:TAsmMcuMs.GetPplRegIdx]'); break; end;
  BMask:=1 shl BRegIdx;
  if (ARegBitList and BMask)<>0 then AParam.AppendError('w','Register '+BRegName+' is already in the list [R:TAsmMcuMs.GetPplRegIdx]');
  ARegBitList:=ARegBitList or BMask;
  end;
 inc(ARegCnt);
 until FALSE;
 if AIncludeEip and ((ARegBitList and $0001)=0) then
  begin
  ARegBitList:=ARegBitList or $0001;
  inc(ARegCnt);
  end;
End;

// PPL    |10111   ||     4cc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU (4 is set by natural way)
Function TAsmMcuMs.CodeGenPPL ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BRegCnt       : Integer;
  BRegBitList   : word;
  BLocalVarSize,
  BStackClrSize : Int64;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(LowerCase(ALine.Params[0].Name),'popl pushl # # leave enter # #');
 if BCmdIdx=-1 then break;
 Result:=cceError;
 BRegCnt:=0;

 case BCmdIdx of
   0,        // popl
   1: begin  // pushl
      if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Register name list required [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if Length(ALine.Params)>2 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if GetPplRegIdx(ALine.Params[1],FALSE,BRegBitList,BRegCnt)=FALSE then break;
      ALine.AppendDataBinW($B8B3 or (Word(BCmdIdx) shl 8));
      ALine.AppendDataBinW(BRegBitList);
      if (BRegBitList and $1)<>0 then
       begin
       if BCmdIdx=0 then ALine.IsIpLoad:=TRUE
       else ALine.IsIpSave:=TRUE;
       end;
      if BCmdIdx=0 then ALine.StackDelta:=-4*BRegCnt
      else ALine.StackDelta:=4*BRegCnt;
      end;
   4: begin  // leave
      if Length(ALine.Params)<4 then begin ALine.AppendError('e',ALine.Params[0],'Not enough parameters (register list, local var size and stack clearance are expected) [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if Length(ALine.Params)>4 then begin ALine.AppendError('e',ALine.Params[4],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if GetPplRegIdx(ALine.Params[1],TRUE,BRegBitList,BRegCnt)=FALSE then break;
      if IsIntegerEqu(ALine.Params[2].Name,BLocalVarSize)=FALSE then begin ALine.AppendError('e',ALine.Params[2],'Cannot convert local var size to integer [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if (BLocalVarSize and $3)<>0 then begin ALine.AppendError('e',ALine.Params[2],'Local var size must be aligned by 4 [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if BLocalVarSize>$3FF then begin ALine.AppendError('e',ALine.Params[2],'Local var size is too big [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if IsIntegerEqu(ALine.Params[3].Name,BStackClrSize)=FALSE then begin ALine.AppendError('e',ALine.Params[3],'Cannot convert stack clearance to integer [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if (BStackClrSize and $3)<>0 then begin ALine.AppendError('e',ALine.Params[3],'Stack clearance must be aligned by 4 [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if BStackClrSize>$3FF then begin ALine.AppendError('e',ALine.Params[3],'Stack clearance is too big [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if (BLocalVarSize=0) and (BStackClrSize=0) then // replace by popl
       begin
       BCmdIdx:=BCmdIdx and $3;
       ALine.AppendDataBinW($B8B3 or (Word(BCmdIdx) shl 8));
       ALine.AppendDataBinW(BRegBitList or $0001);
       end
      else
       begin
       ALine.AppendDataBinW($B8B3 or (Word(BCmdIdx) shl 8));
       ALine.AppendDataBinW(BRegBitList or $0001);
       ALine.AppendDataBinW((Word(BStackClrSize) shl 6) or (Word(BLocalVarSize) shr 2));
       end;
      ALine.StackDelta:=-4*BRegCnt-BStackClrSize-BLocalVarSize;
      ALine.IsIpLoad:=TRUE;
      end;
   5: begin  // enter
      if Length(ALine.Params)<3 then begin ALine.AppendError('e',ALine.Params[0],'Not enough parameters (register list, local var size and stack clearance are expected) [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if Length(ALine.Params)>3 then begin ALine.AppendError('e',ALine.Params[3],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if GetPplRegIdx(ALine.Params[1],TRUE,BRegBitList,BRegCnt)=FALSE then break;
      if IsIntegerEqu(ALine.Params[2].Name,BLocalVarSize)=FALSE then begin ALine.AppendError('e',ALine.Params[2],'Cannot convert local var size to integer [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if (BLocalVarSize and $3)<>0 then begin ALine.AppendError('e',ALine.Params[2],'Local var size must be aligned by 4 [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if BLocalVarSize>$3FF then begin ALine.AppendError('e',ALine.Params[2],'Local var size is too big [R:TAsmMcuMs.CodeGenPPL]'); break; end;
      if BLocalVarSize=0 then // replace by pushl
       begin
       BCmdIdx:=BCmdIdx and $3;
       ALine.AppendDataBinW($B8B3 or (Word(BCmdIdx) shl 8));
       ALine.AppendDataBinW(BRegBitList or $0001);
       end
      else
       begin
       ALine.AppendDataBinW($B8B3 or (Word(BCmdIdx) shl 8));
       ALine.AppendDataBinW(BRegBitList or $0001);
       ALine.AppendDataBinW(Word(BLocalVarSize) shr 2);
       end;
      ALine.StackDelta:=4*BRegCnt+BLocalVarSize;
      ALine.IsIpSave:=TRUE;
      end;
 else begin
      ALine.AppendError('e','Internal error [R:TAsmMcuMs.CodeGenPPL]');
      break;
      end;
 end;

 Result:=cceCompiled;
 until TRUE;
End;

// J      |110     ||   diiii|iiiiffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
// ONS    |110     ||   djjjj|jjjjffff|                 | sjmp jbe jc jnc jz jnz ja RFU jg jge js jse jn jv jnn jnv
Function TAsmMcuMs.CodeGenONS ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIndex     : Integer;
  BFfff         : Word;
  BIsSwt        : boolean;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIndex:=-1;
 BIsSwt:=FALSE;
 repeat
 if LowerCase(ALine.Params[0].Name)='call' then begin BCmdIndex:=0; break; end; // Call is same as bra but more readable
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CBraListA);
 if BCmdIndex>=0 then break;
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CBraListB);
 if BCmdIndex>=0 then break;
 BIsSwt:=TRUE;
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CSwtListA);
 if BCmdIndex>=0 then break;
 BCmdIndex:=GetStrIndex(ALine.Params[0].Name,CSwtListB);
 if BCmdIndex>=0 then break;
 until TRUE;
 if BCmdIndex=-1 then break;
 BFfff:=Word(BCmdIndex);
 Result:=cceError;
 if Length(ALine.Params)>2 then begin ALine.AppendError('e',ALine.Params[2],'Extra parameter in line: %p [R:TAsmMcuMs.CodeGenJ]'); break; end
 else if Length(ALine.Params)<2 then begin ALine.AppendError('e',ALine.Params[0],'Jmp label required [R:TAsmMcuMs.CodeGenJ]'); break; end;
 if BIsSwt=FALSE then
  begin
  ALine.AppendRef(ALine.Params[1],'j',0);
  ALine.AppendDataBinW($C000 or BFfff);
  end
 else
  begin
  ALine.AppendDataBinW($B000 or ($1 shl 8) or BFfff);
  ALine.AppendDataBinW(0);
  ALine.AppendRef(ALine.Params[1],'l',2);
  end;
 if BCmdIndex=0 then ALine.IsJmp:=TRUE else ALine.IsJxx:=TRUE;
 ALine.SetDstLabel(ALine.Params[1].Name);
 Result:=cceCompiled;
 until TRUE;
End;

// FRUC   |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
Function TAsmMcuMs.CodeGenFRUC ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR,
  BIdxU         : Integer;
  BCcc          : word;
  BWwR,
  BWwU          : Word;
  BIsNilR,
  BIsNilU       : boolean;
  BUuuu,
  BRrrr         : word;
  BLabel        : string;
  BConst        : Cardinal;
  BConstI       : Int64;
  BConstFD      : Double;
  BConstFS      : Single;
  BConstFI      : Cardinal absolute BConstFS;
  BParamConst   : TAsmLineParam;
Begin
 Result:=cceCheckNext;
 BLabel:='';
 BParamConst:=nil;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'fadd fsub fmul fdiv');
 if BCmdIdx=-1 then break;
 BCcc:=Word(BCmdIdx);

 if Length(ALine.Params)=4 then
  begin
  BParamConst:=ALine.Params[3];
  BIdxR:=-1; BIdxU:=-1;
  BIsNilR:=ALine.Params[1].Name='nil';
  if BIsNilR=FALSE then
   begin
   BIdxR:=GetRegIndex(ALine.Params[1].Name);
   if BIdxR=-1 then break;
   end;
  BIsNilU:=ALine.Params[2].Name='nil';
  if BIsNilU=FALSE then
   begin
   BIdxU:=GetRegIndex(ALine.Params[2].Name);
   if BIdxU=-1 then break;
   end;
  if BIsNilR and BIsNilU then break;
  if BIsNilR then begin BWwR:=BIdxU shr 4; BRrrr:=$7; end
  else begin BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F; end;
  if BIsNilU then begin BWwU:=BIdxR shr 4; BUuuu:=$7; end
  else begin BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F; end;
  if BWwR<>BWwU then begin ALine.AppendError('e',ALine.Params[2],'Registers must have same size [R:TAsmMcuMs.CodeGenFRUC]'); Result:=cceError; break; end;
  end
 else if Length(ALine.Params)=3 then
  begin
  BParamConst:=ALine.Params[2];
  BIdxR:=GetRegIndex(ALine.Params[1].Name);
  if BIdxR=-1 then break;
  BIdxU:=BIdxR;
  BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
  BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F;
  end
 else break;

 if BWwR<>2 then begin ALine.AppendError('e',ALine.Params[1],'Only 32-bit registers are allowed [R:TAsmMcuMs.CodeGenFRUC]'); Result:=cceError; break; end;
 if BWwU<>2 then begin ALine.AppendError('e',ALine.Params[2],'Only 32-bit registers are allowed [R:TAsmMcuMs.CodeGenFRUC]'); Result:=cceError; break; end;

 if IsPointer(BParamConst.Name,BLabel) then break; // This is not our problem anymore, just verify other cases
 if GetRegIndex(BParamConst.Name)>=0 then break;

 BConst:=0;
 if IsIntegerEqu(BParamConst.Name,BConstI) then begin ALine.AppendError('e',BParamConst,'Floating point parameter required [R:TAsmMcuMs.CodeGenFRUC]'); Result:=cceError; break; end;
 if IsFloatEqu(BParamConst.Name,BConstFD) then
  begin
  if BRrrr=CRegSpx then begin ALine.AppendError('e',BParamConst,'Floating point constants are not applicable for ESP [R:TAsmMcuMs.CodeGenAC]'); Result:=cceError; break; end;
  BConstFS:=BConstFD;
  BConst:=BConstFI;
  end
 else begin ALine.AppendError('e',BParamConst,'Floating point parameter required [R:TAsmMcuMs.CodeGenFRUC]'); Result:=cceError; break; end;

 // FRUC   |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
 ALine.AppendDataBinW($E000 or (BCcc shl 8) or (BUuuu shl 4) or BRrrr);
 ALine.AppendDataBinD(BConst);

 Result:=cceCompiled;
 until TRUE;
End;

Function TAsmMcuMs.CodeGenFRRS ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR,
  BIdxU         : Integer;
  BWwR,
  BWwU          : Word;
  BUuuu,
  BRrrr         : word;
  BCcc          : word;
Begin
 Result:=cceCheckNext;

 repeat
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'fadd fsub fmul fdiv');
 if BCmdIdx<0 then break;
 BCcc:=Word(BCmdIdx);

 if Length(ALine.Params)<>3 then break;

 BIdxR:=GetRegIndex(ALine.Params[1].Name); if BIdxR<0 then break;
 BIdxU:=GetRegIndex(ALine.Params[2].Name); if BIdxU<0 then break;

 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;
 BWwU:=BIdxU shr 4; BUuuu:=BIdxU and $F;
 if BWwR<>BWwU then break;
 if BWwR<>2 then begin ALine.AppendError('e',ALine.Params[1],'Floating point constants are only applicable for 32-bit registers [R:TAsmMcuMs.CodeGenFRRS]'); Result:=cceError; break; end;

 if BRrrr=CRegSpx then ALine.AppendError('w',ALine.Params[2],'With ESP use constant, otherwise it is not possible to calculate stack use [R:TAsmMcuMs.CodeGenFRRS]');

 // FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
 ALine.AppendDataBinW($E800 or (BCcc shl 8) or (BUuuu shl 4) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

// BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
Function TAsmMcuMs.CodeGenBTR ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx,
  BIdxR         : Integer;
  BWwR          : Word;
  BRrrr         : word;
  BCmd          : Word;
  BConstIdx     : Cardinal;
  BIdxParam     : TAsmLineParam;
  BConstI       : Int64;
Begin
 Result:=cceCheckNext;

 repeat
 // Common case
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'bt btr bts btx');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>3 then break;

 BIdxR:=GetRegIndex(ALine.Params[1].Name);
 if BIdxR=-1 then break;
 BWwR:=BIdxR shr 4; BRrrr:=BIdxR and $F;

 BIdxParam:=ALine.Params[2];

 if IsIntegerEqu(BIdxParam.Name,BConstI)=FALSE then begin BIdxParam.AppendError('e','Invalid bit index [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;
 if (BConstI<0) or (BConstI>((1 shl BWwR)*8-1)) then begin BIdxParam.AppendError('e','Bit index out of range [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;
 if (BRrrr and $8)<>0 then
  begin
  case BWwR of
    $0: begin BConstI:=BConstI+8;  BRrrr:=BRrrr and $7; end;
    $1: begin BConstI:=BConstI+16; BRrrr:=BRrrr and $7; end;
  end; // case
  end;
 BConstIdx:=Cardinal(BConstI);

 // BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
 BCmd:=$F000;
 ALine.AppendDataBinW(BCmd or (Cardinal(BCmdIdx) shl 8) or ((BConstIdx and $0F) shl 4) or ((BConstIdx and $10) shl 6) or BRrrr);
 Result:=cceCompiled;
 until TRUE;
End;

// BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
Function TAsmMcuMs.CodeGenBTM ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BAaaa         : word;
  BLabel        : string;
  BConstSize    : byte;
  BRef          : string;
  BCmd          : Word;
  BConstIdx     : Cardinal;
  BConstAddr    : Cardinal;
  BRefType      : char;
  BIdxParam     : TAsmLineParam;
  BPointer      : TAsmLineParam;
  BConstI       : Int64;
Begin
 Result:=cceCheckNext;
 BLabel:=''; BRef:=''; BConstAddr:=0; BConstSize:=0;
 BAaaa:=0;

 repeat
 // Common case
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'bt btr bts btx');
 if BCmdIdx=-1 then break;
 if Length(ALine.Params)<>3 then break;

 BPointer:=ALine.Params[1];
 BIdxParam:=ALine.Params[2];

 if IsIntegerEqu(BIdxParam.Name,BConstI)=FALSE then begin BIdxParam.AppendError('e','Invalid bit index [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;
 if (BConstI<0) or (BConstI>7) then begin BIdxParam.AppendError('e','Bit index must be in the interval 0..7 [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;
 BConstIdx:=Cardinal(BConstI);

 if IsPointer(BPointer.Name,BLabel)=FALSE then begin BPointer.AppendError('e','Pointer type expected [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;

 if Pos('++',BLabel)<>0 then begin BPointer.AppendError('e','Increment is not allowed [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;
 if Pos('--',BLabel)<>0 then begin BPointer.AppendError('e','Decrement is not allowed [R:TAsmMcuMs.CodeGenBTx]'); Result:=cceError; break; end;

 if GetXMem(BPointer,BLabel,BAaaa,BConstSize,BConstAddr,BRef)=FALSE then begin Result:=cceError; break; end;

 // BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
 BCmd:=$F800;
 if BConstSize>2 then BCmd:=BCmd or $0008;
 ALine.AppendDataBinW(BCmd or (Cardinal(BCmdIdx) shl 8) or (BAaaa shl 4) or BConstIdx);
 if BConstSize=0 then BConstSize:=2;
 ALine.AppendDataBinX(BConstAddr,BConstSize);
 if BRef<>'' then
  begin
  if BConstSize=2 then BRefType:='r' else BRefType:='R';
  ALine.AppendRef(BPointer,BRef,BRefType,2,BConstAddr);
  end;

 Result:=cceCompiled;
 until TRUE;
End;

// ARUI   |1111110 ||       e|uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex xdivex xremex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd i)
// ARUS   |1111111 ||       e|uuuurrrr|ccccwwww|00wwssss| addex subex andex orex xorex mulex xdivex xremex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd s)
Function TAsmMcuMs.CodeGenARUI_ARUS ( ALine : TAsmFlowLine ) : TCmdCompError;
Var
  BCmdIdx       : Integer;
  BCccc         : Word;
  BIndex        : Integer;
  BRegIdx       : Integer;
  BRegIdxList   : array [0..2] of Integer;
  BWwList       : array [0..2] of Word;
  BFldList      : array [0..2] of Word;
  BConst        : Cardinal;
  BConstI       : Int64;
  BE            : byte;
  BSignExt      : char;
  BIsRor        : boolean;
Begin
 Result:=cceCheckNext;

 BWwList[0]:=0; BWwList[1]:=0; BWwList[2]:=0;
 BFldList[0]:=0; BFldList[1]:=0; BFldList[2]:=0;

 repeat

 BCmdIdx:=-1; BIsRor:=FALSE;
 repeat
 BSignExt:='.'; BE:=0;
 if LowerCase(ALine.Params[0].Name)='ror' then begin BCmdIdx:=10; BIsRor:=TRUE; break; end;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'add sub and or xor mul udiv urem shl shr rol asr fadd fsub fmul fdiv');
 if BCmdIdx>=0 then begin BE:=0; break; end;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'# # # # # # sdiv srem # # # # # # # #');
 if BCmdIdx>=0 then begin BE:=1; break; end;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'addzx subzx andzx orzx xorzx mulzx udivzx uremzx shlzx shrzx rolzx asrzx # # # #');
 if BCmdIdx>=0 then begin BE:=0; BSignExt:='z'; break; end;
 BCmdIdx:=GetStrIndex(ALine.Params[0].Name,'addsx subsx andsx orsx xorsx mulsx sdivsx sremsx shlsx shrsx rolsx asrsx # # # #');
 if BCmdIdx>=0 then begin BE:=1; BSignExt:='s'; break; end;
 until TRUE;
 if BCmdIdx<0 then break;
 BCccc:=BCmdIdx;

 // ARUI   |1111110 ||       e|uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex udivex sdivex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd i)
 // ARUS   |1111111 ||       e|uuuurrrr|ccccwwww|00wwssss| addex subex andex orex xorex mulex udivex sdivex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd s)
 if Length(ALine.Params)=4 then // 3 regs classic or 2 regs + #
  begin
  BIndex:=0;
  while BIndex<3 do
   begin
   BRegIdx:=GetRegIndex(ALine.Params[1+BIndex].Name);
   if BRegIdx<0 then break;
   BRegIdxList[BIndex]:=BRegIdx;
   BWwList[BIndex]:=BRegIdxList[BIndex] shr 4;
   BFldList[BIndex]:=BRegIdxList[BIndex] and $F;
   inc(BIndex);
   end;
  if BIndex<2 then break;
  // 3 registers
  if BIndex=3 then
   begin
   if BIsRor then begin ALine.Params[0].AppendError('e','ROR command will only work with constant [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
   if ((BWwList[0]<>BWwList[1]) or (BWwList[1]<>BWwList[2])) and (BSignExt='.') then begin ALine.Params[0].AppendError('e','Registers of different size: use prefixes ZX or SX [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
   ALine.AppendDataBinW($FE00 or (BE shl 8) or (BFldList[1] shl 4) or (BFldList[0] shl 0));
   ALine.AppendDataBinW((BCccc shl 12) or (BWwList[1] shl 10) or (BWwList[0] shl 8) or (BWwList[2] shl 4) or BFldList[2]);
   Result:=cceCompiled;
   break;
   end;
  // 2 registers and Const
  if BIsRor then begin ALine.Params[0].AppendError('e','ROR command will only work with constant [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
  BConst:=0;
  if (BWwList[0]<>BWwList[1]) and (BSignExt='.') then begin ALine.Params[0].AppendError('e','Registers of different size: use prefixes ZX or SX [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
  if IsIntegerEqu(ALine.Params[3].Name,BConstI)=FALSE then begin ALine.AppendError('e',ALine.Params[3],'Cannot convert constant to integer [R:TAsmMcuMs.CodeGenBBx]'); Result:=cceError; break; end;
  if (BConstI>255) or (BConstI<0) then begin ALine.AppendError('e',ALine.Params[2],'Constant is too big or too small [R:TAsmMcuMs.CodeGenBBx]'); Result:=cceError; break; end;
  BConst:=Cardinal(BConstI);
  ALine.AppendDataBinW($FC00 or (BE shl 8) or (BFldList[1] shl 4) or (BFldList[0] shl 0));
  ALine.AppendDataBinW((BCccc shl 12) or (BWwList[1] shl 10) or (BWwList[0] shl 8) or BConst);
  Result:=cceCompiled;
  break;
  end;

 if Length(ALine.Params)=3 then // 2 regs or 1 reg + # (then uuuu and rrrr are the same)
  begin
  BIndex:=0;
  while BIndex<2 do
   begin
   BRegIdx:=GetRegIndex(ALine.Params[1+BIndex].Name);
   if BRegIdx<0 then break;
   BRegIdxList[BIndex]:=BRegIdx;
   BWwList[BIndex]:=BRegIdxList[BIndex] shr 4;
   BFldList[BIndex]:=BRegIdxList[BIndex] and $F;
   inc(BIndex);
   end;
  if BIndex<1 then break;
  if BIndex=2 then // 2 registers
   begin
   if BIsRor then begin ALine.Params[0].AppendError('e','ROR command will only work with constant [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
   if (BWwList[0]<>BWwList[1]) and (BSignExt='.') then begin ALine.Params[0].AppendError('e','Registers of different size: use prefixes ZX or SX [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
   ALine.AppendDataBinW($FE00 or (BE shl 8) or (BFldList[0] shl 4) or (BFldList[0] shl 0));
   ALine.AppendDataBinW((BCccc shl 12) or (BWwList[0] shl 10) or (BWwList[0] shl 8) or (BWwList[1] shl 4) or BFldList[1]);
   Result:=cceCompiled;
   break;
   end;
  // 1 registers and Const
  BConst:=0;
  if IsIntegerEqu(ALine.Params[2].Name,BConstI)=FALSE then begin ALine.AppendError('e',ALine.Params[2],'Cannot convert constant to integer [R:TAsmMcuMs.CodeGenBBx]'); Result:=cceError; break; end;
  if (BConstI>255) or (BConstI<0) then begin ALine.AppendError('e',ALine.Params[2],'Constant is too big or too small [R:TAsmMcuMs.CodeGenBBx]'); Result:=cceError; break; end;
  BConst:=Cardinal(BConstI);
  if BIsRor then BConst:=((1 shl BWwList[0])*8)-BConst;
  ALine.AppendDataBinW($FC00 or (BE shl 8) or (BFldList[0] shl 4) or (BFldList[0] shl 0));
  ALine.AppendDataBinW((BCccc shl 12) or (BWwList[0] shl 10) or (BWwList[0] shl 8) or BConst);
  Result:=cceCompiled;
  break;
  end;

 if BIsRor then begin ALine.Params[0].AppendError('e','ROR command will only work with constant [R:TAsmMcuMs.CodeGenARUI_ARUS]'); Result:=cceError; break; end;
 if Length(ALine.Params)=2 then // 1 regs by 1 (then uuuu and rrrr are the same)
  begin
  BIndex:=0;
  while BIndex<1 do
   begin
   BRegIdx:=GetRegIndex(ALine.Params[1+BIndex].Name);
   if BRegIdx<0 then break;
   BRegIdxList[BIndex]:=BRegIdx;
   BWwList[BIndex]:=BRegIdxList[BIndex] shr 4;
   BFldList[BIndex]:=BRegIdxList[BIndex] and $F;
   inc(BIndex);
   end;
  if BIndex<1 then break;
  // 1 registers and Const
  BConst:=1;
  ALine.AppendDataBinW($FC00 or (BE shl 8) or (BFldList[0] shl 4) or (BFldList[0] shl 0));
  ALine.AppendDataBinW((BCccc shl 12) or (BWwList[0] shl 10) or (BWwList[0] shl 8) or BConst);
  Result:=cceCompiled;
  break;
  end;

 until TRUE;
End;

end.

