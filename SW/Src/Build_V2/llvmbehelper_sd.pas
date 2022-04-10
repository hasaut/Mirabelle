unit LlvmBeHelper_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsHelper_sd, LlvmBase_sd, LlvmLine_sd, LlvmSplit_sd;

Type
  TTargRegMap = record
    FTypeC      : char;
    FAsgnRow,
    FAsgnWww    : byte;
    FWeight     : Single;
  end;

  PTargRegMap = ^TTargRegMap;
  TTargRegMapList = array of TTargRegMap;

  TConfl = record
    FTextIdx,
    FTargIdx    : Integer;
    FWeight     : Single;
  end;

  TConflList = array of TConfl;

  TRegDescr = record
    FRow,
    FWww        : byte;
  end;

  TRegDescrList = array of TRegDescr;

  TStackState = record
    FCallIdx    : Integer;
    FDepth      : Integer;
  end;

  TStackStateList = array of TStackState;

  TVsmItem = class(TObject) // VarStackMap item shows where variable is located in the stack
  protected
    FVarName    : string;
    FSize       : Integer;
    FRefOffset,            // Reference in parameter list in case of VAR
    FVarOffset  : Integer; // Offset in stack of a variable or of a copy in case of VAR
    FIsA        : boolean;
    FIsAV,
    FIsAP,                 // If pointer, i.e. Const/Var String, Array, Record
    FIsAR       : boolean;
    FHasCopy    : boolean;
    FMapped     : boolean; // Used when mapping variables in stack and detect overlaps
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function SaveToString : string;
    Function LoadFromString ( Var ADataS : string ) : boolean; Virtual;

    property VarName : string read FVarName write FVarName;
    property Size : Integer read FSize write FSize;
    property RefOffset : Integer read FRefOffset write FRefOffset;
    property VarOffset : Integer read FVarOffset write FVarOffset;
    property IsA : boolean read FIsA write FIsA;
    property IsAV : boolean read FIsAV write FIsAV;
    property IsAP : boolean read FIsAP write FIsAP;
    property IsAR : boolean read FIsAR write FIsAR;
    property HasCopy : boolean read FHasCopy write FHasCopy;
    property Mapped : boolean read FMapped write FMapped;
  end;

  TVsmList = array of TVsmItem;

  TProcBeHelper = class(TProcSplit)
  private
    Procedure MarkStackDepthA ( Const AStackState : TStackStateList; Var AUsage : string; ATextIdx : Integer );
    Function VsmOverlapInStack ( AVsmItemA, AVsmItemB : TVsmItem ) : boolean;
    Function VsmOverlapInTime ( AVsmIdxA, AVsmIdxB : Integer ) : boolean;
  protected
    FStackDepth         : array of Integer;
    FVsmList            : TVsmList;

    Function IsProcCallRes ( ATargIdx : Integer ) : boolean;
    Function IsRefByAddr ( ATargIdx : Integer ) : boolean;
    Function HasStackCopy ( Const AVarName : string ) : boolean;

    Procedure MarkStackDepth;

    Procedure ClearVarStackMapList;
    Procedure VsmListInit;
    Procedure VsmListMap;

    Function DbgVarStackMapList : string;
    Function DbgTargList : string;
    Function DbgProcHeader : string; Override;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TModuleBeHelper = class(TLlvmModule)
  protected
    Procedure CleanupConsts;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

Const
  CProcMoveCwx  = CTagS+'mf_'+CTagM+'Sys@_MoveCwx'+CTagP+'dap'+CTagP+'dap'+CTagP+'dad'+CTagP+CTagE;

  ZConfl : TConfl = (FTextIdx: -1; FTargIdx: -1; FWeight: 0.0);
  ZTargRegMap : TTargRegMap =
   (
    FTypeC: '.';
    FAsgnRow: 0;
    FAsgnWww: 0;
    FWeight: 0.0;
   );


implementation

Uses
  ConComL;

// *** TVsmItem

Constructor TVsmItem.Create;
Begin
 Inherited;
End;

Destructor TVsmItem.Destroy;
Begin
 Inherited;
End;

Function TVsmItem.SaveToString : string;
Begin
 Result:=FVarName+' '+IntToStr(FSize)+' '+IntToStr(FRefOffset)+' '+IntToStr(FVarOffset)+' ';
 if FIsA then Result:=Result+'1' else Result:=Result+'0';
 if FIsAV then Result:=Result+'1' else Result:=Result+'0';
 if FIsAP then Result:=Result+'1' else Result:=Result+'0';
 if FIsAR then Result:=Result+'1' else Result:=Result+'0';
 if FHasCopy then Result:=Result+'1' else Result:=Result+'0';
End;

Function TVsmItem.LoadFromString ( Var ADataS : string ) : boolean;
Var
  BDummyS       : string;
Begin
 Result:=FALSE;
 repeat
 FVarName:=ReadParamStr(ADataS); if FVarName='' then break;
 BDummyS:=ReadParamStr(ADataS); if BDummyS='' then break; if TryStrToInt(BDummyS,FSize)=FALSE then break;
 BDummyS:=ReadParamStr(ADataS); if BDummyS='' then break; if TryStrToInt(BDummyS,FRefOffset)=FALSE then break;
 BDummyS:=ReadParamStr(ADataS); if BDummyS='' then break; if TryStrToInt(BDummyS,FVarOffset)=FALSE then break;
 BDummyS:=ReadParamStr(ADataS); if Length(BDummyS)<>5 then break;
 FIsA:=BDummyS[1]='1';
 FIsAV:=BDummyS[2]='1';
 FIsAP:=BDummyS[3]='1';
 FIsAR:=BDummyS[4]='1';
 FHasCopy:=BDummyS[5]='1';
 Result:=TRUE;
 until TRUE;
End;

// *** ProcBeHelper

Constructor TProcBeHelper.Create;
Begin
 Inherited;
End;

Destructor TProcBeHelper.Destroy;
Begin
 FStackDepth:=nil;
 ClearVarStackMapList;
 Inherited;
End;

Function TProcBeHelper.IsProcCallRes ( ATargIdx : Integer ) : boolean;
Var
  BLine         : TFlowLine;
  BTarg,
  BUsage        : string;
  BIndex        : Integer;
Begin
 BTarg:=FChTargList.Strings[ATargIdx];
 BUsage:=FChUsageList.Strings[ATargIdx];
 BIndex:=0;
 while BIndex<Length(BUsage) do
  begin
  if BUsage[1+BIndex]='d' then
   begin
   BLine:=FFlow[BIndex];
   if BLine.IsCall and (BLine.Param[0].Targ=BTarg) then break;
   end;
  inc(BIndex);
  end;
 Result:=BIndex<Length(BUsage);
End;

Function TProcBeHelper.IsRefByAddr ( ATargIdx : Integer ) : boolean;
Var
  BLine         : TFlowLine;
  BTarg,
  BUsage        : string;
  BIndex        : Integer;
Begin
 BTarg:=FChTargList.Strings[ATargIdx];
 BUsage:=FChUsageList.Strings[ATargIdx];
 BIndex:=0;
 while BIndex<Length(BUsage) do
  begin
  if BUsage[1+BIndex]='d' then
   begin
   BLine:=FFlow[BIndex];
   if BLine.IsPush and (Copy(BLine.Param[0].Targ,1,2)='ev') and (BLine.Param[1].Targ=BTarg) then break;
   end;
  inc(BIndex);
  end;
 Result:=BIndex<Length(BUsage);
End;

Function TProcBeHelper.HasStackCopy ( Const AVarName : string ) : boolean;
Var
  BLine         : TFlowLine;
  BRowIdx,
  BColIdx       : Integer;
  BUsage        : char;
Begin
 Result:=FALSE;
 repeat
 // if something is referenced by address, force this element to be stored in stack
 BRowIdx:=0;
 while BRowIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BRowIdx];
  if BLine.IsLea and (BLine.Param[1].Targ=AVarName) then begin Result:=TRUE; break; end;
  if ParsIsArray(AVarName) then
   begin
   if BLine.IsMovMr then
    begin
    if BLine.Param[0].Arr=AVarName then begin Result:=TRUE; break; end;
    end;
   if BLine.IsMovRm then
    begin
    if BLine.Param[1].Arr=AVarName then begin Result:=TRUE; break; end;
    end;
   end; // IsArray
  inc(BRowIdx);
  end;
 if Result then break;
 // If identifier is forced to be stored or loaded, we keep it in stack
 BColIdx:=0;
 while BColIdx<FChTargList.Count do
  begin
  repeat
  if AVarName<>FChTargList.Strings[BColIdx] then break;
  BRowIdx:=0;
  while BRowIdx<Length(FFlow) do
   begin
   BUsage:=FUsageMatr[BRowIdx,BColIdx];
   if BUsage in ['s', 'd'] then
    begin
    BLine:=FFlow[BRowIdx];
    if BLine.IsLdx or BLine.IsStx then begin Result:=TRUE; break; end;
    end;
   if Result then break;
   inc(BRowIdx);
   end;
  until TRUE;
  if Result then break;
  inc(BColIdx);
  end;
 until TRUE;
End;

Procedure TProcBeHelper.MarkStackDepth;
Var
  BDepthIdx,
  BDepthLen     : Integer;
  BStackState   : array of TStackState;
  BUsage        : string;
Begin
 BStackState:=nil;
 repeat
 BDepthLen:=Length(FFlow); if BDepthLen=0 then break;
 SetLength(FStackDepth,BDepthLen);
 for BDepthIdx:=0 to BDepthLen-1 do FStackDepth[BDepthIdx]:=0;
 BUsage:=GetInitMask;
 MarkStackDepthA(BStackState,BUsage,0)
 until TRUE;
 BStackState:=nil;
End;

Procedure TProcBeHelper.MarkStackDepthA ( Const AStackState : TStackStateList; Var AUsage : string; ATextIdx : Integer );
Var
  BLine         : TFlowLine;
  BTextIdx      : Integer;
  BStackState   : TStackStateList;
  BLen          : Integer;
  BStackTop     : TStackState;
  BCallIdx      : Integer;
Begin
 BStackState:=Copy(AStackState);
 BLen:=Length(BStackState);
 if BLen>0 then BStackTop:=BStackState[BLen-1]
 else begin BStackTop.FCallIdx:=-1; BStackTop.FDepth:=0; end;
 BTextIdx:=ATextIdx;
 while BTextIdx<Length(FFlow) do
  begin
  if CheckSetMask(AUsage,BTextIdx) then break;
  FStackDepth[BTextIdx]:=BStackTop.FDepth;
  BLine:=FFlow[BTextIdx];
  if BLine.IsPush then
   begin
   BStackTop.FCallIdx:=ExtractCallIdx(BLine.Cmd);
   inc(BStackTop.FDepth);
   inc(BLen); SetLength(BStackState,BLen); BStackState[BLen-1]:=BStackTop;
   end
  else if BLine.IsCall then
   begin
   BCallIdx:=ExtractCallIdx(BLine.Cmd);
   repeat
   if BCallIdx<>BStackTop.FCallIdx then break;
   if BLen>0 then begin dec(BLen); SetLength(BStackState,BLen); end;
   if BLen>0 then BStackTop:=BStackState[BLen-1]
   else begin BStackTop.FCallIdx:=-1; BStackTop.FDepth:=0; break; end;
   until FALSE;
   end;

  if IsJmp(BLine.Cmd) then BTextIdx:=FindLabelIdx(BLine.Param[0].Targ)
  else if BLine.IsJxx then
   begin
   MarkStackDepthA(BStackState,AUsage,FindLabelIdx(BLine.Param[0].Targ));
   inc(BTextIdx);
   end
  else inc(BTextIdx);
  end;
 BStackState:=nil;
End;

Procedure TProcBeHelper.ClearVarStackMapList;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FVsmList) do
  begin
  FVsmList[BIndex].Free;
  inc(BIndex);
  end;
 FVsmList:=nil;
End;

Procedure TProcBeHelper.VsmListInit;
Var
  BVarIdx       : Integer;
  BVsmItem      : TVsmItem;
  BTypeS        : string;
  BVarSpec      : string;
Begin
 ClearVarStackMapList;
 SetLength(FVsmList,FParVarList.Count);
 BVarIdx:=0;
 while BVarIdx<FParVarList.Count do
  begin
  BVsmItem:=TVsmItem.Create;
  FVsmList[BVarIdx]:=BVsmItem;
  BVsmItem.VarName:=FParVarList.Strings[BVarIdx];
  BVarSpec:=ParsExtractSpec(BVsmItem.VarName);
  if ParsIsArray(BVsmItem.VarName) then
   begin
   BTypeS:=ParsExtractElemType(BVsmItem.VarName);
   BVsmItem.Size:=FModule.GetTypeSize(BTypeS)*ParsGetArrayDim(BVsmItem.VarName);
   end
  else if ParsIsRecord(BVsmItem.VarName) then
   begin
   BTypeS:=ParsExtractType(BVsmItem.VarName);
   BVsmItem.Size:=FModule.GetTypeSize(BTypeS);
   end
  else if ParsIsPointer(BVsmItem.VarName) then
   begin
   BVsmItem.Size:=FModule.GetTypeSize('p');
   end
  else if ParsIsStringP(BVsmItem.VarName) then
   begin
   if ParsIsParamConst(BVarSpec) or ParsIsParamVar(BVarSpec) then BVsmItem.Size:=FModule.GetTypeSize('p')
   else BVsmItem.Size:=FModule.GetTypeSize(ExtractFinalType(BVsmItem.VarName));
   end
  else
   begin
   BTypeS:=ExtractFinalType(BVsmItem.VarName);
   BVsmItem.Size:=FModule.GetTypeSize(BTypeS);
   end;
  BVsmItem.RefOffset:=0;
  BVsmItem.VarOffset:=0;
  BVsmItem.IsA:=FALSE;
  BVsmItem.IsAV:=FALSE;
  BVsmItem.IsAR:=FALSE;
  BVsmItem.IsAP:=FALSE;

  if (Pos('dcsp',BVarSpec)=1) or (Pos('dvsp',BVarSpec)=1) or (Pos('drsp',BVarSpec)=1) then BVsmItem.IsAP:=TRUE
  else if (Pos('dcr',BVarSpec)=1) or (Pos('drr',BVarSpec)=1) then BVsmItem.IsAP:=TRUE
  else if Pos('dv',BVarSpec)=1 then BVsmItem.IsAV:=TRUE
  else if Pos('dr',BVarSpec)=1 then BVsmItem.IsAR:=TRUE;
  BVsmItem.IsA:=BVsmItem.IsAR or BVsmItem.IsAV or BVsmItem.IsAP or (Pos('da',BVarSpec)=1) or ((Pos('dc',BVarSpec)=1) and ParsIsTypeBasic(ParsExtractType(BVsmItem.VarName)));

  BTypeS:=ParsExtractType(BVsmItem.VarName);

  if (ParsIsArray(BVsmItem.VarName) or ParsIsStringP(BVsmItem.VarName) or ParsIsRecord(BVsmItem.VarName)) and (BVsmItem.IsAV=FALSE) and (BVsmItem.IsAP=FALSE) then BVsmItem.HasCopy:=TRUE
  else if BVsmItem.IsAV then BVsmItem.HasCopy:=ParsIsTypeBasic(BTypeS) or ParsIsTypePointer(BTypeS)
  else if BVsmItem.IsA or BVsmItem.IsAP then BVsmItem.HasCopy:=FALSE
  else BVsmItem.HasCopy:=HasStackCopy(BVsmItem.VarName);

  inc(BVarIdx);
  end;
End;

Function PointInside ( APointTest : Integer; APointA, APointB : Integer ) : boolean;
Begin
 Result:=(APointTest>=APointA) and (APointTest<APointB);
End;

Function TProcBeHelper.VsmOverlapInStack ( AVsmItemA, AVsmItemB : TVsmItem ) : boolean;
Var
  BPointA,
  BPointB,
  BPointC,
  BPointD       : Integer;
Begin
 BPointA:=AVsmItemA.VarOffset; BPointB:=BPointA+AVsmItemA.Size;
 BPointC:=AVsmItemB.VarOffset; BPointD:=BPointC+AVsmItemB.Size;
 Result:=TRUE;
 repeat
 if PointInside(BPointC,BPointA,BPointB) then break;
 if PointInside(BPointD,BPointA,BPointB) then break;
 if PointInside(BPointA,BPointC,BPointD) then break;
 if PointInside(BPointB,BPointC,BPointD) then break;
 Result:=FALSE;
 until TRUE;
End;

Function TProcBeHelper.VsmOverlapInTime ( AVsmIdxA, AVsmIdxB : Integer ) : boolean;
Begin
 Result:=TRUE;
End;

Procedure TProcBeHelper.VsmListMap;
Var
  BVsmIdxA,
  BVsmIdxB      : Integer;
  BBiggestSize  : Integer;
  BVsmItemA,
  BVsmItemB     : TVsmItem;
  BConflict     : boolean;
  BOffset       : Integer;

Begin
 for BVsmIdxB:=0 to Length(FVsmList)-1 do FVsmList[BVsmIdxB].Mapped:=FALSE;

 BConflict:=FALSE;
 repeat
 // Find biggest unmapped variable
 BVsmIdxA:=-1;
 BBiggestSize:=-1;
 BVsmIdxB:=0;
 while BVsmIdxB<Length(FVsmList) do
  begin
  BVsmItemB:=FVsmList[BVsmIdxB];
  if BVsmItemB.HasCopy and (BVsmItemB.Mapped=FALSE) and (BVsmItemB.Size>BBiggestSize) then
   begin
   BVsmIdxA:=BVsmIdxB;
   BBiggestSize:=BVsmItemB.Size;
   end;
  inc(BVsmIdxB);
  end;
 if BVsmIdxA=-1 then break;

 // Assign offset
 BVsmItemA:=FVsmList[BVsmIdxA];
 BVsmItemA.VarOffset:=0;
 repeat
 for BVsmIdxB:=0 to Length(FVsmList)-1 do
  begin
  BVsmItemB:=FVsmList[BVsmIdxB];
  repeat
  if BVsmItemB.Mapped=FALSE then break;
  if BVsmItemB.HasCopy=FALSE then break;
  if VsmOverlapInStack(BVsmItemA,BVsmItemB)=FALSE then break;
  BConflict:=VsmOverlapInTime(BVsmIdxA,BVsmIdxB);
  until TRUE;
  if BConflict then
   begin
   BOffset:=BVsmItemB.VarOffset+BVsmItemB.Size;
   if (BVsmItemA.Size=0) or (BVsmItemA.Size=1) then
   else if (BVsmItemA.Size=2) or (BVsmItemA.Size=3) then BOffset:=(BOffset+1) and $FFFFFFFE
   else FModule.AlignSize(BOffset);
   BVsmItemA.VarOffset:=BOffset;
   break;
   end;
  end;
 if BConflict=FALSE then break;
 until FALSE;

 until FALSE;
End;

Function TProcBeHelper.DbgVarStackMapList : string;
Var
  BIndex        : Integer;
Begin
 Result:='';
 for BIndex:=0 to Length(FVsmList)-1 do Result:=Result+FVsmList[BIndex].SaveToString+' ';
End;

Function TProcBeHelper.DbgTargList : string;
Var
  BTargIdx      : Integer;
Begin
 Result:='';
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  Result:=Result+FChTargList.Strings[BTargIdx]+' ';
  inc(BTargIdx);
  end;
End;

Function TProcBeHelper.DbgProcHeader : string;
Begin
 Result:=';@P [Proc:'+FNameL+'][ChTargList:'+DbgTargList+'][VarStackMap:'+DbgVarStackMapList+']';
End;

{ *** TModuleBeHelper *** }

Constructor TModuleBeHelper.Create;
Begin
 Inherited;
End;

Destructor TModuleBeHelper.Destroy;
Begin
 Inherited;
End;

Procedure TModuleBeHelper.CleanupConsts;
Var
  BConstIdx,
  BProcIdx      : Integer;
  BConstDescr   : string;
  BConstTarg    : string;
  BReferenced   : boolean;
  BProc         : TLlvmProc;
Begin
 BConstIdx:=0;
 while BConstIdx<FConstList.Count do
  begin
  BConstDescr:=FConstList.Strings[BConstIdx];
  BReferenced:=FALSE;
  repeat
  BConstTarg:=ReadParamStr(BConstDescr);
  if BConstTarg='' then break;
  //if ParsIsTmp(BConstTarg)=FALSE then break;
  if StrInList(BConstTarg,FPublicNames) then begin BReferenced:=TRUE; break; end;
  BProcIdx:=0;
  while BProcIdx<Length(FProcList) do
   begin
   BProc:=FProcList[BProcIdx] as TLlvmProc;
   if BProc.IsTargUsed(BConstTarg) then break;
   inc(BProcIdx);
   end;
  if BProcIdx<Length(FProcList) then begin BReferenced:=TRUE; break; end;
  until TRUE;
  if BReferenced then inc(BConstIdx)
  else FConstList.Delete(BConstIdx);
  end;
End;

end.

