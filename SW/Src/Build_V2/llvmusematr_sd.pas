unit LlvmUseMatr_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsHelper_sd, LlvmBase_sd, LlvmDbg_sd, LlvmLine_sd,
  LlvmOrder_sd;

Type
  TForceCmd = record
    FCodeIdx    : Integer;
    FChainIdx   : Integer;
    FCmd        : char;
  end;

  TForceCmdList = array of TForceCmd;

  TUsageMatr = array of array of char;

  TProcUseMatr = class(TProcOrder)
  private
    FTextMatr : TStringList;
    FTmpList    : TStringList;

    Function IsUsageOverlap ( Const AUsageA, AUsageB : string; Out AUsageM : string ) : boolean;

    Procedure FillChains ( Const ATarg : string );

    Procedure WrForceCmd ( ALineIdx, AChainIdx : Integer; Const ACmd : char );
    Procedure WarnUsage ( Var ADirty : boolean );
    Procedure DbgUpdateUseMatr;

  protected
    FInitMask   : string;

    FParVarList : TStringList;
    FChTargList,
    FChUsageList : TStringList;
    FUsageMatr  : TUsageMatr;

    FForceCmdList       : TForceCmdList;

    Procedure FillChains;
    Procedure BuildUsageMatr;

    Procedure RecompileChainsOpti;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure DebugInfo ( AList : TStringList ); Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;

    property TextMatr : TStringList read FTextMatr;
    //property VarUseList : TStringList read FVarUseList;
  end;

Procedure PropVarUsageToMask ( Const AVarUsage : string; Var AFullMask : string );

implementation

Uses
  ConComL;

Procedure PropVarUsageToMask ( Const AVarUsage : string; Var AFullMask : string );
Var
  BIndex        : Integer;
Begin
 for BIndex:=1 to Length(AVarUsage) do
  begin
  if AVarUsage[BIndex]<>'.' then AFullMask[BIndex]:='*';
  end;
End;

Constructor TProcUseMatr.Create;
Begin
 Inherited;
 FTextMatr:=TStringList.Create;
 FChUsageList:=TStringList.Create;
 FChTargList:=TStringList.Create;
 FParVarList:=TStringList.Create;
 FTmpList:=TStringList.Create;
End;

Destructor TProcUseMatr.Destroy;
Begin
 FUsageMatr:=nil;
 FForceCmdList:=nil;
 FTmpList.Free;
 FParVarList.Free;
 FChTargList.Free;
 FChUsageList.Free;
 FTextMatr.Free;
 Inherited;
End;

Procedure TProcUseMatr.DebugInfo ( AList : TStringList );
Var
  BIndex,
  BIndexA       : Integer;
  BDummyS       : string;
  BTargUsageAll : string;
Begin
 Inherited;

 //AList.Append('*** Atom');
 //for BIndex:=0 to TextJmps.Count-1 do AList.Append(TextJmps.Strings[BIndex]);
 repeat
 //if GetErrorCountA<>0 then break;

 AList.Append('');
 AList.Append('*** Chains');
 for BIndex:=0 to FChTargList.Count-1 do
  begin
  AList.Append(AddSpacesResL(IntToStr(BIndex),2)+' '+RemoveRecInside(FChTargList.Strings[BIndex]));
  end;

 BDummyS:=''; AddSpacesVarR(BDummyS,73);
 for BIndex:=0 to FChTargList.Count-1 do BDummyS:=BDummyS+AddSpacesResL(IntToStr(BIndex),2);
 AList.Append(BDummyS);

 BTargUsageAll:='';
 for BIndexA:=0 to FChUsageList.Count-1 do
  begin
  BTargUsageAll:=BTargUsageAll+FChUsageList.Strings[BIndexA];
  end;

 for BIndex:=0 to FTextMatr.Count-1 do
  begin
  BDummyS:=DbgFormatFlow(FTextMatr.Strings[BIndex]);

  AddSpacesVarR(BDummyS,73);
  for BIndexA:=0 to FChTargList.Count-1 do BDummyS:=BDummyS+' '+BTargUsageAll[1+FTextMatr.Count*BIndexA+BIndex];

  AList.Append(BDummyS);
  end;
 until TRUE;
End;

Procedure TProcUseMatr.Compile;
Var
  BDirty        : boolean;
Begin
 Inherited;

 FUsageMatr:=nil;

 repeat
 if FFatalError then break;

  repeat
  BDirty:=FALSE;                      if FModule.DbgPath<>'' then DbgSave(Self,'F_Matr',FFlow,TRUE);
  FillChains;                         if FModule.DbgPath<>'' then DbgSave(Self,'F_Matr',FFlow,TRUE);
  WarnUsage(BDirty);                  if FModule.DbgPath<>'' then DbgSave(Self,'F_Matr',FFlow,TRUE);
  if BDirty=FALSE then break;
  if GetErrorCountA<>0 then break;
  until FALSE;

 //if GetErrorCountA<>0 then break;
 BuildUsageMatr;                      //if FModule.DbgPath<>'' then DbgSave(Self,'Matr',FTextSrc,TRUE);

 until TRUE;

 DbgMarkFlow('Matr');
 DbgUpdateUseMatr;
 FlowListToStringList(FFlowList,FFlow);
 FTextMatr.Assign(FFlowList);

 TimeStamp('Matr');
 if FModule.DbgPath<>'' then DbgSave(Self,'F_Matr',FFlowList,TRUE);
End;

Procedure TProcUseMatr.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T Matr');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextMatr.Count-1 do ADbg.Append(FTextMatr.Strings[BLineIdx]);
End;

Procedure TProcUseMatr.RecompileChainsOpti; // FTextSrc must be valid
Begin
 FillChains;
 BuildUsageMatr;
 DbgUpdateUseMatr;
 FlowListToStringList(FTextMatr,FFlow);
End;

Procedure TProcUseMatr.FillChains;
Var
  BVarList      : string;
  BVarName      : string;
Begin
 FChTargList.Clear; FChUsageList.Clear;
 FParVarList.Clear;
 FInitMask:=GetInitMask;

 BVarList:=InvertParamsOrder(FParListS)+' '+FRetListS+' '+FVarListS;
 repeat
 BVarName:=ReadParamStr(BVarList);
 if BVarName='' then break;
 FParVarList.Append(BVarName);
 if ParsIsRecord(BVarName) or ParsIsArray(BVarName) or ParsIsStringP(BVarName) then
 else FillChains(BVarName);
 until FALSE;
End;

Function TProcUseMatr.IsUsageOverlap ( Const AUsageA, AUsageB : string; Out AUsageM : string ) : boolean;
Var
  BIndex        : Integer;
  BUseA,
  BUseB         : char;
Begin
 Result:=FALSE;
 AUsageM:=FInitMask;
 for BIndex:=1 to Length(AUsageA) do
  begin
  BUseA:=AUsageA[BIndex];
  BUseB:=AUsageB[BIndex];
  if (BUseA<>'.') and (BUseB<>'.') then Result:=TRUE;
  if BUseA<>'.' then AUsageM[BIndex]:=BUseA else AUsageM[BIndex]:=BUseB;
  end;
End;

Procedure TProcUseMatr.FillChains ( Const ATarg : string );
Var
  BLineIdx      : Integer;
  BFullMask     : string;
  BUsage        : char;
  BVarUsage     : string;
  BMergeIdxA,
  BMergeIdxB    : Integer;
  BUsageA,
  BUsageB,
  BUsageM       : string;
  BIndex        : Integer;
  BMerged       : boolean;
Begin
 FTmpList.Clear;
 BFullMask:=FInitMask;
 BLineIdx:=0;
 while BLineIdx<Length(FFlow) do
  begin
  repeat
  if CheckSetMask(BFullMask,BLineIdx) then break;
  BUsage:=FFlow[BLineIdx].CheckTargUsage(ATarg);
  if BUsage='.' then break;
  BVarUsage:=FInitMask;
  FillChainA(ATarg,BUsage,BLineIdx,BVarUsage);
  FTmpList.Append(BVarUsage);
  PropVarUsageToMask(BVarUsage,BFullMask);
  until TRUE;
  inc(BLineIdx);
  end;
 {
  Some "strange" programming styles result in chain overlapping.
  Example:
  for BIndex:=0 to 3 do
   begin
   if AData=0 then BIndex:=2
   else BIndex=0;
   end;

  we then need to merge these chains
 }
 repeat
 BMerged:=FALSE;
 BMergeIdxA:=0;
 while BMergeIdxA<FTmpList.Count-1 do
  begin
  BUsageA:=FTmpList.Strings[BMergeIdxA];
  BMergeIdxB:=BMergeIdxA+1;
  while BMergeIdxB<FTmpList.Count do
   begin
   BUsageB:=FTmpList.Strings[BMergeIdxB];
   if IsUsageOverlap(BUsageA,BUsageB,BUsageM) then
    begin
    FTmpList.Strings[BMergeIdxA]:=BUsageM;
    FTmpList.Delete(BMergeIdxB);
    BMerged:=TRUE;
    break;
    end;
   inc(BMergeIdxB);
   end;
  if BMerged then break;
  inc(BMergeIdxA);
  end;
 if BMerged=FALSE then break;
 until FALSE;

 for BIndex:=0 to FTmpList.Count-1 do
  begin
  FChTargList.Append(ATarg);
  FChUsageList.Append(FTmpList.Strings[BIndex]);
  end;
End;

Procedure TProcUseMatr.WrForceCmd ( ALineIdx, AChainIdx : Integer; Const ACmd : char );
Var
  BIndex        : Integer;
Begin
 BIndex:=Length(FForceCmdList);
 SetLength(FForceCmdList,BIndex+1);
 with FForceCmdList[BIndex] do
  begin
  FCodeIdx:=ALineIdx;
  FChainIdx:=AChainIdx;
  FCmd:=ACmd;
  end;
End;

Procedure TProcUseMatr.WarnUsage ( Var ADirty : boolean );
Var
  BChainIdx,
  BCodeIdx,
  BCodeIdxA     : Integer;
  BTarg,
  BUsageStr     : string;
  BSpec,
  BName         : string;
  BUsageA       : string;
  BIndexA,
  BIndexB       : Integer;
  BLineA        : TFlowLine;
  BInsertPos    : Integer;
  BForceCmd     : TForceCmd;

Begin
 FForceCmdList:=nil;

 repeat
 BCodeIdx:=0;
 BTarg:='';
 for BChainIdx:=0 to FChTargList.Count-1 do
  begin
  BTarg:=FChTargList.Strings[BChainIdx]; BSpec:=ParsExtractSpec(BTarg);
  BUsageStr:=FChUsageList.Strings[BChainIdx];
  BCodeIdx:=1; // Start from 1, because 1st line is nop
  while BCodeIdx<Length(FFlow)-1 do // Go till x-1, because last line is nop
   begin
   BLineA:=FFlow[BCodeIdx];
   repeat
   // S part
   BUsageA:=Copy(BUsageStr,1+BCodeIdx-1,2);
   if (BUsageA='.s') or (BUsageA='.x') then begin WrForceCmd(BCodeIdx,BChainIdx,'l'); break; end;
   if BLineA.IsLabel and (BUsageA[2]<>'.') then
    begin
    if (FFlow[BCodeIdx-1].IsJmp=FALSE) and (BUsageA[1]='.') then begin WrForceCmd(BCodeIdx,BChainIdx,'l'); break; end; // BCodeIdx stays the same, i.e. insert before label
    BCodeIdxA:=1;
    while BCodeIdxA<Length(FFlow) do // Check all Jmps to BLineA
     begin
     if (BUsageStr[1+BCodeIdxA]='.') and FFlow[BCodeIdxA].IsJxxTo(BLineA.LabelName) then begin WrForceCmd(BCodeIdxA,BChainIdx,'l'); break; end; // Change CodeIdx
     inc(BCodeIdxA);
     end;
    end;
   // D part
   BUsageA:=Copy(BUsageStr,1+BCodeIdx,2);
   if (BUsageA='d.') or (BUsageA='x.') then begin WrForceCmd(BCodeIdx,BChainIdx,'s'); break; end;
   if BLineA.IsJxx and IsSpecOut(BSpec) and (BUsageA[1]<>'.') then
    begin
    BCodeIdxA:=FindLabelIdx(BLineA.Param[0].Targ);
    if BCodeIdxA<0 then begin AppendError('Internal error: JmpLabel not found [r:TProcUseMatr.WarnUsage]'); break; end;
    if BUsageStr[1+BCodeIdxA]='.' then begin WrForceCmd(BCodeIdxA,BChainIdx,'s'); break; end;
    if (BLineA.IsJmp=FALSE) and (BUsageA[2]='.') then begin WrForceCmd(BCodeIdx,BChainIdx,'s'); break; end;
    end;
   until TRUE;
   inc(BCodeIdx);
   end;
  end;

 if Length(FForceCmdList)=0 then break;
 ADirty:=TRUE;

 BInsertPos:=0;
 BIndexA:=0;
 while BIndexA<Length(FForceCmdList) do
  begin
  BForceCmd:=FForceCmdList[BIndexA];
  BLineA:=FFlow[BForceCmd.FCodeIdx];
  BTarg:=FChTargList.Strings[BForceCmd.FChainIdx];
  BSpec:=ParsExtractSpec(BTarg); BName:=ParsExtractName(BTarg);
  case BForceCmd.FCmd of
   'l': begin
        BInsertPos:=BForceCmd.FCodeIdx;
        if IsSpecParam(BSpec) then InsertFlowLine(FFlow,BInsertPos,'ldp_s '+BTarg+' ;'+BLineA.Tail+' [A:TProcUseMatr.WarnUsage]')
        else
         begin
         AppendError('w',BLineA,'Variable '+BName+' can be used uninitialized [R:TProcUseMatr.WarnUsage]');
         InsertFlowLine(FFlow,BInsertPos,'ldp_z '+BTarg+' ;'+BLineA.Tail+' [A:TProcUseMatr.WarnUsage]');
         end;
        end;
   's': begin
        BInsertPos:=BForceCmd.FCodeIdx+1;
        if IsSpecOut(BSpec) then InsertFlowLine(FFlow,BInsertPos,'stp_s '+BTarg+' ;'+BLineA.Tail+' [A:TProcUseMatr.WarnUsage]')
        else
         begin
         AppendError('w',BLineA,'Value assigned to '+BName+' is unused [R:TProcUseMatr.WarnUsage]');
         InsertFlowLine(FFlow,BInsertPos,'stp_nil '+BTarg+' ;'+BLineA.Tail+' [A:TProcUseMatr.WarnUsage]');
         end;
        end;
   end; // case
  inc(BIndexA);
  BIndexB:=BIndexA;
  while BIndexB<Length(FForceCmdList) do
   begin
   with FForceCmdList[BIndexB] do
    begin
    if FCodeIdx>=BInsertPos then inc(FCodeIdx);
    end;
   inc(BIndexB);
   end;
  end;
 until TRUE;
End;

Procedure TProcUseMatr.BuildUsageMatr;
Var
  BRowIdx,
  BColIdx       : Integer;
  BUsage        : string;
Begin
 SetLength(FUsageMatr,Length(FFlow),FChTargList.Count);
 BColIdx:=0;
 while BColIdx<FChTargList.Count do
  begin
  BUsage:=FChUsageList.Strings[BColIdx];
  BRowIdx:=0;
  while BRowIdx<Length(FFlow) do
   begin
   FUsageMatr[BRowIdx,BColIdx]:=BUsage[1+BRowIdx];
   inc(BRowIdx);
   end;
  inc(BColIdx);
  end;
End;

Procedure TProcUseMatr.DbgUpdateUseMatr;
Var
  BLineIdx      : Integer;
  BLine         : TFlowLine;
  BUseLine      : string;
  BTargIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlow)-1 do
  begin
  BUseLine:='';
  for BTargIdx:=0 to FChTargList.Count-1 do BUseLine:=BUseLine+FUsageMatr[BLineIdx,BTargIdx];
  BLine:=FFlow[BLineIdx];
  BLine.TailMarkDel('UseMatr');
  BLine.TailMarkAdd('[UseMatr:'+BUseLine+']');
  end;
End;

end.

