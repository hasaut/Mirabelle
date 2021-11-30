unit LlvmSplit_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, LlvmLine_sd, LlvmUseMatr_sd;

Type
  TChWeights = array of array of Single;

  TProcSplit = class(TProcUseMatr)
  private
    FSplitIdx   : Integer;

    Procedure FillWeightsDn ( ATextIdx, ATargIdx : Integer; AWeight : Single; Const AMask : string );
    Procedure FillWeightsUp ( ATextIdx, ATargIdx : Integer; AWeight : Single; Const AMask : string );

    Function ImportTargChain ( ATargIdx : Integer ) : string;

    Procedure OptimizeLdsSts;

  protected
    FChWeights          : TChWeights;

    Procedure FillWeights;
    Procedure SplitChainA ( ATextIdx, ATargIdx : Integer ); // Old version. To be deleted?
    Procedure SplitChain ( ATextIdx, ATargIdx : Integer );

    Procedure RecompileWeightsOpti;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
  end;

implementation

Constructor TProcSplit.Create;
Begin
 Inherited;
End;

Destructor TProcSplit.Destroy;
Begin
 FChWeights:=nil;
 Inherited;
End;

Procedure TProcSplit.Compile;
Begin
 Inherited;

 FillWeights;
 TimeStamp('Split');
End;

Procedure TProcSplit.RecompileWeightsOpti; // FTextSrc must be valid
Begin
 FillWeights;
End;

Procedure TProcSplit.FillWeights;
Var
  BTextIdx,
  BTargIdx      : Integer;
  BUsage        : char;
Begin
 SetLength(FChWeights,Length(FFlow),FChTargList.Count);
 BTargIdx:=0;
 while BTargIdx<FChTargList.Count do
  begin
  BTextIdx:=1;
  while BTextIdx<Length(FFlow)-1 do
   begin
   BUsage:=FUsageMatr[BTextIdx,BTargIdx];
   if BUsage in ['d', 's', 'x'] then
    begin
    FChWeights[BTextIdx,BTargIdx]:=1;
    FillWeightsDn(BTextIdx+1,BTargIdx,0.5,FInitMask);
    FillWeightsUp(BTextIdx-1,BTargIdx,0.5,FInitMask);
    end;
   inc(BTextIdx);
   end;
  inc(BTargIdx);
  end;
End;

Procedure TProcSplit.FillWeightsDn ( ATextIdx, ATargIdx : Integer; AWeight : Single; Const AMask : string );
Var
  BTextIdx      : Integer;
  BMask         : string;
  BUsage        : char;
  BLine         : TFlowLine;
Begin
 BMask:=AMask;
 BTextIdx:=ATextIdx;
 while BTextIdx<Length(FFlow)-1 do
  begin
  BUsage:=FUsageMatr[BTextIdx,ATargIdx];
  if BUsage='.' then break;
  if CheckSetMask(BMask,BTextIdx) then break;
  if BUsage<>'|' then break;
  FChWeights[BTextIdx,ATargIdx]:=FChWeights[BTextIdx,ATargIdx]+AWeight;
  AWeight:=0.5*AWeight;
  BLine:=FFlow[BTextIdx];
  if BLine.IsJmp then BTextIdx:=FindLabelIdx(BLine.Param[0].Targ)
  else if BLine.IsJxx then
   begin
   FillWeightsDn(FindLabelIdx(BLine.Param[0].Targ),ATargIdx,AWeight,BMask);
   FillWeightsDn(BTextIdx+1,ATargIdx,AWeight,BMask);
   break;
   end
  else inc(BTextIdx);
  end;
End;

Procedure TProcSplit.FillWeightsUp ( ATextIdx, ATargIdx : Integer; AWeight : Single; Const AMask : string );
Var
  BTextIdx      : Integer;
  BMask         : string;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
Begin
 BMask:=AMask;
 BTextIdx:=ATextIdx;
 while BTextIdx>1 do
  begin
  BUsage:=FUsageMatr[BTextIdx,ATargIdx];
  if BUsage='.' then break;
  if CheckSetMask(BMask,BTextIdx) then break;
  if BUsage<>'|' then break;
  FChWeights[BTextIdx,ATargIdx]:=FChWeights[BTextIdx,ATargIdx]+AWeight;
  AWeight:=0.5*AWeight;
  BLine:=FFlow[BTextIdx];
  if BLine.IsLabel then
   begin
   BTextIdxA:=0;
   while BTextIdxA<Length(FFlow) do
    begin
    if FFlow[BTextIdxA].IsJxxTo(BLine.LabelName) then FillWeightsUp(BTextIdxA,ATargIdx,AWeight,BMask);
    inc(BTextIdxA);
    end;
   Dec(BTextIdx);
   if FFlow[BTextIdx].IsJmp=FALSE then FillWeightsUp(BTextIdx,ATargIdx,AWeight,BMask);
   break;
   end
  else Dec(BTextIdx);
  end;
End;

Function TProcSplit.ImportTargChain ( ATargIdx : Integer ) : string;
Var
  BTextIdx      : Integer;
Begin
 SetLength(Result,Length(FFlow));
 for BTextIdx:=0 to Length(FFlow)-1 do
  begin
  Result[1+BTextIdx]:=FUsageMatr[BTextIdx,ATargIdx];
  end;
End;

Procedure TProcSplit.SplitChainA ( ATextIdx, ATargIdx : Integer );
Var
  BVisibleDn,
  BVisibleUp    : string;
  BSplitPos     : string;
  BTextIdx      : Integer;
  BUsage        : char;
  BTargChain    : string;
  BLineA,
  BLineB        : TFlowLine;
Begin
 BTargChain:=ImportTargChain(ATargIdx);

 repeat
 if Length(FFlow)=0 then break;
 // Initial visibility
 BVisibleDn:=GetVisibleDn(ATextIdx,BTargChain);
 BVisibleUp:=GetVisibleUp(ATextIdx,BTargChain);
 if (GetMask(BVisibleDn,ATextIdx)<>'|') or (GetMask(BVisibleUp,ATextIdx)<>'|') then
  begin
  BUsage:='.';
  BTextIdx:=ATextIdx-1;
  while BTextIdx>1 do
   begin
   BUsage:=GetMask(BTargChain,BTextIdx);
   if BUsage in ['.', '|'] then break;
   dec(BTextIdx);
   end;
  if BUsage='.' then
   begin
   BTextIdx:=ATextIdx+1;
   while BTextIdx<Length(FFlow)-1 do
    begin
    BUsage:=GetMask(BTargChain,BTextIdx);
    if BUsage in ['.', '|'] then break;
    inc(BTextIdx);
    end;
   end;
  if BUsage='.' then
   begin
   AppendError('e','Split chain internal error: chain is not used [R:TProcSplit.SplitChain]');
   break;
   end;
  ATextIdx:=BTextIdx;
  BVisibleDn:=GetVisibleDn(ATextIdx,BTargChain);
  BVisibleUp:=GetVisibleUp(ATextIdx,BTargChain);
  end;
 BSplitPos:=FindSplitUpDn(ATextIdx,BTargChain,BVisibleUp,BVisibleDn);

 FTextDst.Clear;
 BTextIdx:=Length(FFlow)-1;
 while BTextIdx>=0 do
  begin
  BUsage:=GetMask(BSplitPos,BTextIdx);
  if BUsage in ['u', 'o'] then
   begin
   BLineA:=FFlow[BTextIdx];
   if BUsage='u' then BLineB:=InsertFlowLine(FFlow,BTextIdx+1,'sts '+FChTargList.Strings[ATargIdx]+' ;'+BLineA.Tail)
   else               BLineB:=InsertFlowLine(FFlow,BTextIdx,'lds '+FChTargList.Strings[ATargIdx]+' ;'+BLineA.Tail);
   BLineB.TailMarkAdd('[A:TProcSplit.SplitChain_'+IntToStr(FSplitIdx)+'_]'); inc(FSplitIdx);
   end;
  dec(BTextIdx);
  end;

 //FlowClearProcessed;
 //if (ATextIdx=74) and (ATargIdx=3) then
 // begin
 // MoveCmdUp('sts');
 // end;
 //RecompileChainsOpti;
 //DbgSave(Self,'QSplit_'+IntToHex(FSplitIdx,3),FFlow,TRUE,'SplitTextIdx: '+IntToStr(ATextIdx)+'; SplitTargIdx: '+IntToStr(ATargIdx)+'; SplitMark: '+BSplitPos);
 //inc(FSplitIdx);
 until TRUE;

 OptimizeLdsSts;
End;

Procedure TProcSplit.SplitChain ( ATextIdx, ATargIdx : Integer );
Var
  BTextIdx      : Integer;
  BUsageA,
  BUsageB       : char;
  BSegm         : string;
  BLineA,
  BLineB        : TFlowLine;
Begin
 FillSegmA(FChTargList.Strings[ATargIdx],ATextIdx,BSegm);

 repeat
 if Length(FFlow)=0 then break;
 if GetMask(BSegm,ATextIdx)<>'|' then
  begin
  AppendError('e','Split chain internal error. Line: "'+FFlow[ATextIdx].FormatExec+'", Targ: "'+FChTargList.Strings[ATargIdx]+'", Mask: "'+GetMask(BSegm,ATextIdx)+'" [R:TProcSplit.SplitChain]');
  break;
  end;

 BTextIdx:=Length(FFlow)-1;
 while BTextIdx>0 do
  begin
  BUsageA:=GetMask(BSegm,BTextIdx-1);
  BUsageB:=GetMask(BSegm,BTextIdx);
  BLineA:=FFlow[BTextIdx]; BLineB:=nil;
  if (BUsageA in ['s', 'd', 'x']) and (BUsageB='|') then BLineB:=InsertFlowLine(FFlow,BTextIdx,'sts '+FChTargList.Strings[ATargIdx]+' ;'+BLineA.Tail);
  if (BUsageA='|') and (BUsageB in ['s', 'd', 'x']) then BLineB:=InsertFlowLine(FFlow,BTextIdx,'lds '+FChTargList.Strings[ATargIdx]+' ;'+BLineA.Tail);
  if BLineB<>nil then BLineB.TailMarkAdd('[A:TProcSplit.SplitChain_'+IntToStr(FSplitIdx)+'_]'); inc(FSplitIdx);
  dec(BTextIdx);
  end;

 //FlowClearProcessed;
 //if (ATextIdx=74) and (ATargIdx=3) then
 // begin
 // MoveCmdUp('sts');
 // end;
 //RecompileChainsOpti;
 //DbgSave(Self,'QSplit_'+IntToHex(FSplitIdx,3),FFlow,TRUE,'SplitTextIdx: '+IntToStr(ATextIdx)+'; SplitTargIdx: '+IntToStr(ATargIdx)+'; SplitMark: '+BSegm);
 //inc(FSplitIdx);
 until TRUE;

 OptimizeLdsSts;
End;

// It happens sometimes that Lds:Sts pair with
Procedure TProcSplit.OptimizeLdsSts;
Var
  BTextIdx      : Integer;
  BLineA,
  BLineB        : TFlowLine;
Begin
 BLineA:=FFlow[0];
 BTextIdx:=1;
 while BTextIdx<Length(FFlow)-3 do
  begin
  BLineA:=FFlow[BTextIdx]; BLineB:=FFlow[BTextIdx+1];
  if BLineA.IsLdx and BLineB.IsStx and (BLineA.Param[0].Targ=BLineB.Param[0].Targ) then DeleteFlowLine(FFlow,BTextIdx,2)
  else Inc(BTextIdx);
  end;
End;

end.

