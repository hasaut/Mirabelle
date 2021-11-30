unit LlvmChains_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, LlvmLine_sd, LlvmFlow_sd;

Type
  TSplitVisi = (svNone, svSet, svExtra);

  TProcChains = class(TProcFlow)
  private
  protected
    // FillChain detects an entire chain. I.e. if moving up it finds 's' or 'x' it will mark it and will continue to search both mirror and UP
    Procedure FillChainA ( Const ATarg : string; AUsage : char; ALineIdx : Integer; Var AVarUsage : string );
    Procedure FillChainUp ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
    Procedure FillChainDn ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
    // FillSegm detects a segment only. Used by SplitChain. If when moving up it finds 's' or 'x' it will mark it and will only mirror
    Procedure FillSegmA ( Const ATarg : string; ALineIdx : Integer; Out AVarUsage : string );
    Procedure FillSegmUp ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
    Procedure FillSegmDn ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );

    Function IsExtraVisi ( Const AVisiCmp, AVisiNew : string; Const ACondition : string ) : boolean;

    Procedure GetVisibleUp ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
    Function GetVisibleUp ( ATextIdx : Integer; Const ATargChain : string ) : string;
    Procedure GetVisibleDn ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
    Function GetVisibleDn ( ATextIdx : Integer; Const ATargChain : string ) : string;
    Procedure GetDirtyUp ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
    Function GetDirtyUp ( ATextIdx : Integer; Const ATargChain : string ) : string;

    Function MoveIdxUp ( ATextIdx : Integer; Const ATargChain : string; Const AVisibleDn : string; Var AUsePos : string; Var AMask : string ) : TSplitVisi;
    Function MoveIdxDn ( ATextIdx : Integer; Const ATargChain : string; Const AVisibleUp : string; Var AUsePos : string; Var AMask : string ) : TSplitVisi;
    Function FindSplitUpDn ( ATextIdx : Integer; Const ATargChain : string; Const AVisibleUp, AVisibleDn : string ) : string;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

Procedure PropTmpToVarUsage ( Const AMaskTmp : string; Var AVarUsage : string );

implementation

Procedure PropTmpToVarUsage ( Const AMaskTmp : string; Var AVarUsage : string );
Var
  BIndex        : Integer;
Begin
 for BIndex:=1 to Length(AMaskTmp) do
  begin
  if AMaskTmp[BIndex]<>'.' then AVarUsage[BIndex]:=AMaskTmp[BIndex];
  end;
End;

Constructor TProcChains.Create;
Begin
 Inherited;
End;

Destructor TProcChains.Destroy;
Begin
 Inherited;
End;

Procedure TProcChains.FillChainA ( Const ATarg : string; AUsage : char; ALineIdx : Integer; var AVarUsage : string );
Var
  BVarUsage     : string;
  BMaskUp,
  BMaskDn       : string;
Begin
 BVarUsage:=AVarUsage; CheckSetMask(BVarUsage,ALineIdx,AUsage);
 repeat
 BMaskUp:=GetInitMask; BMaskDn:=GetInitMask;
 if AUsage<>'d' then FillChainUp(BVarUsage,ATarg,ALineIdx-1,BMaskUp,BMaskDn,GetInitMask);
 FillChainDn(BVarUsage,ATarg,ALineIdx+1,BMaskUp,BMaskDn,GetInitMask);
 if BVarUsage=AVarUsage then break;
 AVarUsage:=BVarUsage;
 until FALSE;
End;

Procedure TProcChains.FillChainUp ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
Var
  BMaskTmp      : string;
  BLineIdx,
  BLineIdxA     : Integer;
  BLine         : TFlowLine;
  BUsage        : char;
Begin
 BMaskTmp:=AMaskTmp;
 BLineIdx:=ALineIdx;
 while BLineIdx>=0 do
  begin
  if CheckSetMask(AMaskUp,BLineIdx) then
   begin
   if GetMask(AVarUsage,BLineIdx)<>'.' then PropTmpToVarUsage(BMaskTmp,AVarUsage);
   break;
   end;
  BLine:=FFlow[BLineIdx];
  BUsage:=BLine.CheckTargUsage(ATarg);
  if BUsage='.' then CheckSetMask(BMaskTmp,BLineIdx,'|')
  else
   begin
   CheckSetMask(BMaskTmp,BLineIdx,BUsage);
   PropTmpToVarUsage(BMaskTmp,AVarUsage);
   if BUsage<>'d' then FillChainUp(AVarUsage,ATarg,BLineIdx-1,AMaskUp,AMaskDn,BMaskTmp);
   FillChainDn(AVarUsage,ATarg,BLineIdx+1,AMaskUp,AMaskDn,BMaskTmp);
   break;
   end;
  if BLine.IsLabel then
   begin
   for BLineIdxA:=0 to Length(FFlow)-1 do
    begin
    if FFlow[BLineIdxA].IsJxxTo(BLine.LabelName) then FillChainUp(AVarUsage,ATarg,BLineIdxA,AMaskUp,AMaskDn,BMaskTmp);
    end;
   if BLineIdx=0 then break;
   if FFlow[BLineIdx-1].IsJmp then break;
   end;
  dec(BLineIdx);
  end;
End;

Procedure TProcChains.FillChainDn ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
Var
  BMaskTmp      : string;
  BLineIdx      : Integer;
  BLine         : TFlowLine;
  BUsage        : char;
Begin
 BMaskTmp:=AMaskTmp;
 BLineIdx:=ALineIdx;
 while BLineIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BLineIdx];
  if CheckSetMask(AMaskDn,BLineIdx) then
   begin
   BUsage:=GetMask(AVarUsage,BLineIdx);
   if (BUsage<>'.') and (BUsage<>'d') then PropTmpToVarUsage(BMaskTmp,AVarUsage);
   break;
   end;
  BUsage:=BLine.CheckTargUsage(ATarg);
  if BUsage='.' then CheckSetMask(BMaskTmp,BLineIdx,'|')
  else if BUsage='d' then break
  else
   begin
   CheckSetMask(BMaskTmp,BLineIdx,BUsage);
   PropTmpToVarUsage(BMaskTmp,AVarUsage);
   FillChainUp(AVarUsage,ATarg,BLineIdx-1,AMaskUp,AMaskDn,BMaskTmp);
   FillChainDn(AVarUsage,ATarg,BLineIdx+1,AMaskUp,AMaskDn,BMaskTmp);
   break;
   end;
  if BLine.IsJxx then
   begin
   FillChainDn(AVarUsage,ATarg,FindLabelIdx(BLine.Param[0].Targ),AMaskUp,AMaskDn,BMaskTmp);
   if BLine.IsJmp then break;
   end;
  inc(BLineIdx);
  end;
End;

Procedure TProcChains.FillSegmA ( Const ATarg : string; ALineIdx : Integer; Out AVarUsage : string );
Var
  BVarUsage     : string;
  BMaskUp,
  BMaskDn       : string;
Begin
 BVarUsage:=GetInitMask; CheckSetMask(BVarUsage,ALineIdx,'|');
 BMaskUp:=GetInitMask; BMaskDn:=GetInitMask;
 FillSegmUp(BVarUsage,ATarg,ALineIdx-1,BMaskUp,BMaskDn,GetInitMask);
 FillSegmDn(BVarUsage,ATarg,ALineIdx+1,BMaskUp,BMaskDn,GetInitMask);
 AVarUsage:=BVarUsage;
End;

Procedure TProcChains.FillSegmUp ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
Var
  BMaskTmp      : string;
  BLineIdx,
  BLineIdxA     : Integer;
  BLine         : TFlowLine;
  BUsage        : char;
Begin
 BMaskTmp:=AMaskTmp;
 BLineIdx:=ALineIdx;
 while BLineIdx>=0 do
  begin
  if CheckSetMask(AMaskUp,BLineIdx) then
   begin
   if GetMask(AVarUsage,BLineIdx)<>'.' then PropTmpToVarUsage(BMaskTmp,AVarUsage);
   break;
   end;
  BLine:=FFlow[BLineIdx];
  BUsage:=BLine.CheckTargUsage(ATarg);
  if BUsage='.' then CheckSetMask(BMaskTmp,BLineIdx,'|')
  else
   begin
   CheckSetMask(BMaskTmp,BLineIdx,BUsage);
   PropTmpToVarUsage(BMaskTmp,AVarUsage);
   FillSegmDn(AVarUsage,ATarg,BLineIdx+1,AMaskUp,AMaskDn,BMaskTmp);
   break;
   end;
  if BLine.IsLabel then
   begin
   for BLineIdxA:=0 to Length(FFlow)-1 do
    begin
    if FFlow[BLineIdxA].IsJxxTo(BLine.LabelName) then FillSegmUp(AVarUsage,ATarg,BLineIdxA,AMaskUp,AMaskDn,BMaskTmp);
    end;
   if BLineIdx=0 then break;
   if FFlow[BLineIdx-1].IsJmp then break;
   end;
  dec(BLineIdx);
  end;
End;

Procedure TProcChains.FillSegmDn ( Var AVarUsage : string; Const ATarg : string; ALineIdx : Integer; Var AMaskUp, AMaskDn : string; Const AMaskTmp : string );
Var
  BMaskTmp      : string;
  BLineIdx      : Integer;
  BLine         : TFlowLine;
  BUsage        : char;
Begin
 BMaskTmp:=AMaskTmp;
 BLineIdx:=ALineIdx;
 while BLineIdx<Length(FFlow) do
  begin
  BLine:=FFlow[BLineIdx];
  if CheckSetMask(AMaskDn,BLineIdx) then
   begin
   BUsage:=GetMask(AVarUsage,BLineIdx);
   if (BUsage<>'.') and (BUsage<>'d') then PropTmpToVarUsage(BMaskTmp,AVarUsage);
   break;
   end;
  BUsage:=BLine.CheckTargUsage(ATarg);
  if BUsage='.' then CheckSetMask(BMaskTmp,BLineIdx,'|')
  else if BUsage='d' then break
  else
   begin
   CheckSetMask(BMaskTmp,BLineIdx,BUsage);
   PropTmpToVarUsage(BMaskTmp,AVarUsage);
   FillSegmUp(AVarUsage,ATarg,BLineIdx-1,AMaskUp,AMaskDn,BMaskTmp);
   break;
   end;
  if BLine.IsJxx then
   begin
   FillSegmDn(AVarUsage,ATarg,FindLabelIdx(BLine.Param[0].Targ),AMaskUp,AMaskDn,BMaskTmp);
   if BLine.IsJmp then break;
   end;
  inc(BLineIdx);
  end;
End;

Function TProcChains.IsExtraVisi ( Const AVisiCmp, AVisiNew : string; Const ACondition : string ) : boolean;
Var
  BIndex        : Integer;
  BVisiCmp,
  BVisiNew      : char;
Begin
 BIndex:=0;
 while BIndex<Length(AVisiCmp) do
  begin
  BVisiCmp:=AVisiCmp[BIndex+1];
  BVisiNew:=AVisiNew[BIndex+1];
  if (Pos(BVisiNew,ACondition)<>0) and (BVisiNew<>BVisiCmp) then break;
  inc(BIndex);
  end;
 Result:=BIndex<Length(AVisiCmp);
End;

Procedure TProcChains.GetVisibleUp ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
Var
  BTextIdx      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
Begin
 BTextIdx:=ATextIdx;
 while BTextIdx>=0 do
  begin
  BUsage:=GetMask(ATargChain,BTextIdx);
  if BUsage='.' then break;
  if CheckSetMask(AUsage,BTextIdx,BUsage) then break;
  if (BUsage<>'|') then break;
  BLine:=FFlow[BTextIdx];
  if BLine.IsLabel then
   begin
   BTextIdxA:=0;
   while BTextIdxA<Length(FFlow) do
    begin
    if FFlow[BTextIdxA].IsJxxTo(BLine.LabelName) then GetVisibleUp(BTextIdxA,ATargChain,AUsage);
    inc(BTextIdxA);
    end;
   if BTextIdx=0 then break;
   if FFlow[BTextIdx-1].IsJmp then break;
   end;
  dec(BTextIdx);
  end;
End;

Procedure TProcChains.GetDirtyUp ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
Var
  BTextIdx      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
Begin
 BTextIdx:=ATextIdx;
 while BTextIdx>=0 do
  begin
  BUsage:=GetMask(ATargChain,BTextIdx);
  if BUsage='.' then break;
  if CheckSetMask(AUsage,BTextIdx,BUsage) then break;
  if BUsage in ['d', 'x'] then break;
  BLine:=FFlow[BTextIdx];
  if BLine.IsLabel then
   begin
   BTextIdxA:=0;
   while BTextIdxA<Length(FFlow) do
    begin
    if FFlow[BTextIdxA].IsJxxTo(BLine.LabelName) then GetDirtyUp(BTextIdxA,ATargChain,AUsage);
    inc(BTextIdxA);
    end;
   if BTextIdx=0 then break;
   if FFlow[BTextIdx-1].IsJmp then break;
   end;
  dec(BTextIdx);
  end;
End;

Procedure TProcChains.GetVisibleDn ( ATextIdx : Integer; Const ATargChain : string; Var AUsage : string );
Var
  BTextIdx      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
Begin
 BTextIdx:=ATextIdx;
 while BTextIdx<Length(FFlow) do
  begin
  BUsage:=GetMask(ATargChain,BTextIdx);
  if BUsage='.' then break;
  if CheckSetMask(AUsage,BTextIdx,BUsage) then break;
  if BUsage in ['s', 'x'] then break;
  BLine:=FFlow[BTextIdx];
  if BLine.IsJmp then BTextIdx:=FindLabelIdx(BLine.Param[0].Targ)
  else
   begin
   if BLine.IsJxx then GetVisibleDn(FindLabelIdx(BLine.Param[0].Targ),ATargChain,AUsage);
   inc(BTextIdx);
   end;
  end;
End;

Function TProcChains.GetVisibleUp ( ATextIdx : Integer; Const ATargChain : string ) : string;
Begin
 Result:=GetInitMask;
 GetVisibleUp(ATextIdx,ATargChain,Result);
End;

Function TProcChains.GetDirtyUp ( ATextIdx : Integer; Const ATargChain : string ) : string;
Begin
 Result:=GetInitMask;
 GetDirtyUp(ATextIdx,ATargChain,Result);
End;

Function TProcChains.GetVisibleDn ( ATextIdx : Integer; Const ATargChain : string ) : string;
Begin
 Result:=GetInitMask;
 GetVisibleDn(ATextIdx,ATargChain,Result);
End;

Function TProcChains.MoveIdxUp ( ATextIdx : Integer; Const ATargChain : string; Const AVisibleDn : string; Var AUsePos : string; Var AMask : string ) : TSplitVisi;
Var
  BTextIdx      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
  BUsePos       : string;
  BVisiNew      : string;
Begin
 Result:=svNone;
 BTextIdx:=ATextIdx;
 while BTextIdx>=0 do
  begin
  if CheckSetMask(AMask,BTextIdx) then break;
  BUsage:=GetMask(ATargChain,BTextIdx);
  if BUsage='.' then begin Result:=svNone; break; end;
  if BUsage in ['s', 'd', 'x'] then begin CheckSetMask(AUsePos,BTextIdx,'u'); Result:=svSet; break; end;
  BLine:=FFlow[BTextIdx];
  if BLine.IsLabel then
   begin
   BUsePos:=AUsePos;
   BTextIdxA:=0;
   while BTextIdxA<Length(FFlow) do
    begin
    if FFlow[BTextIdxA].IsJxxTo(BLine.LabelName) then
     begin
     if (MoveIdxUp(BTextIdxA,ATargChain,AVisibleDn,BUsePos,AMask)=svExtra) and (GetMask(BUsePos,BTextIdxA)='u') then break;
     end;
    inc(BTextIdxA);
    end;
   if BTextIdxA<Length(FFlow) then begin CheckSetMask(AUsePos,BTextIdx,'u'); Result:=svSet; break; end;
   AUsePos:=BUsePos;
   if BTextIdx>0 then
    begin
    if FFlow[BTextIdx-1].IsJmp=FALSE then MoveIdxUp(BTextIdx-1,ATargChain,AVisibleDn,AUsePos,AMask);
    end;
   Result:=svSet;
   break;
   end
  //else if IsJmp(BCmd) then begin Result:=svNone; break; end
  else if BLine.IsJxx then
   begin
   BVisiNew:=GetVisibleDn(BTextIdx,ATargChain);
   if IsExtraVisi(AVisibleDn,BVisiNew,'sx') then begin CheckSetMask(AUsePos,BTextIdx,'u'); Result:=svExtra; break; end;
   end;
  dec(BTextIdx);
  end;
End;

{
Function TProcChains.MoveIdxUpA ( AIdxThis : Integer; Const ATargChain : string; Const AVisibleDn : string; Var AUsePos : Integer; Var AMask : string ) : TSplitVisi;
Var
  BIdxTail,
  BIdxPrev,
  BIdxNext,
  BIdxThis      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
Begin
 Result:=svNone;
 BIdxTail:=-1;
 BIdxPrev:=AIdxThis; BIdxThis:=AIdxThis-1;
 while BIdxThis>0 do
  begin
  if CheckSetMask(AMask,BIdxThis) then break;
  BUsage:=GetMask(ATargChain,BIdxThis);
  if BUsage='.' then begin Result:=svNone; break; end;
  if BUsage in ['s', 'd', 'x'] then begin AUsePos:=BIdxPrev; Result:=svSet; break; end;
  BLine:=FFlow[BIdxThis];
  if BLine.IsLabel then // we can only move UP if there is only one path. Otherwise we stop
   begin
   BIdxNext:=-1;
   if (FFlow[BIdxThis-1].IsJmp=FALSE) and (GetMask(ATargChain,BIdxThis-1)<>'.') then BIdxNext:=BIdxThis-1;
   BTextIdxA:=0;
   while BTextIdxA<Length(FFlow) do
    begin
    if FFlow[BTextIdxA].IsJxxTo(BLine.LabelName) and (GetMask(ATargChain,BTextIdxA)<>'.') then
     begin
     if BIdxNext<>-1 then break;
     BIdxNext:=BTextIdxA;
     end;
    inc(BTextIdxA);
    end;
   if BTextIdxA<Length(FFlow) then begin AUsePos:=BIdxPrev; Result:=svSet; break; end;
   if BIdxNext=-1 then begin AppendError('Internal error: All branches are invalid [R:TProcChains.MoveIdxUp]'); Result:=svNone; break end;
   BIdxTail:=BIdxPrev;
   BIdxPrev:=BIdxThis;
   BIdxThis:=BIdxNext;
   end
  else if BLine.IsJmp then begin AppendError('Internal error: Must not see JMP [R:TProcChains.MoveIdxUp]'); Result:=svNone; break end
  else if BLine.IsJxx and (GetMask(ATargChain,FindLabelIdx(BLine.Param[0].Targ)) in ['|', 's', 'x']) then
   begin
   if BIdxPrev=(BIdxThis+1) then AUsePos:=BIdxPrev else AUsePos:=BIdxTail;
   Result:=svSet;
   break;
   end
  else
   begin
   BIdxTail:=BIdxPrev;
   BIdxPrev:=BIdxThis;
   dec(BIdxThis);
   end;
  end;
End;}

Function TProcChains.MoveIdxDn ( ATextIdx : Integer; Const ATargChain : String; Const AVisibleUp : string; Var AUsePos : string; Var AMask : string ) : TSplitVisi;
Var
  BTextIdx      : Integer;
  BUsage        : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
  BTextIdxB     : Integer;
  BUsePos       : string;
Begin
 Result:=svNone;
 BTextIdx:=ATextIdx;
 while BTextIdx<Length(FFlow) do
  begin
  if CheckSetMask(AMask,BTextIdx) then break;
  BUsage:=GetMask(ATargChain,BTextIdx);
  if BUsage='.' then begin Result:=svNone; break; end;
  if BUsage in ['s', 'x'] then begin CheckSetMask(AUsePos,BTextIdx,'o'); Result:=svSet; break; end;
  BLine:=FFlow[BTextIdx];
  if BLine.IsJmp then
   begin
   BTextIdxA:=FindLabelIdx(BLine.Param[0].Targ);
   BUsePos:=AUsePos;
   if (MoveIdxDn(BTextIdxA,ATargChain,AVisibleUp,BUsePos,AMask)=svExtra) and (GetMask(BUsePos,BTextIdxA)='o') then CheckSetMask(AUsePos,BTextIdx,'o')
   else AUsePos:=BUsePos;
   Result:=svSet;
   break;
   end
  else if BLine.IsJxx then
   begin
   BTextIdxA:=FindLabelIdx(BLine.Param[0].Targ); BTextIdxB:=BTextIdx+1;
   BUsePos:=AUsePos;
   if ((MoveIdxDn(BTextIdxA,ATargChain,AVisibleUp,BUsePos,AMask)=svExtra) and (GetMask(BUsePos,BTextIdxA)='o')) or
      ((MoveIdxDn(BTextIdxB,ATargChain,AVisibleUp,BUsePos,AMask)=svExtra) and (GetMask(BUsePos,BTextIdxB)='o')) then CheckSetMask(AUsePos,BTextIdx,'o')
   else AUsePos:=BUsePos;
   Result:=svSet;
   break;
   end
  else if BLine.IsLabel then
   begin
   if IsExtraVisi(AVisibleUp,GetDirtyUp(BTextIdx,ATargChain),'dx') then begin CheckSetMask(AUsePos,BTextIdx,'o'); Result:=svExtra; break; end;
   inc(BTextIdx);
   end
  else inc(BTextIdx);
  end;
End;

{
Function TProcChains.MoveIdxDnA ( AIdxThis : Integer; Const ATargChain : String; Const AVisibleUp : string; Var AUsePos : Integer; Var AMask : string ) : TSplitVisi;
Var
  BIdxPrev,
  BIdxThis      : Integer;
  BUsage        : char;
  BUsageA,
  BUsageB       : char;
  BLine         : TFlowLine;
  BTextIdxA     : Integer;
  BTextIdxB     : Integer;
Begin
 Result:=svNone;
 BIdxPrev:=AIdxThis; BIdxThis:=AIdxThis+1;
 while BIdxThis<Length(FFlow) do
  begin
  if CheckSetMask(AMask,BIdxThis) then break;
  BUsage:=GetMask(ATargChain,BIdxThis);
  if BUsage='.' then begin Result:=svNone; break; end;
  if BUsage in ['s', 'x'] then begin AUsePos:=BIdxThis; Result:=svSet; break; end;
  BLine:=FFlow[BIdxThis];
  if BLine.IsJmp then begin BIdxPrev:=BIdxThis; BIdxThis:=FindLabelIdx(BLine.Param[0].Targ); end // just continue our way down
  else if BLine.IsJxx then // If both branches of Jxx are not '.', then Jxx is an obstacle and we do not move down anymore
   begin
   BTextIdxA:=FindLabelIdx(BLine.Param[0].Targ); BTextIdxB:=BIdxThis+1;
   BUsageA:=GetMask(ATargChain,BTextIdxA); BUsageB:=GetMask(ATargChain,BTextIdxB);
   if (BUsageA in ['.', 'd']) and (BUsageB in ['.', 'd']) then begin AppendError('Internal error: Both branches are invalid [R:TProcChains.MoveIdxDn]'); Result:=svNone; break end;
   if (not (BUsageA in ['.', 'd'])) and (not (BUsageB in ['.', 'd'])) then begin AUsePos:=BIdxThis; Result:=svSet; break; end;
   BIdxPrev:=BIdxThis;
   if (BUsageA in ['.', 'd']) then BIdxThis:=BTextIdxB else BIdxThis:=BTextIdxA;
   end
  else if BLine.IsLabel and IsExtraVisi(AVisibleUp,GetDirtyUp(BIdxThis,ATargChain),'dx') then
   begin
   if FFlow[BIdxPrev].IsJmp then AUsePos:=BIdxPrev else AUsePos:=BIdxThis;
   Result:=svSet;
   break;
   end
  else
   begin
   BIdxPrev:=BIdxThis;
   inc(BIdxThis);
   end;
  end;
End;}

Function TProcChains.FindSplitUpDn ( ATextIdx : Integer; Const ATargChain : string; Const AVisibleUp, AVisibleDn : string ) : string;
Var
  BMaskUp,
  BMaskDn       : string;
  BResultA,
  BResultB      : TSplitVisi;
Begin
 Result:=GetInitMask;
 BMaskUp:=GetInitMask; BMaskDn:=GetInitMask;
 BResultA:=MoveIdxUp(ATextIdx-1,ATargChain,AVisibleDn,Result,BMaskUp);
 BResultB:=MoveIdxDn(ATextIdx+1,ATargChain,AVisibleUp,Result,BMaskDn);
 if (BResultA<>svSet) and (BResultA<>svExtra) then begin AppendError('Internal error: Order expected result is svSet [R:TProcChains.OrderChain]'); end;
 if (BResultB<>svSet) and (BResultB<>svExtra) then begin AppendError('Internal error: Order expected result is svSet [R:TProcChains.OrderChain]'); end;
End;

end.

