unit LlvmVars_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsBase_sd, ParsHelper_sd,
  LlvmLine_sd, LlvmDbg_sd, LlvmJmps_sd;

Type
  TProcVars = class(TProcJmps)
  private
    FTextVars   : TStringList;
    FSilentList : string;

    FGlobalRefs : string;

    Procedure CheckAppendGlobal ( Const ATarg : string );

    Procedure ProcessDiscard ( Var ADirty : boolean );
    Procedure OptiPushProcParams ( Var ADirty : boolean );
    //Procedure DelExtraMov1 ( Var ADirty : boolean );
    Procedure DelExtraMov2 ( Var ADirty : boolean );
    Procedure DelExtraMovX ( Var ADirty : boolean );
    Procedure DelExtraBaseRM ( Var ADirty : boolean );
    Procedure DelExtraBaseMR ( Var ADirty : boolean );
    Procedure DelMovSame ( Var ADirty : boolean );
    Procedure DelAddSubZero ( Var ADirty : boolean );
    Procedure DelMulDivOne ( Var ADirty : boolean );
    Procedure DelMovZeroAddSub ( Var ADirty : boolean );
    Procedure DelExtraLoad ( Var ADirty : boolean );
    Procedure CheckArrIdx ( Var ADirty : boolean );
    //Procedure OptiSymmetric2 ( Var ADirty : boolean );
    //Procedure OptiMovZX_Mov ( Var ADirty : boolean );
    //Procedure OptiMovDM_Mov ( Var ADirty : boolean );
    //Procedure OptiCallDiscard ( Var ADirty : boolean );
    Procedure DelExtraMov2Str ( Var ADirty : boolean );
    //Procedure OptiStrAdd ( Var ADirty : boolean );
    Procedure DelUnusedVars ( Var ADirty : boolean );
    //Procedure ProcessGlobalRefs;

    Function TargUsedAfter ( AIndexStart : Integer; Var AUsed : string; Const ATarg : string ) : boolean;
  protected
    // Following procedure is unused
    Function TargAsgndAlways ( AIndexStart : Integer; Var AUsed : string; Const ATarg : string ) : boolean;

    Procedure ClearSilent;
    Procedure AppendSilent ( Const ATarg : string );
    Function CheckSilent ( Const ATarg : string ) : boolean;

    Procedure OptiVarsA;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function IsTargUsed ( Const ATarg : string ) : boolean; Override;

    Procedure Compile; Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;

    property TextVars : TStringList read FTextVars;
  end;

implementation

Uses
  ConComL;

Constructor TProcVars.Create;
Begin
 Inherited;
 FTextVars:=TStringList.Create;
End;

Destructor TProcVars.Destroy;
Begin
 FTextVars.Free;
 Inherited;
End;

Procedure TProcVars.Compile;
Var
  BDirty        : boolean;
Begin
 Inherited;

 repeat
 if FFatalError then break;
 BDirty:=FALSE;
 ClearSilent;
 ProcessDiscard(BDirty);

 OptiVarsA;
 until TRUE;

 DbgMarkFlow('Vars');
 FlowListToStringList(FFlowList,FFlow);
 FTextVars.Assign(FFlowList);

 TimeStamp('Vars');
 if FModule.DbgPath<>'' then DbgSave(Self,'C_Vars',FFlowList,TRUE);
End;

Procedure TProcVars.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T Vars');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextVars.Count-1 do ADbg.Append(FTextVars.Strings[BLineIdx]);
End;

Procedure TProcVars.ClearSilent;
Begin
 FSilentList:='';
End;

Procedure TProcVars.AppendSilent ( Const ATarg : string );
Begin
 if CheckSilent(ATarg)=FALSE then FSilentList:=FSilentList+ATarg+' ';
End;

Function TProcVars.CheckSilent ( Const ATarg : string ) : boolean;
Begin
 Result:=Pos(ATarg,FSilentList)<>0;
End;

// call     ~de_:BTmp0# ~mh_:WDReset,#       ;~rm:31,8#
// mov      ~___:Discard# ~de_:BTmp0#        ;~rm:31,8#
Procedure TProcVars.ProcessDiscard ( Var ADirty : boolean );
Var
  BLineA        : TFlowLine;
  BIndexA,
  BIndexB       : Integer;
Begin
 BIndexA:=0;
 while BIndexA<Length(FFlow) do
  begin
  BLineA:=FFlow[BIndexA];
  if BLineA.IsMov and (BLineA.Param[0].Targ=CDiscardValue) then
   begin
   ADirty:=TRUE;
   for BIndexB:=0 to Length(FFlow)-1 do
    begin
    if BIndexB<>BIndexA then FFlow[BIndexB].DiscardTarg(BLineA.Param[1].Targ);
    end;
   DeleteFlowLine(FFlow,BIndexA);
   end
  else inc(BIndexA);
  end;
End;


Procedure TProcVars.OptiVarsA;
Var
  BDirty        : Boolean;
Begin
 repeat
 BDirty:=FALSE;
 //DbgSave(Self,'Vars',FFlow,TRUE);
 //OptiPushProcParams(BDirty);
 //DelExtraMov1(BDirty);      //DbgSave(Self,'Vars',FFlow,TRUE);
 DelExtraMov2(BDirty);        //DbgSave(Self,'Vars',FFlow,TRUE);
 DelExtraMovX(BDirty);        //DbgSave(Self,'Vars',FFlow,TRUE);
 DelExtraBaseRM(BDirty);      //DbgSave(Self,'Vars',FFlow,TRUE);
 DelExtraBaseMR(BDirty);      //DbgSave(Self,'Vars',FFlow,TRUE);
 DelMovSame(BDirty);          //DbgSave(Self,'Vars',FFlow,TRUE);
 DelAddSubZero(BDirty);
 DelMulDivOne(BDirty);
 DelMovZeroAddSub(BDirty);
 CheckArrIdx(BDirty);         //DbgSave(Self,'Vars',FFlow,TRUE);
 //DelExtraLoad(BDirty);
 //OptiSymmetric2(BDirty);    //DbgSave(Self,'Vars',FFlow,TRUE);
 //OptiMovZX_Mov(BDirty);     //DbgSave(Self,'Vars',FFlow,TRUE);
 //OptiMovDM_Mov(BDirty);     //DbgSave(Self,'Vars',FFlow,TRUE);
 //OptiCallDiscard(BDirty);
 DelExtraMov2Str(BDirty);     //DbgSave(Self,'Vars',FFlow,TRUE);
 DelUnusedVars(BDirty);       //DbgSave(Self,'Vars',FFlow,TRUE);
 until BDirty=FALSE;
End;

Function TProcVars.TargUsedAfter ( AIndexStart : Integer; Var AUsed : string; Const ATarg : string ) : boolean;
Var
  BIndex        : Integer;
  BLine         : TFlowLine;
  BUsage        : char;
Begin
 Result:=FALSE;
 BIndex:=AIndexStart;
 while BIndex<Length(FFlow) do
  begin
  if CheckSetMask(AUsed,BIndex) then break;
  BLine:=FFlow[BIndex];
  if BLine.IsJxx then
   begin
   Result:=TargUsedAfter(FindLabelIdx(BLine.Param[0].Targ),AUsed,ATarg);
   if Result then break;
   if BLine.IsJmp then break;
   Result:=TargUsedAfter(BIndex+1,AUsed,ATarg);
   break;
   end;
  BUsage:=BLine.CheckTargUsage(ATarg);
  if BUsage='d' then break;
  if BUsage in ['s', 'x'] then begin Result:=TRUE; break; end;
  inc(BIndex);
  end;
End;

Function TProcVars.TargAsgndAlways ( AIndexStart : Integer; Var AUsed : string; Const ATarg : string ) : boolean;
Var
  BIndexA,
  BIndexB       : Integer;
  BLineA,
  BLineB        : TFlowLine;
Begin
 Result:=FALSE;
 BIndexA:=AIndexStart;
 while BIndexA>0 do
  begin
  if CheckSetMask(AUsed,BIndexA) then begin Result:=TRUE; break; end; // We expect it to be assigned if we have come to the starting point
  BLineA:=FFlow[BIndexA];
  if BLineA.IsJmp then break;
  if BLineA.IsLabel then
   begin
   BIndexB:=0;
   while BIndexB<Length(FFlow) do
    begin
    BLineB:=FFlow[BIndexB];
    if BLineB.IsJxxTo(BLineA.LabelName) then
     begin
     if TargAsgndAlways(BIndexB,AUsed,ATarg)=FALSE then break;
     end;
    inc(BIndexB);
    end;
   if BIndexB<Length(FFlow) then break;
   dec(BIndexA);
   if BIndexA<0 then break;
   BLineA:=FFlow[BIndexA];
   if BLineA.IsJmp then begin Result:=TRUE; break; end;
   Result:=TargAsgndAlways(BIndexA,AUsed,ATarg);
   break;
   end;
  if BLineA.IsJxx then
  else if BLineA.IsCall and (BLineA.Param[0].Targ=ATarg) then begin Result:=TRUE; break; end;
  dec(BIndexA);
  end;
End;

// mov      ~deq:BTmp1# ~deq:BTmp0#          ;~rm:19,16#
// Both BTmp1 and BTmp0 are used afterwards
{Procedure TProcVars.DelExtraMov1 ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB       : Integer;
  BFound        : boolean;
Begin
 repeat
 FTextDst.Clear;
 if Length(FFlow)<2 then break;
 BIndexA:=0;
 while BIndexA<Length(FFlow) do
  begin
  BLineA.RdLine(FTextSrc.Strings[BIndexA]);
  BFound:=FALSE;
  repeat
  if BLineA.Cmd<>'mov' then break;
  if ParsIsTmp(BLineA.Param[0].Targ)=FALSE then break;
  if ParsIsTmp(BLineA.Param[1].Targ)=FALSE then break;
  if ParsExtractType(BLineA.Param[0].Targ)<>ParsExtractType(BLineA.Param[1].Targ) then break;
  if BLineA.Param[0].Targ=BLineA.Param[1].Targ then break; // This case is done separately
  BFound:=TRUE
  until TRUE;
  if BFound then break;
  inc(BIndexA);
  end;
 if BIndexA=Length(FFlow) then break;

 ADirty:=TRUE;
 BIndexB:=0;
 while BIndexB<Length(FFlow) do
  begin
  BLineB.RdLine(FTextSrc.Strings[BIndexB]);
  BLineB.TryChange(BLineA.Param[0].Targ,BLineA.Param[1].Targ);
  FTextDst.Append(BLineB.Orig);
  inc(BIndexB);
  end;

 FTextSrc.Assign(FTextDst);
 until FALSE;
End;}

Procedure TProcVars.OptiPushProcParams ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLine         : TFlowLine;
  BLineA,
  BLineB        : TFlowLine;
  BTypeA,
  BTypeB        : string;
  BTargA,
  BTargB        : string;
  BFound        : boolean;
  BCmd          : string;
Begin
 BTypeA:=''; BTypeB:='';
 repeat
 if Length(FFlow)<2 then break;
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  BFound:=FALSE;
  repeat
  if FFlow[BIndex].IsPush=FALSE then break;
  BTargA:=BLine.Param[1].Targ;
  if ParsIsConst(BTargA) then break;
  BTypeA:=ParsExtractType(BTargA);
  BTypeB:=BestTypeSuitableForPush(BTypeA);
  if BTypeB=BTypeA then break;
  BFound:=TRUE;
  until TRUE;
  if BFound=FALSE then inc(BIndex)
  else
   begin
   ADirty:=TRUE;
   BTargB:=AppendTmpVar(BTypeB);
   BCmd:='mov_'+BTypeB+BTypeA;
   BLineA:=TFlowLine.Create; BLineA.RdLine(BCmd+' '+BTargB+' '+BTargA+' ; '+BLine.Tail);
   BLineB:=TFlowLine.Create; BLineB.RdLine(BLine.Cmd+' '+BLine.Param[0].Targ+' '+BTargB+' ; '+BLine.Tail);
   DeleteFlowLine(FFlow,BLine);
   InsertFlowLine(FFlow,BIndex,BLineB);
   InsertFlowLine(FFlow,BIndex,BLineA);
   inc(BIndex,2);
   //DbgSave(Self,'Vars',FFlow,TRUE);
   end;
  end;
 until TRUE;
End;

// mov      ~deb:BTmp7# ~dfb:FDataA#         ;~rm:12,23#
// mov      ~deb:BTmp2# ~deb:BTmp7#          ;~rm:12,23#
Procedure TProcVars.DelExtraMov2 ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA,
  BLineB        : TFlowLine;
  BUsed         : string;
  BFound        : boolean;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  BFound:=FALSE;
  repeat
  if BLineA.IsMov=FALSE then break;
  if BLineB.IsMov=FALSE then break;
  if BLineA.Param[0].Targ<>BLineB.Param[1].Targ then break;
  if BLineB.Param[0].Targ=BLineA.Param[1].Targ then break;
  if ParsIsGlobalOrExtern(BLineB.Param[0].Targ) then break;
  if ParsIsTmp(BLineB.Param[1].Targ)=FALSE then break;
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndex);
  if TargUsedAfter(BIndex+1,BUsed,BLineB.Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;
  if BFound then
   begin
   ADirty:=TRUE;
   BLineA.Param[0].Targ:=BLineB.Param[0].Targ;
   DeleteFlowLine(FFlow,BIndex);
   end
  else begin BLineA:=BLineB; inc(BIndex); end;
  end;
 until TRUE;
End;

// mov      ~deb:BTmp1# ~dbb:BLedG#          ;~rm:50,24#
// shl      ~deb:BTmp1# ~c_b:1#              ;~rm:50,24#
// add      ~deb:BTmp1# ~c_b:1#              ;~rm:50,24#
// mov      ~dbb:BLedG# ~deb:BTmp1#          ;~rm:50,24#
Procedure TProcVars.DelExtraMovX ( Var ADirty : boolean );
Var
  BLineS,
  BLineE        : TFlowLine;
  BIndexS,
  BIndexE,
  BIndexA       : Integer;
  BFound        : boolean;
  BUsed         : string;
Begin
 BIndexE:=0;
 while BIndexE<Length(FFlow) do
  begin
  BLineE:=FFlow[BIndexE];
  BFound:=FALSE;
  repeat
  if BLineE.Processed then break;
  if BLineE.IsMov=FALSE then break;
  if BLineE.Param[0].Targ=CDiscardValue then break; // Processed separately
  if ParsIsLocalOrTmp(BLineE.Param[1].Targ)=FALSE then break;
  if ParsIsGlobalOrExtern(BLineE.Param[0].Targ) then break;
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexE);
  if TargUsedAfter(BIndexE+1,BUsed,BLineE.Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;

  if BFound then
   begin
   BFound:=FALSE;
   BIndexS:=BIndexE-1;
   while BIndexS>=0 do
    begin
    BLineS:=FFlow[BIndexS];
    if BLineS.IsJxx or BLineS.IsLabel then break;
    if BLineS.CheckTargUsage(BLineE.Param[1].Targ)='d' then begin BFound:=BLineS.Cmd<>'ld_lea'; break; end;
    if BLineS.CheckTargUsage(BLineE.Param[0].Targ)<>'.' then break;
    dec(BIndexS);
    end;
   if BFound then
    begin
    ADirty:=TRUE;
    if ParsIsTmp(BLineE.Param[1].Targ)=FALSE then AppendSilent(BLineE.Param[1].Targ);
    BIndexA:=BIndexS;
    while BIndexA<=BIndexE do
     begin
     FFlow[BIndexA].ChangeTargs(BLineE.Param[1].Targ,BLineE.Param[0].Targ);
     inc(BIndexA);
     end;
    end;
   end;

  inc(BIndexE);
  end;
End;

// mov      ~deqr39e:BTmp1# ~dapr39e:APointerParVal# ;~rm:90,51#     s d . . . . . . . . . . . . . . . . . . . . . . . . . . | .
// mov_rm   ~deq:BTmp34# ~deqr39e:BTmp1#[~c_i:32#] ;~rm:90,51#       | s . . . . . . . . . . . . . . . . . . . . . . . . . . | d
Procedure TProcVars.DelExtraBaseRM ( Var ADirty : boolean );
Var
  BLineS,
  BLineE        : TFlowLine;
  BIndexS,
  BIndexE,
  BIndexA       : Integer;
  BFound        : boolean;
  BUsed         : string;
  BUsage        : char;
Begin
 BIndexE:=0;
 while BIndexE<Length(FFlow) do
  begin
  BLineE:=FFlow[BIndexE];
  BFound:=FALSE;
  repeat
  if BLineE.IsMovRm=FALSE then break;
  if ParsIsLocalOrParamOrTmp(BLineE.Param[1].Arr)=FALSE then break;
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexE);
  if TargUsedAfter(BIndexE+1,BUsed,BLineE.Param[1].Arr) then break;
  BFound:=TRUE
  until TRUE;

  if BFound then
   begin
   BFound:=FALSE;
   BIndexS:=BIndexE-1;
   while BIndexS>=0 do
    begin
    BLineS:=FFlow[BIndexS];
    if BLineS.IsJxx or BLineS.IsLabel then break;
    BUsage:=BLineS.CheckTargUsage(BLineE.Param[1].Arr);
    if BUsage='x' then break;
    if BUsage='d' then
     begin
     if BLineS.IsMov=FALSE then break;
     BFound:=TRUE;
     break;
     end;
    dec(BIndexS);
    end;
   if BFound then
    begin
    ADirty:=TRUE;
    if ParsIsTmp(BLineE.Param[1].Arr)=FALSE then AppendSilent(BLineE.Param[1].Arr);
    BIndexA:=BIndexS;
    while BIndexA<=BIndexE do
     begin
     FFlow[BIndexA].ChangeTargs(BLineE.Param[1].Arr,BLineS.Param[1].Targ);
     inc(BIndexA);
     end;
    end;
   end;

  inc(BIndexE);
  end;
End;

// In the case below BTmp3 can be replaced by APointerParVal. Line with BTmp4 is just a line in-between
// mov      ~deqr39e:BTmp3# ~dapr39e:APointerParVal# ;~rm:91,34#     s d . . . . . . . . . . . . . . . . .
// mov      ~deb:BTmp4# ~c_b:5#              ;~rm:91,34#             | | d . . . . . . . . . . . . . . . .
// mov_mr   ~deqr39e:BTmp3#[~c_i:38#]_b ~deb:BTmp4# ;~rm:91,34#      | s s . . . . . . . . . . . . . . . .
Procedure TProcVars.DelExtraBaseMR ( Var ADirty : boolean );
Var
  BLineS,
  BLineE        : TFlowLine;
  BIndexS,
  BIndexE,
  BIndexA       : Integer;
  BFound        : boolean;
  BUsed         : string;
  BUsage        : char;
Begin
 BIndexE:=0;
 while BIndexE<Length(FFlow) do
  begin
  BLineE:=FFlow[BIndexE];
  BFound:=FALSE;
  repeat
  if BLineE.IsMovMr=FALSE then break;
  if ParsIsLocalOrParamOrTmp(BLineE.Param[0].Arr)=FALSE then break;
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexE);
  if TargUsedAfter(BIndexE+1,BUsed,BLineE.Param[0].Arr) then break;
  BFound:=TRUE
  until TRUE;

  if BFound then
   begin
   BFound:=FALSE;
   BIndexS:=BIndexE-1;
   while BIndexS>=0 do
    begin
    BLineS:=FFlow[BIndexS];
    if BLineS.IsJxx or BLineS.IsLabel then break;
    BUsage:=BLineS.CheckTargUsage(BLineE.Param[0].Arr);
    if BUsage='x' then break;
    if BUsage='d' then
     begin
     if BLineS.IsMov=FALSE then break;
     BFound:=TRUE;
     break;
     end;
    dec(BIndexS);
    end;
   if BFound then
    begin
    ADirty:=TRUE;
    if ParsIsTmp(BLineE.Param[0].Arr)=FALSE then AppendSilent(BLineE.Param[1].Arr);
    BIndexA:=BIndexS;
    while BIndexA<=BIndexE do
     begin
     FFlow[BIndexA].ChangeTargs(BLineE.Param[0].Arr,BLineS.Param[1].Targ);
     inc(BIndexA);
     end;
    end;
   end;

  inc(BIndexE);
  end;
End;

// mov_rm   ~deqr39e:BTmp7# ~dapr39e:APointerParVal#[~c_i:32#] ;~rm:92,41# s . d . . . . . . . . . . . . . . .
// mov_rm   ~deb:BTmp8# ~dapr39e:APointerParVal#[~c_i:38#]_b ;~rm:92,41# s . | d . . . . . . . . . . . . . .
// mov_mr   ~deqr39e:BTmp7#[~c_i:38#]_b ~deb:BTmp8# ;~rm:92,41#      | . s s . . . . . . . . . . . . . .
// mov_rm   ~deqr39e:BTmp11# ~dapr39e:APointerParVal#[~c_i:32#] ;~rm:93,49# s . . . d . . . . . . . . . . . . .
// mov_rm   ~depr39e:BTmp12# ~dapr39e:APointerParVal#[~c_i:32#]_pr39e{sp30eo0_FStringComp,p"TRecordType"o32_FPointerComp,bo36_FDiscr,bo37_FEnumComp,bo38_FIntComp,} ;~rm:93,49# s . . . | d . . . . . . . . . . . .

Procedure TProcVars.DelExtraLoad ( Var ADirty : boolean );
Var
  BLineS,
  BLineE        : TFlowLine;
  BIndexS,
  BIndexE       : Integer;
  BFound        : boolean;
  BDirtySA,
  BDirtySI,
  BDirtyEA,
  BDirtyEI      : string;
  BTarg         : string;
  BTargChainA,
  BTargChainI   : string;
Begin
 //DbgSave(Self,'C_Vars',FFlow,TRUE);
 BLineE:=nil;
 BIndexS:=0;
 while BIndexS<Length(FFlow) do
  begin
  BLineS:=FFlow[BIndexS];
  BFound:=FALSE;
  repeat
  if BLineS.IsMovRm=FALSE then break;
  if ParsIsLocalOrParamOrTmp(BLineS.Param[1].Arr)=FALSE then break;
  BTargChainA:=GetInitMask;
  FillChainA(BLineS.Param[1].Arr,'s',BIndexS,BTargChainA);
  BTargChainI:=GetInitMask;
  if (BLineS.Param[1].Idx='') or ParsIsConst(BLineS.Param[1].Idx) then
  else FillChainA(BLineS.Param[1].Idx,'s',BIndexS,BTargChainI);
  BDirtySA:=GetDirtyUp(BIndexS,BTargChainA);
  if (BLineS.Param[1].Idx='') or ParsIsConst(BLineS.Param[1].Idx) then BDirtySI:='' else BDirtySI:=GetDirtyUp(BIndexS,BTargChainI);
  BIndexE:=BIndexS+1;
  while BIndexE<Length(FFlow) do
   begin
   BLineE:=FFlow[BIndexE];
   repeat
   if BLineE.IsMovRm=FALSE then break;
   if (BLineE.Param[1].Arr<>BLineS.Param[1].Arr) or (BLineE.Param[1].Idx<>BLineS.Param[1].Idx) then break;
   BDirtyEA:=GetDirtyUp(BIndexE,BTargChainA);
   if (BLineE.Param[1].Idx='') or ParsIsConst(BLineE.Param[1].Idx) then BDirtyEI:='' else BDirtyEI:=GetDirtyUp(BIndexE,BTargChainI);
   if IsExtraVisi(BDirtySA,BDirtyEA,'dx') then break;
   if (BDirtySI<>'') and IsExtraVisi(BDirtySI,BDirtyEI,'dx') then break;
   BFound:=TRUE;
   until TRUE;
   if BFound then break;
   inc(BIndexE);
   end;
  until TRUE;
  if BFound then
   begin
   ADirty:=TRUE;
   BTarg:=AppendTmpVar(ExtractFinalType(BLineS.Param[0].Targ));
   InsertFlowLine(FFlow,BIndexS+1,'mov '+BLineS.Param[0].Targ+' '+BTarg+' ;'+BLineS.Tail);
   BLineS.Param[0].SetTarg(BTarg);
   BLineE.RdLine('mov '+BLineE.Param[0].Targ+' '+BTarg+' ;'+BLineE.Tail);
   //DbgSave(Self,'C_Vars',FFlow,TRUE);
   end
  else inc(BIndexS);
  end;
End;

Procedure TProcVars.CheckArrIdx ( Var ADirty : boolean );
Var
  BLine     : TFlowLine;
  BIndex    : Integer;
  BTarg     : string;
  BType     : string;
Begin
 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  repeat
  if BLine.IsMovMr=FALSE then break;
  if BLine.Param[0].Arr='' then break;
  if BLine.Param[0].Idx='' then break;
  if ParsIsLocalOrParamOrTmp(BLine.Param[0].Arr)=FALSE then break;
  if ParsIsLocalOrParamOrTmp(BLine.Param[0].Idx)=FALSE then break;
  if ParsIsConst(BLine.Param[0].Idx) then break;
  BType:=ExtractFinalType(BLine.Param[1].Targ);
  BTarg:=AppendTmpVar('q'+BType);
  InsertFlowLine(FFlow,BIndex+0,'mov '+BTarg+' '+BLine.Param[0].Arr+' ;'+BLine.Tail);
  InsertFlowLine(FFlow,BIndex+1,'add '+BTarg+' '+BLine.Param[0].Idx+' ;'+BLine.Tail);
  BLine.Param[0].SetTarg(BTarg+'['+CConstZero+']_'+BType);
  inc(BIndex,2);
  ADirty:=TRUE;
  until TRUE;
  inc(BIndex);
  end;

 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  repeat
  if BLine.IsMovRm=FALSE then break;
  if BLine.Param[1].Arr='' then break;
  if BLine.Param[1].Idx='' then break;
  if ParsIsLocalOrParamOrTmp(BLine.Param[1].Arr)=FALSE then break;
  if ParsIsLocalOrParamOrTmp(BLine.Param[1].Idx)=FALSE then break;
  if ParsIsConst(BLine.Param[1].Idx) then break;
  BType:=ExtractFinalType(BLine.Param[0].Targ);
  BTarg:=AppendTmpVar('q'+BType);
  InsertFlowLine(FFlow,BIndex+0,'mov '+BTarg+' '+BLine.Param[1].Arr+' ;'+BLine.Tail);
  InsertFlowLine(FFlow,BIndex+1,'add '+BTarg+' '+BLine.Param[1].Idx+' ;'+BLine.Tail);
  BLine.Param[1].SetTarg(BTarg+'['+CConstZero+']_'+BType);
  inc(BIndex,2);
  ADirty:=TRUE;
  until TRUE;
  inc(BIndex);
  end;

End;


{
// add      ~deb:BTmp2# ~dbb:BDataA#         ;~rm:12,23#
// mov      ~dbb:BDataA# ~deb:BTmp2#         ;~rm:12,23#
Procedure TProcVars.OptiSymmetric2 ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB       : Integer;
  BUsed         : string;
  BFound        : boolean;
Begin
 repeat
 FTextDst.Clear;
 if Length(FFlow)<2 then break;
 //SetLength(BUsed,Length(FFlow));
 FWnd2.RdLine(FTextSrc.Strings[0]);
 BIndexA:=1;
 while BIndexA<Length(FFlow) do
  begin
  FWnd2.RdLine(FTextSrc.Strings[BIndexA]);
  BFound:=FALSE;
  repeat
  if StrInList(FWnd2.LineList[0].Cmd,'add and or xor mul') then
  else break;
  if FWnd2.LineList[1].Cmd<>'mov' then break;
  if FWnd2.LineList[0].Param[0].Targ<>FWnd2.LineList[1].Param[1].Targ then break;
  if FWnd2.LineList[1].Param[0].Targ<>FWnd2.LineList[0].Param[1].Targ then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[0].Targ) then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[1].Targ) then break;
  if ParsIsTmp(FWnd2.LineList[1].Param[1].Targ)=FALSE then break;
  //FillChar(BUsed[1],Length(FFlow),' '); BUsed[BIndexA+1]:='*';
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexA);
  if TargUsedAfter(BIndexA+1,BUsed,FWnd2.LineList[1].Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;
  if BFound then break;
  inc(BIndexA);
  end;
 if BIndexA=Length(FFlow) then break;

 ADirty:=TRUE;
 BIndexB:=0;
 while BIndexB<Length(FFlow) do
  begin
  BLineA.RdLine(FTextSrc.Strings[BIndexB]);
  if BIndexB=(BIndexA-1) then begin BLineA.XchgSrcDst; FTextDst.Append(BLineA.WrLine); end
  else if BIndexB=BIndexA then
  else FTextDst.Append(BLineA.Orig);
  inc(BIndexB);
  end;

 FTextSrc.Assign(FTextDst);
 until FALSE;
End;

// mov_wb   ~dew:BTmp6# ~deb:BTmp5#          ;~rm:16,16#
// mov      ~drw:Result# ~dew:BTmp6#         ;~rm:16,16#
Procedure TProcVars.OptiMovZX_Mov ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB       : Integer;
  BUsed         : string;
  BFound        : boolean;
Begin
 repeat
 FTextDst.Clear;
 if Length(FFlow)<2 then break;
 //SetLength(BUsed,Length(FFlow));
 FWnd2.RdLine(FTextSrc.Strings[0]);
 BIndexA:=1;
 while BIndexA<Length(FFlow) do
  begin
  FWnd2.RdLine(FTextSrc.Strings[BIndexA]);
  BFound:=FALSE;
  repeat
  // mov_wb   ~dew:BTmp6# ~deb:BTmp5#          ;~rm:16,16#
  // mov      ~drw:Result# ~dew:BTmp6#         ;~rm:16,16#
  if StrInList(FWnd2.LineList[0].Cmd,'mov_wb mov_db mov_dw mov_iw mov_ib') then
  else break;
  if FWnd2.LineList[1].Cmd<>'mov' then break;
  if FWnd2.LineList[0].Param[0].Targ<>FWnd2.LineList[1].Param[1].Targ then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[0].Targ) then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[1].Targ) then break;
  if ParsIsTmp(FWnd2.LineList[1].Param[1].Targ)=FALSE then break;
  //FillChar(BUsed[1],Length(FFlow),' '); BUsed[BIndexA+1]:='*';
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexA);
  if TargUsedAfter(BIndexA+1,BUsed,FWnd2.LineList[1].Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;
  if BFound then break;
  inc(BIndexA);
  end;
 if BIndexA=Length(FFlow) then break;

 ADirty:=TRUE;
 BIndexB:=0;
 while BIndexB<Length(FFlow) do
  begin
  BLineA.RdLine(FTextSrc.Strings[BIndexB]);
  if BIndexB=(BIndexA-1) then
   begin
   BLineA.Param[0].Targ:=FWnd2.LineList[1].Param[0].Targ;
   FTextDst.Append(BLineA.WrLine);
   end
  else if BIndexB=BIndexA then
  else FTextDst.Append(BLineA.Orig);
  inc(BIndexB);
  end;

 FTextSrc.Assign(FTextDst);
 until FALSE;
End;

// mov_rm   ~def:BTmp9# ~dep:BTmp7#[~ded:BTmp8#] ;~rm:43,43#
// mov      ~def:BTmp11# ~def:BTmp9#         ;~rm:43,43#
Procedure TProcVars.OptiMovDM_Mov ( Var ADirty : boolean );
Var
  BIndexA,
  BIndexB       : Integer;
  BUsed         : string;
  BFound        : boolean;
Begin
 repeat
 FTextDst.Clear;
 if Length(FFlow)<2 then break;
 //SetLength(BUsed,Length(FFlow));
 FWnd2.RdLine(FTextSrc.Strings[0]);
 BIndexA:=1;
 while BIndexA<Length(FFlow) do
  begin
  FWnd2.RdLine(FTextSrc.Strings[BIndexA]);
  BFound:=FALSE;
  repeat
  // mov_rm   ~def:BTmp9# ~dep:BTmp7#[~ded:BTmp8#] ;~rm:43,43#
  // mov      ~def:BTmp11# ~def:BTmp9#         ;~rm:43,43#
  if FWnd2.LineList[0].Cmd<>'mov_rm' then break;
  if FWnd2.LineList[1].Cmd<>'mov' then break;
  if FWnd2.LineList[0].Param[0].Targ<>FWnd2.LineList[1].Param[1].Targ then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[0].Targ) then break;
  if ParsIsGlobal(FWnd2.LineList[1].Param[1].Targ) then break;
  if ParsIsTmp(FWnd2.LineList[1].Param[1].Targ)=FALSE then break;
  //FillChar(BUsed[1],Length(FFlow),' '); BUsed[BIndexA+1]:='*';
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexA);
  if TargUsedAfter(BIndexA+1,BUsed,FWnd2.LineList[1].Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;
  if BFound then break;
  inc(BIndexA);
  end;
 if BIndexA=Length(FFlow) then break;

 ADirty:=TRUE;
 BIndexB:=0;
 while BIndexB<Length(FFlow) do
  begin
  BLineA.RdLine(FTextSrc.Strings[BIndexB]);
  if BIndexB=(BIndexA-1) then
   begin
   BLineA.Param[0].Targ:=FWnd2.LineList[1].Param[0].Targ;
   FTextDst.Append(BLineA.WrLine);
   end
  else if BIndexB=BIndexA then
  else FTextDst.Append(BLineA.Orig);
  inc(BIndexB);
  end;

 FTextSrc.Assign(FTextDst);
 until FALSE;
End;}

// After previous operation we can have cases like following
// mov      ~b_d:BIndex# ~b_d:BIndex#        ;~rm:43,5#
Procedure TProcVars.DelMovSame ( Var ADirty : boolean );
Var
  BLine         : TFlowLine;
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  if BLine.IsMov and (BLine.Param[0].Targ=BLine.Param[1].Targ) then begin ADirty:=TRUE; DeleteFlowLine(FFlow,BIndex); end
  else inc(BIndex);
  end;
End;

Procedure TProcVars.DelAddSubZero ( Var ADirty : boolean );
Var
  BLine         : TFlowLine;
  BIndex        : Integer;
  BFound        : boolean;
Begin
 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  BFound:=FALSE;
  repeat
  if BLine.IsLabel or BLine.IsJxx then break;
  if StrInList(BLine.Cmd,'add sub')=FALSE then break;
  if ParsIsConst(BLine.Param[1].Targ)=FALSE then break;
  if ParsExtractName(BLine.Param[1].Targ)<>'0' then break;
  BFound:=TRUE;
  until TRUE;
  if BFound then begin ADirty:=TRUE; DeleteFlowLine(FFlow,BIndex); end
  else inc(BIndex);
  end;
End;

Procedure TProcVars.DelMulDivOne ( Var ADirty : boolean );
Var
  BLine         : TFlowLine;
  BIndex        : Integer;
  BFound        : boolean;
Begin
 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  BFound:=FALSE;
  repeat
  if BLine.IsLabel or BLine.IsJxx then break;
  if StrInList(BLine.Cmd,'mul div')=FALSE then break;
  if ParsIsConst(BLine.Param[1].Targ)=FALSE then break;
  if ParsExtractName(BLine.Param[1].Targ)<>'1' then break;
  BFound:=TRUE;
  until TRUE;
  if BFound then begin ADirty:=TRUE; DeleteFlowLine(FFlow,BIndex); end
  else inc(BIndex);
  end;
End;

// mov ~dei:BTmp4# ~c_i:0#
// add ~dei:BTmp4# ~dei:BTmp2#

Procedure TProcVars.DelMovZeroAddSub ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA,
  BLineB        : TFlowLine;
  BFound        : boolean;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  BFound:=FALSE;
  repeat
  if BLineA.IsMov=FALSE then break;
  if BLineB.Cmd<>'add' then break;
  if BLineA.Param[0].Targ<>BLineB.Param[0].Targ then break;
  if ParsIsConst(BLineA.Param[1].Targ)=FALSE then break;
  if ParsExtractName(BLineA.Param[1].Targ)<>'0' then break;
  BFound:=TRUE
  until TRUE;
  if BFound then begin ADirty:=TRUE; BLineB.Cmd:='mov'; DeleteFlowLine(FFlow,BIndex-1); BLineA:=BLineB; end
  else begin BLineA:=BLineB; inc(BIndex); end;
  end;
 until TRUE;
End;


// Strings optimization

// mov      ~desp127e:BTmp0# ~c_sp3e:ConstStrP0# ;~rm:23,18#
// mov      ~dhsp127e:FDummyStr# ~desp127e:BTmp0# ;~rm:23,18#
Procedure TProcVars.DelExtraMov2Str ( Var ADirty : boolean );
Var
  BIndexA       : Integer;
  BLineA,
  BLineB        : TFlowLine;
  BUsed         : string;
  BFound        : boolean;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndexA:=1;
 while BIndexA<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndexA];
  BFound:=FALSE;
  repeat
  if (BLineA.IsMov=FALSE) or (BLineB.IsMov=FALSE) then break;
  if BLineA.Param[0].Targ<>BLineB.Param[1].Targ then break;
  if BLineB.Param[0].Targ=BLineA.Param[1].Targ then break;
  if ParsIsStringX(BLineA.Param[1].Targ)=FALSE then break;
  if ParsIsStringX(BLineB.Param[0].Targ)=FALSE then break;
  if ParsIsTmp(BLineB.Param[1].Targ)=FALSE then break;
  BUsed:=GetInitMask; CheckSetMask(BUsed,BIndexA);
  if TargUsedAfter(BIndexA+1,BUsed,BLineB.Param[1].Targ) then break;
  BFound:=TRUE
  until TRUE;
  if BFound then begin ADirty:=TRUE; BLineA.Param[0].Targ:=BLineB.Param[1].Targ; DeleteFlowLine(FFlow,BIndexA); end
  else begin BLineA:=BLineB; inc(BIndexA); end;
  end;
 until TRUE;
End;

Function TProcVars.IsTargUsed ( Const ATarg : string ) : boolean;
Var
  BIndex        : Integer;
  BLine         : TFlowLine;
Begin
 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLine:=FFlow[BIndex];
  if BLine.CheckTargUsage(ATarg)<>'.' then break;
  inc(BIndex);
  end;
 Result:=BIndex<Length(FFlow);
End;

Procedure TProcVars.DelUnusedVars ( Var ADirty : boolean );
Var
  BVarList      : string;
  BVarThis      : string;
  BSpecA        : string;
Begin
 BVarList:='';
 repeat
 BVarThis:=ReadParamStr(FVarListS);
 if BVarThis='' then break;
 if IsTargUsed(BVarThis) then BVarList:=BVarList+BVarThis+' '
 else
  begin
  ADirty:=TRUE;
  BSpecA:=ParsExtractSpec(BVarThis);
  if BSpecA='' then begin AppendError('Internal error: Var spec is empty [r:TProcVars.DelUnusedVars]'); break; end;
  if (ParsIsSpecTmp(BSpecA)=FALSE) and (CheckSilent(BVarThis)=FALSE) then AppendError('w',FEndLine,FEndPos,'Variable '+ParsExtractName(BVarThis)+' is declared but not used [R:TProcVars.DelUnusedVars]');
  end;
 until FALSE;
 FVarListS:=BVarList;
End;

Procedure TProcVars.CheckAppendGlobal ( Const ATarg : string );
Begin
 if StrInList(ATarg,FGlobalRefs) then
 else
  begin
  if FGlobalRefs<>'' then FGlobalRefs:=FGlobalRefs+' ';
  FGlobalRefs:=FGlobalRefs+ATarg;
  end;
End;

end.
// 900
