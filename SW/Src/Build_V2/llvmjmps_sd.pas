unit LlvmJmps_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsHelper_sd, LlvmDbg_sd, LlvmLine_sd, LlvmChains_sd;

Const
  COpcodeList = ' add sub mul div and or test xor cmpe cmpne cmpb cmpa cmpbe cmpae ';

Type
  TProcJmps = class(TProcChains)
  private
    FTextJmps   : TStringList;

    Function InvJmp ( Const AJmp : string ) : string;

    Procedure SanityCheck;
    Procedure DoubleLabels ( Var ADirty : boolean );
    Procedure DelUnrefLabels ( Var ADirty : boolean );
    Procedure DelJmpNextLabel ( Var ADirty : boolean );
    Procedure RenameLabelJmp ( Var ADirty : boolean );
    Procedure InvJmpOrder ( Var ADirty : boolean );
    Procedure DelDualJmp ( Var ADirty : boolean );
    Procedure DelUnreachable ( Var ADirty : boolean );
    Procedure OptiLoopsA ( Var ADirty : boolean );
    Procedure OptiConstA ( Var ADirty : boolean );

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure GenCompDbg ( ADbg : TStringList ); Override;

    property TextJmps : TStringList read FTextJmps;
  end;

implementation

Uses
  ConComL, ConComAsm;

Constructor TProcJmps.Create;
Begin
 Inherited;
 FTextJmps:=TStringList.Create;
End;

Destructor TProcJmps.Destroy;
Begin
 FTextJmps.Free;
 Inherited;
End;

Procedure TProcJmps.Compile;
Var
  BDirty        : Boolean;
Begin
 Inherited;

 SanityCheck;

 repeat
 if FFatalError then break;
 BDirty:=FALSE;
 DoubleLabels(BDirty);       //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 DelUnrefLabels(BDirty);     //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 DelJmpNextLabel(BDirty);    //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 RenameLabelJmp(BDirty);     //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 InvJmpOrder(BDirty);        //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 DelDualJmp(BDirty);         //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 DelUnreachable(BDirty);     //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 OptiLoopsA(BDirty);         //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 OptiConstA(BDirty);         //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'B_Jmps',FTextSrc,TRUE); end;
 until BDirty=FALSE;

 DbgMarkFlow('Jmps');
 FlowListToStringList(FTextJmps,FFlow);
 FFlowList.Assign(FTextJmps);

 TimeStamp('Jmps');
 if FModule.DbgPath<>'' then DbgSave(Self,'B_Jmps',FFlowList,TRUE);
End;

Procedure TProcJmps.SanityCheck;
Var
  BFlowIdx      : Integer;
  BLine         : TFlowLine;
  BLabelIdx     : Integer;
Begin
 for BFlowIdx:=0 to Length(FFlow)-1 do
  begin
  BLine:=FFlow[BFlowIdx];
  if BLine.IsJxx then
   begin
   BLabelIdx:=FindLabelIdx(BLine.Param[0].Targ);
   if BLabelIdx<0 then begin AppendError('e',BLine,'Internal error: Label '+BLine.Param[0].Targ+' is not found [R:TProcJmps.SanityCheck]'); FFatalError:=TRUE; end;
   end;
  end;
End;

// Replaces double labels with a single label and updates references
Procedure TProcJmps.DoubleLabels ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA,
  BLineB        : TFlowLine;
  BLabelA,
  BLabelB       : string;
  BLabelX       : string;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineA.IsLabel and BLineB.IsLabel then break;
  BLineA:=BLineB;
  inc(BIndex);
  end;
 if BIndex>=Length(FFlow) then break;

 ADirty:=TRUE;
 BLabelA:=BLineA.LabelName; BLabelB:=BLineB.LabelName;
 BLabelX:=NewLabelName('X');
 DeleteFlowLine(FFlow,BIndex);

 BIndex:=0;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineB.IsJxxTo(BLabelA) or BLineB.IsJxxTo(BLabelB) then BLineB.SetParamA(BLabelX);
  inc(BIndex);
  end;
 BLineA.SetLabelName(BLabelX);
 until FALSE;
End;

// Deletes unreferenced labels
Procedure TProcJmps.DelUnrefLabels ( Var ADirty : boolean );
Var
  BIndexL,
  BIndexR       : Integer;
  BLineA        : TFlowLine;
  BDelete       : boolean;
Begin
 BIndexL:=0;
 while BIndexL<Length(FFlow) do
  begin
  BLineA:=FFlow[BIndexL];
  BDelete:=FALSE;
  if BLineA.IsLabel then
   begin
   BIndexR:=0;
   while BIndexR<Length(FFlow) do
    begin
    if FFlow[BIndexR].IsJxxTo(BLineA.LabelName) then break;
    inc(BIndexR);
    end;
   if BIndexR>=Length(FFlow) then BDelete:=TRUE;
   end;
  if BDelete then begin DeleteFlowLine(FFlow,BIndexL); ADirty:=TRUE; end
  else inc(BIndexL);
  end;
End;

// Deletes JMP if next line is directly a JMP label
Procedure TProcJmps.DelJmpNextLabel ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA,
  BLineB        : TFlowLine;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineA.IsJmp and BLineB.IsLabel and (BLineA.Param[0].Targ=BLineB.LabelName) then
   begin
   ADirty:=TRUE;
   DeleteFlowLine(FFlow,BIndex-1);
   if BIndex>=Length(FFlow) then break;
   BLineA:=FFlow[BIndex-1];
   end
  else
   begin
   BLineA:=BLineB;
   inc(BIndex);
   end;
  end;
 until TRUE;
End;

// Detects cases with label and then a direct JMP to another label. Updates references
Procedure TProcJmps.RenameLabelJmp ( Var ADirty : boolean );
Var
  BLineA,
  BLineB        : TFlowLine;
  BIndex        : Integer;
  BLabelOld,
  BLabelNew     : string;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineA.IsLabel and BLineB.IsJmp and (BLineB.Param[0].Targ<>BLineA.LabelName) then break;
  inc(BIndex);
  end;
 if BIndex>=Length(FFlow) then break;

 ADirty:=TRUE;
 BLabelOld:=BLineA.LabelName; BLabelNew:=BLineB.Param[0].Targ;
 DeleteFlowLine(FFlow,BIndex-1);

 for BIndex:=0 to Length(FFlow)-1 do
  begin
  BLineA:=FFlow[BIndex];
  if BLineA.IsJxx and (BLineA.Param[0].Targ=BLabelOld) then BLineA.SetParamA(BLabelNew);
  end;

 until FALSE;
End;

Function TProcJmps.InvJmp ( Const AJmp : string ) : string;
Begin
 Result:='jmp_invalid';
 if AJmp='je' then Result:='jne'
 else if AJmp='jne' then Result:='je'
 else if AJmp='ja' then Result:='jbe'
 else if AJmp='jb' then Result:='jae'
 else if AJmp='jae' then Result:='jb'
 else if AJmp='jbe' then Result:='ja'
 else if AJmp='jg' then Result:='jse'
 else if AJmp='js' then Result:='jge'
 else if AJmp='jge' then Result:='js'
 else if AJmp='jse' then Result:='jg'
 else AppendError('Internal error: cannot invert JMP condition '+AJmp+' [r:TProcJmps.InvJmp]');
End;

//  je       Proba1_M6                        ;~rm:30,35#
//  jmp      Proba1_X7                        ;~rm:30,35#
// Proba1_M6:                                 ;~rm:30,35#
Procedure TProcJmps.InvJmpOrder ( Var ADirty : boolean );
Var
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BIndex        : Integer;
Begin
 repeat
 if Length(FFlow)<3 then break;
 BLineA:=FFlow[0]; BLineB:=FFlow[1];
 BIndex:=2;
 while BIndex<Length(FFlow) do
  begin
  BLineC:=FFlow[BIndex];
  if (BLineA.IsJmp=FALSE) and BLineB.IsJmp and BLineC.IsLabel and BLineA.IsJxxTo(BLineC.LabelName) then
   begin
   ADirty:=TRUE;
   BLineA.InvJmp(BLineB.Param[0].Targ);
   DeleteFlowLine(FFlow,BIndex-1);
   BLineB:=BLineC;
   //if FModule.DbgPath<>'' then begin FlowListToStringList(FTextSrc,FFlow); DbgSave(Self,'Jmps',FTextSrc,TRUE); end;
   end
  else
   begin
   BLineA:=BLineB;
   BLineB:=BLineC;
   inc(BIndex);
   end;
  end;
 until TRUE;
End;

// Sometimes we have 2 consecutive JMPs
// jmp      Func1_CaseEA0_U2                 ;~rm:13,10#
// jmp      Func1_CaseThen_1_A0_U6           ;~rm:13,11#
// This can happen when parser inserts JMP after break.
// Remove 2nd JMP silently.
// This is the only case when JMP can be silently removed (without "Unreachable code" warning)
Procedure TProcJmps.DelDualJmp ( Var ADirty : boolean );
Var
  BLineA,
  BLineB        : TFlowLine;
  BIndex        : Integer;
Begin
 repeat
 if Length(FFlow)<2 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineA.IsJmp and BLineB.IsJmp then begin ADirty:=TRUE; DeleteFlowline(FFlow,BIndex); end
  else begin BLineA:=BLineB; inc(BIndex); end;
  end;
 until TRUE;
End;

Procedure TProcJmps.DelUnreachable ( Var ADirty : boolean );
Var
  BIndex        : Integer;
  BLineA,
  BLineB        : TFlowLine;
Begin
 repeat
 if Length(FFlow)<1 then break;
 BLineA:=FFlow[0];
 BIndex:=1;
 while BIndex<Length(FFlow) do
  begin
  BLineB:=FFlow[BIndex];
  if BLineA.IsJmp and (BLineB.IsJmp=FALSE) and (BLineB.IsLabel=FALSE) then
   begin
   ADirty:=TRUE;
   AppendError('w',BLineB,'Unreachable code deleted [r:TProcJmps.DelUnreachable]');
   while BIndex<Length(FFlow) do
    begin
    if FFlow[BIndex].IsLabel then break;
    DeleteFlowline(FFlow,BIndex);
    end;
   if BIndex>=Length(FFlow) then break;
   BLineA:=FFlow[BIndex];
   end
  else
   begin
   BLineA:=BLineB;
   inc(BIndex);
   end;
  end;
 until TRUE;
End;

Procedure TProcJmps.GenCompDbg ( ADbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Inherited;
 ADbg.Append(';@T Jmps');
 ADbg.Append(DbgProcHeader);
 for BLineIdx:=0 to FTextJmps.Count-1 do ADbg.Append(FTextJmps.Strings[BLineIdx]);
End;

{
 U0:        // LineC  IndexB
   cmp ...  // LineD
   jxx U2   // LineE
   ...
   ...
   jmp U0   // LineA  IndexA
 U2:        // LineB

 --------

 U0:        // LineC IndexB
   cmp ...  // LineD
   jxx U2   // LineE
 UN:        // LineF
   ...
   ...
   cmp ...  // LineG IndexA
   jnx UN   // LineH
 U2:        // LineB
}

Procedure TProcJmps.OptiLoopsA ( Var ADirty : boolean );
Var
  BLineA,
  BLineB,
  //BLineC,
  BLineD,
  BLineE,
  BLineF,
  BLineG,
  BLineH        : TFlowLine;
  BIndexA,
  BIndexB       : Integer;
  BLabelN       : string;
Begin
 BIndexA:=0;
 while BIndexA<Length(FFlow)-1 do
  begin
  BLineA:=FFlow[BIndexA+0];
  BLineB:=FFlow[BIndexA+1];
  repeat
  if BLineA.IsJmp=FALSE then break;
  if BLineB.IsLabel=FALSE then break;
  if BLineA.IsJxxTo(BLineB.LabelName) then break;
  BIndexB:=FindLabelIdx(BLineA.Param[0].Targ);
  if BIndexB=-1 then break;
  if (BIndexB+3)>=Length(FFlow) then break;
  if BIndexB>BIndexA then break;
  //BLineC:=FFlow[BIndexB+0];
  BLineD:=FFlow[BIndexB+1];
  BLineE:=FFlow[BIndexB+2];
  if BLineE.IsJxx=FALSE then break;
  if BLineE.IsJmp then break;
  if BLineE.IsJxxTo(BLineB.LabelName)=FALSE then break;
  if BLineD.IsCmpTest=FALSE then break;
  ADirty:=TRUE;
  BLabelN:=NewLabelName('Loop');
  BLineF:=TFlowLine.Create; BLineF.RdLine(BLabelN+': ; '+BLineA.Tail);
  BLineG:=BLineD.Clone;
  BLineH:=BLineE.Clone; BLineH.InvJmp(BLabelN);
  DeleteFlowLine(FFlow,BLineA);
  InsertFlowLine(FFlow,BIndexA,BLineH);
  InsertFlowLine(FFlow,BIndexA,BLineG);
  InsertFlowLine(FFlow,BIndexB+3,BLineF);
  until TRUE;
  if ADirty then break;
  inc(BIndexA);
  end;
End;

//  mov      ~dbi:BIndex#,~c_i:0#       ;[SrcFile:C:\ZukH\Projects\CompTestSuite\ShortNonRegrA\TestSuites\TestA_Cpp\tsMainA.ci][SrcPos:13,15][ProcNameL:~mhw:MainA,#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:.d|][Esp:0][TargRegMap:  . awx  bw ][SrcOrig:]for (BIndex=0; BIndex<10; BIndex++) BResult=BResult+1;
//  cmp      ~dbi:BIndex#,~c_i:10#      ;[SrcFile:C:\ZukH\Projects\CompTestSuite\ShortNonRegrA\TestSuites\TestA_Cpp\tsMainA.ci][SrcPos:13,26][ProcNameL:~mhw:MainA,#][AtomPos:3][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:.s|][Esp:0][TargRegMap:  . awx  bw ][SrcOrig:]for (BIndex=0; BIndex<10; BIndex++) BResult=BResult+1;
//  jge      MainA_ForEndA0_U3          ;[SrcFile:C:\ZukH\Projects\CompTestSuite\ShortNonRegrA\TestSuites\TestA_Cpp\tsMainA.ci][SrcPos:13,26][ProcNameL:~mhw:MainA,#][AtomPos:4][JmpsPos:4][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:.||][Esp:0][TargRegMap:  . awx  bw ][SrcOrig:]for (BIndex=0; BIndex<10; BIndex++) BResult=BResult+1;

Procedure TProcJmps.OptiConstA ( Var ADirty : boolean );
Var
  BLineA,
  BLineB,
  BLineC        : TFlowLine;
  BType         : string;
  BLineIdx      : Integer;
  BConstA,
  BConstB       : Int64;
  BCond         : boolean;
Begin
 BLineIdx:=0;
 while BLineIdx<Length(FFlow)-2 do
  begin
  BLineA:=FFlow[BLineIdx+0];
  BLineB:=FFlow[BLineIdx+1];
  BLineC:=FFlow[BLineIdx+2];
  repeat
  if BLineA.IsMov=FALSE then break;
  if BLineB.Cmd<>'cmp' then break;
  if BLineC.IsJxx=FALSE then break;
  if BLineC.IsJmp then break;
  if Length(BLineA.Param)<2 then break;
  if Length(BLineB.Param)<2 then break;
  if BLineA.Param[0].Targ<>BLineB.Param[0].Targ then break;
  BType:=ParsExtractType(BLineA.Param[0].Targ);
  if ParsIsTypeBasic(BType)=FALSE then break;
  if StrInList(BType,'b k w m d i')=FALSE then break;
  if ParsIsConst(BLineA.Param[1].Targ)=FALSE then break;
  if ParsIsConst(BLineB.Param[1].Targ)=FALSE then break;
  if IsInteger(ParsExtractName(BLineA.Param[1].Targ),BConstA)=FALSE then break;
  if IsInteger(ParsExtractName(BLineB.Param[1].Targ),BConstB)=FALSE then break;
  BCond:=FALSE;
  if BLineC.Cmd='je' then BCond:=BConstA=BConstB
  else if BLineC.Cmd='jz'  then BCond:=BConstA=BConstB
  else if BLineC.Cmd='jne' then BCond:=BConstA<>BConstB
  else if BLineC.Cmd='jnz' then BCond:=BConstA<>BConstB
  else if BLineC.Cmd='ja'  then BCond:=BConstA>BConstB
  else if BLineC.Cmd='jb'  then BCond:=BConstA<BConstB
  else if BLineC.Cmd='jae' then BCond:=BConstA>=BConstB
  else if BLineC.Cmd='jbe' then BCond:=BConstA<=BConstB
  else if BLineC.Cmd='jg'  then BCond:=BConstA>BConstB
  else if BLineC.Cmd='js'  then BCond:=BConstA<BConstB
  else if BLineC.Cmd='jge' then BCond:=BConstA>=BConstB
  else if BLineC.Cmd='jse' then BCond:=BConstA<=BConstB
  else break;
  ADirty:=TRUE;
  DeleteFlowLine(FFlow,BLineB);
  if BCond then BLineC.Cmd:='jmp'
  else DeleteFlowLine(FFlow,BLineC);
  until TRUE;
  //if ADirty then break;
  inc(BLineIdx);
  end;
End;



end.


