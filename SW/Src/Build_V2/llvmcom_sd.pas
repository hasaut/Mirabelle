unit LlvmCom_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, ParsBase_sd, ParsHelper_sd, AsmTypes_sd, LlvmDbg_sd,
  LlvmLine_sd;

Type
  TWndMove2 = class(TObject)
  private
    FLineList   : TFlowLineList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Reset;
    Procedure RdLine ( Const ALine : string );

    property LineList : TFlowLineList read FLineList;
  end;

  TWndMove3 = class(TObject)
  private
    FLineList   : TFlowLineList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Reset;
    Procedure RdLine ( Const ALine : string );

    property LineList : TFlowLineList read FLineList;
  end;

  TProcComm = class(TLlvmProc)
  private
    FCallIdx    : Integer;
    FLastTime   : Double;

  protected
    FTextSrc,
    FTextDst    : TStringList;
    FTimeStamp  : TStringList;
    FLineA,
    FLineB,
    FLineC,
    FLineD      : TFlowLine;
    FWnd2       : TWndMove2;
    FWnd3       : TWndMove3;

    FSrcL,
    FSrcP       : Integer;
    FComment    : string;

    Function ReadSrc ( Const ALine : string ) : string;
    Function ReadSrc ( ALineIdx : Integer ) : string;
    //Procedure SplitLine ( Const ALine : string; Out AExecS : string );
    //Procedure ExtractComment ( Const ALine : string );
    Procedure AppendError ( AErrorType : char; ASrcL, ASrcP : Integer; Const AMessage : string );
    Procedure AppendError ( AErrorType : char; ALine : TFlowLine; Const AMessage : string );
    Procedure AppendError ( Const AMessage : string );
    Procedure AppendError ( AErrorType : char; Const AMessage : string );
    Function NewLabelName ( Const ASuffix : string ) : string;
    Function TypesCompatible ( Const ATypeA, ATypeB : string; Out ATypeR : string ) : boolean;
    Function ChangeTypeOpti ( Const ATarg : string; Const ATypeR : string ) : string;
    Function ProcSignedCond ( Const ACond : string; Const ATypeR : string ) : string;

    Procedure AppendExec ( Const AExec : string );
    Procedure AppendCmd ( ALine : TFlowLine );
    Procedure AppendCmd2 ( Const ACmd, ATargD, ATargS : string );
    Procedure AppendCmd3 ( Const ACmd, ATargA, ATargB, ATargC : string );
    Procedure AppendCmd ( Const ACmd, ATargD, ATargS, ATail : string );
    Procedure AppendCmd ( Const ACmd, ATargD, ATargS, ATargE, ATail : string );
    Procedure AppendLabel ( Const ALabel : string );
    Procedure AppendJmp ( Const AJmp : string; Const ALabel : string );
    Procedure AppendJmp ( Const AJmp : string; Const ALabel : string; Const ATail : string );
    Procedure AppendPush ( Const AParamType : string; Const ATarg : string );
    Procedure AppendCall ( Const AResult : string; Const AProcName : string; ADynCnt : Integer );

    Function IsAssignable ( Const ATarg : string ) : boolean;

    Function GetMask ( Const AMask : string; AIndex : Integer ) : char;
    Function CheckSetMask ( Var AMask : string; AIndex : Integer ) : boolean;
    Function CheckSetMask ( Var AMask : string; AIndex : Integer; AUsage : char ) : boolean;

    Procedure TimeStamp ( Const AReporter : string );
    Procedure DbgMarkLines ( AList : TStringList; Const AReporter : string );

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
  end;

implementation

Uses
  ConComL;

Constructor TWndMove2.Create;
Var
  BIndex        : Integer;
Begin
 Inherited;
 SetLength(FLineList,2);
 for BIndex:=0 to 1 do FLineList[BIndex]:=TFlowLine.Create;
End;

Destructor TWndMove2.Destroy;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 1 do FLineList[BIndex].Free;
 FLineList:=nil;
 Inherited;
End;

Procedure TWndMove2.Reset;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 1 do FLineList[BIndex].Reset;
End;

Procedure TWndMove2.RdLine ( Const ALine : string );
Begin
 FLineList[0].RdLine(FLineList[1].Orig);
 FLineList[1].RdLine(ALine);
End;

Constructor TWndMove3.Create;
Var
  BIndex        : Integer;
Begin
 Inherited;
 SetLength(FLineList,3);
 for BIndex:=0 to 2 do FLineList[BIndex]:=TFlowLine.Create;
End;

Destructor TWndMove3.Destroy;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 2 do FLineList[BIndex].Free;
 FLineList:=nil;
 Inherited;
End;

Procedure TWndMove3.Reset;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 2 do FLineList[BIndex].Reset;
End;

Procedure TWndMove3.RdLine ( Const ALine : string );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 1 do FLineList[BIndex].RdLine(FLineList[BIndex+1].Orig);
 FLineList[2].RdLine(ALine);
End;

Constructor TProcComm.Create;
Begin
 Inherited;
 FTextSrc:=TStringList.Create;
 FTextDst:=TStringList.Create;
 FTimeStamp:=TStringList.Create;
 FLineA:=TFlowLine.Create;
 FLineB:=TFlowLine.Create;
 FLineC:=TFlowLine.Create;
 FLineD:=TFlowLine.Create;
 FWnd2:=TWndMove2.Create;
 FWnd3:=TWndMove3.Create;
 FCallIdx:=0;
End;

Destructor TProcComm.Destroy;
Begin
 FLineD.Free;
 FLineC.Free;
 FLineB.Free;
 FLineA.Free;
 FWnd3.Free;
 FWnd2.Free;
 FTimeStamp.Free;
 FTextDst.Free;
 FTextSrc.Free;
 Inherited;
End;

Procedure TProcComm.Compile;
Begin
 Inherited;
 FTimeStamp.Clear;
 TimeStamp('Comp');
 if FModule.DbgPath<>'' then DbgSave(Self,'0_Pars',FFlowList,FALSE);
End;

Procedure TProcComm.TimeStamp ( Const AReporter : string );
Var
  BNow          : TDateTime;
Begin
 BNow:=Now;
 FTimeStamp.Append(AReporter+' '+FloatToStr(BNow)+' '+FormatDateTime('hh:nn.ss_zzz',BNow));
 if AReporter='Atom' then         FModule.CompStat.AtomTime:=FModule.CompStat.AtomTime+BNow-FLastTime
 else if AReporter='Flow' then    FModule.CompStat.FlowTime:=FModule.CompStat.FlowTime+BNow-FLastTime
 else if AReporter='Jmps' then    FModule.CompStat.JmpsTime:=FModule.CompStat.JmpsTime+BNow-FLastTime
 else if AReporter='Vars' then    FModule.CompStat.VarsTime:=FModule.CompStat.VarsTime+BNow-FLastTime
 else if AReporter='Order' then   FModule.CompStat.OrdrTime:=FModule.CompStat.OrdrTime+BNow-FLastTime
 else if AReporter='Matr' then    FModule.CompStat.MatrTime:=FModule.CompStat.MatrTime+BNow-FLastTime
 else if AReporter='Split' then   FModule.CompStat.SpltTime:=FModule.CompStat.SpltTime+BNow-FLastTime
 else if AReporter='BackEnd' then FModule.CompStat.BEndTime:=FModule.CompStat.BEndTime+BNow-FLastTime;
 FLastTime:=BNow;
End;

Procedure TProcComm.AppendError ( AErrorType : char; ASrcL, ASrcP : Integer; Const AMessage : string );
Begin
 AppendErrorA(AErrorType,ASrcL,ASrcP,FModule.Filename,AMessage);
End;

Procedure TProcComm.AppendError ( AErrorType : char; ALine : TFlowLine; Const AMessage : string );
Var
  BTail         : string;
  BSrcL,
  BSrcP         : Integer;
Begin
 BTail:=ALine.Tail;
 ParsTailExtractTagValue(BTail,'SrcPos',BSrcL,BSrcP);
 AppendErrorA(AErrorType,BSrcL,BSrcP,FModule.Filename,AMessage);
End;

Procedure TProcComm.AppendError ( AErrorType : char; Const AMessage : string );
Begin
 AppendErrorA(AErrorType,FSrcL,FSrcP,FModule.Filename,AMessage);
End;

Procedure TProcComm.AppendError ( Const AMessage : string );
Begin
 AppendError('e',AMessage);
End;

Function TProcComm.ReadSrc ( Const ALine : string ) : string;
Var
  BLine         : string;
Begin
 BLine:=ALine;
 Result:=ReadTillS(BLine,';');
 DelFirstSpace(BLine);
 FComment:=BLine;
 ParsTailExtractTagValue(BLine,'SrcPos',FSrcL,FSrcP);
End;

Function TProcComm.ReadSrc ( ALineIdx : Integer ) : string;
Begin
 Result:=ReadSrc(FTextSrc.Strings[ALineIdx]);
End;

{Procedure TProcComm.ExtractComment ( Const ALine : string );
Var
  BLineS        : string;
Begin
 BLineS:=ALine;
 ReadTillS(BLineS,';');
 FComment:=BLineS;
 RdSrcPos(FComment,FSrcL,FSrcP);
End;}

{Procedure TProcComm.SplitLine ( Const ALine : string; Out AExecS : string );
Var
  BLineS        : string;
Begin
 BLineS:=ALine;
 AExecS:=ReadTillS(BLineS,';');
 FComment:=BLineS;
 RdPosMarker(FComment,FSrcL,FSrcP);
End;}

Function TProcComm.NewLabelName ( Const ASuffix : string ) : string;
Begin
 Result:=FName+'_'+ASuffix+FModule.NextLabelUid;
End;

Function TProcComm.TypesCompatible ( Const ATypeA, ATypeB : string; Out ATypeR : string ) : boolean;
Var
  BTypeAB       : string;
Begin
 Result:=FALSE;
 ATypeR:='_';
 repeat
 if ATypeA=ATypeB then begin ATypeR:=ATypeA; Result:=TRUE; break; end;
 if ParsIsTypePointer(ATypeA) and (ATypeB='p') then begin ATypeR:='p'; Result:=TRUE; break; end;
 if ParsIsTypeStringP(ATypeA) and ((ATypeB='c') or ParsIsTypeStringP(ATypeB)) then begin ATypeR:=CTypeStringP; Result:=TRUE; break end;
 if ParsIsTypeStringP(ATypeB) and ((ATypeA='c') or ParsIsTypeStringP(ATypeA)) then begin ATypeR:=CTypeStringP; Result:=TRUE; break end;
 BTypeAB:=ATypeA+ATypeB;
 if (BTypeAB='bk') or (BTypeAB='kb') then begin ATypeR:='k'; Result:=TRUE; break; end;
 if (BTypeAB='bm') or (BTypeAB='mb') then begin ATypeR:='m'; Result:=TRUE; break; end;
 if (BTypeAB='bw') or (BTypeAB='wb') then begin ATypeR:='w'; Result:=TRUE; break; end;
 if (BTypeAB='bd') or (BTypeAB='db') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='kd') or (BTypeAB='dk') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='wd') or (BTypeAB='dw') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='md') or (BTypeAB='dm') then begin ATypeR:='d'; Result:=TRUE; break; end;
 if (BTypeAB='bi') or (BTypeAB='ib') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='ki') or (BTypeAB='ik') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='wi') or (BTypeAB='iw') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='mi') or (BTypeAB='im') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='di') or (BTypeAB='id') then begin ATypeR:='i'; Result:=TRUE; break; end;
 if (BTypeAB='bf') or (BTypeAB='fb') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='kf') or (BTypeAB='fk') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='wf') or (BTypeAB='fw') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='mf') or (BTypeAB='fm') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='df') or (BTypeAB='fd') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='if') or (BTypeAB='fi') then begin ATypeR:='f'; Result:=TRUE; break; end;
 if (BTypeAB='bC') or (BTypeAB='Cb') then begin ATypeR:='b'; Result:=TRUE; break; end;
 if (BTypeAB='iC') or (BTypeAB='Ci') then begin ATypeR:='i'; Result:=TRUE; break; end;
 until TRUE;
End;

Function TProcComm.ProcSignedCond ( Const ACond : string; Const ATypeR : string ) : string;
Begin
 Result:=ACond;
 if (ATypeR='i') or (ATypeR='f') then
  begin
  case ACond of
   'a':  Result:='g';
   'b':  Result:='s';
   'ae': Result:='ge';
   'be': Result:='se';
  end;
  end;
End;

Procedure TProcComm.AppendExec ( Const AExec : string );
Begin
 FTextDst.Append(AExec+' ;'+FComment);
End;

Procedure TProcComm.AppendCmd2 ( Const ACmd, ATargD, ATargS : string );
Begin
 AppendExec(ACmd+' '+ATargD+' '+ATargS);
End;

Procedure TProcComm.AppendCmd3 ( Const ACmd, ATargA, ATargB, ATargC : string );
Begin
 AppendExec(ACmd+' '+ATargA+' '+ATargB+' '+ATargC);
End;

Procedure TProcComm.AppendCmd ( Const ACmd, ATargD, ATargS, ATail : string );
Begin
 FTextDst.Append(ACmd+' '+ATargD+' '+ATargS+' ;'+ATail);
End;

Procedure TProcComm.AppendCmd ( Const ACmd, ATargD, ATargS, ATargE, ATail : string );
Begin
 FTextDst.Append(ACmd+' '+ATargD+' '+ATargS+' '+ATargE+' ;'+ATail);
End;

Procedure TProcComm.AppendCmd ( ALine : TFlowLine );
Begin
 AppendExec(ALine.Cmd+' '+ALine.Param[0].Targ+' '+ALine.Param[1].Targ);
End;

Procedure TProcComm.AppendJmp ( Const AJmp : string; Const ALabel : string );
Begin
 AppendCmd2(AJmp,ALabel,'');
End;

Procedure TProcComm.AppendJmp ( Const AJmp : string; Const ALabel : string; Const ATail : string );
Begin
 FTextDst.Append(AJmp+' '+ALabel+' ;'+ATail);
End;

Procedure TProcComm.AppendLabel ( Const ALabel : string );
Begin
 AppendCmd2(ALabel+':','','');
End;

Procedure TProcComm.AppendPush ( Const AParamType : string; Const ATarg : string );
Begin
 AppendCmd2('push_'+IntToStr(FCallIdx),AParamType,ATarg);
End;

Procedure TProcComm.AppendCall ( Const AResult : string; Const AProcName : string; ADynCnt :Integer );
Begin
 AppendCmd3('call_'+IntToStr(FCallIdx),AResult,AProcName,GenConstX(ADynCnt));
 inc(FCallIdx);
End;

Function TProcComm.ChangeTypeOpti ( Const ATarg : string; Const ATypeR : string ) : string;
Var
  BType         : String;
Begin
 Result:=ATarg;
 repeat
 BType:=ExtractFinalType(ATarg);
 if BType=ATypeR then break;
 Result:=AppendTmpVar(ATypeR);
 AppendCmd2('mov_'+ATypeR+BType,Result,ATarg);
 until TRUE;
End;

Function TProcComm.IsAssignable ( Const ATarg : string ) : boolean;
Var
  BSpec         : string;
Begin
 Result:=FALSE;
 repeat
 if ParsIsQ(ATarg) then begin Result:=TRUE; break; end;
 BSpec:=ParsExtractSpec(ATarg);
 if Pos('dc',BSpec)=1 then break;
 if (ATarg<>CDiscardValue) and (ParsExtractType(ATarg)='_') then break;
 if ParsIsProc(ATarg) then break;
 Result:=TRUE;
 until TRUE;
End;

Function TProcComm.GetMask ( Const AMask : string; AIndex : Integer ) : char;
Begin
 Result:=AMask[AIndex+1];
End;

Function TProcComm.CheckSetMask ( Var AMask : string; AIndex : Integer ) : boolean;
Begin
 if AMask[AIndex+1]='*' then Result:=TRUE
 else begin AMask[AIndex+1]:='*'; Result:=FALSE; end;
End;

Function TProcComm.CheckSetMask ( Var AMask : string; AIndex : Integer; AUsage : char ) : boolean;
Begin
 if AMask[AIndex+1]<>'.' then Result:=TRUE
 else begin AMask[AIndex+1]:=AUsage; Result:=FALSE; end;
End;

Procedure TProcComm.DbgMarkLines ( AList : TStringList; Const AReporter : string );
Var
  BLineIdx      : Integer;
  BLineS        : string;
  BPos          : Integer;
Begin
 for BLineIdx:=0 to AList.Count-1 do
  begin
  BLineS:=AList.Strings[BLineIdx];
  repeat
  if BLineS='' then break;
  BPos:=Pos('[SrcOrig:]',BLineS);
  if BPos=0 then break;
  Insert('[ProcNameL:'+FNameL+']['+AReporter+'Pos:'+IntToStr(BLineIdx)+']',BLineS,BPos);
  AList.Strings[BLineIdx]:=BLineS;
  until TRUE;
  end;
End;

end.

