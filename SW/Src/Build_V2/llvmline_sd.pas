unit LlvmLine_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParsBase_sd, ParsHelper_sd;

Type
  TFlowLineParam = class(TObject)
  private
    FTarg       : string;
    FArr,
    FIdx        : string;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure SetTarg ( Const ATarg : string );
    Procedure ChangeNames ( Const AOldName, ANewName : string );

    property Targ : string read FTarg write SetTarg;
    property Arr : string read FArr;
    property Idx : string read FIdx;
  end;

  TFlowLineParamList = array of TFlowLineParam;

  TFlowLine = class(TObject)
  private
    FOrig,
    FExec       : string;
    FCmd        : string;
    FParamList  : TFlowLineParamList;
    FTail       : string;
    FFixedCorr  : string; // Allows to pack "mov awx,FArray add 4"

    FLabelName  : string;
    FIsLabel,
    FIsJxx,
    FIsJmp,
    FIsPush,
    FIsCall,
    FIsNop,
    FIsClassX,
    FIsCmpTest,
    FIsLdx,
    FIsStx,
    FIsLea,
    FIsLam,
    FIsMovMr,
    FIsMovRm,
    FIsMov,
    FIsMovPart,
    FIsMovXx,
    FIsCopyRec,
    FIsCopyArr  : boolean;

    FProcessed  : boolean;
    FRemoved    : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function Clone : TFlowLine;

    Procedure Reset;
    Procedure RdLine ( Const ALine : string );
    Procedure SetCmd ( Const ACmd : string );
    Procedure CorrectCmd ( Const ANewName : string );
    Function WrLine : string;
    Function FormatBase : string;
    Function FormatNice : string;
    Function FormatMatr : string;
    Function FormatExec : string;
    Procedure TailMarkAdd ( Const AMark : string );
    Procedure TailMarkDel ( Const ATag : string );

    Procedure SetLabelName ( Const AName : string );
    Procedure SetParamA ( Const AData : string );
    Procedure InvJmp ( Const ANewLabel : string );
    Function CheckTargUsage ( Const ATarg : string ) : char;
    Function CheckVarUsage ( Const ATarg : string ) : char;
    Procedure DiscardTarg ( Const ATarg : string );

    Procedure XchgSrcDst;

    Procedure ChangeTargs ( Const AOldName, ANewName : string );
    Function TryChange ( Const AStrOld, AStrNew : string ) : boolean;
    Function IsJxxTo ( Const ALabel : string ) : boolean;

    property Orig : string read FOrig;
    property Exec : string read FExec;
    property Cmd : string read FCmd write SetCmd;
    property Param : TFlowLineParamList read FParamList;
    property Tail : string read FTail write FTail;
    property FixedCorr : string read FFixedCorr write FFixedCorr;

    property Processed : boolean read FProcessed write FProcessed;
    property Removed : boolean read FRemoved write FRemoved;

    property LabelName : string read FLabelName;
    property IsLabel   : boolean read FIsLabel;
    property IsJxx     : boolean read FIsJxx;
    property IsJmp     : boolean read FIsJmp;
    property IsPush    : boolean read FIsPush;
    property IsCall    : boolean read FIsCall;
    property IsNop     : boolean read FIsNop;
    property IsClassX  : boolean read FIsClassX;
    property IsCmpTest : boolean read FIsCmpTest;
    property IsLdx     : boolean read FIsLdx;
    property IsStx     : boolean read FIsStx;
    property IsLea     : boolean read FIsLea;
    property IsLam     : boolean read FIsLam;
    property IsMovMr   : boolean read FIsMovMr;
    property IsMovRm   : boolean read FIsMovRm;
    property IsMov     : boolean read FIsMov;
    property IsMovPart : boolean read FIsMovPart;
    property IsMovXx   : boolean read FIsMovXx;
    property IsCopyRec : boolean read FIsCopyRec;
    property IsCopyArr : boolean read FIsCopyArr;
  end;

  TFlowLineList = array of TFlowLine;

Const
  CMov_BW = ' '+
            'mov_lw mov_lm '+
            'mov_bw mov_bm '+
            'mov_cw mov_cm '+
            'mov_kw mov_km ';
  CMov_BD = ' '+
            'mov_li mov_ld mov_lq mov_lp '+
            'mov_bi mov_bd mov_bq mov_bp '+
            'mov_ci mov_cd mov_cq mov_cp '+
            'mov_ki mov_kd mov_kq mov_kp ';
  CMov_WD = ' '+
            'mov_wi mov_wd mov_wq mov_wp '+
            'mov_mi mov_md mov_mq mov_mp ';

Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer );
Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; ACount : Integer );
Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; ALine : TFlowLine );
Procedure RemoveFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer );
Procedure InsertFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; ALine : TFlowLine );
Function  InsertFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; Const ALine : string ) : TFlowLine;
Procedure AppendFlowLine ( Var ALineList : TFlowlineList; ALine : TFlowLine );

Procedure StringListToFlowList ( AStringList : TStringList; Var AFlowList : TFlowLineList );
Procedure FlowListToStringList ( AStringList : TStringList; Var AFlowList : TFlowLineList );
Procedure DestroyFlowList ( Var AFlowList : TFlowLineList );

implementation

Uses
  ConComL;

Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer );
Var
  BIndex        : Integer;
Begin
 ALineList[AIndex].Free;
 for BIndex:=AIndex+1 to Length(ALineList)-1 do ALineList[BIndex-1]:=ALineList[BIndex];
 SetLength(ALineList,Length(ALineList)-1);
End;

Procedure RemoveFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer );
Var
  BIndex        : Integer;
Begin
 for BIndex:=AIndex+1 to Length(ALineList)-1 do ALineList[BIndex-1]:=ALineList[BIndex];
 SetLength(ALineList,Length(ALineList)-1);
End;

Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; ACount : Integer );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to ACount-1 do ALineList[AIndex+BIndex].Free;
 for BIndex:=AIndex to Length(ALineList)-1-ACount do ALineList[BIndex]:=ALineList[BIndex+ACount];
 SetLength(ALineList,Length(ALineList)-ACount);
End;

Procedure DeleteFlowLine ( Var ALineList : TFlowLineList; ALine : TFlowLine );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(ALineList)-1 do
  begin
  if ALineList[BIndex]=ALine then
   begin
   DeleteFlowLine(ALineList,BIndex);
   break;
   end;
  end;
End;

Procedure InsertFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; ALine : TFlowLine );
Var
  BLen,
  BIndex        : Integer;
Begin
 BLen:=Length(ALineList);
 SetLength(ALineList,BLen+1);
 for BIndex:=BLen-1 downto AIndex do ALineList[BIndex+1]:=ALineList[BIndex];
 ALineList[AIndex]:=ALine;
End;

Function InsertFlowLine ( Var ALineList : TFlowLineList; AIndex : Integer; Const ALine : string ) : TFlowLine;
Begin
 Result:=TFlowLine.Create;
 Result.RdLine(ALine);
 InsertFlowLine(ALineList,AIndex,Result);
End;

Procedure AppendFlowLine ( Var ALineList : TFlowlineList; ALine : TFlowLine );
Var
  BLen          : Integer;
Begin
 BLen:=Length(ALineList);
 SetLength(ALineList,BLen+1);
 ALineList[BLen]:=ALine;
End;

Procedure StringListToFlowList ( AStringList : TStringList; Var AFlowList : TFlowLineList );
Var
  BIndex,
  BLen          : Integer;
  BLine         : TFlowLine;
Begin
 BLen:=AStringList.Count;
 SetLength(AFlowList,BLen);
 for BIndex:=0 to BLen-1 do
  begin
  BLine:=TFlowLine.Create;
  AFlowList[BIndex]:=BLine;
  BLine.RdLine(AStringList.Strings[BIndex]);
  end;
End;

Procedure FlowListToStringList ( AStringList : TStringList; Var AFlowList : TFlowLineList );
Var
  BIndex,
  BLen          : Integer;
Begin
 AStringList.Clear;
 BLen:=Length(AFlowList);
 for BIndex:=0 to BLen-1 do AStringList.Append(AFlowList[BIndex].WrLine);
End;

Procedure DestroyFlowList ( Var AFlowList : TFlowLineList );
Var
  BIndex,
  BLen          : Integer;
Begin
 BLen:=Length(AFlowList);
 for BIndex:=0 to BLen-1 do AFlowList[BIndex].Free;
 AFlowlist:=nil;
End;

{
 *** TFlowLineParam
}

Constructor TFlowLineParam.Create;
Begin
 Inherited;
End;

Destructor TFlowLineParam.Destroy;
Begin
 Inherited;
End;

Procedure TFlowLineParam.SetTarg ( Const ATarg : string );
Begin
 FTarg:=ATarg;
 ParsSplitArrayIdx(FTarg,FArr,FIdx);
End;

Procedure TFlowLineParam.ChangeNames ( Const AOldName, ANewName : string );
Var
  BPos  : Integer;
  BTarg : string;
Begin
 BPos:=Pos(AOldName,FTarg);
 if BPos<>0 then
  begin
  BTarg:=FTarg;
  Delete(BTarg,BPos,Length(AOldName));
  Insert(ANewName,BTarg,BPos);
  SetTarg(BTarg);
  end;
End;

{
 *** TFlowLine
}

Constructor TFlowLine.Create;
Var
  BIndex        : Integer;
Begin
 Inherited;
 SetLength(FParamList,3);
 for BIndex:=0 to 2 do FParamList[BIndex]:=TFlowLineParam.Create;
End;

Destructor TFlowLine.Destroy;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(FParamList)-1 do FParamList[BIndex].Free;
 FParamlist:=nil;
 Inherited;
End;

Function TFlowLine.Clone : TFlowLine;
Begin
 Result:=TFlowLine.Create;
 Result.RdLine(FormatBase);
End;

Procedure TFlowLine.Reset;
Begin
 RdLine('');
End;

Procedure TFlowLine.RdLine ( Const ALine : string );
Var
  BIndex        : Integer;
  BExec         : string;
  BCmd          : string;
Begin
 FOrig:=ALine;
 FTail:=FOrig;
 BExec:=ReadTillC(FTail,';'); FExec:=BExec;
 BCmd:=ReadParamStr(BExec);
 for BIndex:=0 to Length(FParamList)-1 do FParamList[BIndex].SetTarg(ReadParamStr(BExec));
 DelFirstSpace(FTail);
 SetCmd(BCmd);
End;

Procedure TFlowLine.SetCmd ( Const ACmd : string );
Var
  BPos          : Integer;
Begin
 FCmd:=ACmd;
 BPos:=Pos(':',FCmd);
 FIsLabel:=FALSE; FLabelName:='';
 if BPos<>0 then begin FIsLabel:=TRUE; FLabelName:=Copy(FCmd,1,BPos-1); end;
 FIsJxx:=Pos('j',FCmd)=1;
 FIsJmp:=FCmd='jmp';
 FIsPush:=Pos('push',FCmd)=1;
 FIsCall:=Pos('call',FCmd)=1;
 FIsNop:=Pos('nop',FCmd)=1;
 FIsClassX:=StrInList(FCmd,'add sub and or xor mul div shl shr');
 FIsCmpTest:=StrInList(FCmd,'cmp test');
 FIsLdx:=Pos('ld',FCmd)=1;
 FIsStx:=Pos('st',FCmd)=1;
 FIsLea:=FCmd='lea';
 FIsLam:=FCmd='lam';
 FIsMovMr:=FCmd='mov_mr';
 FIsMovRm:=FCmd='mov_rm';
 FIsMov:=FCmd='mov';
 FIsMovPart:=StrInList(FCmd,CMov_BW+CMov_BD+CMov_WD);
 FIsMovXx:=Pos('mov',FCmd)=1;
 FIsCopyRec:=FCmd='copy_rec';
 FIsCopyArr:=FCmd='copy_arr';
End;

Procedure TFlowLine.CorrectCmd ( Const ANewName : string );
Begin
 FCmd:=ANewName;
End;

Function TFlowLine.WrLine : string;
Var
  BParam        : TFlowLineParam;
  BIndex        : Integer;
Begin
 Result:=FCmd;
 {if FParamList[0].FArr<>'' then Result:=Result+' '+FParamList[0].FArr;
 if FParamList[0].FIdx<>'' then Result:=Result+'['+FParamList[0].FIdx+']';
 if FParamList[1].FArr<>'' then Result:=Result+' '+FParamList[1].FArr;
 if FParamList[1].FIdx<>'' then Result:=Result+'['+FParamList[1].FIdx+']';
 if FParamList[2].FArr<>'' then Result:=Result+' '+FParamList[2].FArr;
 if FParamList[2].FIdx<>'' then Result:=Result+'['+FParamList[2].FIdx+']';}
 for BIndex:=0 to Length(FParamList)-1 do
  begin
  BParam:=FParamList[BIndex];
  if BParam.FTarg<>'' then Result:=Result+' '+BParam.FTarg;
  end;
 Result:=Result+' ;'+FTail;
End;

Function TFlowLine.FormatBase : string;
Var
  BParam        : TFlowLineParam;
  BIndex        : Integer;
Begin
 Result:='';
 repeat
 if FIsLabel then begin Result:='     '+FLabelName+':'; break; end;
 Result:='        '+FCmd+' ';
 AddSpacesVarR(Result,17);
 for BIndex:=0 to Length(FParamList)-1 do
  begin
  BParam:=FParamList[BIndex];
  if BParam.FTarg<>'' then Result:=Result+BParam.FTarg+' ';
  end;
 AddSpacesVarR(Result,50);
 Result:=Result+' ;'+FTail;
 until TRUE;
End;

Function TFlowLine.FormatNice : string;
Var
  BParam        : TFlowLineParam;
  BIndex        : Integer;
  BTail,
  BRegMap       : string;
Begin
 Result:='';
 repeat
 if FIsLabel then begin Result:='     '+FLabelName+':'; break; end;
 Result:='        '+FCmd+' ';
 AddSpacesVarR(Result,17);
 for BIndex:=0 to Length(FParamList)-1 do
  begin
  BParam:=FParamList[BIndex];
  if BParam.FTarg<>'' then Result:=Result+BParam.FTarg+' ';
  end;
 AddSpacesVarR(Result,50);
 BTail:=FTail;
 BRegMap:=CheckTag('M',BTail);
 Result:=Result+' ;'+BTail;
 if BRegMap<>'' then
  begin
  AddSpacesVarR(Result,73);
  Result:=Result+BRegMap;
  end;
 until TRUE;
End;

Function TFlowLine.FormatMatr : string;
Var
  BParam        : TFlowLineParam;
  BIndex        : Integer;
  BTail,
  BUseMatr      : string;
Begin
 Result:='';
 repeat
 if FIsLabel then begin Result:='     '+FLabelName+':'; break; end;
 Result:='        '+FCmd+' ';
 AddSpacesVarR(Result,17);
 for BIndex:=0 to Length(FParamList)-1 do
  begin
  BParam:=FParamList[BIndex];
  if BParam.FTarg<>'' then Result:=Result+BParam.FTarg+' ';
  end;
 AddSpacesVarR(Result,64);
 BTail:=FTail;
 BUseMatr:=CheckTag('UseMatr',BTail);
 Result:=Result+' '+BUseMatr;
{ if BRegMap<>'' then
  begin
  AddSpacesVarR(Result,73);
  Result:=Result+BRegMap;
  end;}
 until TRUE;
End;

Function TFlowLine.FormatExec : string;
Var
  BParam        : TFlowLineParam;
  BIndex        : Integer;
Begin
 Result:='';
 repeat
 if FIsLabel then begin Result:=FLabelName+':'; break; end;
 Result:=FCmd+' ';
 for BIndex:=0 to Length(FParamList)-1 do
  begin
  BParam:=FParamList[BIndex];
  if BParam.FTarg<>'' then Result:=Result+BParam.FTarg+' ';
  end;
 until TRUE;
End;

Procedure TFlowLine.TailMarkAdd ( Const AMark : string );
Var
  BPos          : Integer;
Begin
 BPos:=Pos('[SrcOrig:]',FTail);
 if BPos<>0 then Insert(AMark,FTail,BPos);
End;

Procedure TFlowLine.TailMarkDel ( Const ATag : string );
Begin
 CheckTag(ATag,FTail);
End;

Procedure TFlowLine.SetLabelName ( Const AName : string );
Begin
 FLabelName:=AName;
 FCmd:=FLabelName+':';
End;

Procedure TFlowLine.SetParamA ( Const AData : string );
Begin
 FParamList[0].SetTarg(AData);
End;

Procedure TFlowLine.InvJmp ( Const ANewLabel : string );
Begin
 if FCmd='je' then FCmd:='jne'
 else if FCmd='jne' then FCmd:='je'
 else if FCmd='ja' then FCmd:='jbe'
 else if FCmd='jb' then FCmd:='jae'
 else if FCmd='jae' then FCmd:='jb'
 else if FCmd='jbe' then FCmd:='ja'
 else if FCmd='jg' then FCmd:='jse'
 else if FCmd='js' then FCmd:='jge'
 else if FCmd='jge' then FCmd:='js'
 else if FCmd='jse' then FCmd:='jg'
 else FCmd:='j_invalid';
 FParamList[0].FTarg:=ANewLabel;
End;

Procedure TFlowLine.XchgSrcDst;
Var
  BParam        : TFlowLineParam;
Begin
 BParam:=FParamList[0]; FParamList[0]:=FParamList[1]; FParamList[1]:=BParam;
End;

Procedure TFlowLine.ChangeTargs ( Const AOldName, ANewName : string );
Var
  BParamIdx     : Integer;
Begin
 for BParamIdx:=0 to Length(FParamList)-1 do FParamList[BParamIdx].ChangeNames(AOldName,ANewName);
End;

Function TFlowLine.TryChange ( Const AStrOld, AStrNew : string ) : boolean;
Var
  BPos          : Integer;
  BOrig         : string;
Begin
 Result:=FALSE;
 repeat
 BPos:=Pos(AStrOld,FOrig);
 if BPos=0 then break;
 Result:=TRUE;
 BOrig:=FOrig;
 Delete(BOrig,BPos,Length(AStrOld));
 Insert(AStrNew,BOrig,BPos);
 RdLine(BOrig);
 until FALSE;
End;

Function TFlowLine.IsJxxTo ( Const ALabel : string ) : boolean;
Begin
 Result:=FIsJxx and (FParamList[0].FTarg=ALabel);
End;

Procedure AppendUsage ( Var AUsage : char; AUsageToAdd : char );
Begin
 if AUsage='.' then AUsage:=AUsageToAdd
 else if AUsageToAdd='x' then AUsage:='x'
 else if (AUsage='s') and (AUsageToAdd='d') then AUsage:='x'
 else if (AUsage='d') and (AUsageToAdd='s') then AUsage:='x';
End;

{Function TFlowLine.CheckTargUsage ( Const ATarg : string ) : char;
Begin
 Result:='.';
 repeat
 if FIsNop then break;
 if FIsLabel or FIsJxx then break;
 if FIsPush then
  begin
  if FParamList[1].FTarg<>ATarg then break;
  Result:='s';
  break;
  end;
 if FIsCall then
  begin
  if FParamList[0].FTarg<>ATarg then break;
  Result:='d';
  break;
  end;

 if FIsCmpTest then begin if (ATarg=FParamList[0].FArr) or (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) then Result:='s'; break; end;
 if FIsLdx then begin if ATarg=FParamList[0].FTarg then Result:='d'; break; end;
 if FIsStx then begin if ATarg=FParamList[0].FTarg then Result:='s'; break; end;

 if FIsLea or FIsLam then
  begin
  if ATarg=FParamList[0].FTarg then Result:='d';
  if (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) then Result:='s';
  end
 else if FIsMovMr then
  begin
  if (ATarg=FParamList[0].FArr) or (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) or (ATarg=FParamList[2].FArr) or (ATarg=FParamList[2].FIdx) then AppendUsage(Result,'s');
  end
 else if FIsMovRm then
  begin
  if ATarg=FParamList[0].FArr then AppendUsage(Result,'d');
  if (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) or (ATarg=FParamList[2].FArr) or (ATarg=FParamList[2].FIdx) then AppendUsage(Result,'s');
  end
 else if FIsMov then
  begin
  if ATarg=FParamList[0].FArr then AppendUsage(Result,'d');
  if ATarg=FParamList[1].FArr then AppendUsage(Result,'s');
  end
 else
  begin
  if ATarg=FParamList[0].FArr then
   begin
   if FParamList[0].FIdx='' then AppendUsage(Result,'x') else AppendUsage(Result,'s');
   end;
  if (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) or (ATarg=FParamList[2].FArr) or (ATarg=FParamList[2].FIdx) then AppendUsage(Result,'s');
  end;
 until TRUE;
End;}

Function TFlowLine.CheckTargUsage ( Const ATarg : string ) : char;
Begin
 Result:='.';
 repeat
 if FIsNop then break;
 if FIsLabel or FIsJxx then break;
 if FIsPush then
  begin
  if FParamList[1].FTarg<>ATarg then break;
  Result:='s';
  break;
  end;
 if FIsCall then
  begin
  if FParamList[0].FTarg<>ATarg then break;
  Result:='d';
  break;
  end;

 if FIsCmpTest then begin if (ATarg=FParamList[0].FArr) or (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) then Result:='s'; break; end;
 if FIsLdx then begin if ATarg=FParamList[0].FTarg then Result:='d'; break; end;
 if FIsStx then begin if ATarg=FParamList[0].FTarg then Result:='s'; break; end;

 if FIsLea or FIsLam then
  begin
  if ATarg=FParamList[0].FTarg then Result:='d';
  if (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) then Result:='s';
  end
 else if FIsMovXx then
  begin
  if ATarg=FParamList[0].FArr then
   begin
   if FParamList[0].FIdx='' then AppendUsage(Result,'d') else AppendUsage(Result,'s');
   end;
  if (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) or (ATarg=FParamList[2].FArr) or (ATarg=FParamList[2].FIdx) then AppendUsage(Result,'s');
  end
 else
  begin
  if ATarg=FParamList[0].FArr then
   begin
   if FParamList[0].FIdx='' then AppendUsage(Result,'x') else AppendUsage(Result,'s');
   end;
  if (ATarg=FParamList[0].FIdx) or (ATarg=FParamList[1].FArr) or (ATarg=FParamList[1].FIdx) or (ATarg=FParamList[2].FArr) or (ATarg=FParamList[2].FIdx) then AppendUsage(Result,'s');
  end;
 until TRUE;
End;

Function TFlowLine.CheckVarUsage ( Const ATarg : string ) : char;
Begin
 Result:='.';
 repeat
 if FIsNop then break;
 if FIsLabel or FIsJxx then break;
 if FIsPush then
  begin
  if FParamList[1].FTarg<>ATarg then break;
  Result:='s';
  break;
  end;
 if FIsCall then
  begin
  if FParamList[0].FTarg<>ATarg then break;
  Result:='d';
  break;
  end;

 if FIsCmpTest then begin if (ATarg=FParamList[0].FTarg) or (ATarg=FParamList[1].FTarg) then Result:='s'; break; end;
 if FIsLdx then begin if ATarg=FParamList[0].FTarg then Result:='d'; break; end;
 if FIsStx then begin if ATarg=FParamList[0].FTarg then Result:='s'; break; end;

 if FIsLea or FIsLam then
  begin
  if ATarg=FParamList[0].FTarg then Result:='d';
  if ATarg=FParamList[1].FTarg then Result:='s';
  end
 else if FIsMovRm then
  begin
  if (ATarg=FParamList[0].FTarg) and (ATarg=FParamList[1].FIdx) then Result:='x'
  else if ATarg=FParamList[0].FTarg then Result:='d'
  else if ATarg=FParamList[1].FIdx then Result:='s'
  else if (ATarg=FParamList[1].FArr) and ParsIsLocalOrTmp(FParamList[1].FArr){ and (ParsExtractType(BTargArr)='p')} then Result:='s';
  end
 else if FIsMovMr then
  begin
  if ATarg=FParamList[1].FTarg then Result:='s'
  else if ATarg=FParamList[0].FIdx then Result:='s'
  else if (ATarg=FParamList[0].FArr) and ParsIsLocalOrTmp(FParamList[0].FArr){ and (ParsExtractType(BTargArr)='p')} then Result:='s';
  end
 else if FisMovXx then
  begin
  if (ATarg=FParamList[0].FTarg) and (ATarg=FParamList[1].FTarg) then Result:='x'
  else if ATarg=FParamList[0].FTarg then Result:='d'
  else if ATarg=FParamList[1].FTarg then Result:='s';
  end
 else
  begin
  if (ATarg=FParamList[0].FTarg) and (ATarg=FParamList[1].FTarg) then Result:='x'
  else if ATarg=FParamList[0].FTarg then Result:='x'
  else if ATarg=FParamList[1].FTarg then Result:='s';
  end;
 until TRUE;
End;

Procedure TFlowLine.DiscardTarg ( Const ATarg : string );
Begin
 if FIsCall and (FParamList[0].FTarg=ATarg) then FParamList[0].FTarg:=CDiscardValue;
End;

end.

