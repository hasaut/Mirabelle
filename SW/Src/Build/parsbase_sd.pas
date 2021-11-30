unit ParsBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConComL, ParsHelper_sd, LlvmBase_sd, AsmTypes_sd;

Type
  TParsBase = class;

  TIncType = (itNone, itPasUnit, itCppSystem, itCppUser, itPyUnit, itRustUnit);
  TGetUses = Function ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TLlvmModule of Object;
  TReadInc = Function ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TParsBase of Object;
  TParsState = (psUnknown, psParsStart, psParsEnd);

  TParsCursor = record
    FLine,
    FPos        : Integer;
  end;

  TParsBase = Class(TObject)
  private
  protected
    FFilename   : string;
    FSrc        : TStringList;
    FModule     : TLlvmModule;
    FDstPath    : string;

    FGetUses    : TGetUses;
    FReadInc    : TReadInc;
    FParsState  : TParsState;

    FRdQueue    : array [0..2] of TParsToken;
    FTokenThis  : TParsToken;
    FRdQueLen   : Integer;
    FParsCursor : TParsCursor;
    FParsOrig   : string;

    FBufSrcS,                   // Source buffer
    FBufDefS    : string;       // Define buffer
    FBufSrcLen  : Integer;
    FDefineCmp  : string;       // Used to check define loops
    FPreprState : string;

    FOneSymbol          : string;
    FSymbolList         : string;
    FStrQuotes          : string;
    FCommentLevel       : string;

    FWithList   : string;

    Function SensCase ( Const ADataS : string ) : string; Virtual; Abstract;

    Function StrToOpcode ( Const AOpcodeS : string ) : string; Virtual; Abstract;
    Function IsReservedWord ( Const AName : string ) : boolean; Virtual; Abstract;
    Function IsSymbol ( ASymbol : char ) : boolean;
    Function IsTargConstant ( Const ANameS : string ) : string; Virtual; Abstract;
    Function LuBufS ( AOffset : Integer ) : char;
    Procedure MoveBufPtr ( AJmpNextLine : boolean );
    Procedure SkipLine;
    Procedure RdTextA ( AJmpNextLine : boolean );
    Procedure RdTextFC ( AJmpNextLine : boolean ); Virtual; Abstract; // Filter comments
    Procedure RdTextFD ( AJmpNextLine : boolean ); Virtual; Abstract; // Filter comments and resolve defines
    Function RdTextSOpti ( AJmpNextLine : boolean ) : string;
    Function RdTextS : string;
    Function RdSameLine : string;
    Function RdSameLine ( ATerminator : char ) : string;
    Function LuTextS : string;
    Function LuTextS2 : string;
    Procedure SkipText ( Const AEndMarker : string ); // Skip some text due to error. For ex. we can go directly to ')' or ';'
    Procedure ResetDefineCmp;
    Procedure PushDefine ( Const ADefineB : string );
    Function PushDefine ( Const ADefineA, ADefineB : string ) : boolean;
    Function ProcConst2 ( Const ADataA, ADataB : string; Const AOper : string ) : Integer;
    Function RdConstI ( Out AData : Integer ) : boolean;

    Procedure AppendErrorA ( AErrorType : char; ALine, APos : Integer; Const AComment : string ); Virtual;
    Procedure AppendError ( AErrorType : char; ALine, APos : Integer; Const AComment : string );
    Procedure AppendError ( AErrorType : char; Const AComment : string );

    Function IsAllowedName ( Const AName : string ) : boolean;

    Function GenTail : string;
    Function IsAllProcImplementedOpti ( Const AList : TLlvmObjList ) : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function GetTarg ( AProc : TLlvmProc; Const ANameS : string ) : string;

    Procedure Init ( Const AFilename, ADstPath : TFilename; ASrc : TStringList );
    Procedure Parse ( AModule : TLlvmModule; AIsInclude : boolean ); Virtual;

    property GetUses : TGetUses read FGetUses write FGetUses;
    property ReadInc : TReadInc read FReadInc write FReadInc;
    property ParsState : TParsState read FParsState;
  end;

Const
  CInvalidIdentifier    = CTagS+'___'+CTagM+'Invalid'+CTagE;
  CDiscardValue         = CTagS+'___'+CTagM+'Discard'+CTagE;
  CBooleanFalse         = CTagS+'c_l'+CTagM+'0'+CTagE;
  CBooleanTrue          = CTagS+'c_l'+CTagM+'1'+CTagE;
  CConstZero            = CTagS+'c_b'+CTagM+'0'+CTagE;
  CConstOne             = CTagS+'c_b'+CTagM+'1'+CTagE;
  CPointerNil           = CTagS+'c_p'+CTagM+'0'+CTagE;
  CConstSizeOfType      = CTagS+'c__'+CTagM+'SizeOfType'+CTagE;
  CTagParamCnt          = CTagS+'da*'+CTagM+'ParamCnt'+CTagE;
  CTypeStringP          = 'sp255e';
  CInvalidOpcode        = '~?~';

  CDirectiveInc         = CTagS+'x__{dv_}'+CTagM+'Directive@_Inc'+CTagE;
  CDirectiveDec         = CTagS+'x__{dv_}'+CTagM+'Directive@_Dec'+CTagE;
  CDirectiveChr         = CTagS+'x_c{dai}'+CTagM+'Directive@_Chr'+CTagE;
  CDirectiveOrd         = CTagS+'x_b{dac}'+CTagM+'Directive@_Ord'+CTagE;

  CBuiltInVaList        = CTagS+'t__'+CTagM+'__builtin_va_list'+CTagP+'dv_'+CTagP+'dv_'+CTagP+CTagE;

  CDiscardStrSearch     = CTagS+'tb_'+CTagM+CTagE+' type ';

  CNullParamName        = '*';

Function GenConstX ( ADataI : Integer ) : string;

implementation

Function GenConstX ( ADataI : Integer ) : string;
Begin
 Result:=CTagS+'c__'+CTagM+IntToStr(ADataI)+CTagE;
End;

Constructor TParsBase.Create;
Var
  BIndex        : Integer;
Begin
 Inherited;
 for BIndex:=0 to 2 do FRdQueue[BIndex]:=TParsToken.Create;
 FTokenThis:=FRdQueue[0];
 FRdQueLen:=0;
 FSymbolList:=#32+';.,:-+=/*()[]<>{}!|&~?^@';
End;

Destructor TParsBase.Destroy;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to 2 do FRdQueue[BIndex].Free;
 Inherited;
End;

Procedure TParsBase.Init ( Const AFilename, ADstPath : TFilename; ASrc : TStringList );
Begin
 FFilename:=AFilename;
 FDstPath:=ADstPath;
 FSrc:=ASrc;
End;

Procedure TParsBase.Parse ( AModule : TLlvmModule; AIsInclude : boolean );
Begin
 FParsState:=psUnknown;
 FModule:=AModule;
 FParsCursor.FLine:=0; FParsCursor.FPos:=0; FParsOrig:='';
 if FSrc.Count=0 then FBufSrcS:=''
 else FBufSrcS:=FSrc.Strings[0];
 FBufDefS:='';
 FBufSrcLen:=Length(FBufSrcS);
 FPreprState:='';
 FRdQueLen:=0;
End;

Function TParsBase.IsSymbol ( ASymbol : char ) : boolean;
Begin
 Result:=Pos(ASymbol,FSymbolList)<>0;
End;

Function TParsBase.LuBufS ( AOffset : Integer ) : char;
Begin
 Result:=#0;
 if FBufDefS<>'' then
  begin
  if AOffset<Length(FBufDefS) then Result:=FBufDefS[AOffset+1];
  end
 else if (FParsCursor.FPos+AOffset)<FBufSrcLen then Result:=FBufSrcS[FParsCursor.FPos+AOffset+1];
End;

Procedure TParsBase.MoveBufPtr ( AJmpNextLine : boolean );
Begin
 repeat
 if FBufDefS<>'' then
  begin
  Delete(FBufDefS,1,1);
  if FBufDefS<>'' then break;
  end
 else if FParsCursor.FPos<FBufSrcLen then inc(FParsCursor.FPos);
 if FParsCursor.FPos<FBufSrcLen then break;
 if AJmpNextLine=FALSE then break;
 if (FParsCursor.FLine+1)>=FSrc.Count then break;
 FParsCursor.FPos:=0; inc(FParsCursor.FLine);
 FBufSrcS:='';
 while FParsCursor.FLine<FSrc.Count do
  begin
  FBufSrcS:=FSrc.Strings[FParsCursor.FLine];
  if FBufSrcS<>'' then break;
  inc(FParsCursor.FLine);
  end;
 FBufSrcLen:=Length(FBufSrcS);
 until TRUE;
End;

Procedure TParsBase.SkipLine;
Begin
 repeat
 FBufDefS:='';
 FParsCursor.FPos:=FBufSrcLen;
 if (FParsCursor.FLine+1)>=FSrc.Count then break;
 FParsCursor.FPos:=0; inc(FParsCursor.FLine);
 FBufSrcS:='';
 while FParsCursor.FLine<FSrc.Count do
  begin
  FBufSrcS:=FSrc.Strings[FParsCursor.FLine];
  if FBufSrcS<>'' then break;
  inc(FParsCursor.FLine);
  end;
 FBufSrcLen:=Length(FBufSrcS);
 until TRUE;
End;

Procedure TParsBase.RdTextA ( AJmpNextLine : boolean );
Var
  BDataC        : char;
  BResultS      : string;
Begin
 BResultS:='';

 repeat
 BDataC:=LuBufS(0);
 if (BDataC=#0) and (AJmpNextLine) then
  begin
  MoveBufPtr(TRUE);
  BDataC:=LuBufS(0);
  end;

 if BDataC=#0 then break;

 while BDataC=#32 do
  begin
  MoveBufPtr(AJmpNextLine);
  BDataC:=LuBufS(0);
  if BDataC=#0 then break;
  end;

 if BDataC=#0 then break;

 BResultS:=BDataC; MoveBufPtr(FALSE);

 BDataC:=LuBufS(0);

 if BDataC=#0 then break;

 if Pos(BResultS[1],FStrQuotes)<>0 then
  begin
  BResultS:=BResultS+BDataC; MoveBufPtr(FALSE);
  if Pos(BDataC,FStrQuotes)<>0 then break;
  repeat
  BDataC:=LuBufS(0);
  if BDataC=#0 then break;
  BResultS:=BResultS+BDataC; MoveBufPtr(FALSE);
  if Pos(BDataC,FStrQuotes)<>0 then break;
  until FALSE;
  break;
  end;

 if BDataC=#32 then break;

 if StrInList(BResultS+BDataC+LuBufS(1),FOneSymbol) then
  begin
  BResultS:=BResultS+BDataC+LuBufS(1); MoveBufPtr(FALSE); MoveBufPtr(FALSE);
  break;
  end;

 if StrInList(BResultS+BDataC,FOneSymbol) then
  begin
  BResultS:=BResultS+BDataC; MoveBufPtr(FALSE);
  repeat
  BDataC:=LuBufS(0);
  if BDataC=#0 then break;
  if BDataC=#32 then break;
  if StrInList(BResultS+BDataC,FOneSymbol)=FALSE then break;
  BResultS:=BResultS+BDataC; MoveBufPtr(FALSE);
  until FALSE;
  break;
  end;

 if IsSymbol(BResultS[1]) then break;

 repeat
 if (BDataC='.') and (BResultS[1] in ['0'..'9']) and (Pos('0x',BResultS)=0) and (LuBufS(1)<>'.') then
 else if (BDataC in ['e', 'E']) and (BResultS[1] in ['0'..'9']) and (Pos('0x',BResultS)=0) then
 else if (BDataC in ['+', '-']) and (BResultS[1] in ['0'..'9']) and (Pos('0x',BResultS)=0) and (Length(BResultS)>=2) and (BResultS[Length(BResultS)] in ['e', 'E']) then
 else if IsSymbol(BDataC) then break;
 BResultS:=BResultS+BDataC; MoveBufPtr(FALSE);
 BDataC:=LuBufS(0);
 if BDataC=#0 then break;
 until FALSE;

 until TRUE;

 FParsOrig:=BResultS;
End;

Function TParsBase.RdTextSOpti ( AJmpNextLine : boolean ) : string;
Var
  BIndex        : Integer;
Begin
 if FRdQueLen>1 then
  begin
  for BIndex:=1 to (FRdQueLen-1) do FRdQueue[BIndex-1].Import(FRdQueue[BIndex]);
  dec(FRdQueLen);
  end
 else
  begin
  RdTextFD(AJmpNextLine);
  FRdQueue[0].Orig:=FParsOrig;
  FRdQueue[0].Line:=FParsCursor.FLine;
  FRdQueue[0].Pos:=FParsCursor.FPos;
  FRdQueLen:=1;
  end;
 FTokenThis:=FRdQueue[0];
 Result:=FRdQueue[0].Orig;
End;

Function TParsBase.RdTextS : string;
Begin
 Result:=RdTextSOpti(TRUE);
End;

Function TParsBase.RdSameLine : string;
Begin
 Result:=RdTextSOpti(FALSE);
End;

Function TParsBase.RdSameLine ( ATerminator : char ) : string;
Var
  BDataC        : char;
Begin
 Result:='';
 repeat
 BDataC:=LuBufS(0);
 if BDataC=#0 then break;
 Result:=Result+BDataC; MoveBufPtr(FALSE);
 if BDataC=ATerminator then break;
 until FALSE;
End;

Function TParsBase.LuTextS : string;
Begin
 if FRdQueLen<2 then
  begin
  RdTextFD(TRUE);
  FRdQueue[1].Orig:=FParsOrig;
  FRdQueue[1].Line:=FParsCursor.FLine;
  FRdQueue[1].Pos:=FParsCursor.FPos;
  FRdQueLen:=2;
  end;
 FTokenThis:=FRdQueue[1];
 Result:=FRdQueue[1].Orig;
End;

Function TParsBase.LuTextS2 : string;
Begin
 if FRdQueLen<3 then
  begin
  RdTextFD(TRUE);
  FRdQueue[2].Orig:=FParsOrig;
  FRdQueue[2].Line:=FParsCursor.FLine;
  FRdQueue[2].Pos:=FParsCursor.FPos;
  FRdQueLen:=3;
  end;
 FTokenThis:=FRdQueue[2];
 Result:=FRdQueue[2].Orig;
End;

Procedure TParsBase.SkipText ( Const AEndMarker : string );
Var
  BReadS        : string;
Begin
 repeat
 BReadS:=LuTextS;
 if (BReadS='') or StrInList(BReadS,AEndMarker) then break;
 RdTextS;
 until FALSE;
End;

Procedure TParsBase.ResetDefineCmp;
Begin
 FDefineCmp:='';
End;

Procedure TParsBase.PushDefine ( Const ADefineB : string );
Begin
 if FBufDefS='' then FBufDefS:=ADefineB
 else FBufDefS:=ADefineB+' '+FBufDefS;
End;

Function TParsBase.PushDefine ( Const ADefineA, ADefineB : string ) : boolean;
Begin
 Result:=Pos(' '+ADefineA+' ',' '+FDefineCmp+' ')=0;
 FDefineCmp:=FDefineCmp+ADefineA+' ';
 PushDefine(ADefineB);
End;

Function TParsBase.ProcConst2 ( Const ADataA, ADataB : string; Const AOper : string ) : Integer;
Var
  BValueA,
  BValueB   : Int64;
Begin
 Result:=0;

 if TryStrToInt0x(ADataA,BValueA)=FALSE then AppendError('e','Error converting string '+ADataA+' to value [R:TParsBase.ProcConst2]');
 if TryStrToInt0x(ADataB,BValueB)=FALSE then AppendError('e','Error converting string '+ADataB+' to value [R:TParsBase.ProcConst2]');

 case AOper of
   'add': Result:=BValueA+BValueB;
   'sub': Result:=BValueA-BValueB;
   'mul': Result:=BValueA*BValueB;
   'div': begin
          if BValueB=0 then begin Result:=0; AppendError('e','Zero division [R:TParsBase.ProcConst2]'); end
          else Result:=BValueA div BValueB;
          end;
   'shl': Result:=BValueA shl BValueB;
   'shr': Result:=BValueA shr BValueB;
   else AppendError('e','Operation '+AOper+' is not supported [R:TParsBase.ProcConst2]');
  end;
End;

Function TParsBase.RdConstI ( Out AData : Integer ) : boolean;
Var
  BEval,
  BSubEval  : string;
  BPos      : Integer;
  BReadS    : string;
  BData     : Integer;
  BOperS    : string;
  BDataA,
  BDataB    : string;
Begin
 AData:=0;
 BEval:='';
 Result:=TRUE;
 repeat
 BReadS:=LuTextS;
 if StrInList(BReadS,'; ) , ]') then begin Result:=FALSE; break; end;
 RdTextS;
 if BReadS='(' then
  begin
  if RdConstI(BData)=FALSE then Result:=FALSE;
  BEval:=BEval+IntToStr(BData)+' ';
  if RdTextS<>')' then AppendError('e','")" expected \r found [R:TParsBase.RdConstI]');
  end
 else BEval:=BEval+BReadS+' ';
 BReadS:=LuTextS;
 if StrInList(BReadS,'; ) , ]') then break;
 RdTextS;
 BOperS:=StrToOpcode(BReadS);
 if BOperS='' then begin AppendError('e','Invalid operation \r [R:TParsBase.RdConstI]'); Result:=FALSE; end;
 BEval:=BEval+BOperS+' ';
 until FALSE;

 repeat
 if Result=FALSE then break;

 Result:=FALSE;
 repeat
 ExtractEvalPair(BEval,BSubEval,' mul div ',BPos);
 if BSubEval='' then break;
 BDataA:=ReadParamStr(BSubEval);
 BOperS:=ReadParamStr(BSubEval);
 BDataB:=ReadParamStr(BSubEval);
 BData:=ProcConst2(BDataA,BDataB,BOperS);
 insert(IntToStr(BData),BEval,BPos);
 until FALSE;

 repeat
 ExtractEvalPair(BEval,BSubEval,' add sub shl shr ',BPos);
 if BSubEval='' then break;
 BDataA:=ReadParamStr(BSubEval);
 BOperS:=ReadParamStr(BSubEval);
 BDataB:=ReadParamStr(BSubEval);
 BData:=ProcConst2(BDataA,BDataB,BOperS);
 insert(IntToStr(BData),BEval,BPos);
 until FALSE;

 BDataA:=ReadParamStr(BEval);
 BOperS:=ReadParamStr(BEval);
 if BOperS<>'' then begin AppendError('e','Extra operation or identifier "'+BOperS+'" [R:TParsBase.RdConstI]'); break; end;

 Result:=TryStrToInt(BDataA,AData);
 until TRUE;
End;

Procedure TParsBase.AppendErrorA ( AErrorType : char; ALine, APos : Integer; Const AComment : string );
Var
  BComment      : string;
  BPos          : Integer;

Begin
 BComment:=AComment;

 repeat
 BPos:=Pos('\r',BComment);
 if BPos<=0 then break;
 Delete(BComment,BPos,2);
 Insert('"'+FRdQueue[0].Orig+'"',BComment,BPos);
 until FALSE;

 repeat
 BPos:=Pos('\l',BComment);
 if BPos<=0 then break;
 Delete(BComment,BPos,2);
 Insert('"'+FRdQueue[1].Orig+'"',BComment,BPos);
 until FALSE;

 if FModule<>nil then FModule.AppendErrorA(AErrorType,ALine+1,APos+1,FFilename,BComment);
End;

Procedure TParsBase.AppendError ( AErrorType : char; ALine, APos : Integer; Const AComment : string );
Begin
 AppendErrorA(AErrorType,ALine,APos,AComment);
End;

Procedure TParsBase.AppendError ( AErrorType : char; Const AComment : string );
Var
  BLineS        : string;
Begin
 BLineS:='';
 if FTokenThis.Line<FSrc.Count then begin BLineS:=FSrc.Strings[FTokenThis.Line]; DelFirstSpace(BLineS); end;
 if BLineS='' then AppendError(AErrorType,FTokenThis.Line,FTokenThis.Pos,AComment)
 else AppendError(AErrorType,FTokenThis.Line,FTokenThis.Pos,AComment+'   -> "'+BLineS+'"');
End;

Function TParsBase.IsAllowedName ( Const AName : string ) : boolean;
Var
  BNameS        : string;

Begin
 Result:=FALSE;

 BNameS:=LowerCase(AName);
 repeat
 if BNameS='' then break;
 if BNameS[1] in ['a'..'z','_'] then
 else break;
 if IsReservedWord(BNameS) then break;
 Result:=TRUE;
 until TRUE;
End;

Function TParsBase.GenTail : string;
Var
  BLineS        : string;
Begin
 BLineS:='';
 if FRdQueue[0].Line<FSrc.Count then begin BLineS:=FSrc.Strings[FRdQueue[0].Line]; DelFirstSpace(BLineS); end;
 Result:=' ; '+ParsTailGen(FFilename,FRdQueue[0].Line+1,FRdQueue[0].Pos+1,BLineS);
End;

Function TParsBase.IsAllProcImplementedOpti ( Const AList : TLlvmObjList ) : boolean;
Var
  BIndex        : Integer;
  BProc         : TLlvmProc;
  BMessage      : string;
Begin
 Result:=TRUE;
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  BProc:=TLlvmProc(AList[BIndex]);
  if BProc.IsImplemented=FALSE then
   begin
   BMessage:='Declared Procedure/Function '+BProc.Name+' is not implemented';
   AppendError('e',BProc.DeclLine,BProc.DeclPos,BMessage);
   Result:=FALSE;
   end;
  inc(BIndex);
  end;
End;

Function TParsBase.GetTarg ( AProc : TLlvmProc; Const ANameS : string ) : string;
Var
  BReadS        : string;
  BNameS        : string;
  BVisibleNames : string;
  BForward,
  BTypeS        : string;
  BWithList     : string;
Begin
 Result:=ANameS;
 repeat
 if AProc=nil then
  begin
  AppendError('e','Internal error AProc=nil [R:ParsBase.GetTarg]');
  break;
  end;

 repeat
 BNameS:=AProc.Module.ResolveAlias(ANameS,TRUE);
 Result:=IsTargConstant(BNameS);
 if Result<>'' then break;
 BVisibleNames:=AProc.RetListS+AProc.VarListS+AProc.ParListS+AProc.Module.PrivateNames+AProc.Module.PublicNames+AProc.Module.ExternNames;
 while BVisibleNames<>'' do
  begin
  BReadS:=ReadParamStr(BVisibleNames); if BReadS='' then break;
  if Pos(CTagM+SensCase(BNameS)+CTagE,SensCase(BReadS))<>0 then begin Result:=BReadS; break; end;
  if Pos(CTagM+SensCase(BNameS)+CTagP,SensCase(BReadS))<>0 then begin Result:=BReadS; break; end;
  end;
 if Result='' then
  begin
  BVisibleNames:=AProc.Module.VisibleNames;
  while BVisibleNames<>'' do
   begin
   BReadS:=ReadParamStr(BVisibleNames); if BReadS='' then break;
   if Pos(CTagM+SensCase(BNameS)+CTagE,SensCase(BReadS))<>0 then begin Result:=BReadS; break; end;
   if Pos(CTagM+SensCase(BNameS)+CTagP,SensCase(BReadS))<>0 then begin Result:=BReadS; break; end;
   end;
  if Result<>'' then AProc.Module.AppendExternName(Result);
  end;
 BVisibleNames:=FWithList; BWithList:='';
 while BVisibleNames<>'' do
  begin
  BReadS:=ReadParamStr(BVisibleNames); if BReadS='' then break;
  BTypeS:=AProc.ExtractFinalType(BReadS);
  ParsGetFieldType(BTypeS,BNameS);
  if BTypeS<>'' then BWithList:=BWithList+BReadS+' ';
  end;
 if Result<>'' then
  begin
  if BWithList<>'' then AppendError('e','Name '+BNameS+' can conflict with a field[s] of with statement '+BWithList+' [R:ParsBase.GetTarg]');
  end
 else if BWithList='' then
  begin
  AppendError('e','Unknown identifier '+BNameS+' [R:ParsBase.GetTarg]');
  Result:=CInvalidIdentifier;
  end
 else
  begin
  Result:=ReadParamStr(BWithList)+'.'+BNameS;
  DelFirstSpace(BWithList);
  if BWithList<>'' then AppendError('e','Name '+BNameS+' exists on another records of with statement '+BWithList+' [R:ParsBase.GetTarg]');
  end;
 until TRUE;

 BTypeS:=ParsExtractType(Result);
 BForward:=ParsCheckGetForwardPointer(BTypeS);
 if BForward<>'' then
  begin
  BVisibleNames:=AProc.Module.PrivateNames+AProc.Module.PublicNames+AProc.Module.VisibleNames;
  BReadS:='';
  while BVisibleNames<>'' do
   begin
   BReadS:=ReadParamStr(BVisibleNames);
   if BReadS='' then break;
   if ParsIsType(BReadS) and (ParsExtractName(BReadS)=BForward) then break;
   end;
  if BReadS='' then AppendError('e','Cannot resolve forward declaration '+BForward+' [R:ParsBase.GetTarg]');
  ParsForceType(Result,'p'+ParsExtractType(BReadS));
  end;

 until TRUE;
End;

end.

