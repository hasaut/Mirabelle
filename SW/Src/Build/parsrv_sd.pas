unit ParsRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ParsBase_sd, ParsPrepr_sd, ParsHelper_sd, LlvmBase_sd;

Type
  TParsRV = class(TParsPrepr)
  protected
    Function StrToOpcode ( Const AOpcodeS : string ) : string; Override;
    Function SensCase ( Const ADataS : string ) : string; Override;
    Function IsReservedWord ( Const AName : string ) : boolean; Override;
    Function IsTargConstant ( Const ANameS : string ) : string; Override;
    Procedure RdTextFC ( AJmpNextLine : boolean ); Override;
    Procedure RdTextFD ( AJmpNextLine : boolean ); Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Parse ( AModule : TLlvmModule; AIsInclude : boolean ); Override;
  end;

implementation

Uses
  ConComL;

Constructor TParsRV.Create;
Begin
 Inherited;
End;

Destructor TParsRV.Destroy;
Begin
 Inherited;
End;

Function TParsRV.SensCase ( Const ADataS : string ) : string;
Begin
 Result:=ADataS;
End;

Procedure TParsRV.RdTextFC ( AJmpNextLine : boolean );
Var
  BLastComment  : char;
Begin
 repeat
 while FCommentLevel<>'' do
  begin
  BLastComment:=FCommentLevel[Length(FCommentLevel)];
  RdTextA(TRUE);
  if FParsOrig='' then break;
  if (FParsOrig='*/') and (BLastComment='*') then Delete(FCommentLevel,Length(FCommentLevel),1);
  end;

 RdTextA(AJmpNextLine);
 if FParsOrig='' then break;
 if FParsOrig='/*' then FCommentLevel:=FCommentLevel+'*'
 else if FParsOrig='//' then
  begin
  SkipLine;
  if AJmpNextLine=FALSE then begin FParsOrig:=''; break; end;
  end
 else break;

 until FALSE;
End;

Procedure TParsRV.RdTextFD ( AJmpNextLine : boolean );
Var
  BDefineA,
  BDefineB      : string;
  BOutValid     : boolean;
  BReady        : boolean;
  BDefType      : TDefType;
Begin
 repeat
 RdTextFC(AJmpNextLine);
 if FParsOrig='' then break;

 BReady:=FALSE;
 repeat
 BDefType:=FModule.ResolveDefine(FParsOrig,BDefineA,BDefineB);
 if BDefType=dtNone then
  begin
  ResetDefineCmp;
  BReady:=(Preprocess(FParsOrig,BOutValid)=FALSE) or BOutValid;
  break;
  end;
 if BDefType=dtConst then
  begin
  if PushDefine(FParsOrig,BDefineB)=FALSE then begin FModule.AppendError('e',FParsCursor.FLine,FParsCursor.FPos,'Define loop: '+FParsOrig+' [R:TParsCpp.RdTextSL]'); BReady:=TRUE; end;
  break;
  end;
 // Macro
 if DefPreprocMacro(BDefineA,BDefineB)=FALSE then BReady:=TRUE;
 until TRUE;

 if BReady then break;

 until FALSE;
End;

Function TParsRV.IsReservedWord ( Const AName : string ) : boolean;
Begin
 Result:=FALSE;
End;

Function TParsRV.StrToOpcode ( Const AOpcodeS : string ) : string;
Begin
 Result:='';
 if AOpcodeS='+' then Result:='add'
 else if AOpcodeS='-'   then Result:='sub'
 else if AOpcodeS='*'   then Result:='mul'
 else if AOpcodeS='/'   then Result:='div'
 else if AOpcodeS='&'   then Result:='and'
 else if AOpcodeS='|'   then Result:='or'
 else if AOpcodeS='^'   then Result:='xor'
 else if AOpcodeS='<<'  then Result:='shl'
 else if AOpcodeS='>>'  then Result:='shr'
 else if AOpcodeS='&&'  then Result:='and'
 else if AOpcodeS='||'  then Result:='or'
 else if AOpcodeS='=='  then Result:='cmpe'
 else if AOpcodeS='!='  then Result:='cmpne'
 else if AOpcodeS='<'   then Result:='cmpb'
 else if AOpcodeS='>'   then Result:='cmpa'
 else if AOpcodeS='<='  then Result:='cmpbe'
 else if AOpcodeS='>='  then Result:='cmpae';
End;

Function TParsRV.IsTargConstant ( Const ANameS : string ) : string;
Var
  BNameS        : string;
  BDataC        : Cardinal;
  BDataF        : Extended;

Begin
 Result:='';

 BNameS:=LowerCase(ANameS);

 repeat
 if BNameS='true' then begin Result:=CBooleanTrue; break; end;
 if BNameS='false' then begin Result:=CBooleanFalse; break; end;

 if Pos('0x',BNameS)=1 then
  begin
  Delete(BNameS,1,2); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('0'))
   else if BNameS[1] in ['a'..'f'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('a')+10)
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if Pos('$',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('0'))
   else if BNameS[1] in ['a'..'f'] then BDataC:=(BDataC shl 4) or Cardinal(ord(BNameS[1])-ord('a')+10)
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if Pos('%',BNameS)=1 then
  begin
  Delete(BNameS,1,1); if BNameS='' then break;
  BDataC:=0;
  while BNameS<>'' do
   begin
   if BDataC>=$0FFFFFFF then break;
   if BNameS[1] in ['0'..'1'] then BDataC:=(BDataC shl 1) or Cardinal(ord(BNameS[1])-ord('0'))
   else break;
   Delete(BNameS,1,1);
   end;
  if BNameS<>'' then break;
  Result:=ParsFormatConstX(BDataC);
  break;
  end;

 if (Pos('.',BNameS)<>0) and (Pos('..',BNameS)=0) then
  begin
  if TryStrToFloat(BNameS,BDataF,HParsFormat)=FALSE then break;
  Result:=CTagS+'c_f'+CTagM+BNameS+CTagE;
  break;
  end;

 BDataC:=0;
 while BNameS<>'' do
  begin
  if BDataC>=($FFFFFFFF-10) then break;
  if BNameS[1] in ['0'..'9'] then BDataC:=(BDataC*10) + Cardinal(ord(BNameS[1])-ord('0'))
  else break;
  Delete(BNameS,1,1);
  end;
 if BNameS<>'' then break;
 Result:=ParsFormatConst(BDataC);
 until TRUE;
End;

Procedure TParsRV.Parse ( AModule : TLlvmModule; AIsInclude : boolean );
Var
  BFilePath,
  BFileName,
  BFileExt      : String;
Begin
 Inherited;
 SplitFilename(FFilename,BFilePath,BFileName,BFileExt);
 FModule.Name:=BFileName;
End;

end.

