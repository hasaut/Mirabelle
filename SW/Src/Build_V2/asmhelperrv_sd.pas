unit AsmHelperRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, AsmTypes_sd, MemSeg_sd;

Type
  TAsmFlowLineRiscV = class(TAsmFlowLine)
  private

  protected
    Function InvJmpA ( Const AJmp : string ) : string; Override;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Parse ( Const AReadS : string; Const AFileName : string; ALineIdx : Integer ); Override;
    Procedure FixAddr ( Var ASegList : TMemSegList; Var ASeg : TMemSeg ); Override;
    Procedure UpdateRefs ( Const AInternList, AExternList : TAsmRefList; Var AIsAddrOor : boolean ); Override;
  end;

  TAsmHelperRiscV = class(TAsmBase)
  private
    FSrcList    : TStringList;
    FLineTail   : string;

    Procedure ExtractDupOpti ( ALine : TAsmFlowLine; AParam : TAsmLineParam; Var AReadS : string; Out ADupCnt : Integer );
    Procedure CodeGenDB ( ALine : TAsmFlowLine );
    Procedure CodeGenDW ( ALine : TAsmFlowLine );
    Procedure CodeGenDD ( ALine : TAsmFlowLine );
    Procedure CodeGenDQ ( ALine : TAsmFlowLine );

  protected
    Function NewFlowLine : TAsmFlowLine; Override;
    Function IsPointer ( Const AStr : string; Out ALabel : string ) : boolean;
    Procedure CompileLine ( ALine : TAsmFlowLine );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure CorrectAddrOor; Override;
  end;

Function RiscV_CorrectLui ( AData : Cardinal ) : Cardinal;

implementation

Uses
  ConComL, ConComS;

Function RiscV_CorrectLui ( AData : Cardinal ) : Cardinal;
Begin
 Result:=AData and $FFFFF000;
 if (AData and $800)<>0 then Result:=Result+$1000;
End;


{ *** TAsmFlowLineRiscV *** }

Constructor TAsmFlowLineRiscV.Create;
Begin
 Inherited;
End;

Destructor TAsmFlowLineRiscV.Destroy;
Begin
 Inherited;
End;

Const
    CRegNamesB : array[0..15] of string = ('zero', 'ra', 'sp', 'gp', 'tp', 't0', 't1', 't2', 's0', 's1', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5');

Procedure TAsmFlowLineRiscV.Parse ( Const AReadS : string; Const AFileName : string; ALineIdx : Integer );
Var
  BCmdL         : string;
  BParamA,
  BParamB,
  BParamC       : string;
  BTextPos      : Integer;
  BTextPosA,
  BTextPosB,
  BTextPosC     : Integer;
  BExec         : string;
  BLabelName    : string;
  BAlign        : Integer;
  BByteCnt      : Integer;
  BDummyInt     : Integer;
  BLine         : TAsmFlowLine;
  BZeroCnt      : Integer;
  BRegIdxS      : string;
  BRegIdx       : Integer;
Begin
 Inherited;

 repeat
 if FCmdIs<>acUnparsed then break;
 if Length(FParams)=0 then begin AppendError('e','Internal error: Params list is empty [R:TAsmFlowLineRiscV.Parse]'); break; end;

 BCmdL:=LowerCase(FParams[0].Name);

 if BCmdL='.mode' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
{ if BCmdL='.file' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos); AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamB,BTextPos); AppendParam(BParamB,BTextPos);
  if BParamB='' then break;
  if FAsmBase=nil then break;
  TAsmHelperRiscV(FAsmBase).ParseDotFile(BParamA,BParamB);
  break;
  end;
 if BCmdL='.loc' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos); AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamB,BTextPos); AppendParam(BParamB,BTextPos);
  ReadParamA(' ',BParamC,BTextPos); AppendParam(BParamC,BTextPos);
  if BParamC='' then break;
  if FAsmBase=nil then break;
  TAsmHelperRiscV(FAsmBase).ParseDotLoc(BParamA,BParamB,BParamC);
  break;
  end;}
 if BCmdL='.option' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
 if BCmdL='.size' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
 if BCmdL='.type' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
 if BCmdL='.ident' then
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
 if BCmdL='.set' then  // Not specified in the spec. Treating as label is the most probable
  begin
  FCmdIs:=acLabel;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Label name is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  FLabelName:=BParamA;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  DelInnerSpaces(BParamA);
  if BParamA<>'.+0' then begin AppendError('e',FParams[0],'". +0" is expected (not clear how to treat this) [R:TAsmFlowLineRiscV.Parse]'); break; end;
  break;
  end;
 if BCmdL='.end' then
  begin
  FCmdIs:=acOther;
  ReadParamA(' ',BParamA,BTextPos);
  break;
  end;
 if BCmdL='.text' then
  begin
  FCmdIs:=acSegName; FAsmBase.SkipCodeGen:=FALSE;
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line %p [R:TAsmFlowLineRiscV.Parse]');
  AppendParam('code',BTextPos);
  break;
  end;
 if (BCmdL='.globl') or (BCmdL='.global') then
  begin
  FCmdIs:=acRiscVGlobal;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Identifier name is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;
 {if BCmdL='.sect' then
  begin
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then AppendError('e',AppendParam(BParamA,BTextPos),'Section is missing [R:TAsmFlowLineRiscV.Parse]');
  BParamB:=ReadTillC(BParamA,',');
  if LowerCase(BParamB)='.dp' then FCmdIs:=acSegmD
  else AppendError('e',AppendParam(BParamB,BTextPos),'Invalid section %p [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;}
 if BCmdL='.section' then
  begin
  FAsmBase.SkipCodeGen:=FALSE;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then AppendError('e',AppendParam(BParamA,BTextPos),'Section is missing [R:TAsmFlowLineRiscV.Parse]');
  if LowerCase(BParamA)='.text' then begin FCmdIs:=acSegName; AppendParam('code',BTextPos); end
  else if LowerCase(BParamA)='.init' then begin FCmdIs:=acSegName; AppendParam('code',BTextPos); end
  else if LowerCase(BParamA)='.text.init' then begin FCmdIs:=acSegName; AppendParam('code',BTextPos); end
  else if Pos('.srodata',LowerCase(BParamA))=1 then begin FCmdIs:=acSegName; AppendParam('code'{data},BTextPos); end
  else if Pos('.rodata',LowerCase(BParamA))=1 then begin FCmdIs:=acSegName; AppendParam('code',BTextPos); end
  else if Pos('.sdata',LowerCase(BParamA))=1 then begin FCmdIs:=acSegName; AppendParam('data',BTextPos); end
  else if Pos('.sbss',LowerCase(BParamA))=1 then begin FCmdIs:=acSegName; AppendParam('data',BTextPos); end
  else if Pos('.debug_',LowerCase(BParamA))=1 then begin FCmdIs:=acOther; FAsmBase.SkipCodeGen:=TRUE; end
  else AppendError('e',AppendParam(BParamA,BTextPos),'Invalid section %p [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;
 {
 if BCmdL='.org' then
  begin
  FCmdIs:=acOrg;
  ReadParamA(' ',BParamA,BTextPos);if BParamA='' then begin AppendError('e',FParams[0],'ORG parameter is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  if TryStrToInt(FParams[1].Name,FAlign)=FALSE then AppendError('e',FParams[1],'Cannot convert ORG to integer [R:TAsmFlowLineRiscV.Parse]');
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line %p [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;}
 if BCmdL='.align' then
  begin
  FCmdIs:=acAlign;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Align parameter is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  if TryStrToInt(FParams[1].Name,BDummyInt)=FALSE then AppendError('e',FParams[1],'Cannot convert Align to integer [R:TAsmFlowLineRiscV.Parse]');
  FAlign:=1 shl BDummyInt;
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;
 if BCmdL='.comm' then
  begin
  // Collect all parameters
  // *Label
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Var name is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  BLabelName:=BParamA;
  // *Size in bytes
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then  begin AppendError('e',FParams[0],'Size is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  if TryStrToInt(BParamA,BByteCnt)=FALSE then AppendError('e',FParams[1],'Cannot convert Size to integer [R:TAsmFlowLineRiscV.Parse]');
  // *Align
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then  begin AppendError('e',FParams[0],'Align is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  if TryStrToInt(BParamA,BAlign)=FALSE then AppendError('e',FParams[1],'Cannot convert Align to integer [R:TAsmFlowLineRiscV.Parse]');
  // *Create data
  BByteCnt:=(BByteCnt*BAlign+BAlign-1) div BAlign;
  // Create lines. There will be 3 additional lines
  // *Data section
  BLine:=AsmBase.AppendFlowLineA;
  BLine.Parse('.data',AFilename,ALineIdx);
  // *Self
  {FCmdIs:=acAlign;
  AppendParam(IntToStr(BAlign),1);
  FAlign:=BAlign;}
  BLine:=AsmBase.AppendFlowLineA;
  BLine.Parse('.align '+IntToStr(BAlign),AFilename,ALineIdx);
  // *Label
  BLine:=AsmBase.AppendFlowLineA;
  BLine.Parse('     '+BLabelName+':',AFilename,ALineIdx);
  // *Data
  BLine:=AsmBase.AppendFlowLineA;
  BLine.Parse('        db 0 dup('+IntToStr(BByteCnt)+')',AFilename,ALineIdx);
  // Declare as global
  FCmdIs:=acRiscVGlobal;
  AppendParam(BLabelName,BTextPos);
  break;
  end;
 if BCmdL='.zero' then
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acData;
  FAlign:=1;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  if TryStrToInt(BParamA,BZeroCnt)=FALSE then AppendError('e',FParams[1],'Cannot convert Zero count to integer [R:TAsmFlowLineRiscV.Parse]');
  //for BZeroIdx:=0 to BZeroCnt-1 do AppendParam('0',BTextPos);
  AppendParam('0 dup('+BParamA+')',BTextPos);
  break;
  end;
 if BCmdL='.4byte' then // Same as "word" ?
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acData;
  FAlign:=4;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.2byte' then // Same as "half" ?
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acData;
  FAlign:=2;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.word' then
  begin
  FCmdIs:=acData;
  FAlign:=4;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.half' then
  begin
  FCmdIs:=acData;
  FAlign:=2;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.byte' then
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acData;
  FAlign:=1;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.string' then
  begin
  if FAsmBase.SkipCodeGen then break;
  FCmdIs:=acData;
  FAlign:=1;
  ReadParamC(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamC(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.data' then
  begin
  FCmdIs:=acSegName; FAsmBase.SkipCodeGen:=FALSE;
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineMcgx.Parse]');
  AppendParam('data',BTextPos);
  break;
  end;
 if BCmdL='.bss' then
  begin
  FCmdIs:=acSegName; FAsmBase.SkipCodeGen:=FALSE; // BSS is a data section
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineMcgx.Parse]');
  AppendParam('data',BTextPos);
  break;
  end;
 if BCmdL='.org' then
  begin
  FCmdIs:=acOrg;
  ReadParamA(' ',BParamA,BTextPos);if BParamA='' then begin AppendError('e',FParams[0],'ORG parameter is missing [R:TAsmFlowLineMcgx.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  if TryStrToInt(FParams[1].Name,FAlign)=FALSE then AppendError('e',FParams[1],'Cannot convert ORG to integer [R:TAsmFlowLineMcgx.Parse]');
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineMcgx.Parse]');
  break;
  end;
 if StrInList(BCmdL,'db dw dd') then
  begin
  FCmdIs:=acData;
  if BCmdL='db' then FAlign:=1
  else if BCmdL='dw' then FAlign:=2
  else {if BCmd='dd' then} FAlign:=4;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='.fill' then
  begin
  FCmdIs:=acData;
  ReadParamA(',',BParamA,BTextPosA); if BParamA='' then begin AppendError('e',FParams[0],'Counter is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  ReadParamA(',',BParamB,BTextPosB); if BParamB='' then begin AppendError('e',FParams[0],'Align is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  ReadParamA(',',BParamC,BTextPosC); if BParamC='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  if BParamB='1' then FAlign:=1
  else if BParamB='2' then FAlign:=2
  else if BParamB='4' then FAlign:=4
  else if BParamB='8' then FAlign:=8
  else begin AppendError('e',FParams[0],'Invalid data width [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamC+' dup('+BParamA+')',BTextPosA);
  break;
  end;

 if BCmdL[Length(BCmdL)]=':' then
  begin
  FCmdIs:=acLabel;
  FLabelName:=FParams[0].Name; Delete(FLabelName,Length(FLabelName),1);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;
 if BCmdL='extern' then
  begin
  FCmdIs:=acExtern;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Identifier name is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;

 if BCmdL='tail' then
  begin
  FCmdIs:=acCmd;
  FParams[0].Name:='j';
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Identifier name is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;

 BExec:=FExec;
 ReadParamStr(BExec);
 if LowerCase(ReadParamStr(BExec))='equ' then
  begin
  FCmdIs:=acEqu;
  ReadParamA(' ',BParamA,BTextPos); AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'EQU parameter is missing [R:TAsmFlowLineRiscV.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineRiscV.Parse]');
  break;
  end;

 FCmdIs:=acCmd;
 repeat
 ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
 repeat
 if BParamA[1]<>'x' then break;
 BRegIdxS:=BParamA; Delete(BRegIdxS,1,1);
 if TryStrToInt(BRegIdxS,BRegIdx)=FALSE then break;
 if BRegIdx<0 then break;
 if BRegIdx>15 then break;
 BParamA:=CRegNamesB[BRegIdx];
 until TRUE;
 AppendParam(BParamA,BTextPos);
 until FALSE;

 until TRUE;
End;

Procedure TAsmFlowLineRiscV.FixAddr ( Var ASegList : TMemSegList; Var ASeg : TMemSeg );
Begin
 // acSegmC, acSegmD, acOrg, acAlign, acData, acLabel, acCmd
 repeat
 case FCmdIs of
   acSegName:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Seg name parameter is not defined'); break; end;
     ASeg:=MemSegSearch(ASegList,FParams[1].Name);
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined [R:TAsmFlowLineRv.FixAddr]'); break; end;
     end;
   acOrg:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Org parameter is not defined'); break; end;
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined'); break; end;
     if ASeg.FillSize>FAlign then begin AppendError('e',FParams[1],'Segment is already bigger than org parameter'); break; end;
     ASeg.FillSize:=FAlign;
     end;
   acAlign:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Align parameter is not defined'); break; end;
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined'); break; end;
     if FAlign<=0 then begin AppendError('e',FParams[1],'Align is too small'); break; end;
     ASeg.FillSize:=((ASeg.FillSize+(FAlign-1)) div FAlign)*FAlign;
     end;
   acData:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     if (FAlign>2) and ((ASeg.FillSize mod FAlign)<>0) then
      begin
      AppendError('w',FParams[0],'Data is misaligned, align is forced [R:TAsmFlowLineRiscV.FixAddr]');
      ASeg.FillSize:=((ASeg.FillSize+(FAlign-1)) div FAlign)*FAlign;
      end;
     end;
   acLabel:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     end;
   acCmd:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     if (ASeg.FillSize mod 2)<>0 then
      begin
      AppendError('w',FParams[0],'Instructions are misaligned, align is forced');
      ASeg.FillSize:=((ASeg.FillSize+(2-1)) div 2)*2;
      end;
     end;
   else
     break;
   end;
 FMemSeg:=ASeg;
 FAddr:=ASeg.HwBase+ASeg.FillSize;
 if FUsed then ASeg.FillSize:=ASeg.FillSize+Length(FCodeBin);
 until TRUE;
End;

// imm[20|10:1|11|19:12]       rd          opcode J-type
Procedure SetRefJ ( Var ACodeBin : string; AConst : Cardinal );
Var
  BConst    : Cardinal;
Begin
 BConst:=((AConst and $0FF000) shr 12) or // 000FF
         ((AConst and $000800) shr 3) or  // 001FF
         ((AConst and $0007FE) shl 8) or  // 7FFFF
         ((AConst and $100000) shr 1);    // FFFFF
 ACodeBin[2]:=Chr((Ord(ACodeBin[2]) and $0F) or ((BConst shl 4) and $F0));
 ACodeBin[3]:=Chr((BConst shr  4) and $FF);
 ACodeBin[4]:=Chr((BConst shr 12) and $FF);
End;

// imm[11|4|9:8|10|6|7|3:1|5]
Procedure SetRefCJ ( Var ACodeBin : string; AConst : Cardinal );
Var
  BConst    : Cardinal;
Begin
 BConst:=((AConst and $020) shr 5) or // 001   5
         ((AConst and $00E) shl 0) or // 00E 3:1
         ((AConst and $080) shr 3) or // 010   7
         ((AConst and $040) shr 1) or // 020   6
         ((AConst and $400) shr 4) or // 040  10
         ((AConst and $300) shr 1) or // 180 9:8
         ((AConst and $010) shl 5) or // 200   4
         ((AConst and $800) shr 1);   // 400  11
 ACodeBin[1]:=Chr((Ord(ACodeBin[1]) and $03) or ((BConst shl 2) and $FC));
 ACodeBin[2]:=Chr((Ord(ACodeBin[2]) and $E0) or ((BConst shr 6) and $1F));
End;

{
FSubdec.FImm:=((ACode shr 20) and $000007FE) or
              ((ACode shr  9) and $00000800) or
              ( ACode         and $000FF800) or
              ((ACode shr 11) and $00100000);
}

//imm[11:0]        rs1 funct3 rd          opcode I-type
Procedure SetRefI ( Var ACodeBin : string; AConst : Cardinal; APos : Integer );
Var
  BConst    : Cardinal;
Begin
 BConst:=AConst and $FFF; // 00000FFF
 ACodeBin[1+APos+2]:=Chr((Ord(ACodeBin[1+APos+2]) and $0F) or ((BConst shl 4) and $F0));
 ACodeBin[1+APos+3]:=Chr((BConst shr 4) and $FF);
End;

// imm[11:5]    rs2 rs1 funct3 imm[4:0]    opcode S-type
Procedure SetRefS ( Var ACodeBin : string; AConst : Cardinal );
Var
  BConst    : Cardinal;
Begin
 BConst:=AConst and $FFF; // 00000FFF
 ACodeBin[1]:=Chr((Ord(ACodeBin[1]) and $7F) or ((BConst shl 7) and $80));
 ACodeBin[2]:=Chr((Ord(ACodeBin[2]) and $F0) or ((BConst shr 1) and $0F));
 ACodeBin[4]:=Chr((Ord(ACodeBin[4]) and $01) or ((BConst shr 4) and $FE));
End;

// imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] opcode B-type
Procedure SetRefB ( Var ACodeBin : string; AConst : Cardinal );
Var
  BConst    : Cardinal;
Begin
 BConst:=((AConst and $000800) shr 11) or // 00001
         ((AConst and $00001E) shr 0) or  // 0001F
         ((AConst and $0007E0) shl 0) or  // 007FF
         ((AConst and $001000) shr 1);    // 00FFF
 SetRefS(ACodeBin,BConst);
End;

Procedure TAsmFlowLineRiscV.UpdateRefs ( Const AInternList, AExternList : TAsmRefList; Var AIsAddrOor : boolean );
Var
  BRefIdx       : Integer;
  BRefA,
  BRefB         : TAsmRef;
  BReadS,
  BRefName,
  BOpcode,
  BConstS       : string;
  BDstAddr      : Cardinal;
  BConst        : Integer;
  BJmpDelta     : Int64;
  BDstRef       : Cardinal;
  BLabel        : TAsmFlowline;
Begin
 for BRefIdx:=0 to Length(FRefList)-1 do
  begin
  BRefA:=FRefList[BRefIdx];
  BRefName:=BRefA.Name;
  BReadS:=BRefA.Param.Name;
  ReadParamStr(BReadS);
  BOpcode:=ReadParamStr(BReadS);
  BConstS:=ReadParamStr(BReadS); BConst:=0;
  DelFirstSpace(BReadS);
  if BReadS<>'' then BRefA.AppendError('e','Extra parameter in line [R:TAsmFlowLineRiscV.UpdateRefs]');
  if (BOpcode<>'') and (BConstS='') then BRefA.AppendError('e','Constant is missing after operation code [R:TAsmFlowLineRiscV.UpdateRefs]');
  if BOpcode<>'' then
   begin
   if StrInList(BOpcode,'div shr shl + -')=FALSE then BRefA.AppendError('e','Invalid operation '+BOpcode+' [R:TAsmFlowLineRiscV.UpdateRefs]');
   if TryStrToInt(BConstS,BConst)=FALSE then BRefA.AppendError('e','Invalid constant '+BConstS+' [R:TAsmFlowLineRiscV.UpdateRefs]');
   end;
  repeat
  if BRefName='.' then BDstAddr:=Addr
  else
   begin
   BLabel:=FAsmBase.FindLabel(Self,BRefName);
   if BLabel<>nil then BDstAddr:=BLabel.Addr
   else
    begin
    BRefB:=RefListSearch(AInternList,BRefName);
    if BRefB=nil then BRefB:=RefListSearch(AExternList,BRefName);
    if BRefB=nil then BRefB:=RefListSearch(FAsmBase.HiddenLine.RefList,BRefName);
    if BRefB=nil then begin BRefA.AppendError('e','Reference '+BRefName+' not found [R:TAsmFlowLineRiscV.UpdateRefs]'); break; end;
    BDstAddr:=BRefB.ObjectAddr;
    end;
   end;
  if BOpcode='div' then
   begin
   if BConst=0 then BRefA.AppendError('e','Cannot divide by zero [R:TAsmFlowLineRiscV.UpdateRefs]')
   else BDstAddr:=BDstAddr div BConst;
   end
  else if BOpcode='shl' then BDstAddr:=BDstAddr shl BConst
  else if BOpcode='shr' then BDstAddr:=BDstAddr shr BConst
  else if BOpcode='+' then BDstAddr:=BDstAddr + BConst
  else if BOpcode='-' then BDstAddr:=BDstAddr - BConst;
  BDstAddr:=BDstAddr+BRefA.FieldAddr;
  case BRefA.RefType of
    'j' : begin // c.j
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>$FFF) or (BJmpDelta<-$1000) then
           begin
           AIsAddrOor:=TRUE;
           FIsAddrOor:=TRUE;
           end;
          if (BRefA.CodeBinPos+2)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length')
          else SetRefCJ(FCodeBin,Cardinal(BJmpDelta shl 1));
          end;
    'J' : begin
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>$FFFFF) or (BJmpDelta<-$100000) then
           begin
           //AIsAddrOor:=TRUE;
           //FIsAddrOor:=TRUE;
           BRefA.AppendError('e','Relative label out of range (jmp offset: '+IntToStr(BJmpDelta)+')[R:TAsmFlowLineMcgx.UpdateRefs]');
           end;
          if (BRefA.CodeBinPos+2)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length')
          else SetRefJ(FCodeBin,Cardinal(BJmpDelta shl 1));
          end;
    'T' : begin // Used for SWT
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>$7FF) or (BJmpDelta<-$800) then
           begin
           //AIsAddrOor:=TRUE;
           //FIsAddrOor:=TRUE;
           BRefA.AppendError('e','Relative label out of range (jmp offset: '+IntToStr(BJmpDelta)+')[R:TAsmFlowLineMcgx.UpdateRefs]');
           end;
          if (BRefA.CodeBinPos+2)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length')
          else AddRefDD(FCodeBin,BRefA.CodeBinPos,Cardinal(BJmpDelta) shl 20);
          end;
    'I' : begin // I-type
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineRiscV.UpdateRefs]')
          else
           begin
           BDstRef:=BDstAddr and $FFF;
           SetRefI(FCodeBin,BDstRef,BRefA.CodeBinPos);
           end;
          end;
    'S' : begin // S-type
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineRiscV.UpdateRefs]')
          else
           begin
           BDstRef:=BDstAddr and $FFF;
           SetRefS(FCodeBin,BDstRef);
           end;
          end;
    'B' : begin // B-type (For the moment the same as S type, not an official RV)
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>$FFFFF) or (BJmpDelta<-$100000) then
           begin
           BRefA.AppendError('e','Relative label out of range (jmp offset: '+IntToStr(BJmpDelta)+')[R:TAsmFlowLineMcgx.UpdateRefs]');
           end;
          if (BRefA.CodeBinPos+2)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length')
          else
           begin
           SetRefB(FCodeBin,BJmpDelta shl 1);
           end;
          end;
    'U' : begin // U-type
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineRiscV.UpdateRefs]')
          else
           begin
           BDstRef:=(BDstAddr and $FFFFF000)+((BDstAddr and $800) shl 1); // Imm[11:0] is always sign-extended and can change the rest. Very annoying
           AddRefDD(FCodeBin,BRefA.CodeBinPos,BDstRef);
           end;
          end;
    'd' : begin
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then
           BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineMs.UpdateRefs]')
          else AddRefDD(FCodeBin,BRefA.CodeBinPos,BDstAddr);
          end;
     else begin
          BRefA.AppendError('e','Internal error: invalid reference type '+BRefA.RefType+' [R:TAsmFlowLineRiscV.UpdateRefs]');
          end;
    end; // case
  until TRUE;
  end;
End;

Function TAsmFlowLineRiscV.InvJmpA ( Const AJmp : string ) : string;
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
 else AppendError('e','Internal error: cannot invert JMP condition '+AJmp+' [R:TAsmFlowLineRiscV.InvJmp]');
End;

{ *** TAsmHelperRiscV *** }

Constructor TAsmHelperRiscV.Create;
Begin
 Inherited;
 FSrcList:=TStringList.Create;
 FSymComment:='#';
 FSymMultiline:=';';
End;

Destructor TAsmHelperRiscV.Destroy;
Begin
 FSrcList.Free;
 Inherited;
End;

Function TAsmHelperRiscV.NewFlowLine : TAsmFlowLine;
Begin
 Result:=TAsmFlowLineRiscV.Create;
End;

Procedure TAsmHelperRiscV.CompileLine ( ALine : TAsmFlowLine );
Var
  BRef          : TAsmRef;
  BLabel        : TAsmFlowLine;
Begin
 case ALine.CmdIs of
   acPublic:
     begin
     BRef:=RefListSearch(FPublicList,ALine.Params[1].Name);
     if BRef<>nil then ALine.AppendError('e',ALine.Params[1],'Declaration with this name already exists [R:TAsmHelperRiscV.Compile]');
     RefListAppend(FPublicList,ALine.Params[1],ALine.Params[1].Name);
     end;
   acExtern:
     begin
     BRef:=RefListSearch(FExternList,ALine.Params[1].Name);
     if BRef<>nil then ALine.AppendError('e',ALine.Params[1],'Declaration with this name already exists [R:TAsmHelperRiscV.Compile]');
     RefListAppend(FExternList,ALine.Params[1],ALine.Params[1].Name);
     end;
   acRiscVGlobal: // Either Public or Extern
     begin
     BLabel:=FindLabel(ALine,ALine.Params[1].Name);
     if BLabel=nil then
      begin
      BRef:=RefListSearch(FExternList,ALine.Params[1].Name);
      if BRef=nil then RefListAppend(FExternList,ALine.Params[1],ALine.Params[1].Name);
      end
     else
      begin
      BRef:=RefListSearch(FPublicList,ALine.Params[1].Name);
      if BRef<>nil then ALine.AppendError('e',ALine.Params[1],'Declaration with this name already exists [R:TAsmHelperRiscV.Compile]');
      RefListAppend(FPublicList,ALine.Params[1],ALine.Params[1].Name);
      end;
     end;
   acLabel:
     begin
     if ALine.LabelName='' then ALine.AppendError('e',ALine.Params[1],'Label name is absent [R:TAsmHelperRiscV.Compile]')
     else
      begin
      if ALine.LabelName[1] in ['1'..'9'] then
      else
       begin
       BRef:=RefListSearch(FLabelList,ALine.LabelName);
       if BRef<>nil then ALine.AppendError('e',ALine.Params[0],'Label already exists [R:TAsmHelperRiscV.Compile]');
       end;
      RefListAppend(FLabelList,ALine.Params[0],ALine.LabelName);
      end;
     end;
   acEqu:
     begin
     if FEquList.Values[ALine.Params[0].Name]<>'' then ALine.AppendError('e',ALine.Params[0],'Duplicated EQU [R:TAsmHelperRiscV.Compile]')
     else FEquList.Values[ALine.Params[0].Name]:=ALine.Params[2].Name;
     end;
   acData:
     begin
     if ALine.Align=1 then CodeGenDB(ALine)
     else if ALine.Align=2 then CodeGenDW(ALine)
     else if ALine.Align=4 then CodeGenDD(ALine)
     else if ALine.Align=8 then CodeGenDQ(ALine)
     else ALine.AppendError('e',ALine.Params[0],'Internal error: Invalid align [R:TAsmHelperRiscV.Compile]');
     end;
   acCmd:
     begin
     CodeGenCmd(ALine);
     end;
   end; // case
End;

Procedure TAsmHelperRiscV.Compile;
Var
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BRef,
  BRefA         : TAsmRef;
  BLabel        : TAsmFlowLine;
  BRefIdx       : Integer;
Begin
 Inherited;

 FSrcList.Clear;

 repeat
 if FParser=nil then
  begin
  FAsmName:=FSrcName;
  FAsmSrc.Assign(FTextSrc);
  end
 else
  begin
  if FModule=nil then begin AppendErrorA('e',0,0,FSrcName,'Internal error: Parser is defined but module is not [R:TAsmHelperRiscV.Compile]'); break; end;
  FModule.OnAppendError:=@AppendErrorA;
  FModule.Init(FSrcName,FPrjPath,FDstPath,FDefCodeSeg,FDefDataSeg);
  FParser.Parse(FModule,FALSE); if FModule.GetErrorCountA<>0 then break;
  FModule.Compile; if FModule.GetErrorCountA<>0 then break;
  FModule.GenAsmSrc(FAsmSrc); if FModule.GetErrorCountA<>0 then break;
  FAsmName:=AssembleFullName(FDstPath,FModule.Name,'srv');
  end;

 ParseList(FAsmSrc,FAsmName,'');

 if GetErrorCountA<>0 then break;
 for BLineIdx:=0 to Length(FFlowList)-1 do
  begin
  CompileLine(FFlowList[BLineIdx]);
  end;

 // Update unreferenced calls: sometimes call labels are not defined ax externs. So just add them
 for BLineIdx:=0 to Length(FFlowList)-1 do
  begin
  BLine:=FFlowList[BLineIdx];
  // For labels
  repeat
  if (BLine.IsJxx=FALSE) and (BLine.IsJmp=FALSE) then break;
  if BLine.DstLabel='' then break;
  if BLine.DstLabel='.' then break;
  BLabel:=FindLabel(BLine,BLine.DstLabel);
  if BLabel<>nil then break;
  BRef:=RefListSearch(FExternList,BLine.DstLabel);
  if BRef<>nil then break;
  RefListAppend(FExternList,BLine.Params[1],BLine.DstLabel);
  until TRUE;
  // For any other ref
  BRefIdx:=0;
  while BRefIdx<Length(BLine.RefList) do
   begin
   BRef:=BLine.RefList[BRefIdx];
   repeat
   BLabel:=FindLabel(BLine,BRef.Name); if BLabel<>nil then break;
   BRefA:=RefListSearch(FPublicList,BRef.Name); if BRefA<>nil then break;
   BRefA:=RefListSearch(FExternList,BRef.Name); if BRefA<>nil then break;
   RefListAppend(FExternList,BRef.Param,BRef.Name);
   until TRUE;
   inc(BRefIdx);
   end;
  end;
 until TRUE;
End;

Procedure TAsmHelperRiscV.ExtractDupOpti ( ALine : TAsmFlowLine; AParam : TAsmLineParam; Var AReadS : string; Out ADupCnt : Integer );
Var
  BReadS        : string;
  BDupS         : string;
  BDupCnt       : Integer;
Begin
 ADupCnt:=1;
 repeat
 BReadS:=AReadS;
 BDupS:=ReadParamStrInv(BReadS);
 if BDupS='' then break;
 if Pos('dup(',LowerCase(BDupS))<>1 then break;
 if BDupS[Length(BDupS)]<>')' then break;
 AReadS:=BReadS;
 Delete(BDupS,1,4);
 Delete(BDupS,Length(BDupS),1);
 if TryStrToInt(BDupS,BDupCnt)=FALSE then begin ALine.AppendError('e',AParam,'Cannot convert dup constant to integer [R:TAsmHelperRiscV.ExtractDupOpti]'); break; end;
 if BDupCnt>65536 then begin ALine.AppendError('e',AParam,'Dup counter is too big (65536 max) [R:TAsmHelperRiscV.ExtractDupOpti]'); break; end;
 if BDupCnt<1 then begin ALine.AppendError('e',AParam,'Dup counter is too small [R:TAsmHelperRiscV.ExtractDupOpti]'); break; end;
 ADupCnt:=BDupCnt;
 until TRUE;
End;

Procedure TAsmHelperRiscV.CodeGenDB ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>=2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do
    begin
    BStrIdx:=2;
    while BStrIdx<Length(BDataS) do begin ALine.AppendDataBinB(Ord(BDataS[BStrIdx])); inc(BStrIdx); end;
    end;
   end
  else if (BDataS[1]='"') and (Length(BDataS)>=2) and (BDataS[Length(BDataS)]='"') then
   begin
   for BDupIdx:=1 to BDupCnt do
    begin
    BStrIdx:=2;
    while BStrIdx<Length(BDataS) do begin ALine.AppendDataBinB(Ord(BDataS[BStrIdx])); inc(BStrIdx); end;
    end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>255 then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperRiscV.CodeGenDB]')
   else if BDataI<-128 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperRiscV.CodeGenDB]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinB(Byte(BDataI));
   end
  else
   begin
   ALine.AppendError('e',BParam,'Reference (if this is a reference) is too big for this data type [R:TAsmHelperRiscV.CodeGenDB]');
   end;
  end;
End;

Procedure TAsmHelperRiscV.CodeGenDW ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-2 do ALine.AppendDataBinW(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>65535 then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperRiscV.CodeGenDW]')
   else if BDataI<-32768 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperRiscV.CodeGenDW]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinW(Word(BDataI));
   end
  else
   begin
   ALine.AppendDataRefW(BParam);
   end;
  end;
End;

Procedure TAsmHelperRiscV.CodeGenDD ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDataFS       : Single;
  BDataFD       : Double;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if BDataS='' then
   begin
   ALine.AppendError('e',BParam,'Cannot extract dup data [R:TAsmHelperRiscV.CodeGenDD]')
   end
  else if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-2 do ALine.AppendDataBinD(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>$FFFFFFFF then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperRiscV.CodeGenDD]')
   else if BDataI<-$80000000 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperRiscV.CodeGenDD]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinD(Cardinal(BDataI));
   end
  else if IsFloatEqu(BDataS,BDataFD) then
   begin
   BDataFS:=BDataFD;
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinD(Cardinal(BDataFS));
   end
  else
   begin
   ALine.AppendDataRefD(BParam);
   end;
  end;
End;

Procedure TAsmHelperRiscV.CodeGenDQ ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDataFS       : Single;
  BDataFD       : Double;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-2 do ALine.AppendDataBinD(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   //if BDataI>$FFFFFFFF then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperRiscV.CodeGenDD]')
   //else if BDataI<-$80000000 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperRiscV.CodeGenDD]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinQ(Cardinal(BDataI));
   end
  else if IsFloatEqu(BDataS,BDataFD) then
   begin
   BDataFS:=BDataFD;
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinQ(Cardinal(BDataFS));
   end
  else
   begin
   ALine.AppendDataRefQ(BParam);
   end;
  end;
End;

Function TAsmHelperRiscV.IsPointer ( Const AStr : string; Out ALabel : string ) : boolean;
Var
  BLen  : integer;
Begin
 Result:=FALSE;
 ALabel:='';
 repeat
 BLen:=Length(AStr);
 if BLen<2 then break;
 if (AStr[1]='[') and (AStr[BLen]=']') then
 else break;
 ALabel:=Copy(AStr,2,BLen-2);
 Result:=TRUE;
 until TRUE;
End;

Procedure TAsmHelperRiscV.CorrectAddrOor;
Var
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BRef          : TAsmRef;
  BCodeBin      : string;
Begin
 BLineIdx:=0;
 while BLineIdx<Length(FFlowList) do
  begin
  BLine:=FFlowList[BLineIdx];
  repeat
  if BLine.IsAddrOor=FALSE then break;
  BRef:=BLine.RefList[0];
  if BRef=nil then begin BLine.AppendError('e','Internal error: ref is nil [R:TAsmHelperRiscV.CorrectAddrOor]'); break; end;
  case BRef.RefType of
   'j': begin // Becomes 'J'
        BRef.RefType:='J';
        BLine.AppendDataBinW(0);
        BCodeBin:=BLine.CodeBin;
        if Length(BCodeBin)<4 then begin BLine.AppendError('e','Internal error: Size of codebin is wrong [R:TAsmHelperRiscV.CorrectAddrOor]'); break; end;
        BCodeBin[4]:=Chr($00); BCodeBin[3]:=Chr($00); BCodeBin[2]:=Chr($00); BCodeBin[1]:=Chr($6F); // JAL zero
        BLine.CodeBin:=BCodeBin;
        SaveCodeBin;
        end;
   else begin
        BLine.AppendError('e','Internal error: Ref cannot be changed anymore [R:TAsmHelperRiscV.CorrectAddrOor]');
        break;
        end;
  end; // case
  until TRUE;
  inc(BLineIdx);
  end;
End;

end.

