unit DbgInfo_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParsHelper_sd, ParsPas_sd, LlvmBeHelper_sd;

Type
  TDbgInfoFile = class;
  TDbgInfoProc = class;
  TDbgInfoLine = class;

  TDbgOnSrcLineChange = Procedure ( Const AFilename : string; ASrcLineIdx : Integer ) of object;
  TDbgOnAsmLineChange = Procedure ( ADbgLine : TDbgInfoLine ) of object;

  TDbgLineType = (dltHeader, dltProc, dltAsmLine);
  TCompLayer = (clPars, clAtom, clJmps, clVars, clSRec, clOrdr, clMatr);
  TCompLayerIdxL = array [TCompLayer] of Integer;
  TCompLayerTextList = array [TCompLayer] of TStringList;

Const
  CCompLayerNames : array [TCompLayer] of string = ('ParsPos', 'AtomPos', 'JmpsPos', 'VarsPos', 'SRecPos', 'OrdrPos', 'MatrPos');

Type
  TDbgInfoLine = class (TObject)
  private
    FLineType   : TDbgLineType;
    FDbgFile    : TDbgInfoFile;
    FDbgProc    : TDbgInfoProc;
    FModuleName : string;
    FOrig       : string;
    FLineIdx    : Integer;
    FReadable   : string;
    FAsmFile    : string;
    FSrcFile    : string;
    FSrcIdxL,
    FSrcIdxP    : Integer;
    FAsmIdxL    : Integer;
    FProcNameL  : string;
    FLayerIdxL  : TCompLayerIdxL;
    FUseMatr    : string;
    FStackOffs  : Integer;
    FRegMap     : TStringList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Parse ( ALineIdxS : Integer; Const AOrig : string );

    property LineIdx : Integer read FLineIdx;
    property LineType : TDbgLineType read FLineType;
    property DbgFile : TDbgInfoFile read FDbgFile write FDbgFile;
    property DbgProc : TDbgInfoProc read FDbgProc write FDbgProc;
    property ModuleName : string read FModuleName write FModuleName;

    property Readable : string read FReadable;
    property AsmFile : string read FAsmFile;
    property SrcFile : string read FSrcFile;
    property SrcIdxL : Integer read FSrcIdxL;
    property SrcIdxP : Integer read FSrcIdxP;
    property AsmIdxL : Integer read FAsmIdxL;
    property ProcNameL : string read FProcNameL;
    property LayerIdxL : TCompLayerIdxL read FLayerIdxL write FLayerIdxL;
    property UseMatr : string read FUseMatr;
    property StackOffs : Integer read FStackOffs;

    property RegMap : TStringList read FRegMap;
  end;

  TDbgInfoLineList = array of TDbgInfoLine;

  TDbgInfoProc = class (TObject)
  private
    FLineIdxS           : Integer;
    FProcNameL,
    FReadableHdr        : string;
    FChTargList         : TStringList;
    FVsmList            : TVsmList;
    FOffsWorst          : Integer;     // Worst offset of a local variable in a stack. Used by debugger to determine how much data to read from the HW

    FDbgDataBin         : string;

    Procedure VsmListClear;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure ParseHdr ( ALineIdxS : Integer; Const AHeader : string );

    property LineIdxS : Integer read FLineIdxS;
    property ProcNameL : string read FProcNameL;
    property ReadableHdr : string read FReadableHdr;
    property ChTargList : TStringList read FChTargList;
    property VsmList : TVsmList read FVsmList;
    property OffsWorst : Integer read FOffsWorst;
  end;

  TDbgInfoProcList = array of TDbgInfoProc;

  TDbgInfoFile = class (TObject)
  private
    FAsmList    : TStringList;
    FSrcList    : TStringList;
    FModuleList : TStringList;
    FIsLst      : boolean;
    FList       : TStringList;
    FLineS,
    FLineE      : Integer;
    FLineList   : TDbgInfoLineList;
    FLinkToSrc  : boolean; // Shows if there is a line link to a

    FProcList   : TDbgInfoProcList;

    Function AppendProc ( ALineIdxS : Integer; Const AHeader : string ) : TDbgInfoProc;

    Procedure ClearProcList;
    Procedure ClearLineList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure ParseLst ( Const AAsmName : string; AList : TStringList; ALineS, ALineE : Integer );
    Procedure ParseDbg ( AList : TStringList );
    Procedure AppendSrcName ( Const ASrcName : string );
    Procedure AppendModuleName ( Const AName : string );

    Function GetLine ( AIndex : Integer ) : TDbgInfoLine;
    //Procedure ListIdxToSrcIdx ( AListIdx : Integer; Out ALine, APos : Integer );
    Function ProcByAsmLineIdx ( AAsmLineIdx : Integer ) : TDbgInfoProc;

    property IsLst : boolean read FIsLst write FIsLst;
    property AsmName : TStringList read FAsmList;
    property SrcList : TStringList read FSrcList;
    property ModuleName : TStringList read FModuleList;
    property LineS : Integer read FLineS;
    property LineE : Integer read FLineE;
    property SrcText : TStringList read FList;
  end;

implementation

Uses
  ConComL;

{ TDbgInfoLine }

Constructor TDbgInfoLine.Create;
Begin
 Inherited;
 FRegMap:=TStringList.Create;
End;

Destructor TDbgInfoLine.Destroy;
Begin
 FRegMap.Free;
 Inherited;
End;

Procedure TDbgInfoLine.Parse ( ALineIdxS : Integer; Const AOrig : string );
Var
  BCmd,
  BLine,
  BTail         : string;
  BProc         : TDbgInfoProc;
  BRegMap,
  BReadS        : string;
  BLayerIdx     : TCompLayer;
Begin
 FRegMap.Clear;
 FOrig:=AOrig;
 FLineIdx:=ALineIdxS;
 if Copy(FOrig,1,2)=';@' then
  begin
  FSrcFile:=ParsTailExtractTagValue(FOrig,'SrcFile');
  FAsmFile:=ParsTailExtractTagValue(FOrig,'AsmFile');
  ParsTailExtractTagValue(FOrig,'SrcPos',FSrcIdxL,FSrcIdxP);
  ParsTailExtractTagValue(FOrig,'AsmPos',FAsmIdxL);
  FProcNameL:=ParsTailExtractTagValue(FOrig,'ProcNameL');
  for BLayerIdx in TCompLayer do ParsTailExtractTagValue(FOrig,CCompLayerNames[BLayerIdx],FLayerIdxL[BLayerIdx]);
  BLine:=FOrig;
  BCmd:=Copy(BLine,1,3);
  Delete(BLine,1,3); DelFirstSpace(BLine);
  if BCmd=';@A' then
   begin
   FLineType:=dltHeader;
   FReadable:='; *** Asm source: '+BLine+' ***';
   end
  else if BCmd=';@M' then
   begin
   FLineType:=dltHeader;
   FReadable:='; Module: '+BLine;
   FDbgFile.AppendModuleName(ReadParamStr(BLine));
   end
  else if BCmd=';@S' then
   begin
   FLineType:=dltHeader;
   FReadable:='; Source: '+BLine;
   FDbgFile.AppendSrcName(ReadParamStr(BLine));
   end
  else if BCmd=';@P' then
   begin
   FLineType:=dltProc;
   BProc:=FDbgFile.AppendProc(ALineIdxS,BLine);
   FReadable:='; '+BProc.ReadableHdr;
   end;
  end
 else
  begin
  BTail:=FOrig; BLine:=ReadTillS(BTail,';');
  FSrcFile:=ParsTailExtractTagValue(BTail,'SrcFile');
  FAsmFile:=ParsTailExtractTagValue(BTail,'AsmFile');
  ParsTailExtractTagValue(BTail,'SrcPos',FSrcIdxL,FSrcIdxP);
  ParsTailExtractTagValue(BTail,'AsmPos',FAsmIdxL);
  FProcNameL:=ParsTailExtractTagValue(BTail,'ProcNameL');
  for BLayerIdx in TCompLayer do ParsTailExtractTagValue(FOrig,CCompLayerNames[BLayerIdx],FLayerIdxL[BLayerIdx]);
  ParsTailExtractTagValue(BTail,'UseMatr',FUseMatr);
  ParsTailExtractTagValue(BTail,'Esp',FStackOffs);
  BRegMap:=ParsTailExtractTagValue(BTail,'TargRegMap');
  ParsTailExtractTagValue(BTail,'A');
  CheckTag('SrcOrig',BTail);
  DelFirstSpace(BTail);
  if BTail<>'' then begin AddSpacesVarR(BLine,32); BLine:=BLine+' ; '+BTail; end;
  FLineType:=dltAsmLine;
  FReadable:=BLine;

  while BRegMap<>'' do
   begin
   BReadS:=ReadParamStr(BRegMap);
   if BReadS='' then break;
   FRegMap.Append(BReadS);
   end;
  end;
End;

{ TDbgInfoProc }

Constructor TDbgInfoProc.Create;
Begin
 Inherited;
 FChTargList:=TStringList.Create;
End;

Destructor TDbgInfoProc.Destroy;
Begin
 VsmListClear;
 FChTargList.Free;
 Inherited;
End;

Procedure TDbgInfoProc.VsmListClear;
Var
  BVsmIdx       : Integer;
Begin
 for BVsmIdx:=0 to Length(FVsmList)-1 do FVsmList[BVsmIdx].Free;
 FVsmList:=nil;
End;

Function InterpretType ( Const ATypeS : string ) : string;
Var
  BTypeS        : string;
Begin
 Result:='?';
 repeat
 if ATypeS='' then break;
 if ATypeS='l' then begin Result:='boolean'; break; end;
 if ATypeS='c' then begin Result:='char'; break; end;
 if ATypeS='b' then begin Result:='byte'; break; end;
 if ATypeS='w' then begin Result:='word'; break; end;
 if ATypeS='d' then begin Result:='cardinal'; break; end;
 if ATypeS='k' then begin Result:='shortint'; break; end;
 if ATypeS='m' then begin Result:='smallint'; break; end;
 if ATypeS='i' then begin Result:='integer'; break; end;
 if ATypeS='f' then begin Result:='real'; break; end;
 if ParsIsTypeArray(ATypeS) then begin Result:='array of '+InterpretType(ParsExtractTypeElemType(ATypeS)); break; end;
 if ParsIsTypeRecord(ATypeS) then begin Result:='record'; break; end;
 if ParsIsTypeStringP(ATypeS) then begin Result:='string'; break; end;
 if ATypeS[1]='p' then
  begin
  Result:='Pointer';
  if Length(ATypeS)<2 then break;
  BTypeS:=ATypeS; Delete(BTypeS,1,1);
  Result:=InterpretType(BTypeS)+'^';
  break;
  end;
 until TRUE;
End;

Procedure TDbgInfoProc.ParseHdr ( ALineIdxS : Integer; Const AHeader : string );
Var
  BHeader       : string;
  BProcParList  : string;
  BTarg         : string;
  BType         : string;
  BNameS,
  BTypeS        : string;
  BNewParam     : string;
  BVsmList      : string;
  BVsmIdx       : Integer;
  BVsmItem      : TVsmItem;
  BTargList     : string;
  BOffsThis     : Integer;
Begin
 FOffsWorst:=0;
 VsmListClear;
 FChTargList.Clear;
 FLineIdxS:=ALineIdxS;
 BHeader:=AHeader;
 FProcNameL:=CheckTag('Proc',BHeader);
 BTargList:=CheckTag('ChTargList',BHeader);
 BVsmList:=CheckTag('VarStackMap',BHeader);
 // VarStackMap
 BVsmIdx:=0;
 while BVsmList<>'' do
  begin
  BVsmItem:=TVsmItem.Create;
  if BVsmItem.LoadFromString(BVsmList)=FALSE then begin BVsmItem.Free; break; end;
  SetLength(FVsmList,BVsmIdx+1); FVsmList[BVsmIdx]:=BVsmItem; Inc(BVsmIdx);
  DelFirstSpace(BVsmList);
  end;
 // TargList
 while BTargList<>'' do
  begin
  BTarg:=ReadParamStr(BTargList);
  if BTarg='' then break;
  FChTargList.Append(BTarg);
  end;
 // Proc name and variables
 BProcParList:='';
 for BVsmIdx:=0 to Length(FVsmList)-1 do
  begin
  BVsmItem:=FVsmList[BVsmIdx];
  BTarg:=BVsmItem.VarName;
  if BTarg='' then break;
  BType:=ParsExtractType(BTarg);
  BTypeS:=InterpretType(BType);
  BNameS:=ParsExtractName(BTarg);
  BNewParam:='';
  if ParsIsTargParamConst(BTarg) then BNewParam:='Const '+BNameS+' : '+BTypeS
  else if ParsIsTargParamVar(BTarg) then BNewParam:='Var '+BNameS+' : '+BTypeS
  else if ParsIsTargParam(BTarg) then BNewParam:=BNameS+' : '+BTypeS;
  if BNewParam<>'' then
   begin
   if BProcParList<>'' then BProcParList:=BProcParList+'; ';
   BProcParList:=BProcParList+BNewParam;
   end;
  if BVsmItem.HasCopy then
   begin
   BOffsThis:=BVsmItem.VarOffset+BVsmItem.Size;
   if BOffsThis>FOffsWorst then FOffsWorst:=BOffsThis;
   end;
  end;
 BTypeS:=ParsExtractRetType(FProcNameL);
 if BTypeS='_' then FReadableHdr:='Procedure ' else FReadableHdr:='Function ';
 FReadableHdr:=FReadableHdr+ParsExtractName(FProcNameL);
 if BProcParList<>'' then FReadableHdr:=FReadableHdr+' ( '+BProcParList+' )';
 if BTypeS='_' then FReadableHdr:=FReadableHdr+';'
 else FReadableHdr:=FReadableHdr+' : '+InterpretType(BTypeS)+';';
End;

{ TDbgInfoFile }

Constructor TDbgInfoFile.Create;
Begin
 Inherited;
 FAsmList:=TStringList.Create;
 FSrcList:=TStringList.Create;
 FModuleList:=TStringList.Create;
 FList:=TStringList.Create;
End;

Destructor TDbgInfoFile.Destroy;
Begin
 ClearProcList;
 ClearLineList;
 FList.Free;
 FModuleList.Free;
 FSrcList.Free;
 FAsmList.Free;
 Inherited;
End;

Procedure TDbgInfoFile.ClearProcList;
Var
  BProcIdx      : Integer;
Begin
 for BProcIdx:=0 to Length(FProcList)-1 do FProcList[BProcIdx].Free;
 FProcList:=nil;
End;

Procedure TDbgInfoFile.ClearLineList;
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FLineList)-1 do FLineList[BLineIdx].Free;
 FLineList:=nil;
End;

Procedure TDbgInfoFile.ParseLst ( Const AAsmName : string; AList : TStringList; ALineS, ALineE : Integer );
Var
  BIndex        : Integer;
  BReadS        : string;
  BDbgLine      : TDbgInfoLine;
Begin
 ClearProcList;
 ClearLineList;

 FAsmList.Clear; FAsmList.Append(AAsmName);
 FSrcList.Clear; FModuleList.Clear;
 FLinkToSrc:=FALSE;
 FLineS:=ALineS; FLineE:=ALineE;
 FList.Clear;
 BIndex:=FLineS;
 while BIndex<FLineE do
  begin
  FList.Append(AList.Strings[BIndex]);
  inc(BIndex);
  end;

 SetLength(FLineList,FList.Count);
 BIndex:=0;
 while BIndex<FList.Count do
  begin
  BDbgLine:=TDbgInfoLine.Create;
  FLineList[BIndex]:=BDbgLine;
  BDbgLine.FDbgFile:=Self;
  BReadS:=FList.Strings[BIndex];
  BDbgLine.Parse(FLineS+BIndex,BReadS);
  if FProcList=nil then BDbgLine.DbgProc:=nil
  else BDbgLine.DbgProc:=FProcList[Length(FProcList)-1];
  if FModuleList.Count=0 then BDbgLine.ModuleName:=''
  else BDbgLine.ModuleName:=FModuleList.Strings[FModuleList.Count-1];
  inc(BIndex);
  end;
End;

Procedure TDbgInfoFile.ParseDbg ( AList : TStringList );
Var
  BIndex        : Integer;
  BReadS        : string;
  BDbgLine      : TDbgInfoLine;
Begin
 ClearProcList;
 ClearLineList;

 FAsmList.Clear; FSrcList.Clear; FModuleList.Clear;
 FLinkToSrc:=FALSE;
 FLineS:=0; FLineE:=AList.Count;
 FList.Assign(AList);

 SetLength(FLineList,FList.Count);
 BIndex:=0;
 while BIndex<FList.Count do
  begin
  BDbgLine:=TDbgInfoLine.Create;
  FLineList[BIndex]:=BDbgLine;
  BDbgLine.FDbgFile:=Self;
  BReadS:=FList.Strings[BIndex];
  BDbgLine.Parse(FLineS+BIndex,BReadS);
  inc(BIndex);
  end;
End;

Procedure TDbgInfoFile.AppendSrcName ( Const ASrcName : string );
Begin
 if FIsLst=FALSE then
  begin
  FSrcList.Append(ASrcName);
  FLinkToSrc:=TRUE;
  end;
End;

Procedure TDbgInfoFile.AppendModuleName ( Const AName : string );
Begin
 FModuleList.Append(AName);
End;

Function TDbgInfoFile.GetLine ( AIndex : Integer ) : TDbgInfoLine;
Begin
 Result:=nil;
 repeat
 if (AIndex<FLineS) or (AIndex>=FLineE) then break;
 Result:=FLineList[AIndex-FLineS];
 until TRUE;
End;

{Procedure TDbgInfoFile.ListIdxToSrcIdx ( AListIdx : Integer; Out ALine, APos : Integer );
Var
  BDbgLine      : TDbgInfoLine;
Begin
 ALine:=AListIdx-FLineS; APos:=0;
 repeat
 if ALine<0 then begin ALine:=-1; break; end;
 if ALine>=FLineE then begin ALine:=-1; break; end;
 BDbgLine:=FLineList[ALine];
 if FLinkToSrc then begin ALine:=BDbgLine.SrcIdxL; APos:=BDbgLine.SrcIdxP; end
 else ALine:=BDbgLine.AsmIdxL;
 until TRUE;
End;}

Function TDbgInfoFile.AppendProc ( ALineIdxS : Integer; Const AHeader : string ) : TDbgInfoProc;
Var
  BProcIdx      : Integer;
Begin
 Result:=TDbgInfoProc.Create;
 BProcIdx:=Length(FProcList); SetLength(FProcList,BProcIdx+1); FProcList[BProcIdx]:=Result;
 Result.ParseHdr(ALineIdxS,AHeader);
End;

Function TDbgInfoFile.ProcByAsmLineIdx ( AAsmLineIdx : Integer ) : TDbgInfoProc;
Var
  BProcIdx      : Integer;
  BProcThis,
  BProcNext     : TDbgInfoProc;
Begin
 BProcThis:=nil;
 repeat
 if (AAsmLineIdx<0) or (AAsmLineIdx>=FLineE) then break;
 BProcIdx:=0;
 while BProcIdx<Length(FProcList) do
  begin
  BProcNext:=FProcList[BProcIdx];
  if BProcNext.LineIdxS>AAsmLineIdx then break;
  BProcThis:=BProcNext;
  inc(BProcIdx);
  end;
 until TRUE;
 Result:=BProcThis;
End;

end.

