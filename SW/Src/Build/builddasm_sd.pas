unit BuildDasm_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, AsmBase_sd, MemSeg_sd, BuildBase_sd, DasmBase_sd, DasmRV_sd, DasmMS_sd;

Type
  TBuildDasm = class(TBuildBase)
  private
    FDasmEpList : TEpList;

    FChunkListFix,
    FChunkListRel   : TCodeChunkList;

    Function FindChunkFix ( AAddr : Cardinal ) : TCodeChunk;
    Function ReadBinS ( AAddr, ASize : Cardinal ) : string;
    Function ReadBinD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;

    Procedure CollectChunksFix;
    Procedure CollectChunksRel;

    Function VerboseBranchFrom ( AExec : TExecLineBase ) : string;
    Function DasmProcessChunk ( AChunk : TCodeChunk; AAddr : Cardinal; ACpuType : char; AExecToLabel : TExecLineBase; AAddLabel : char; AExportNow : boolean ) : boolean;
  protected
    Procedure DasmCollectChunks ( AConstFile : TAsmBase ); Override;
    Function DasmProcessFix : boolean; Override;
    Function DasmProcessRel ( AConstFile : TAsmBase ) : boolean; Override;
    Procedure DasmCorrectAddrFix; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function DasmMissing ( AIpPrev, AIpThis : Cardinal; Out AIsMissing : boolean ) : boolean; Override;
    Function CallOrJmp ( AAddr : Cardinal; ACpuType : char ) : TCallOrJmp; Override;
  end;

implementation

{ *** TBuildDasm *** }

Constructor TBuildDasm.Create;
Begin
 Inherited;
End;

Destructor TBuildDasm.Destroy;
Begin
 FChunkListFix:=nil; FChunkListRel:=nil;
 Inherited;
End;

Function IsInside ( ASrc : TAsmBase; AAddr : Cardinal ) : boolean;
Var
  BChunkIdx : Integer;
  BChunk    : TCodeChunk;
Begin
 Result:=FALSE;
 BChunkIdx:=0;
 while BChunkIdx<Length(ASrc.CodeChunkList) do
  begin
  BChunk:=ASrc.CodeChunkList[BChunkIdx];
  if BChunk.IsInside(AAddr) then begin Result:=TRUE; break; end;
  inc(BChunkIdx);
  end;
End;

Function TBuildDasm.FindChunkFix ( AAddr : Cardinal ) : TCodeChunk;
Var
  BChunkIdx : Integer;
  BChunk    : TCodeChunk;
Begin
 Result:=nil;
 BChunkIdx:=0;
 while BChunkIdx<Length(FChunkListFix) do
  begin
  BChunk:=FChunkListFix[BChunkIdx];
  if BChunk.IsInside(AAddr) then begin Result:=BChunk; break; end;
  inc(BChunkIdx);
  end;
End;

Function TBuildDasm.ReadBinS ( AAddr, ASize : Cardinal ) : string;
Var
  BChunk        : TCodeChunk;
  BSize         : Integer;
Begin
 Result:='';
 repeat
 BChunk:=FindChunkFix(AAddr);
 if BChunk=nil then break;
 if Length(BChunk.FixBinData)<=(AAddr-BChunk.FixBinBase) then break;
 BSize:=Length(BChunk.FixBinData)-(AAddr-BChunk.FixBinBase);
 if ASize<BSize then BSize:=ASize;
 Result:=Copy(BChunk.FixBinData,1+AAddr-BChunk.FixBinBase,BSize);
 until TRUE;
End;

Function TBuildDasm.ReadBinD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;
Var
  BDataS    : string;
Begin
 AData:=0;
 Result:=FALSE;
 repeat
 BDataS:=ReadBinS(AAddr,4);
 if Length(BDataS)<4 then break;
 AData:=(Cardinal(BDataS[4]) shl 24) or
        (Cardinal(BDataS[3]) shl 16) or
        (Cardinal(BDataS[2]) shl  8) or
        (Cardinal(BDataS[1]) shl  0);
 Result:=TRUE;
 until TRUE;
End;

Procedure TBuildDasm.CollectChunksFix;
Var
  BSrcIdx   : Integer;
  BSrc      : TAsmBase;
  BChunkIdx : Integer;
  BChunk    : TCodeChunk;
Begin
 FChunkListFix:=nil;
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BChunkIdx:=0;
  while BChunkIdx<Length(BSrc.CodeChunkList) do
   begin
   BChunk:=BSrc.CodeChunkList[BChunkIdx];
   if BChunk.CanRelocate=FALSE then CodeChunkListAppend(FChunkListFix,BChunk);
   inc(BChunkIdx);
   end;
  inc(BSrcIdx);
  end;
End;

Procedure TBuildDasm.CollectChunksRel;
Var
  BSrcIdx   : Integer;
  BSrc      : TAsmBase;
  BChunkIdx : Integer;
  BChunk    : TCodeChunk;
Begin
 FChunkListRel:=nil;
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BChunkIdx:=0;
  while BChunkIdx<Length(BSrc.CodeChunkList) do
   begin
   BChunk:=BSrc.CodeChunkList[BChunkIdx];
   if BChunk.CanRelocate then CodeChunkListAppend(FChunkListRel,BChunk);
   inc(BChunkIdx);
   end;
  inc(BSrcIdx);
  end;
End;

Procedure TBuildDasm.DasmCollectChunks ( AConstFile : TAsmBase );
Var
  BChunkIdx     : Integer;
  BChunk        : TCodeChunk;
Begin
 CollectChunksFix;
 CollectChunksRel;
 BChunkIdx:=0;
 while BChunkIdx<Length(FChunkListRel) do
  begin
  BChunk:=FChunkListRel[BChunkIdx];
  AConstFile.ConstFileAppend('@'+BChunk.ConstName,'dd 0, 0, 0, 0');
  inc(BChunkIdx);
  end;
End;

Function TBuildDasm.DasmProcessFix : boolean;
Var
  BChunkIdxA,
  BChunkIdxB    : Integer;
  BChunkA,
  BChunkB       : TCodeChunk;
  BSrcA,
  BSrcB         : TAsmBase;
  BCoreIdx      : Integer;
  BAddrA,
  BAddrB        : Cardinal;
  BEpIdx        : Integer;
  BSrcIdx       : Integer;
  BSeg          : TMemSeg;
  BEp           : TEntryPoint;
  BChunk        : TCodeChunk;
Begin
 Result:=TRUE;

 repeat
 if FChunkListFix=nil then break;
 CodeChunkListOrder(FChunkListFix);

 // Check for overlaps
 BChunkIdxA:=0;
 while BChunkIdxA<Length(FChunkListFix) do
  begin
  BChunkA:=FChunkListFix[BChunkIdxA];
  BChunkIdxB:=BChunkIdxA+1;
  while BChunkIdxB<Length(FChunkListFix) do
   begin
   BChunkB:=FChunkListFix[BChunkIdxB];
   if BChunkA.IsOverlap(BChunkB) then
    begin
    BSrcA:=TAsmBase(BChunkA.Parent);
    BSrcB:=TAsmBase(BChunkB.Parent);
    AppendError('e',BSrcB.SrcName,0,0,'Data overlap with previously declared file '+BSrcA.SrcName+'[R:TBuildDasm.DasmProcess]');
    Result:=FALSE;
    end;
   inc(BChunkIdxB);
   end;
  inc(BChunkIdxA);
  end;

 if Result=FALSE then break;
 Result:=TRUE;
 if FSrcList=nil then break;

 // Collect entry points
 FDasmEpList:=nil;
 BSeg:=MemSegSearch(FMemSegList,FDefCodeSeg);
 if BSeg=nil then begin AppendError('e','',0,0,'There is no segment named "'+FDefCodeSeg+'" which is mandatory to load entry points [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
 BAddrA:=BSeg.HwBase;
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  if FCoreList[1+BCoreIdx]='e' then
   begin
   if ReadBinD(BAddrA,BAddrB)=FALSE then begin AppendError('e','',0,0,'Cannot find entry point for core '+IntToStr(BCoreIdx)+'[R:TBuildDasm.DasmProcess]'); Result:=FALSE; end;
   BEp.FAddr:=BAddrB; BEp.FCpuType:='e';
   CheckAppendDasmEP(FDasmEpList,BEp);
   end;
  inc(BAddrA,4);
  inc(BCoreIdx);
  end;
 if Result=FALSE then break;

 // Disassemble (and collect new EPs)
 BEpIdx:=0;
 while BEpIdx<Length(FDasmEpList) do
  begin
  BChunk:=FindChunkFix(FDasmEpList[BEpIdx].FAddr);
  if BChunk=nil then begin AppendError('e','',0,0,'Cannot find entry point for core '+IntToStr(BCoreIdx)+'[R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
  if DasmProcessChunk(BChunk,FDasmEpList[BEpIdx].FAddr,FDasmEpList[BEpIdx].FCpuType,nil,'e',FALSE)=FALSE then begin Result:=FALSE; break; end;
  inc(BEpIdx);
  end;

 if Result=FALSE then break;

 // Export line list to Src
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  FSrcList[BSrcIdx].ExportExecLines(FMemSegList);
  inc(BSrcIdx);
  end;

 until TRUE;
End;

Function TBuildDasm.DasmProcessRel ( AConstFile : TAsmBase ) : boolean;
Var
  BChunkIdx     : Integer;
  BChunk        : TCodeChunk;
  BSrc          : TAsmBase;
  BAddr         : Cardinal;
  BEpCnt,
  BEpIdx        : Cardinal;
  BSeg          : TMemSeg;
  BLineData     : TAsmFlowLine;
Begin
 Result:=TRUE;

 repeat
 if FChunkListRel=nil then break;

 // Check for overlaps
 BChunkIdx:=0;
 while BChunkIdx<Length(FChunkListRel) do
  begin
  BChunk:=FChunkListRel[BChunkIdx]; BSrc:=TAsmBase(BChunk.Parent);
  BSeg:=MemSegSearch(FMemSegList,FDefCodeSeg);
  if BSeg=nil then begin AppendError('e','',0,0,'There is no segment named "'+FDefCodeSeg+'" [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
  BChunk.RelBinBase:=BSeg.HwBase+((BSeg.FillSize+15) and $FFFFFFF0);
  if BSeg.IsInside(BChunk.RelBinBase,BChunk.FixBinData)=FALSE then begin AppendError('e',BSrc.SrcName,1,1,'Code does not fit the segment "'+FDefCodeSeg+'" [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
  inc(BChunkIdx);
  end;

 if Result=FALSE then break;
 // Disassemble (and collect new EPs)
 BChunkIdx:=0;
 while BChunkIdx<Length(FChunkListRel) do
  begin
  BChunk:=FChunkListRel[BChunkIdx]; BSrc:=TAsmBase(BChunk.Parent);
  BEpCnt:=(BChunk.RelFileHdr shr 8) and $FF;
  BEpIdx:=0;
  while BEpIdx<BEpCnt do
   begin
   if BChunk.ReadBinD(BChunk.FixBinBase+4*4+4*BEpIdx,BAddr)=FALSE then begin AppendError('e',BSrc.SrcName,1,0,'Entry point '+IntToStr(BEpIdx)+' is not inside the file [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
   if DasmProcessChunk(BChunk,BAddr,'e',nil,'e',FALSE)=FALSE then begin Result:=FALSE; break; end;
   inc(BEpIdx);
   end;
  if Result=FALSE then break;
  BChunk.RebaseLines;
  BSrc.ExportExecLines(FMemSegList);
  BLineData:=AConstFile.ConstFileGetLineData('@'+BChunk.ConstName); if BLineData=nil then begin AppendError('e',BSrc.SrcName,1,0,'Internal error: cannot set label value in Const file [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
  if Length(BLineData.CodeBin)<>16 then begin AppendError('e',BSrc.SrcName,1,0,'Internal error: cannot set label value in Const file [R:TBuildDasm.DasmProcess]'); Result:=FALSE; break; end;
  BLineData.ClearDataBin; BLineData.AppendDataBinD(BChunk.RelBinBase); BLineData.AppendDataBinD(BChunk.FixBinBase); BLineData.AppendDataBinD(Length(BChunk.FixBinData)); BLineData.AppendDataBinD(0);
  inc(BChunkIdx);
  end;

 if Result=FALSE then break;

 until TRUE;
End;

Function TBuildDasm.CallOrJmp ( AAddr : Cardinal; ACpuType : char ) : TCallOrJmp;
Var
  BAddr         : Cardinal;
  BCodeBin      : string;
  BReadSize     : Cardinal;
  BExecLine     : TExecLineBase;
  BResult       : TCallOrJmp;
  BSeg          : TMemSeg;
Begin
 Result:=cjUnknown;
 BAddr:=AAddr;
 if ACpuType='e' then BExecLine:=TExecLineRV.Create
 else BExecLine:=TExecLineSD.Create;
 repeat
 BSeg:=MemSegSearch(FMemSegList,BAddr);
 if BSeg=nil then break;
 BCodeBin:=BSeg.RdData(BAddr,6);
 BReadSize:=Length(BCodeBin); if BReadSize=0 then break;
 if BExecLine.CmdDec(BAddr,BCodeBin)=FALSE then break;
 if BExecLine.IsDecStop then break;
 BResult:=BExecLine.CallOrJmp;
 if BResult<>cjUnknown then begin Result:=BResult; break; end;
 BAddr:=BAddr+Length(BExecLine.CodeBin);
 until FALSE;
 BExecLine.Free;
End;

Function TBuildDasm.DasmProcessChunk ( AChunk : TCodeChunk; AAddr : Cardinal; ACpuType : char; AExecToLabel : TExecLineBase; AAddLabel : char; AExportNow : boolean ) : boolean;
Var
  BAddr         : Cardinal;
  BCodeBin      : string;
  BReadSize     : Cardinal;
  BExecBase     : TExecLineBase;
  BExecRV       : TExecLineRV;
  BExecToLabel  : TExecLineBase;
  BAddLabel     : char;
  BSrc          : TAsmBase;
Begin
 Result:=FALSE;
 BAddLabel:=AAddLabel;
 BExecToLabel:=AExecToLabel;
 BAddr:=AAddr;
 BSrc:=TAsmBase(AChunk.Parent);
 repeat
 if AChunk.IsInside(BAddr)=FALSE then
  begin
  AppendError('wAddress out of range at 0x'+IntToHex(BAddr,8)+' (in order to force Disassembling to stop, insert a command with code 0000)[R:TBuildDasm.DasmProcessChain]');
  Result:=TRUE;
  break;
  end;
 BExecBase:=AChunk.ExecList[BAddr-AChunk.FixBinBase];
 if BExecBase<>nil then
  begin
  if BAddLabel<>#0 then begin BExecBase.SetLabel(BAddLabel); BAddLabel:=#0; end;
  if BExecToLabel<>nil then begin BExecToLabel.SetDstLabel(BExecBase.LabelName); BExecToLabel:=nil; end;
  Result:=TRUE;
  break;
  end;
 BCodeBin:=AChunk.ReadBinS(BAddr,6); BReadSize:=Length(BCodeBin);
 if BReadSize=0 then
  begin
  AppendError('e',BSrc.SrcName,1,0,'Cannot read any data at 0x'+IntToHex(BAddr,8)+VerboseBranchFrom(AExecToLabel)+'[R:TBuildDasm.DasmProcessChain]');
  break;
  end;
 BExecRV:=TExecLineRV.Create;
 AChunk.ExecList[BAddr-AChunk.FixBinBase]:=BExecRV;
 if BExecRV.CmdDec(BAddr,BCodeBin)=FALSE then
  begin
  AppendError('e',BSrc.SrcName,1,0,'Error parsing line at 0x'+IntToHex(BAddr,8)+' ('+BExecRV.LastError+')[R:TBuildDasm.DasmProcessChain]');
  break;
  end;
 if BAddLabel<>#0 then begin BExecRV.SetLabel(BAddLabel); BAddLabel:=#0; end;
 if BExecToLabel<>nil then begin BExecToLabel.SetDstLabel(BExecRV.LabelName); BExecToLabel:=nil; end;
 if BExecRV.IsDecStop then
  begin
  AChunk.ExecList[BAddr-AChunk.FixBinBase]:=nil;
  BExecRV.Free;
  Result:=TRUE;
  break;
  end;
 if AExportNow then BSrc.ExportExecLine(FMemSegList,BExecRV);
 if BExecRV.IsRet then begin Result:=TRUE; break; end;
 if BExecRV.IsJmp then
  begin
  if CallOrJmp(BAddr+BExecRV.Subdec.FImm,ACpuType)=cjCall then
   begin
   if DasmProcessChunk(AChunk,BAddr+BExecRV.Subdec.FImm,ACpuType,BExecRV,'c',AExportNow)=FALSE then break;
   BAddr:=BAddr+Length(BExecRV.CodeBin);
   end
  else
   begin
   BExecToLabel:=BExecRV; BAddLabel:='j';
   BAddr:=BAddr+BExecRV.Subdec.FImm;
   end;
  end
 else if BExecRV.IsJxx then
  begin
  if DasmProcessChunk(AChunk,BAddr+BExecRV.Subdec.FImm,ACpuType,BExecRV,'j',AExportNow)=FALSE then break;
  BAddr:=BAddr+Length(BExecRV.CodeBin);
  end
 {else if BExecRV.IsCall then
  begin
  if DasmProcessChain(BAddr+BExecRV.Subdec.FImm,BExecRV,'c',AExportNow)=FALSE then break;
  BAddr:=BAddr+Length(BExecRV.CodeBin);
  end}
 else BAddr:=BAddr+Length(BExecRV.CodeBin);
 until FALSE;
End;

Function TBuildDasm.VerboseBranchFrom ( AExec : TExecLineBase ) : string;
Begin
 Result:='';
 repeat
 if AExec=nil then break;
 Result:=' (Branch from 0x'+IntToHex(AExec.VirtAddr,8)+' "'+AExec.AsmLineS+'")';
 until TRUE;
End;

Procedure TBuildDasm.DasmCorrectAddrFix;
Var
  BChunkIdx     : Integer;
  BChunk        : TCodeChunk;
  BAddr,
  BFillSize     : Cardinal;
  BSeg          : TMemSeg;
Begin
 BChunkIdx:=0;
 while BChunkIdx<Length(FChunkListFix) do
  begin
  BChunk:=FChunkListFix[BChunkIdx];
  repeat
  BSeg:=MemSegSearch(FMemSegList,BChunk.FixBinBase);
  if BSeg=nil then break;
  BAddr:=BChunk.FixBinBase+Length(BChunk.FixBinData);
  BFillSize:=((BAddr-BSeg.HwBase)+$F) and $FFFFFFF0;
  if BSeg.FillSize>=BFillSize then break;
  BSeg.FillSize:=BFillSize;
  until TRUE;
  inc(BChunkIdx);
  end;
End;

Function TBuildDasm.DasmMissing ( AIpPrev, AIpThis : Cardinal; Out AIsMissing : boolean ) : boolean;
Var
  BExecToLabel,
  BExecBase     : TExecLineBase;
  BChunk        : TCodeChunk;
Begin
 Result:=FALSE; AIsMissing:=FALSE;
 repeat
 BChunk:=FindChunkFix(AIpThis);
 if BChunk=nil then begin Result:=TRUE; break; end; // Doesn't fall to FixChain
 BExecBase:=BChunk.ExecList[AIpThis-BChunk.FixBinBase];
 if BExecBase<>nil then begin Result:=TRUE; break; end; // Already exists
 AIsMissing:=TRUE;
 BExecToLabel:=nil;
 BChunk:=FindChunkFix(AIpPrev);
 if BChunk<>nil then BExecToLabel:=BChunk.ExecList[AIpPrev-BChunk.FixBinBase];
 if DasmProcessChunk(BChunk,AIpThis,'e',BExecToLabel,'j',TRUE)=FALSE then break;
 CreateLst;
 Result:=TRUE;
 until TRUE;
End;

end.

