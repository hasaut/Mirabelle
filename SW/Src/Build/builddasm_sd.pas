unit BuildDasm_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, AsmBase_sd, MemSeg_sd, BuildBase_sd, DasmBase_sd, DasmRV_sd, DasmMS_sd;

Type
  TBuildDasm = class(TBuildBase)
  private
    FDasmEpList : TEpList;

    FFixChainList   : TFixChainList;

    Procedure CollectFixChains;
    Function FindFixChain ( AAddr : Cardinal ) : TFixChain;
    Function ReadBinS ( AAddr, ASize : Cardinal ) : string;
    Function ReadBinD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;

    Function VerboseBranchFrom ( AExec : TExecLineBase ) : string;
    Function DasmProcessChain ( AAddr : Cardinal; ACpuType : char; AExecToLabel : TExecLineBase; AAddLabel : char; AExportNow : boolean ) : boolean;
  protected
    Function DasmProcess : boolean; Override;
    Procedure DasmCorrectAddr; Override;
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
 FFixChainList:=nil;
 Inherited;
End;

Function IsInside ( ASrc : TAsmBase; AAddr : Cardinal ) : boolean;
Var
  BChainIdx : Integer;
  BFixChain : TFixChain;
Begin
 Result:=FALSE;
 BChainIdx:=0;
 while BChainIdx<Length(ASrc.FixChainList) do
  begin
  BFixChain:=ASrc.FixChainList[BChainIdx];
  if BFixChain.IsInside(AAddr) then begin Result:=TRUE; break; end;
  inc(BChainIdx);
  end;
End;

Procedure TBuildDasm.CollectFixChains;
Var
  BSrcIdx   : Integer;
  BSrc      : TAsmBase;
  BChainIdx : Integer;
  BFixChain : TFixChain;
Begin
 FFixChainList:=nil;
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BChainIdx:=0;
  while BChainIdx<Length(BSrc.FixChainList) do
   begin
   BFixChain:=BSrc.FixChainList[BChainIdx];
   FixChainListAppend(FFixChainList,BFixChain);
   inc(BChainIdx);
   end;
  inc(BSrcIdx);
  end;
End;

Function TBuildDasm.FindFixChain ( AAddr : Cardinal ) : TFixChain;
Var
  BChainIdx : Integer;
  BChain    : TFixChain;
Begin
 Result:=nil;
 BChainIdx:=0;
 while BChainIdx<Length(FFixChainList) do
  begin
  BChain:=FFixChainList[BChainIdx];
  if BChain.IsInside(AAddr) then begin Result:=BChain; break; end;
  inc(BChainIdx);
  end;
End;

Function TBuildDasm.ReadBinS ( AAddr, ASize : Cardinal ) : string;
Var
  BChain        : TFixChain;
  BSize         : Integer;
Begin
 Result:='';
 repeat
 BChain:=FindFixChain(AAddr);
 if BChain=nil then break;
 if Length(BChain.FixBinData)<=(AAddr-BChain.FixBinBase) then break;
 BSize:=Length(BChain.FixBinData)-(AAddr-BChain.FixBinBase);
 if ASize<BSize then BSize:=ASize;
 Result:=Copy(BChain.FixBinData,1+AAddr-BChain.FixBinBase,BSize);
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

Function TBuildDasm.DasmProcess : boolean;
Var
  BChainIdxA,
  BChainIdxB    : Integer;
  BChainA,
  BChainB       : TFixChain;
  BSrcA,
  BSrcB         : TAsmBase;
  BCoreIdx      : Integer;
  BAddrA,
  BAddrB        : Cardinal;
  BEpIdx        : Integer;
  BSrcIdx       : Integer;
  BSeg          : TMemSeg;
  BEp           : TEntryPoint;
Begin
 Result:=TRUE;

 repeat
 CollectFixChains;
 if FFixChainList=nil then break;
 FixChainListOrder(FFixChainList);

 // Check for overlaps
 BChainIdxA:=0;
 while BChainIdxA<Length(FFixChainList) do
  begin
  BChainA:=FFixChainList[BChainIdxA];
  BChainIdxB:=BChainIdxA+1;
  while BChainIdxB<Length(FFixChainList) do
   begin
   BChainB:=FFixChainList[BChainIdxB];
   if BChainA.IsOverlap(BChainB) then
    begin
    BSrcA:=TAsmBase(BChainA.Parent);
    BSrcB:=TAsmBase(BChainB.Parent);
    AppendError('e',BSrcB.SrcName,0,0,'Data overlap with previously declared file '+BSrcA.SrcName+'[R:TBuildDasm.DasmProcess]');
    Result:=FALSE;
    end;
   inc(BChainIdxB);
   end;
  inc(BChainIdxA);
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
  if DasmProcessChain(FDasmEpList[BEpIdx].FAddr,FDasmEpList[BEpIdx].FCpuType,nil,'e',FALSE)=FALSE then begin Result:=FALSE; break; end;
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

Function TBuildDasm.DasmProcessChain ( AAddr : Cardinal; ACpuType : char; AExecToLabel : TExecLineBase; AAddLabel : char; AExportNow : boolean ) : boolean;
Var
  BAddr         : Cardinal;
  BChain        : TFixChain;
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
 repeat
 BChain:=FindFixChain(BAddr);
 if BChain=nil then
  begin
  AppendError('wAddress out of range at 0x'+IntToHex(BAddr,8)+' (in order to force Disassembling to stop, insert a command with code 0000)[R:TBuildDasm.DasmProcessChain]');
  Result:=TRUE;
  break;
  end;
 BExecBase:=BChain.ExecList[BAddr-BChain.FixBinBase];
 if BExecBase<>nil then
  begin
  if BAddLabel<>#0 then begin BExecBase.SetLabel(BAddLabel); BAddLabel:=#0; end;
  if BExecToLabel<>nil then begin BExecToLabel.SetDstLabel(BExecBase.LabelName); BExecToLabel:=nil; end;
  Result:=TRUE;
  break;
  end;
 BCodeBin:=ReadBinS(BAddr,6);
 BReadSize:=Length(BCodeBin);
 BSrc:=TAsmBase(BChain.Parent);
 if BReadSize=0 then
  begin
  AppendError('e',BSrc.SrcName,1,0,'Cannot read any data at 0x'+IntToHex(BAddr,8)+VerboseBranchFrom(AExecToLabel)+'[R:TBuildDasm.DasmProcessChain]');
  break;
  end;
 BExecRV:=TExecLineRV.Create;
 BChain.ExecList[BAddr-BChain.FixBinBase]:=BExecRV;
 if BExecRV.CmdDec(BAddr,BCodeBin)=FALSE then
  begin
  AppendError('e',BSrc.SrcName,1,0,'Error parsing line at 0x'+IntToHex(BAddr,8)+' ('+BExecRV.LastError+')[R:TBuildDasm.DasmProcessChain]');
  break;
  end;
 if BAddLabel<>#0 then begin BExecRV.SetLabel(BAddLabel); BAddLabel:=#0; end;
 if BExecToLabel<>nil then begin BExecToLabel.SetDstLabel(BExecRV.LabelName); BExecToLabel:=nil; end;
 if BExecRV.IsDecStop then
  begin
  BChain.ExecList[BAddr-BChain.FixBinBase]:=nil;
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
   if DasmProcessChain(BAddr+BExecRV.Subdec.FImm,ACpuType,BExecRV,'c',AExportNow)=FALSE then break;
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
  if DasmProcessChain(BAddr+BExecRV.Subdec.FImm,ACpuType,BExecRV,'j',AExportNow)=FALSE then break;
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
 Result:=' (Branch from 0x'+IntToHex(AExec.Addr,8)+' "'+AExec.AsmLineS+'")';
 until TRUE;
End;

Procedure TBuildDasm.DasmCorrectAddr;
Var
  BSrcIdx       : Integer;
  BSrc          : TAsmBase;
  BChainIdx     : Integer;
  BChain        : TFixChain;
  BAddr,
  BFillSize     : Cardinal;
  BSeg          : TMemSeg;
Begin
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BChainIdx:=0;
  while BChainIdx<Length(BSrc.FixChainList) do
   begin
   BChain:=BSrc.FixChainList[BChainIdx];
   repeat
   BSeg:=MemSegSearch(FMemSegList,BChain.FixBinBase);
   if BSeg=nil then break;
   BAddr:=BChain.FixBinBase+Length(BChain.FixBinData);
   BFillSize:=((BAddr-BSeg.HwBase)+$F) and $FFFFFFF0;
   if BSeg.FillSize>=BFillSize then break;
   BSeg.FillSize:=BFillSize;
   until TRUE;
   inc(BChainIdx);
   end;
  inc(BSrcIdx);
  end;
End;

Function TBuildDasm.DasmMissing ( AIpPrev, AIpThis : Cardinal; Out AIsMissing : boolean ) : boolean;
Var
  BExecToLabel,
  BExecBase     : TExecLineBase;
  BChain        : TFixChain;
Begin
 Result:=FALSE; AIsMissing:=FALSE;
 repeat
 BChain:=FindFixChain(AIpThis);
 if BChain=nil then begin Result:=TRUE; break; end; // Doesn't fall to FixChain
 BExecBase:=BChain.ExecList[AIpThis-BChain.FixBinBase];
 if BExecBase<>nil then begin Result:=TRUE; break; end; // Already exists
 AIsMissing:=TRUE;
 BExecToLabel:=nil;
 BChain:=FindFixChain(AIpPrev);
 if BChain<>nil then BExecToLabel:=BChain.ExecList[AIpPrev-BChain.FixBinBase];
 if DasmProcessChain(AIpThis,'e',BExecToLabel,'j',TRUE)=FALSE then break;
 CreateLst;
 Result:=TRUE;
 until TRUE;
End;

end.

