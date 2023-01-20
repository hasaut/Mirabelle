unit BuildBase_sd;

{$mode objfpc}{$H+}

interface

Uses
  Classes, SysUtils, AsmTypes_sd, ParsBase_sd, DbgBase_sd, LlvmBase_sd, AsmBase_sd, MemSeg_sd, DasmBase_sd;

Type
  TEntryPoint = record
    FAddr       : Cardinal;
    FCpuType    : char;
  end;
  TEpList = array of TEntryPoint;

  TBuildBase = class(TObject)
  private
    FFileList           : TStringList;
    FIncSearchPath      : TStringList;
    FOnAppendError      : TOnViewAny;
    FPrjPath,
    FDstPath            : string;

    FPrjFullName,
    FPrjName            : string;
    FBinName,
    FLstName,
    FDbgName,
    FMapName            : string;

    FUid                : Integer;
    FErrWarnCnt         : Cardinal; // Used to prevent View overload. If there are too many errors and warnings, we will stop viewing them
    FCodeBin            : string;

    Procedure ClearSrcList;
    Procedure ClearIncList;
    Procedure AppendErrorA ( Const AErrorCode : string );

    Function GetUses ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TLlvmModule;
    Function ReadInc ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TParsBase;

    Procedure LtoMarkAllUsed;
    Function SetRamEndLabel ( AConstFile : TAsmBase ) : boolean;

  protected
    FPrjParams          : TStringList;
    FSrcList            : TAsmBaseList;
    FIncList            : TAsmBaseList;
    FLinkRefList        : TAsmRefList;
    FCoreList           : string;
    FMemSegList         : TMemSegList;
    FDefCodeSeg,
    FDefDataSeg         : string;

    FLst            : TStringList;
    FDbg            : TStringList;
    FBin            : TMemoryStream;
    FMap            : TStringList;

    Procedure AppendError ( AErrorList : TStringList ); Overload;
    Procedure AppendError ( AErrorType : char; ARef : TAsmRef; Const AErrorCode : string );
    Procedure AppendError ( AErrorType : char; Const AFilename : string; ALine, APos : Integer; Const AErrorCode : string );
    Procedure AppendError ( Const AMessage : string );
    Function CreateUnit ( Const AFilename : string ) : TAsmBase; Virtual; Abstract;
    Procedure DasmCorrectAddrFix; Virtual; Abstract;
    Procedure DasmCollectChunks ( AConstFile : TAsmBase ); Virtual; Abstract;
    Function DasmProcessFix : boolean; Virtual; Abstract;
    Function DasmProcessRel ( AConstFile : TAsmBase ) : boolean; Virtual; Abstract;
    Procedure LtoProcess; Virtual; // This marks all lines as used
    Procedure StackProcess; Virtual; Abstract;
    Function FindPublicRef ( Const AName : string ) : TAsmRef;

    Procedure CreateLst;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure Clear;
    Procedure InitPrj ( Const APrjFullName, ADstPath : string; Const ACoreList : string; Const ASegListS : string; Const ADefSegNames : string; Const AExtCompiler, ALinkOptions : string );
    Procedure AppendIncPath ( Const APath : string );
    Procedure AppendSrcName ( Const AFilename : string );
    Function AppendSrc ( Const AFilename : string; Const ADbgPath : string ) : TAsmBase;
    Function AppendInc ( Const AFilename : string ) : TAsmBase;
    Function DeleteOld : boolean;
    Function SaveNew : boolean;
    Function Build : boolean;
    Function ExportExec : string;

    Function DasmMissing ( AIpPrev, AIpThis : Cardinal; Out AIsMissing : boolean ) : boolean; Virtual; Abstract;
    Function CallOrJmp ( AAddr : Cardinal; ACpuType : char ) : TCallOrJmp; Virtual; Abstract;

    property OnAppendError : TOnViewAny read FOnAppendError write FOnAppendError;
    property CodeBin : string read FCodeBin;
    property Lst : TStringList read FLst;
    property Dbg : TStringList read FDbg;
    property Bin : TMemoryStream read FBin;
    property Map : TStringList read FMap;

    property PrjName : string read FPrjName;
    property PrjPath : string read FPrjPath;
    property LstName : string read FLstName;

    property MemSegList : TMemSegList read FMemSegList;
    property DefCodeSeg : string read FDefCodeSeg;
    property DefDataSeg : string read FDefDataSeg;
  end; (* TBuildBase *)

Procedure CorrectSearchPath ( ASearchPath : TStringList );
Procedure CheckAppendDasmEp ( Var AList : TEpList; Const AEp : TEntryPoint );

implementation

Uses
  ConComL, ConComS;

Const
  //CDbgPath = 'C:\ZukHL\UtilL\CeresIde\CompDbg\';
  CDbgPath = '';

Procedure CorrectSearchPath ( ASearchPath : TStringList );
Var
  BIndex        : Integer;
  BList         : TStringList;
  BPath         : string;

Begin
 BList:=TStringList.Create;

 BIndex:=0;
 while BIndex<ASearchPath.Count do
  begin
  BPath:=ASearchPath.Strings[BIndex];

  repeat
  DelFirstSpace(BPath);
  DelLastSpace(BPath);
  if BPath='' then break;
  while BPath<>'' do
   begin
   if BPath[Length(BPath)]<>'\' then break;
   Delete(BPath,Length(BPath),1);
   end;
  if BPath='' then break;
  if DirectoryExists(BPath)=FALSE then break;
  BList.Append(BPath);
  until TRUE;

  inc(BIndex);
  end;

 ASearchPath.Assign(BList);

 BList.Free;
End;

Procedure CheckAppendDasmEp ( Var AList : TEpList; Const AEp : TEntryPoint );
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  if AList[BIndex].FAddr=AEp.FAddr then break;
  inc(BIndex);
  end;
 if BIndex=Length(AList) then
  begin
  BIndex:=Length(AList); SetLength(AList,BIndex+1); AList[BIndex]:=AEp;
  end;
End;

// *** TBuildBase ***

Constructor TBuildBase.Create;
Begin
 Inherited;
 FFileList:=TStringList.Create;
 FIncSearchPath:=TStringList.Create;
 FLst:=TStringList.Create;
 FDbg:=TStringList.Create;
 FBin:=TMemoryStream.Create;
 FMap:=TStringList.Create;
 FPrjParams:=TStringList.Create;
End;

Destructor TBuildBase.Destroy;
Begin
 Clear;
 FPrjParams.Free;
 FMap.Free;
 FBin.Free;
 FDbg.Free;
 FLst.Free;
 FIncSearchPath.Free;
 FFileList.Free;
 Inherited;
End;

Procedure TBuildBase.Clear;
Begin
 MemSegListClear(FMemSegList);
 ClearSrcList;
 ClearIncList;
 FIncSearchPath.Clear;
 FFileList.Clear;
 FCodeBin:='';
 FLst.Clear;
 FDbg.Clear;
 FBin.Clear;
 FMap.Clear;
End;

Procedure TBuildBase.InitPrj ( Const APrjFullName, ADstPath : string; Const ACoreList : string; Const ASegListS : string; Const ADefSegNames : string; Const AExtCompiler, ALinkOptions : string );
Var
  BSegIdx   : Integer;
  BSeg      : TMemSeg;
  BExt      : string;
  BDummyS   : string;
Begin
 FPrjParams.Clear;
 FPrjParams.Values['ExtCompiler']:=AExtCompiler;
 FPrjParams.Values['LinkOptions']:=ALinkOptions;

 FPrjFullName:=APrjFullName;
 SplitFilename(FPrjFullName,FPrjPath,FPrjName,BExt);
 FDstPath:=ADstPath;

 FCoreList:=ACoreList;
 MemSegListImport(FMemSegList,ASegListS);
 BDummyS:=ADefSegNames;
 FDefCodeSeg:=ReadParamStr(BDummyS); if FDefCodeSeg='' then AppendError('eDefault code segment is not defined [TBuildBase.InitPrj]');
 FDefDataSeg:=ReadParamStr(BDummyS); if FDefDataSeg='' then AppendError('eDefault data segment is not defined [TBuildBase.InitPrj]');

 FBinName:=AssembleFullName(FDstPath,FPrjName,'bin');
 FLstName:=AssembleFullName(FDstPath,FPrjName,'lst');
 FDbgName:=AssembleFullName(FDstPath,FPrjName,'dbg');
 FMapName:=AssembleFullName(FDstPath,FPrjName,'map');

 for BSegIdx:=0 to Length(FMemSegList)-1 do
  begin
  BSeg:=FMemSegList[BSegIdx];
  BSeg.HexName:=AssembleFullName(FDstPath,FPrjName+'_'+BSeg.SegName,'hex');
  BSeg.MemName:=AssembleFullName(FDstPath,FPrjName+'_'+BSeg.SegName,'mem');
  end;
End;

Procedure TBuildBase.ClearSrcList;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FSrcList) do
  begin
  FSrcList[BIndex].Free;
  inc(BIndex);
  end;
 SetLength(FSrcList,0);
End;

Procedure TBuildBase.ClearIncList;
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FIncList) do
  begin
  FIncList[BIndex].Free;
  inc(BIndex);
  end;
 SetLength(FIncList,0);
End;

Procedure TBuildBase.AppendIncPath ( Const APath : string );
Begin
 FIncSearchPath.Append(APath);
 CorrectSearchPath(FIncSearchPath);
End;

Procedure TBuildBase.AppendSrcName ( Const AFilename : string );
Begin
 FFileList.Append(AFilename);
End;

{Procedure TBuildBase.SetSegments ( ACodeSeg, ADataSeg : word );
Begin
 FCodeStart:=ACodeSeg;
 FDataStart:=ADataSeg;
End;}

Procedure TBuildBase.AppendErrorA ( Const AErrorCode : string );
Var
  BDoAppend : boolean;
Begin
 BDoAppend:=FALSE;
 repeat
 if AErrorCode='' then break;
 if AErrorCode[1] in ['e', 'w'] then
 else begin BDoAppend:=TRUE; break; end;
 inc(FErrWarnCnt);
 if FErrWarnCnt>100 then break;
 if FErrWarnCnt=100 then
  begin
  if Assigned(FOnAppendError) then FOnAppendError('eToo many error messages. The rest will be skipped [R:TBuildBase.AppendErrorA]');
  break;
  end;
 BDoAppend:=TRUE;
 until TRUE;
 if BDoAppend and Assigned(FOnAppendError) then FOnAppendError(AErrorCode);
End;

Procedure TBuildBase.AppendError ( Const AMessage : string );
Begin
 AppendErrorA(AMessage);
End;

Procedure TBuildBase.AppendError ( AErrorList : TStringList );
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to AErrorList.Count-1 do AppendErrorA(AErrorList.Strings[BIndex]);
End;

Procedure TBuildBase.AppendError ( AErrorType : char; ARef : TAsmRef; Const AErrorCode : string );
Begin
 ARef.AppendError(AErrorType,AErrorCode);
End;

Procedure TBuildBase.AppendError ( AErrorType : char; Const AFilename : string; ALine, APos : Integer; Const AErrorCode : string );
Begin
 AppendErrorA(FormatError(AErrorType+AErrorCode,AFilename,ALine,APos));
End;

Function TBuildBase.AppendSrc ( Const AFilename : string; Const ADbgPath : string ) : TAsmBase;
Var
  BIndex        : Integer;
Begin
 Result:=CreateUnit(AFilename);
 Result.Init(AFilename,FPrjPath,FDstPath,FIncSearchPath,FDefCodeSeg,FDefDataSeg,@GetUses,@ReadInc,@FUid);
 if Result.Module<>nil then Result.Module.DbgPath:=ADbgPath;
 BIndex:=Length(FSrcList); SetLength(FSrcList,BIndex+1); FSrcList[BIndex]:=Result;
 Result.OnAppendError:=@AppendErrorA;
End;

Function TBuildBase.AppendInc ( Const AFilename : string ) : TAsmBase;
Var
  BIndex        : Integer;
Begin
 Result:=CreateUnit(AFilename);
 Result.Init(AFilename,FPrjPath,FDstPath,FIncSearchPath,FDefCodeSeg,FDefDataSeg,@GetUses,@ReadInc,@FUid);
 BIndex:=Length(FIncList); SetLength(FIncList,BIndex+1); FIncList[BIndex]:=Result;
 Result.OnAppendError:=@AppendErrorA;
End;

Function DeleteFileA ( Const AFilename : string ) : boolean;
Begin
 if FileExists(AFilename) then Result:=DeleteFile(AFilename)
 else Result:=TRUE;
End;

Function TBuildBase.DeleteOld : boolean;
Var
  BSegIdx   : Integer;
  BSeg      : TMemSeg;
Begin
 Result:=TRUE;
 if DeleteFileA(FBinName)=FALSE then begin AppendError('wCannot delete old file '+FBinName); Result:=FALSE; end;
 if DeleteFileA(FLstName)=FALSE then begin AppendError('wCannot delete old file '+FLstName); Result:=FALSE; end;
 if DeleteFileA(FDbgName)=FALSE then begin AppendError('wCannot delete old file '+FDbgName); Result:=FALSE; end;
 if DeleteFileA(FMapName)=FALSE then begin AppendError('wCannot delete old file '+FMapName); Result:=FALSE; end;
 for BSegIdx:=0 to Length(FMemSegList)-1 do
  begin
  BSeg:=FMemSegList[BSegIdx];
  if DeleteFileA(BSeg.HexName)=FALSE then begin AppendError('wCannot delete old file '+BSeg.HexName); Result:=FALSE; end;
  if DeleteFileA(BSeg.MemName)=FALSE then begin AppendError('wCannot delete old file '+BSeg.MemName); Result:=FALSE; end;
  end;
End;

Function TBuildBase.SaveNew : boolean;
Var
  BSegIdx   : Integer;
  BSeg      : TMemSeg;
Begin
 Result:=TRUE;
 repeat
 try
   if ForceDirectories(FDstPath)=FALSE then
    begin
    AppendError('eCannot create directory '+FDstPath);
    Result:=FALSE;
    break;
    end;
 except
   AppendError('eUnexpected error when creating directory '+FDstPath);
   Result:=FALSE;
   break;
 end;
 try FBin.SaveToFile(FBinName); except AppendError('eCannot save file '+FBinName); Result:=FALSE; end;
 try FLst.SaveToFile(FLstName); except AppendError('eCannot save file '+FLstName); Result:=FALSE; end;
 try FDbg.SaveToFile(FDbgName); except AppendError('eCannot save file '+FDbgName); Result:=FALSE; end;
 try FMap.SaveToFile(FMapName); except AppendError('eCannot save file '+FMapName); Result:=FALSE; end;
 for BSegIdx:=0 to Length(FMemSegList)-1 do
  begin
  BSeg:=FMemSegList[BSegIdx];
  try BSeg.Hex.SaveToFile(BSeg.HexName); except AppendError('eCannot save file '+BSeg.HexName); Result:=FALSE; end;
  try BSeg.Mem.SaveToFile(BSeg.MemName); except AppendError('eCannot save file '+BSeg.MemName); Result:=FALSE; end;
  end;
 until TRUE;
End;

Function TBuildBase.FindPublicRef ( Const AName : string ) : TAsmRef;
Var
  BRefIdx   : Integer;
  BRef      : TAsmRef;
Begin
 Result:=nil;
 BRefIdx:=0;
 while BRefIdx<Length(FLinkRefList) do
  begin
  BRef:=FLinkRefList[BRefIdx];
  if BRef.Name=AName then begin Result:=BRef; break; end;
  inc(BRefIdx);
  end;
End;

Function TBuildBase.Build : boolean;
Var
  BIndex        : Integer;
  BDummyS       : string;
  BHasErrors    : boolean;
  BConstFile,
  BSrc          : TAsmBase;
  BSrcIdx       : Integer;
  BLinkRefSize  : Integer;
  BRefIdxA,
  BRefIdxB      : Integer;
  BRefA,
  BRefB         : TAsmRef;
  BWrIdx,
  BRdIdx        : Integer;
  BIsAddrOor    : boolean;
  BSegIdx       : Integer;
  BSeg          : TMemSeg;
Begin
 Result:=FALSE;
 FLinkRefList:=nil;
 FLst.Clear; FDbg.Clear; FBin.Clear; FMap.Clear;
 FUid:=0;
 FErrWarnCnt:=0;

 repeat
 if FFileList.Count=0 then begin AppendError('eProject file list is empty'); break; end;

 //if FSegDimList[astCode].FHwWidth in [2, 4, 8, 16] then
 //else begin AppendError('eInvalid code memory width (check project parameters) [R:TBuildBase.Build]'); break; end;
  
 ClearSrcList; ClearIncList;

 // Normal file list
 for BSrcIdx:=0 to FFileList.Count-1 do
  begin
  AppendSrc(FFileList.Strings[BSrcIdx],CDbgPath);
  end;

 // Auto-generated constant file
 BConstFile:=AppendSrc('','');

 // Loading sources
 BIndex:=0;
 while BIndex<FFileList.Count do
  begin
  if FSrcList[BIndex].LoadSrc=FALSE then break;
  inc(BIndex);
  end;

 if BIndex<>FFileList.Count then begin AppendError('e',FFileList.Strings[BIndex],0,0,'Cannot open source file'); break; end;

 // Prepare Const file
 BConstFile.ConstFileStart;

 // Prepare code chunks
 DasmCollectChunks(BConstFile);

 // Append EndLabel to ConstFile
 BConstFile.ConstFileAppend('@HRamEnd','dd 0');

 // Disassemble HEX files (Fixed addressing)
 if DasmProcessFix=FALSE then break;

 // Compiling individually
 BHasErrors:=FALSE;
 BIndex:=0;
 while BIndex<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BIndex];
  repeat
  if BSrc.CodeChunkList<>nil then break;
  BSrc.Compile;
  if BSrc.Module<>nil then BSrc.Module.GenCompDbg(FDbg);
  if BSrc.GetErrorCountA<>0 then BHasErrors:=TRUE;
  if BIndex<>FFileList.Count then BConstFile.ConstFileAppend(BSrc.ConstStrList);
  until TRUE;
  inc(BIndex);
  end;
 if BHasErrors then break;

 // Create a common public list
 BLinkRefSize:=0;
 for BSrcIdx:=0 to Length(FSrcList)-1 do BLinkRefSize:=BLinkRefSize+Length(FSrcList[BSrcIdx].PublicList);
 SetLength(FLinkRefList,BLinkRefSize);
 BWrIdx:=0;
 for BSrcIdx:=0 to Length(FSrcList)-1 do
  begin
  BSrc:=FSrcList[BSrcIdx];
  for BRdIdx:=0 to Length(BSrc.PublicList)-1 do
   begin
   FLinkRefList[BWrIdx]:=BSrc.PublicList[BRdIdx];
   inc(BWrIdx);
   end;
  end;

 // Check public list for unique names
 BRefIdxA:=1;
 while BRefIdxA<Length(FLinkRefList) do
  begin
  BRefA:=FLinkRefList[BRefIdxA];
  BRefIdxB:=0;
  while BRefIdxB<BRefIdxA do
   begin
   BRefB:=FLinkRefList[BRefIdxB];
   if BRefA.Name=BRefB.Name then
    begin
    BRefB.AppendError('e','Linker error. Duplicated public reference, declared earlier in file "'+BRefA.Param.Line.FileName+'"[R:TBuildBase.Build]');
    BHasErrors:=TRUE;
    end;
   inc(BRefIdxB);
   end;
  inc(BRefIdxA);
  end;
 if BHasErrors then break;

 // LTO
 if StrInList('lto',FPrjParams.Values['LinkOptions']) then
  begin
  LtoProcess;
  AppendError('iLTO process finished');
  end
 else
  begin
  LtoMarkAllUsed;
  AppendError('hLTO is disabled');
  end;

 // Stack
 StackProcess;
 AppendError('iStack process finished');

 // Preserve original offsets before doing a linker loop
 for BSrcIdx:=0 to Length(FSrcList)-1 do FSrcList[BSrcIdx].SaveCodeBin;

 // Linker loop
 repeat
 BIsAddrOor:=FALSE;

 // Fix physical addreses
 MemSegListReset(FMemSegList);
 DasmCorrectAddrFix;
 for BSrcIdx:=0 to Length(FSrcList)-1 do
  begin
  BSrc:=FSrcList[BSrcIdx];
  repeat
  if BSrc.CodeChunkList<>nil then break;
  BSrc.FixAddr(FMemSegList);
  BSrc.FillPublicAddr;
  //BSrc.CheckExternUsed;
  if BSrc.GetErrorCountA<>0 then BHasErrors:=TRUE;
  until TRUE;
  end;
 if BHasErrors then break;

 // Correct internal and external references individually for each src file
 // Update all references in the text
 for BSrcIdx:=0 to Length(FSrcList)-1 do
  begin
  BSrc:=FSrcList[BSrcIdx];
  repeat
  if BSrc.CodeChunkList<>nil then break;
  BSrc.FillLabelList;
  BSrc.FillExternList(FLinkRefList);
  BSrc.UpdateTextRefs(BIsAddrOor);
  if BSrc.GetErrorCountA<>0 then BHasErrors:=TRUE;
  until TRUE;
  end;
 if BHasErrors then break;

 if BIsAddrOor=FALSE then break;
 // Correct OOR
 for BSrcIdx:=0 to Length(FSrcList)-1 do
  begin
  BSrc:=FSrcList[BSrcIdx];
  repeat
  if BSrc.CodeChunkList<>nil then break;
  // Restore original offsets
  BSrc.LoadCodeBin;
  // Correct "Out of Range"
  BSrc.CorrectAddrOor;
  until TRUE;
  end;
 until FALSE; // Linker loop

 if BHasErrors then break;

 // Disassemble HEX files (Floating addressing)
 SetRamEndLabel(BConstFile);
 if DasmProcessRel(BConstFile)=FALSE then break;

 // Create Map
 BRefIdxA:=0;
 while BRefIdxA<Length(FLinkRefList) do
  begin
  BRefA:=FLinkRefList[BRefIdxA];
  if BRefA.Used then
   begin
   BDummyS:=BRefA.Name+' '+IntToHex(BRefA.ObjectAddr+BRefA.FieldAddr,8);
   FMap.Append(BDummyS);
   end;
  inc(BRefIdxA);
  end;

 // Create binary
 for BSrcIdx:=0 to Length(FSrcList)-1 do FSrcList[BSrcIdx].WriteCodeBin(FMemSegList);
 FCodeBin:='';
 BSegIdx:=0;
 while BSegIdx<Length(FMemSegList) do
  begin
  BSeg:=FMemSegList[BSegIdx];
  if (BSeg.SegFlags and $08)<>0 then FCodeBin:=FCodeBin+BSeg.Bin;
  inc(BSegIdx);
  end;
 FBin.Clear; FBin.Write(FCodeBin[1],Length(FCodeBin));

 CreateLst;

 // Create HEX
 BSegIdx:=0;
 while BSegIdx<Length(FMemSegList) do
  begin
  BSeg:=FMemSegList[BSegIdx];
  BSeg.CreateHex;
  BSeg.CreateMem;
  inc(BSegIdx);
  end;

 Result:=TRUE;
 until TRUE;

 FLinkRefList:=nil;
End;

Function TBuildBase.SetRamEndLabel ( AConstFile : TAsmBase ) : boolean;
Var
  BSegData  : TMemSeg;
  BRamEnd   : Cardinal;
  BLineData : TAsmFlowLine;
Begin
 Result:=FALSE;
 repeat
 BSegData:=MemSegSearch(FMemSegList,FDefDataSeg); if BSegData=nil then begin AppendError('e','',0,0,'There is no segment named "'+FDefDataSeg+'" [R:TBuildBase.SetRamEndLabel]'); break; end;
 BRamEnd:=BSegData.HwBase+((BSegData.FillSize+15) and $FFFFFFF0);
 BLineData:=AConstFile.ConstFileGetLineData('@HRamEnd'); if BLineData=nil then begin AppendError('e','',0,0,'Internal error setting RamEnd value [R:TBuildBase.SetRamEndLabel]'); break; end;
 if Length(BLineData.CodeBin)<>4 then begin AppendError('e','',0,0,'Internal error setting RamEnd value [R:TBuildBase.SetRamEndLabel]'); break; end;
 BLineData.ClearDataBin; BLineData.AppendDataBinD(BRamEnd);
 Result:=TRUE;
 until TRUE;
End;

Procedure TBuildBase.CreateLst;
Var
  BIndex        : Integer;
  BSrc          : TAsmBase;
  BSrcIdx       : Integer;
  BSegIdx       : Integer;
  BSeg          : TMemSeg;
  BTypeS,
  BAddrS,
  BDataS,
  BOrigS        : string;
  BAddr         : Cardinal;
  BSegType      : char;
Begin
 FLst.Clear;
 BSegIdx:=0;
 while BSegIdx<Length(FMemSegList) do
  begin
  FMemSegList[BSegIdx].IpToLineClear;
  inc(BSegIdx);
  end;

 // Create lst
 for BSrcIdx:=0 to Length(FSrcList)-1 do
  begin
  BSrc:=FSrcList[BSrcIdx];
  FLst.Append('');
  FLst.Append('');
  FLst.Append(';@A '+BSrc.AsmName+' '+BSrc.SrcName);
  BSrc.WriteLst(FLst);
  end;

 // Create LineToIp index
 BIndex:=0;
 while BIndex<FLst.Count do
  begin
  BAddrS:=''; BDataS:=''; BOrigS:='';
  SplitLineLst(FLst.Strings[BIndex],BTypeS,BAddrS,BDataS,BOrigS,BSegType);
  if (BAddrS<>'') and (BSegType='c') and Hex32ToInt(BAddrS,BAddr) then
   begin
   BSeg:=MemSegSearch(FMemSegList,BAddr);
   if BSeg<>nil then BSeg.IpToLine[BAddr-BSeg.HwBase]:=BIndex;
   end;
  inc(BIndex);
  end;
End;

Function TBuildBase.GetUses ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TLlvmModule;
Var
  BFilename     : string;
  BPath,
  BName,
  BExt          : string;
  BFullName     : string;
  BIndex        : Integer;
  BList         : TStringList;
  BSrc          : TAsmBase;
Begin
 Result:=nil;
 BList:=TStringList.Create;
 AError:='Internal error [TBuildBase.GetUses]: Undefined error';

 BFilename:=LowerCase(AUnitName);
 BPath:=''; BName:=''; BExt:='';
 SplitFilename(AUnitName,BPath,BName,BExt);
 if BExt='' then
  begin
  if AIncType=itPasUnit then begin BFilename:=BFilename+'.pas'; BExt:='pas'; end
  else if AIncType=itPyUnit then begin BFilename:=BFilename+'.py'; BExt:='py'; end;
  end;

 BSrc:=nil;

 repeat
 BIndex:=0;
 while BIndex<Length(FSrcList) do
  begin
  if LowerCase(ExtractFileName(FSrcList[BIndex].SrcName))=BFilename then begin BSrc:=FSrcList[BIndex]; break; end;
  inc(BIndex);
  end;
 if BSrc=nil then
  begin
  BFullName:='';
  BIndex:=0;
  while BIndex<FIncSearchPath.Count do
   begin
   BFullName:=FileSearch(AssembleFullName('',BName,BExt),FIncSearchPath.Strings[BIndex],FALSE);
   if BFullName<>'' then break;
   Inc(BIndex);
   end;
  if BIndex<FIncSearchPath.Count then BSrc:=AppendSrc(BFullName,'');
  end;
 if BSrc=nil then begin AError:='Include or unit file ['+BFilename+'] is not found [R:TBuildBase.GetUses]'; break; end;

 // if BSrc is just appended, the type of compiler/parser is determined inside AppendUnit, then in SetFileName
 if BSrc.Parser=nil then begin AError:='Internal error: BSrc.FPars=nil [R:TBuildBase.GetUses]'; break; end;

 if BSrc.Parser.ParsState=psParsStart then begin AError:='Include dependency loop [R:TBuildBase.GetUses]'; break; end;
 if BSrc.Parser.ParsState=psParsEnd then begin AError:=''; Result:=BSrc.Module; break; end;

 // BSrc.FXComp.CompState=csUnknown
 if BSrc.LoadSrc=FALSE then begin AError:='Cannot load source file "'+BSrc.SrcName+'" [R:TBuildBase.GetUses]'; break; end;

 BSrc.Compile;
 Result:=BSrc.Module;
 until TRUE;

 BList.Free;
End;

Function TBuildBase.ReadInc ( Const AUnitName : string; AIncType : TIncType; Var AError : string ) : TParsBase;
Var
  BFilename     : string;
  BPath,
  BName,
  BExt          : string;
  BFullName     : string;
  BIndex        : Integer;
  BList         : TStringList;
  BSrc          : TAsmBase;
Begin
 Result:=nil;
 BList:=TStringList.Create;
 AError:='Internal error [TBuildBase.ReadInc]: Undefined error';

 BFilename:=LowerCase(AUnitName);
 BPath:=''; BName:=''; BExt:='';
 SplitFilename(AUnitName,BPath,BName,BExt);
 if BExt='' then
  begin
  if AIncType=itPasUnit then begin BFilename:=BFilename+'.pas'; BExt:='pas'; end
  else if AIncType=itPyUnit then begin BFilename:=BFilename+'.py'; BExt:='py'; end;
  end;

 BSrc:=nil;

 repeat
 BIndex:=0;
 while BIndex<Length(FSrcList) do
  begin
  if LowerCase(ExtractFileName(FSrcList[BIndex].SrcName))=BFilename then begin BSrc:=FSrcList[BIndex]; break; end;
  inc(BIndex);
  end;
 if BSrc=nil then
  begin
  BFullName:='';
  BIndex:=0;
  while BIndex<FIncSearchPath.Count do
   begin
   BFullName:=FileSearch(AssembleFullName('',BName,BExt),FIncSearchPath.Strings[BIndex],FALSE);
   if BFullName<>'' then break;
   Inc(BIndex);
   end;
  if BIndex<FIncSearchPath.Count then BSrc:=AppendInc(BFullName);
  end;
 if BSrc=nil then begin AError:='Include or unit file ['+BFilename+'] is not found [R:TBuildBase.ReadInc]'; break; end;

 // if BSrc is just appended, the type of compiler/parser is determined inside AppendUnit, then in SetFileName
 if BSrc.Parser=nil then begin AError:='Internal error: BSrc.FPars=nil [R:TBuildBase.ReadInc]'; break; end;
 if BSrc.Parser.ParsState=psParsStart then begin AError:='Include dependency loop [R:TBuildBase.ReadInc]'; break; end;
 if BSrc.LoadSrc=FALSE then begin AError:='Cannot load source file "'+BSrc.SrcName+'" [R:TBuildBase.ReadInc]'; break; end;

 Result:=BSrc.Parser;
 until TRUE;

 BList.Free;
End;

Procedure TBuildBase.LtoMarkAllUsed;
Var
  BSrcIdx       : Integer;
Begin
 for BSrcIdx:=0 to Length(FSrcList)-1 do FSrcList[BSrcIdx].MarkLinesUsed;
End;

Procedure TBuildBase.LtoProcess;
Begin
 LtoMarkAllUsed;
End;

Function TBuildBase.ExportExec : string;
Begin
 Result:=FCoreList+#13+FDefCodeSeg+' '+FDefDataSeg+#13+MemSegListExport(FMemSegList);
End;

end.


