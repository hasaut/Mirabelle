unit BackEndRV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, LlvmBase_sd, LlvmBeHelper_sd, Process, FileUtil;

Type
  TSrcLoc = record
    FFileIdx    : Integer;
    FSrcPosL,
    FSrcPosP    : Integer;
  end;

  TModuleRV = class(TModuleBeHelper)
  private
    FTmpFilename        : string;
    FSrcFileList        : TStringList;
    FLastLoc            : TSrcLoc;

    Procedure ParseFile ( Const ALine : string );
    Procedure ParseLoc ( Const ALine : string );

    Function GenTail : string;
    Function FormatAsmLine ( Const ALineData : string ) : string;
    Procedure ProcessGccAsmLine ( AAsmSrc : TStringList; Const ALineData : string );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function AppendProc : TLlvmProc; Override;

    Procedure GenAsmSrc ( AAsmSrc : TStringList ); Override;
  end;


implementation

Uses
  ConComL, ConComS;

Constructor TModuleRV.Create;
Begin
 Inherited;
 FSrcFileList:=TStringList.Create;
End;

Destructor TModuleRV.Destroy;
Begin
 FSrcFileList.Free;
 Inherited;
End;

Function TModuleRV.AppendProc : TLlvmProc;
Begin
 Result:=nil;
End;

Procedure FileNameLinStyle ( Var AName : string );
Var
  BIndex        : Integer;
  BDirName      : string;
Begin
 repeat
 for BIndex:=1 to Length(AName) do
  begin
  if AName[BIndex]='\' then AName[BIndex]:='/';
  end;
 if Length(AName)<2 then break;
 if AName[2]=':' then
  begin
  BDirName:=Copy(AName,1,1);
  Delete(AName,1,2);
  AName:=LowerCase(BDirName)+AName;
  end;
 until TRUE;
End;

Procedure TModuleRV.GenAsmSrc ( AAsmSrc : TStringList );
Var
  BCppCompStr   : string;
  BExecutable   : string;
  BProcess      : TProcess;
  BOutStream    : TStream;
  BOutList      : TStringList;
  BBytesRead    : Int64;
  BReadBuf      : array [0..255] of byte;
  BParam        : string;
  BDstName,
  BAsmName      : string;
  BPath,
  BName,
  BExt          : string;
  BMsgIdx       : Integer;
  BAsmSrc       : TStringList;
  BLineIdx      : Integer;
Begin
 Inherited;
 BProcess:=TProcess.Create(nil);
 BOutStream:=TMemoryStream.Create;
 BOutList:=TStringList.Create;
 BAsmSrc:=TStringList.Create;
 BCppCompStr:='';
 if FPrjParams<>nil then BCppCompStr:=FPrjParams.Values['ExtCompiler'];

 FSrcFileList.Clear; FLastLoc.FFileIdx:=-1;

 BReadBuf[0]:=0;

 repeat
 if BCppCompStr='' then begin AppendError('e',0,0,'External compiler line is not defined'); break; end;
 SplitFilename(Filename,BPath,BName,BExt);
 FTmpFilename:=Filename; // AssembleFullName(FDstPath,BName,'c');
 BDstName:=AssembleFullName(FDstPath,BName,'s');
 BAsmName:=AssembleFullName(FDstPath,BName,'srv');

 if FileExists(BDstName) then
  begin
  if DeleteFile(BDstName)=FALSE then AppendError('e',0,0,'Cannot delete file '+BDstName+' [R:TModuleRV.GenAsmSrc]');
  end;

 {if FileExists(FTmpFilename) then
  begin
  if DeleteFile(FTmpFilename)=FALSE then AppendError('e',0,0,'Cannot delete file '+FTmpFilename+' [R:TModuleRV.GenAsmSrc]');
  end;

 if CopyFile(Filename,FTmpFilename,TRUE)=FALSE then
  begin
  AppendError('e',0,0,'Cannot copy file '+FTmpFilename+' [R:TModuleRV.GenAsmSrc]');
  break;
  end;}

 BExecutable:=ReadParamStr(BCppCompStr);
 BProcess.Executable:=BExecutable;
 BProcess.CurrentDirectory:=FDstPath;
 repeat
 BParam:=ReadParamStr(BCppCompStr);
 if BParam='' then break;
 ReplaceParam(BParam,'%src%',FTmpFilename);
 ReplaceParam(BParam,'%dst%',BDstName);
 BProcess.Parameters.Append(BParam);
 until FALSE;

 BProcess.Options:=[poUsePipes, poStderrToOutPut];
 BProcess.ShowWindow:=swoHIDE;

 try
   BProcess.Execute;
 except
   AppendError('e',0,0,'Cannot execute external compiler [R:TModuleRV.GenAsmSrc]');
   break;
 end;
 while BProcess.Running do
  begin
  BBytesRead:=BProcess.Output.Read(BReadBuf[0],256);
  if BBytesRead<>0 then BOutStream.Write(BReadBuf[0],BBytesRead);
  Sleep(10);
  end;

 repeat
 BBytesRead:=BProcess.Output.Read(BReadBuf[0],256);
 if BBytesRead=0 then break;
 BOutStream.Write(BReadBuf[0],BBytesRead);
 until FALSE;

 BOutStream.Position:=0;
 BOutList.LoadFromStream(BOutStream);
 if BOutList.Count=0 then
  begin
  AppendError('i',0,0,'File '+RelFilename(FPrjPath,Filename)+' was compiled by an external compiler with no messages [R:TModuleRV.GenAsmSrc]');  end
 else
  begin
  AppendError('i',0,0,'File '+RelFilename(FPrjPath,Filename)+' was compiled by an external compiler with following messages [R:TModuleRV.GenAsmSrc]');
  for BMsgIdx:=0 to BOutList.Count-1 do AppendError('G',0,0,BOutList.Strings[BMsgIdx])
  end;

 try
   BAsmSrc.LoadFromFile(BDstName);
 except
   AppendError('i',0,0,'Cannot read file '+BDstName+' [R:TModuleRV.GenAsmSrc]');
   break;
 end;

 CorrectTabs(BAsmSrc,8);

 // write final file
 AAsmSrc.Clear;
 AAsmSrc.Append(';@M '+FName);
 AAsmSrc.Append(';@S '+Filename);

 AAsmSrc.Append('');

 BLineIdx:=0;
 while BLineIdx<BAsmSrc.Count do
  begin
  ProcessGccAsmLine(AAsmSrc,BAsmSrc.Strings[BLineIdx]);
  inc(BLineIdx);
  end;

 AAsmSrc.Append('');

 try
   AAsmSrc.SaveToFile(BAsmName);
 except
   AppendError('e',0,0,'Cannot save file '+BAsmName+' [R:TModuleRV.GenAsmSrc]');
 end;

 until TRUE;

 BAsmSrc.Free;
 BOutList.Free;
 BOutStream.Free;
 BProcess.Free;
End;

Function TModuleRV.GenTail : string;
Begin
 Result:='';
 repeat
 if FLastLoc.FFileIdx<0 then break;
 if FLastLoc.FFileIdx>=FSrcFileList.Count then break;
 Result:='[SrcFile:'+FSrcFileList.Strings[FLastLoc.FFileIdx]+']'+
         '[SrcPos:'+IntToStr(FLastLoc.FSrcPosL)+','+IntToStr(FLastLoc.FSrcPosP)+']';
 until TRUE;
End;

Function TModuleRV.FormatAsmLine ( Const ALineData : string ) : string;
Var
  BLineData     : string;
Begin
 BLineData:=ALineData;
 DelLastSpace(BLineData);
 AddSpacesVarR(BLineData,50);
 BLineData:=BLineData+' # '+GenTail;
 Result:=BLineData;
End;

Function DelDoubleSlash ( Const AName : string ) : string;
Var
  BPos          : Integer;
Begin
 Result:=AName;
 repeat
 BPos:=Pos('\\',Result);
 if BPos=0 then break;
 Delete(Result,BPos,1);
 until FALSE;
End;

Procedure TModuleRV.ParseFile ( Const ALine : string );
Var
  BLine         : string;
  BParamA,
  BParamB       : string;
  BFileIdx      : Integer;
  BFilename     : string;
Begin
 BLine:=ALine;
 repeat
 BParamA:=ReadParamStr(BLine);
 BParamB:=ReadParamStr(BLine);
 if BParamA='' then break;
 if (BParamB='') and (FSrcFileList.Count=0) then
  begin
  FSrcFileList.Append(DelDoubleSlash(BParamA));
  break;
  end;
 if TryStrToInt(BParamA,BFileIdx)=FALSE then break;
 while FSrcFileList.Count<=BFileIdx do FSrcFileList.Append('');
 if Length(BParamB)<2 then break;
 if (BParamB[1]<>'"') or (BParamB[Length(BParamB)]<>'"') then break;
 Delete(BParamB,Length(BParamB),1); Delete(BParamB,1,1);
 BFilename:=DelDoubleSlash(BParamB);
 if LowerCase(BFilename)=FTmpFilename then BFilename:=Filename;
 FSrcFileList.Strings[BFileIdx]:=BFilename;
 until TRUE;
End;

Procedure TModuleRV.ParseLoc ( Const ALine : string );
Var
  BLine         : string;
  BParamA,
  BParamB,
  BParamC       : string;
  BLoc          : TSrcLoc;
Begin
 BLine:=ALine;
 repeat
 BParamA:=ReadParamStr(BLine);
 BParamB:=ReadParamStr(BLine);
 BParamC:=ReadParamStr(BLine);
 if BParamC='' then break;
 if TryStrToInt(BParamA,BLoc.FFileIdx)=FALSE then break;
 if TryStrToInt(BParamB,BLoc.FSrcPosL)=FALSE then break;
 if TryStrToInt(BParamC,BLoc.FSrcPosP)=FALSE then break;
 FLastLoc:=BLoc;
 until TRUE;
End;


Procedure TModuleRV.ProcessGccAsmLine ( AAsmSrc : TStringList; Const ALineData : string );
Var
  BLineData     : string;
  BCmd          : string;
Begin
 BLineData:=ALineData;
 DelFirstSpace(BLineData);
 repeat
 if BLineData='' then begin AAsmSrc.Append(''); break; end;
 BCmd:=ReadParamStr(BLineData);
 if BCmd='' then break;
 //if (BCmd[1]='.') and (BCmd[Length(BCmd)]=':') then break;
 if BCmd[Length(BCmd)]=':' then
  begin
  if Pos('.LVL',BCmd)=1 then break;
  if Pos('.LASF',BCmd)=1 then break;
  if Pos('.LLST',BCmd)=1 then break;
  if Pos('.LBE',BCmd)=1 then break;
  if Pos('.LBB',BCmd)=1 then break;
  if Pos('.LFB',BCmd)=1 then break;
  if Pos('.LFE',BCmd)=1 then break;
  end;
 if BCmd='.file' then
  begin
  ParseFile(BLineData);
  break;
  end;
 if BCmd='.loc' then
  begin
  ParseLoc(BLineData);
  break;
  end;
 //if BCmd='.byte' then break;
 //if BCmd='.2byte' then break;
 //if BCmd='.4byte' then break;
 if Pos('.cfi_',BCmd)=1 then begin FLastLoc.FFileIdx:=-1; break; end;
 //if BCmd='.section' then begin FLastLoc.FFileIdx:=-1; AAsmSrc.Append(FormatAsmLine(ALineData)); break; end;
 //if BCmd='.string' then break;
 AAsmSrc.Append(FormatAsmLine(ALineData));
 until TRUE;
End;

end.

