unit BuildLto_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, BuildBase_sd, BuildDasm_sd;

Type
  TBuildLto = class(TBuildDasm)
  private
    FEntryPointList     : TAsmFlowList;
    FEpIdxThis          : Integer;

    Function IsBraCall ( ALine : TAsmFlowLine ) : boolean;
    Procedure ExecLines;
    Procedure ExecLinesA ( ALine : TAsmFlowLine );
    Procedure CheckAppendEntryPoint ( ALine : TAsmFlowLine );
    Procedure CollectEntryPoints ( ASrc : TAsmBase );
    Procedure CommentLinesA ( ASrc : TAsmBase );
    Procedure CommentLines;

  protected
    Function FindExternLabel ( Const AName : string ) : TAsmFlowLine;
    Procedure LtoProcess; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

implementation

Constructor TBuildLto.Create;
Begin
 Inherited;
End;

Destructor TBuildLto.Destroy;
Begin
 FEntryPointList:=nil;
 Inherited;
End;

Procedure TBuildLto.CheckAppendEntryPoint ( ALine : TAsmFlowLine );
Var
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FEntryPointList) do
  begin
  if FEntryPointList[BIndex]=ALine then break;
  inc(BIndex);
  end;
 if BIndex=Length(FEntryPointList) then
  begin
  BIndex:=Length(FEntryPointList); SetLength(FEntryPointList,BIndex+1); FEntryPointList[BIndex]:=ALine;
  end;
End;

Procedure TBuildLto.CollectEntryPoints ( ASrc : TAsmBase );
Var
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BRefIdx       : Integer;
  BRef          : TAsmRef;
  BLabel        : TAsmFlowLine;
Begin
 for BLineIdx:=0 to Length(ASrc.FlowList)-1 do
  begin
  BLine:=ASrc.FlowList[BLineIdx];
  repeat
  if BLine.CmdIs<>acData then break;
  for BRefIdx:=0 to Length(BLine.RefList)-1 do
   begin
   BRef:=BLine.RefList[BRefIdx];
   BLabel:=ASrc.FindLabel(nil,BRef.Name);
   if BLabel=nil then BLabel:=FindExternLabel(BRef.Name);
   if BLabel=nil then BRef.AppendError('e','LTO cannot find reference %p [R:TBuildLto.CollectEntryPoints]')
   else CheckAppendEntryPoint(BLabel);
   end;
  until TRUE;
  end;
End;

Procedure TBuildLto.LtoProcess;
Begin
 ExecLines;
 CommentLines;
End;

Procedure TBuildLto.ExecLines;
Var
  BSrcIdx       : Integer;
  BSrc          : TAsmBase;
  BLineIdx      : Integer;
  BEp           : TAsmFlowLine;
  BEpIdx        : Integer;
Begin
 FEntryPointList:=nil;
 FEpIdxThis:=0;
 repeat
 if Length(FSrcList)=0 then begin AppendError('e','',0,0,'Source file list is empty [R:TBuildLto.ExecLines]'); break; end;
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BLineIdx:=0;
  repeat
  BEp:=BSrc.NextEp(BLineIdx);
  if BEp=nil then break;
  CheckAppendEntryPoint(BEp);
  until FALSE;
  inc(BSrcIdx);
  end;
 for BSrcIdx:=0 to Length(FSrcList)-1 do CollectEntryPoints(FSrcList[BSrcIdx]);
 BEpIdx:=0;
 while BEpIdx<Length(FEntryPointList) do
  begin
  ExecLinesA(FEntryPointList[BEpIdx]);
  inc(BEpIdx);
  end;
 until TRUE;
End;

Function TBuildLto.FindExternLabel ( Const AName : string ) : TAsmFlowLine;
Var
  BRef          : TAsmRef;
Begin
 Result:=nil;
 repeat
 BRef:=RefListSearch(FLinkRefList,AName);
 if BRef=nil then break;
 Result:=BRef.Param.Line.AsmBase.FindLabel(nil,BRef.Name);
 until TRUE;
End;

Procedure TBuildLto.ExecLinesA ( ALine : TAsmFlowLine );
Var
  BSrc          : TAsmBase;
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BLabel        : TAsmFlowLine;
  BRefIdx       : Integer;
  BRef          : TAsmRef;
  BEpIdx        : Integer;
  BEpLabel      : TAsmFlowLine;
Begin
 BSrc:=ALine.AsmBase;
 BLineIdx:=ALine.FlowIdx;
 while BLineIdx<Length(BSrc.FlowList) do
  begin
  BLine:=BSrc.FlowList[BLineIdx];
  if BLine.Used then break;
  BLine.Used:=TRUE;
  if BLine.CmdIs=acLabel then
   begin
   BRef:=FindPublicRef(BLine.LabelName);
   if BRef<>nil then BRef.Used:=TRUE;
   end;
  if BLine.IsIpLoad then break;

  if (BLine.IsCall=FALSE) and (BLine.IsJmp and (BLine.DstLabel<>'')) then // Check if it is a call
   begin
   BLine.IsCall:=IsBraCall(BLine);
   end;

  if BLine.IsCall then
   begin
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then BLabel:=FindExternLabel(BLine.DstLabel);
   if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BLine.DstLabel+'[R:TBuildLto.ExecLinesA]'); break; end;
   ExecLinesA(BLabel);
   inc(BLineIdx);
   end
  else if BLine.IsJmp and (BLine.DstLabel<>'') then // In case of "jmp Register" DstLabel can be empty
   begin
   if BLine.DstLabel='.' then break;
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then
    begin
    BLabel:=FindExternLabel(BLine.DstLabel);
    if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BLine.DstLabel+'[R:TBuildLto.ExecLinesA]'); break; end;
    ExecLinesA(BLabel);
    if BLabel.IsIpSave=FALSE then break
    else inc(BLineIdx);
    end
   else
    begin
    if BLabel.IsIpSave then begin ExecLinesA(BLabel); inc(BLineIdx); end
    else BLineIdx:=BLabel.FlowIdx;
    end;
   end
  else if BLine.IsJxx then
   begin
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then BLabel:=FindExternLabel(BLine.DstLabel);
   if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BLine.DstLabel+'[R:TBuildLto.ExecLinesA]'); break; end;
   ExecLinesA(BLabel);
   inc(BLineIdx);
   end
  //  TCmdIs = (acUnparsed, acSysComment, acEmpty, acInclude, acSegmC, acSegmD, acOrg, acAlign, acData, acLabel, acPublic, acExtern, acEqu, acCmd);
  // else if BLine.CmdIs in
  else if BLine.CmdIs in [acLabel, acCmd, acEmpty] then // Do not execute over data
   begin
   for BRefIdx:=0 to Length(BLine.RefList)-1 do
    begin
    BRef:=BLine.RefList[BRefIdx];
    BLabel:=BSrc.FindLabel(BLine,BRef.Name);
    if BLabel=nil then BLabel:=FindExternLabel(BRef.Name);
    if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BRef.Name+'[R:TBuildLto.ExecLinesA]'); break; end;
    BEpIdx:=0;
    while BEpIdx<Length(FEntryPointList) do
     begin
     BEpLabel:=FEntryPointList[BEpIdx];
     if BEpLabel=BLabel then break;
     inc(BEpIdx);
     end;
    if BEpIdx=Length(FEntryPointList) then CheckAppendEntryPoint(BLabel);
    end;
   inc(BLineIdx);
   end
  else if BLine.CmdIs=acData then
   begin
   inc(BLineIdx);
   end;
  end;
End;

Function TBuildLto.IsBraCall ( ALine : TAsmFlowLine ) : boolean;
Var
  BSrc          : TAsmBase;
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
Begin
 Result:=FALSE;
 repeat
 BSrc:=ALine.AsmBase;
 BLine:=BSrc.FindLabel(nil,ALine.DstLabel);
 if BLine=nil then BLine:=FindExternLabel(ALine.DstLabel);
 if BLine=nil then break;
 BSrc:=BLine.AsmBase;
 BLineIdx:=BLine.FlowIdx;
 while BLineIdx<Length(BSrc.FlowList) do
  begin
  BLine:=BSrc.FlowList[BLineIdx];
  if BLine.IsIpSave then begin Result:=TRUE; break; end;
  if BLine.IsIpLoad then break;
  if BLine.IsCall then break;
  if BLine.IsJmp then break;
  if BLine.IsJxx then break;
  if BLine.CmdIs in [acLabel, acCmd, acEmpty] then
  else break;
  inc(BLineIdx);
  end;
 until TRUE;
End;

Procedure TBuildLto.CommentLines;
Var
  BSrcIdx       : Integer;
Begin
 for BSrcIdx:=0 to Length(FSrcList)-1 do CommentLinesA(FSrcList[BSrcIdx]);
End;

Procedure TBuildLto.CommentLinesA ( ASrc : TAsmBase );
Var
  BLineIdx      : Integer;
  BLinePrev,
  BLineThis     : TAsmFlowLine;
Begin
 BLineThis:=nil; BLinePrev:=nil;
 for BLineIdx:=0 to Length(ASrc.FlowList)-1 do
  begin
  //(acUnparsed, acSysComment, acEmpty, acInclude, acSegmC, acSegmD, acOrg, acAlign, acData, acLabel, acPublic, acExtern, acEqu, acCmd, acOther);
  if (BLineThis<>nil) and (BLineThis.CmdIs in [acOrg, acAlign, acData, acLabel, acCmd]) then BLinePrev:=BLineThis;
  BLineThis:=ASrc.FlowList[BLineIdx];
  if BLineThis.CmdIs=acLabel then
   begin
   if BLineThis.Used=FALSE then BLineThis.AppendError('l','Label '+BLineThis.LabelName+' is not referenced [R:TBuildLto.CommentLinesA]');
   end
  else if BLineThis.CmdIs=acData then
   begin
   if BLineThis.Used=FALSE then BLineThis.Used:=TRUE;
   end
  else if BLineThis.CmdIs=acCmd then
   begin
   repeat
   if BLinePrev=nil then break;
   if BLineThis.Used then break;
   if BLinePrev.CmdIs<>acCmd then break;
   if BLinePrev.Used=FALSE then break;
   BLineThis.AppendError('l','Command line is hidden by the previous line[s] [R:TBuildLto.CommentLinesA]');
   until TRUE;
   end;
  end;
End;

end.

