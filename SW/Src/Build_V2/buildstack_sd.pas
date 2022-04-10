unit BuildStack_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, BuildBase_sd, BuildLto_sd;

Type
  TLineUse      = record
    FUsed       : boolean;
    FDepth      : Integer;
  end;
  TLineUseList  = array of TLineUse;

  TBuildStack = class(TBuildLto)
  private
    Function NewCallStack ( Const AStackOld : TAsmFlowList; Out AStackNew : TAsmFlowList; ALabel : TAsmFlowLine ) : Integer;
    Function StackProcessP ( ALine : TAsmFlowLine; Const AUseList : TLineUseList; Const ACallStack : TAsmFlowList; ARecursAllow : boolean; ARecursLevel : Integer; Var ADepthThis, ADepthMax : Integer ) : boolean;
    Function StackProcessA ( ALine : TAsmFlowLine; ARecursAllow : boolean; ARecursLevel : Integer ) : Integer;

  protected
    Procedure StackProcess; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

implementation

Constructor TBuildStack.Create;
Begin
 Inherited;
End;

Destructor TBuildStack.Destroy;
Begin
 Inherited;
End;

Function TBuildStack.NewCallStack ( Const AStackOld : TAsmFlowList; Out AStackNew : TAsmFlowList; ALabel : TAsmFlowLine ) : Integer;
Var
  BLineIdx      : Integer;
Begin
 Result:=0;
 BLineIdx:=0;
 while BLineIdx<Length(AStackOld) do
  begin
  if AStackOld[BLineIdx]=ALabel then break;
  inc(BLineIdx);
  end;
 if BLineIdx<Length(AStackOld) then Result:=1;
 AStackNew:=Copy(AStackOld);
 BLineIdx:=Length(AStackNew); SetLength(AStackNew,BLineIdx+1); AStackNew[BLineIdx]:=ALabel;
End;

// Main search recursive function
// Returns TRUE if branch exit by RET instruction
// AUseList is a list of all lines in a module. In case of CALL a new clean copy is created. In case of JXX, old data is copied.
// AUseList is used to exit a function if different branches hit already used flow (in the same call level). It is not used to detect recursions.
// ACallStack grows up if CALL is found and down if RET is found. Each recursive copy creates an own instance of CallStack
// ACallStack is used to detect recursions.
Function TBuildStack.StackProcessP ( ALine : TAsmFlowLine; Const AUseList : TLineUseList; Const ACallStack : TAsmFlowList; ARecursAllow : boolean; ARecursLevel : Integer; Var ADepthThis, ADepthMax : Integer ) : boolean;
Var
  BCallStack    : TAsmFlowList;
  BLineIdx      : Integer;
  BSrc          : TAsmBase;
  BLine         : TAsmFlowLine;
  BLabel        : TAsmFlowLine;
  BUseList      : TLineUseList;
  BRecursDec    : Integer;
  BDepthThisA,
  BDepthThisB   : Integer;
  BRetHitA,
  BRetHitB      : boolean;
Begin
 Result:=FALSE;

 repeat
 if ARecursLevel<0 then
  begin
  if ARecursAllow=FALSE then ALine.AppendError('e','Recursive call (which was not allowed by settings) [R:TBuildStack.StackProcessP]');
  break;
  end;

 BSrc:=ALine.AsmBase;
 if AUseList<>nil then BUseList:=Copy(AUseList)
 else
  begin
  SetLength(BUseList,Length(BSrc.FlowList));
  for BLineIdx:=0 to Length(BUseList)-1 do begin BUseList[BLineIdx].FUsed:=FALSE; BUseList[BLineIdx].FDepth:=0; end;
  end;

 BLineIdx:=ALine.FlowIdx;
 while BLineIdx<Length(BSrc.FlowList) do
  begin
  BLine:=BSrc.FlowList[BLineIdx];
  if BUseList[BLineIdx].FUsed then
   begin
   {if BUseList[BLineIdx].FDepth<>ADepthThis then
    begin
    BLine.AppendError('w','Stack leakage by '+IntToStr(BUseList[BLineIdx].FDepth-ADepthThis)+' bytes [R:TBuildStack.StackProcessP]');
    end;}
   break;
   end;
  BUseList[BLineIdx].FUsed:=TRUE; BUseList[BLineIdx].FDepth:=ADepthThis;
  ADepthThis:=ADepthThis+BLine.StackDelta;
  if ADepthMax<ADepthThis then ADepthMax:=ADepthThis;
  if BLine.IsIpLoad then
   begin
   Result:=TRUE;
   break;
   end
  else if BLine.IsCall then
   begin
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then BLabel:=FindExternLabel(BLine.DstLabel);
   if BLabel=nil then begin BLine.AppendError('e','Stack analyser cannot find destination label '+BLine.DstLabel+'[R:TBuildStack.StackProcessP]'); break; end;
   BRecursDec:=NewCallStack(ACallStack,BCallStack,BLabel);
   StackProcessP(BLabel,nil,BCallStack,ARecursAllow,ARecursLevel-BRecursDec,ADepthThis,ADepthMax);
   BCallStack:=nil;
   inc(BLineIdx);
   end
  else if BLine.IsJmp and (BLine.DstLabel<>'') then // In case of "jmp Register" DstLabel can be empty
   begin
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then
    begin
    BLabel:=FindExternLabel(BLine.DstLabel);
    if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BLine.DstLabel+'[R:TBuildStack.StackProcessP]'); break; end;
    BRecursDec:=NewCallStack(ACallStack,BCallStack,BLabel);
    Result:=StackProcessP(BLabel,nil,BCallStack,ARecursAllow,ARecursLevel-BRecursDec,ADepthThis,ADepthMax);
    BCallStack:=nil;
    break;
    end;
   BLineIdx:=BLabel.FlowIdx;
   end
  else if BLine.IsJxx then
   begin
   // Analyse branches separately
   // One branch normally should hit RET, another one will exit by FUsed
   // The 1st one should return TRUE and update ADepthThis
   // The 2nd one should return FALSE and ADepthThis must not change (we can sense stack leakage)
   // Due to JMP play we do not know which branch hits what.
   // Both can return TRUE if there are more than one RET (GCC compiler often produces code like this)
   // Both can return FALSE (if branches end by an endless loop)
   BDepthThisA:=ADepthThis; BDepthThisB:=ADepthThis;
   BLabel:=BSrc.FindLabel(BLine,BLine.DstLabel);
   if BLabel=nil then
    begin
    BLabel:=FindExternLabel(BLine.DstLabel);
    if BLabel=nil then begin BLine.AppendError('e','Linker cannot find destination label '+BLine.DstLabel+'[R:TBuildStack.StackProcessP]'); break; end;
    BRecursDec:=NewCallStack(ACallStack,BCallStack,BLabel);
    BRetHitA:=StackProcessP(BLabel,nil,BCallStack,ARecursAllow,ARecursLevel-BRecursDec,BDepthThisA,ADepthMax);
    BCallStack:=nil;
    end
   else
    begin
    BRetHitA:=StackProcessP(BLabel,BUseList,ACallStack,ARecursAllow,ARecursLevel,BDepthThisA,ADepthMax);
    end;
   inc(BLineIdx);
   if BLineIdx>=Length(BSrc.FlowList) then begin BLine.AppendError('e','This line cannot be the last one [R:TBuildStack.StackProcessP]'); break; end;
   BLine:=BSrc.FlowList[BLineIdx];
   BRetHitB:=StackProcessP(BLine,BUseList,ACallStack,ARecursAllow,ARecursLevel,BDepthThisB,ADepthMax);
   if BRetHitA and BRetHitB then
    begin
    if BDepthThisA<>BDepthThisB then
     begin
     BLabel.AppendError('w','Suspected stack leakage [R:TBuildStack.StackProcessP]');
     BLine.AppendError('w','Suspected stack leakage (see previous message) [R:TBuildStack.StackProcessP]');
     end;
    ADepthThis:=BDepthThisA;
    Result:=TRUE;
    end
   else if BRetHitA and (not BRetHitB) then
    begin
    if BDepthThisB<>ADepthThis then BLine.AppendError('w','Suspected stack leakage [R:TBuildStack.StackProcessP]');
    ADepthThis:=BDepthThisA;
    Result:=TRUE;
    end
   else if (not BRetHitA) and BRetHitB then
    begin
    if BDepthThisA<>ADepthThis then BLabel.AppendError('w','Suspected stack leakage [R:TBuildStack.StackProcessP]');
    ADepthThis:=BDepthThisB;
    Result:=TRUE;
    end
   else
    begin
    if BDepthThisB<>ADepthThis then BLine.AppendError('w','Suspected stack leakage [R:TBuildStack.StackProcessP]');
    if BDepthThisA<>ADepthThis then BLabel.AppendError('w','Suspected stack leakage [R:TBuildStack.StackProcessP]');
    Result:=FALSE;
    end;
   // Exit here
   break;
   end
  else
   begin
   inc(BLineIdx);
   end;
  end;

 until TRUE;

 BUseList:=nil;
End;

Function TBuildStack.StackProcessA ( ALine : TAsmFlowLine; ARecursAllow : boolean; ARecursLevel : Integer ) : Integer;
Var
  BCallStack    : TAsmFlowList;
  BDepthThis,
  BDepthMax     : Integer;
Begin
 SetLength(BCallStack,1);
 BCallStack[0]:=ALine;
 BDepthThis:=0; BDepthMax:=0;
 StackProcessP(ALine,nil,BCallStack,ARecursAllow,ARecursLevel,BDepthThis,BDepthMax);
 if BDepthThis<>0 then ALine.AppendError('w','Possible stack leakage [R:TBuildStack.StackProcess]');
 Result:=BDepthMax;
 BCallStack:=nil;
End;

Procedure TBuildStack.StackProcess;
Var
  BSrc          : TAsmBase;
  BSrcIdx       : Integer;
  BLine         : TAsmFlowLine;
  BLineIdx      : Integer;
  BStackSize    : Integer;
  BRecursCnt    : Integer;
  BLabel        : TAsmFlowLine;
Begin
 repeat
 if Length(FSrcList)=0 then begin AppendError('e','',0,0,'Source file list is empty [R:TBuildStack.StackProcess]'); break; end;
 BSrcIdx:=0;
 while BSrcIdx<Length(FSrcList) do
  begin
  BSrc:=FSrcList[BSrcIdx];
  BLineIdx:=0;
  while BLineIdx<Length(BSrc.FlowList) do
   begin
   BLine:=BSrc.FlowList[BLineIdx];
   if BLine.CmdIs=acStack then
    begin
    if Length(BLine.Params)<>3 then begin BLine.AppendError('e','There must be 2 parameters: EntryLabel and RecursionCnt [R:TBuildStack.StackProcess]'); break; end;
    if TryStrToInt(BLine.Params[2].Name,BRecursCnt)=FALSE then begin BLine.AppendError('e',BLine.Params[2],'Cannot convert parameter to integer [R:TBuildStack.StackProcess]'); break; end;
    BLabel:=BSrc.FindLabel(nil,BLine.Params[1].Name);
    if BLabel=nil then BLabel:=FindExternLabel(BLine.Params[1].Name);
    if BLabel=nil then BLine.AppendError('e','Linker cannot find destination label '+BLine.Params[1].Name+'[R:TBuildStack.StackProcess]')
    else
     begin
     BStackSize:=StackProcessA(BLabel,BRecursCnt<>0,BRecursCnt);
     if (BStackSize mod 4)<>0 then BLine.AppendError('w','Invalid stack alignment [R:TBuildStack.StackProcess]');
     BLine.SetDataSize(BStackSize);
     BLine.Used:=TRUE;
     end;
    end;
   inc(BLineIdx);
   end;
  inc(BSrcIdx);
  end;
 until TRUE;
End;

end.

