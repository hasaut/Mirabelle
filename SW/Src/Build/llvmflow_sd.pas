unit LlvmFlow_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmAtom_sd, LlvmLine_sd, ParsHelper_sd;

Type
  TProcFlow = class(TProcAtom)
  private
  protected
    FFlow       : TFlowLineList;

    Function GetInitMask : string;
    Procedure FlowClearProcessed;
    Procedure FlowClearRemoved;
    Function FindLabelIdx ( Const ALabel : string ) : Integer;
    Procedure InsertTextDstToFlow ( Var AIndex : Integer; Const ATail : string );
    Procedure ProcessPointersA ( Const ATail : string );

    Procedure DbgMarkFlow ( Const AReporter : string );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
  end;

implementation

Uses
  ConComL;

Constructor TProcFlow.Create;
Begin
 Inherited;
End;

Destructor TProcFlow.Destroy;
Begin
 DestroyFlowList(FFlow);
 Inherited;
End;

Procedure TProcFlow.Compile;
{Var
  BLineIdx      : Integer;}
Begin
 Inherited;

 FFlowList.Insert(0,'nop ; '+ParsTailGen(FModule.Filename,FStartLine,FStartPos,'')); // Insert NOPs to unify analysis
 FFlowList.Append('nop ; '+ParsTailGen(FModule.Filename,FEndLine,FEndPos,''));
 StringListToFlowList(FFlowList,FFlow);
 //for BLineIdx:=0 to Length(FFlow)-1 do FFlow[BLineIdx].TailMark('[ProcNameL:'+FNameL+']');
 TimeStamp('Flow');
End;

Procedure TProcFlow.FlowClearProcessed;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(FFlow)-1 do FFlow[BIndex].Processed:=FALSE;
End;

Procedure TProcFlow.FlowClearRemoved;
Var
  BIndex        : Integer;
Begin
 for BIndex:=0 to Length(FFlow)-1 do FFlow[BIndex].Removed:=FALSE;
End;

Function TProcFlow.GetInitMask : string;
Var
  BLen  : Integer;
Begin
 BLen:=Length(FFlow);
 SetLength(Result,BLen); FillChar(Result[1],BLen,'.');
End;

Function TProcFlow.FindLabelIdx ( Const ALabel : string ) : Integer;
Var
  BIndex        : Integer;
  BLineA        : TFlowLine;
Begin
 Result:=-1;
 for BIndex:=0 to Length(FFlow)-1 do
  begin
  BLineA:=FFlow[BIndex];
  if BLineA.IsLabel and (BLineA.LabelName=ALabel) then begin Result:=BIndex; break; end;
  end;
End;

Procedure TProcFlow.InsertTextDstToFlow ( Var AIndex : Integer; Const ATail : string );
Var
  BLineI        : TFlowLine;
Begin
 while FTextDst.Count<>0 do
  begin
  BLineI:=TFlowLine.Create;
  BLineI.RdLine(FTextDst.Strings[0]); BLineI.Tail:=ATail;
  InsertFlowLine(FFlow,AIndex,BLineI);
  inc(AIndex);
  FTextDst.Delete(0);
  end;
End;

Procedure TProcFlow.ProcessPointersA ( Const ATail : string );
Var
  BInsertIdx    : Integer;
  BDummyS,
  BArrayName,
  BPointer,
  BPtrType      : string;
Begin
 BInsertIdx:=1;
 if Length(FFlow)=0 then BInsertIdx:=0;
 while FPointers.Count>0 do
  begin
  BDummyS:=FPointers[0]; FPointers.Delete(0);
  BArrayName:=ReadParamStr(BDummyS,'=');
  BPointer:=ReadParamStr(BDummyS);
  BPtrType:=Copy(BPointer,1,1); Delete(BPointer,1,1);
  if BPtrType='m' then InsertFlowLine(FFlow,BInsertIdx,'lam '+BPointer+' '+BArrayName+' ;'+ATail)
  else InsertFlowLine(FFlow,BInsertIdx,'lea '+BPointer+' '+BArrayName+' ;'+ATail);
  inc(BInsertIdx);
  end;
End;

Procedure TProcFlow.DbgMarkFlow ( Const AReporter : string );
Var
  BLineIdx      : Integer;
Begin
 for BLineIdx:=0 to Length(FFlow)-1 do FFlow[BLineIdx].TailMarkAdd('['+AReporter+'Pos:'+IntToStr(BLineIdx)+']');
End;

end.

