unit DasmBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, AsmTypes_sd, SysUtils;

Type
  TCallOrJmp = (cjUnknown, cjJmp, cjCall);

  TExecLineBase = class (TObject)
  protected
    FAddr       : Cardinal;
    FCodeBin    : string;

    FDstAddr    : Cardinal;

    FLastError  : string;
    FAsmLineS   : string;
    FLabelName,
    FDstLabel   : string;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure ResetErrors;
    Procedure SetLabel ( AAddLabel : char );
    Procedure SetDstLabel ( Const ALabelName : string );
    Function CmdDec ( AAddr : Cardinal; Const ACodeBin : string ) : boolean; Virtual; Abstract;
    Procedure CheckFixDstLabel; Virtual; Abstract;

    Function IsJmp : boolean; Virtual; Abstract;
    Function IsJxx : boolean; Virtual; Abstract;
    Function IsRet : boolean; Virtual; Abstract;
    Function IsCall : boolean; Virtual; Abstract;
    Function IsDecStop : boolean; Virtual; Abstract;
    Function CallOrJmp : TCallOrJmp; Virtual; Abstract; // Used to determine if this chain is CALL or JMP (i.e. previous instruction is CALL or JMP)

    property Addr : Cardinal read FAddr;
    property CodeBin : string read FCodeBin;
    property LastError : string read FLastError;
    property AsmLineS : string read FAsmLineS;
    property DstAddr : Cardinal read FDstAddr;

    property LabelName : string read FLabelName;
    property DstLabel : string read FDstLabel;
  end;

  TSubdecProc = Procedure ( ALine : TExecLineBase );

  TExecList = array of TExecLineBase;

  TFixChain = class (TObject)
  private
    FParent     : TObject;
    FFixBinBase : Cardinal;
    FFixBinData : string;
    FExecList   : TExecList; // Used for disassembler

    Procedure ClearExecList;
    Procedure InitExecList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TObject; ABinBase : Cardinal; Const ABinData : string );
    Function IsInside ( AAddr : Cardinal ) : boolean;
    Function IsOverlap ( AChain : TFixChain ) : boolean;

    property Parent : TObject read FParent;
    property FixBinBase : Cardinal read FFixBinBase;
    property FixBinData : string read FFixBinData;
    property ExecList : TExecList read FExecList;
  end;

  TFixChainList = array of TFixChain;

Procedure FixChainListAppend ( Var AList : TFixChainList; AChain : TFixChain );
Procedure FixChainListClear ( Var AList : TFixChainList );
Procedure FixChainListOrder ( Var AList : TFixChainList );

implementation

Procedure FixChainListAppend ( Var AList : TFixChainList; AChain : TFixChain );
Var
  BChainIdx : Integer;
Begin
 BChainIdx:=Length(AList);
 SetLength(AList,BChainIdx+1);
 AList[BChainIdx]:=AChain;
End;

Procedure FixChainListClear ( Var AList : TFixChainList );
Var
  BChainIdx : Integer;
Begin
 BChainIdx:=0;
 while BChainIdx<Length(AList) do
  begin
  AList[BChainIdx].Free;
  inc(BChainIdx);
  end;
 AList:=nil;
End;

Procedure FixChainListOrder ( Var AList : TFixChainList );
Var
  BIndexA,
  BIndexB   : Integer;
  BChainA,
  BChainB   : TFixChain;
Begin
 repeat
 BIndexB:=Length(AList);
 if BIndexB<2 then break;
 Dec(BIndexB);
 while BIndexB>0 do
  begin
  BIndexA:=0;
  while BIndexA<BIndexB do
   begin
   BChainA:=AList[BIndexA+0];
   BChainB:=AList[BIndexA+1];
   if BChainA.FFixBinBase>BChainB.FFixBinBase then
    begin
    AList[BIndexA+0]:=BChainB;
    AList[BIndexA+1]:=BChainA;
    end;
   inc(BIndexA);
   end;
  Dec(BIndexB);
  end;
 until TRUE;
End;

{ *** ExecLineBase *** }

Constructor TExecLineBase.Create;
Begin
 Inherited;
End;

Destructor TExecLineBase.Destroy;
Begin
 Inherited;
End;

Procedure TExecLineBase.ResetErrors;
Begin
 FLastError:='';
End;

Procedure TExecLineBase.SetLabel ( AAddLabel : char );
Begin
 repeat
 if FLabelName<>'' then break;
 if AAddLabel=#0 then break;
 case AAddLabel of
   'j': FLabelName:='m_'+IntToHex(FAddr,8);
   'c': FLabelName:='Proc_'+IntToHex(FAddr,8);
   'e': FLabelName:='Entry_'+IntToHex(FAddr,8);
 end;
 until TRUE;
End;

Procedure TExecLineBase.SetDstLabel ( Const ALabelName : string );
Begin
 FDstLabel:=ALabelName;
End;

{ *** TFixChain *** }

Constructor TFixChain.Create;
Begin
 Inherited;
End;

Destructor TFixChain.Destroy;
Begin
 ClearExecList;
 Inherited;
End;

Procedure TFixChain.ClearExecList;
Var
  BIndex    : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(FExecList) do
  begin
  if FExecList[BIndex]<>nil then FExecList[BIndex].Free;
  inc(BIndex);
  end;
 FExecList:=nil;
End;

Procedure TFixChain.InitExecList;
Var
  BIndex    : Integer;
Begin
 SetLength(FExecList,Length(FFixBinData));
 BIndex:=0;
 while BIndex<Length(FExecList) do
  begin
  FExecList[BIndex]:=nil;
  inc(BIndex);
  end;
End;

Procedure TFixChain.Init ( AParent : TObject; ABinBase : Cardinal; Const ABinData : string );
Begin
 FParent:=AParent;
 FFixBinBase:=ABinBase;
 FFixBinData:=ABinData;
 InitExecList;
End;

Function TFixChain.IsInside ( AAddr : Cardinal ) : boolean;
Begin
 Result:=(AAddr>=FFixBinBase) and (AAddr<(FFixBinBase+Length(FFixBinData)));
End;

Function TFixChain.IsOverlap ( AChain : TFixChain ) : boolean;
Begin
 Result:=TRUE;
 repeat
 if IsInside(AChain.FFixBinBase) then break;
 if IsInside(AChain.FFixBinBase+Length(AChain.FFixBinData)) then break;
 if AChain.IsInside(FFixBinBase) then break;
 if AChain.IsInside(FFixBinBase+Length(FFixBinData)) then break;
 Result:=FALSE;
 until TRUE;
End;

end.

