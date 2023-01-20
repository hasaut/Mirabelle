unit DasmBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, AsmTypes_sd, SysUtils;

Type
  TCallOrJmp = (cjUnknown, cjJmp, cjCall);

  TExecLineBase = class (TObject)
  protected
    FVirtAddr,
    FBaseAddr   : Cardinal;
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
    Function CmdDec ( AVirtAddr : Cardinal; Const ACodeBin : string ) : boolean; Virtual; Abstract;
    Procedure CheckFixDstLabel; Virtual; Abstract;

    Function IsJmp : boolean; Virtual; Abstract;
    Function IsJxx : boolean; Virtual; Abstract;
    Function IsRet : boolean; Virtual; Abstract;
    Function IsCall : boolean; Virtual; Abstract;
    Function IsDecStop : boolean; Virtual; Abstract;
    Function CallOrJmp : TCallOrJmp; Virtual; Abstract; // Used to determine if this chain is CALL or JMP (i.e. previous instruction is CALL or JMP)

    property VirtAddr : Cardinal read FVirtAddr;
    property BaseAddr : Cardinal read FBaseAddr write FBaseAddr;
    property CodeBin : string read FCodeBin;
    property LastError : string read FLastError;
    property AsmLineS : string read FAsmLineS;
    property DstAddr : Cardinal read FDstAddr;

    property LabelName : string read FLabelName;
    property DstLabel : string read FDstLabel;
  end;

  TSubdecProc = Procedure ( ALine : TExecLineBase );

  TExecList = array of TExecLineBase;

  TCodeChunk = class (TObject)
  private
    FParent     : TObject;
    FConstName  : string;
    FFixBinBase : Cardinal;
    FFixBinData : string;
    FRelBinBase : Cardinal;
    FExecList   : TExecList; // Used for disassembler

    FCanRelocate    : boolean;
    FRelFileHdr,
    FVirtRamBase,
    FVirtRamSize    : Cardinal;

    Procedure ClearExecList;
    Procedure InitExecList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TObject; Const AFilename : string; ABinBase : Cardinal; Const ABinData : string );
    Function IsInside ( AAddr : Cardinal ) : boolean;
    Function IsOverlap ( AChunk : TCodeChunk ) : boolean;
    Function ReadBinS ( AAddr, ASize : Cardinal ) : string;
    Function ReadBinD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;
    Function FindMarker ( Const AMarker : string ) : boolean;
    Function ParseRelHdr ( Out AErrorS : string ) : boolean;

    Procedure RebaseLines;

    property Parent : TObject read FParent;
    property ConstName : string read FConstName;
    property FixBinBase : Cardinal read FFixBinBase;
    property FixBinData : string read FFixBinData;
    property RelBinBase : Cardinal read FRelBinBase write FRelBinBase;
    property ExecList : TExecList read FExecList;
    property CanRelocate : boolean read FCanRelocate;
    property RelFileHdr : Cardinal read FRelFileHdr;
    property VirtRamBase : Cardinal read FVirtRamBase;
    property VirtRamSize : Cardinal read FVirtRamSize;
  end;

  TCodeChunkList = array of TCodeChunk;

Procedure CodeChunkListAppend ( Var AList : TCodeChunkList; AChunk : TCodeChunk );
Procedure CodeChunkListClear ( Var AList : TCodeChunkList );
Procedure CodeChunkListOrder ( Var AList : TCodeChunkList );

implementation

Uses
  ConComL;

Procedure CodeChunkListAppend ( Var AList : TCodeChunkList; AChunk : TCodeChunk );
Var
  BChunkIdx : Integer;
Begin
 BChunkIdx:=Length(AList);
 SetLength(AList,BChunkIdx+1);
 AList[BChunkIdx]:=AChunk;
End;

Procedure CodeChunkListClear ( Var AList : TCodeChunkList );
Var
  BChunkIdx : Integer;
Begin
 BChunkIdx:=0;
 while BChunkIdx<Length(AList) do
  begin
  AList[BChunkIdx].Free;
  inc(BChunkIdx);
  end;
 AList:=nil;
End;

Procedure CodeChunkListOrder ( Var AList : TCodeChunkList );
Var
  BIndexA,
  BIndexB   : Integer;
  BChunkA,
  BChunkB   : TCodeChunk;
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
   BChunkA:=AList[BIndexA+0];
   BChunkB:=AList[BIndexA+1];
   if BChunkA.FFixBinBase>BChunkB.FFixBinBase then
    begin
    AList[BIndexA+0]:=BChunkB;
    AList[BIndexA+1]:=BChunkA;
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
   'j': FLabelName:='m_'+IntToHex(FVirtAddr,8);
   'c': FLabelName:='Proc_'+IntToHex(FVirtAddr,8);
   'e': FLabelName:='Entry_'+IntToHex(FVirtAddr,8);
 end;
 until TRUE;
End;

Procedure TExecLineBase.SetDstLabel ( Const ALabelName : string );
Begin
 FDstLabel:=ALabelName;
End;

{ *** TCodeChunk *** }

Constructor TCodeChunk.Create;
Begin
 Inherited;
End;

Destructor TCodeChunk.Destroy;
Begin
 ClearExecList;
 Inherited;
End;

Procedure TCodeChunk.ClearExecList;
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

Procedure TCodeChunk.InitExecList;
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

Procedure TCodeChunk.Init ( AParent : TObject; Const AFilename : string; ABinBase : Cardinal; Const ABinData : string );
Var
  BConstName    : string;
  BPos          : Integer;
Begin
 FParent:=AParent;
 FFixBinBase:=ABinBase;
 FFixBinData:=ABinData;
 InitExecList;
 BConstName:=ExtractFilename(AFilename);
 repeat
 BPos:=Pos('.',BConstName);
 if BPos=0 then break;
 BConstName[BPos]:='_';
 until FALSE;
 FConstName:=BConstName;
End;

Function TCodeChunk.IsInside ( AAddr : Cardinal ) : boolean;
Begin
 Result:=(AAddr>=FFixBinBase) and (AAddr<(FFixBinBase+Length(FFixBinData)));
End;

Function TCodeChunk.IsOverlap ( AChunk : TCodeChunk ) : boolean;
Begin
 Result:=TRUE;
 repeat
 if IsInside(AChunk.FFixBinBase) then break;
 if IsInside(AChunk.FFixBinBase+Length(AChunk.FFixBinData)) then break;
 if AChunk.IsInside(FFixBinBase) then break;
 if AChunk.IsInside(FFixBinBase+Length(FFixBinData)) then break;
 Result:=FALSE;
 until TRUE;
End;

Function TCodeChunk.ReadBinS ( AAddr, ASize : Cardinal ) : string;
Var
  BSize         : Integer;
Begin
 Result:='';
 repeat
 if IsInside(AAddr)=FALSE then break;
 if Length(FFixBinData)<=(AAddr-FFixBinBase) then break;
 BSize:=Length(FFixBinData)-(AAddr-FFixBinBase);
 if ASize<BSize then BSize:=ASize;
 Result:=Copy(FFixBinData,1+AAddr-FFixBinBase,BSize);
 until TRUE;
End;

Function TCodeChunk.ReadBinD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;
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

Function TCodeChunk.FindMarker ( Const AMarker : string ) : boolean;
Var
  BAddr         : Cardinal;
  BRdIdx        : Integer;
  BMarkerLen    : Cardinal;
  BMarker       : string;
Begin
 Result:=FALSE;
 repeat
 if Length(FFixBinData)<4 then break;
 BRdIdx:=0;
 if StrAsDWord(FFixBinData,BRdIdx,BAddr)=FALSE then break;
 if BAddr<FFixBinBase then break;
 BAddr:=BAddr-FFixBinBase;
 if BAddr<4 then break;
 BMarkerLen:=Length(AMarker);
 if BAddr+BMarkerLen>=Length(FFixBinData) then break;
 if FFixBinData[1+BAddr+BMarkerLen]<>#0 then break;
 BMarker:=Copy(FFixBinData,1+BAddr,BMarkerLen);
 if BMarker<>AMarker then break;
 Result:=TRUE;
 until TRUE;
End;

Function TCodeChunk.ParseRelHdr ( Out AErrorS : string ) : boolean;
Var
  BRdIdx        : Integer;
Begin
 Result:=FALSE; AErrorS:='';
 repeat
 BRdIdx:=4;
 if StrAsDWord(FFixBinData,BRdIdx,FRelFileHdr)=FALSE then begin AErrorS:='Cannot load HEX file header info'; break; end;
 if StrAsDWord(FFixBinData,BRdIdx,FVirtRamBase)=FALSE then begin AErrorS:='Cannot load HEX RamBase'; break; end;
 if StrAsDWord(FFixBinData,BRdIdx,FVirtRamSize)=FALSE then begin AErrorS:='Cannot load HEX RamSize'; break; end;
 FCanRelocate:=TRUE;
 Result:=TRUE;
 until TRUE;
End;

Procedure TCodeChunk.RebaseLines;
Var
  BLineIdx  : Integer;
  BExecBase : TExecLineBase;
Begin
 BLineIdx:=0;
 while BLineIdx<Length(FExecList) do
  begin
  BExecBase:=FExecList[BLineIdx];
  if BExecBase<>nil then
   begin
   BExecBase.BaseAddr:=BExecBase.VirtAddr-FFixBinBase+FRelBinBase;
   end;
  inc(BLineIdx);
  end;
End;

end.

