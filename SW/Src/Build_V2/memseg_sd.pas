unit MemSeg_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataFiles, MemSegHelper_sd;

Type
  TIpToLine = array of Cardinal;
  TMemSeg = class(TObject)
  private
    FSegName    : string;
    FSegFlags   : Byte;
    FHwBase,
    FHwSize     : Cardinal;
    FHwWidth    : byte;
    FFillSize   : Cardinal;
    FBin        : string;
    FIpToLine   : TIpToLine;

    FHex        : TStringList;
    FMem        : TStringList;
    FHexName,
    FMemName    : string;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure IpToLineClear;
    Procedure Reset;
    Function MemSegExport : string;
    Procedure MemSegImport ( Var ADataS : string );
    Procedure MemSegImport ( AMemSeg : TMemSeg );
    Procedure CreateHex;
    Procedure CreateMem;

    Function IsInside ( AAddr : Cardinal ) : boolean;
    Function IsInside ( AAddr : Cardinal; ASize : Cardinal ) : boolean;
    Function IsInside ( AAddr : Cardinal; Const AData : string ) : boolean;
    Function WrData ( AAddr : Cardinal; Const AData : string ) : Cardinal;
    Procedure WrDataX ( AAddr : Cardinal; ASize : byte; AData : Cardinal );
    Function RdData ( AAddr : Cardinal; ASize : Cardinal ) : string;
    Function RdDataX ( AAddr : Cardinal; ASize : byte ) : Cardinal;

    property SegName : string read FSegName;
    property SegFlags : Byte read FSegFlags;
    property HwBase : Cardinal read FHwBase;
    property HwSize : Cardinal read FHwSize;
    property HwWidth : byte read FHwWidth;
    property FillSize : Cardinal read FFillSize write FFillSize;
    property Bin : string read FBin;
    property Hex : TStringList read FHex;
    property Mem : TStringList read FMem;
    property HexName : string read FHexName write FHexName;
    property MemName : string read FMemName write FMemName;
    property IpToLine : TIpToLine read FIpToLine write FIpToLine;
  end;

  TMemSegList = array of TMemSeg;

Procedure MemSegListAppend ( Var AList : TMemSegList; AMemSeg : TMemSeg );
Procedure MemSegListClear ( Var AList : TMemSegList );
Procedure MemSegListReset ( Var AList : TMemSegList );
Procedure MemSegListOrder ( Var AList : TMemSegList );
Function MemSegListExport ( Const AList : TMemSegList ) : string;
Procedure MemSegListImport ( Var AList : TMemSegList; Const ADataS : string );
Procedure MemSegListImport ( Var AListDst : TMemSegList; Const AListSrc : TMemSegList );
Function MemSegSearch ( Const AList : TMemSegList; AAddr : Cardinal ) : TMemSeg;
Function MemSegSearch ( Const AList : TMemSegList; Const AName : string ) : TMemSeg;

Function MemSegParamsToStr ( Const ASegName : string; ASegFlags : byte; AHwBase, AHwSize : Cardinal; AHwWidth : byte ) : string;

implementation

Uses
  ConComL;

Procedure MemSegListAppend ( Var AList : TMemSegList; AMemSeg : TMemSeg );
Var
  BIndex    : Integer;
Begin
 BIndex:=Length(AList);
 SetLength(AList,BIndex+1);
 AList[BIndex]:=AMemSeg;
End;

Procedure MemSegListClear ( Var AList : TMemSegList );
Var
  BIndex    : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  AList[BIndex].Free;
  inc(BIndex);
  end;
 AList:=nil;
End;

Procedure MemSegListReset ( Var AList : TMemSegList );
Var
  BIndex    : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  AList[BIndex].Reset;
  inc(BIndex);
  end;
End;

Procedure MemSegListOrder ( Var AList : TMemSegList );
Var
  BSegIdxA,
  BSegIdxB  : Integer;
  BMemSegA,
  BMemSegB  : TMemSeg;
Begin
 repeat
 BSegIdxB:=Length(AList);
 if BSegIdxB<2 then break;
 Dec(BSegIdxB);
 while BSegIdxB>0 do
  begin
  BSegIdxA:=0;
  while BSegIdxA<BSegIdxB do
   begin
   BMemSegA:=AList[BSegIdxA+0];
   BMemSegB:=AList[BSegIdxA+1];
   if BMemSegA.HwBase>BMemSegB.HwBase then
    begin
    AList[BSegIdxA+0]:=BMemSegB;
    AList[BSegIdxA+1]:=BMemSegA;
    end;
   inc(BSegIdxA);
   end;
  Dec(BSegIdxB);
  end;
 until TRUE;
End;

Function MemSegListExport ( Const AList : TMemSegList ) : string;
Var
  BIndex    : Integer;
Begin
 Result:='';
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  Result:=Result+AList[BIndex].MemSegExport+' ';
  inc(BIndex);
  end;
End;

Procedure MemSegListImport ( Var AList : TMemSegList; Const ADataS : string );
Var
  BDataS    : string;
  BMemSeg   : TMemSeg;
Begin
 MemSegListClear(AList);
 BDataS:=ADataS;
 repeat
 DelFirstSpace(BDataS);
 if BDataS='' then break;
 BMemSeg:=TMemSeg.Create;
 MemSegListAppend(AList,BMemSeg);
 BMemSeg.MemSegImport(BDataS);
 until FALSE;
End;

Procedure MemSegListImport ( Var AListDst : TMemSegList; Const AListSrc : TMemSegList );
Var
  BSegIdx   : Integer;
  BMemSeg   : TMemSeg;
Begin
 MemSegListClear(AListDst);
 BSegIdx:=0;
 while BSegIdx<Length(AListSrc) do
  begin
  BMemSeg:=TMemSeg.Create;
  MemSegListAppend(AListDst,BMemSeg);
  BMemSeg.MemSegImport(AListSrc[BSegIdx]);
  inc(BSegIdx);
  end;
End;

Function MemSegSearch ( Const AList : TMemSegList; AAddr : Cardinal ) : TMemSeg;
Var
  BIndex    : Integer;
Begin
 Result:=nil;
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  if AList[BIndex].IsInside(AAddr) then break;
  inc(BIndex);
  end;
 if BIndex<Length(AList) then Result:=AList[BIndex];
End;

Function MemSegSearch ( Const AList : TMemSegList; Const AName : string ) : TMemSeg;
Var
  BIndex    : Integer;
  BNameS    : string;
Begin
 Result:=nil;
 BNameS:=LowerCase(AName);
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  if LowerCase(AList[BIndex].FSegName)=BNameS then break;
  inc(BIndex);
  end;
 if BIndex<Length(AList) then Result:=AList[BIndex];
End;

Function MemSegParamsToStr ( Const ASegName : string; ASegFlags : byte; AHwBase, AHwSize : Cardinal; AHwWidth : byte ) : string;
Begin
 Result:=ASegName+' '+
         IntToHex(ASegFlags,2)+' '+
         IntToHex(AHwBase,8)+' '+
         IntToHex(AHwSize,8)+' '+
         IntToHex(AHwWidth,8)+' '+
         IntToHex(0,8)+' '+
         IntToHex(0,8)+'#';
End;

{ *** TMemSeg *** }

Constructor TMemSeg.Create;
Begin
 Inherited;
 FHex:=TStringList.Create;
 FMem:=TStringList.Create;
End;

Destructor TMemSeg.Destroy;
Begin
 FIpToLine:=nil;
 FMem.Free;
 FHex.Free;
 Inherited;
End;

Procedure TMemSeg.IpToLineClear;
Var
  BIndex    : Cardinal;
Begin
 BIndex:=0; while BIndex<FHwSize do begin FIpToLine[BIndex]:=$FFFFFFFF; inc(BIndex); end;
End;

Procedure TMemSeg.Reset;
Begin
 FBin:='';
 FFillSize:=0;
 SetLength(FBin,FHwSize);
 SetLength(FIpToLine,FHwSize); IpToLineClear;
End;

Function TMemSeg.MemSegExport : string;
Begin
 Result:=FSegName+' '+
         IntToHex(FSegFlags,2)+' '+
         IntToHex(FHwBase,8)+' '+
         IntToHex(FHwSize,8)+' '+
         IntToHex(FHwWidth,2)+' '+
         IntToHex(FFillSize,8)+' '+
         MsBinExport(@FBin)+'#';
End;

Procedure TMemSeg.MemSegImport ( Var ADataS : string );
Var
  BDataS    : string;
  BIndex    : Cardinal;
  BDataC    : char;
  BParam    : string;
  BData     : Cardinal;
  BPBin     : TMsBinPtr;
Begin
 BDataS:=ReadTillC(ADataS,'#');
 BParam:=ReadParamStr(BDataS); FSegName:=BParam;
 BParam:=ReadParamStr(BDataS); HexToDWordCheck(BParam,BData); FSegFlags:=BData;
 BParam:=ReadParamStr(BDataS); HexToDWordCheck(BParam,BData); FHwBase:=BData;
 BParam:=ReadParamStr(BDataS); HexToDWordCheck(BParam,BData); FHwSize:=BData;
 BParam:=ReadParamStr(BDataS); HexToDWordCheck(BParam,BData); FHwWidth:=BData;
 BParam:=ReadParamStr(BDataS); HexToDWordCheck(BParam,BData); FFillSize:=BData;
 BParam:=ReadParamStr(BDataS); BPBin:=MsBinImport(BParam);
 SetLength(FBin,FHwSize);
 SetLength(FIpToLine,FHwSize);
 BIndex:=0;
 while BIndex<Length(FBin) do
  begin
  if BPBin=nil then BDataC:=#0
  else if BIndex<FFillSize then BDataC:=BPBin^[1+BIndex]
  else BDataC:=#0;
  FBin[1+BIndex]:=BDataC;
  FIpToLine[BIndex]:=$FFFFFFFF;
  inc(BIndex);
  end;
End;

Procedure TMemSeg.MemSegImport ( AMemSeg : TMemSeg );
Begin
 FSegName:=AMemSeg.FSegName;
 FSegFlags:=AMemSeg.FSegFlags;
 FHwBase:=AMemSeg.FHwBase;
 FHwSize:=AMemSeg.FHwSize;
 FHwWidth:=AMemSeg.FHwWidth;
 FFillSize:=AMemSeg.FFillSize;
 FBin:=AMemSeg.FBin;
 FIpToLine:=Copy(AMemSeg.FIpToLine);
End;

Procedure TMemSeg.CreateHex;
Begin
 HexFileCreate(FHwBase,FBin,FHex);
End;

Procedure TMemSeg.CreateMem;
Begin
 MemFileCreate(FHwSize,FHwWidth,FBin,FMem);
End;

Function TMemSeg.IsInside ( AAddr : Cardinal ) : boolean;
Begin
 Result:=(AAddr>=FHwBase) and (AAddr<(FHwBase+FHwSize));
End;

Function TMemSeg.IsInside ( AAddr : Cardinal; ASize : Cardinal ) : boolean;
Begin
 Result:=(AAddr>=FHwBase) and ((AAddr+ASize)<=(FHwBase+FHwSize));
End;

Function TMemSeg.IsInside ( AAddr : Cardinal; Const AData : string ) : boolean;
Begin
 Result:=IsInside(AAddr,Length(AData));
End;

Function TMemSeg.WrData ( AAddr : Cardinal; Const AData : string ) : Cardinal;
Begin
 if IsInside(AAddr,AData) then begin Move(AData[1],FBin[1+AAddr-FHwBase],Length(AData)); Result:=Length(AData); end
 else Result:=0;
End;

Procedure TMemSeg.WrDataX ( AAddr : Cardinal; ASize : byte; AData : Cardinal );
Var
  BAddr     : Cardinal;
  BIndex    : byte;
Begin
 repeat
 if IsInside(AAddr,ASize)=FALSE then break;
 BAddr:=AAddr-FHwBase;
 BIndex:=0;
 while BIndex<ASize do
  begin
  FBin[1+BAddr+BIndex]:=Char(AData);
  AData:=AData shr 8;
  inc(BIndex);
  end;
 until TRUE;
End;

Function TMemSeg.RdData ( AAddr : Cardinal; ASize : Cardinal ) : string;
Begin
 if IsInside(AAddr,ASize) then Result:=Copy(FBin,1+AAddr-FHwBase,ASize)
 else Result:='';
End;

Function TMemSeg.RdDataX ( AAddr : Cardinal; ASize : byte ) : Cardinal;
Var
  BAddr     : Cardinal;
  BIndex    : byte;
Begin
 Result:=0;
 repeat
 if IsInside(AAddr,ASize)=FALSE then break;
 BAddr:=AAddr-FHwBase;
 BIndex:=0;
 while BIndex<ASize do
  begin
  Result:=Result or (Cardinal(FBin[1+BAddr+BIndex]) shl (BIndex*8));
  inc(BIndex);
  end;
 until TRUE;
End;

end.

