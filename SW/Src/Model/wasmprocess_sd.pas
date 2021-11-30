unit WasmProcess_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TSectId = ( wsCust, wsType, wsImport, wsFunc, wsTable, wsMem, wsGlobal, wsExport, wsStart, wsElem, wsCode, wsData);

  TWasmFunc = class(TObject)
    private
      FParams,
      FReturns,
      FLocals   : string;
      FCode     : string;

      FLastError    : string;
    public
      Constructor Create; Virtual;
      Destructor Destroy; Override;

      Function ParseType ( Var AData : string ) : boolean;
      Procedure SetType ( Const AReadS : string );
      Function ParseCode ( Const AReadS : string ) : boolean;
      Procedure VerboseLines ( ALines : TStrings );

      property LastError : string read FLastError;
  end;

  TWasmFuncList = array of TWasmFunc;

  TWasmFile = class(TObject)
    private
      FLastError    : string;
      FMagic,
      FVersion      : Cardinal;
      FFuncTypeList : TStringList;
      FFuncList     : TWasmFuncList;

      Procedure ClearFuncList;
      Function AppendFunc : TWasmFunc;
      Function ReadSect ( AStream : TStream ) : boolean;

    public
      Constructor Create; Virtual;
      Destructor Destroy; Override;

      Function Parse ( AStream : TStream ) : boolean;
      Procedure ExportText ( ALines : TStrings );

      property LastError : string read FLastError;
  end;

Function WasmParse ( Const AFullName : string; AText : TStrings; Out AErrorCode : string ) : boolean;

implementation

Uses
  ConComL;

Function WasmParse ( Const AFullName : string; AText : TStrings; Out AErrorCode : string ) : boolean;
Var
  BFile     : TWasmFile;
  BStream   : TMemoryStream;
Begin
 Result:=FALSE; AErrorCode:='';
 BFile:=TWasmFile.Create; BStream:=TMemoryStream.Create;

 repeat
 try
   BStream.LoadFromFile(AFullName);
 except
   AErrorCode:='Cannot read file '+AFullName+'[R:WasmRead/WasmParse]';
   break;
 end;
 Result:=BFile.Parse(BStream);
 AErrorCode:=BFile.LastError;
 if Result then BFile.ExportText(AText);
 until TRUE;

 BFile.Free; BStream.Free;
End;

Function ReadLem128 ( AStream : TStream; Out AData : Cardinal ) : boolean;
Var
  BShift    : byte;
  BByte     : byte;
Begin
 Result:=FALSE;
 AData:=0;
 BShift:=0;
 BByte:=0;
 repeat
 if BShift>32 then break;
 if AStream.Read(BByte,1)<>1 then break;
 AData:=AData or ((BByte and $7F) shl BShift);
 if (BByte and $80)=$00 then begin Result:=TRUE; break; end;
 BShift:=BShift+7;
 until FALSE;
End;

Function ReadLem128 ( Var AReadS : string; Out AData : Cardinal ) : boolean;
Var
  BShift    : byte;
  BByte     : byte;
Begin
 Result:=FALSE;
 AData:=0;
 BShift:=0;
 BByte:=0;
 repeat
 if BShift>32 then break;
 if AReadS='' then break;
 BByte:=Ord(AReadS[1]); Delete(AReadS,1,1);
 AData:=AData or ((BByte and $7F) shl BShift);
 if (BByte and $80)=$00 then begin Result:=TRUE; break; end;
 BShift:=BShift+7;
 until FALSE;
End;

Function DecWasmType ( ATypeB : byte; Out ATypeC : char ) : boolean;
Begin
 Result:=FALSE; ATypeC:='?';
 repeat
 case ATypeB of
   $7F: ATypeC:='i';
   $7D: ATypeC:='f';
   else break;
 end;
 Result:=TRUE;
 until TRUE;
End;

Function PropType ( ACount : byte; ATypeC : char ) : string;
Var
  BIndex    : byte;
Begin
 Result:='';
 BIndex:=0; while BIndex<ACount do begin Result:=Result+ATypeC; Inc(BIndex); end;
End;

{ *** TWasmSect *** }

Constructor TWasmFunc.Create;
Begin
 Inherited;
End;

Destructor TWasmFunc.Destroy;
Begin
 Inherited;
End;

Function TWasmFunc.ParseType ( Var AData : string ) : boolean;
Var
  BLen      : Integer;
  BIndex    : Integer;
  BTypeB    : byte;
Begin
 repeat
 if AData='' then begin FLastError:='Function data is absent [R:TWasmFunc.ParseType]'; break; end;
 if Ord(AData[1])<>$60 then begin FLastError:='Incorrect function TAG [R:TWasmFunc.ParseType]'; break; end;
 Delete(AData,1,1);
 if Length(AData)<1 then break;
 BLen:=Ord(AData[1]); Delete(AData,1,1);
 if Length(AData)<BLen then break;
 BIndex:=0;
 while BIndex<BLen do
  begin
  BTypeB:=Ord(AData[1+BIndex]);
  case BTypeB of
    $7F: FParams:=FParams+'i';
    $7D: FParams:=FParams+'f';
    else begin FLastError:='Unknown data type 0x'+IntToHex(BTypeB,2)+'[R:TWasmFunc.ParseType]'; end;
  end;
  inc(BIndex);
  end;
 if BIndex<>BLen then break;
 Delete(AData,1,BLen);
 // Returns
 if Length(AData)<1 then break;
 BLen:=Ord(AData[1]); Delete(AData,1,1);
 if Length(AData)<BLen then break;
 BIndex:=0;
 while BIndex<BLen do
  begin
  BTypeB:=Ord(AData[1+BIndex]);
  case BTypeB of
    $7F: FReturns:=FReturns+'i';
    $7D: FReturns:=FReturns+'f';
    else begin FLastError:='Unknown data type 0x'+IntToHex(BTypeB,2)+'[R:TWasmFunc.ParseType]'; end;
  end;
  inc(BIndex);
  end;
 if BIndex<>BLen then break;
 Delete(AData,1,BLen);
 Result:=TRUE;
 until TRUE;

 if (Result=FALSE) and (FLastError='') then FLastError:='Error parsing function [R:TWasmFunc.ParseType]';
End;

Procedure TWasmFunc.SetType ( Const AReadS : string );
Var
  BReadS    : string;
Begin
 BReadS:=AReadS;
 FParams:=ReadParamStr(BReadS);
 FReturns:=ReadParamStr(BReadS);
End;

Function TWasmFunc.ParseCode ( Const AReadS : string ) : boolean;
Var
  BReadS    : string;
  BLocCnt,
  BLocIdx   : Cardinal;
  BTypeCnt  : byte;
  BTypeB    : byte;
  BTypeC    : Char;
Begin
 Result:=FALSE;
 BReadS:=AReadS;
 repeat
 if BReadS='' then break;
 BLocCnt:=Ord(BReadS[1]); Delete(BReadS,1,1);
 BLocIdx:=0;
 while BLocIdx<BLocCnt do
  begin
  if Length(BReadS)<2 then break;
  BTypeCnt:=Ord(BReadS[1]); BTypeB:=Ord(BReadS[2]); Delete(BReadS,1,2);
  if DecWasmType(BTypeB,BTypeC)=FALSE then break;
  FLocals:=FLocals+PropType(BTypeCnt,BTypeC);
  inc(BLocIdx);
  end;
 if BLocIdx<>BLocCnt then break;
 FCode:=BReadS;
 Result:=TRUE;
 until TRUE;
End;

Function DecodeTypes ( Const ATypes : string ) : string;
Var
  BIndex    : Integer;
Begin
 Result:='';
 BIndex:=0;
 while BIndex<Length(ATypes) do
  begin
  if Result<>'' then Result:=Result+' ';
  case ATypes[1+Bindex] of
    'i': Result:=Result+'i32';
    'f': Result:=Result+'f32';
    else Result:=Result+'?';
  end; // case
  inc(BIndex);
  end;
End;

Procedure TWasmFunc.VerboseLines ( ALines : TStrings );
Begin
 ALines.Append('; func ( '+DecodeTypes(FParams)+' ) : '+DecodeTypes(FReturns));
 ALines.Append('; locals: '+DecodeTypes(FLocals));
End;

{ *** TWasmFile *** }

Constructor TWasmFile.Create;
Begin
 Inherited;
 FFuncTypeList:=TStringList.Create;
End;

Destructor TWasmFile.Destroy;
Begin
 ClearFuncList;
 FFuncTypeList.Free;
 Inherited;
End;

Procedure TWasmFile.ClearFuncList;
Var
  BFuncIdx  : Integer;
Begin
 BFuncIdx:=0;
 while BFuncIdx<Length(FFuncList) do
  begin
  FFuncList[BFuncIdx].Free;
  inc(BFuncIdx);
  end;
 FFuncList:=nil;
End;

Function TWasmFile.AppendFunc : TWasmFunc;
Var
  BFuncIdx  : Integer;
Begin
 Result:=TWasmFunc.Create;
 BFuncIdx:=Length(FFuncList);
 SetLength(FFuncList,BFuncIdx+1);
 FFuncList[BFuncIdx]:=Result;
End;

Function ParseTypeList ( Var AReadS : string; Out ATypeList : string ) : boolean;
Var
  BIndex,
  BLen      : Integer;
  BTypeB    : byte;
  BTypeC    : char;
Begin
 Result:=FALSE;
 ATypeList:='';
 repeat
 if Length(AReadS)<1 then break;
 BLen:=Ord(AReadS[1]); Delete(AReadS,1,1);
 if Length(AReadS)<BLen then break;
 BIndex:=0;
 while BIndex<BLen do
  begin
  BTypeB:=Ord(AReadS[1+BIndex]);
  if DecWasmType(BTypeB,BTypeC)=FALSE then break;
  ATypeList:=ATypeList+BTypeC;
  inc(BIndex);
  end;
 if BIndex<>BLen then break;
 Delete(AReadS,1,BLen);
 Result:=TRUE;
 until TRUE;
End;

Function TWasmFile.ReadSect ( AStream : TStream ) : boolean;
Var
  BSectId   : byte;
  BDataLen  : Cardinal;
  BDataBin  : string;
  BFnIdx    : Integer;
  BFunc     : TWasmFunc;
  BResult   : boolean;
  BParams,
  BReturns  : string;
  BFuncType : Cardinal;
  BSize     : Cardinal;
  BReadS    : string;
  BDummyS   : string;
Begin
 Result:=FALSE;
 BSectId:=0;
 repeat
 if AStream.Read(BSectId,1)<>1 then begin FLastError:='Cannot read section ID [R:TWasmFile.Parse]'; break; end;
 if BSectId>11 then begin FLastError:='Invalid section ID [R:TWasmFile.Parse]'; break; end;
 if ReadLem128(AStream,BDataLen)=FALSE then begin FLastError:='Error reading section size [R:TWasmFile.Parse]'; break; end;
 if BDataLen=0 then begin Result:=TRUE; break; end;
 SetLength(BDataBin,BDataLen);
 if AStream.Read(BDataBin[1],BDataLen)<>BDataLen then begin FLastError:='Error reading section data [R:TWasmFile.Parse]'; break; end;
 BDummyS:=StrBinToHex(BDataBin);
 if BDummyS='' then break;
 case BSectId of
   $00: begin // Custom
        Sleep(0);
        end;
   $01: begin // Type
        Delete(BDataBin,1,1); // Redundant item count
        BResult:=TRUE;
        while BDataBin<>'' do
         begin
         if Ord(BDataBin[1])<>$60 then break;
         Delete(BDataBin,1,1);
         BResult:=FALSE;
         if ParseTypeList(BDataBin,BParams)=FALSE then begin FLastError:='Cannot read function params [R:TWasmFile.Parse]'; break; end;
         if ParseTypeList(BDataBin,BReturns)=FALSE then begin FLastError:='Cannot read function returns [R:TWasmFile.Parse]'; break; end;
         FFuncTypeList.Append(BParams+' '+BReturns);
         BResult:=TRUE;
         end;
        if BResult=FALSE then break;
        end;
   $03: begin // Function
        Delete(BDataBin,1,1); // Redundant item count
        BResult:=TRUE;
        while BDataBin<>'' do
         begin
         if ReadLem128(BDataBin,BFuncType)=FALSE then begin BResult:=FALSE; FLastError:='Error reading function table [R:TWasmFile.Parse]'; break; end;
         if BFuncType>=FFuncTypeList.Count then begin BResult:=FALSE; FLastError:='Function type outs of range [R:TWasmFile.Parse]'; break; end;
         BFunc:=AppendFunc;
         BFunc.SetType(FFuncTypeList.Strings[BFuncType]);
         end;
        if BResult=FALSE then break;
        end;
   $0A: begin // CodeSection
        Delete(BDataBin,1,1); // Redundant item count
        BFnIdx:=0;
        while BFnIdx<Length(FFuncList) do
         begin
         if ReadLem128(BDataBin,BSize)=FALSE then break;
         if Length(BDataBin)<BSize then begin FLastError:='Error reading code section [R:TWasmFile.Parse]'; break; end;
         BReadS:=Copy(BDataBin,1,BSize); Delete(BDataBin,1,BSize);
         BFunc:=FFuncList[BFnIdx];
         if BFunc.ParseCode(BReadS)=FALSE then begin FLastError:='Error reading code section [R:TWasmFile.Parse]'; break; end;
         inc(BFnIdx);
         end;
        if BFnIdx<Length(FFuncList) then break;
        end;
   end; // case
 Result:=TRUE;
 until TRUE;
End;

Function TWasmFile.Parse ( AStream : TStream ) : boolean;
Var
  BResult   : boolean;
Begin
 ClearFuncList; FFuncTypeList.Clear;
 Result:=FALSE; FLastError:='Unknown error [R:TWasmFile.Parse]';
 repeat
 AStream.Position:=0;
 if AStream.Read(FMagic,4)<>4 then begin FLastError:='File is too small [R:TWasmFile.Parse]'; break; end;
 if AStream.Read(FVersion,4)<>4 then begin FLastError:='File is too small [R:TWasmFile.Parse]'; break; end;
 BResult:=TRUE;
 while AStream.Position<AStream.Size do
  begin
  if ReadSect(AStream)=FALSE then begin BResult:=FALSE; break; end;
  end;
 if BResult=FALSE then break;
 Result:=TRUE; FLastError:='';
 until TRUE;
End;

Procedure TWasmFile.ExportText ( ALines : TStrings );
Var
  BFuncIdx  : Integer;
  BFunc     : TWasmFunc;
Begin
 ALines.Clear;
 ALines.Append('; WASM V_'+IntToStr(FVersion));
 ALines.Append('');
 BFuncIdx:=0;
 while BFuncIdx<Length(FFuncList) do
  begin
  BFunc:=FFuncList[BFuncIdx];
  BFunc.VerboseLines(ALines);
  ALines.Append('');
  Inc(BFuncIdx);
  end;
End;

end.

