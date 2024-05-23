unit Mpu_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TMpuSeg = record
    FOrig   : QWord;
    FGran   : byte;
    FUsage  : byte;
    FAddrA  : Cardinal;
    FAddrE  : Cardinal;
    FAddrS  : Cardinal;
  end;

  TMpuRegs = record
    FSegDs  : array [0..2] of TMpuSeg;
    FSegCs  : TMpuSeg;
  end;

Function LoadMpuSeg ( Const ARegsS : string ) : TMpuSeg;
Function LoadMpuRegs ( Const ARegsS : string ) : TMpuRegs;
Function UnmapMpuAddrCode ( Const AMpuRegs : TMpuRegs; AAddrVirt : Cardinal ) : Cardinal;
Function UnmapMpuAddrData ( Const AMpuRegs : TMpuRegs; AAddrVirt : Cardinal ) : Cardinal;

Const
  ZMpuSeg : TMpuSeg = (FOrig: 0; FGran: 0; FUsage: 0; FAddrA: 0; FAddrE: 0; FAddrS: 0);
  ZMpuRegs : TMpuRegs =
    (
     FSegDs:
      (
       (FOrig: 0; FGran: 0; FUsage: 0; FAddrA: 0; FAddrE: 0; FAddrS: 0),
       (FOrig: 0; FGran: 0; FUsage: 0; FAddrA: 0; FAddrE: 0; FAddrS: 0),
       (FOrig: 0; FGran: 0; FUsage: 0; FAddrA: 0; FAddrE: 0; FAddrS: 0)
      );
     FSegCs: ( FOrig: 0; FGran: 0; FUsage: 0; FAddrA: 0; FAddrE: 0; FAddrS: 0 )
    );

implementation

Uses
  ConComL;

Function LoadMpuSeg ( Const ARegsS : string ) : TMpuSeg;
Var
  BOrig     : QWord;
Begin
 Result:=ZMpuSeg;
 HexToQWordCheck(ARegsS,BOrig); Result.FOrig:=BOrig;
 if (BOrig and $8000000000000000)<>0 then Result.FGran:=1;
 Result.FUsage:=(BOrig shr 60) and $7;
 Result.FAddrA:=(BOrig shr 40) and $FFFFF;
 Result.FAddrE:=(BOrig shr 20) and $FFFFF;
 Result.FAddrS:=(BOrig shr  0) and $FFFFF;
 if Result.FGran=0 then
  begin
  Result.FAddrA:=Result.FAddrA shl 4;
  Result.FAddrE:=Result.FAddrE shl 4;
  Result.FAddrS:=Result.FAddrS shl 4;
  end
 else
  begin
  Result.FAddrA:=Result.FAddrA shl 12;
  Result.FAddrE:=Result.FAddrE shl 12;
  Result.FAddrS:=Result.FAddrS shl 12;
  end
End;

Function LoadMpuRegs ( Const ARegsS : string ) : TMpuRegs;
Begin
 Result:=ZMpuRegs;
 Result.FSegCs:=LoadMpuSeg(Copy(ARegsS,1+0,16));
 Result.FSegDs[0]:=LoadMpuSeg(Copy(ARegsS,1+1*16,16));
 Result.FSegDs[1]:=LoadMpuSeg(Copy(ARegsS,1+2*16,16));
 Result.FSegDs[2]:=LoadMpuSeg(Copy(ARegsS,1+3*16,16));
End;

Function UnmapSegAddr ( Const AMpuSeg : TMpuSeg; AAddrVirt : Cardinal; Var AAddrReal : Cardinal ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if AMpuSeg.FAddrE=0 then break;
 if (AAddrVirt>=AMpuSeg.FAddrS) and (AAddrVirt<AMpuSeg.FAddrE) then
 else break;
 AAddrReal:=AAddrVirt+AMpuSeg.FAddrA;
 Result:=TRUE;
 until TRUE;
End;


Function UnmapMpuAddrCode ( Const AMpuRegs : TMpuRegs; AAddrVirt : Cardinal ) : Cardinal;
Begin
 Result:=AAddrVirt;
 UnmapSegAddr(AMpuRegs.FSegCs,AAddrVirt,Result);
End;

Function UnmapMpuAddrData ( Const AMpuRegs : TMpuRegs; AAddrVirt : Cardinal ) : Cardinal;
Var
  BSegIdx   : Integer;
  BSegInUse : boolean;
Begin
 Result:=0;
 BSegIdx:=0; BSegInUse:=FALSE;
 while BSegIdx<3 do
  begin
  if AMpuRegs.FSegDs[BSegIdx].FAddrE<>0 then BSegInUse:=TRUE;
  if UnmapSegAddr(AMpuRegs.FSegDs[BSegIdx],AAddrVirt,Result) then break;
  inc(BSegIdx);
  end;
 if BSegInUse=FALSE then Result:=AAddrVirt;
End;

end.

