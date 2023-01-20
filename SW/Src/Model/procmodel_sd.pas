unit ProcModel_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MemSeg_sd, AsmTypes_sd, CoreBase_sd, CoreRV_sd, CoreMS_sd, CoreWA_sd;

Type
  TCoreList = array of TCoreBase;
  TOnProcessTerm = Procedure ( AData : char ) of Object;
  TOnCommWr = Procedure ( AData : byte ) of object;
  TOnCommRd = Procedure ( Out AData : byte ) of object;

  TThListHdr = record
    FQueMask    : byte;
    FTailIdx    : byte;
    FHeadIdx    : byte;
  end;

  TProcModel = class(TObject)
  private
    FCoreListS      : string;
    FMemSegList     : TMemSegList;
    FDefCodeSeg,
    FDefDataSeg     : string;

    FMcuType        : byte;
    FCoreList       : TCoreList;
    FBreakList      : TBreakList;
    FExecLog        : TStringList;

    FIoSpace        : Cardinal;
    FTermBase,
    FCommBase       : Cardinal;

    FThList         : Cardinal;

    FOnViewAny      : TOnViewAny;
    FOnProcessTerm  : TOnProcessTerm;
    FOnCommWr       : TOnCommWr;
    FOnCommRd       : TOnCommRd;

    Procedure ViewAny ( Const AMessage : string );
    Procedure CoreListClear;
    Procedure CoreListCreate ( Const ACoreListS : string; AOnDasmMissing : TOnDasmMissing );
    Procedure CoreListUpdate;
    Function MemRd ( AAddr : Cardinal; ASize : Cardinal; Out AData : string ) : boolean;
    Function MemWr ( AAddr : Cardinal; Const AData : string ) : boolean;
    Function MemRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
    Function MemWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
    Function IoRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
    Function IoWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
    Function SysCmd ( ACmd : TSysCmd; ACore : TCoreBase ) : boolean;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure SetParams ( Const ACoreListS, ADefCodeSeg, ADefDataSeg : string; AIoSpace, ATermBase : Cardinal; AOnDasmMissing : TOnDasmMissing; AOnProcessTerm : TOnProcessTerm; AOnCommWr : TOnCommWr; AOnCommRd : TOnCommRd );
    Function LoadMem ( AMemSegList : TMemSegList ) : boolean;
    Function RdRegsBin : string;
    Function RdRegsStr : string;
    Function VerboseStat : string;
    Function GetIp ( ACoreIdx : Integer ) : Cardinal;
    Procedure SetBreakList ( Const ABreakList : TBreakList );
    Procedure Reset;
    Procedure FirstStep;
    Procedure StepInto;
    Procedure StepCombi ( Const ARegsBin : string );
    Procedure RunTo ( AAddr : Cardinal );
    Function IsTrapHit : boolean;
    Function IsTestEnd : boolean;
    Function IsBreakHit : boolean;
    Function IsStuckAtError : boolean;
    Function GetEtb : Cardinal;
    Procedure ClearTrapHit;
    Procedure SegListOrder;

    Procedure SetMcuType ( AMcuType : byte );

    property MemSegList : TMemSegList read FMemSegList;
    property ExecLog : TStringList read FExecLog;

    property OnViewAny : TOnViewAny read FOnViewAny write FOnViewAny;
  end;


implementation

Uses
  ConComL;

Function ImportThListHdr ( ADataD : Cardinal ) : TThListHdr;
Begin
 Result.FQueMask:=(ADataD shr 16) and $FF;
 Result.FTailIdx:=(ADataD shr  8) and $FF;
 Result.FHeadIdx:=(ADataD shr  0) and $FF;
End;

Function ExportThListHdr ( Const AHdr : TThListHdr ) : Cardinal;
Begin
 Result:=(AHdr.FQueMask shl 16) or
         (AHdr.FTailIdx shl 8) or
         (AHdr.FHeadIdx shl 0);
End;

{ *** TProcModel *** }

Constructor TProcModel.Create;
Begin
 Inherited;
 FExecLog:=TStringList.Create;
End;

Destructor TProcModel.Destroy;
Begin
 MemSegListClear(FMemSegList);
 CoreListClear;
 FExecLog.Free;
 Inherited;
End;

Procedure TProcModel.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TProcModel.CoreListClear;
Var
  BCoreIdx  : Integer;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].Free;
  inc(BCoreIdx);
  end;
 FCoreList:=nil;
End;

Procedure TProcModel.CoreListCreate ( Const ACoreListS : string; AOnDasmMissing : TOnDasmMissing );
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 CoreListClear;
 BCore:=nil;
 SetLength(FCoreList,Length(ACoreListS));
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreListS) do
  begin
  case ACoreListS[1+BCoreIdx] of
    'e': BCore:=TCoreRV.Create;
    'w': BCore:=TCoreWA.Create;
    's': BCore:=TCoreMS.Create;
    else BCore:=TCoreMS.Create;
  end;
  BCore.OnViewAny:=@ViewAny;
  BCore.OnMemRd:=@MemRd;
  BCore.OnMemRdX:=@MemRdX;
  BCore.OnMemWrX:=@MemWrX;
  BCore.OnIoRdX:=@IoRdX;
  BCore.OnIoWrX:=@IoWrX;
  BCore.IoSpace:=FIoSpace;
  BCore.OnSysCmd:=@SysCmd;
  BCore.OnDasmMissing:=AOnDasmMissing;
  FCoreList[BCoreIdx]:=BCore;
  inc(BCoreIdx);
  end;
End;

Procedure TProcModel.CoreListUpdate;
Var
  BCoreIdx  : Integer;
  BCoreA,
  BCoreB    : TCoreBase;
  BCoreType : byte;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreListS) do
  begin
  BCoreA:=FCoreList[BCoreIdx];
  repeat
  if BCoreA.IsCoreTypeCorrect then break;
  BCoreType:=(BCoreA.ExportMIp shr 28) and $7;
  case BCoreType of
    2: BCoreB:=TCoreWA.Create;
    1: BCoreB:=TCoreMS.Create;
    0: BCoreB:=TCoreRV.Create;
    else BCoreB:=TCoreMS.Create;
  end;
  BCoreB.ImportAll(BCoreA);
  FCoreList[BCoreIdx]:=BCoreB;
  BCoreA.Free;
  until TRUE;
  inc(BCoreIdx);
  end;
End;


Procedure TProcModel.SetParams ( Const ACoreListS, ADefCodeSeg, ADefDataSeg : string; AIoSpace, ATermBase : Cardinal; AOnDasmMissing : TOnDasmMissing; AOnProcessTerm : TOnProcessTerm; AOnCommWr : TOnCommWr; AOnCommRd : TOnCommRd );
Begin
 MemSegListClear(FMemSegList);
 CoreListClear;
 FCoreListS:=ACoreListS;
 FDefCodeSeg:=ADefCodeSeg; FDefDataSeg:=ADefDataSeg;
 FOnProcessTerm:=AOnProcessTerm; FIoSpace:=AIoSpace; FTermBase:=ATermBase; FCommBase:=ATermBase+1;
 FOnCommWr:=AOnCommWr; FOnCommRd:=AOnCommRd;
 CoreListCreate(FCoreListS,AOnDasmMissing);
End;

Function TProcModel.LoadMem ( AMemSegList : TMemSegList ) : boolean;
Var
  BSeg          : TMemSeg;
  BAddr         : Cardinal;
  BCoreIdx      : byte;
Begin
 Result:=FALSE;
 MemSegListImport(FMemSegList,AMemSegList);
 repeat
 BSeg:=MemSegSearch(AMemSegList,FDefCodeSeg);
 if BSeg=nil then begin ViewAny('eDefault code segment (to load core IPs) is not defined [R:TProcModel.LoadMem]'); break; end;
 BAddr:=BSeg.HwBase;
 if (Length(FCoreList)*4)>BSeg.HwSize then begin ViewAny('eDefault code segement is too small to read all core IPs [R:TProcModel.LoadMem]'); break; end;
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].Init(BSeg.RdDataX(BAddr,4),Length(FCoreList),BCoreIdx);
  inc(BAddr,4);
  inc(BCoreIdx);
  end;
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.RdRegsBin : string;
Var
  BRegsThis : string;
  BCoreIdx  : Integer;
Begin
 BRegsThis:='';
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BRegsThis:=BRegsThis+FCoreList[BCoreIdx].RdRegs;
  if FMcuType=9 then BRegsThis:=BRegsThis+DWordAsStr(0)+DWordAsStr(0)+DWordAsStr(0)+DWordAsStr(0); // Add "MPU" registers
  inc(BCoreIdx);
  end;
 Result:=BRegsThis;
End;

Function TProcModel.RdRegsStr : string;
Var
  BRegsThis : string;
  BCoreIdx  : Integer;
Begin
 BRegsThis:='';
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BRegsThis:=BRegsThis+FCoreList[BCoreIdx].RegsAsStr;
  inc(BCoreIdx);
  end;
 Result:=BRegsThis;
End;

Function TProcModel.VerboseStat : string;
Var
  BCoreIdx  : Integer;
Begin
 Result:='';
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  if Result<>'' then Result:=Result+' | ';
  Result:=Result+FCoreList[BCoreIdx].VerboseStat;
  inc(BCoreIdx);
  end;
End;

Function TProcModel.GetIp ( ACoreIdx : Integer ) : Cardinal;
Begin
 Result:=FCoreList[ACoreIdx].RegMIp.FIp;
End;

Function TProcModel.MemRd ( AAddr : Cardinal; ASize : Cardinal; Out AData : string ) : boolean;
Var
  BSeg      : TMemSeg;
Begin
 Result:=FALSE; AData:='';
 repeat
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 AData:=BSeg.RdData(AAddr,ASize);
 if Length(AData)<>ASize then break;
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.MemWr ( AAddr : Cardinal; Const AData : string ) : boolean;
Var
  BSeg      : TMemSeg;
Begin
 Result:=FALSE;
 repeat
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 if BSeg.WrData(AAddr,AData)<>Length(AData) then break;
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.MemRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
Var
  BSeg      : TMemSeg;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if (AAddr and (ASize-1))<>0 then
  begin
  ViewAny('eMemory read is misaligned [R:TProcModel.MemRdX]');
  break;
  end;
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 if BSeg.IsInside(AAddr,ASize)=FALSE then break;
 AData:=BSeg.RdDataX(AAddr,ASize);
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.MemWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
Var
  BSeg      : TMemSeg;
Begin
 Result:=FALSE;
 repeat
 if (AAddr and (ASize-1))<>0 then
  begin
  ViewAny('eMemory write is misaligned [R:TProcModel.MemWrX]');
  break;
  end;
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 if (BSeg.SegFlags and $02)=0 then begin ViewAny('eWrite attempt to read-only segment [R:TProcModel.MemWrD]'); break; end;
 if BSeg.IsInside(AAddr,ASize)=FALSE then break;
 BSeg.WrDataX(AAddr,ASize,AData);
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.IoRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
Var
  BSeg      : TMemSeg;
  BData     : byte;
Begin
 Result:=FALSE; AData:=0;
 repeat
 if (AAddr=FCommBase) and (ASize=1) then
  begin
  if Assigned(FOnCommRd) then begin FOnCommRd(BData); AData:=BData; end;
  Result:=TRUE;
  break;
  end;
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 if BSeg.IsInside(AAddr,ASize)=FALSE then break;
 AData:=BSeg.RdDataX(AAddr,ASize);
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.IoWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
Var
  BSeg      : TMemSeg;
Begin
 Result:=FALSE;
 repeat
 if (AAddr=FTermBase) and (ASize=1) then
  begin
  if Assigned(FOnProcessTerm) then FOnProcessTerm(Char(AData));
  Result:=TRUE;
  break;
  end;
 if (AAddr=FCommBase) and (ASize=1) then
  begin
  if Assigned(FOnCommWr) then FOnCommWr(AData);
  Result:=TRUE;
  break;
  end;
 BSeg:=MemSegSearch(FMemSegList,AAddr);
 if BSeg=nil then break;
 if (BSeg.SegFlags and $02)=0 then begin ViewAny('eWrite attempt to read-only segment [R:TProcModel.MemWrD]'); break; end;
 if BSeg.IsInside(AAddr,ASize)=FALSE then break;
 BSeg.WrDataX(AAddr,ASize,AData);
 Result:=TRUE;
 until TRUE;
End;

Function TProcModel.SysCmd ( ACmd : TSysCmd; ACore : TCoreBase ) : boolean;
Var
  BDataD        : Cardinal;
  BThHdr        : TThListHdr;
  BRegsData     : string;
Begin
 Result:=FALSE;
 repeat
 case ACmd of
   scSwt:
     begin
     if MemRdX(FThList,4,BDataD)=FALSE then begin ViewAny('eCannot read thread list header [R:TProcModel.SysCmd]'); break; end;
     BThHdr:=ImportThListHdr(BDataD);
     BRegsData:=ACore.RdRegs;
     if MemWr(ACore.ContAddr,BRegsData)=FALSE then begin ViewAny('eCannot save context [R:TProcModel.SysCmd]'); break; end;
     if MemWrX(FThList+4+4*BThHdr.FTailIdx,4,ACore.ContAddr)=FALSE then begin ViewAny('eCannot save context [R:TProcModel.SysCmd]'); break; end;
     BThHdr.FTailIdx:=(BThHdr.FTailIdx+1) and BThHdr.FQueMask;
     if MemRdX(FThList+4+4*BThHdr.FHeadIdx,4,BDataD)=FALSE then begin ViewAny('eCannot load context [R:TProcModel.SysCmd]'); break; end;
     BThHdr.FHeadIdx:=(BThHdr.FHeadIdx+1) and BThHdr.FQueMask;
     ACore.ContAddr:=BDataD;
     if MemRd(ACore.ContAddr,64,BRegsData)=FALSE then begin ViewAny('eCannot load context [R:TProcModel.SysCmd]'); break; end;
     ACore.WrRegs(BRegsData);
     if MemWrX(FThList,4,ExportThListHdr(BThHdr))=FALSE then begin ViewAny('eCannot write thread list header [R:TProcModel.SysCmd]'); break; end;
     Result:=TRUE;
     end;
   scSiEnd:
     begin
     if MemRdX(FThList,4,BDataD)=FALSE then begin ViewAny('eCannot read thread list header [R:TProcModel.SysCmd]'); break; end;
     BThHdr:=ImportThListHdr(BDataD);
     if MemRdX(FThList+4+4*BThHdr.FHeadIdx,4,BDataD)=FALSE then begin ViewAny('eCannot load context [R:TProcModel.SysCmd]'); break; end;
     BThHdr.FHeadIdx:=(BThHdr.FHeadIdx+1) and BThHdr.FQueMask;
     ACore.ContAddr:=BDataD;
     if MemRd(ACore.ContAddr,64,BRegsData)=FALSE then begin ViewAny('eCannot load context [R:TProcModel.SysCmd]'); break; end;
     ACore.WrRegs(BRegsData);
     if MemWrX(FThList,4,ExportThListHdr(BThHdr))=FALSE then begin ViewAny('eCannot write thread list header [R:TProcModel.SysCmd]'); break; end;
     Result:=TRUE;
     end;
   scSiLock:
     begin
     Result:=TRUE;
     end;
   scSiUnlock:
     begin
     Result:=TRUE;
     end;
   scSiConf:
     begin
     BRegsData:=ACore.RdRegs;
     if Length(BRegsData)<12 then begin ViewAny('eInternal error [R:TProcModel.SysCmd]'); break; end;
     BDataD:=(Cardinal(BRegsData[ 9]) shl  0) +
             (Cardinal(BRegsData[10]) shl  8) +
             (Cardinal(BRegsData[11]) shl 16) +
             (Cardinal(BRegsData[12]) shl 24);
     FThList:=BDataD;
     Result:=TRUE;
     end;
   else
     begin
     ViewAny('eInternal error [R:TProcModel.SysCmd]');
     break;
     end;
 end;
 until TRUE;
End;

Procedure TProcModel.SetBreakList ( Const ABreakList : TBreakList );
Begin
 FBreakList:=ABreakList;
End;

Procedure TProcModel.Reset;
Var
  BCoreIdx  : Integer;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].Reset;
  inc(BCoreIdx);
  end;
 FExecLog.Clear;
 CoreListUpdate;
End;

Procedure TProcModel.FirstStep;
Var
  BCoreIdx  : Integer;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].FirstStep;
  inc(BCoreIdx);
  end;
 FExecLog.Append(RdRegsBin);
 CoreListUpdate;
End;

Procedure TProcModel.StepInto;
Var
  BCoreIdx  : Integer;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].StepInto;
  inc(BCoreIdx);
  end;
 FExecLog.Append(RdRegsBin);
 CoreListUpdate;
End;

Procedure TProcModel.StepCombi ( Const ARegsBin : string );
Var
  BRegsBin  : string;
  BRegsA,
  BRegsB    : string;
  BRegsAs,
  BRegsBs   : string;
  BCoreIdx  : Integer;
  BLen      : Integer;
Begin
 BRegsBin:=ARegsBin;
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BRegsA:=FCoreList[BCoreIdx].RdRegs;
  BLen:=Length(BRegsA);
  BRegsB:=Copy(BRegsBin,1,BLen); Delete(BRegsBin,1,BLen);
  BRegsAs:=StrBinToHex(BRegsA); BRegsBs:=StrBinToHex(BRegsB);
  if BRegsBs<>BRegsAs then
   begin
   FCoreList[BCoreIdx].StepInto;
   BRegsAs:=StrBinToHex(FCoreList[BCoreIdx].RdRegs);
   end;
  inc(BCoreIdx);
  end;
 FExecLog.Append(RdRegsBin);
 CoreListUpdate;
End;

Const
  CMaxRunCnt    = 1000000;

Procedure TProcModel.RunTo ( AAddr : Cardinal );
Var
  BRunIdx   : Integer;
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 BRunIdx:=0;
 while BRunIdx<CMaxRunCnt do
  begin
  StepInto;
  BCoreIdx:=0;
  while BCoreIdx<Length(FCoreList) do
   begin
   BCore:=FCoreList[BCoreIdx];
   if BCore.RegMIp.FIp=AAddr then break;
   inc(BCoreIdx);
   end;
  if BCoreIdx<>Length(FCoreList) then break;
  if IsTrapHit or IsBreakHit or IsStuckAtError then break;
  inc(BRunIdx);
  end;
End;

Function TProcModel.IsTrapHit : boolean;
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BCore:=FCoreList[BCoreIdx];
  if BCore.IsTrapHit then break;
  inc(BCoreIdx);
  end;
 Result:=BCoreIdx<>Length(FCoreList);
End;

Function TProcModel.IsTestEnd : boolean;
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BCore:=FCoreList[BCoreIdx];
  if BCore.IsTestEnd then break;
  inc(BCoreIdx);
  end;
 Result:=BCoreIdx<>Length(FCoreList);
End;

Function TProcModel.IsBreakHit : boolean;
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BCore:=FCoreList[BCoreIdx];
  if BCore.IsBreakHit(FBreakList) then break;
  inc(BCoreIdx);
  end;
 Result:=BCoreIdx<>Length(FCoreList);
End;

Function TProcModel.IsStuckAtError : boolean;
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BCore:=FCoreList[BCoreIdx];
  if BCore.StuckAtError then break;
  inc(BCoreIdx);
  end;
 Result:=BCoreIdx<>Length(FCoreList);
End;

Function TProcModel.GetEtb : Cardinal;
Var
  BCoreIdx  : Integer;
  BCore     : TCoreBase;
Begin
 Result:=0;
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  BCore:=FCoreList[BCoreIdx];
  if BCore.IsTestEnd then Result:=Result or (1 shl (BCoreIdx+16));
  if BCore.IsTrapHit then Result:=Result or (1 shl (BCoreIdx+8));
  if BCore.IsBreakHit(FBreakList) then Result:=Result or (1 shl (BCoreIdx+0));
  inc(BCoreIdx);
  end;
End;

Procedure TProcModel.ClearTrapHit;
Var
  BCoreIdx  : Integer;
Begin
 BCoreIdx:=0;
 while BCoreIdx<Length(FCoreList) do
  begin
  FCoreList[BCoreIdx].ClearTrapHit;;
  inc(BCoreIdx);
  end;
End;

Procedure TProcModel.SegListOrder;
Begin
 MemSegListOrder(FMemSegList);
End;

Procedure TProcModel.SetMcuType ( AMcuType : byte );
Begin
 FMcuType:=AMcuType;
End;

end.

