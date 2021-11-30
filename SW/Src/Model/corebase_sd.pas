unit CoreBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd;

Type
  TRegMIp = record
    FIp     : Cardinal;
    FFlags  : byte;
  end;

  TOnDasmMissing = Function ( AIpPrev, AIpThis : Cardinal ) : boolean of Object;

  TSysCmd = (scSwt, scSiEnd, scSiLock, scSiUnlock, scSiConf);

  {TThreadContext = record
    FRegGR, FRegGWX : Cardinal;
    FRegFR, FRegFWX : Cardinal;
    FRegER, FRegEWX : Cardinal;
    FRegDR, FRegDWX : Cardinal;
    FRegCR, FRegCWX : Cardinal;
    FRegBR, FRegBWX : Cardinal;
    FRegAR, FRegAWX : Cardinal;
    FRegSP, FRegIP  : Cardinal;
  end;}

  TCoreBase = class;

  TOnMemRd = Function ( AAddr : Cardinal; ASize : Cardinal; Out AData : string ) : boolean of object;
  TOnMemRdX = Function ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean of object;
  TOnMemWrX = Function ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean of object;
  TOnIoRdX = Function ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean of object;
  TOnIoWrX = Function ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean of object;
  TOnSysCmd = Function ( ACmd : TSysCmd; ACore : TCoreBase ) : boolean of object;

  TCoreBase = class(TObject)
  private
    FOnViewAny  : TOnViewAny;
    FOnMemRd    : TOnMemRd;
    FOnMemRdX   : TOnMemRdX;
    FOnMemWrX   : TOnMemWrX;
    FOnIoRdX    : TOnIoRdX;
    FOnIoWrX    : TOnIoWrX;
    FOnSysCmd   : TOnSysCmd;
    FContAddr   : Cardinal;

    Function MemRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
    Function MemWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
    Function IoRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
    Function IoWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;

  protected
    FCoreType       : byte;
    FExtReg         : array [0..1] of Cardinal;
    FRegMIp         : TRegMIp;
    FCoreCnt,
    FCoreIdx        : Byte;
    FStuckAtError   : boolean;
    FStartMIp       : TRegMIp;
    FIsTrapHit,
    FIsTestEnd      : boolean;
    FIoSpace        : Cardinal;

    FCycleCnt,           // Cycle counter
    FInstRet    : QWord; // instructions retired counter

    FOnDasmMissing  : TOnDasmMissing;

    Procedure ViewAny ( Const AMessage : string );
    Function MemRd ( AAddr : Cardinal; ASize : Cardinal; Out AData : string ) : boolean;
    //Function MemRdD ( AAddr : Cardinal; Out AData : Cardinal ) : boolean;
    //Function MemWrD ( AAddr : Cardinal; AData : Cardinal ) : boolean;
    Function MioRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
    Function MioWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
    Function SysCmd ( ACmd : TSysCmd ) : boolean;

    Procedure SetFlagsA ( AFlags : byte );
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( ARegMIpData : Cardinal; ACoreCnt, ACoreIdx : byte );
    Procedure ImportMIp ( AData : Cardinal );
    Function ExportMIp : Cardinal;
    Function RegsAsStr : string; Virtual; Abstract;
    Function RdRegs : string; Virtual; Abstract;
    Procedure WrRegs ( Const ARegs : string ); Virtual; Abstract;
    Procedure Reset; Virtual;
    Procedure FirstStep;
    Procedure StepInto; Virtual;
    Function IsBreakHit ( Const ABreakList : TBreakList ) : boolean;
    Procedure ClearTrapHit;
    Function IsCoreTypeCorrect : boolean;
    Procedure ImportAll ( ASrc : TCoreBase );
    Function VerboseStat : string;

    property OnViewAny : TOnViewAny read FOnViewAny write FOnViewAny;
    property OnMemRd : TOnMemRd read FOnMemRd write FOnMemRd;
    property OnMemRdX : TOnMemRdX read FOnMemRdX write FOnMemRdX;
    property OnMemWrX : TOnMemWrX read FOnMemWrX write FOnMemWrX;
    property OnIoRdX : TOnIoRdX read FOnIoRdX write FOnIoRdX;
    property OnIoWrX : TOnIoWrX read FOnIoWrX write FOnIoWrX;
    property OnSysCmd : TOnSysCmd read FOnSysCmd write FOnSysCmd;
    property OnDasmMissing : TOnDasmMissing read FOnDasmMissing write FOnDasmMissing;

    property ContAddr : Cardinal read FContAddr write FContAddr;
    property RegMIp : TRegMIp read FRegMIp;
    property StuckAtError : boolean read FStuckAtError;
    property IsTrapHit : boolean read FIsTrapHit;
    property IsTestEnd : boolean read FIsTestEnd;
    property IoSpace : Cardinal read FIoSpace write FIoSpace;
  end;

implementation

Constructor TCoreBase.Create;
Begin
 Inherited;
End;

Destructor TCoreBase.Destroy;
Begin
 Inherited;
End;

Procedure TCoreBase.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TCoreBase.Init ( ARegMIpData : Cardinal; ACoreCnt, ACoreIdx : byte );
Begin
 ImportMIp(ARegMIpData);
 FStartMIp:=FRegMIp;
 FCoreCnt:=ACoreCnt;
 FCoreIdx:=ACoreIdx;
 FExtReg[0]:=0; FExtReg[1]:=0;
 FStuckAtError:=FALSE;
 Reset;
End;

Function TCoreBase.IsCoreTypeCorrect : boolean;
Begin
 Result:=((FRegMIp.FFlags shr 4) and $7)=FCoreType;
End;

Function TCoreBase.MemRd ( AAddr : Cardinal; ASize : Cardinal; Out AData : string ) : boolean;
Begin
 Result:=FALSE; AData:='';
 if Assigned(FOnMemRd) then Result:=FOnMemRd(AAddr,ASize,AData);
End;

Function TCoreBase.MemRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
Begin
 Result:=FOnMemRdX(AAddr,ASize,AData);
End;

Function TCoreBase.MemWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
Begin
 Result:=FOnMemWrX(AAddr,ASize,AData);
End;

Function TCoreBase.IoRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
Begin
 Result:=FOnIoRdX(AAddr,ASize,AData);
End;

Function TCoreBase.IoWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
Begin
 Result:=FOnIoWrX(AAddr,ASize,AData);
End;

Function TCoreBase.MioRdX ( AAddr : Cardinal; ASize : byte; Out AData : Cardinal ) : boolean;
Begin
 if AAddr<FIoSpace then Result:=IoRdX(AAddr,ASize,AData)
 else Result:=MemRdX(AAddr,ASize,AData);
End;

Function TCoreBase.MioWrX ( AAddr : Cardinal; ASize : byte; AData : Cardinal ) : boolean;
Begin
 if AAddr<FIoSpace then Result:=IoWrX(AAddr,ASize,AData)
 else Result:=MemWrX(AAddr,ASize,AData);
End;

Function TCoreBase.SysCmd ( ACmd : TSysCmd ) : boolean;
Begin
 Result:=FOnSysCmd(ACmd,Self);
End;

Procedure TCoreBase.Reset;
Begin
 FRegMIp:=FStartMIp;
 FIsTrapHit:=FALSE;
 FIsTestEnd:=FALSE;
 FCycleCnt:=0; FInstRet:=0;
End;

Procedure TCoreBase.FirstStep;
Begin
 FIsTrapHit:=FALSE;
 FIsTestEnd:=FALSE;
End;

Procedure TCoreBase.StepInto;
Begin
 FIsTrapHit:=FALSE;
End;

Function TCoreBase.IsBreakHit ( Const ABreakList : TBreakList ) : boolean;
Var
  BBreakIdx : Integer;
  BBreakIp  : Cardinal;
Begin
 Result:=FALSE;
 BBreakIdx:=0;
 while BBreakIdx<Length(ABreakList) do
  begin
  BBreakIp:=ABreakList[BBreakIdx];
  if BBreakIp=0 then break;
  if FRegMIp.FIp=BBreakIp then begin Result:=TRUE; break; end;
  inc(BBreakIdx);
  end;
End;

Procedure TCoreBase.ClearTrapHit;
Begin
 FIsTrapHit:=FALSE;
End;

Function MIpToData ( Const ARegMIp : TRegMIp ) : Cardinal;
Begin
 Result:=(Cardinal(ARegMIp.FFlags) shl 24) or (ARegMIp.FIp and $FFFFFF);
End;

Function TCoreBase.ExportMIp : Cardinal;
Begin
 Result:=MIpToData(FRegMIp);
End;

Procedure TCoreBase.ImportMIp ( AData : Cardinal );
Begin
 FRegMIp.FFlags:=(AData shr 24) and $FF;
 FRegMIp.FIp:=AData and $FFFFFF;
End;

Procedure TCoreBase.SetFlagsA ( AFlags : byte );
Begin
 FRegMIp.FFlags:=(FRegMIp.FFlags and $F0) or (AFlags and $0F);
End;

Procedure TCoreBase.ImportAll ( ASrc : TCoreBase );
Begin
 OnViewAny := ASrc.OnViewAny;
 OnMemRd   := ASrc.OnMemRd;
 OnMemRdX  := ASrc.OnMemRdX;
 OnMemWrX  := ASrc.OnMemWrX;
 OnIoRdX   := ASrc.OnIoRdX;
 OnIoWrX   := ASrc.OnIoWrX;
 IoSpace   := ASrc.IoSpace;
 OnSysCmd  := ASrc.OnSysCmd;
 OnDasmMissing := ASrc.OnDasmMissing;
 FStartMIp:=ASrc.FStartMIp;
 FCoreCnt:=ASrc.FCoreCnt;
 FCoreIdx:=ASrc.FCoreIdx;
 FContAddr:=ASrc.FContAddr;
 WrRegs(ASrc.RdRegs);
End;

Function TCoreBase.VerboseStat : string;
Begin
 Result:='Core_'+IntToStr(FCoreIdx)+': InstExecuted='+IntToStr(FInstRet);
End;

end.

