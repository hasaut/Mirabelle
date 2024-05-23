unit WDebug_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Buttons,
  Graphics,
  MirLibBase, AsmTypes_sd, DbgBase_sd, DbgInfo_sd, DbgViewText_sd, DbgViewTextMs_sd, DbgViewTextRv_sd,
  DbgHwPanel_sd, DbgViewStack_sd, DbgRegs_sd, ConComI, LlvmBeHelper_sd, DbgViewVars_sd,
  Mpu_sd;

type
  TOnBreakChange = Procedure ( Const ABreakList : string ) of Object;
  TOnDbgBtnClick = Procedure ( ACmd : char ) of Object;
  TDbgRegsList = array of TDbgRegs;
  TIpToLine = Function ( AIp : Cardinal ) : Integer of object;


  { TMgDebug }

  TMgDebug = class(TFrame)
    BtForceRecompile: TSpeedButton;
    BtnCoverage: TSpeedButton;
    BtPause: TSpeedButton;
    BtReset: TSpeedButton;
    BtRunToCar: TSpeedButton;
    BtStart: TSpeedButton;
    BtStepBack: TSpeedButton;
    BtStepInto: TSpeedButton;
    BtStepOver: TSpeedButton;
    IlRegs: TImageList;
    IlTabs: TImageList;
    PcCoreFrame: TPageControl;
    PcEdit: TPageControl;
    PnlBtn: TPanel;
    PnlRegs: TPanel;
    procedure BtForceRecompileClick(Sender: TObject);
    procedure BtPauseClick(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
    procedure BtRunToCarClick(Sender: TObject);
    procedure BtStartClick(Sender: TObject);
    procedure BtStepBackClick(Sender: TObject);
    procedure BtStepIntoClick(Sender: TObject);
    procedure BtStepOverClick(Sender: TObject);
    procedure PcCoreFrameChange(Sender: TObject);
  private
    FParent     : TWinControl;
    FPrjName,
    FPrjPath,
    FLstName    : string;
    FLst        : TStringList;
    FActive     : boolean;
    FExecActive : boolean;
    FRegsThis   : string;

    FDbgFile    : TDbgInfoFile;
    FDbgProc    : TDbgInfoProc;
    FDbgLine    : TDbgInfoLine;

    FWndVars    : TWndVarsSd;

    FAddrBase   : Cardinal; // Lowest IP
    FLineToIp   : array of TIpCmd;   // Used to find IP by line LST
    FBreakList  : string; // FileName LineIdx
    FOnViewAny  : TOnViewAny;
    FOnProcAny  : TOnProcAny;
    FIpToLineA  : TIpToLine;

    FDbgAsmLine : Integer;

    FBreakReady         : boolean;
    FOnBreakChange      : TOnBreakChange;
    FDbgOnAsmLineChange : TDbgOnAsmLineChange;
    FDbgBtnClick        : TOnDbgBtnClick;

    FPnlHw              : TDbgHwPanel;
    FDbgStack           : TDbgViewStack;
    FDbgRegsList        : TDbgRegsList;
    FProgrBar           : TOzhProgrBar;
    FEipListVirt,
    FEspListVirt        : array of Cardinal;
    FEipListReal,
    FEspListReal        : array of Cardinal;
    FCoreIdx            : Integer;
    FActIdx             : Integer; // Set when we need to change ActiveCpuIndex due to Break/Trap
    FEip                : Cardinal;
    FIpAtLine           : Cardinal; // IP address of highlighted line (used to do RunTo = "F4")

    FPresetBreakList    : string;

    Procedure ViewAny ( Const AMessage : string );
    Procedure ProcAny ( Const AProcCmd : string );

    Function GetDbgView ( AIndex : Integer ) : TDbgViewText;
    Procedure DbgListClear;
    Procedure DbgListAddOrUpdate ( ATabIdx : Integer; Const AAsmName, ASrcName : string; ALineStart, ALineEnd : Integer; AIsLst : boolean );
    Procedure DbgListAddOrUpdate ( Var ATabIdx : Integer; Var ALineIdx : Integer );

    Procedure LineChange ( ADbgInfoLine : TDbgInfoLine );
    Procedure SetDbgLine ( ADbgInfoLine : TDbgInfoLine );
    Procedure BreakChange;
    Procedure BreakListCollect;
    Procedure UpdateBreakList;
    Function  SelectBreakS ( Const AFullName : string ) : string;
    Procedure CreateIndex;
    Procedure ViewList;
    Procedure UpdateList;
    Procedure PresetBreakListA;

    Procedure CorrectSizes;
    Procedure RegsListClear;
    Procedure RegsListCreate ( Const ACoreList : string );
    Procedure CoreSetRegsState ( AActive : boolean; Const AMcuType : string; Const ARegsPrevThis : string );

    Function FormatNice ( Const AType : string; AOffs : Cardinal; Const ADataBin : string ) : string;
    Function FormatNice ( Const AName, AType : string; AOffs : Cardinal; Const ADataBin : string ) : string;
    Function ParsRecord ( Const AType : string; AOffsBase : Cardinal; Const ADataBin : string ) : string;
    Function ParsArray ( AVsmItem : TVsmItem; Const AType : string; AOffsBase : Cardinal; Const ADataBin : string ) : string;
    Function TryDetermineStackSize ( ADbgAsmLine : Integer ) : Integer;
    Procedure UpdateWatch ( AEip, AEsp : Cardinal; Const ADataBin : string );
    Function ExtractDbgDataReg ( AVsmItem : TVsmItem; Out ARdblLoc, ARdblVal : string ) : boolean;
    Function ExtractDbgDataStack ( AEip, AEsp : Cardinal; Const ADataBin : string; AVsmItem : TVsmItem; Out ARdblLoc, ARdblVal : string ) : boolean;
    Procedure ViewStackData ( AEip, AEsp : Cardinal; Const AStackDataS : string );

    Function IpToLine ( AAddr : Cardinal ) : Integer;
    Procedure SetLineIp;
    Procedure HighlightIp;

    Procedure DbgBtnClick ( ACmd : char );
    Procedure SetBtnEn;

    Procedure ProcessEtb ( Const AEtbS : string ); // Which CPU core caused stop. Memorized while Stat is received. Clears to Zero when NextReg is performed

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TWinControl; ADbgOnAsmLineChange : TDbgOnAsmLineChange; AOnViewAny : TOnViewAny; AOnProcAny : TOnProcAny; ADbgBtnClick : TOnDbgBtnClick );
    Procedure Done;

    Procedure PresetBreakList ( Const ABreakList : string );
    Procedure DbgSrcLineChange ( Const AFilename : string; ASrcLineIdx : Integer );
    Function IsEmpty : boolean;
    Function GetEipReal ( Out AAddr : Cardinal ) : boolean;
    Function GetEipReal ( Out AAddr : Cardinal; Out ACore : char; Out ACoreIdx : Integer ) : boolean;
    Function GetEspReal ( Out AAddr : Cardinal ) : boolean;

    Procedure CloseAll;
    Procedure ProcessAny ( Const ADataS : string );

    property Active : boolean read FActive write FActive;
    property IpToLineA : TIpToLine read FIpToLineA write FIpToLineA;
    property IpAtLine : Cardinal read FIpAtLine;
    property WndVars : TWndVarsSd read FWndVars write FWndVars;
  end;

implementation

Uses
  Math, ConComL, ConComS, ParsHelper_sd;

{$R *.lfm}

Constructor TMgDebug.Create ( AOwner : TComponent );
Begin
 Inherited;
 FLst:=TStringList.Create;
End;

Destructor TMgDebug.Destroy;
Begin
 FLineToIp:=nil;
 FEspListVirt:=nil; FEipListVirt:=nil;
 FEspListReal:=nil; FEipListReal:=nil;
 DbgListClear;
 FLst.Free;
 Inherited;
End;

Procedure TMgDebug.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage);
End;

Procedure TMgDebug.ProcAny ( Const AProcCmd : string );
Begin
 if Assigned(FOnProcAny) then FOnProcAny(AProcCmd);
End;

Function TMgDebug.IpToLine ( AAddr : Cardinal ) : Integer;
Begin
 if Assigned(FIpToLineA) then Result:=FIpToLineA(AAddr) else Result:=-1;
End;

Procedure TMgDebug.PcCoreFrameChange(Sender: TObject);
Begin
 SetLineIp;
End;

Procedure TMgDebug.DbgBtnClick ( ACmd : char );
Begin
 if Assigned(FDbgBtnClick) then FDbgBtnClick(ACmd);
End;

procedure TMgDebug.BtStartClick(Sender: TObject);
begin
 DbgBtnClick('S');
end;

procedure TMgDebug.BtStepBackClick(Sender: TObject);
begin
 DbgBtnClick('1');
end;

procedure TMgDebug.BtStepIntoClick(Sender: TObject);
begin
 DbgBtnClick('7');
end;

procedure TMgDebug.BtStepOverClick(Sender: TObject);
begin
 DbgBtnClick('8');
end;

procedure TMgDebug.BtForceRecompileClick(Sender: TObject);
begin
 DbgBtnClick('6');
end;

procedure TMgDebug.BtPauseClick(Sender: TObject);
begin
 DbgBtnClick('P');
end;

procedure TMgDebug.BtResetClick(Sender: TObject);
begin
 DbgBtnClick('R');
end;

procedure TMgDebug.BtRunToCarClick(Sender: TObject);
begin
 DbgBtnClick('4');
end;

Procedure FixBtnPosA ( ABtnPrev : TSpeedButton; ADelta : Integer; ABtnThis : TSpeedButton );
Var
  BLeft     : Integer;
Begin
 ABtnThis.Top:=2;
 ABtnThis.Height:=20; ABtnThis.Width:=20;
 if ABtnPrev=nil then BLeft:=0
 else BLeft:=ABtnPrev.Left+ABtnPrev.Width;
 ABtnThis.Left:=BLeft+ADelta;
End;

Procedure TMgDebug.Init ( AParent : TWinControl; ADbgOnAsmLineChange : TDbgOnAsmLineChange; AOnViewAny : TOnViewAny; AOnProcAny : TOnProcAny; ADbgBtnClick : TOnDbgBtnClick );
Begin
 FDbgOnAsmLineChange:=ADbgOnAsmLineChange;
 FOnViewAny:=AOnViewAny; FOnProcAny:=AOnProcAny;
 FDbgBtnClick:=ADbgBtnClick;
 FParent:=AParent;
 FParent.InsertControl(Self);
 PcEdit.Align:=alClient;

 PnlRegs.BevelOuter:=bvNone;
 FProgrBar:=TOzhProgrBar.Create(Self); PnlRegs.InsertControl(FProgrBar); FProgrBar.Align:=alBottom; FProgrBar.Visible:=TRUE;
 FPnlHw:=TDbgHwPanel.Create(Self); PnlRegs.InsertControl(FPnlHw); FPnlHw.Align:=alBottom;
 FDbgStack:=TDbgViewStack.Create(Self); PnlRegs.InsertControl(FDbgStack); FDbgStack.Align:=alClient;
 PnlBtn.Top:=0; PnlBtn.Align:=alBottom;

 FixBtnPosA(nil,2,BtStart);
 FixBtnPosA(BtStart,0,BtPause);
 FixBtnPosA(BtPause,0,BtReset);
 FixBtnPosA(BtReset,8,BtStepBack);
 FixBtnPosA(BtStepBack,0,BtStepInto);
 FixBtnPosA(BtStepInto,0,BtStepOver);
 FixBtnPosA(BtStepOver,0,BtRunToCar);
 FixBtnPosA(BtRunToCar,8,BtForceRecompile);
 FixBtnPosA(BtForceRecompile,4,BtnCoverage);

 PnlBtn.ClientHeight:=BtStart.Height+2;

 if IsEmpty then
 else UpdateBreakList;
End;

Procedure TMgDebug.Done;
Begin
 DbgListClear;
 RegsListClear;
 PnlRegs.RemoveControl(FProgrBar); FProgrBar.Free;
 PnlRegs.RemoveControl(FDbgStack); FDbgStack.Free;
 PnlRegs.RemoveControl(FPnlHw); FPnlHw.Free;
 FParent.RemoveControl(Self);
End;

Procedure TMgDebug.CloseAll;
Begin
 DbgListClear;
End;

Function TMgDebug.GetDbgView ( AIndex : Integer ) : TDbgViewText;
Var
  BTabSheet     : TTabSheet;
Begin
 BTabSheet:=PcEdit.Pages[AIndex];
 Result:=TDbgViewText(BTabSheet.Controls[0]);
End;

Procedure TMgDebug.DbgListClear;
Var
  BIndex        : Integer;
  BTabSheet     : TTabSheet;
  BView         : TDbgViewText;
Begin
 BIndex:=0;
 while BIndex<PcEdit.PageCount do
  begin
  BView:=GetDbgView(BIndex);
  BView.Parent.RemoveControl(BView);
  BView.Free;
  inc(BIndex);
  end;
 while PcEdit.PageCount<>0 do
  begin
  BTabSheet:=PcEdit.Pages[0];
  BTabSheet.Free;
  end;
End;

Procedure TMgDebug.DbgListAddOrUpdate ( ATabIdx : Integer; Const AAsmName, ASrcName : string; ALineStart, ALineEnd : Integer; AIsLst : boolean );
Var
  BTabSheet     : TTabSheet;
  BView         : TDbgViewText;
  BExt          : string;
  BCpuType      : char;
  BImageIdx     : Integer;
Begin
 BExt:=LowerCase(ExtractFileExt(AAsmName));
 if StrInList(BExt,'.srv .s .i .hex .elf') then BCpuType:='e'
 else BCpuType:='s';

 BImageIdx:=0;
 if StrInList(BExt,'.asm') then BImageIdx:=1
 else if StrInList(BExt,'.pas') then BImageIdx:=2
 else if StrInList(BExt,'.c') then BImageIdx:=3
 else if StrInList(BExt,'.py') then BImageIdx:=4
 else if StrInList(BExt,'.rst') then BImageIdx:=5
 else if StrInList(BExt,'.v') then BImageIdx:=6
 else if StrInList(BExt,'.srv .s .i') then BImageIdx:=7
 else if StrInList(BExt,'.hex') then BImageIdx:=8
 else if StrInList(BExt,'.elf') then BImageIdx:=9;

 BView:=nil;
 if ATabIdx<PcEdit.PageCount then
  begin
  BTabSheet:=PcEdit.Pages[ATabIdx];
  BView:=GetDbgView(ATabIdx);
  if BView.CpuType<>BCpuType then begin BView.Parent.RemoveControl(BView); BView.Free; BView:=nil; end;
  end
 else
  begin
  BTabSheet:=TTabSheet.Create(PcEdit);
  BTabSheet.PageControl:=PcEdit;
  BTabSheet.Caption:=ExtractFilename(AAsmName);
  end;

 if BTabSheet.ImageIndex<>BImageIdx then BTabSheet.ImageIndex:=BImageIdx;

 if BView=nil then
  begin
  if BCpuType='e' then BView:=TDbgViewTextRv.Create(Self)
  else BView:=TDbgViewTextMs.Create(Self);
  BView.Visible:=FALSE;
  BView.Init(BTabSheet,@LineChange,@BreakChange,@SetDbgLine);
  BView.SetList(AAsmName,FLst,ALineStart,ALineEnd,SelectBreakS(AAsmName),AIsLst);
  BView.Align:=alClient;
  BView.Visible:=TRUE;
  end
 else
  begin
  BView.SetList(AAsmName,FLst,ALineStart,ALineEnd,SelectBreakS(AAsmName),AIsLst);
  end;
End;

Procedure TMgDebug.DbgListAddOrUpdate ( Var ATabIdx : Integer; Var ALineIdx : Integer );
Var
  BIndexS       : Integer;
  BReadS        : string;
  BAsmName,
  BSrcName      : string;
Begin
 BReadS:=FLst.Strings[ALineIdx];
 Delete(BReadS,1,3); DelFirstSpace(BReadS); DelLastSpace(BReadS);
 BAsmName:=ReadParamStr(BReadS); BSrcName:=ReadParamStr(BReadS);
 BIndexS:=ALineIdx;
 inc(ALineIdx);
 while ALineIdx<FLst.Count do
  begin
  BReadS:=FLst.Strings[ALineIdx];
  if Copy(BReadS,1,3)=';@A' then break;
  inc(ALineIdx);
  end;
 if BAsmName<>'' then
  begin
  DbgListAddOrUpdate(ATabIdx,BAsmName,BSrcName,BIndexS,ALineIdx,FALSE);
  inc(ATabIdx);
  end;
End;

Procedure TMgDebug.DbgSrcLineChange ( Const AFilename : string; ASrcLineIdx : Integer );
Var
  BIndex        : Integer;
  BView         : TDbgViewText;
  BDbgInfo      : TDbgInfoFile;
  BSrcName      : string;
Begin
 BIndex:=0;
 while BIndex<PcEdit.PageCount do
  begin
  BView:=GetDbgView(BIndex);
  BDbgInfo:=BView.DbgFile;
  if BDbgInfo.SrcList.Count=0 then BSrcName:=''
  else BSrcName:=BDbgInfo.SrcList.Strings[0];
  if (BSrcName<>'') and (BDbgInfo.SrcList.Count>0) and (BDbgInfo.SrcList.Strings[0]=AFilename) then
   begin
   BView.SetDbgHighlight(AFilename,ASrcLineIdx,-1);
   if PcEdit.ActivePage<>BView.TabSheet then PcEdit.ActivePage:=BView.TabSheet;
   end
  else if (BDbgInfo.AsmName.Count>0) and (BDbgInfo.AsmName.Strings[0]=AFilename) then
   begin
   BView.SetDbgHighlight(AFilename,-1,ASrcLineIdx);
   if PcEdit.ActivePage<>BView.TabSheet then PcEdit.ActivePage:=BView.TabSheet;
   end
  else
   begin
   BView.SetDbgHighlight('',-1,-1);
   end;
  inc(BIndex);
  end;
End;

Procedure TMgDebug.LineChange ( ADbgInfoLine : TDbgInfoLine );
Var
  BLineIndex    : Integer;
Begin
 BLineIndex:=ADbgInfoLine.LineIdx;
 if BLineIndex<0 then BLineIndex:=0;
 if BLineIndex>=Length(FLineToIp) then BLineIndex:=Length(FLineToIp)-1;
 FIpAtLine:=FLineToIp[BLineIndex].FIp;
End;

Procedure TMgDebug.SetDbgLine ( ADbgInfoLine : TDbgInfoLine );
Begin
 if Assigned(FDbgOnAsmLineChange) then FDbgOnAsmLineChange(ADbgInfoLine);
End;

Function TMgDebug.SelectBreakS ( Const AFullName : string ) : string;
Var
  BNameS,
  BBreakS       : string;
  BBreakList    : string;
Begin
 Result:='';
 BBreakList:=FBreakList;
 while BBreakList<>'' do
  begin
  BNameS:=ReadParamStr(BBreakList);
  BBreakS:=ReadParamStr(BBreakList);
  if BBreakS='' then break;
  if LowerCase(BNameS)=LowerCase(RelFilename(FPrjPath,AFullName)) then
   begin
   if Result<>'' then Result:=Result+' ';
   Result:=Result+BBreakS;
   end;
  end;
End;

Procedure TMgDebug.BreakChange;
Begin
 BreakListCollect;
 UpdateBreakList;
 if Assigned(FOnBreakChange) then FOnBreakChange(FBreakList);
 FBreakReady:=TRUE;
End;

Procedure TMgDebug.BreakListCollect;
Var
  BViewIdx,
  BBreakIdx     : Integer;
  BView         : TDbgViewText;
Begin
 FBreakList:='';
 BViewIdx:=0;
 while BViewIdx<PcEdit.PageCount do
  begin
  BView:=GetDbgView(BViewIdx);
  for BBreakIdx:=0 to Length(BView.BreakList)-1 do FBreakList:=FBreakList+RelFilename(FPrjPath,BView.FullName)+' '+IntToStr(BView.BreakList[BBreakIdx])+' ';
  inc(BViewIdx);
  end;
End;

Procedure TMgDebug.UpdateBreakList;
Var
  BView         : TDbgViewText;
  BIndex        : Integer;
  BBreakIdxV    : Integer;
  BIp           : Cardinal;
  BBreakList    : string;
Begin
 BBreakList:='';
 BIndex:=0;
 while BIndex<PcEdit.PageCount do
  begin
  BView:=GetDbgView(BIndex);
  BBreakIdxV:=0;
  while BBreakIdxV<Length(BView.BreakList) do
   begin
   BIp:=FLineToIp[BView.BreakList[BBreakIdxV]].FIp;
   if BIp<>$FFFFFFFF then BBreakList:=BBreakList+#32+IntToHex(BIp,8);
   inc(BBreakIdxV);
   end;
  inc(BIndex);
  end;

 ViewAny('b'+BBreakList);
End;

Procedure TMgDebug.CreateIndex;
Var
  BTypeS,
  BAddrS,
  BDataS,
  BOrigS        : string;
  BIndex,
  BIndexA       : Integer;
  BAddr         : Cardinal;
  BIpCmd        : TIpCmd;
  BCmdLen,
  BCmdIdx       : Integer;
  BSegType      : char;
Begin
 SetLength(FLineToIp,FLst.Count);

 BIndex:=0;
 while BIndex<FLst.Count do
  begin
  FLineToIp[BIndex].FCmdLen:=0;
  inc(BIndex);
  end;

 BIndexA:=0;
 BIndex:=0;
 while BIndex<FLst.Count do
  begin
  BAddrS:=''; BDataS:=''; BOrigS:='';
  SplitLineLst(FLst.Strings[BIndex],BTypeS,BAddrS,BDataS,BOrigS,BSegType);
  if (BAddrS<>'') and (BSegType='c') then
   begin
   BAddr:=0;
   if Hex32ToInt(BAddrS,BAddr) then
    begin
    BIpCmd.FIp:=BAddr;
    BCmdLen:=Length(BDataS) div 2;
    BIpCmd.FCmdLen:=BCmdLen;
    if BCmdLen>12 then BCmdLen:=12;
    BCmdIdx:=0;
    while BCmdIdx<BCmdLen do begin BIpCmd.FCmd[BCmdIdx]:=Hex32ToInt(Copy(BDataS,1+(BCmdIdx*2),2)); inc(BCmdIdx); end;
    while BIndexA<=BIndex do begin FLineToIp[BIndexA]:=BIpCmd; inc(BIndexA); end;
    end;
   end;
  inc(BIndex);
  end;
End;

Procedure TMgDebug.ViewList;
Begin
 FDbgAsmLine:=-1;
 FDbgFile:=nil;
 FDbgProc:=nil;
 FDbgLine:=nil;
 DbgListClear;
 FAddrBase:=0;

 // Create file list
 DbgListAddOrUpdate(0,FLstName,FLstName,0,FLst.Count,TRUE);

 UpdateList;

 PresetBreakListA;
 BreakChange;
End;

Procedure TMgDebug.UpdateList;
Var
  BLineIdx,
  BDbgIdx       : Integer;
  BReadS        : string;
Begin
 CreateIndex;

 BDbgIdx:=1;
 BLineIdx:=0;
 while BLineIdx<FLst.Count do
  begin
  BReadS:=FLst.Strings[BLineIdx];
  if Pos(';@A',BReadS)=1 then DbgListAddOrUpdate(BDbgIdx,BLineIdx)
  else inc(BLineIdx);
  end;
End;

Function TMgDebug.IsEmpty : boolean;
Begin
 Result:=(FLst.Count=0) or (Length(FLineToIp)=0) or (PcEdit.PageCount=0);
End;

Procedure TMgDebug.CorrectSizes;
Var
  BDbgRegs      : TDbgRegs;
  BDeltaX,
  BDeltaY       : Integer;
Begin
 repeat
 if Length(FDbgRegsList)=0 then break;
 BDbgRegs:=FDbgRegsList[0];
 BDeltaX:=BDbgRegs.GetViewDeltaX;
 BDeltaY:=BDbgRegs.GetViewDeltaY;
 if BDeltaX<>0 then PnlRegs.Width:=PnlRegs.Width+BDeltaX;
 if BDeltaY<>0 then PcCoreFrame.Height:=PcCoreFrame.Height+BDeltaY;
 until TRUE;
End;

Procedure TMgDebug.RegsListClear;
Var
  BRegsIdx      : Integer;
  BTabSheet     : TTabSheet;
  BDbgRegs      : TDbgRegs;
Begin
 BRegsIdx:=Length(FDbgRegsList)-1;
 while BRegsIdx>=0 do
  begin
  BTabSheet:=PcCoreFrame.Pages[BRegsIdx];
  BDbgRegs:=FDbgRegsList[BRegsIdx];
  BDbgRegs.Done;
  BTabSheet.Free;
  BDbgRegs.Free;
  dec(BRegsIdx);
  end;
 FDbgRegsList:=nil;
End;

Procedure TMgDebug.RegsListCreate ( Const ACoreList : string );
Var
  BCoreIdx      : Integer;
  BTabSheet     : TTabSheet;
  BDbgRegs      : TDbgRegs;
Begin
 RegsListClear;
 SetLength(FDbgRegsList,Length(ACoreList));
 SetLength(FEipListVirt,Length(ACoreList));
 SetLength(FEspListVirt,Length(ACoreList));
 SetLength(FEipListReal,Length(ACoreList));
 SetLength(FEspListReal,Length(ACoreList));
 BCoreIdx:=0;
 while BCoreIdx<Length(FDbgRegsList) do
  begin
  BTabSheet:=TTabSheet.Create(PcCoreFrame); BTabSheet.PageControl:=PcCoreFrame;
  BDbgRegs:=TDbgRegs.Create(Self); FDbgRegsList[BCoreIdx]:=BDbgRegs;
  BDbgRegs.Init(BTabSheet,ACoreList[1+BCoreIdx],BCoreIdx,@CorrectSizes);
  inc(BCoreIdx);
  end;
 CorrectSizes;
End;

Procedure TMgDebug.CoreSetRegsState ( AActive : boolean; Const AMcuType : string; Const ARegsPrevThis : string );
Var
  BMcuType      : Cardinal;
  BRegsPrevThis : string;
  BCoreIdx      : Integer;
  BDbgRegs      : TDbgRegs;
  BPrev,
  BThis,
  BComb         : string;
  BPrevA,
  BThisA,
  BCombA        : string;
  //BMcxStat      : QWord;
  //BBreakTrapIdx : word;
  BFlIp         : Cardinal;
  BEipVirt,
  BEspVirt      : Cardinal;
  BEipReal,
  BEspReal      : Cardinal;
  BMpuRegs      : TMpuRegs;
Begin
 HexToDWordCheck(AMcuType,BMcuType);
 BRegsPrevThis:=ARegsPrevThis;
 FExecActive:=AActive; SetBtnEn;
 BPrev:=ReadTillC(BRegsPrevThis,#13); BThis:=ReadTillC(BRegsPrevThis,#13); BComb:=ReadTillC(BRegsPrevThis,#13);
 FRegsThis:=BThis;

 repeat
 BCoreIdx:=0;
 while BCoreIdx<Length(FDbgRegsList) do
  begin
  BDbgRegs:=FDbgRegsList[BCoreIdx];
  BPrevA:=ReadParamStr(BPrev);
  BThisA:=ReadParamStr(BThis);
  BCombA:=ReadParamStr(BComb);
  BFlIp:=Hex32ToInt(Copy(BThisA,9,8)); BEipVirt:=BFlIp and $00FFFFFE;
  BEspVirt:=$0;
  case ((BFlIp shr 28) and $3) of
    0: BEspVirt:=Hex32ToInt(Copy(BThisA,41,8));
    1: BEspVirt:=Hex32ToInt(Copy(BThisA,1,8));
  end;
  if BMcuType=9 then
   begin
   BMpuRegs:=LoadMpuRegs(Copy(BThisA,1+128,64));
   BEipReal:=UnmapMpuAddrCode(BMpuRegs,BEipVirt);
   BEspReal:=UnmapMpuAddrData(BMpuRegs,BEspVirt);
   end
  else
   begin
   BEipReal:=BEipVirt;
   BEspReal:=BEspVirt;
   end;
  FEipListVirt[BCoreIdx]:=BEipVirt;
  FEspListVirt[BCoreIdx]:=BEspVirt;
  FEipListReal[BCoreIdx]:=BEipReal;
  FEspListReal[BCoreIdx]:=BEspReal;
  BDbgRegs.SetState(AActive,BMcuType,BPrevA,BThisA,BCombA);
  inc(BCoreIdx);
  end;
 {
 if BThis='' then break;
 if HexToQWordCheck(BThis,BMcxStat)=FALSE then break;
 BBreakTrapIdx:=BMcxStat shr 16;
 BCoreIdx:=0;
 while BCoreIdx<Length(FDbgRegsList) do
  begin
  if (BBreakTrapIdx and ($0101 shl BCoreIdx))<>0 then break;
  inc(BCoreIdx);
  end;
 if (BCoreIdx<Length(FDbgRegsList)) and (PcCoreFrame.ActivePageIndex<>BCoreIdx) then
  begin
  FCoreIdx:=BCoreIdx;
  PcCoreFrame.ActivePageIndex:=BCoreIdx;
  end;
 }
 BCoreIdx:=FActIdx; FActIdx:=-1;
 if BCoreIdx<>-1 then
  begin
  if (BCoreIdx<Length(FDbgRegsList)) and (PcCoreFrame.ActivePageIndex<>BCoreIdx) then
   begin
   FCoreIdx:=BCoreIdx;
   PcCoreFrame.ActivePageIndex:=BCoreIdx;
   end;
  end;
 until TRUE;
End;

Function TMgDebug.GetEipReal ( Out AAddr : Cardinal ) : boolean;
Begin
 AAddr:=$FFFFFFFF;
 Result:=FALSE;
 repeat
 if (PcCoreFrame.TabIndex<0) or (PcCoreFrame.TabIndex>=Length(FDbgRegsList)) then break;
 AAddr:=FEipListReal[PcCoreFrame.TabIndex];
 Result:=TRUE;
 until TRUE;
End;

Function TMgDebug.GetEipReal ( Out AAddr : Cardinal; Out ACore : char; Out ACoreIdx : Integer ) : boolean;
Begin
 AAddr:=$FFFFFFFF; ACore:=#0; ACoreIdx:=-1;
 Result:=FALSE;
 repeat
 if (PcCoreFrame.TabIndex<0) or (PcCoreFrame.TabIndex>=Length(FDbgRegsList)) then break;
 AAddr:=FEipListReal[PcCoreFrame.TabIndex];
 ACore:=FDbgRegsList[PcCoreFrame.TabIndex].CoreType;
 ACoreIdx:=PcCoreFrame.TabIndex;
 Result:=TRUE;
 until TRUE;
End;

Function TMgDebug.GetEspReal ( Out AAddr : Cardinal ) : boolean;
Begin
 AAddr:=$FFFFFFFF;
 Result:=FALSE;
 repeat
 if (PcCoreFrame.TabIndex<0) or (PcCoreFrame.TabIndex>=Length(FDbgRegsList)) then break;
 AAddr:=FEspListReal[PcCoreFrame.TabIndex];
 Result:=TRUE;
 until TRUE;
End;

Function TMgDebug.TryDetermineStackSize ( ADbgAsmLine : Integer ) : Integer;
Var
  BDbgProc  : TDbgInfoProc;
  BDbgLine  : TDbgInfoLine;
Begin
 Result:=128;
 repeat
 BDbgLine:=FDbgFile.GetLine(ADbgAsmLine);
 if BDbgLine=nil then break;
 BDbgProc:=BDbgLine.DbgProc;
 if BDbgProc=nil then break;
 Result:=BDbgProc.OffsWorst;
 if BDbgLine.StackOffs>0 then Result:=Result+BDbgLine.StackOffs;
 if Result<128 then Result:=128;
 until TRUE;
End;

Procedure TMgDebug.SetLineIp;
Var
  BDbgIndex,
  BDbgIndexA    : Integer;
  BDbgView      : TDbgViewText;
  BEsp          : Cardinal;
  BStackSize    : Integer;
Begin
 FDbgAsmLine:=-1;
 FDbgFile:=nil;
 FDbgProc:=nil;
 FDbgLine:=nil;
 repeat
 if FExecActive or (FRegsThis='') then
  begin
  BDbgIndex:=0;
  while BDbgIndex<PcEdit.PageCount do
   begin
   BDbgView:=GetDbgView(BDbgIndex);
   BDbgView.SetLineIp(-1);
   inc(BDbgIndex);
   end;
  break;
  end;
 if GetEipReal(FEip)=FALSE then break;
 FDbgAsmLine:=IpToLine(FEip); if FDbgAsmLine<0 then break;
 if PcEdit.PageCount=0 then break;

 BDbgIndexA:=0;
 BDbgIndex:=1;
 while BDbgIndex<PcEdit.PageCount do
  begin
  BDbgView:=GetDbgView(BDbgIndex);
  if BDbgView.IsInside(FDbgAsmLine) then BDbgIndexA:=BDbgIndex
  else begin BDbgView.SetLineIp(-1); BDbgView.SetLineHighlight(-1); end;
  inc(BDbgIndex);
  end;
 BDbgView:=GetDbgView(0);
 BDbgView.SetLineIp(FDbgAsmLine);
 PcEdit.PageIndex:=BDbgIndexA;
 GetDbgView(BDbgIndexA).SetFocus;
 if BDbgIndexA<>0 then
  begin
  BDbgView:=GetDbgView(BDbgIndexA);
  BDbgView.SetLineIp(FDbgAsmLine);
  FDbgFile:=BDbgView.DbgFile;
  end;
 if FDbgFile<>nil then
  begin
  //UpdateWatch;
  SetDbgLine(FDbgFile.GetLine(FDbgAsmLine));
  end;

 if GetEspReal(BEsp) then
  begin
  if (FExecActive=FALSE) and (BEsp>0) then BStackSize:=TryDetermineStackSize(FDbgAsmLine)
  else BStackSize:=0;
  ProcAny('o'+IntToHex(FEip,8)+' '+IntToHex(BEsp,8)+' '+IntToHex(BStackSize,8));
  end;
 until TRUE;
End;

Procedure TMgDebug.HighlightIp;
Var
  BAddr         : Cardinal;
  BDbgIndex,
  BDbgIndexA    : Integer;
  BDbgView      : TDbgViewText;
Begin
 FDbgAsmLine:=-1;
 FDbgFile:=nil;
 FDbgProc:=nil;
 FDbgLine:=nil;
 repeat
 for BDbgIndex:=0 to PcEdit.PageCount-1 do GetDbgView(BDbgIndex).SetLineIp(-1);
 if GetEipReal(BAddr)=FALSE then break;
 FDbgAsmLine:=IpToLine(BAddr); if FDbgAsmLine<0 then break;
 if PcEdit.PageCount=0 then break;
 BDbgIndexA:=0;
 BDbgIndex:=1;
 while BDbgIndex<PcEdit.PageCount do
  begin
  if GetDbgView(BDbgIndex).IsInside(FDbgAsmLine) then BDbgIndexA:=BDbgIndex;
  inc(BDbgIndex);
  end;
 GetDbgView(0).SetLineHighlight(FDbgAsmLine);
 PcEdit.PageIndex:=BDbgIndexA;
 GetDbgView(BDbgIndexA).SetFocus;
 if BDbgIndexA<>0 then
  begin
  BDbgView:=GetDbgView(BDbgIndexA);
  BDbgView.SetLineHighlight(FDbgAsmLine);
  FDbgFile:=BDbgView.DbgFile;
  end;
 if FDbgFile<>nil then
  begin
  //UpdateWatch;
  SetDbgLine(FDbgFile.GetLine(FDbgAsmLine));
  end;

 until TRUE;
End;

Procedure TMgDebug.UpdateWatch ( AEip, AEsp : Cardinal; Const ADataBin : string );
Var
  BList         : TStringList;
  BVsmItem      : TVsmItem;
  BVsmIdx       : Integer;
  BDataS        : string;
  BTarg         : string;
  BLocation,
  BValue        : string;
Begin
 BList:=TStringList.Create;

 repeat
 FDbgProc:=nil;
 if FDbgFile=nil then break;

 FDbgProc:=FDbgFile.ProcByAsmLineIdx(FDbgAsmLine);
 FDbgLine:=FDbgFile.GetLine(FDbgAsmLine);

 if FDbgProc=nil then break;

 for BVsmIdx:=0 to Length(FDbgProc.VsmList)-1 do
  begin
  BVsmItem:=FDbgProc.VsmList[BVsmIdx];
  BTarg:=BVsmItem.VarName;
  BDataS:=BTarg;

  if ExtractDbgDataReg(BVsmItem,BLocation,BValue) then BDataS:=BDataS+' '+BLocation+'#'+BValue
  else if ExtractDbgDataStack(AEip,AEsp,ADataBin,BVsmItem,BLocation,BValue) then BDataS:=BDataS+' '+BLocation+'#'+BValue;
  BList.Append(BDataS);
  end;

 until TRUE;

 if FWndVars<>nil then FWndVars.SetData(FDbgProc,BList);

 BList.Free;
End;

Function TMgDebug.ExtractDbgDataReg ( AVsmItem : TVsmItem; Out ARdblLoc, ARdblVal : string ) : boolean;
Var
  BTargIdx      : Integer;
  BTarg         : string;
  BRegName      : string;
  BRow          : char;
  BRegOffs,
  BRegSize      : Integer;
  BDummyS       : string;
  BDataH        : string;
  BData         : QWord;
  BType         : string;
  BDataSI       : Cardinal;
  BDataDI       : QWord;
  BDataSF       : single absolute BDataSI;
  BDataDF       : double absolute BDataDI;
  BRegsThis     : string;
  BCoreIdx      : Integer;
Begin
 Result:=FALSE; ARdblLoc:=''; ARdblVal:='';
 repeat
 if FDbgLine.RegMap.Count<>FDbgProc.ChTargList.Count then break;
 BRegName:='';
 BTargIdx:=0;
 while BTargIdx<FDbgProc.ChTargList.Count do
  begin
  repeat
  BTarg:=FDbgProc.ChTargList.Strings[BTargIdx];
  if BTarg<>AVsmItem.VarName then break;
  BRegName:=FDbgLine.RegMap.Strings[BTargIdx];
  if BRegName='.' then BRegName:='';
  until TRUE;
  if BRegName<>'' then break;
  inc(BTargIdx);
  end;
 if BRegName<>'' then
  begin
  BRow:=BRegName[1];
  if not (BRow in ['a'..'g']) then break;
  BDummyS:=BRegName; Delete(BDummyS,1,1);
  BRegOffs:=(1+Ord(BRow)-Ord('a'))*16; BRegSize:=0;
  if BDummyS='q' then begin BRegOffs:=BRegOffs+0; BRegSize:=16; end
  else if BDummyS='r' then begin BRegOffs:=BRegOffs+0; BRegSize:=8; end
  else if BDummyS='wx' then begin BRegOffs:=BRegOffs+8; BRegSize:=8; end
  else if BDummyS='w' then begin BRegOffs:=BRegOffs+8; BRegSize:=4; end
  else if BDummyS='x' then begin BRegOffs:=BRegOffs+12; BRegSize:=4; end
  else if BDummyS='h' then begin BRegOffs:=BRegOffs+12; BRegSize:=2; end
  else if BDummyS='l' then begin BRegOffs:=BRegOffs+14; BRegSize:=2; end
  else break;
  BRegsThis:=FRegsThis; BCoreIdx:=0; while BCoreIdx<FCoreIdx do begin ReadParamStr(BCoreIdx); Inc(BCoreIdx); end;
  BDataH:=Copy(BRegsThis,1+BRegOffs,BRegSize);
  if HexToQWordCheck(BDataH,BData)=FALSE then break;
  ARdblLoc:=BRegName;
  BType:=ParsExtractType(AVsmItem.VarName);
  if (BType='p') or (BType='q') then ARdblVal:='0x'+IntToHex(BData,8)+'->'
  else if BType='f' then
   begin
   if  BRegSize=16 then
    begin
    BDataDI:=BData;
    if IsNan(BDataDF) then ARdblVal:='NaN'
    else ARdblVal:=FloatToStr(BDataDF);
    end
   else
    begin
    BDataSI:=BData;
    if IsNan(BDataSF) then ARdblVal:='NaN'
    else ARdblVal:=FloatToStr(BDataSF);
    end;
   end
  else ARdblVal:=IntToStr(BData);
  Result:=TRUE;
  end;
 until TRUE;
End;

Function BinToQWord ( Const ADataBin : string ) : QWord;
Var
  BIndex    : Integer;
Begin
 Result:=0;
 BIndex:=Length(ADataBin);
 while BIndex>0 do begin Result:=Result*256+Ord(ADataBin[BIndex]); Dec(BIndex); end;
End;

Function TMgDebug.FormatNice ( Const AType : string; AOffs : Cardinal; Const ADataBin : string ) : string;
Var
  BDataC    : Cardinal;
  BDataF    : ^Single;
  BLenMax,
  BLenBin,
  BLenThis  : Integer;
  BIndex    : Integer;
  BChar     : Char;
Begin
 Result:='?';
 repeat
 if AType='l' then
  begin
  if BinToQWord(Copy(ADataBin,1+AOffs,1))=0 then Result:='False'
  else Result:='True';
  break;
  end;

 if AType='b' then
  begin
  Result:=IntToStr(Byte(BinToQWord(Copy(ADataBin,1+AOffs,1))));
  break;
  end;

 if AType='k' then
  begin
  Result:=IntToStr(ShortInt(BinToQWord(Copy(ADataBin,1+AOffs,1))));
  break;
  end;

 if AType='w' then
  begin
  Result:=IntToStr(Word(BinToQWord(Copy(ADataBin,1+AOffs,2))));
  break;
  end;

 if AType='m' then
  begin
  Result:=IntToStr(SmallInt(BinToQWord(Copy(ADataBin,1+AOffs,2))));
  break;
  end;

 if AType='i' then
  begin
  Result:=IntToStr(Integer(BinToQWord(Copy(ADataBin,1+AOffs,4))));
  break;
  end;

 if AType='d' then
  begin
  Result:=IntToStr(Cardinal(BinToQWord(Copy(ADataBin,1+AOffs,4))));
  break;
  end;

 if AType='f' then
  begin
  BDataC:=Cardinal(BinToQWord(Copy(ADataBin,1+AOffs,4)));
  BDataF:=@BDataC;
  if IsNan(BDataF^) then Result:='NaN'
  else Result:=FloatToStr(BDataF^);
  break;
  end;

 if ParsIsTypeStringP(AType) then
  begin
  BLenBin:=Length(ADataBin)-1-AOffs;
  if BLenBin<=0 then break;
  BLenMax:=ParsGetStrPTypeLen(AType); if BLenMax=-1 then break;
  BLenThis:=Ord(ADataBin[1+AOffs]);
  if BLenBin<BLenThis then BLenThis:=BLenBin;
  if BLenMax<BLenThis then BLenThis:=BLenMax;
  Result:='';
  BIndex:=0;
  while BIndex<BLenThis do
   begin
   BChar:=ADataBin[2+AOffs+BIndex];
   if BChar<#32 then Result:=Result+'<'+IntToStr(Ord(BChar))+'>'
   else Result:=Result+BChar;
   inc(BIndex);
   end;
  break;
  end;
 until TRUE;
End;

Function TMgDebug.FormatNice ( Const AName, AType : string; AOffs : Cardinal; Const ADataBin : string ) : string;
Begin
 Result:=AName+': '+FormatNice(AType,AOffs,ADataBin);
End;

Function TMgDebug.ParsRecord ( Const AType : string; AOffsBase : Cardinal; Const ADataBin : string ) : string;
Var
  BFieldList    : string;
  BType,
  BName,
  BOffsS        : string;
  BOffs         : Cardinal;
Begin
 Result:='';
 BFieldList:=ParsReadFieldList(AType);
 repeat
 ParsReadRecField(BFieldList,BType,BName,BOffsS);
 if BType='' then break;
 if TryStrToDWord(BOffsS,BOffs)=FALSE then break;
 if ParsIsTypeRecord(BType) then
  begin
  Result:=Result+' '+BName+': '+'{'+ParsRecord(BType,AOffsBase+BOffs,ADataBin)+'}';
  end
 else
  begin
  if Result<>'' then Result:=Result+'; ';
  Result:=Result+FormatNice(BName,BType,AOffsBase+BOffs,ADataBin);
  end;
 until FALSE;
End;

Function TMgDebug.ParsArray ( AVsmItem : TVsmItem; Const AType : string; AOffsBase : Cardinal; Const ADataBin : string ) : string;
Var
  BDimS,
  BDimE         : Integer;
  BElemType     : string;
  BElemSize     : Integer;
  BElemCnt,
  BElemIdx      : Integer;
  BOffs         : Cardinal;
Begin
 Result:='';
 ParsGetTypeArrayDim(AType,BDimS,BDimE,BElemType);
 repeat
 BElemCnt:=BDimE-BDimS;
 if BElemCnt<=0 then break;
 if ParsIsTypeRecord(BElemType) then BElemSize:=ParsGetRecordSize(BElemType)
 else BElemSize:=AVsmItem.Size div BElemCnt;
 if BElemSize=0 then break;
 BElemIdx:=0;
 while BElemIdx<BElemCnt do
  begin
  BOffs:=(BDimS+BElemIdx)*BElemSize;
  if Result<>'' then Result:=Result+' ';
  if ParsIsTypeRecord(BElemType) then Result:=Result+'{'+ParsRecord(BElemType,AOffsBase+BOffs,ADataBin)+'}'
  else Result:=Result+FormatNice(BElemType,AOffsBase+BOffs,ADataBin);
  inc(BElemIdx);
  end;
 until TRUE;
End;

Function TMgDebug.ExtractDbgDataStack ( AEip, AEsp : Cardinal; Const ADataBin : string; AVsmItem : TVsmItem; Out ARdblLoc, ARdblVal : string ) : boolean;
Var
  BDbgAsmLine   : Integer;
  BDbgLine      : TDbgInfoLine;
  BOffsBase     : Integer;
  BTarg         : string;
  BType         : string;
  BCanView      : boolean;
Begin
 Result:=FALSE; ARdblLoc:=''; ARdblVal:='';
 repeat
 BCanView:=AVsmItem.HasCopy;
 if BCanView=FALSE then BCanView:=(AVsmItem.IsA and (AVsmItem.IsAP=FALSE) and (AVsmItem.IsAR=FALSE) and (AVsmItem.IsAV=FALSE));
 if BCanView=FALSE then break;
 BDbgAsmLine:=IpToLine(FEip); if BDbgAsmLine<0 then break;
 BDbgLine:=FDbgFile.GetLine(BDbgAsmLine);
 if BDbgLine=nil then break;
 BOffsBase:=BDbgLine.StackOffs+AVsmItem.VarOffset;
 if BOffsBase<0 then break;
 BTarg:=AVsmItem.VarName;
 if BTarg='' then break;
 BType:=ParsExtractType(BTarg);
 ARdblLoc:='[esp+'+IntToStr(BDbgLine.StackOffs+AVsmItem.VarOffset)+'=0x'+IntToHex(AEsp+BDbgLine.StackOffs+AVsmItem.VarOffset,8)+']';
 if ParsIsTypeRecord(BType) then
  begin
  ARdblVal:=ParsRecord(BType,BOffsBase,ADataBin);
  Result:=TRUE;
  break;
  end;
 if ParsIsTypeArray(BType) then
  begin
  ARdblVal:=ParsArray(AVsmItem,BType,BOffsBase,ADataBin);
  Result:=TRUE;
  break;
  end;
 ARdblVal:=FormatNice(BType,BOffsBase,ADataBin);
 Result:=TRUE;
 until TRUE;
End;

Procedure TMgDebug.ViewStackData ( AEip, AEsp : Cardinal; Const AStackDataS : string );
Var
  BIndex,
  BLen          : Integer;
  BData         : Cardinal;
  BDataSH,
  BDataSI       : string;
  BEspS,
  BDataS        : string;
  BIndexA       : Integer;
Begin
 FDbgStack.ClearData;
 repeat
 if AStackDataS='' then break;
 BLen:=Length(AStackDataS) div 8;
 BIndex:=BLen-2;
 while BIndex>=0 do
  begin
  if BIndex=0 then BEspS:=IntToHex(AEsp,8)
  else BEspS:=AddSpacesResL('+'+IntToStr(BIndex*4),12);
  BDataS:='';
  for BIndexA:=0 to 3 do BDataS:=Copy(AStackDataS,1+BIndex*8+BIndexA*2,2)+BDataS;
  if HexToDWordCheck(BDataS,BData) then begin BDataSH:=IntToHex(BData,8); BDataSI:=IntToStr(BData); end
  else begin BDataSH:='????????'; BDataSI:='?'; end;
  FDbgStack.AppendData(BEspS,BDataSH,BDataSI);
  dec(BIndex);
  end;
 until TRUE;
 FDbgStack.InvalidateData(AEsp);
 FDbgStack.PaintA;
 UpdateWatch(AEip,AEsp,StrHexToBin(AStackDataS));
End;

Procedure TMgDebug.ProcessAny ( Const ADataS : string );
Var
  BDataS    : string;
  BParamS   : string;
  BParamA,
  BParamB   : string;
  BEip,
  BEsp      : Cardinal;
  BCoreList : string;
Begin
 repeat
 if ADataS='' then break;
 case ADataS[1] of
  'd': begin
       BDataS:=ADataS;
       Delete(BDataS,1,1);
       FPrjName:=ReadTillC(BDataS,#13); FPrjPath:=ExtractFilePath(FPrjName);
       BCoreList:=ReadTillC(BDataS,#13);
       FLstName:=ReadTillC(BDataS,#13);
       FLst.Text:=BDataS;
       RegsListCreate(BCoreList);
       ViewList;
       end;
  'D': begin
       BDataS:=ADataS;
       Delete(BDataS,1,1);
       ReadTillC(BDataS,#13);
       FLst.Text:=BDataS;
       UpdateList;
       end;
  's': begin
       BDataS:=ADataS;
       if Length(BDataS)<2 then break;
       Delete(BDataS,1,2);
       FPnlHw.SetText(BDataS);
       case ADataS[2] of
        'e': FPnlHw.SetColorA(clRed);
        '4',
        '5',
        '6': FPnlHw.SetColorA(clGreen);
        else FPnlHw.SetColorA(clGray);
       end; // case
       end;
  'p': begin
       BDataS:=ADataS;
       Delete(BDataS,1,1);
       FProgrBar.SetProgr(BDataS);
       end;
  'h': begin // Set active CPU
       BDataS:=ADataS; Delete(BDataS,1,1);
       ProcessEtb(BDataS);
       end;
  'r': begin
       BDataS:=ADataS;
       Delete(BDataS,1,1);
       BParamS:=ReadTillC(BDataS,#13);
       BParamA:=ReadParamStr(BParamS); BParamB:=ReadParamStr(BParamS);
       CoreSetRegsState(BParamA='1',BParamB,BDataS);
       SetLineIp;
       end;
  'o': begin
       BDataS:=ADataS;
       Delete(BDataS,1,1);
       repeat
       BParamS:=ReadParamStr(BDataS); if HexToDWordCheck(BParamS,BEip)=FALSE then break;
       BParamS:=ReadParamStr(BDataS); if HexToDWordCheck(BParamS,BEsp)=FALSE then break;
       DelFirstSpace(BDataS);
       BParamS:=ReadTillC(BDataS,#13);
       if BEip<>FEip then break;
       ViewStackData(FEip,BEsp,BParamS);
       until TRUE;
       end;

 end;
 until TRUE;
End;

Procedure TMgDebug.PresetBreakList ( Const ABreakList : string );
Begin
 FPresetBreakList:=ABreakList;
End;

Procedure TMgDebug.PresetBreakListA;
Var
  BBreakList    : string;
  BParam        : string;
  BAddr         : Cardinal;
  BBreakLine    : Integer;
  BDbgIndex     : Integer;
  BDbgView      : TDbgViewText;
Begin
 BBreakList:=FPresetBreakList;

 BDbgIndex:=0;
 while BDbgIndex<PcEdit.PageCount do
  begin
  BDbgView:=GetDbgView(BDbgIndex);
  BDbgView.ClearBreakList;
  inc(BDbgIndex);
  end;

 repeat
 BParam:=ReadParamStr(BBreakList);
 if BParam='' then break;
 if HexToDWordCheck(BParam,BAddr)=FALSE then break;
 BBreakLine:=IpToLine(BAddr);
 if BBreakLine=-1 then break;

 BDbgIndex:=1; // Starts from 1, because 1st page is LST
 while BDbgIndex<PcEdit.PageCount do
  begin
  BDbgView:=GetDbgView(BDbgIndex);
  if BDbgView.IsInside(BBreakLine) then begin BDbgView.AppendBreakList(BBreakLine); break; end;
  inc(BDbgIndex);
  end;

 until FALSE;
End;

Procedure TMgDebug.SetBtnEn;
Begin
 BtPause.Enabled:=FExecActive;
 BtStart.Enabled:=not FExecActive;
End;

Procedure TMgDebug.ProcessEtb ( Const AEtbS : string ); // Which CPU core caused stop
Var
  BEtb      : Cardinal;
  BCpuIdx   : Integer;
  BEventCnt : Integer;
  BActIdx   : Integer;
Begin
 repeat
 if HexToDWordCheck(AEtbS,BEtb)=FALSE then break;
 BActIdx:=-1;
 BCpuIdx:=0; BEventCnt:=0;
 while BCpuIdx<Length(FDbgRegsList) do
  begin
  if BEtb and (1 shl (BCpuIdx+16))<>0 then begin BActIdx:=BCpuIdx; inc(BEventCnt) end;
  if BEtb and (1 shl (BCpuIdx+ 8))<>0 then begin BActIdx:=BCpuIdx; inc(BEventCnt) end;
  if BEtb and (1 shl (BCpuIdx+ 0))<>0 then begin BActIdx:=BCpuIdx; inc(BEventCnt) end;
  inc(BCpuIdx);
  end;
 if BEventCnt=1 then
  begin
  FActIdx:=BActIdx;
  end;
 until TRUE;
End;

end.

