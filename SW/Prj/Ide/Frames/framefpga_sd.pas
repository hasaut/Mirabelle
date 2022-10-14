unit FrameFpga_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, Buttons,
  StdCtrls, Grids, Dialogs,
  AsmTypes_sd, InfoViewBase_sd;

type
  TOnGetFlashLog = Function : TStringList of object;


  TSectUsage = (suAll, suUsed, suUnused);

  { TWndFpgaSd }

  TWndFpgaSd = class(TFrame)
    ImageList1: TImageList;
    ImageList2: TImageList;
    Panel1: TPanel;
    PnlBtn: TPanel;
    SgSectList: TStringGrid;
    BtReflash: TSpeedButton;
    BtRead: TSpeedButton;
    BtReset: TSpeedButton;
    BtDel: TSpeedButton;
    BtLog: TSpeedButton;
    Splitter1: TSplitter;
    procedure BtLogClick(Sender: TObject);
    procedure BtReadClick(Sender: TObject);
    procedure BtReflashClick(Sender: TObject);
    procedure BtDelClick(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
    procedure SgSectListDblClick(Sender: TObject);
    procedure SgSectListEditingDone(Sender: TObject);
    procedure SgSectListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SgSectListResize(Sender: TObject);
    procedure SgSectListSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FSheet          : TTabSheet;
    FInfoView       : TInfoViewBase;
    FResizeLock     : boolean;
    FOnProcAny      : TOnProcAny;
    FMouseDownCol,
    FMouseDownRow   : Integer;
    FEditingRow,
    FEditingCol     : Integer;

    FGetFlashLog     : TOnGetFlashLog;

    Procedure UpdateTabName;
    Procedure ViewLog ( Const ADataS : string );
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet; AOnProcAny : TOnProcAny; AOnGetFlashLog : TOnGetFlashLog );
    Procedure Done;
    Procedure ViewAny ( Const AMessage : string );

    Procedure Clear;
    Procedure SetSectList ( Const ASectList : string );
    Function CollectSectList ( AUsed : TSectUsage ) : string;
  end;

implementation

{$R *.lfm}

Uses
  ConComL, MMesBase_sd;

Constructor TWndFpgaSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndFpgaSd.Destroy;
Begin
 Inherited;
End;

Procedure TWndFpgaSd.Init ( ASheet : TTabSheet; AOnProcAny : TOnProcAny; AOnGetFlashLog : TOnGetFlashLog );
Begin
 FResizeLock:=TRUE;
 FSheet:=ASheet; FOnProcAny:=AOnProcAny; FGetFlashLog:=AOnGetFlashLog;
 FSheet.InsertControl(Self);
 SgSectList.Align:=alClient;
 FInfoView:=TInfoViewBase.Create(Self); InsertControl(FInfoView); FInfoView.Align:=alClient;
 FResizeLock:=FALSE;
 Align:=alClient;
 Clear;
End;

Procedure TWndFpgaSd.Done;
Begin
 Clear;
 FInfoView.Clear; RemoveControl(FInfoView); FInfoView.Free;
 FResizeLock:=TRUE;
 FSheet.RemoveControl(Self);
End;

Procedure TWndFpgaSd.ViewAny ( Const AMessage : string );
Begin
 repeat
 if AMessage='' then break;
 if AMessage[1]='l' then ViewLog(AMessage)
 else FInfoView.AppendAny(AMessage);
 UpdateTabName;
 until TRUE;
End;

Procedure TWndFpgaSd.Clear;
Begin
 FInfoView.Clear;
 UpdateTabName;
End;

Const
  CSelColWidth  = 30;

Procedure TWndFpgaSd.SgSectListResize(Sender: TObject);
Var
  BWidth    : Integer;
Begin
 repeat
 if FResizeLock then break;
 SgSectList.ColWidths[0]:=CSelColWidth;
 BWidth:=50+SgSectList.Canvas.TextWidth('000000');
 SgSectList.ColWidths[1]:=BWidth;
 SgSectList.ColWidths[2]:=BWidth;
 BWidth:=SgSectList.ClientWidth-CSelColWidth-BWidth*2;
 if BWidth<30 then BWidth:=30;
 SgSectList.ColWidths[3]:=BWidth;
 until TRUE;
End;

Procedure TWndFpgaSd.UpdateTabName;
Begin
 FSheet.Caption:='FPGA Reflash ('+IntToStr(FInfoView.GetMessageCount)+')';
End;

Procedure TWndFpgaSd.SetSectList ( Const ASectList : string );
Var
  BSectList : string;
  BRowCnt,
  BRowIdx   : Integer;
  BColIdx   : Integer;
  BLineS    : string;
Begin
 BSectList:=ASectList;
 BRowCnt:=0;
 repeat
 BLineS:=ReadParamStr(BSectList,';');
 if BLineS='' then break;
 inc(BRowCnt);
 until FALSE;
 SgSectList.RowCount:=BRowCnt+2;

 BSectList:=ASectList;
 BRowIdx:=0;
 while BRowIdx<=BRowCnt do // "<=" in order to make last line empty
  begin
  SgSectList.Cells[0,1+BRowIdx]:='';
  BLineS:=ReadParamStr(BSectList,';');
  for BColIdx:=1 to 3 do
   begin
   SgSectList.Cells[BColIdx,1+BRowIdx]:=ReadParamStr(BLineS);
   end;
  inc(BRowIdx);
  end;

 UpdateTabName;
End;

Function TWndFpgaSd.CollectSectList ( AUsed : TSectUsage ) : string;
Var
  BRowIdx,
  BColIdx   : Integer;
  BParamS   : string;
Begin
 Result:='';
 BRowIdx:=0;
 while BRowIdx<(SgSectList.RowCount-2) do
  begin
  repeat
  case AUsed of
    suAll:
      begin
      end;
    suUsed:
      begin
      if SgSectList.Cells[0,1+BRowIdx]<>'+' then break;
      end;
    suUnused:
      begin
      if SgSectList.Cells[0,1+BRowIdx]='+' then break;
      end;
  end;
  for BColIdx:=1 to 3 do
   begin
   BParamS:=SgSectList.Cells[BColIdx,1+BRowIdx];
   DelFirstLastSpace(BParamS);
   if BParamS='' then BParamS:='#';
   Result:=Result+' '+BParamS;
   end;
  Result:=Result+';';
  until TRUE;
  inc(BRowIdx);
  end;
End;

Procedure TWndFpgaSd.BtReflashClick(Sender: TObject);
Var
  BSectList     : string;
Begin
 Clear;
 repeat
 if Assigned(FOnProcAny)=FALSE then break;
 BSectList:=CollectSectList(suUsed);
 if BSectList='' then
  begin
  VpMesOk(Self,'Error: FPGA Reflash','Section list is empty.'+#13+'Select sections by clicking on "Use" column');
  break;
  end;
 FOnProcAny('Fw'+BSectList);
 until TRUE;
End;

Procedure TWndFpgaSd.BtDelClick(Sender: TObject);
Begin
 SetSectList(CollectSectList(suUnused));
End;

Procedure TWndFpgaSd.BtResetClick(Sender: TObject);
Begin
 Clear;
 repeat
 if Assigned(FOnProcAny)=FALSE then break;
 FOnProcAny('Fn');
 until TRUE;
End;

Procedure TWndFpgaSd.BtLogClick(Sender: TObject);
Begin
 Clear;
 repeat
 if Assigned(FOnProcAny)=FALSE then break;
 FOnProcAny('Fl');
 until TRUE;
End;

Procedure TWndFpgaSd.BtReadClick(Sender: TObject);
Var
  BSectList     : string;
Begin
 Clear;
 repeat
 if Assigned(FOnProcAny)=FALSE then break;
 BSectList:=CollectSectList(suUsed);
 if BSectList='' then
  begin
  VpMesOk(Self,'Error: FPGA Reflash','Section list is empty.'+#13+'Select sections by clicking on "Use" column');
  break;
  end;
 FOnProcAny('Fr'+BSectList);
 until TRUE;
End;

Procedure TWndFpgaSd.SgSectListDblClick(Sender: TObject);
Var
  BDialog   : TOpenDialog;
  BFilename : string;
Begin
 BDialog:=nil;
 repeat
 if (FMouseDownCol=3) and (FMouseDownRow>0) and (FMouseDownRow<SgSectList.RowCount) then
  begin
  BDialog:=TOpenDialog.Create(nil);
  BDialog.Title:='FPGA reflash: select file';
  BDialog.Filter:='All recognizable files (*.rpd; *.hex; *.bit; *.mcs)|*.rpd;*.hex;*.bit;*.mcs|HEX files (*.hex)|*.hex|All files (*.*)|*.*';
  if BDialog.Execute=FALSE then break;
  BFilename:=BDialog.FileName;
  if BFilename='' then break;
  SgSectList.Cells[FMouseDownCol,FMouseDownRow]:=BFilename;
  break;
  end;
 until TRUE;
 if BDialog<>nil then BDialog.Free;
End;

Procedure TWndFpgaSd.SgSectListEditingDone(Sender: TObject);
Var
  BRowIdx,
  BColIdx   : Integer;
Begin
 repeat
 if (FEditingRow+1)=SgSectList.RowCount then
  begin
  BRowIdx:=SgSectList.RowCount;
  SgSectList.RowCount:=BRowIdx+1;
  BColIdx:=0;
  while BColIdx<SgSectList.ColCount do
   begin
   SgSectList.Cells[BColIdx,BRowIdx]:='';
   inc(BColIdx);
   end;
  break;
  end;
 until TRUE;
End;

Procedure TWndFpgaSd.SgSectListSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
Begin
 repeat
 if Value='' then break;
 FEditingRow:=ARow; FEditingCol:=ACol;
 until TRUE;
End;

Procedure TWndFpgaSd.SgSectListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  BColIdx,
  BRowIdx   : Integer;
Begin
 BColIdx:=-1; BRowIdx:=-1;
 SgSectList.MouseToCell(X,Y,BColIdx,BRowIdx);
 FMouseDownCol:=BColIdx; FMouseDownRow:=BRowIdx;
 repeat
 if (BColIdx=0) and (BRowIdx>0) and (BRowIdx<SgSectList.RowCount) and (Button=mbLeft) then
  begin
  if SgSectList.Cells[BColIdx,BRowIdx]='' then SgSectList.Cells[BColIdx,BRowIdx]:='+' else SgSectList.Cells[BColIdx,BRowIdx]:='';
  end;
 until TRUE;
End;

Procedure TWndFpgaSd.ViewLog ( Const ADataS : string );
Var
  BIndex    : Integer;
  BList     : TStringList;
Begin
 repeat
 if Assigned(FGetFlashLog)=FALSE then break;
 BList:=FGetFlashLog(); if BList=nil then break;
 BIndex:=0;
 while BIndex<BList.Count do
  begin
  FInfoView.AppendAny('-'+BList.Strings[BIndex]);
  inc(BIndex);
  end;
 until TRUE;
End;

end.

