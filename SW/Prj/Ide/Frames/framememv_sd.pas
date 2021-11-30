unit FrameMemV_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, Buttons,
  StdCtrls, DbgInfo_sd, MemViewBase_sd, MemSegHelper_sd;

type
  TOnWndMemVReqData = Procedure ( Const ASegName : string ) of Object;

  { TWndMemVSd }
  TWndMemVSd = class(TFrame)
    BtLoadData: TSpeedButton;
    CbSegName: TComboBox;
    CbUpdate: TCheckBox;
    LbSegName: TLabel;
    PnlTop: TPanel;
    procedure BtLoadDataClick(Sender: TObject);
    procedure CbSegNameChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FSheet      : TTabSheet;
    FResizeLock : boolean;

    FMemViewList    : TMemViewList;
    FOnReqSegData   : TOnWndMemVReqData;


    Procedure UpdateTabName;
    Function GetActiveWnd : TMemViewBase;
    Procedure SelectWndMemA ( AIndex : Integer );
    Procedure ReqData ( Const ASegName : string );

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Clear;
    Procedure Init ( ASheet : TTabSheet; AOnReqSegData : TOnWndMemVReqData );
    Procedure Done;

    Procedure SetSegParams ( Const ASegParams : string );
    Procedure SetSegData ( Const AParams : string );
    Procedure IsMemUpdateNeeded;
  end;

implementation

{$R *.lfm}

Uses
  ConComL, AsmTypes_sd;

Constructor TWndMemVSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndMemVSd.Destroy;
Begin
 Inherited;
End;

Procedure TWndMemVSd.Init ( ASheet : TTabSheet; AOnReqSegData : TOnWndMemVReqData );
Begin
 FOnReqSegData:=AOnReqSegData;
 FResizeLock:=TRUE;
 FSheet:=ASheet;
 FSheet.InsertControl(Self);
 FResizeLock:=FALSE;
 Align:=alClient;
 Clear;
End;

Procedure TWndMemVSd.Done;
Begin
 Clear;
 FResizeLock:=TRUE;
 FSheet.RemoveControl(Self);
End;

Procedure TWndMemVSd.Clear;
Begin
 MemViewListClear(FMemViewList);
 CbSegName.Clear;
 UpdateTabName;
End;

Procedure TWndMemVSd.FrameResize(Sender: TObject);
Begin
 repeat
 if FResizeLock then break;
 CbSegName.Left:=LbSegName.Left+LbSegName.Width+4;
 LbSegName.Top:=CbSegName.Top+((CbSegName.Height-LbSegName.Height) div 2);
 PnlTop.Height:=CbSegName.Height+8;
 BtLoadData.Left:=CbSegName.Left+CbSegName.Width+16; BtLoadData.Top:=CbSegName.Top; BtLoadData.Height:=CbSegName.Height; BtLoadData.Width:=BtLoadData.Height;
 CbUpdate.Left:=BtLoadData.Left+BtLoadData.Width+16;
 CbUpdate.Top:=CbSegName.Top+((CbSegName.Height-CbUpdate.Height) div 2);
 until TRUE;
End;

Procedure TWndMemVSd.CbSegNameChange(Sender: TObject);
Begin
 SelectWndMemA(CbSegName.ItemIndex);
End;

Function TWndMemVSd.GetActiveWnd : TMemViewBase;
Begin
 Result:=nil;
 repeat
 if (CbSegName.ItemIndex<0) or (CbSegName.ItemIndex>=Length(FMemViewList)) then break;
 Result:=FMemViewList[CbSegName.ItemIndex];
 until TRUE;
End;

Procedure TWndMemVSd.BtLoadDataClick(Sender: TObject);
Var
  BMemView  : TMemViewBase;
Begin
 repeat
 BMemView:=GetActiveWnd;
 if BMemView=nil then break;
 BMemView.GrayData;
 ReqData(BMemView.SegName);
 until TRUE;
End;

Procedure TWndMemVSd.UpdateTabName;
Begin
 FSheet.Caption:='Memory View ('+IntToStr(Length(FMemViewList))+')';
End;

Procedure TWndMemVSd.SetSegParams ( Const ASegParams : string );
Var
  BSegParams,
  BSegParamsA   : string;
  BSegStartS,
  BSegSizeS,
  BSegName      : string;
  BSegStart,
  BSegSize      : Cardinal;
  BMemView      : TMemViewBase;
Begin
 Clear;

 BSegParamsA:=ASegParams;
 repeat
 BSegParams:=ReadTillC(BSegParamsA,'#'); DelFirstSpace(BSegParams);
 if BSegParams='' then break;
 BSegName:=ReadParamStr(BSegParams); if BSegName='' then break;
 ReadParamStr(BSegParams); // SegFlags
 BSegStartS:=ReadParamStr(BSegParams); if BSegStartS='' then break; HexToDWordCheck(BSegStartS,BSegStart);
 BSegSizeS:=ReadParamStr(BSegParams); if BSegSizeS='' then break; HexToDWordCheck(BSegSizeS,BSegSize);
 BMemView:=MemViewListAppend(FMemViewList,Self,BSegStart,BSegSize,BSegName,@ReqData);
 CbSegName.Items.Append(BMemView.SegName);
 until FALSE;

 UpdateTabName;
End;

Procedure TWndMemVSd.SelectWndMemA ( AIndex : Integer );
Var
  BIndex        : Integer;
  BMemView      : TMemViewBase;
Begin
 BIndex:=0;
 while BIndex<Length(FMemViewList) do
  begin
  BMemView:=FMemViewList[BIndex];
  BMemView.SetActive(BIndex=AIndex);
  inc(BIndex);
  end;
End;

Procedure TWndMemVSd.ReqData ( Const ASegName : string );
Begin
 if Assigned(FOnReqSegData) then FOnReqSegData(ASegName);
End;

Procedure TWndMemVSd.SetSegData ( Const AParams : string );
Var
  BParams       : string;
  BSegName      : string;
  BPBin         : TMsBinPtr;
  BIndex        : Integer;
  BMemView      : TMemViewBase;
Begin
 repeat
 BParams:=AParams;
 BSegName:=ReadParamStr(BParams);
 ReadParamStr(BParams); // Base addr
 BPBin:=MsBinImport(ReadParamStr(BParams));
 if BPBin=nil then break;
 BIndex:=0;
 while BIndex<Length(FMemViewList) do
  begin
  BMemView:=FMemViewList[BIndex];
  if BMemView.SegName=BSegName then BMemView.SetData(BPBin^);
  inc(BIndex);
  end;
 until TRUE;
End;

Procedure TWndMemVSd.IsMemUpdateNeeded;
Var
  BMemView  : TMemViewBase;
Begin
 repeat
 if CbUpdate.Checked=FALSE then break;
 BMemView:=GetActiveWnd;
 if BMemView=nil then break;
 BMemView.GrayData;
 ReqData(BMemView.SegName);
 until TRUE;
End;

end.

