unit PrjOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TPrjOptionsForm }

  TPrjOptionsForm = class(TForm)
    BvCrvCompStr: TBevel;
    BvMonFile: TBevel;
    BvMemWidth: TBevel;
    BvMemSize: TBevel;
    BvPasCompStr: TBevel;
    BvCppCompStr: TBevel;
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    CbOpenDebug: TCheckBox;
    EdCompCpp: TEdit;
    EdCompCrv: TEdit;
    EdMonFile: TEdit;
    EdMemSize: TEdit;
    EdCompPas: TEdit;
    EdMemWidth: TEdit;
    LbCrvCompStr: TLabel;
    LbMonFileB: TLabel;
    LbMonFileA: TLabel;
    LbMonFile: TLabel;
    LbHdrMemWidth: TLabel;
    LbHintMemWidth: TLabel;
    LbHdrMemSize: TLabel;
    LbPasCompStr: TLabel;
    LbCppCompStr: TLabel;
    LbMemWidth: TLabel;
    LbMemSize: TLabel;
    RgMemType: TRadioGroup;
    procedure BtCancelClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure CbOpenDebugChange(Sender: TObject);
    procedure EdMemSizeChange(Sender: TObject);
    procedure EdMemWidthChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FParamsSelf : TStringList;
    FParamsExt  : TStringList;

  public
    { Public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SetParams ( AParams : TStringList );
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Constructor TPrjOptionsForm.Create ( AOwner : TComponent );
Begin
 Inherited;
 FParamsSelf:=TStringList.Create;
End;

Destructor TPrjOptionsForm.Destroy;
Begin
 FParamsSelf.Free;
 Inherited;
End;

Procedure TPrjOptionsForm.SetParams ( AParams : TStringList );
Begin
 FParamsExt:=AParams;
 FParamsSelf.Assign(AParams);
End;

Procedure TPrjOptionsForm.FormShow(Sender: TObject);
Var
  BWidth        : Integer;
  BTextHeight   : Integer;
Begin
 FormStyle:=fsStayOnTop;
 //BWidth:=LbHintMemWidth.Width+32;
 BWidth:=LbMonFileA.Width+32;
 if BWidth<260 then BWidth:=260;
 if ClientWidth<BWidth then ClientWidth:=BWidth;
 BTextHeight:=LbHdrMemWidth.Height;

 LbHdrMemWidth.Left:=16; LbHdrMemWidth.Top:=4;
 EdMemWidth.Left:=LbHdrMemWidth.Left; EdMemWidth.Top:=LbHdrMemWidth.Top+BTextHeight+4;
 LbMemWidth.Left:=EdMemWidth.Left+EdMemWidth.Width+4; LbMemWidth.Top:=EdMemWidth.Top+(EdMemWidth.Height div 2)-(BTextHeight div 2);;
 LbHintMemWidth.Left:=12; LbHintMemWidth.Top:=EdMemWidth.Top+EdMemWidth.Height+4;
 BvMemWidth.Left:=4; BvMemWidth.Width:=ClientWidth-8;
 BvMemWidth.Top:=LbHdrMemWidth.Top+(BTextHeight div 2); BvMemWidth.Height:=LbHintMemWidth.Top+BTextHeight+4-BvMemWidth.Top;

 LbHdrMemSize.Left:=LbHdrMemWidth.Left; LbHdrMemSize.Top:=BvMemWidth.Top+BvMemWidth.Height+8;
 EdMemSize.Left:=LbHdrMemSize.Left; EdMemSize.Top:=LbHdrMemSize.Top+BTextHeight+4;
 LbMemSize.Left:=EdMemSize.Left+EdMemSize.Width+4; LbMemSize.Top:=EdMemSize.Top+(EdMemSize.Height div 2)-(BTextHeight div 2);;
 BvMemSize.Left:=4; BvMemSize.Width:=ClientWidth-8;
 BvMemSize.Top:=LbHdrMemSize.Top+(BTextHeight div 2); BvMemSize.Height:=BvMemWidth.Height;

 CbOpenDebug.Left:=BvMemSize.Left; CbOpenDebug.Top:=BvMemSize.Top+BvMemSize.Height+8;

 LbPasCompStr.Left:=LbHdrMemSize.Left; LbPasCompStr.Top:=CbOpenDebug.Top+CbOpenDebug.Height+8;
 EdCompPas.Left:=12; EdCompPas.Width:=ClientWidth-24; EdCompPas.Top:=LbPasCompStr.Top+BTextHeight+4;
 BvPasCompStr.Left:=4; BvPasCompStr.Width:=ClientWidth-8;
 BvPasCompStr.Top:=LbPasCompStr.Top+(BTextHeight div 2); BvPasCompStr.Height:=EdCompPas.Top+EdCompPas.Height+4-BvPasCompStr.Top;

 LbCppCompStr.Left:=LbHdrMemSize.Left; LbCppCompStr.Top:=BvPasCompStr.Top+BvPasCompStr.Height+8;
 EdCompCpp.Left:=12; EdCompCpp.Width:=ClientWidth-24; EdCompCpp.Top:=LbCppCompStr.Top+BTextHeight+4;
 BvCppCompStr.Left:=4; BvCppCompStr.Width:=ClientWidth-8;
 BvCppCompStr.Top:=LbCppCompStr.Top+(BTextHeight div 2); BvCppCompStr.Height:=EdCompCpp.Top+EdCompCpp.Height+4-BvCppCompStr.Top;

 LbCrvCompStr.Left:=LbHdrMemSize.Left; LbCrvCompStr.Top:=BvCppCompStr.Top+BvCppCompStr.Height+8;
 EdCompCrv.Left:=12; EdCompCrv.Width:=ClientWidth-24; EdCompCrv.Top:=LbCrvCompStr.Top+BTextHeight+4;
 BvCrvCompStr.Left:=4; BvCrvCompStr.Width:=ClientWidth-8;
 BvCrvCompStr.Top:=LbCrvCompStr.Top+(BTextHeight div 2); BvCrvCompStr.Height:=EdCompCrv.Top+EdCompCrv.Height+4-BvCrvCompStr.Top;

 RgMemType.Left:=BvMemSize.Left; RgMemType.Top:=BvCrvCompStr.Top+BvCrvCompStr.Height+8;
 RgMemType.AutoSize:=TRUE;
 RgMemType.AutoSize:=FALSE;
 RgMemType.Width:=BvCrvCompStr.Width;

 LbMonFile.Left:=LbHdrMemSize.Left; LbMonFile.Top:=RgMemType.Top+RgMemType.Height+8;
 EdMonFile.Left:=12; EdMonFile.Width:=ClientWidth-24; EdMonFile.Top:=LbMonFile.Top+BTextHeight+4;
 BvMonFile.Left:=4; BvMonFile.Width:=ClientWidth-8;
 LbMonFileA.Left:=12; LbMonFileA.Top:=EdMonFile.Top+EdMonFile.Height+4;
 LbMonFileB.Left:=12; LbMonFileB.Top:=LbMonFileA.Top+LbMonFileA.Height+4;
 BvMonFile.Top:=LbMonFile.Top+(BTextHeight div 2); BvMonFile.Height:=LbMonFileB.Top+LbMonFileB.Height+4-BvMonFile.Top;

 //BtOk.Anchors:=[]; BtCancel.Anchors:=[];
 BtOk.Left:=BvMonFile.Left+BvMonFile.Width-BtOk.Width-BtCancel.Width-4;
 BtOk.Top:=BvMonFile.Top+BvMonFile.Height+(BTextHeight div 2)+4;
 BtCancel.Left:=BtOk.Left+BtOk.Width+4;
 BtCancel.Top:=BtOk.Top;

 ClientHeight:=BtCancel.Top+BtCancel.Height+8;
 //BtOk.Anchors:=[akRight,akBottom]; BtCancel.Anchors:=[akRight,akBottom];

 EdMemWidth.Text:=FParamsSelf.Values['MemWidth'];
 EdMemSize.Text:=FParamsSelf.Values['MemSize'];
 EdCompPas.Text:=FParamsSelf.Values['PasCompStr'];
 EdCompCpp.Text:=FParamsSelf.Values['CppCompStr'];
 EdCompCrv.Text:=FParamsSelf.Values['CrvCompStr'];
 CbOpenDebug.Checked:=LowerCase(FParamsSelf.Values['OpenDebug'])='true';
 if FParamsSelf.Values['NvMemType']='IntFlash' then RgMemType.ItemIndex:=0
 else RgMemType.ItemIndex:=1;
 EdMonFile.Text:=FParamsSelf.Values['MonFileName'];
End;

Procedure TPrjOptionsForm.EdMemWidthChange(Sender: TObject);
Var
  BData         : Integer;
Begin
 StringToInteger(EdMemWidth.Text,BData);
 if BData=2 then LbMemWidth.Caption:='bytes (16 bits)'
 else if BData=4 then LbMemWidth.Caption:='bytes (32 bits)'
 else if BData=8 then LbMemWidth.Caption:='bytes (64 bits)'
 else if BData=16 then LbMemWidth.Caption:='bytes (128 bits)'
 else if BData=32 then LbMemWidth.Caption:='bytes (256 bits)'
 else LbMemWidth.Caption:='bytes (Invalid value)';
 FParamsSelf.Values['MemWidth']:=IntToStr(BData);
End;

Procedure TPrjOptionsForm.EdMemSizeChange(Sender: TObject);
Var
  BData         : Integer;
  BWidth        : Integer;
Begin
 StringToInteger(EdMemSize.Text,BData);
 StringToInteger(FParamsSelf.Values['MemWidth'],BWidth);
 LbMemSize.Caption:='words ('+IntToStr(BData*BWidth)+' bytes [0x'+IntToHex(BData*BWidth,4)+'])';
 FParamsSelf.Values['MemSize']:=IntToStr(BData);
End;

procedure TPrjOptionsForm.CbOpenDebugChange(Sender: TObject);
begin
 if CbOpenDebug.Checked then FParamsSelf.Values['OpenDebugWnd']:='True'
 else FParamsSelf.Values['OpenDebugWnd']:='False';
end;

procedure TPrjOptionsForm.BtCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TPrjOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

procedure TPrjOptionsForm.BtOkClick(Sender: TObject);
begin
 FParamsSelf.Values['PasCompStr']:=EdCompPas.Text;
 FParamsSelf.Values['CppCompStr']:=EdCompCpp.Text;
 FParamsSelf.Values['CrvCompStr']:=EdCompCrv.Text;
 if RgMemType.ItemIndex=0 then FParamsSelf.Values['NvMemType']:='IntFlash'
 else FParamsSelf.Values['NvMemType']:='ExtEeprom';
 FParamsSelf.Values['MonFileName']:=EdMonFile.Text;
 FParamsExt.Assign(FParamsSelf);
 Close;
end;

end.

