unit TbListOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, MdHelper;

type

  { TTbListOptionsForm }

  TTbListOptionsForm = class(TForm)
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    EPort: TEdit;
    ETOut: TEdit;
    EHost: TEdit;
    LHost: TLabel;
    LPort: TLabel;
    LTOut: TLabel;
    procedure BtCancelClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FTbListParams   : TTbListParams;
    FUpdateFlag     : boolean;

    Procedure SetSizeA;
  public
    { Public declarations }
    property TbListParams : TTbListParams read FTbListParams write FTbListParams;
    property UpdateFlag : boolean read FUpdateFlag write FUpdateFlag;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Procedure TTbListOptionsForm.FormShow(Sender: TObject);
Begin
 FormStyle:=fsStayOnTop;

 SetSizeA;

 EHost.Text:=FTbListParams.FHost;
 EPort.Text:=FTbListParams.FPort;
 ETOut.Text:=IntToStr(FTbListParams.FTOut);
end;

Procedure TTbListOptionsForm.SetSizeA;
Var
  BWidthMax     : Integer;
Begin
 BWidthMax:=10;
 LHost.Caption:='Host';
 LPort.Caption:='Port';
 LTOut.Caption:='Time out';
 if LHost.Width>BWidthMax then BWidthMax:=LHost.Width;
 if LPort.Width>BWidthMax then BWidthMax:=LPort.Width;
 if LTOut.Width>BWidthMax then BWidthMax:=LTOut.Width;
 ClientWidth:=BWidthMax+8+EHost.Width;
 EHost.Top:=8; EHost.Left:=BWidthMax+6;
 EPort.Top:=EHost.Top+EHost.Height+4; EPort.Left:=BWidthMax+6;
 ETOut.Top:=EPort.Top+EPort.Height+4; ETOut.Left:=EPort.Left;
 LHost.Left:=BWidthMax+4-LHost.Width; LHost.Top:=EHost.Top+((EHost.Height-LHost.Height) div 2);
 LPort.Left:=BWidthMax+4-LPort.Width; LPort.Top:=EPort.Top+((EPort.Height-LPort.Height) div 2);
 LTOut.Left:=BWidthMax+4-LTOut.Width; LTOut.Top:=ETOut.Top+((ETOut.Height-LTOut.Height) div 2);

 ClientHeight:=ETOut.Top+ETOut.Height+4+BtOk.Height+4;
 BtOk.Top:=ETOut.Top+ETOut.Height+4;
 BtCancel.Top:=BtOk.Top;
 BtCancel.Left:=ETOut.Left+ETOut.Width-BtCancel.Width;
 BtOk.Left:=BtCancel.Left-4-BtOk.Width;
End;

procedure TTbListOptionsForm.BtCancelClick(Sender: TObject);
begin
 FUpdateFlag:=FALSE;
 Close;
end;

procedure TTbListOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

Procedure TTbListOptionsForm.BtOkClick(Sender: TObject);
Var
  BDummyS   : string;
Begin
 BDummyS:=EHost.Text; FTbListParams.FHost:=ReadParamStr(BDummyS);
 BDummyS:=EPort.Text; FTbListParams.FPort:=ReadParamStr(BDummyS);
 TryStrToInt(ETOut.Text,FTbListParams.FTOut);
 FUpdateFlag:=TRUE;
 Close;
End;

end.

