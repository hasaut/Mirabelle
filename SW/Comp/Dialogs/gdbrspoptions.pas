unit GdbRspOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, MdHelper;

type

  { TGdbRspOptionsForm }

  TGdbRspOptionsForm = class(TForm)
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
    FGdbRspParams   : TGdbRspParams;
    FUpdateFlag     : boolean;

  public
    { Public declarations }
    property GdbRspParams : TGdbRspParams read FGdbRspParams write FGdbRspParams;
    property UpdateFlag : boolean read FUpdateFlag write FUpdateFlag;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Procedure TGdbRspOptionsForm.FormShow(Sender: TObject);
Var
  BWidthMax     : Integer;
Begin
 FormStyle:=fsStayOnTop;
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

 EHost.Text:=FGdbRspParams.FHost;
 EPort.Text:=FGdbRspParams.FPort;
 ETOut.Text:=IntToStr(FGdbRspParams.FTOut);
end;

procedure TGdbRspOptionsForm.BtCancelClick(Sender: TObject);
begin
 FUpdateFlag:=FALSE;
 Close;
end;

procedure TGdbRspOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

Procedure TGdbRspOptionsForm.BtOkClick(Sender: TObject);
Var
  BDummyS   : string;
Begin
 BDummyS:=EHost.Text; FGdbRspParams.FHost:=ReadParamStr(BDummyS);
 BDummyS:=EPort.Text; FGdbRspParams.FPort:=ReadParamStr(BDummyS);
 TryStrToInt(ETOut.Text,FGdbRspParams.FTOut);
 FUpdateFlag:=TRUE;
 Close;
End;

end.

