unit DbgOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TDbgOptionsForm }

  TDbgOptionsForm = class(TForm)
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    EComName: TEdit;
    EComBaud: TEdit;
    LComName: TLabel;
    LComBaud: TLabel;
    procedure BtCancelClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FPrefix     : string;
    FParams     : TStringList;
    FUpdateFlag : boolean;

  public
    { Public declarations }
    Procedure SetParams ( Const APrefix : string; AParams : TStringList );
    property UpdateFlag : boolean read FUpdateFlag write FUpdateFlag;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Procedure TDbgOptionsForm.FormShow(Sender: TObject);
Var
  BDummyS       : string;
  BWidthMax     : Integer;
Begin
 FormStyle:=fsStayOnTop;
 BWidthMax:=10;
 LComName.Caption:='Com Port Name';
 LComBaud.Caption:='Com baud rate';
 if LComName.Width>BWidthMax then BWidthMax:=LComName.Width;
 if LComBaud.Width>BWidthMax then BWidthMax:=LComBaud.Width;
 ClientWidth:=BWidthMax+8+EComName.Width;
 EComName.Top:=8; EComName.Left:=BWidthMax+6;
 EComBaud.Top:=EComName.Top+EComName.Height+4; EComBaud.Left:=EComName.Left; EComBaud.Width:=EComName.Width;
 LComName.Left:=BWidthMax+4-LComName.Width; LComName.Top:=EComName.Top+((EComName.Height-LComName.Height) div 2);
 LComBaud.Left:=BWidthMax+4-LComBaud.Width; LComBaud.Top:=EComBaud.Top+((EComBaud.Height-LComBaud.Height) div 2);

 ClientHeight:=EComBaud.Top+EComBaud.Height+4+BtOk.Height+4;
 BtOk.Top:=EComBaud.Top+EComBaud.Height+4;
 BtCancel.Top:=BtOk.Top;
 BtCancel.Left:=EComBaud.Left+EComBaud.Width-BtCancel.Width;
 BtOk.Left:=BtCancel.Left-4-BtOk.Width;

 BDummyS:=FParams.Values[FPrefix];
 EComName.Text:=ReadParamStr(BDummyS);
 EComBaud.Text:=ReadParamStr(BDummyS);
end;

Procedure TDbgOptionsForm.SetParams ( Const APrefix : string; AParams : TStringList );
Begin
 FPrefix:=APrefix;
 FParams:=AParams;
End;

procedure TDbgOptionsForm.BtCancelClick(Sender: TObject);
begin
 FUpdateFlag:=FALSE;
 Close;
end;

procedure TDbgOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

procedure TDbgOptionsForm.BtOkClick(Sender: TObject);
begin
 FParams.Values[FPrefix]:=EComName.Text+' '+EComBaud.Text;
 FUpdateFlag:=TRUE;
 Close;
end;


end.

