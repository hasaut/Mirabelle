unit PlayerParams_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TPlayerParamsForm }

  TPlayerParamsForm = class(TForm)
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    CbIssFpga: TComboBox;
    EComName: TEdit;
    EComBaud: TEdit;
    LIssFpga: TLabel;
    LComName: TLabel;
    LComBaud: TLabel;
    procedure BtCancelClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure CbIssFpgaChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FPlayerParams   : string;
    FUpdateFlag     : boolean;

    Procedure SetBtnEnable;
  public
    { Public declarations }
    property PlayerParams : string read FPlayerParams write FPlayerParams;
    property UpdateFlag : boolean read FUpdateFlag write FUpdateFlag;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Procedure TPlayerParamsForm.FormShow(Sender: TObject);
Var
  BDummyS       : string;
  BWidthMax     : Integer;
  BMode         : Char;
Begin
 FormStyle:=fsStayOnTop;
 BWidthMax:=10;
 LIssFpga.Caption:='ISS, HW or Combi';
 LComName.Caption:='Com Port Name';
 LComBaud.Caption:='Com baud rate';
 if LIssFpga.Width>BWidthMax then BWidthMax:=LIssFpga.Width;
 if LComName.Width>BWidthMax then BWidthMax:=LComName.Width;
 if LComBaud.Width>BWidthMax then BWidthMax:=LComBaud.Width;
 ClientWidth:=BWidthMax+8+CbIssFpga.Width;
 CbIssFpga.Top:=8; CbIssFpga.Left:=BWidthMax+6;
 EComName.Top:=CbIssFpga.Top+CbIssFpga.Height+4; EComName.Left:=BWidthMax+6;
 EComBaud.Top:=EComName.Top+EComName.Height+4; EComBaud.Left:=EComName.Left;
 LIssFpga.Left:=BWidthMax+4-LIssFpga.Width; LIssFpga.Top:=CbIssFpga.Top+((CbIssFpga.Height-LIssFpga.Height) div 2);
 LComName.Left:=BWidthMax+4-LComName.Width; LComName.Top:=EComName.Top+((EComName.Height-LComName.Height) div 2);
 LComBaud.Left:=BWidthMax+4-LComBaud.Width; LComBaud.Top:=EComBaud.Top+((EComBaud.Height-LComBaud.Height) div 2);

 ClientHeight:=EComBaud.Top+EComBaud.Height+4+BtOk.Height+4;
 BtOk.Top:=EComBaud.Top+EComBaud.Height+4;
 BtCancel.Top:=BtOk.Top;
 BtCancel.Left:=EComBaud.Left+EComBaud.Width-BtCancel.Width;
 BtOk.Left:=BtCancel.Left-4-BtOk.Width;

 CbIssFpga.Items.Clear;
 CbIssFpga.Items.Append('ISS (simulator)');
 CbIssFpga.Items.Append('HW (FPGA)');
 CbIssFpga.Items.Append('Combi (both Simulator and FPGA)');

 BDummyS:=FPlayerParams;
 if BDummyS<>'' then begin BMode:=BDummyS[1]; Delete(BDummyS,1,1); end
 else BMode:=#0;
 if BMode='f' then CbIssFpga.ItemIndex:=1
 else if BMode='b' then CbIssFpga.ItemIndex:=2
 else CbIssFpga.ItemIndex:=0;
 EComName.Text:=ReadParamStr(BDummyS);
 EComBaud.Text:=ReadParamStr(BDummyS);
 SetBtnEnable;
end;

procedure TPlayerParamsForm.BtCancelClick(Sender: TObject);
begin
 FUpdateFlag:=FALSE;
 Close;
end;

procedure TPlayerParamsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

Procedure TPlayerParamsForm.BtOkClick(Sender: TObject);
Var
  BDummyS   : string;
Begin
 BDummyS:='-';
 case CbIssFpga.ItemIndex of
  0: BDummyS:='i';
  1: BDummyS:='f';
  2: BDummyS:='b';
 end;
 BDummyS:=BDummyS+EComName.Text+' '+EComBaud.Text;
 FPlayerParams:=BDummyS;
 FUpdateFlag:=TRUE;
 Close;
End;

procedure TPlayerParamsForm.CbIssFpgaChange(Sender: TObject);
begin
 SetBtnEnable;
end;

Procedure TPlayerParamsForm.SetBtnEnable;
Begin
 case CbIssFpga.ItemIndex of
  0: begin
     EComName.Enabled:=FALSE;
     EComBaud.Enabled:=FALSE;
     end;
  1,
  2: begin
     EComName.Enabled:=TRUE;
     EComBaud.Enabled:=TRUE;
     end;
 end;
End;

end.

