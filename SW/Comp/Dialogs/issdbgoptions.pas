unit IssDbgOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, MdHelper;

type

  { TDbgOptionsForm }

  TDbgOptionsForm = class(TForm)
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    CbIssFpga: TComboBox;
    EComList: TEdit;
    EComName: TEdit;
    EComBaud: TEdit;
    LComList: TLabel;
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
    FPlayerParams   : TPlayerParams;
    FUpdateFlag     : boolean;

    Procedure SetBtnEnable;
  public
    { Public declarations }
    property PlayerParams : TPlayerParams read FPlayerParams write FPlayerParams;
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
  BModeS        : string;
Begin
 FormStyle:=fsStayOnTop;
 BWidthMax:=10;
 LIssFpga.Caption:='ISS or HW';
 LComName.Caption:='Com Port Name';
 LComBaud.Caption:='Com baud rate';
 LComList.Caption:='Com scan list';
 if LIssFpga.Width>BWidthMax then BWidthMax:=LIssFpga.Width;
 if LComName.Width>BWidthMax then BWidthMax:=LComName.Width;
 if LComBaud.Width>BWidthMax then BWidthMax:=LComBaud.Width;
 if LComList.Width>BWidthMax then BWidthMax:=LComList.Width;
 ClientWidth:=BWidthMax+8+CbIssFpga.Width;
 CbIssFpga.Top:=8; CbIssFpga.Left:=BWidthMax+6;
 EComName.Top:=CbIssFpga.Top+CbIssFpga.Height+4; EComName.Left:=BWidthMax+6;
 EComBaud.Top:=EComName.Top+EComName.Height+4; EComBaud.Left:=EComName.Left;
 EComList.Top:=EComBaud.Top+EComBaud.Height+4; EComList.Left:=EComName.Left;
 LIssFpga.Left:=BWidthMax+4-LIssFpga.Width; LIssFpga.Top:=CbIssFpga.Top+((CbIssFpga.Height-LIssFpga.Height) div 2);
 LComName.Left:=BWidthMax+4-LComName.Width; LComName.Top:=EComName.Top+((EComName.Height-LComName.Height) div 2);
 LComBaud.Left:=BWidthMax+4-LComBaud.Width; LComBaud.Top:=EComBaud.Top+((EComBaud.Height-LComBaud.Height) div 2);
 LComList.Left:=BWidthMax+4-LComList.Width; LComList.Top:=EComList.Top+((EComList.Height-LComList.Height) div 2);

 ClientHeight:=EComList.Top+EComList.Height+4+BtOk.Height+4;
 BtOk.Top:=EComList.Top+EComList.Height+4;
 BtCancel.Top:=BtOk.Top;
 BtCancel.Left:=EComList.Left+EComList.Width-BtCancel.Width;
 BtOk.Left:=BtCancel.Left-4-BtOk.Width;

 CbIssFpga.Items.Clear;
 CbIssFpga.Items.Append('ISS (simulator)');
 CbIssFpga.Items.Append('HW (FPGA)');

 BDummyS:=VerbosePlayerParams(FPlayerParams);
 BModeS:=LowerCase(ReadParamStr(BDummyS));
 if BModeS='fpga' then CbIssFpga.ItemIndex:=1
 else CbIssFpga.ItemIndex:=0;
 EComName.Text:=ReadParamStr(BDummyS);
 EComBaud.Text:=ReadParamStr(BDummyS);
 DelFirstLastSpace(BDummyS); EComList.Text:=BDummyS;
 SetBtnEnable;
end;

procedure TDbgOptionsForm.BtCancelClick(Sender: TObject);
begin
 FUpdateFlag:=FALSE;
 Close;
end;

procedure TDbgOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

Procedure TDbgOptionsForm.BtOkClick(Sender: TObject);
Var
  BDummyS   : string;
Begin
 BDummyS:='none';
 case CbIssFpga.ItemIndex of
  0: BDummyS:='Iss';
  1: BDummyS:='Fpga';
 end;
 BDummyS:=BDummyS+' '+EComName.Text+' '+EComBaud.Text+' '+EComList.Text;
 ParsePlayerParams(BDummyS,FPlayerParams);
 FUpdateFlag:=TRUE;
 Close;
End;

procedure TDbgOptionsForm.CbIssFpgaChange(Sender: TObject);
begin
 SetBtnEnable;
end;

Procedure TDbgOptionsForm.SetBtnEnable;
Begin
 case CbIssFpga.ItemIndex of
  0: begin
     EComName.Enabled:=FALSE;
     EComBaud.Enabled:=FALSE;
     EComList.Enabled:=FALSE;
     end;
  1: begin
     EComName.Enabled:=TRUE;
     EComBaud.Enabled:=TRUE;
     EComList.Enabled:=TRUE;
     end;
 end;
End;

end.

