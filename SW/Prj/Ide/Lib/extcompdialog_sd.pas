unit ExtCompDialog_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TWndExtCompDialog }

  TWndExtCompDialog = class(TForm)
    EdExec: TEdit;
    EdParams: TEdit;
    GbExec: TGroupBox;
    BtExec: TSpeedButton;
    GbParams: TGroupBox;
    LbParamsA: TLabel;
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    procedure BtCancelClick(Sender: TObject);
    procedure BtExecClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FParams     : string;

    Procedure SetSizes;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetParams ( Const AParams : string );
    Function GetParams : string;
  end;

implementation

Uses
  ConComL;

{$R *.lfm}

Constructor TWndExtCompDialog.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndExtCompDialog.Destroy;
Begin
 Inherited;
End;

Procedure TWndExtCompDialog.SetSizes;
Var
  BLeft,
  BTop,
  BHeight   : Integer;
Begin
 BLeft:=EdExec.Left; BTop:=EdExec.Top; BHeight:=EdExec.Height;
 EdExec.Align:=alNone; EdExec.Left:=BLeft; EdExec.Top:=BTop;
 BtExec.Height:=BHeight; BtExec.Width:=BHeight;
 BtExec.Top:=BTop; BtExec.Left:=GbExec.ClientWidth-BHeight-2;
 EdExec.Width:=BtExec.Left-EdExec.Left-2;
 GbExec.ClientHeight:=BHeight+2;
 GbParams.ClientHeight:=LbParamsA.Height+EdParams.Height+2;
 BtOk.Height:=BHeight; BtCancel.Height:=BHeight;
 BtOk.Width:=100; BtCancel.Width:=100;
 BTop:=GbParams.Top+GbParams.Height+4;
 BtCancel.Left:=ClientWidth-BtCancel.Width-4; BtCancel.Top:=BTop;
 BtOk.Left:=BtCancel.Left-BtOk.Width-4; BtOk.Top:=BTop;
 ClientHeight:=BtOk.Top+BtOk.Height+4;
End;

procedure TWndExtCompDialog.FormResize(Sender: TObject);
begin
 SetSizes;
end;

procedure TWndExtCompDialog.FormShow(Sender: TObject);
begin
 SetSizes;
end;

Procedure TWndExtCompDialog.BtOkClick(Sender: TObject);
Var
  BParam    : string;
Begin
 BParam:=EdExec.Text; DelLastSpace(BParam);
 if BParam='' then BParam:='#';
 FParams:=BParam+' ';
 BParam:=EdParams.Text;
 DelFirstSpace(BParam); DelLastSpace(BParam);
 FParams:=FParams+BParam;
 Close;
End;

procedure TWndExtCompDialog.BtCancelClick(Sender: TObject);
begin
 Close;
end;

Procedure TWndExtCompDialog.SetParams ( Const AParams : string );
Var
  BParams,
  BParam    : string;
Begin
 BParams:=AParams;
 FParams:=BParams;
 BParam:=ReadParamStr(BParams);
 if BParam='#' then EdExec.Text:='' else EdExec.Text:=BParam;
 DelFirstSpace(BParams); DelLastSpace(BParams);
 EdParams.Text:=BParams;
End;

Function TWndExtCompDialog.GetParams : string;
Begin
 Result:=FParams;
End;

Procedure TWndExtCompDialog.BtExecClick(Sender: TObject);
Var
  BDialog   : TOpenDialog;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Select external compiler';
 BDialog.Filter:='Executable files (*.exe)|*.exe|All files (*.*)|*.*';
 BDialog.FilterIndex:=-1;
 BDialog.Options:=BDialog.Options+[ofEnableSizing,ofViewDetail];
 BDialog.InitialDir:='';
 BDialog.DefaultExt:='';

 if BDialog.Execute then EdExec.Text:=BDialog.FileName;

 BDialog.Free;
End;

end.

