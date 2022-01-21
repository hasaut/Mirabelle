unit EnvOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TEnvOptionsForm }

  TEnvOptionsForm = class(TForm)
    Bevel3: TBevel;
    Bevel4: TBevel;
    BtCancel: TSpeedButton;
    BtOk: TSpeedButton;
    EdCompPas: TEdit;
    EdCompCpp: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    procedure BtCancelClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FParams     : TStringList;

  public
    { Public declarations }
    Procedure SetParams ( AParams : TStringList );
  end;

implementation

{$R *.lfm}

Procedure TEnvOptionsForm.SetParams ( AParams : TStringList );
Begin
 FParams:=AParams;
End;

procedure TEnvOptionsForm.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
 EdCompPas.Text:=FParams.Values['PasExecName'];
 EdCompCpp.Text:=FParams.Values['CppExecName'];
end;

procedure TEnvOptionsForm.BtCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TEnvOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

procedure TEnvOptionsForm.BtOkClick(Sender: TObject);
begin
 FParams.Values['PasExecName']:=EdCompPas.Text;
 FParams.Values['CppExecName']:=EdCompCpp.Text;
 Close;
end;


//initialization
//  {$I prjoptions.lrs}

end.

