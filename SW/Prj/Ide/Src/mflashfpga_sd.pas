unit MFlashFpga_sd;

{$mode objfpc}{$H+}

interface

uses
  LResources, LCLType,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, Grids;

type

  { TFlashFpgaDialog }

  TFlashFpgaDialog = class(TForm)
    BtnOk: TSpeedButton;
    BtnCancel: TSpeedButton;
    StringGrid1: TStringGrid;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FParams     : TStringList;
    FFlashList  : string;

    Procedure FillList;
  public
    { Public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetParams ( AParams : TStringList );

    property FlashList : string read FFlashList;
  end;

implementation

Uses
  ConComF;

{$R *.lfm}

Constructor TFlashFpgaDialog.Create ( AOwner : TComponent );
Begin
 Inherited;
 FParams:=nil;
End;

Destructor TFlashFpgaDialog.Destroy;
Begin
 Inherited;
End;

Procedure TFlashFpgaDialog.SetParams ( AParams : TStringList );
Begin
 FParams:=AParams;
End;

procedure TFlashFpgaDialog.BtnCancelClick(Sender: TObject);
begin
 FFlashList:='';
 Close;
end;

procedure TFlashFpgaDialog.BtnOkClick(Sender: TObject);
begin
 FillList;
 Close;
end;

procedure TFlashFpgaDialog.FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
   VK_ESCAPE :
    begin
    FFlashList:='';
    Close;
    end;

   VK_RETURN :
    begin
    FillList;
    Close;
    end;

 end; (* case *)
end;

procedure TFlashFpgaDialog.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
 if FParams<>nil then RdScrPos(FParams,Self,'FlashFpgaDialog');
end;

procedure TFlashFpgaDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if FParams<>nil then WrScrPos(FParams,Self,'FlashFpgaDialog');
end;

procedure TFlashFpgaDialog.FillList;
begin

end;

end.
