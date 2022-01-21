unit MMesOk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType;

type

  { TMesOkForm }

  TMesOkForm = class(TForm)
    Label1: TLabel;
    BtOk: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText ( AText : string );
  end;

implementation

{$R *.lfm}

procedure TMesOkForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (not (ssAlt in Shift)) and (not (ssCtrl in Shift)) and (not (ssShift in Shift)) then
  begin
  case Key of
    VK_RETURN : Close;
    VK_ESCAPE : Close;
  end (* case *)
  end;
end;

procedure TMesOkForm.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
end;

procedure TMesOkForm.BtOkClick(Sender: TObject);
begin
 Close;
end;

Procedure TMesOkForm.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesOkForm.SetText ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label1.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then
  begin
  ClientWidth:=BTextWidth;
  Label1.Width:=BTextWidth;
  BtOk.Left:=(BTextWidth div 2)-(BtOk.Width div 2);
  end;
 Label1.Caption:=AText;
 BtOk.Top:=Label1.Top+Label1.Height+8;
 ClientHeight:=BtOk.Top+BtOk.Height+8;
 BtOk.Left:=(ClientWidth-BtOk.Width) div 2;
End;


//initialization
  //{$I mmesok.lrs}

end.

