unit MMesOk_old;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  LCLType;

type
  TMesOkForm = class(TForm)
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZSpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText ( AText : string );
  end;

implementation

procedure TMesOkForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_RETURN : Close;
   VK_ESCAPE : Close;
 end (* case *)
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
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=AText;
End;

procedure TMesOkForm.ZSpeedButton1Click(Sender: TObject);
begin
 Close;
end;

end.
 
