unit MMesOk2a;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, LCLType;

type
  TMesOk2Form = class(TForm)
    ZSpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZSpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText1 ( AText : string );
    Procedure SetText2 ( AText : string );
  end;

implementation               

{$R *.lfm}

procedure TMesOk2Form.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Shift=[] then
  begin
  case Key of
   VK_RETURN : Close;
   VK_ESCAPE : Close;
  end (* case *)
  end;
end;

Procedure TMesOk2Form.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesOk2Form.SetText1 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label1.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=AText;
End;

Procedure TMesOk2Form.SetText2 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label2.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label2.Caption:=AText;
End;

procedure TMesOk2Form.ZSpeedButton1Click(Sender: TObject);
begin
 Close;
end;

//initialization
//{$I mmesok2a.lrs}

end.
 