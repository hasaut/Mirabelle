unit MMesOk2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType;

type

  { TMesOk2Form }

  TMesOk2Form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ZSpeedButton1: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ZSpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText1 ( AText : string );
    Procedure SetText2 ( AText : string );
  end;

implementation

{$R *.lfm}

{ TMesOk2Form }

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

procedure TMesOk2Form.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
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
  //{$I mmesok2.lrs}

end.

