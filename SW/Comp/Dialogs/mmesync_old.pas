unit MMesYNC_old;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  LCLType;

type
  TMesYNCForm = class(TForm)
    BtnYes: TSpeedButton;
    BtnNo: TSpeedButton;
    Label1: TLabel;
    BtnCancel: TSpeedButton;
    Label2: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnYesClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FResult     : byte;

  public                          
    { Public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText1 ( AText : string );
    Procedure SetText2 ( AText : string );

    Property Result : byte read FResult;
  end;

implementation

procedure TMesYNCForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Shift=[] then
  begin
  case Key of
   VK_RETURN,
   ord('Y') :
     begin
     FResult:=2;
     Close;
     end;
   ord('N') :
     begin
     FResult:=1;
     Close;
     end;
   VK_ESCAPE,
   ord('C') : Close;
  end (* case *)
  end;
end;

Procedure TMesYNCForm.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesYNCForm.SetText1 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label1.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=AText;
End;

Procedure TMesYNCForm.SetText2 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label2.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label2.Caption:=AText;
End;

procedure TMesYNCForm.BtnYesClick(Sender: TObject);
begin
 FResult:=2;
 Close;
end;

procedure TMesYNCForm.BtnNoClick(Sender: TObject);
begin
 FResult:=1;
 Close;
end;

procedure TMesYNCForm.BtnCancelClick(Sender: TObject);
begin
 FResult:=0;
 Close;
end;

end.
 
