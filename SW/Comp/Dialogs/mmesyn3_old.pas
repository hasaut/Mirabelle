unit MMesYN3_old;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  LCLType;

type
  TMesYN3Form = class(TForm)
    ZSpeedButton1: TSpeedButton;
    ZSpeedButton2: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZSpeedButton1Click(Sender: TObject);
    procedure ZSpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    FResult     : boolean;

  public                        
    { Public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText1 ( AText : string );
    Procedure SetText2 ( AText : string );
    Procedure SetText3 ( AText : string );

    Property Result : boolean read FResult;
  end;

implementation

procedure TMesYN3Form.FormKeyDown(Sender: TObject; var Key: Word;
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

Procedure TMesYN3Form.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesYN3Form.SetText1 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label1.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=AText;
End;

Procedure TMesYN3Form.SetText2 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label2.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label2.Caption:=AText;
End;

Procedure TMesYN3Form.SetText3 ( AText : string );
Var
  BTextWidth    : Integer;

Begin
 BTextWidth:=Label3.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label3.Caption:=AText;
End;

procedure TMesYN3Form.ZSpeedButton1Click(Sender: TObject);
begin
 FResult:=TRUE;
 Close;
end;

procedure TMesYN3Form.ZSpeedButton2Click(Sender: TObject);
begin
 Close;
end;

initialization
  {$I mmesyn3_old.lrs}

end.
 
