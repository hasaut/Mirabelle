unit MMesYN;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  LCLType;

type

  { TMesYesNoForm }

  TMesYesNoForm = class(TForm)
    BtnYes: TSpeedButton;
    BtnNo: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnYesClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
  private
    { Private declarations }
    FResult     : boolean;

  public
    { Public declarations }
    Procedure SetHeader ( Const AText : string );
    Procedure SetText ( Const ATextA, ATextB : string );

    Property Result : boolean read FResult;
  end;

implementation

{$R *.lfm}

procedure TMesYesNoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_RETURN : Close;
   VK_ESCAPE : Close;
 end (* case *)
end;

Procedure TMesYesNoForm.SetHeader ( Const AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesYesNoForm.SetText ( Const ATextA, ATextB : string );
Var
  BTextWidthA,
  BTextWidthB   : Integer;
  BTextWidth    : Integer;
Begin
 BTextWidthA:=Label1.Canvas.TextWidth(ATextA)+8;
 BTextWidthB:=Label2.Canvas.TextWidth(ATextB)+8;
 BTextWidth:=BTextWidthA; if BTextWidthB>BTextWidth then BTextWidth:=BTextWidthB;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=ATextA;
 Label2.Caption:=ATextB;
 BtnYes.Top:=Label2.Top+Label2.Height+8; BtnYes.Left:=ClientWidth-BtnYes.Width-4-BtnNo.Width-4;
 BtnNo.Top:=BtnYes.Top; BtnNo.Left:=ClientWidth-BtnNo.Width-4;
 ClientHeight:=BtnYes.Top+BtnYes.Height+8;
End;

procedure TMesYesNoForm.BtnYesClick(Sender: TObject);
begin
 FResult:=TRUE;
 Close;
end;

procedure TMesYesNoForm.BtnNoClick(Sender: TObject);
begin
 Close;
end;

end.
 
