unit MMesYNC;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType;

type

  { TMesYNCForm }

  TMesYNCForm = class(TForm)
    BtnCancel: TSpeedButton;
    BtnNo: TSpeedButton;
    BtnYes: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
    procedure BtnYesClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FResult   : byte;

  public
    { public declarations }
    Procedure SetHeader ( Const AText : string );
    Procedure SetText ( Const ATextA, ATextB : string );

    Property Result : byte read FResult;
  end;

implementation

{$R *.lfm}

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

procedure TMesYNCForm.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
end;

Procedure TMesYNCForm.SetHeader ( Const AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesYNCForm.SetText ( Const ATextA, ATextB : string );
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
 BtnYes.Top:=Label2.Top+Label2.Height+8; BtnYes.Left:=ClientWidth-BtnYes.Width-4-BtnNo.Width-4-BtnCancel.Width-4;
 BtnNo.Top:=BtnYes.Top; BtnNo.Left:=ClientWidth-BtnNo.Width-4-BtnCancel.Width-4;
 BtnCancel.Top:=BtnYes.Top; BtnCancel.Left:=ClientWidth-BtnCancel.Width-4;
 ClientHeight:=BtnYes.Top+BtnYes.Height+8;
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


//initialization
//  {$I mmesync.lrs}

end.

