unit MMesYN3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType;

type

  { TMesYN3Form }

  TMesYN3Form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BtnYes: TSpeedButton;
    BtnNo: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BtnYesClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
  private
    { Private declarations }
    FResult     : boolean;

  public
    { Public declarations }
    Procedure SetHeader ( AText : string );
    Procedure SetText ( Const ATextA, ATextB, ATextC : string );

    Property Result : boolean read FResult;
  end;

implementation

{$R *.lfm}

{ TMesYN3Form }

procedure TMesYN3Form.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if not (ssShift in Shift) then
  begin
  case Key of
   VK_RETURN:
     begin
     FResult:=TRUE;
     Close;
     end;
   VK_ESCAPE:
     Close;
   VK_Y:
     begin
     FResult:=TRUE;
     Close;
     end;
   VK_N:
     Close;
  end (* case *)
  end;
end;

procedure TMesYN3Form.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
end;

Procedure TMesYN3Form.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMesYN3Form.SetText ( Const ATextA, ATextB, ATextC : string );
Var
  BTextWidthA,
  BTextWidthB,
  BTextWidthC   : Integer;
  BTextWidth    : Integer;
Begin
 BTextWidthA:=Label1.Canvas.TextWidth(ATextA)+8;
 BTextWidthB:=Label2.Canvas.TextWidth(ATextB)+8;
 BTextWidthC:=Label3.Canvas.TextWidth(ATextC)+8;
 BTextWidth:=BTextWidthA;
 if BTextWidthB>BTextWidth then BTextWidth:=BTextWidthB;
 if BTextWidthC>BTextWidth then BTextWidth:=BTextWidthC;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 Label1.Caption:=ATextA;
 Label2.Caption:=ATextB;
 Label3.Caption:=ATextC;
 BtnYes.Top:=Label3.Top+Label3.Height+8; BtnYes.Left:=ClientWidth-BtnYes.Width-4-BtnNo.Width-4;
 BtnNo.Top:=BtnYes.Top; BtnNo.Left:=ClientWidth-BtnNo.Width-4;
 ClientHeight:=BtnYes.Top+BtnYes.Height+8;
End;

procedure TMesYN3Form.BtnYesClick(Sender: TObject);
begin
 FResult:=TRUE;
 Close;
end;

procedure TMesYN3Form.BtnNoClick(Sender: TObject);
begin
 Close;
end;


//initialization
//  {$I MMesYN3.lrs}

end.

