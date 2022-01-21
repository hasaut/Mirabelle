unit MChoose3a;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  LCLType;

type

  { TMChooseForm }

  TMChooseForm = class(TForm)
    BtnOk: TSpeedButton;
    BtnCancel: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FLabelList  : array of TLabel;
    FRadioList  : array of TRadioButton;

    FSized      : boolean;
    FResult     : Integer;

    FTop        : Integer;

    Procedure SetSizes;
    Procedure RadioClick ( Sender : TObject );

  public
    { Public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure IncTop ( AData : Integer );

    Procedure SetHeader ( AText : string );
    Procedure AddText ( AText : string );
    Procedure AddRadio ( AText : string );

    property Result : Integer read FResult;
  end;

implementation

Constructor TMChooseForm.Create ( AOwner : TComponent );
Begin
 Inherited Create(AOwner);

 FTop:=4;
End;

Destructor TMChooseForm.Destroy;
Var
  BIndex   : Integer;

Begin
 BIndex:=0;
 while BIndex<Length(FLabelList) do
  begin
  FLabelList[BIndex].Free;
  inc(BIndex);
  end;
 FLabelList:=nil;

 BIndex:=0;
 while BIndex<Length(FRadioList) do
  begin
  FRadioList[BIndex].Free;
  inc(BIndex);
  end;
 FRadioList:=nil;

 Inherited Destroy;
End;

procedure TMChooseForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_RETURN : Close;
   VK_ESCAPE : begin
               FResult:=0;
               Close;
               end;
 end (* case *)
end;

Procedure TMChooseForm.SetHeader ( AText : string );
Begin
 Caption:=AText;
End;

Procedure TMChooseForm.AddText ( AText : string );
Var
  BTextWidth    : Integer;
  BLabel        : TLabel;
  BIndex        : Integer;

Begin
 BLabel:=TLabel.Create(Self);
 BLabel.Name:='';
 BLabel.Top:=FTop;
 inc(FTop,13);
 BLabel.Left:=0;
 InsertControl(BLabel);
 BLabel.Alignment:=taCenter;
 BLabel.AutoSize:=FALSE;
 BTextWidth:=BLabel.Canvas.TextWidth(AText)+8;
 if ClientWidth<BTextWidth then ClientWidth:=BTextWidth;
 BLabel.Width:=ClientWidth;
 BLabel.Caption:=AText;
 BIndex:=Length(FLabelList);
 SetLength(FLabelList,BIndex+1);
 FLabelList[BIndex]:=BLabel;
End;

Procedure TMChooseForm.AddRadio ( AText : string );
Var
  BWidth        : Integer;
  BRadio        : TRadioButton;
  BIndex        : Integer;

Begin
 BRadio:=TRadioButton.Create(Self);
 BRadio.Top:=FTop;
 inc(FTop,17);
 BRadio.Left:=0;
 BRadio.Caption:=AText;
 BRadio.OnClick:=@RadioClick;
 InsertControl(BRadio);
 BWidth:=Canvas.TextWidth(AText)+22;
 if BRadio.Width<BWidth then BRadio.Width:=BWidth;
 BRadio.Left:=16;
 BIndex:=Length(FRadioList);
 SetLength(FRadioList,BIndex+1);
 FRadioList[BIndex]:=BRadio;
 if BIndex=0 then BRadio.Checked:=TRUE;
End;

procedure TMChooseForm.BtnOkClick(Sender: TObject);
begin
 Close;
end;

procedure TMChooseForm.BtnCancelClick(Sender: TObject);
begin
 FResult:=0;
 Close;
end;

procedure TMChooseForm.FormPaint(Sender: TObject);
begin
  if FSized=FALSE then
   begin
   SetSizes;
   FSized:=TRUE;
   end;
end;

Procedure TMChooseForm.SetSizes;
Var
  BHeight       : Integer;

Begin
 BHeight:=FTop+60;
 if Height<BHeight then
  begin
  Height:=BHeight;
  BtnOk.Top:=BHeight-53;
  BtnCancel.Top:=BHeight-53;
  end;

 BtnOk.Left:=Width-149;
 BtnCancel.Left:=Width-79;
End;

Procedure TMChooseForm.IncTop ( AData : Integer );
Begin
 inc(FTop,AData);
End;

Procedure TMChooseForm.RadioClick ( Sender : TObject );
Var
  BRadio        : TRadioButton;
  BIndex        : Integer;

Begin
 BIndex:=0;
 FResult:=0;
 while BIndex<Length(FRadioList) do
  begin
  BRadio:=FRadioList[BIndex];
  if BRadio=Sender then
   begin
   FResult:=BIndex+1;
   break;
   end
  else
   begin
   BRadio.Checked:=FALSE;
   end;
  inc(BIndex);
  end;
End;

end.
