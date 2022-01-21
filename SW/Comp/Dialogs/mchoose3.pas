unit MChoose3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLType, Buttons;

type

  { TMChooseForm }

  TMChooseForm = class(TForm)
    BtnCancel: TSpeedButton;
    BtnOk: TSpeedButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FLabelListS,
    FRadioListS : TStringList;
    FLabelList  : array of TLabel;
    FRadioList  : array of TRadioButton;

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

{$R *.lfm}

Constructor TMChooseForm.Create ( AOwner : TComponent );
Begin
 Inherited Create(AOwner);

 FTop:=4;
 FLabelListS:=TStringList.Create;
 FRadioListS:=TStringList.Create;
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

 FRadioListS.Free;
 FLabelListS.Free;

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
Begin
 FLabelListS.Append(AText);
End;

Procedure TMChooseForm.AddRadio ( AText : string );
Begin
 FRadioListS.Append(AText);
End;

Procedure TMChooseForm.FormShow(Sender: TObject);
Var
  BTextWidth    : Integer;
  BLabel        : TLabel;
  BRadio        : TRadioButton;
  BLineIdx      : Integer;
  BLineS        : string;
  BIndex        : Integer;
  BTop          : Integer;
  BMaxWidth     : Integer;
Begin
 BTop:=FTop;
 for BLineIdx:=0 to FLabelListS.Count-1 do
  begin
  BLineS:=FLabelListS.Strings[BLineIdx];
  BLabel:=TLabel.Create(Self); BLabel.Name:='';
  InsertControl(BLabel);
  BIndex:=Length(FLabelList); SetLength(FLabelList,BIndex+1); FLabelList[BIndex]:=BLabel;
  BLabel.Caption:=BLineS;
  BLabel.AutoSize:=TRUE;
  BLabel.Left:=4; BLabel.Top:=BTop; inc(BTop,BLabel.Height);
  end;

 BTop:=BTop+4;
 for BLineIdx:=0 to FRadioListS.Count-1 do
  begin
  BLineS:=FRadioListS.Strings[BLineIdx];
  BRadio:=TRadioButton.Create(Self); BRadio.Name:='';
  InsertControl(BRadio);
  BIndex:=Length(FRadioList); SetLength(FRadioList,BIndex+1); FRadioList[BIndex]:=BRadio;
  BRadio.Caption:=BLineS;
  BRadio.Left:=4; BRadio.Top:=BTop; inc(BTop,BRadio.Height);
  BRadio.OnClick:=@RadioClick;
  if BLineIdx=0 then BRadio.Checked:=TRUE;
  end;

 BMaxWidth:=0;
 for BIndex:=0 to Length(FLabelList)-1 do
  begin
  BTextWidth:=FLabelList[BIndex].Width;
  if BTextWidth>BMaxWidth then BMaxWidth:=BTextWidth;
  end;
 for BIndex:=0 to Length(FRadioList)-1 do
  begin
  BTextWidth:=FRadioList[BIndex].Width;
  if BTextWidth>BMaxWidth then BMaxWidth:=BTextWidth;
  end;
 BMaxWidth:=BMaxWidth+8;
 if ClientWidth<BMaxWidth then ClientWidth:=BMaxWidth;

 FTop:=BTop;
 FormStyle:=fsStayOnTop;
 SetSizes;
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

Procedure TMChooseForm.SetSizes;
Var
  BHeight       : Integer;

Begin
 BHeight:=FTop+BtnOk.Height+16;
 if ClientHeight<BHeight then
  begin
  ClientHeight:=BHeight;
  BtnOk.Top:=BHeight-BtnOk.Height-8;
  BtnCancel.Top:=BtnOk.Top;
  end;

 BtnOk.Left:=ClientWidth-BtnOk.Width-4-BtnCancel.Width-4;
 BtnCancel.Left:=ClientWidth-BtnCancel.Width-4;
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


//initialization
//  {$I mchoose3.lrs}

end.

