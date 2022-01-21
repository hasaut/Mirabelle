unit MGoLine;

{$mode objfpc}{$H+}

interface

uses
  LResources, LCLType,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls;

type

  { TGotoLine }

  TGotoLine = class(TForm)
    ZLabel1: TLabel;
    ComboBox1: TComboBox;
    BtnOk: TSpeedButton;
    BtnCancel: TSpeedButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FParams     : TStringList;
    FResult     : boolean;
    FLinesCount : Integer;

    Function GetLineNumber : string;
    Procedure SetLineNumber ( AData : string );
    Procedure UpdateDropDownListA ( AData : string );
    Procedure AppendText;

  public
    { Public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetParams ( AParams : TStringList );

    property LineNumber : string read GetLineNumber write SetLineNumber;
    property Result : boolean read FResult;
    property LinesCount : Integer read FLinesCount write FLinesCount;
  end;

implementation

{$R *.lfm}

Constructor TGotoLine.Create ( AOwner : TComponent );
Begin
 Inherited Create(AOwner);
 FParams:=nil;
End;

Destructor TGotoLine.Destroy;
Var
  i             : Integer;
  BDummyS       : string;

Begin
 if FParams<>nil then
  begin
  i:=0;
  while i<ComboBox1.Items.Count do
   begin
   BDummyS:=ComboBox1.Items.Strings[i];
   FParams.Values['GotoLineLast'+IntToStr(i)]:=BDummyS;
   inc(i);
   end;

  while i<16 do
   begin
   FParams.Values['GotoLineLast'+IntToStr(i)]:='';
   inc(i);
   end;

  end;

 Inherited Destroy;
End;

Procedure TGotoLine.SetParams ( AParams : TStringList );
Var
  BDummyS       : string;
  i             : Integer;

Begin
 FParams:=AParams;
 ComboBox1.Items.Clear;

 repeat
 if FParams=nil then break;

 i:=0;
 while i<16 do
  begin
  BDummyS:=FParams.Values['GotoLineLast'+IntToStr(i)];
  if BDummyS='' then break;
  ComboBox1.Items.Append(BDummyS);
  inc(i);
  end;

 until TRUE;
End;

Function TGotoLine.GetLineNumber : string;
Begin
 Result:=ComboBox1.Text;
End;

Procedure TGotoLine.SetLineNumber ( AData : string );
Begin
 ComboBox1.Text:=AData;
End;

procedure TGotoLine.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TGotoLine.BtnOkClick(Sender: TObject);
begin
 FResult:=TRUE;
 Close;
end;

procedure TGotoLine.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_ESCAPE :
    begin
    Close;
    end;

   VK_RETURN :
    begin
    AppendText;
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

procedure TGotoLine.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_ESCAPE :
    begin
    Close;
    end;

   VK_RETURN :
    begin
    AppendText;
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

Procedure TGotoLine.UpdateDropDownListA ( AData : string );
Var
  BDummyS       : string;
  i             : Integer;

Begin
 BDummyS:=AData;
 repeat
 if BDummyS='' then break;
 i:=0;
 while i<ComboBox1.Items.Count do
  begin
  if BDummyS=ComboBox1.Items.Strings[i] then
   begin
   ComboBox1.Items.Delete(i);
   ComboBox1.Text:=BDummyS;
   end
  else
   begin
   inc(i);
   end;
  end;
 while ComboBox1.Items.Count>31 do ComboBox1.Items.Delete(ComboBox1.Items.Count-1);
 ComboBox1.Items.Insert(0,BDummyS);
 until TRUE;
End;

Procedure TGotoLine.AppendText;
Begin
 UpdateDropDownListA(ComboBox1.Text);
End;

Procedure TGotoLine.ComboBox1Change(Sender: TObject);
Var
  BDummyS       : string;
  BDummyF       : Extended;
  BLine         : Integer;

Begin
 repeat
 BDummyS:=ComboBox1.Text;
 if BDummyS='' then
  begin
  ComboBox1.Color:=clRed;
  break;
  end;
 if TextToFloat(PChar(BDummyS),BDummyF,fvExtended)=FALSE then
  begin
  ComboBox1.Color:=clRed;
  break;
  end;
 BLine:=Round(BDummyF);
 if (BLine=0) or (BLine>FLinesCount) then
  begin
  ComboBox1.Color:=clYellow;
  break;
  end;
 ComboBox1.Color:=clWindow;
 until TRUE;
End;

procedure TGotoLine.FormShow(Sender: TObject);
begin
 FormStyle:=fsStayOnTop;
end;

//initialization
//  {$I mgoline.lrs}

end.
