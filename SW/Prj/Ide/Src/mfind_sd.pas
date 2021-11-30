unit MFind_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, SynEditTypes, LCLType;

type

  { TFindText }

  TFindText = class(TForm)
    BtnCancel: TSpeedButton;
    BtnOk: TSpeedButton;
    CbCaseSensitive: TCheckBox;
    CbTextToFind: TComboBox;
    RgDirection: TRadioGroup;
    GbOptions: TGroupBox;
    RgOrigin: TRadioGroup;
    RgScope: TRadioGroup;
    CbWholeWordsOnly: TCheckBox;
    LbText: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure CbTextToFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FParams     : string;
    FOptions    : TSynSearchOptions;
    FResult     : boolean;

    Procedure SetParamsA;
    Function GetTextToFind : string;
    Procedure SetTextToFind ( Const AData : string );
    Procedure UpdateDropDownListA ( Const AData : string );
    Procedure AppendText;
    Procedure SaveOptions;

  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SetParams ( AParams : string );
    Function GetParams : string;

    property TextToFind : string read GetTextToFind write SetTextToFind;
    property Result : boolean read FResult;
    property Options : TSynSearchOptions read FOptions;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

{ TFindText }

Constructor TFindText.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TFindText.Destroy;
Begin
 Inherited;
End;

Procedure TFindText.FormShow(Sender: TObject);
Var
  BWidthA,
  BWidthB,
  BMaxWidth     : Integer;
  BHeightA,
  BHeightB      : Integer;
  BTop          : Integer;
Begin
 //BeginFormUpdate;
 BMaxWidth:=Width;
 BWidthA:=4+LbText.Width+4+CbTextToFind.Width+4;
 if BWidthA>BMaxWidth then BMaxWidth:=BWidthA;
 // Respect symmetry more or less
 BWidthB:=4+GbOptions.Width;
 if RgOrigin.Width>BWidthB then BWidthB:=RgOrigin.Width;
 if RgScope.Width>BWidthB then BWidthB:=RgScope.Width;
 if RgDirection.Width>BWidthB then BWidthB:=RgDirection.Width;
 BWidthA:=4+BWidthB+4+BWidthB+4;
 if BWidthA>BMaxWidth then BMaxWidth:=BWidthA;

 BHeightB:=GbOptions.Height;
 if RgOrigin.Height>BHeightB then BHeightB:=RgOrigin.Height;
 if RgScope.Height>BHeightB then BHeightB:=RgScope.Height;
 if RgDirection.Height>BHeightB then BHeightB:=RgDirection.Height;
 BHeightA:=8+CbTextToFind.Height+4+RgOrigin.Height+4+RgDirection.Height+4+BtnOk.Height+4;

 if Width<>BMaxWidth then ClientWidth:=BMaxWidth;
 if Height<>BHeightA then ClientHeight:=BHeightA;

 CbTextToFind.Left:=BMaxWidth-4-CbTextToFind.Width; CbTextToFind.Top:=4;
 LbText.Left:=CbTextToFind.Left-4-LbText.Width; LbText.Top:=CbTextToFind.Top+((CbTextToFind.Height-LbText.Height) div 2);

 BWidthA:=(BMaxWidth-4-4-4) div 2;
 if BWidthA>BWidthB then BWidthB:=BWidthA;

 BTop:=CbTextToFind.Top+CbTextToFind.Height+4;
 GbOptions.AutoSize:=FALSE; GbOptions.SetBounds(4,BTop,BWidthB,BHeightB);
 RgOrigin.AutoSize:=FALSE; RgOrigin.SetBounds(4+BWidthB+4,BTop,BWidthB,BHeightB);
 BTop:=GbOptions.Top+GbOptions.Height+4;
 RgScope.AutoSize:=FALSE; RgScope.SetBounds(4,BTop,BWidthB,BHeightB);
 RgDirection.AutoSize:=FALSE; RgDirection.SetBounds(4+BWidthB+4,BTop,BWidthB,BHeightB);
 BTop:=RgScope.Top+RgScope.Height+4;
 BtnCancel.Left:=BMaxWidth-4-BtnCancel.Width;BtnCancel.Top:=BTop;
 BtnOk.Left:=BtnCancel.Left-4-BtnOk.Width; BtnOk.Top:=BTop;

 CbCaseSensitive.Left:=4;
 CbWholeWordsOnly.Left:=4; CbWholeWordsOnly.Top:=CbCaseSensitive.Top+CbCaseSensitive.Height;

 SetParamsA;
 //EndFormUpdate;
End;

Procedure TFindText.FormClose(Sender: TObject; var CloseAction: TCloseAction);
Begin
 CloseAction:=caHide;
End;

procedure TFindText.CbTextToFindKeyDown(Sender: TObject; var Key: Word;
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
    SaveOptions;
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

procedure TFindText.BtnOkClick(Sender: TObject);
begin
 AppendText;
 SaveOptions;
 FResult:=TRUE;
 Close;
end;

procedure TFindText.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFindText.FormKeyDown(Sender: TObject; var Key: Word;
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
    SaveOptions;
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

Procedure TFindText.SaveOptions;
Begin
 FOptions:=[];
 if RgDirection.ItemIndex=1 then FOptions:=FOptions+[ssoBackwards];
 if RgScope.ItemIndex=1 then FOptions:=FOptions+[ssoSelectedOnly];
 if RgOrigin.ItemIndex=1 then FOptions:=FOptions+[ssoEntireScope];
 if CbCaseSensitive.Checked then FOptions:=FOptions+[ssoMatchCase];
 if CbWholeWordsOnly.Checked then FOptions:=FOptions+[ssoWholeWord];

 {if FParams<>nil then
  begin
  FParams.Values['FindTextOptions']:=IntToStr(Integer(FOptions));
  i:=0;
  while i<CbTextToFind.Items.Count do
   begin
   BDummyS:=CbTextToFind.Items.Strings[i];
   FParams.Values['FindTextLast'+IntToStr(i)]:=BDummyS;
   inc(i);
   end;
  while i<32 do
   begin
   FParams.Values['FindTextLast'+IntToStr(i)]:='';
   inc(i);
   end;
  end;}
 End;

// ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt

Procedure TFindText.SetParams ( AParams : string );
Begin
 FParams:=AParams;
End;

Function TFindText.GetParams : string;
Var
  BIndex    : Integer;
Begin
 Result:=IntToHex(Cardinal(FOptions),8);
 BIndex:=0;
 while BIndex<CbTextToFind.Items.Count do
  begin
  Result:=Result+' '+CbTextToFind.Items.Strings[BIndex];
  inc(BIndex);
  end;
End;

Procedure TFindText.SetParamsA;
Var
  BDummyS       : string;
  BParamsI      : Cardinal;
  BParams       : string;
Begin
 CbTextToFind.Items.Clear;

 repeat
 if FParams='' then break;
 BParams:=FParams;
 FOptions:=[];
 if HexToDWordCheck(ReadParamStr(BParams),BParamsI) then FOptions:=TSynSearchOptions(BParamsI);

 repeat
 BDummyS:=ReadParamStr(BParams);
 if BDummyS='' then break;
 CbTextToFind.Items.Append(BDummyS);
 until FALSE;

 if ssoBackwards    in FOptions then RgDirection.ItemIndex:=1 else RgDirection.ItemIndex:=0;
 if ssoSelectedOnly in FOptions then RgScope.ItemIndex:=1 else RgScope.ItemIndex:=0;
 if ssoEntireScope  in FOptions then RgOrigin.ItemIndex:=1 else RgOrigin.ItemIndex:=0;

 CbCaseSensitive.Checked:=ssoMatchCase in FOptions;
 CbWholeWordsOnly.Checked:=ssoWholeWord in FOptions;
 until TRUE;
End;

Function TFindText.GetTextToFind : string;
Begin
 Result:=CbTextToFind.Text;
End;

Procedure TFindText.SetTextToFind ( Const AData : string );
Begin
 CbTextToFind.Text:=AData;
End;

Procedure TFindText.UpdateDropDownListA ( Const AData : string );
Var
  BDummyS       : string;
  i             : Integer;

Begin
 BDummyS:=AData;
 repeat
 if BDummyS='' then break;
 i:=0;
 while i<CbTextToFind.Items.Count do
  begin
  if BDummyS=CbTextToFind.Items.Strings[i] then
   begin
   CbTextToFind.Items.Delete(i);
   CbTextToFind.Text:=BDummyS;
   end
  else
   begin
   inc(i);
   end;
  end;
 while CbTextToFind.Items.Count>31 do CbTextToFind.Items.Delete(CbTextToFind.Items.Count-1);
 CbTextToFind.Items.Insert(0,BDummyS);
 until TRUE;
End;

Procedure TFindText.AppendText;
Begin
 UpdateDropDownListA(CbTextToFind.Text);
End;

//initialization
//  {$I mfind.lrs}

end.

