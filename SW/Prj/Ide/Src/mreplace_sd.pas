unit MReplace_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, SynEditTypes, LCLType;

type

  { TReplText }

  TReplText = class(TForm)
    BtnCancel: TSpeedButton;
    BtnOk: TSpeedButton;
    BtnAll: TSpeedButton;
    CbCaseSensitive: TCheckBox;
    CbTextToFind: TComboBox;
    CbTextToRepl: TComboBox;
    RgDirection: TRadioGroup;
    GbOptions: TGroupBox;
    LbTextToRepl: TLabel;
    RgOrigin: TRadioGroup;
    RgScope: TRadioGroup;
    CbWholeWordsOnly: TCheckBox;
    LbTextToFind: TLabel;
    CbPromptOnReplace: TCheckBox;
    procedure BtnAllClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure CbTextToFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CbTextToReplKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FParams     : string;
    FOptions    : TSynSearchOptions;
    FResult     : boolean;
    FTextToFind : string;

    Procedure SetParamsA;
    Function GetTextToFind : string;
    Procedure SetTextToFind ( Const AData : string );
    Function GetTextToRepl : string;
    Procedure SetTextToRepl ( Const AData : string );

    Procedure UpdateDropDownListA ( AComboBox : TComboBox );
    Procedure SaveOptions ( AAll : boolean );

  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SetParams ( AParams : string );
    Function GetParams : string;

    property TextToFind : string read GetTextToFind write SetTextToFind;
    property TextToRepl : string read GetTextToRepl write SetTextToRepl;
    property Result : boolean read FResult;
    property Options : TSynSearchOptions read FOptions;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

{ TReplText }

Constructor TReplText.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TReplText.Destroy;
Begin
 Inherited;
End;

Procedure TReplText.FormShow ( Sender : TObject );
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
 BWidthA:=4+LbTextToFind.Width+4+CbTextToFind.Width+4;
 if BWidthA>BMaxWidth then BMaxWidth:=BWidthA;
 BWidthA:=4+LbTextToRepl.Width+4+CbTextToRepl.Width+4;
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

 CbTextToFind.Left:=BMaxWidth-4-CbTextToFind.Width; CbTextToFind.Top:=4;
 LbTextToFind.Left:=CbTextToFind.Left-4-LbTextToFind.Width; LbTextToFind.Top:=CbTextToFind.Top+((CbTextToFind.Height-LbTextToFind.Height) div 2);

 CbTextToRepl.Left:=BMaxWidth-4-CbTextToRepl.Width; CbTextToRepl.Top:=CbTextToFind.Top+CbTextToFind.Height+4;
 LbTextToRepl.Left:=CbTextToRepl.Left-4-LbTextToRepl.Width; LbTextToRepl.Top:=CbTextToRepl.Top+((CbTextToRepl.Height-LbTextToRepl.Height) div 2);

 BWidthA:=(BMaxWidth-4-4-4) div 2;
 if BWidthA>BWidthB then BWidthB:=BWidthA;

 BTop:=CbTextToRepl.Top+CbTextToRepl.Height+4;
 GbOptions.AutoSize:=FALSE; GbOptions.SetBounds(4,BTop,BWidthB,BHeightB);
 RgOrigin.AutoSize:=FALSE; RgOrigin.SetBounds(4+BWidthB+4,BTop,BWidthB,BHeightB);
 BTop:=GbOptions.Top+GbOptions.Height+4;
 RgScope.AutoSize:=FALSE; RgScope.SetBounds(4,BTop,BWidthB,BHeightB);
 RgDirection.AutoSize:=FALSE; RgDirection.SetBounds(4+BWidthB+4,BTop,BWidthB,BHeightB);
 BTop:=RgScope.Top+RgScope.Height+4;
 BtnCancel.Left:=BMaxWidth-4-BtnCancel.Width;BtnCancel.Top:=BTop;
 BtnOk.Left:=BtnCancel.Left-4-BtnOk.Width; BtnOk.Top:=BTop;
 BtnAll.Left:=BtnOk.Left-4-BtnAll.Width; BtnAll.Top:=BTop;

 CbCaseSensitive.Left:=4;
 CbWholeWordsOnly.Left:=4; CbWholeWordsOnly.Top:=CbCaseSensitive.Top+CbCaseSensitive.Height;
 CbPromptOnReplace.Left:=4; CbPromptOnReplace.Top:=CbWholeWordsOnly.Top+CbWholeWordsOnly.Height;

 BHeightA:=8+CbTextToFind.Height+4+CbTextToRepl.Height+4+RgOrigin.Height+4+RgDirection.Height+4+BtnOk.Height+4;

 if Width<>BMaxWidth then ClientWidth:=BMaxWidth;
 if Height<>BHeightA then ClientHeight:=BHeightA;

 SetParamsA;
 //EndFormUpdate;
End;

procedure TReplText.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 CloseAction:=caHide;
end;

procedure TReplText.CbTextToFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
   VK_ESCAPE :
    begin
    Close;
    end;

   VK_RETURN :
    begin
    SaveOptions(FALSE);
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

procedure TReplText.CbTextToReplKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
   VK_ESCAPE :
    begin
    Close;
    end;

   VK_RETURN :
    begin
    SaveOptions(FALSE);
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)

end;

procedure TReplText.BtnOkClick(Sender: TObject);
begin
 SaveOptions(FALSE);
 FResult:=TRUE;
 Close;
end;

procedure TReplText.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TReplText.BtnAllClick(Sender: TObject);
begin
 SaveOptions(TRUE);
 FResult:=TRUE;
 Close;
end;

procedure TReplText.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
   VK_ESCAPE :
    begin
    Close;
    end;

   VK_RETURN :
    begin
    SaveOptions(FALSE);
    FResult:=TRUE;
    Close;
    end;

 end; (* case *)
end;

Procedure TReplText.SaveOptions ( AAll : boolean );
Begin
 UpdateDropDownListA(CbTextToFind);
 UpdateDropDownListA(CbTextToRepl);

 FOptions:=[ssoReplace];
 if AAll then FOptions:=FOptions+[ssoReplaceAll];
 if RgDirection.ItemIndex=1 then FOptions:=FOptions+[ssoBackwards];
 if RgScope.ItemIndex=1 then FOptions:=FOptions+[ssoSelectedOnly];
 if RgOrigin.ItemIndex=1 then FOptions:=FOptions+[ssoEntireScope];
 if CbCaseSensitive.Checked then FOptions:=FOptions+[ssoMatchCase];
 if CbWholeWordsOnly.Checked then FOptions:=FOptions+[ssoWholeWord];
 if CbPromptOnReplace.Checked then FOptions:=FOptions+[ssoPrompt];

End;

// ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt

Procedure TReplText.SetParams ( AParams : string );
Begin
 FParams:=AParams;
End;

Function TReplText.GetParams : string;
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

Procedure TReplText.SetParamsA;
Var
  BDummyS       : string;
  BParamsI      : Cardinal;
  BParams       : string;
Begin
 CbTextToFind.Items.Clear;
 CbTextToRepl.Items.Clear;

 CbTextToFind.Text:='';
 CbTextToRepl.Text:='';

 BParamsI:=0; // Compiler satisfaction

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
 CbPromptOnReplace.Checked:=ssoPrompt in FOptions;

 CbTextToFind.Text:=FTextToFind;
 CbTextToRepl.Text:='';
 until TRUE;
End;

Function TReplText.GetTextToFind : string;
Begin
 Result:=CbTextToFind.Text;
End;

Procedure TReplText.SetTextToFind ( Const AData : string );
Begin
 FTextToFind:=AData;
 CbTextToFind.Text:=AData;
End;

Function TReplText.GetTextToRepl : string;
Begin
 Result:=CbTextToRepl.Text;
End;

Procedure TReplText.SetTextToRepl ( Const AData : string );
Begin
 CbTextToRepl.Text:=AData;
End;

Procedure TReplText.UpdateDropDownListA ( AComboBox : TComboBox );
Var
  BDummyS       : string;
  i             : Integer;

Begin
 BDummyS:=AComboBox.Text;
 repeat
 if BDummyS='' then break;
 i:=0;
 while i<AComboBox.Items.Count do
  begin
  if BDummyS=AComboBox.Items.Strings[i] then
   begin
   AComboBox.Items.Delete(i);
   AComboBox.Text:=BDummyS;
   end
  else
   begin
   inc(i);
   end;
  end;
 while AComboBox.Items.Count>31 do AComboBox.Items.Delete(AComboBox.Items.Count-1);
 AComboBox.Items.Insert(0,BDummyS);
 until TRUE;
End;

//initialization
//  {$I mreplace.lrs}

end.

