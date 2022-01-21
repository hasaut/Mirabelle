unit DColorScheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ColorBox, Buttons, ColorScheme, AFile;

type

  { TColorSchemeDialog }

  TColorSchemeDialog = class(TForm)
    BtnCancel: TSpeedButton;
    BtnYes: TSpeedButton;
    CbItalic: TCheckBox;
    CbColorScheme: TComboBox;
    CbEditorBg: TColorBox;
    CbFgColor: TColorBox;
    CbBgColor: TColorBox;
    CbBold: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LbItems: TListBox;
    PnlBottom: TPanel;
    PnlRight: TPanel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnYesClick(Sender: TObject);
    procedure CbBgColorChange(Sender: TObject);
    procedure CbBoldChange(Sender: TObject);
    procedure CbColorSchemeChange(Sender: TObject);
    procedure CbEditorBgChange(Sender: TObject);
    procedure CbFgColorChange(Sender: TObject);
    procedure CbItalicChange(Sender: TObject);
    procedure LbItemsClick(Sender: TObject);
    procedure LbItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FActiveFile         : TActiveFile;
    FColorScheme        : TColorScheme;
    FDefaultScheme      : TColorScheme;
    FResultA            : boolean;
    FSelfChange         : Integer;

    Procedure SetItemAttri ( AIndex : Integer );
  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetColors ( Const AColorScheme : TColorScheme; Const ANames : string; Const AExt : string );
    Procedure SetFont ( Const AFontName : string; ASize : Integer );
    Procedure AddExampleLine ( Const AStr : string );

    property ColorScheme : TColorScheme read FColorScheme;
    property ResultA : boolean read FResultA;
    property DefaultScheme : TColorScheme read FDefaultScheme write FDefaultScheme;
  end; 

implementation

{$R *.lfm}

Uses
  ConComL;

Constructor TColorSchemeDialog.Create ( AOwner : TComponent );
Begin
 Inherited;
 FActiveFile:=TActiveFile.Create(Self);
 InsertControl(FActiveFile);
 FActiveFile.Align:=alClient;
End;

Destructor TColorSchemeDialog.Destroy;
Begin
 RemoveControl(FActiveFile);
 FActiveFile.Free;
 Inherited;
End;

Procedure TColorSchemeDialog.SetColors ( Const AColorScheme : TColorScheme; Const ANames : string; Const AExt : string );
Var
  BNames        : string;
  BNameA        : string;
Begin
 FColorScheme:=AColorScheme;
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 FActiveFile.SetHighlighter(AExt);
 LbItems.Items.Clear;
 BNames:=ANames;
 repeat
 BNameA:=ReadParamStr(BNames);
 if BNameA='' then break;
 LbItems.Items.Append(BNameA);
 until FALSE;

 CbEditorBg.Selected:=AColorScheme.FEditorColor;
 SetItemattri(0);
End;

Procedure TColorSchemeDialog.SetFont ( Const AFontName : string; ASize : Integer );
Begin
 FActiveFile.SetFont(AFontName,ASize);
End;

procedure TColorSchemeDialog.LbItemsClick(Sender: TObject);
begin
 SetItemAttri(LbItems.ItemIndex);
end;

procedure TColorSchemeDialog.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TColorSchemeDialog.BtnYesClick(Sender: TObject);
begin
 FResultA:=TRUE;
 Close;
end;

procedure TColorSchemeDialog.CbColorSchemeChange(Sender: TObject);
begin
 if CbColorScheme.ItemIndex=1 then
  begin
  FColorScheme:=FDefaultScheme;
  SetItemAttri(LbItems.ItemIndex);
  FActiveFile.SetEditorColorAsm(FColorScheme);
  FActiveFile.SetEditorColorPas(FColorScheme);
  FActiveFile.SetEditorColorVhd(FColorScheme);
  end;
end;

procedure TColorSchemeDialog.CbEditorBgChange(Sender: TObject);
begin
 repeat
 if FSelfChange>0 then break;
 FColorScheme.FEditorColor:=CbEditorBg.Selected;
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 until TRUE;
end;

procedure TColorSchemeDialog.CbFgColorChange(Sender: TObject);
begin
 repeat
 if FSelfChange>0 then break;
 if LbItems.ItemIndex>9 then break;
 FColorScheme.FAttriList[LbItems.ItemIndex].FFgColor:=CbFgColor.Selected;
 SetItemAttri(LbItems.ItemIndex);
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 until TRUE;
end;

procedure TColorSchemeDialog.CbBgColorChange(Sender: TObject);
begin
 repeat
 if FSelfChange>0 then break;
 if LbItems.ItemIndex>9 then break;
 FColorScheme.FAttriList[LbItems.ItemIndex].FBgColor:=CbBgColor.Selected;
 SetItemAttri(LbItems.ItemIndex);
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 until TRUE;
end;

procedure TColorSchemeDialog.CbBoldChange(Sender: TObject);
begin
 repeat
 if FSelfChange>0 then break;
 if LbItems.ItemIndex>9 then break;
 if CbBold.Checked then FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle:=FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle+[fsBold]
 else FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle:=FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle-[fsBold];
 SetItemAttri(LbItems.ItemIndex);
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 until TRUE;
end;

procedure TColorSchemeDialog.CbItalicChange(Sender: TObject);
begin
 repeat
 if FSelfChange>0 then break;
 if LbItems.ItemIndex>9 then break;
 if CbItalic.Checked then FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle:=FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle+[fsItalic]
 else FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle:=FColorScheme.FAttriList[LbItems.ItemIndex].FFontStyle-[fsItalic];
 SetItemAttri(LbItems.ItemIndex);
 FActiveFile.SetEditorColorAsm(FColorScheme);
 FActiveFile.SetEditorColorPas(FColorScheme);
 FActiveFile.SetEditorColorVhd(FColorScheme);
 until TRUE;
end;

procedure TColorSchemeDialog.LbItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 SetItemAttri(LbItems.ItemIndex);
end;

Procedure TColorSchemeDialog.SetItemAttri ( AIndex : Integer );
Begin
 inc(FSelfChange);
 repeat
 if AIndex>9 then break;
 CbFgColor.Selected:=FColorScheme.FAttriList[AIndex].FFgColor;
 CbBgColor.Selected:=FColorScheme.FAttriList[AIndex].FBgColor;
 CbBold.Checked:=fsBold in FColorScheme.FAttriList[AIndex].FFontStyle;
 CbItalic.Checked:=fsItalic in FColorScheme.FAttriList[AIndex].FFontStyle;
 until TRUE;
 dec(FSelfChange);
End;

Procedure TColorSchemeDialog.AddExampleLine ( Const AStr : string );
Begin
 FActiveFile.Edit.Lines.Append(AStr);
End;

//initialization
//  {$I dcolorscheme.lrs}

end.

