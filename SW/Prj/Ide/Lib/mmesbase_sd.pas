unit MMesBase_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  LCLType;

type

  { TMesBaseForm }

  TMesBaseForm = class(TForm)
    BtImages: TImageList;
    ImgText: TImage;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FLines      : TStringList;
    FBtnList    : array of TSpeedButton;
    FBitmap     : TBitmap;

    FRetBtn     : Integer;
    FEscExit    : Integer;

    procedure BtnClick ( ASender : TObject );
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetParams ( Const AHeader, AText : string; ABtnList : string; AEscExit : Integer );

    property RetBtn : Integer read FRetBtn;
  end;

Function VpMesOk ( ASender : TComponent; Const AHeader, AText : string ) : Integer;
Function VpMesYN ( ASender : TComponent; Const AHeader, AText : string ) : Integer;

implementation

Uses
  ConComL, ConComI;

{$R *.lfm}

Function VpMesOk ( ASender : TComponent; Const AHeader, AText : string ) : Integer;
Var
  BDialog   : TMesBaseForm;
Begin
 BDialog:=TMesBaseForm.Create(ASender);
 BDialog.FormStyle:=fsStayOnTop;
 BDialog.SetParams(AHeader,AText,'0 OK',0);
 BDialog.ShowModal;
 Result:=BDialog.RetBtn;
 BDialog.Free;
End;

Function VpMesYN ( ASender : TComponent; Const AHeader, AText : string ) : Integer;
Var
  BDialog   : TMesBaseForm;
Begin
 BDialog:=TMesBaseForm.Create(ASender);
 BDialog.FormStyle:=fsStayOnTop;
 BDialog.SetParams(AHeader,AText,'0 Yes 1 No',1);
 BDialog.ShowModal;
 Result:=BDialog.RetBtn;
 BDialog.Free;
End;

// TMesBaseForm

Constructor TMesBaseForm.Create ( AOwner : TComponent );
Begin
 Inherited;
 FLines:=TStringList.Create;
 FBitmap:=TBitmap.Create;
 FRetBtn:=-1;
 BorderIcons:=[];
End;

Destructor TMesBaseForm.Destroy;
Begin
 FBitmap.Free;
 FLines.Free;
 Inherited;
End;

Procedure TMesBaseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction );
Var
  BBtnIdx   : Integer;
  BButton   : TSpeedButton;
Begin
 BBtnIdx:=0;
 while BBtnIdx<Length(FBtnList) do
  begin
  BButton:=FBtnList[BBtnIdx];
  RemoveControl(BButton);
  BButton.Free;
  inc(BBtnIdx);
  end;
 FBtnList:=nil;
End;

Procedure TMesBaseForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 repeat
 if (Key=VK_ESCAPE) and (not (ssShift in Shift)) then
  begin
  FRetBtn:=FEscExit;
  Close;
  break;
  end;
 until TRUE;
End;

Procedure TMesBaseForm.SetParams  ( Const AHeader, AText : string; ABtnList : string; AEscExit : Integer );
Var
  BText,
  BParam        : string;
  BWidthThis,
  BWidthMax     : Integer;
  BLineIdx      : Integer;
  BLineHeight   : Integer;
  BBtnList      : string;
  BBtnIdx       : Integer;
  BButton       : TSpeedButton;
  BImageIdx     : Integer;
  BLen          : Integer;
Begin
 Caption:=AHeader;
 BText:=AText;
 repeat
 BParam:=ReadTillC(BText,#13);
 if BParam='' then break;
 FLines.Append(BParam);
 until FALSE;

 FBitmap.Canvas.Font.Assign(Font);

 BWidthMax:=0;
 BLineIdx:=0;
 while BLineIdx<FLines.Count do
  begin
  BWidthThis:=FBitmap.Canvas.TextWidth(FLines.Strings[BLineIdx]);
  if BWidthThis>BWidthMax then BWidthMax:=BWidthThis;
  inc(BLineIdx);
  end;
 BLineHeight:=FBitmap.Canvas.TextHeight('Ayla');

 FBitmap.SetSize(BWidthMax,BLineHeight*FLines.Count);
 BmpBox(FBitmap,clForm);

 BLineIdx:=0;
 while BLineIdx<FLines.Count do
  begin
  BWidthThis:=FBitmap.Canvas.TextWidth(FLines.Strings[BLineIdx]);
  FBitmap.Canvas.TextOut((BWidthMax-BWidthThis) div 2,BLineIdx*BLineHeight,FLines.Strings[BLineIdx]);
  if BWidthThis>BWidthMax then BWidthMax:=BWidthThis;
  inc(BLineIdx);
  end;

 if BLineHeight<16 then BLineHeight:=16;
 BLineHeight:=BLineHeight+8;
 ClientWidth:=FBitmap.Width+8;
 ClientHeight:=4+FBitmap.Height+4+BLineHeight+4;
 Position:=poOwnerFormCenter;

 BWidthMax:=0;
 BBtnIdx:=0;
 BBtnList:=ABtnList;
 repeat
 BParam:=ReadParamStr(BBtnList);
 if BParam='' then break;
 BButton:=TSpeedButton.Create(Self);
 SetLength(FBtnList,BBtnIdx+1); FBtnList[BBtnIdx]:=BButton;
 TryStrToInt(BParam,BImageIdx);
 InsertControl(BButton); BButton.Top:=4+FBitmap.Height+4;
 BButton.AutoSize:=FALSE; BButton.Height:=BLineHeight; BButton.Images:=BtImages; BButton.ImageIndex:=BImageIdx;
 BParam:=ReadParamStr(BBtnList);
 BWidthThis:=BButton.Canvas.TextWidth(BParam);
 if BWidthThis>BWidthMax then BWidthMax:=BWidthThis; BButton.Caption:=BParam;
 BButton.OnClick:=@BtnClick;
 inc(BBtnIdx);
 until FALSE;

 FEscExit:=AEscExit;

 BWidthMax:=BWidthMax+16+16;

 BBtnIdx:=0; BLen:=Length(FBtnList);
 while BBtnIdx<BLen do
  begin
  BButton:=FBtnList[BBtnIdx];
  BButton.Width:=BWidthMax; BButton.Left:=ClientWidth-(BLen-BBtnIdx)*(BWidthMax+4);
  inc(BBtnIdx);
  end;

 ImgText.SetBounds(4,4,FBitmap.Width,FBitmap.Height);
 ImgText.Canvas.Draw(0,0,FBitmap);
End;

Procedure TMesBaseForm.BtnClick ( ASender : TObject );
Var
  BBtnIdx   : Integer;
Begin
 BBtnIdx:=0;
 while BBtnIdx<Length(FBtnList) do
  begin
  if FBtnList[BBtnIdx]=ASender then break;
  inc(BBtnIdx);
  end;
 FRetBtn:=BBtnIdx;
 Close;
End;

end.

