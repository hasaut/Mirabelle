unit AboutForm_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, resource;

type

  { TAboutFormSd }

  TAboutFormSd = class(TForm)
    BtClose: TButton;
    ImgLogo: TImage;
    MemChangeLog: TMemo;
    MemVersion: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    PcVersion: TPageControl;
    PnlBot: TPanel;
    TsChangeLog: TTabSheet;
    TsVersion: TTabSheet;
    procedure BtCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FVersion    : Cardinal;
    FChangeLog  : string;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetChangeLog ( AVersion : Cardinal; Const AChangeLog : string );
  end;

implementation

Uses
  ConComL;

{$R *.lfm}

Constructor TAboutFormSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TAboutFormSd.Destroy;
Begin
 Inherited;
End;

Procedure TAboutFormSd.FormShow(Sender: TObject);
Var
  BDateS,
  BTimeS        : string;
  BReadS,
  BParamS       : string;
  BVersion      : string;
Begin
 PcVersion.PageIndex:=0;
 MemVersion.Lines.Clear;
 MemChangeLog.Lines.Clear;

 Panel1.Width:=ImgLogo.Width; Panel2.Align:=alClient;

 //Load(HInstance);
 BDateS:={$I %DATE%};
 BTimeS:={$I %TIME%};

{ MemVersion.Lines.Append('File version: '+
                     IntToStr(FVersResource.FixedInfo.FileVersion[0])+'.'+
                     IntToStr(FVersResource.FixedInfo.FileVersion[1])+'.'+
                     IntToStr(FVersResource.FixedInfo.FileVersion[2])+'.'+
                     IntToStr(FVersResource.FixedInfo.FileVersion[3]));
 MemVersion.Lines.Append('Product version: '+
                     IntToStr(FVersResource.FixedInfo.ProductVersion[0])+'.'+
                     IntToStr(FVersResource.FixedInfo.ProductVersion[1])+'.'+
                     IntToStr(FVersResource.FixedInfo.ProductVersion[2])+'.'+
                     IntToStr(FVersResource.FixedInfo.ProductVersion[3])); }

 BVersion:=IntToStr((FVersion shr 24) and $FF)+'.'+
           IntToStr((FVersion shr 16) and $FF)+'.'+
           IntToStr((FVersion shr  8) and $FF)+'.'+
           IntToStr((FVersion shr  0) and $FF);
 MemVersion.Lines.Append(BVersion);
 MemVersion.Lines.Append('Build date: '+BDateS+' '+BTimeS);

 BReadS:=FChangeLog;
 repeat
 BParamS:=ReadTillC(BReadS,#13);
 if BParamS='' then break;
 MemChangeLog.Lines.Append(BParamS);
 until FALSE;

End;

Procedure TAboutFormSd.BtCloseClick(Sender: TObject);
Begin
 Close;
End;

Procedure TAboutFormSd.SetChangeLog ( AVersion : Cardinal; Const AChangeLog : string );
Begin
 FVersion:=AVersion;
 FChangeLog:=AChangeLog;
End;

end.


