unit FrameInfo_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLIntf, LCLType, ExtCtrls, ComCtrls, Buttons,
  InfoViewBase_sd, AsmTypes_sd;

type
  { TWndInfoSd }
  TWndInfoSd = class(TFrame)
  private
    { private declarations }
    FTabSheet   : TTabSheet;

    FInfoView   : TInfoViewBase;
    FOnLocalizeError : TOnLocalizeError;

    Procedure ErrorClick ( Const AErrorCode : string );
  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet; AOnLocalizeError : TOnLocalizeError );
    Procedure Done;
    Procedure Clear;
    Procedure SetPrjPath ( Const APrjPath : string );
    Procedure AppendAny ( Const AMessage : string );
  end;

implementation

{$R *.lfm}

Constructor TWndInfoSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndInfoSd.Destroy;
Begin
 Clear;
 Inherited;
End;

Procedure TWndInfoSd.Init ( ASheet : TTabSheet; AonLocalizeError : TOnLocalizeError );
Begin
 FOnLocalizeError:=AOnLocalizeError;
 FTabSheet:=ASheet;
 FTabSheet.InsertControl(Self);
 FInfoView:=TInfoViewBase.Create(Self);
 FInfoView.OnErrorClick:=@ErrorClick;
 InsertControl(FInfoView);
 FInfoView.Align:=alClient;
 Align:=alClient;
 Clear;
End;

Procedure TWndInfoSd.Done;
Begin
 Clear;
 RemoveControl(FInfoView); FInfoView.Free; FInfoView:=nil;
 FTabSheet.RemoveControl(Self);
End;

Procedure TWndInfoSd.Clear;
Begin
 if FInfoView<>nil then FInfoView.Clear;
 FTabSheet.Caption:='Info (0)';
End;

Procedure TWndInfoSd.SetPrjPath ( Const APrjPath : string );
Begin
 FInfoView.PrjPath:=APrjPath;
End;

Procedure TWndInfoSd.AppendAny ( Const AMessage : string );
Begin
 repeat
 if FInfoView=nil then break;
 FInfoView.AppendAny(AMessage);
 FTabSheet.Caption:='Info ('+IntToStr(FInfoView.GetMessageCount)+')';
 until TRUE;
End;

Procedure TWndInfoSd.ErrorClick ( Const AErrorCode : string );
Var
  BComment,
  BReporter,
  BFilename     : string;
  BTextL,
  BTextP        : Integer;
Begin
 repeat
 ParseError(AErrorCode,BComment,BReporter,BFilename,BTextL,BTextP);
 if BFilename='' then break;
 if Assigned(FOnLocalizeError) then FOnLocalizeError(BFilename,BTextL,BTextP,BComment);
 until TRUE;
End;

end.

