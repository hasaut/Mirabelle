unit TabWarn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TTabWarning }
   TTabWarning = class(TForm)
    ZLabel1: TLabel;
    Panel1: TPanel;
    ZLabel2: TLabel;
    ZLabel3: TLabel;
    ZLabel4: TLabel;
    ZSpeedButton2: TSpeedButton;
    ZSpeedButton3: TSpeedButton;
    procedure ZSpeedButton3Click(Sender: TObject);
    procedure ZSpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    FResult     : boolean;

  public
    { Public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SetFilename ( AName : string );
    property OpenAnyway : boolean read FResult;
  end;

implementation

{$R *.lfm}

{ TTabWarning }
Constructor TTabWarning.Create ( AOwner : TComponent );
Begin
 Inherited Create(AOwner);
 FResult:=FALSE;
End;

Destructor TTabWarning.Destroy;
Begin
 Inherited Destroy;
End;

Procedure TTabWarning.SetFilename ( AName : string );
Begin
 Panel1.Caption:=AName;
End;

procedure TTabWarning.ZSpeedButton3Click(Sender: TObject);
begin
 Close;
end;

procedure TTabWarning.ZSpeedButton2Click(Sender: TObject);
begin
 FResult:=TRUE;
 Close;
end;

//initialization
//  {$I tabwarn.lrs}

end.
