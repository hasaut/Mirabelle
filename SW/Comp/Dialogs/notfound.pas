unit NotFound;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, LCLType;

type
  TNotFoundMsg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    ZSpeedButton1: TSpeedButton;
    procedure ZSpeedButton1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure SetText ( AText : string );
  end;

implementation
                             
{$R *.lfm}

Procedure TNotFoundMsg.SetText ( AText : string );
Begin
 Label2.Caption:=#39+AText+#39;
End;

procedure TNotFoundMsg.ZSpeedButton1Click(Sender: TObject);
begin
 Close;
end;

procedure TNotFoundMsg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Shift=[] then
  begin
  case Key of
    VK_ESCAPE :
     begin
     Close;
     end;

    VK_RETURN :
     begin
     Close;
     end;

  end; (* case *)
  end;
end;

//initialization
 // {$I notfound.lrs}

end.
 
