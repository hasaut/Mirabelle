unit DEnvOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ComCtrls;

type

  { TEnvOptForm }

  TEnvOptForm = class(TForm)
    BtnCancel: TSpeedButton;
    BtnYes: TSpeedButton;
    PnlFont: TPanel;
    Panel2: TPanel;
    PnlBottom: TPanel;
    TreeView1: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  EnvOptForm: TEnvOptForm;

implementation

{$R *.lfm}

//initialization
//  {$I denvoptions.lrs}

end.

