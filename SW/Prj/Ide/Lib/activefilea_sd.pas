unit ActiveFileA_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls;

Type
  TAfItem = class(TObject)
  private
  protected
    FParent     : TTabSheet;
    FStatBar    : TStatusBar;
    FFullName   : string;
    FShortName,
    FExt        : string;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TTabSheet; AStatBar : TStatusBar; Const AFullName : string ); Virtual;
    Procedure Done; Virtual;

    Function IsDirty : boolean; Virtual; Abstract;
    Procedure SaveDirty; Virtual; Abstract;
    Procedure Show; Virtual; Abstract;
    Procedure Hide; Virtual; Abstract;
    Procedure UpdateLabels; Virtual; Abstract;

    property FullName : string read FFullName;
  end;

implementation

Uses
  ConComL;

// TAfItem

Constructor TAfItem.Create;
Begin
 Inherited;
End;

Destructor TAfItem.Destroy;
Begin
 Inherited;
End;

Procedure TAfItem.Init ( AParent : TTabSheet; AStatBar : TStatusBar; Const AFullName : string );
Var
  BFileName     : string;
Begin
 FParent:=AParent;
 FStatBar:=AStatBar;
 FFullName:=AFullName;
 BFileName:=ExtractFileName(FFullName);
 FShortName:=ReadTillC(BFileName,'.');
 FExt:=LowerCase(BFileName);
 FParent.Caption:=FShortName;
 FStatBar.Panels[3].Text:=FFullName;
End;

Procedure TAfItem.Done;
Begin
End;

end.

