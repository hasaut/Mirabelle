unit FrameCdModule_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLIntf, LCLType, ExtCtrls, ComCtrls, Buttons, AsmTypes_sd,
  DbgInfo_sd, DbgViewComp_sd;

type
  { TCdModuleFrame }

  TCdModuleFrame = class(TFrame)
  private
    { private declarations }
    FTabSheet   : TTabSheet;
    FViewList   : array [TCompLayer] of TDbgViewComp;
    FSrcName    : string;

  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ATabSheet : TTabSheet );
    Procedure Deinit;

    Procedure Clear;
    Procedure SetTextDbg ( Const ATextList : TCompLayerTextList );
    Procedure SetViewIdx ( AViewIdx : TCompLayer );
    Procedure SetDbgAsmLine ( ADbgLine : TDbgInfoLine );
    Procedure SetDbgSrcLine ( Const AFilename : string; ASrcLineIdx : Integer );
    Procedure ExportSrc ( ALayer : TCompLayer; AList : TStringList );

    property TabSheet : TTabSheet read FTabSheet write FTabSheet;
    property SrcName : string read FSrcName;
  end;

implementation

{$R *.lfm}

Constructor TCdModuleFrame.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TCdModuleFrame.Destroy;
Begin
 Inherited;
End;

Procedure TCdModuleFrame.Init ( ATabSheet : TTabSheet );
Var
  BLayerIdx     : TCompLayer;
  BView         : TDbgViewComp;
Begin
 FTabSheet:=ATabSheet;
 FTabSheet.InsertControl(Self);
 Align:=alClient;
 Clear;
 for BLayerIdx in TCompLayer do
  begin
  BView:=TDbgViewComp.Create(Self);
  InsertControl(BView);
  BView.Visible:=FALSE;
  BView.Align:=alClient;
  BView.CompLayer:=BLayerIdx;
  FViewList[BLayerIdx]:=BView;
  end;
End;

Procedure TCdModuleFrame.Deinit;
Var
  BViewIdx      : TCompLayer;
  BView         : TDbgViewComp;
Begin
 for BViewIdx in TCompLayer do
  begin
  BView:=FViewList[BViewIdx];
  RemoveControl(BView);
  BView.Free;
  end;
 FTabSheet.RemoveControl(Self);
End;

Procedure TCdModuleFrame.Clear;
Begin
 FTabSheet.Caption:='';
End;

Procedure TCdModuleFrame.SetTextDbg ( Const ATextList : TCompLayerTextList );
Var
  BViewIdx      : TCompLayer;
Begin
 for BViewIdx in TCompLayer do FViewList[BViewIdx].SetList(ATextList[BViewIdx]);
 if FViewList[clPars].DbgFile.ModuleName.Count>0 then  FTabSheet.Caption:=FViewList[clPars].DbgFile.ModuleName.Strings[0];
 if FViewList[clPars].DbgFile.SrcList.Count>0 then FSrcName:=FViewList[clPars].DbgFile.SrcList.Strings[0]
 else FSrcName:='';
End;

Procedure TCdModuleFrame.SetViewIdx ( AViewIdx : TCompLayer );
Var
  BViewIdx      : TCompLayer;
Begin
 for BViewIdx in TCompLayer do FViewList[BViewIdx].Visible:=BViewIdx=AViewIdx;
End;

Procedure TCdModuleFrame.SetDbgAsmLine ( ADbgLine : TDbgInfoLine );
Var
  BViewIdx      : TCompLayer;
Begin
 if ADbgLine.SrcFile=FSrcName then
  begin
  FTabSheet.PageControl.ActivePage:=FTabSheet;
  for BViewIdx in TCompLayer do FViewList[BViewIdx].SetHlCust(ADbgLine.ProcNameL,ADbgLine.LayerIdxL[BViewIdx]);
  end
 else
  begin
  for BViewIdx in TCompLayer do FViewList[BViewIdx].SetHlCust('',-1);
  end;
End;

Procedure TCdModuleFrame.SetDbgSrcLine ( Const AFilename : string; ASrcLineIdx : Integer );
Var
  BViewIdx      : TCompLayer;
Begin
 if AFilename=FSrcName then
  begin
  FTabSheet.PageControl.ActivePage:=FTabSheet;
  for BViewIdx in TCompLayer do FViewList[BViewIdx].SetHlSrc(ASrcLineIdx);
  end
 else
  begin
  for BViewIdx in TCompLayer do FViewList[BViewIdx].SetHlSrc(-1);
  end;
End;

Procedure TCdModuleFrame.ExportSrc ( ALayer : TCompLayer; AList : TStringList );
Begin
 AList.Assign(FViewList[ALayer].DbgFile.SrcText);
End;

end.

