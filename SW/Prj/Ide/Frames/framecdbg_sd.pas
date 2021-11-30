unit FrameCDbg_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLIntf, LCLType, ExtCtrls, ComCtrls, Buttons, StdCtrls, Dialogs, AsmTypes_sd,
  DbgInfo_sd, FrameCdModule_sd;

type

  { TWndCDbgSd }

  TWndCDbgSd = class(TFrame)
    BtDirName: TSpeedButton;
    BtSave: TSpeedButton;
    EdDirName: TEdit;
    LbDirName: TLabel;
    Panel2: TPanel;
    PcModules: TPageControl;
    Panel1: TPanel;
    PnlSave: TPanel;
    PnlSel: TPanel;
    SbPars: TSpeedButton;
    SbAtom: TSpeedButton;
    SbJmps: TSpeedButton;
    SbVars: TSpeedButton;
    SbSRec: TSpeedButton;
    SbOrdr: TSpeedButton;
    SbMatr: TSpeedButton;
    procedure BtDirNameClick(Sender: TObject);
    procedure BtSaveClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SbParsClick(Sender: TObject);
    procedure SbAtomClick(Sender: TObject);
    procedure SbJmpsClick(Sender: TObject);
    procedure SbMatrClick(Sender: TObject);
    procedure SbOrdrClick(Sender: TObject);
    procedure SbSRecClick(Sender: TObject);
    procedure SbVarsClick(Sender: TObject);
  private
    { private declarations }
    FExeParams  : TStringList;
    FTabSheet   : TTabSheet;
    FTextDbg    : TStringList;

    FTextCommon : TStringList;
    FTextLayer  : TCompLayerTextList;

    FViewIdx    : TCompLayer;
    FModuleList : array of TCdModuleFrame;

    Procedure ParseText ( Var ALineIdx : Integer );
    Procedure SetViewIdx ( AViewIdx : TCompLayer );
    Procedure SetSizeA;
    Procedure SetParams;
    Procedure SaveParams;
  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet; AExeParams : TStringList );
    Procedure Done;

    Procedure Clear;
    Procedure SetTextDbg ( ATextDbg : TStringList );
    Procedure SetDbgAsmLine ( ADbgLine : TDbgInfoLine );
    Procedure SetDbgSrcLine ( Const AFilename : string; ASrcLineIdx : Integer );

    property TabSheet : TTabSheet read FTabSheet write FTabSheet;
  end;

implementation

Uses
  ConComL, ConComI, ConComS, MMesBase_sd;

{$R *.lfm}

Const
  CCompDbgSaveDir = 'CompDbgSaveDir';

Constructor TWndCDbgSd.Create ( AOwner : TComponent );
Var
  BLayerIdx     : TCompLayer;
Begin
 Inherited;
 FTextDbg:=TStringList.Create;
 FTextCommon:=TStringList.Create;
 for BLayerIdx in TCompLayer do FTextLayer[BLayerIdx]:=TStringList.Create;
End;

Destructor TWndCDbgSd.Destroy;
Var
  BLayerIdx     : TCompLayer;
Begin
 Clear;
 for BLayerIdx in TCompLayer do FTextLayer[BLayerIdx].Free;
 FTextCommon.Free;
 FTextDbg.Free;
 Inherited;
End;

Procedure TWndCDbgSd.Init ( ASheet : TTabSheet; AExeParams : TStringList );
Begin
 FTabSheet:=ASheet;
 FTabSheet.InsertControl(Self);
 Align:=alClient;
 Panel1.Align:=alClient;
 FExeParams:=AExeParams;
 PnlSave.Color:=CColorBG;
 PnlSave.Height:=EdDirName.Height+4;
 EdDirName.Top:=2;
 EdDirName.Left:=LbDirName.Left+LbDirName.Width+4; EdDirName.Text:='';
 BtDirName.Top:=2; BtDirName.Height:=EdDirName.Height; BtDirName.Width:=EdDirName.Height;
 BtSave.Top:=2; BtSave.Height:=EdDirName.Height;
 LbDirName.Top:=(PnlSave.Height-LbDirName.Height+1) div 2;
 Clear;
 SetSizeA;
 SetParams;
End;

Procedure TWndCDbgSd.Done;
Begin
 SaveParams;
 Clear;
 FTabSheet.RemoveControl(Self);
End;

Procedure TWndCDbgSd.SetParams;
Begin
 EdDirName.Text:=FExeParams.Values[CCompDbgSaveDir];
End;

Procedure TWndCDbgSd.SaveParams;
Begin
 FExeParams.Values[CCompDbgSaveDir]:=EdDirName.Text;
End;

Procedure TWndCDbgSd.Clear;
Var
  BModuleIdx    : Integer;
  BModule       : TCdModuleFrame;
Begin
 for BModuleIdx:=0 to Length(FModuleList)-1 do
  begin
  BModule:=FModuleList[BModuleIdx];
  BModule.Deinit;
  BModule.TabSheet.Free;
  BModule.Free;
  end;
 FModuleList:=nil;

 FTabSheet.Caption:='Compiler Debug ('+IntToStr(Length(FModuleList))+')';
End;

Procedure TWndCDbgSd.SetSizeA;
Begin
 BtSave.Left:=PnlSave.ClientWidth-BtSave.Width-2;
 BtDirName.Left:=BtSave.Left-BtDirName.Width-2;
 EdDirName.Width:=BtDirName.Left-EdDirName.Left-2;
End;

procedure TWndCDbgSd.FrameResize(Sender: TObject);
begin
 SetSizeA;
end;

Procedure TWndCDbgSd.BtDirNameClick(Sender: TObject);
Var
  BDialog       : TSelectDirectoryDialog;
  BFilename     : string;
Begin
 BDialog:=TSelectDirectoryDialog.Create(Self);
 repeat
 BDialog.Title:='Select saving directory';
 BDialog.InitialDir:=EdDirName.Text;
 if BDialog.Execute=FALSE then break;
 BFilename:=BDialog.FileName;
 if BFilename='' then break;
 EdDirName.Text:=BFilename;
 until TRUE;
 BDialog.Free;
End;


Procedure TWndCDbgSd.SetTextDbg ( ATextDbg : TStringList );
Var
  BLineIdx      : Integer;
Begin
 Clear;
 FTextDbg.Assign(ATextDbg);
 BLineIdx:=0; while BLineIdx<FTextDbg.Count do ParseText(BLineIdx);
 FTabSheet.Caption:='Compiler Debug ('+IntToStr(Length(FModuleList))+')';
End;

procedure TWndCDbgSd.SbParsClick(Sender: TObject);
begin
 SetViewIdx(clPars);
end;

procedure TWndCDbgSd.SbAtomClick(Sender: TObject);
begin
 SetViewIdx(clAtom);
end;

procedure TWndCDbgSd.SbJmpsClick(Sender: TObject);
begin
 SetViewIdx(clJmps);
end;

procedure TWndCDbgSd.SbVarsClick(Sender: TObject);
begin
 SetViewIdx(clVars);
end;

procedure TWndCDbgSd.SbSRecClick(Sender: TObject);
begin
 SetViewIdx(clSRec);
end;

procedure TWndCDbgSd.SbOrdrClick(Sender: TObject);
begin
 SetViewIdx(clOrdr);
end;

procedure TWndCDbgSd.SbMatrClick(Sender: TObject);
begin
 SetViewIdx(clMatr);
end;

Procedure TWndCDbgSd.ParseText ( Var ALineIdx : Integer );
Var
  BReadS        : string;
  BParamA       : string;
  BTextThis     : TStringList;
  BTag          : string;
  BLineIdx      : Integer;
  BModuleIdx    : Integer;
  BModule       : TCdModuleFrame;
  BLayerIdx     : TCompLayer;
Begin
 FTextCommon.Clear;
 for BLayerIdx in TCompLayer do FTextLayer[BLayerIdx].Clear;

 BReadS:='';
 repeat
 while ALineIdx<FTextDbg.Count do
  begin
  BReadS:=FTextDbg.Strings[ALineIdx];
  if Copy(BReadS,1,3)=';@M' then break;
  inc(ALineIdx);
  end;
 if ALineIdx>=FTextDbg.Count then break;

 FTextCommon.Append(BReadS); inc(ALineIdx);
 BTextThis:=FTextCommon;
 while ALineIdx<FTextDbg.Count do
  begin
  BReadS:=FTextDbg.Strings[ALineIdx];
  BTag:=Copy(BReadS,1,3);
  if BTag=';@M' then break;
  if BTag=';@T' then
   begin
   Delete(BReadS,1,3); BParamA:=ReadParamStr(BReadS);
   if BParamA='Pars' then BTextThis:=FTextLayer[clPars]
   else if BParamA='Atom' then BTextThis:=FTextLayer[clAtom]
   else if BParamA='Jmps' then BTextThis:=FTextLayer[clJmps]
   else if BParamA='Vars' then BTextThis:=FTextLayer[clVars]
   else if BParamA='SRec' then BTextThis:=FTextLayer[clSRec]
   else if BParamA='Ordr' then BTextThis:=FTextLayer[clOrdr]
   else if BParamA='Matr' then BTextThis:=FTextLayer[clMatr]
   else BTextThis:=nil;
   end
  else if BTextThis<>nil then BTextThis.Append(BReadS);
  inc(ALineIdx);
  end;
 for BLineIdx:=0 to FTextCommon.Count-1 do
  begin
  BReadS:=FTextCommon.Strings[BLineIdx];
  for BLayerIdx in TCompLayer do FTextLayer[BLayerIdx].Insert(BLineIdx,BReadS);
  end;

 BModule:=TCdModuleFrame.Create(Self); BModule.Name:='';
 BModuleIdx:=Length(FModuleList); SetLength(FModuleList,BModuleIdx+1); FModuleList[BModuleIdx]:=BModule;
 BModule.Init(PcModules.AddTabSheet);
 BModule.SetTextDbg(FTextLayer);
 BModule.SetViewIdx(FViewIdx);
 until TRUE;
End;

Procedure TWndCDbgSd.SetViewIdx ( AViewIdx : TCompLayer );
Var
  BModuleIdx    : Integer;
Begin
 FViewIdx:=AViewIdx;
 for BModuleIdx:=0 to Length(FModuleList)-1 do FModuleList[BModuleIdx].SetViewIdx(AViewIdx);
End;

Procedure TWndCDbgSd.SetDbgAsmLine ( ADbgLine : TDbgInfoLine );
Var
  BModuleIdx    : Integer;
Begin
 for BModuleIdx:=0 to Length(FModuleList)-1 do FModuleList[BModuleIdx].SetDbgAsmLine(ADbgLine);
End;

Procedure TWndCDbgSd.SetDbgSrcLine ( Const AFilename : string; ASrcLineIdx : Integer );
Var
  BModuleIdx    : Integer;
Begin
 for BModuleIdx:=0 to Length(FModuleList)-1 do FModuleList[BModuleIdx].SetDbgSrcLine(AFilename,ASrcLineIdx);
End;

Procedure TWndCDbgSd.BtSaveClick(Sender: TObject);
Var
  BDateTime     : TDateTime;
  BDateTimeS    : string;
  BSaveDirName  : string;
  BFullDirName  : string;
  BFilename     : string;
  BPath,
  BName,
  BExt          : string;
  BModule       : TCdModuleFrame;
  BList         : TStringList;
  BError        : boolean;
  BModuleIdx    : Integer;
  BViewIdx      : TCompLayer;
Begin
 BList:=TStringList.Create;

 repeat
 BSaveDirName:=EdDirName.Text;
 if BSaveDirName='' then begin VpMesOk(Self,'Error','Saving directory is not specified'); break; end;
 if DirectoryExists(BSaveDirName)=FALSE then begin VpMesOk(Self,'Error','Saving directory does not exist'); break; end;
 BDateTime:=Now;
 BDateTimeS:=FormatDateTime('YYYYMMDD_HHNNSS',BDateTime);
 BFullDirName:=BSaveDirName; AppendLastSlash(BFullDirName);
 BFullDirName:=BFullDirName+BDateTimeS;
 if DirectoryExists(BFullDirName) then begin VpMesOk(Self,'Error','Directory '+BFullDirName+' already exists'); break; end;
 if FileExists(BFullDirName) then begin VpMesOk(Self,'Error','Cannot create a directory '+BFullDirName+' because there exists a file with the same name'); break; end;
 if ForceDirectories(BFullDirName)=FALSE then begin VpMesOk(Self,'Error','Cannot create directory '+BFullDirName); break; end;

 BError:=FALSE;
 BModuleIdx:=0;
 while BModuleIdx<Length(FModuleList) do
  begin
  BModule:=FModuleList[BModuleIdx];
  SplitFileName(BModule.SrcName,BPath,BName,BExt);
  for BViewIdx in TCompLayer do
   begin
   BModule.ExportSrc(BViewIdx,BList);
   BFilename:=AssembleFullName(BFullDirName,BName+'_'+CCompLayerNames[BViewIdx],'txt');
   try
     BList.SaveToFile(BFilename);
   except
     BError:=TRUE;
     break;
   end;
   end;
  if BError then break;

  inc(BModuleIdx);
  end;


 until TRUE;

 BList.Free;
End;

end.

