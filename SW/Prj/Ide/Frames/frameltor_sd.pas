unit FrameLtoR_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLIntf, LCLType, ExtCtrls, ComCtrls, AsmTypes_sd;

type

  TOnLocalizeError = Procedure ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string ) of Object;

  TLtoData = class(TObject)
  private
    FFilename     : string;
    FTextL,
    FTextP        : Integer;
    FComment      : string;
    FReporter     : string;
    FViewFilename : string;
    FChildCount   : Integer;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    property Filename : string read FFilename write FFilename;
    property TextL : Integer read FTextL write FTextL;
    property TextP : Integer read FTextP write FTextP;
    property Comment : string read FComment write FComment;
    property Reporter : string read FReporter write FReporter;

    property ViewFilename : string read FViewFilename write FViewFilename;
    property ChildCount : Integer read FChildCount write FChildCount;
  end;

  TLtoDataList = array of TLtoData;

  { TWndLtoRSd }

  TWndLtoRSd = class(TFrame)
    ImageList1: TImageList;
    TvReport: TTreeView;
    procedure TvReportChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    FTabSheet   : TTabSheet;
    FErrorList  : TStringList;
    FPrjPath    : string;

    FLocalizeError      : TOnLocalizeError;
    FLtoDataList        : TLtoDataList;

    Function NewLtoData : TLtoData;
  public
    { public declarations }
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet; AOnLocalizeError : TOnLocalizeError );
    Procedure Done;
    Procedure AppendAny ( Const AMessage : string );
    Procedure SetPrjPath ( Const APrjPath : string );

    Procedure Clear;
  end;

implementation

Uses
  ConComS;

{$R *.lfm}

Constructor TLtoData.Create;
Begin
 Inherited;
End;

Destructor TLtoData.Destroy;
Begin
 Inherited;
End;

Constructor TWndLtoRSd.Create ( AOwner : TComponent );
Begin
 Inherited;
 FErrorList:=TStringList.Create;
End;

Destructor TWndLtoRSd.Destroy;
Begin
 FErrorList.Free;
 Inherited;
End;

Procedure TWndLtoRSd.Init ( ASheet : TTabSheet; AOnLocalizeError : TOnLocalizeError );
Begin
 FLocalizeError:=AOnLocalizeError;
 FTabSheet:=ASheet;
 FTabSheet.InsertControl(Self);
 TvReport.Align:=alClient;
 Align:=alClient;
 Clear;
End;

Procedure TWndLtoRSd.Done;
Begin
 Clear;
 FTabSheet.RemoveControl(Self);
End;

Procedure TWndLtoRSd.SetPrjPath ( Const APrjPath : string );
Begin
 FPrjPath:=APrjPath;
End;

Procedure TWndLtoRSd.Clear;
Var
  BIndex        : Integer;
Begin
 FErrorList.Clear;
 TvReport.Items.Clear;
 for BIndex:=0 to Length(FLtoDataList)-1 do FLtoDataList[BIndex].Free;
 FLtoDataList:=nil;
 FTabSheet.Caption:='LTO report (0)';
End;

Function TWndLtoRSd.NewLtoData : TLtoData;
Var
  BIndex        : Integer;
Begin
 Result:=TLtoData.Create;
 BIndex:=Length(FLtoDataList);
 setLength(FLtoDataList,BIndex+1);
 FLtoDataList[BIndex]:=Result;
End;

Procedure TWndLtoRSd.AppendAny ( Const AMessage : string );
Var
  BFilename     : string;
  BLine,
  BPos          : Integer;
  BComment      : string;
  BReporter     : string;
  BNode         : TTreeNode;
  BNodeTop,
  BNodeFile     : TTreeNode;
  BLtoFile,
  BLtoData      : TLtoData;
  BNodeLto      : TTreeNode;
  BImageIdx     : Integer;
  BCommentS     : string;
Begin
 FErrorList.Append(AMessage);
 if TvReport.Items.Count=0 then
  begin
  BNode:=TvReport.Items.Add(nil,'LTO report');
  BNode.Data:=nil;
  BNode.ImageIndex:=0;
  BNode.SelectedIndex:=0;
  end;
 BNodeTop:=TvReport.Items[0];
 ParseError(AMessage,BComment,BReporter,BFilename,BLine,BPos);
 BNodeFile:=BNodeTop.GetFirstChild;
 while BNodeFile<>nil do
  begin
  BLtoData:=TLtoData(BNodeFile.Data);
  if (BLtoData<>nil) and (BLtoData.Filename=BFilename) then break;
  BNodeFile:=BNodeFile.GetNextSibling;
  end;
 if BNodeFile=nil then
  begin
  BLtoData:=NewLtoData;
  BLtoData.Filename:=BFilename;
  if BFilename='' then BLtoData.ViewFilename:='System Const File'
  else BLtoData.ViewFilename:=RelFilename(FPrjPath,BFilename);
  BNodeFile:=TvReport.Items.AddChild(BNodeTop,BLtoData.ViewFilename);
  BNodeFile.Data:=BLtoData;
  BNodeFile.ImageIndex:=1;
  BNodeFile.SelectedIndex:=1;
  end;
 BLtoFile:=TLtoData(BNodeFile.Data);
 BLtoFile.ChildCount:=BLtoFile.ChildCount+1;
 BLtoData:=NewLtoData;
 BLtoData.Filename:=BFilename;
 BLtoData.TextL:=BLine;
 BLtoData.TextP:=BPos;
 BLtoData.Comment:=BComment;
 BLtoData.Reporter:=BReporter;
 BNodeLto:=TvReport.Items.AddChild(BNodeFile,BComment);
 BNodeLto.Data:=BLtoData;
 BCommentS:=LowerCase(BComment);
 BImageIdx:=-1;
 if (Pos('label',BCommentS)<>0) and (Pos('not referenced',BCommentS)<>0) then BImageIdx:=2
 else if Pos('hidden by the previous line',BCommentS)<>0 then BImageIdx:=4;
 BNodeLto.ImageIndex:=BImageIdx;
 BNodeLto.SelectedIndex:=BImageIdx;
 BNodeFile.Text:=BLtoFile.ViewFilename+' ('+IntToStr(BLtoFile.ChildCount)+')';
 if FTabSheet<>nil then FTabSheet.Caption:='LTO report ('+IntToStr(FErrorList.Count)+')';
End;

Procedure TWndLtoRSd.TvReportChange(Sender: TObject; Node: TTreeNode);
Var
  BLtoData      : TLtoData;
Begin
 repeat
 if Node=nil then break;
 BLtoData:=TLtoData(Node.Data);
 if BLtoData=nil then break;
 if BLtoData.Comment='' then break;
 if Assigned(FLocalizeError) then FLocalizeError(BLtoData.Filename,BLtoData.TextL,BLtoData.TextP,BLtoData.Comment);
 until TRUE;
End;



end.

