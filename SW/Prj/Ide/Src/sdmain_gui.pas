unit SdMain_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynHighlighterAny,
  SynEditTypes,
  Forms, Controls, Graphics, Dialogs, LCLType, LCLIntf, LMessages, Menus,
  ExtCtrls, ComCtrls, MgCmdLine, MgEdit, WDebug_sd, MsProcess, FrameTree,
  FrameInfo_sd, FrameTerm_sd, FrameBrkp_sd, FrameLtoR_sd, FrameCDbg_sd, FrameMemV_sd, FrameFpga_sd,
  DbgInfo_sd, AfItem_sd, AboutForm_sd, ChangeLog_sd, DbgViewVars_sd;

Const
  CMgViewAny    = LM_USER + 2000;

Type
  TOnCloseA = Procedure of object;
  TExtAppendCmd = Procedure ( Const ACmdS : string ) of object;

  { TSdMainForm }

  TSdMainForm = class(TForm)
    IlFrames: TImageList;
    IlMenu: TImageList;
    MAbout: TMenuItem;
    MainMenu1: TMainMenu;
    MBuildAll: TMenuItem;
    MDebug: TMenuItem;
    MDebugOptions: TMenuItem;
    MEdit: TMenuItem;
    MGotoLine: TMenuItem;
    MFindNext: TMenuItem;
    MReplace: TMenuItem;
    MFind: TMenuItem;
    MSearch: TMenuItem;
    MViewDbg: TMenuItem;
    MView: TMenuItem;
    MRedo: TMenuItem;
    MUndo: TMenuItem;
    MFile: TMenuItem;
    MFileClose: TMenuItem;
    MFileOpen: TMenuItem;
    MGdbRspOptions: TMenuItem;
    MHelp: TMenuItem;
    MOpenProject: TMenuItem;
    MPause: TMenuItem;
    MPrjNew: TMenuItem;
    MProject: TMenuItem;
    MRecentProjects: TMenuItem;
    MReset: TMenuItem;
    MRun: TMenuItem;
    MRunToCar: TMenuItem;
    MSaveFile: TMenuItem;
    MStart: TMenuItem;
    MStepBack: TMenuItem;
    MStepInto: TMenuItem;
    MStepOver: TMenuItem;
    MStepOverJ: TMenuItem;
    PcAux: TPageControl;
    PnlBot: TPanel;
    PnlTop: TPanel;
    Splitter1: TSplitter;
    SpDebugWnd: TSplitter;
    SpTvProject: TSplitter;
    SynAnySyn1: TSynAnySyn;
    TsVars: TTabSheet;
    TsCDbg: TTabSheet;
    TsFpga: TTabSheet;
    TsMemV: TTabSheet;
    TsLtoR: TTabSheet;
    TsBrkp: TTabSheet;
    TsTerm: TTabSheet;
    TsInfo: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MBuildAllClick(Sender: TObject);
    procedure MDebugOptionsClick(Sender: TObject);
    procedure MFileCloseClick(Sender: TObject);
    procedure MFileOpenClick(Sender: TObject);
    procedure MFindClick(Sender: TObject);
    procedure MFindNextClick(Sender: TObject);
    procedure MGotoLineClick(Sender: TObject);
    procedure MOpenProjectClick(Sender: TObject);
    procedure MPrjNewClick(Sender: TObject);
    procedure MRecentProjectsClick(Sender: TObject);
    procedure MReplaceClick(Sender: TObject);
    procedure MResetClick(Sender: TObject);
    procedure MRunToCarClick(Sender: TObject);
    procedure MSaveFileClick(Sender: TObject);
    procedure MStartClick(Sender: TObject);
    procedure MStepBackClick(Sender: TObject);
    procedure MStepIntoClick(Sender: TObject);
    procedure MStepOverClick(Sender: TObject);
    procedure MViewDbgClick(Sender: TObject);
  private
    FDestroing      : boolean;
    FParams         : TStringList;
    FViewList,
    FViewListM      : TStringList;
    FViewLock       : TRtlCriticalSection;
    FViewActive     : boolean;
    FHandle         : THandle;
    FWndName        : string;
    FPath           : string;
    FParamsFilename : string;
    FCloseAnyway    : boolean;
    FIsClosed       : boolean;
    FRcynDoubleEnter    : Integer;
    FCanCompile     : boolean;

    FSearchParams       : string;
    FSearchWord,
    FSearchRepl         : string;
    FSearchOptions      : TSynSearchOptions;

    // Project vars
    FPrjParams  : TStringList;
    FPrjPath    : string;
    FPrjTree    : TPrjTree;

    // SrcEditor / Debugger
    FMgEdit         : TMgEdit;
    FMgDebug        : TMgDebug;

    // Command line
    FCmdParams      : TCmdParams;

    // Threads
    FMsProcess      : TMsProcess;

    // Params
    FPlayerParams   : string;
    FGdbRspParams   : string;

    // Frames
    FWndInfo        : TWndInfoSd;
    FWndTerm        : TWndTermSd;
    FWndBrkp        : TWndBrkpSd;
    FWndVars        : TWndVarsSd;
    FWndLtoR        : TWndLtoRSd;
    FWndCDbg        : TWndCDbgSd;
    FWndMemV        : TWndMemVSd;
    FWndFpga        : TWndFpgaSd;
    FRecentFolder   : string;

    FStartupPrjName : string; // Used when called from another shell
    FOnCloseA       : TOnCloseA;

    FExtAppendCmd   : TExtAppendCmd;

    Procedure ViewAny ( Const AMessage : string );
    Procedure MViewAny ( Var AMessage : TLMessage ); Message CMgViewAny;
    Procedure UpdateUndoRedo ( ACanUndo, ACanRedo : boolean );
    Procedure RefreshChangedYN;
    Procedure ProcAny ( Const ACmd : string );
    Procedure LocalizeErrorA ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string );
    Procedure ApplicationActivate ( Sender : TObject );
    Procedure ApplicationDeactivate ( Sender : TObject );

    // Common
    Procedure CollectParamsE ( AList : TStringList );

    // Project func
    Procedure RdPrjList ( Const APrjList : string );
    Procedure RdPrjList;
    Procedure WrPrjList;
    Procedure PopProject ( Const APrjName : string );
    Procedure RecentPrjClick ( ASender : TObject );
    Function OpenProject ( Const APrjName : string; AUseCmdLine : boolean ) : boolean;
    Procedure SaveProject;

    // Debugger
    Procedure DebugFrameShow;
    Procedure DebugFrameHide;
    Procedure DbgAsmLineChange ( ADbgInfoLine : TDbgInfoLine );
    Procedure DbgSrcLineChange ( Const AFilename : string; ASrcLineIdx : Integer );
    Procedure DbgBtnClick ( ACmd : Char );

    // Compiler
    Procedure ClearHelpers;
    Function BCompile ( Const ADebugActions : string ) : boolean;

    // Helpers
    Procedure ReqSegData ( Const ASegName : string );
    Function GetFlashLog : TStringList;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure ProcessSignal ( ASignal : Cardinal );
    Procedure ExtInit ( Const AWndName, AParamsFilename, APrjFilename : string; AExtAppendCmd : TExtAppendCmd );
    Procedure OpenProject ( Const APrjName : string );

    Procedure AppendCmdExt ( Const ACmdS : string );

    property OnCloseA : TOnCloseA read FOnCloseA write FOnCloseA;
    property ExtAppendCmd : TExtAppendCmd read FExtAppendCmd write FExtAppendCmd;
  end;

var
  SdMainForm: TSdMainForm;

implementation

{$R *.lfm}

Uses
  ConComF, ConComL, ConComS, PlayerParams_sd, MMesBase_sd,
  MFind_sd, MReplace_sd, MGoLine_sd;

Constructor TSdMainForm.Create ( AOwner : TComponent );
Begin
 Inherited;
 FWndName:='Mirabelle IDE';
 InitCriticalSection(FViewLock);
 FParams:=TStringList.Create;
 FPrjParams:=TStringList.Create;
 FViewList:=TStringList.Create;
 FViewListM:=TStringList.Create;
 FCmdParams:=TCmdParams.Create;
End;

Destructor TSdMainForm.Destroy;
Begin
 FDestroing:=TRUE;
 FCmdParams.Free;
 FViewListM.Free;
 FViewList.Free;
 FPrjParams.Free;
 FParams.Free;
 DoneCriticalSection(FViewLock);
 Sleep(100);
 if Assigned(FOnCloseA) then FOnCloseA;
 Inherited;
End;

Procedure TSdMainForm.ProcessSignal ( ASignal : Cardinal );
Begin
 if ASignal in [2, 15, 21] then Close;
End;

Const
  CParamsFilename   = 'params.cfg';
  CMainWndDim       = 'MainWndDim';
  CCmdLineItems     = 'SrcF IncF OutF LnkF';

Procedure TSdMainForm.FormShow(Sender: TObject);
Begin
 FHandle:=Handle;

 FPath:=IncludeTrailingPathDelimiter(GetCurrentDir);

 FRecentFolder:=FPath;

 if FParamsFilename='' then FParamsFilename:=FPath+CParamsFilename;

 try
   FParams.LoadFromFile(FParamsFilename);
 except
 end;

 RdScrPos(FParams,Self,CMainWndDim);

 // Application
 Application.OnActivate:=@ApplicationActivate;
 Application.OnDeactivate:=@ApplicationDeactivate;

 // TvProject
 SpTvProject.Align:=alNone;
 FPrjTree:=TPrjTree.Create(Self); FPrjTree.Init(Self); FPrjTree.OnViewAny:=@ViewAny;

 // MgEdit / Debug
 FMgEdit:=TMgEdit.Create(Self); FMgEdit.Init(Self,@UpdateUndoRedo,@DbgSrcLineChange); FMgEdit.Align:=alClient;
 FMgDebug:=TMgDebug.Create(Self); FMgDebug.Init(Self,@DbgAsmLineChange,@ViewAny,@ProcAny,@DbgBtnClick); FMgDebug.Align:=alRight; DebugFrameHide;

 // Search params
 FSearchParams:=FParams.Values['SearchParams'];

 // Visual components
 PnlTop.Height:=24; PnlTop.Caption:='';
 PnlBot.Height:=RdScrParam(FParams,'AuxHeight',50,ClientHeight div 2);
 FPrjTree.Width:=RdScrParam(FParams,'TvPrjWidth',50,(ClientWidth*4) div 9); SpTvProject.Align:=alLeft; FPrjTree.Left:=0;
 FMgDebug.Width:=RdScrParam(FParams,'MgDebugWidth',50,ClientWidth-FPrjTree.Width-50);

 // Frames
 PcAux.ActivePageIndex:=0;
 FWndInfo:=TWndInfoSd.Create(Self); FWndInfo.Init(TsInfo,@LocalizeErrorA);
 FWndTerm:=TWndTermSd.Create(Self); FWndTerm.Init(TsTerm);
 FWndBrkp:=TWndBrkpSd.Create(Self); FWndBrkp.Init(TsBrkp); //FWndBrkp.OnBrkpChange:=@BrkpChange; FWndBrkp.OnShowBrkp:=@ShowBrkp;
 FWndVars:=TWndVarsSd.Create(Self); FWndVars.Init(TsVars,FPrjParams); FMgDebug.WndVars:=FWndVars;
 FWndLtoR:=TWndLtoRSd.Create(Self); FWndLtoR.Init(TsLtoR,@LocalizeErrorA);
 FWndCDbg:=TWndCDbgSd.Create(Self); FWndCDbg.Init(TsCDbg,FParams);
 FWndMemV:=TWndMemVSd.Create(Self); FWndMemV.Init(TsMemV,@ReqSegData);
 FWndFpga:=TWndFpgaSd.Create(Self); FWndFpga.Init(TsFpga,@ProcAny,@GetFlashLog);

 // Thread
 FMsProcess:=TMsProcess.Create(TRUE);
 FMsProcess.OnViewAny:=@ViewAny;
 FMsProcess.Start;

 // Project
 if FStartupPrjName<>'' then  // Call from another shell
  begin
  RdPrjList(FStartupPrjName);
  FCanCompile:=TRUE;
  OpenProject(FStartupPrjName);
  end
 else
  begin
  // PrjList
  RdPrjList;
  // Read Params
  if FCmdParams.Parse(CCmdLineItems)=FALSE then ViewAny('eCommand line parameters parse error ('+FCmdParams.LastError+') [R:TMdForm.FormShow]')
  else
   begin
   if FCmdParams.PrjFilename<>'' then PopProject(FCmdParams.PrjFilename);
   end;
  if MRecentProjects.Count<>0 then OpenProject(MRecentProjects.Items[0].Caption,TRUE);
  end;

End;

Procedure TSdMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
Var
  BCanClose : boolean;
Begin
 repeat
 if FIsClosed then break;
 if (FCloseAnyway=FALSE) and (FMgEdit<>nil) and (FMgEdit.SaveDirty=FALSE) then
  begin
  if VpMesYN(Self,'Warning','There was an error saving some files.'+LineEnding+'Would you like to exit anyway?')<>0 then begin CloseAction:=caNone; break; end;
  FCloseAnyway:=TRUE;
  end;

 BCanClose:=TRUE;
 if FMsProcess.IsEnded=FALSE then begin BCanClose:=FALSE; FMsProcess.Terminate; end;
 if BCanClose=FALSE then begin CloseAction:=caNone; break; end;

 FIsClosed:=TRUE;

 FParams.Clear;
 WrScrPos(FParams,Self,CMainWndDim);
 WrScrParam(FParams,'MgDebugWidth',FMgDebug.Width);
 WrScrParam(FParams,'AuxHeight',PnlBot.Height);
 WrScrParam(FParams,'TvPrjWidth',FPrjTree.Width);
 FParams.Values['SearchParams']:=FSearchParams;
 SaveProject;
 if FMgDebug.Active then DebugFrameHide;
 WrPrjList;

 try
   FParams.SaveToFile(FParamsFilename);
 except
 end;

 // Frames
 FWndFpga.Done; FWndFpga.Free;
 FWndMemV.Done; FWndMemV.Free;
 FWndCDbg.Done; FWndCDbg.Free;
 FWndLtoR.Done; FWndLtoR.Free;
 FWndVars.Done; FWndVars.Free;
 FWndBrkp.Done; FWndBrkp.Free;
 FWndTerm.Done; FWndTerm.Free;
 FWndInfo.Done; FWndInfo.Free;

 // MgEdit / MgDebug
 FMgEdit.Done; FMgEdit.Free; FMgEdit:=nil;
 FMgDebug.Done; FMgDebug.Free; FMgDebug:=nil;
 // PrjTree
 FPrjTree.Done; FPrjTree.Free;

 FMsProcess.Free;
 Sleep(50);
 CloseAction:=caFree;
 until TRUE;
End;

Procedure TSdMainForm.ViewAny ( Const AMessage : string );
Var
  BViewActive   : boolean;
  BMessage      : string;
Begin
 repeat
 if (AMessage<>'') and (AMessage[1]='A') then
  begin
  BMessage:=AMessage; Delete(BMessage,1,1); if BMessage='' then break;
  if Assigned(FExtAppendCmd) then FExtAppendCmd(BMessage);
  end
 else
  begin
  EnterCriticalSection(FViewLock);
  FViewList.Append(AMessage);
  BViewActive:=FViewActive; FViewActive:=TRUE;
  LeaveCriticalSection(FViewLock);
  if BViewActive=FALSE then PostMessage(FHandle,CMgViewAny,WParam(Self),0);
  end;
 until TRUE;
End;

Procedure TSdMainForm.MViewAny ( Var AMessage : TLMessage );
Var
  BStrIdx   : Integer;
  BCmdS     : string;
  BParam    : string;
Begin
 EnterCriticalSection(FViewLock);
 FViewListM.Assign(FViewList);
 FViewList.Clear;
 FViewActive:=FALSE;
 LeaveCriticalSection(FViewLock);

 repeat
 if FDestroing then break;

 BStrIdx:=0;
 while BStrIdx<FViewListM.Count do
  begin
  BCmdS:=FViewListM.Strings[BStrIdx];
  repeat
  if BCmdS='' then break;
  case BCmdS[1] of
   'i',
   'w',
   'e',
   '-',
   'j',
   'J',
   'k',
   'K',
   'G': FWndInfo.AppendAny(BCmdS);
   't': FWndTerm.AppendAny(BCmdS);
   'l': FWndLtoR.AppendAny(BCmdS);
   'm': begin
        Delete(BCmdS,1,1);
        BParam:=ReadParamStr(BCmdS);
        if BParam='Close' then Close;
        end;
   's': begin
        //Delete(BCmdS,1,1);
        //PnlTop.Caption:='Communication status: '+BCmdS;
        FMgDebug.ProcessAny(BCmdS);
        end;
   'v': begin // From tree view (to editor)
        Delete(BCmdS,1,1);
        BParam:=ReadParamStr(BCmdS);
        if BParam='a' then
         begin
         BParam:=ReadParamStr(BCmdS);
         if FMgEdit<>nil then FMgEdit.AddOpen(BParam,TRUE,FALSE);
         end;
        end;
   'u': begin // Compilation state
        if BCmdS='u0' then begin RefreshChangedYN; FCanCompile:=TRUE; FMgDebug.IpToLineA:=@FMsProcess.IpToLine; end
        else if BCmdS='u1' then DebugFrameHide
        else if BCmdS='u2' then begin DebugFrameShow; FWndCDbg.SetTextDbg(FMsProcess.TextDbg); FMgDebug.IpToLineA:=@FMsProcess.IpToLine; end;
        end;
   'd',
   'D': begin // PrjFullName LstName LstFile[s] (from ThBuild to MgDebug)
        FMgDebug.ProcessAny(BCmdS);
        end;
{   'c': begin // Runner actions
        Delete(BCmdS,1,1);
        FMsProcess.AppendCmd(BCmdS);
        end;}
   'a': begin // Seg list
        Delete(BCmdS,1,1);
        FWndMemV.SetSegParams(BCmdS);
        end;
   'g': begin
        Delete(BCmdS,1,1);
        FWndMemV.SetSegData(BCmdS);
        end;
   'o': begin // Stack
        FMgDebug.ProcessAny(BCmdS);
        end;
   'b': begin // Break list
        BParam:=BCmdS; Delete(BParam,1,1); DelFirstSpace(BParam);
        FWndBrkp.SetAddrList(BParam);
        FMsProcess.AppendCmd(BCmdS+'.');
        end;
   'h': begin // Active CPU index
        FMgDebug.ProcessAny(BCmdS);
        end;
   'p': begin
        FMgDebug.ProcessAny(BCmdS);
        end;
   'r': begin
        FMgDebug.ProcessAny(BCmdS);
        FWndMemV.IsMemUpdateNeeded;
        end;
   'f': begin
        Delete(BCmdS,1,1);
        FWndFpga.ViewAny(BCmdS);
        end;
   else FWndInfo.AppendAny('#'+BCmdS);
  end;
  until TRUE;
  inc(BStrIdx);
  end;

 until TRUE;
End;

Procedure TSdMainForm.RefreshChangedYN;
Begin
 inc(FRcynDoubleEnter);
 repeat
 if FRcynDoubleEnter>1 then break;
 if FMgEdit<>nil then FMgEdit.RefreshChangedYN;
 until TRUE;
 dec(FRcynDoubleEnter);
End;

Procedure TSdMainForm.ProcAny ( Const ACmd : string );
Begin
 if FMsProcess<>nil then FMsProcess.AppendCmd(ACmd);
End;

Procedure TSdMainForm.LocalizeErrorA ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string );
Begin
 if FMgEdit<>nil then FMgEdit.LocalizeError(AFilename,ATextL,ATextP,AComment);
End;

Procedure TSdMainForm.ApplicationActivate ( Sender : TObject );
Begin
 RefreshChangedYN;
End;

Procedure TSdMainForm.ApplicationDeactivate ( Sender : TObject );
Begin
 if FMgEdit<>nil then FMgEdit.DestroyErrorHint;
End;

Procedure TSdMainForm.UpdateUndoRedo ( ACanUndo, ACanRedo : boolean );
Begin
 if MUndo.Enabled<>ACanUndo then MUndo.Enabled:=ACanUndo;
 if MRedo.Enabled<>ACanRedo then MRedo.Enabled:=ACanRedo;
End;


// Menu section

Procedure TSdMainForm.MDebugOptionsClick(Sender: TObject);
Var
  BForm         : TPlayerParamsForm;
Begin
 BForm:=TPlayerParamsForm.Create(Self);
 BForm.PlayerParams:=FPlayerParams;
 repeat
 BForm.ShowModal;
 if BForm.UpdateFlag=FALSE then break;
 FPlayerParams:=BForm.PlayerParams;
 FMsProcess.AppendCmd('y'+FPlayerParams);
 until TRUE;
 BForm.Free;
End;

Procedure TSdMainForm.MFileCloseClick(Sender: TObject);
Begin
 if FMgEdit<>nil then FMgEdit.CloseActive;
End;

Const
  CAllProjectFiles = 'All project files (*.asm, *.pas, *.c, *.hex, *.hex_rv, *.v, *.h)|*.asm;*.pas;*.c;*.hex;*.hex_rv;*.v;*.h|'+
                     'Assembler files (*.asm)|*.asm|'+
                     'Pascal files (*.pas)|*.pas|'+
                     'C files (*.c)|*.c|'+
                     'Verilog files (*.v)|*.v|'+
                     'Include files (*.h)|*.h|'+
                     'Data files (*.hex, *.hex_rv)|*.hex;*.hex_rv|'+
                     'All files (*.*)|*.*';

Procedure TSdMainForm.MFileOpenClick(Sender: TObject);
Var
  BDialog   : TOpenDialog;
  BFullName : string;
  BAfItem   : TAfItem;
  BDefDir   : string;
Begin
 BDialog:=TOpenDialog.Create(Self);
 repeat
 BDialog.Title:='Open file';
 BDialog.Filter:=CAllProjectFiles;
 BDefDir:='';
 BAfItem:=FMgEdit.ActiveItem;
 if BAfItem<>nil then BDefDir:=ExtractFilePath(BAfItem.FullName);
 BDialog.InitialDir:=BDefDir;
 if BDialog.Execute=FALSE then break;
 if BDialog.FileName='' then break;
 BFullName:=ExpandFilename(BDialog.FileName);
 FMgEdit.AddOpen(BFullName,TRUE,FALSE);
 until TRUE;
 BDialog.Free;
End;

Procedure TSdMainForm.MSaveFileClick(Sender: TObject);
Begin
 FMgEdit.SaveActive;
End;

Procedure TSdMainForm.MPrjNewClick(Sender: TObject);
Var
  BDialog   : TSaveDialog;
  BFilename : string;
  BList     : TStringList;
Begin
 BList:=TStringList.Create;
 BDialog:=TSaveDialog.Create(Self);
 BDialog.Title:='Mirabelle Debug: New project';
 BDialog.Filter:='Project files (*.prj)|*.prj|All files (*.*)|*.*';
 repeat
 if BDialog.Execute=FALSE then break;
 BFilename:=BDialog.FileName;
 if BFileName='' then break;
 if ExtractFileExt(BFilename)='' then BFilename:=BFilename+'.prj';
 SaveProject;
 if FMgDebug.Active then DebugFrameHide;
 FMgDebug.CloseAll; FMgEdit.CloseAll;
 PopProject(BFilename);
 if FileExists(BFilename) then
 else
  begin
  try
   BList.SaveToFile(BFilename);
  except
  end;
  end;
 OpenProject(BFilename,FALSE);
 until TRUE;
 BDialog.Free;
 BList.Free;
End;

procedure TSdMainForm.MRecentProjectsClick(Sender: TObject);
begin

end;

Procedure TSdMainForm.MFindClick(Sender: TObject);
Var
 BDialog        : TFindText;
 BAfItem        : TAfItem;
 BResult        : Integer;

Begin
 BDialog:=TFindText.Create(Self);

 BAfItem:=FMgEdit.ActiveItem;
 repeat
 if BAfItem=nil then break;
 BDialog.SetParams(FSearchParams);
 BDialog.TextToFind:=BAfItem.Edit.GetWordAtRowCol(BAfItem.Edit.CaretXY);
 BDialog.ShowModal;
 if BDialog.Result=FALSE then break;
 FSearchParams:=BDialog.GetParams;
 FSearchWord:=BDialog.TextToFind;
 if FSearchWord='' then break;
 FSearchOptions:=BDialog.Options;
 BResult:=BAfItem.Edit.SearchReplaceEx(FSearchWord,'',FSearchOptions,BAfItem.Edit.CaretXY);
 if BResult=0 then VpMesOk(Self,'Text search','Cannot find '+#39+FSearchWord+#39);
 until TRUE;

 BDialog.Free;
End;

Procedure TSdMainForm.MReplaceClick(Sender: TObject);
Var
 BDialog        : TReplText;
 BAfItem        : TAfItem;
 BResult        : Integer;

Begin
 BDialog:=TReplText.Create(Self);

 BAfItem:=FMgEdit.ActiveItem;
 repeat
 if BAfItem=nil then break;
 BDialog.SetParams(FSearchParams);
 BDialog.TextToFind:=BAfItem.Edit.GetWordAtRowCol(BAfItem.Edit.CaretXY);
 BDialog.ShowModal;
 if BDialog.Result=FALSE then break;
 FSearchWord:=BDialog.TextToFind;
 if FSearchWord='' then break;
 FSearchRepl:=BDialog.TextToRepl;
 FSearchParams:=BDialog.GetParams;
 FSearchOptions:=BDialog.Options;
 BResult:=BAfItem.Edit.SearchReplaceEx(FSearchWord,FSearchRepl,FSearchOptions,BAfItem.Edit.CaretXY);
 if BResult=0 then VpMesOk(Self,'Text replace','Cannot find '+#39+FSearchWord+#39);
 until TRUE;

 BDialog.Free;
End;

Procedure TSdMainForm.MFindNextClick(Sender: TObject);
Var
 BAfItem        : TAfItem;
 BResult        : Integer;
 BSearchOptions : TSynSearchOptions;
 BCaret         : TPoint;

Begin
 BAfItem:=FMgEdit.ActiveItem;
 repeat
 if BAfItem=nil then break;
 if FSearchWord='' then break;
 BSearchOptions:=AddSearchAgain(FSearchOptions);
 //BSearchOptions:=FSearchOptions;
 BCaret:=BAfItem.Edit.CaretXY;
 BResult:=BAfItem.Edit.SearchReplaceEx(FSearchWord,FSearchRepl,BSearchOptions,BCaret);
 if BResult=0 then VpMesOk(Self,'Find again','Cannot find '+#39+FSearchWord+#39);
 until TRUE;
End;

Procedure TSdMainForm.MGotoLineClick(Sender: TObject);
Var
  BAfItem       : TAfItem;
  BGotoLine     : TGotoLine;
  BResult       : boolean;
  BLineNumberS  : string;
  BLineNumber   : Integer;
  BDummyF       : Extended;

Begin
 repeat
 BAfItem:=FMgEdit.ActiveItem;
 if BAfItem=nil then break;
 BGotoLine:=TGotoLine.Create(Self);
 BGotoLine.SetParams(FParams);
 BGotoLine.LinesCount:=BAfItem.Edit.Lines.Count;
 BGotoLine.LineNumber:='';
 BGotoLine.ShowModal;
 BResult:=BGotoLine.Result;
 BLineNumberS:=BGotoLine.LineNumber;
 BGotoLine.Free;
 if BResult=FALSE then break;
 if BAfItem.Edit.Focused=FALSE then BAfItem.Edit.SetFocus;

 if BLineNumberS='' then break;
 if TextToFloat(PChar(BLineNumberS),BDummyF,fvExtended)=FALSE then
  begin
  VpMesOk(Self,'Goto line number','Invalid line number');
  break;
  end;

 BLineNumber:=Round(BDummyF);
 //if BLineNumber<>0 then dec(BLineNumber);

 BAfItem.GotoLinePos(BLineNumber,0);
 until TRUE;
End;

Procedure TSdMainForm.DbgBtnClick ( ACmd : Char );
Var
  BCore         : char;
  BCoreIdx      : Integer;
  BEip          : Cardinal;
  BBreakList    : string;
Begin
 case ACmd of
  'S': begin
       FMgEdit.SaveDirty;
       if FMgEdit.IsRunDirty or FMgDebug.IsEmpty then
        begin
        PcAux.ActivePageIndex:=0;
        BBreakList:=FWndBrkp.CollectAddrList;
        FMgDebug.PresetBreakList(BBreakList);
        BCompile('RIMRnib'+BBreakList+'.');
        end
       else
        begin
        DebugFrameShow;
        ProcAny('S');
        end;
       end;
  'P': ProcAny('7.');
  'R': begin
       BBreakList:=FWndBrkp.CollectAddrList;
       FMgDebug.PresetBreakList(BBreakList);
       ProcAny('RIMRnib'+BBreakList+'.');
       end;
  'Q': ProcAny('Q');
  '1': begin
       ProcAny('1');
       end;
  '7': begin
       if FMgDebug.GetEip(BEip,BCore,BCoreIdx)=FALSE then ProcAny('7.')
       else ProcAny('7'+BCore+IntToHex(BCoreIdx,2)+IntToHex(BEip,8)+'.');
       end;
  '8': begin
       if FMgDebug.GetEip(BEip,BCore,BCoreIdx)=FALSE then ProcAny('7.')
       else ProcAny('8'+BCore+IntToHex(BCoreIdx,2)+IntToHex(BEip,8)+'.');
       end;
  '4': begin
       ProcAny('4'+IntToHex(FMgDebug.IpAtLine,8)+'.');
       end;
  '6': begin
       PcAux.ActivePageIndex:=0;
       BBreakList:=FWndBrkp.CollectAddrList;
       FMgDebug.PresetBreakList(BBreakList);
       BCompile('RIMRnib'+BBreakList+'.J');
       end;
 end;
End;

procedure TSdMainForm.MStartClick(Sender: TObject);
begin
 DbgBtnClick('S');
end;

procedure TSdMainForm.MStepBackClick(Sender: TObject);
begin
 DbgBtnClick('1');
end;

procedure TSdMainForm.MResetClick(Sender: TObject);
begin
 DbgBtnClick('R');
end;

procedure TSdMainForm.MRunToCarClick(Sender: TObject);
begin
 DbgBtnClick('4');
end;

procedure TSdMainForm.MBuildAllClick(Sender: TObject);
begin
 DbgBtnClick('6');
end;

procedure TSdMainForm.MStepIntoClick(Sender: TObject);
begin
 DbgBtnClick('7');
end;

Procedure TSdMainForm.MStepOverClick(Sender: TObject);
Begin
 DbgBtnClick('8');
End;

Procedure TSdMainForm.MOpenProjectClick(Sender: TObject);
Var
  BDialog   : TOpenDialog;
  BFilename : string;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Open project';
 BDialog.Filter:='Project files (*.prj)|*.prj|All files (*.*)|*.*';
 repeat
 if BDialog.Execute=FALSE then break;
 BFilename:=BDialog.FileName;
 if BFileName='' then break;
 SaveProject;
 if FMgDebug.Active then DebugFrameHide;
 FMgDebug.CloseAll; FMgEdit.CloseAll;
 PopProject(BFilename);
 OpenProject(BFilename,FALSE);
 until TRUE;
 BDialog.Free;
End;

procedure TSdMainForm.MViewDbgClick(Sender: TObject);
begin
 if FMgDebug.Active then DebugFrameHide
 else DebugFrameShow;
end;

Procedure TSdMainForm.MAboutClick(Sender: TObject);
Var
  BAbout    : TAboutFormSd;
Begin
 BAbout:=TAboutFormSd.Create(Self);
 BAbout.SetChangeLog(CVersion,CChangeLog);
 BAbout.ShowModal;
 BAbout.Free;
End;

// Project section

Procedure TSdMainForm.RdPrjList ( Const APrjList : string );
Var
  BReadS    : string;
  BPrjName  : string;
  BItem     : TMenuItem;
Begin
 MRecentProjects.Clear;
 BReadS:=APrjList;

 repeat
 BPrjName:=ReadParamStr(BReadS);
 if BPrjName='' then break;
 BItem:=TMenuItem.Create(MRecentProjects); BItem.Name:='';
 BItem.Caption:=BPrjName; BItem.OnClick:=@RecentPrjClick;
 MRecentProjects.Add(BItem);
 until FALSE;
End;

Procedure TSdMainForm.RdPrjList;
Begin
 RdPrjList(FParams.Values['RecentProjects']);
End;

Procedure TSdMainForm.WrPrjList;
Var
  BListS    : string;
  BIndex    : Integer;
Begin
 BListS:='';

 BIndex:=0;
 while BIndex<MRecentProjects.Count do
  begin
  if BListS<>'' then BListS:=BListS+#32;
  BListS:=BListS+MRecentProjects.Items[BIndex].Caption;
  inc(BIndex);
  end;

 FParams.Values['RecentProjects']:=BListS;
End;

Procedure TSdMainForm.PopProject ( Const APrjName : string );
Var
  BIndex    : Integer;
  BItem     : TMenuItem;
Begin
 BIndex:=0;
 while BIndex<MRecentProjects.Count do
  begin
  if MRecentProjects.Items[BIndex].Caption=APrjName then MRecentProjects.Delete(BIndex)
  else inc(BIndex);
  end;
 while MRecentProjects.Count>12 do MRecentProjects.Delete(12);
 BItem:=TMenuItem.Create(MRecentProjects);
 BItem.Name:='';
 BItem.Caption:=APrjName;
 BItem.OnClick:=@RecentPrjClick;
 MRecentProjects.Insert(0,BItem);
End;

Procedure TSdMainForm.RecentPrjClick ( ASender : TObject );
Var
  BIndex        : Integer;
  BNewPrj       : string;
Begin
 repeat
 SaveProject;
 if FMgDebug.Active then DebugFrameHide;
 FMgDebug.CloseAll; FMgEdit.CloseAll;

 BIndex:=0;
 while BIndex<MRecentProjects.Count do
  begin
  if MRecentProjects.Items[BIndex]=ASender then break;
  inc(BIndex);
  end;
 if BIndex>=MRecentProjects.Count then break;
 BNewPrj:=MRecentProjects.Items[BIndex].Caption;

 PopProject(BNewPrj);
 OpenProject(BNewPrj,FALSE);
 until TRUE;
End;

{
FNodeTop,
 FNodeCfg,
  FNodeSeg,
  FNodeHex,
  FNodeGdb,
  FNodeDir,
  FNodeCov,
FNodeSrc,
FNodeAsm        : TTreeNode;
}

Function TSdMainForm.OpenProject ( Const APrjName : string; AUseCmdLine : boolean ) : boolean;
Var
  BReadS    : string;
  BParamS   : string;
  BLineIdx  : Integer;
  BIndex    : Integer;
  BParamsE  : TStringList;
  BPlayerSet: boolean;
  BPath     : string;
Begin
 Result:=FALSE;
 FPrjTree.Clear;
 FPrjParams.Clear;
 FPlayerParams:=''; FGdbRspParams:='';
 BParamsE:=TStringList.Create;
 BPath:=ExtractFilePath(APrjName);
 if BPath='' then BPath:=FPath;
 FPrjPath:=BPath;

 DebugFrameHide;
 FMgDebug.CloseAll;

 repeat
 try
  FPrjParams.LoadFromFile(APrjName);
 except
  ViewAny('eCannot open project file '+APrjName+' [R:TMgMainForm.OpenProject]');
  break;
 end;

 Caption:=FWndName+': '+ExtractFilename(APrjName);
 Application.Title:=Caption;

 FPrjTree.SetPrjParams(APrjName,FPrjParams);
 FWndInfo.SetPrjPath(FPrjPath);

 BPlayerSet:=FALSE;
 BLineIdx:=0;
 while BLineIdx<FPrjParams.Count do
  begin
  BReadS:=FPrjParams.Strings[BLineIdx];
  BParamS:=LowerCase(ReadParamStr(BReadS)); DelFirstSpace(BReadS);
  if BParamS='player' then begin BPlayerSet:=TRUE; FPlayerParams:=BReadS; end
  else if BParamS='gdbrsp' then begin FGdbRspParams:=BReadS; end
  else if BParamS='brkp' then FWndBrkp.SetAddrList(BReadS)
  else if BParamS='fpga' then FWndFpga.SetSectList(BReadS);
  inc(BLineIdx);
  end;

 if BPlayerSet=FALSE then
  begin
  ViewAny('eThere are no Player params in the project file (Mode Iss/Fpga, com name and baud) [R:TMgMainForm.OpenProject]');
  ViewAny('AJwThere are no Player params in the project file (Mode Iss/Fpga, com name and baud) [R:TMgMainForm.OpenProject]');
  end;

 BIndex:=0;
 while BIndex<FPrjParams.Count do
  begin
  BReadS:=FPrjParams.Strings[BIndex];
  BParamS:=ReadParamStr(BReadS);
  if BParamS='OpenFiles' then
   begin
   repeat
   BParamS:=ReadParamStr(BReadS);
   if BParamS='' then break;
   FMgEdit.AddOpen(AbsFilename(FPrjPath,ReplacePathSlash(BParamS)),FALSE,TRUE);
   until FALSE;
   FPrjParams.Delete(BIndex);
   break;
   end;
  inc(BIndex);
  end;

 BIndex:=0;
 while BIndex<FPrjParams.Count do
  begin
  BReadS:=FPrjParams.Strings[BIndex];
  BParamS:=ReadParamStr(BReadS);
  if BParamS='ActiveFile' then
   begin
   repeat
   BParamS:=ReadParamStr(BReadS);
   if BParamS='' then break;
   FMgEdit.SetActiveFile(FPrjPath,ReplacePathSlash(BParamS));
   until FALSE;
   FPrjParams.Delete(BIndex);
   break;
   end;
  inc(BIndex);
  end;

 FRecentFolder:=ExtractFilePath(APrjName);
 FMsProcess.AppendCmd('y'+FPlayerParams);
 CollectParamsE(BParamsE);
 if AUseCmdLine then FCmdParams.ExpandPrjParams(BParamsE);

 Result:=TRUE;
 until TRUE;

 BParamsE.Free;
End;

Procedure TSdMainForm.SaveProject;
Var
  BPrjName      : string;
Begin
 CollectParamsE(FPrjParams);

 FPrjTree.Clear;
 repeat
 if MRecentProjects.Count=0 then break;
 BPrjName:=MRecentProjects.Items[0].Caption;

 if FMgEdit.SaveDirty=FALSE then
  begin
  VpMesOk(Self,'Error','There was an error saving some of project files');
  end;

 FPrjParams.Append('OpenFiles '+FMgEdit.GetOpenList(FPrjPath));
 FPrjParams.Append('ActiveFile '+FMgEdit.GetActiveFile(FPrjPath));

 try
  FPrjParams.SaveToFile(BPrjName);
 except
  ViewAny('eCannot save file '+BPrjName);
  break;
 end;

 ClearHelpers;
 until TRUE;
End;

// Common

Procedure TSdMainForm.CollectParamsE ( AList : TStringList );
Begin
 AList.Clear;

 AList.Append('Player '+FPlayerParams);
 AList.Append('GdbRsp '+FGdbRspParams);
 FPrjTree.CollectParamsE(AList);
 AList.Append('Brkp '+FWndBrkp.CollectAddrList);
 AList.Append('Fpga '+FWndFpga.CollectSectList(suAll));
End;

// Debugger

Procedure TSdMainForm.DebugFrameShow;
Begin
 FMgDebug.Show; FMgDebug.Active:=TRUE;
 SpDebugWnd.Show;
 SpDebugWnd.Left:=FMgDebug.Left-SpDebugWnd.Width;
 if FMgEdit<>nil then FMgEdit.DebugModeSet;
End;

Procedure TSdMainForm.DebugFrameHide;
Begin
 SpDebugWnd.Hide; FMgDebug.Active:=FALSE;
 FMgDebug.Hide;
 if FMgEdit<>nil then FMgEdit.DebugModeClr;
End;

Procedure TSdMainForm.DbgAsmLineChange ( ADbgInfoLine : TDbgInfoLine );
Var
  BAfItem       : TAfItem;
  BTextLine,
  BTextPos      : Integer;
  BSrcName      : string;
Begin
 repeat
 if FMgEdit=nil then break;

 if ADbgInfoLine=nil then
  begin
  FMgEdit.DebugModeClr;
  break;
  end;

 BTextLine:=-1; BTextPos:=-1; BSrcName:='';
 repeat
 if ADbgInfoLine=nil then break;
 BSrcName:=ADbgInfoLine.SrcFile; if BSrcName<>'' then begin BTextLine:=ADbgInfoLine.SrcIdxL; BTextPos:=ADbgInfoLine.SrcIdxP; break; end;
 BSrcName:=ADbgInfoLine.AsmFile; if BSrcName<>'' then begin BTextLine:=ADbgInfoLine.AsmIdxL; BTextPos:=0; break; end;
 until TRUE;

 if BSrcName='' then BAfItem:=nil
 else BAfItem:=FMgEdit.AddOpen(BSrcName,FALSE,TRUE);
 FMgEdit.SetDbgLine(BAfItem,ADbgInfoLine,BTextLine,BTextPos);

 FWndCDbg.SetDbgAsmLine(ADbgInfoLine);
 until TRUE;
End;

Procedure TSdMainForm.DbgSrcLineChange ( Const AFilename : string; ASrcLineIdx : Integer );
Begin
 FMgDebug.DbgSrcLineChange(AFilename,ASrcLineIdx);
 FWndCDbg.SetDbgSrcLine(AFilename,ASrcLineIdx);
End;

// Compiler

Procedure TSdMainForm.ClearHelpers;
Begin
 if FWndInfo<>nil then FWndInfo.Clear;
 if FWndTerm<>nil then FWndTerm.Clear;
 if FWndBrkp<>nil then FWndBrkp.Clear;
 if FWndLtoR<>nil then FWndLtoR.Clear;
 if FWndCDbg<>nil then FWndCDbg.Clear;
 if FWndMemV<>nil then FWndMemV.Clear;
 if FWndFpga<>nil then FWndFpga.Clear;
End;

Function TSdMainForm.BCompile ( Const ADebugActions : string ) : boolean;
Var
  BSrcList      : TStringList;
  BPlayerS      : string;
Begin
 Result:=FALSE;
 BSrcList:=TStringList.Create;

 repeat
 if FCanCompile=FALSE then break;
 //if FDebugWnd<>nil then FDebugWnd.Close;

 FMgDebug.IpToLineA:=nil;
 ClearHelpers;

 FMgEdit.DestroyErrorHint;
 if FMgEdit.SaveDirty=FALSE then begin ViewAny('eCould not save all open files, compilation stopped'); break; end;
 FMgEdit.ResetRunDirty;

 if FPrjParams.Count=0 then begin ViewAny('eProject is not created or is not opened'); break; end;

 FPrjTree.CollectParamsE(BSrcList);
 FCmdParams.ExpandPrjParams(BSrcList);

 FCanCompile:=FALSE;
 FMsProcess.AppendCmd('B'+ADebugActions+#13+BuildParamsToStr(FPrjTree.PrjName,BSrcList,BPlayerS));

 Result:=TRUE;
 until TRUE;

 BSrcList.Free;
End;

// Helpers
Procedure TSdMainForm.ReqSegData ( Const ASegName : string );
Begin
 FMsProcess.AppendCmd('g '+ASegName);
End;

Function TSdMainForm.GetFlashLog : TStringList;
Begin
 Result:=FMsProcess.FlashLog;
End;

// Ext interface

Procedure TSdMainForm.ExtInit ( Const AWndName, AParamsFilename, APrjFilename : string; AExtAppendCmd : TExtAppendCmd  );
Begin
 FWndName:=AWndName; Caption:=FWndName;
 FParamsFilename:=AParamsFilename;
 FStartupPrjName:=APrjFilename;
 FExtAppendCmd:=AExtAppendCmd;
End;

Procedure TSdMainForm.OpenProject ( Const APrjName : string );
Var
  BBreakList    : string;
Begin
 repeat
 if FPrjParams.Count<>0 then SaveProject;
 if OpenProject(APrjName,FALSE)=FALSE then begin ViewAny('AJeCannot open project '+APrjName+' [R:TSdMainForm.OpenProject]'); break; end;
 if FMsProcess=nil then begin ViewAny('AJeSD internal error (FMsProcess is not defined) [R:TSdMainForm.OpenProject]'); break; end;
 PcAux.ActivePageIndex:=0;
 BBreakList:=FWndBrkp.CollectAddrList;
 FMgDebug.PresetBreakList(BBreakList);
 if BCompile('RIMRnib'+BBreakList+'.SJ')=FALSE then begin ViewAny('AJeProject build error [R:TSdMainForm.OpenProject]'); break; end;
 until TRUE;
End;

Procedure TSdMainForm.AppendCmdExt ( Const ACmdS : string );
Begin
 if FMsProcess<>nil then FMsProcess.AppendCmd('A'+ACmdS);
End;

end.

