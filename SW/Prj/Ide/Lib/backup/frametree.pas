unit FrameTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus, Dialogs, ExtCtrls,
  Buttons, StdCtrls, MgNameHolder, AsmTypes_sd, ExtCompDialog_sd, LCLType, Graphics;

type

  { TPrjTree }

  TPrjTree = class(TFrame)
    BtCompile: TSpeedButton;
    BtDebugWnd: TSpeedButton;
    CbGroupTree: TCheckBox;
    IlPopupMenu: TImageList;
    IlTree: TImageList;
    MLocG_Append: TMenuItem;
    MLnkF_Remove: TMenuItem;
    MModF_Remove: TMenuItem;
    MLnkG_Append: TMenuItem;
    MModG_Append: TMenuItem;
    MLocF_Remove: TMenuItem;
    MTstF_MoveDn: TMenuItem;
    MTstF_MoveUp: TMenuItem;
    MTstF_Remove: TMenuItem;
    MTstG_Append: TMenuItem;
    MRvcF_Remove: TMenuItem;
    MRvcG_Append: TMenuItem;
    MPerF_MoveDn: TMenuItem;
    MMemF_MoveUp: TMenuItem;
    MMemF_MoveDn: TMenuItem;
    MIncF_MoveDn: TMenuItem;
    MIncF_MoveUp: TMenuItem;
    MIncF_Remove: TMenuItem;
    MThrF_MoveDn: TMenuItem;
    MPerF_MoveUp: TMenuItem;
    MThrF_MoveUp: TMenuItem;
    MPerF_Remove: TMenuItem;
    MThrF_Remove: TMenuItem;
    MPerG_Append: TMenuItem;
    MOutF_Remove: TMenuItem;
    MIncG_Append: TMenuItem;
    MMemF_Remove: TMenuItem;
    MMemG_Append: TMenuItem;
    MGrpG_AddOld: TMenuItem;
    MGrpG_AddNew: TMenuItem;
    MCpuF_MoveDn: TMenuItem;
    MCpuF_MoveUp: TMenuItem;
    MCpuF_Remove: TMenuItem;
    MOutG_Append: TMenuItem;
    MThrG_Append: TMenuItem;
    MSrcG_AddOld: TMenuItem;
    MSrcF_MoveDn: TMenuItem;
    MSrcF_MoveUp: TMenuItem;
    MSrcG_AddNew: TMenuItem;
    MSrcF_Open: TMenuItem;
    MSrcF_Remove: TMenuItem;
    MHrwF_Remove: TMenuItem;
    MCpuG_Append: TMenuItem;
    MHrwG_Append: TMenuItem;
    PmIncF: TPopupMenu;
    PmLnkF: TPopupMenu;
    PmModF: TPopupMenu;
    PmLnkG: TPopupMenu;
    PmModG: TPopupMenu;
    PmLocF: TPopupMenu;
    PmLocG: TPopupMenu;
    PmTstF: TPopupMenu;
    PmTstG: TPopupMenu;
    PmRvcF: TPopupMenu;
    PmRvcG: TPopupMenu;
    PmPerF: TPopupMenu;
    PmThrF: TPopupMenu;
    PmPerG: TPopupMenu;
    PmOutF: TPopupMenu;
    PmIncG: TPopupMenu;
    PmMemF: TPopupMenu;
    PmMemG: TPopupMenu;
    PmGrpG: TPopupMenu;
    PmCpuF: TPopupMenu;
    PmOutG: TPopupMenu;
    PmThrG: TPopupMenu;
    PmSrcG: TPopupMenu;
    PmSrcF: TPopupMenu;
    PmHrwF: TPopupMenu;
    PmCpuG: TPopupMenu;
    PmHrwG: TPopupMenu;
    PnlTreeBot: TPanel;
    TvProject: TTreeView;
    procedure CbGroupTreeClick(Sender: TObject);
    procedure MLnkF_RemoveClick(Sender: TObject);
    procedure MLnkG_AppendClick(Sender: TObject);
    procedure MLocF_RemoveClick(Sender: TObject);
    procedure MLocG_AppendClick(Sender: TObject);
    procedure MModF_RemoveClick(Sender: TObject);
    procedure MModG_AppendClick(Sender: TObject);
    procedure MOutG_Append1Click(Sender: TObject);
    procedure MTstF_RemoveClick(Sender: TObject);
    procedure MIncF_RemoveClick(Sender: TObject);
    procedure MIncG_AppendClick(Sender: TObject);
    procedure MMemF_RemoveClick(Sender: TObject);
    procedure MMemG_AppendClick(Sender: TObject);
    procedure MAnyF_MoveDnClick(Sender: TObject);
    procedure MAnyF_MoveUpClick(Sender: TObject);
    procedure MGrpG_AddOldClick(Sender: TObject);
    procedure MGrpG_AddNewClick(Sender: TObject);
    procedure MOutF_RemoveClick(Sender: TObject);
    procedure MOutG_AppendClick(Sender: TObject);
    procedure MPerF_RemoveClick(Sender: TObject);
    procedure MPerG_AppendClick(Sender: TObject);
    procedure MRvcF_RemoveClick(Sender: TObject);
    procedure MRvcG_AppendClick(Sender: TObject);
    procedure MSrcF_OpenClick(Sender: TObject);
    procedure MSrcF_RemoveClick(Sender: TObject);
    procedure MSrcG_AddNewClick(Sender: TObject);
    procedure MSrcG_AddOldClick(Sender: TObject);
    procedure MHrwF_RemoveClick(Sender: TObject);
    procedure MCpuF_RemoveClick(Sender: TObject);
    procedure MHrwG_AppendClick(Sender: TObject);
    procedure MCpuG_AppendClick(Sender: TObject);
    procedure MThrF_RemoveClick(Sender: TObject);
    procedure MThrG_AppendClick(Sender: TObject);
    procedure MTstG_AppendClick(Sender: TObject);
    procedure TvProjectDblClick(Sender: TObject);
    procedure TvProjectDeletion(Sender: TObject; Node: TTreeNode);
    procedure TvProjectEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TvProjectMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FParent         : TWinControl;
    FPrjName,
    FPrjPath        : string;
    FRecentFolder   : string;
    FPlayerParams,
    FGdbRspParams   : string;
    FSrcFiles       : string;
    FInConstruction : boolean;

    FNodeTop,
      FNodeHrw,
      FNodeDig,
        FNodeCpu,
        FNodeMem,
        FNodePer,
      FNodeFrw,
        FNodeThr,
        FNodeInc,
        FNodeOut,
        FNodeSrc,
        FNodeLoc,
        FNodeTst,
      FNodeTle,
        FNodeRvc,
        FNodeLnk,
      FNodeDbg,
      FNodeMod      : TTreeNode;
     FActionNode    : TTreeNode;

    FViewAny        : TOnViewAny;

    Procedure ViewAny ( Const AMessage : string );

    Function GetNodePosOpti ( ANode : TTreeNode ) : TPoint;
    Function AddTreeItem ( AParent : TTreeNode; AClass : TNhObjectClass; Const ACaption : string; AImageIdx : Integer ) : TTreeNode;
    Function AddTreeItem ( AParent : TTreeNode; AClass : TNhObjectClass; Const AFullName, ACaption : string; AImageIdx : Integer ) : TTreeNode;
    Procedure TreeXchgNode ( ANodeA, ANodeB : TTreeNode );

    Procedure PrjAddFiles ( AList : TStrings );
    Procedure ModAddFiles ( AList : TStrings );
    Procedure AddSrcNew ( Const APath : string );
    Procedure AddSrcOld ( Const APath : string );
    Procedure AddDllOld ( Const APath : string );

    Procedure FillSrcTree;
    Procedure FillSrcTree_Flat;
    Procedure FillSrcTree_Tree;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TWinControl );
    Procedure Done;

    Procedure Clear;
    Procedure SetPrjParams ( Const APrjName : string; APrjParams : TStringList );
    Function CollectItemsFNoSp ( AClass : TNhObjectClass ) : string;
    Function CollectItems ( AClass : TNhObjectClass ) : string;
    Function CollectItemsF ( AClass : TNhObjectClass ) : string;
    Function CollectItemsF ( AClass : TNhObjectClass; Var ANodeIdx : Integer ) : string;
    Procedure CollectParamsE ( AList : TStringList );
    //Function CollectParamsF : string;

    property OnViewAny : TOnViewAny read FViewAny write FViewAny;
    property PrjName : string read FPrjName;
  end;

implementation

Uses
  ConComL, ConComS, MMesBase_sd;

{$R *.lfm}

Const
  CAllSourceFiles =
    'All source files (*.asm; *.srv; *.hex; *.pas; *.c; *.ci; *.crv; *.wasm; *.py; *.rs; *.i; *.v)|*.asm;*.srv;*.hex;*.pas;*.c;*.ci;*.crv;*.wasm;*.py;*.rs;*.i;*.v'+
    '|Assembler files (*.asm)|*.asm'+
    '|Risc-V asm files (*.srv)|*.srv'+
    '|HEX files (*.hex)|*.hex'+
    '|Pascal files (*.pas)|*.pas'+
    '|C files (*.c)|*.c'+
    '|Risc-V C files (*.crv)|*.crv'+
    '|WebAssembly files (*.wasm)|*.wasm'+
    '|Python files (*.py)|*.py'+
    '|Rust files (*.rs)|*.rs'+
    '|Postprocessor files (*.i)|*.i'+
    '|Verilog files (*.v)|*.v'+
    '|All files (*.*)|*.*';

Function SrcIdxByFileType ( Const AFullName : string ) : Integer;
Var
  BExtS     : string;
Begin
 Result:=19;
 BExtS:=LowerCase(ExtractFileExt(AFullName));
 if BExtS='.asm' then Result:=6
 else if BExtS='.s' then Result:=30
 else if BExtS='.srv' then Result:=30
 else if BExtS='.i' then Result:=30
 else if BExtS='.pas' then Result:=7
 else if BExtS='.c' then Result:=8
 else if BExtS='.ci' then Result:=8
 else if BExtS='.py' then Result:=9
 else if BExtS='.rs' then Result:=10
 else if BExtS='.wasm' then Result:=36
 else Result:=5;
End;

Function HexIdxByFileType ( Const AFullName : string ) : Integer;
Var
  BExtS     : string;
Begin
 Result:=19;
 BExtS:=LowerCase(ExtractFileExt(AFullName));
 if BExtS='.hex' then Result:=11
 else if BExtS='.bin' then Result:=12
 else Result:=10;
End;

{ TPrjTree }

Constructor TPrjTree.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TPrjTree.Destroy;
Begin
 Inherited;
End;

Procedure TPrjTree.Init ( AParent : TWinControl );
Begin
 FParent:=AParent;
 FParent.InsertControl(Self);
 Align:=alLeft; Left:=0;
 TvProject.Align:=alClient;
 PnlTreeBot.ClientHeight:=CbGroupTree.Height+4;
 BtCompile.Top:=(PnlTreeBot.ClientHeight-BtCompile.Height) div 2; BtCompile.Left:=4;
 BtDebugWnd.Top:=(PnlTreeBot.ClientHeight-BtDebugWnd.Height) div 2; BtDebugWnd.Left:=BtCompile.Left+BtCompile.Width+2;
End;

Procedure TPrjTree.Done;
Begin
 Clear;
 FParent.RemoveControl(Self);
End;

Procedure TPrjTree.Clear;
Begin
 TvProject.Items.Clear;
 FNodeSrc:=nil;
 FNodeLoc:=nil;
End;

Procedure TPrjTree.SetPrjParams ( Const APrjName : string; APrjParams : TStringList );
Var
  BLineIdx  : Integer;
  BReadS,
  BParamS   : string;
Begin
 FInConstruction:=TRUE;
 Clear;
 FPrjName:=APrjName;
 FPrjPath:=ExtractFilePath(FPrjName);
 FSrcFiles:='';

 FNodeTop:=AddTreeItem(nil,ocTop,FPrjName,0);
   FNodeHrw:=AddTreeItem(FNodeTop,ocHrwG,'Hardware',5);
   FNodeDig:=AddTreeItem(FNodeTop,ocDigG,'Digital',12);
     FNodeCpu:=AddTreeItem(FNodeDig,ocCpuG,'CPU',3);
     FNodeMem:=AddTreeItem(FNodeDig,ocMemG,'Memory',5);
     FNodePer:=AddTreeItem(FNodeDig,ocPerG,'Periphery',5);
   FNodeFrw:=AddTreeItem(FNodeTop,ocFrwG,'Firmware',27);
     FNodeThr:=AddTreeItem(FNodeFrw,ocThrG,'Threads',22);
     FNodeInc:=AddTreeItem(FNodeFrw,ocIncG,'Include path',24);
     FNodeOut:=AddTreeItem(FNodeFrw,ocOutG,'Output path',26);
     FNodeSrc:=AddTreeItem(FNodeFrw,ocSrcG,'Source files',1);
     FNodeLoc:=AddTreeItem(FNodeFrw,ocLocG,'Source search path',39);
     FNodeTst:=AddTreeItem(FNodeFrw,ocTstG,'Unit tests',34);
   FNodeTle:=AddTreeItem(FNodeTop,ocTleG,'Tools',31);
     FNodeRvc:=AddTreeItem(FNodeTle,ocRvcG,'Risc-V external compiler',32);
     FNodeLnk:=AddTreeItem(FNodeTle,ocLnkG,'Linker options',35);
   FNodeDbg:=AddTreeItem(FNodeTop,ocDbgG,'Debugger',29);
   FNodeMod:=AddTreeItem(FNodeTop,ocModG,'Math model',37);

 BLineIdx:=0;
 while BLineIdx<APrjParams.Count do
  begin
  BReadS:=APrjParams.Strings[BLineIdx];
  BParamS:=ReadParamStr(BReadS); DelFirstSpace(BReadS);
  if BParamS='Player' then begin FPlayerParams:=BReadS; end
  else if BParamS='GdbRsp' then begin FGdbRspParams:=BReadS; end
  else if BParamS='HrwF' then begin AddTreeItem(FNodeHrw,ocHrwF,BReadS,BReadS,13); end
  else if BParamS='CpuF' then begin AddTreeItem(FNodeCpu,ocCpuF,BReadS,BReadS,13); end
  else if BParamS='MemF' then begin AddTreeItem(FNodeMem,ocMemF,BReadS,BReadS,13); end
  else if BParamS='PerF' then begin AddTreeItem(FNodePer,ocPerF,BReadS,BReadS,13); end
  else if BParamS='ThrF' then begin AddTreeItem(FNodeThr,ocThrF,BReadS,BReadS,22); end
  else if BParamS='IncF' then begin DoDirSeparators(BReadS); AddTreeItem(FNodeInc,ocIncF,AbsFilename(FPrjPath,BReadS),BReadS,23); end
  else if BParamS='OutF' then begin DoDirSeparators(BReadS); AddTreeItem(FNodeOut,ocOutF,AbsFilename(FPrjPath,BReadS),BReadS,25); end
  else if BParamS='SrcF' then begin DoDirSeparators(BReadS); FSrcFiles:=FSrcFiles+BReadS+#32; end
  else if BParamS='LocF' then begin DoDirSeparators(BReadS); AddTreeItem(FNodeLoc,ocLocF,AbsFilename(FPrjPath,BReadS),BReadS,17); end
  else if BParamS='TstF' then begin DoDirSeparators(BReadS); AddTreeItem(FNodeTst,ocTstF,AbsFilename(FPrjPath,BReadS),BReadS,34); end
  else if BParamS='RvcF' then begin AddTreeItem(FNodeRvc,ocRvcF,BReadS,BReadS,33); end
  else if BParamS='LnkF' then begin AddTreeItem(FNodeLnk,ocLnkF,BReadS,BReadS,35); end
  else if BParamS='ModF' then begin DoDirSeparators(BReadS); AddTreeItem(FNodeMod,ocModF,AbsFilename(FPrjPath,BReadS),BReadS,38); end
  else if BParamS='GroupTree' then CbGroupTree.Checked:=LowerCase(BReadS)<>'false';
  inc(BLineIdx);
  end;

 FInConstruction:=FALSE;
 FillSrcTree;
 FNodeTop.Expand(FALSE);
 FNodeFrw.Expand(FALSE);
 FNodeSrc.Expand(FALSE);
End;

Function TPrjTree.CollectItemsFNoSp ( AClass : TNhObjectClass ) : string;
Var
  BNodeIdx      : Integer;
  BNode         : TTreeNode;
  BNameHolder   : TNameHolder;
Begin
 Result:='';

 BNodeIdx:=0;
 while BNodeIdx<TvProject.Items.Count do
  begin
  BNode:=TvProject.Items[BNodeIdx];
  repeat
  BNameHolder:=TNameHolder(BNode.Data);
  if BNameHolder=nil then break;
  if BNameHolder.ObjectClass=AClass then Result:=Result+BNameHolder.FullName;
  until TRUE;
  inc(BNodeIdx);
  end;
End;

Function TPrjTree.CollectItems ( AClass : TNhObjectClass ) : string;
Var
  BNodeIdx      : Integer;
  BNode         : TTreeNode;
  BNameHolder   : TNameHolder;
Begin
 Result:='';

 BNodeIdx:=0;
 while BNodeIdx<TvProject.Items.Count do
  begin
  BNode:=TvProject.Items[BNodeIdx];
  repeat
  BNameHolder:=TNameHolder(BNode.Data);
  if BNameHolder=nil then break;
  if BNameHolder.ObjectClass=AClass then Result:=Result+RelFilename(FPrjPath,BNameHolder.FullName)+#32;
  until TRUE;
  inc(BNodeIdx);
  end;
End;

// With full name
Function TPrjTree.CollectItemsF ( AClass : TNhObjectClass ) : string;
Var
  BNodeIdx      : Integer;
  BNode         : TTreeNode;
  BNameHolder   : TNameHolder;
Begin
 Result:='';

 BNodeIdx:=0;
 while BNodeIdx<TvProject.Items.Count do
  begin
  BNode:=TvProject.Items[BNodeIdx];
  repeat
  BNameHolder:=TNameHolder(BNode.Data);
  if BNameHolder=nil then break;
  if BNameHolder.ObjectClass=AClass then Result:=Result+BNameHolder.FullName+#32;
  until TRUE;
  inc(BNodeIdx);
  end;
End;

Function TPrjTree.CollectItemsF ( AClass : TNhObjectClass; Var ANodeIdx : Integer ) : string;
Var
  BNode         : TTreeNode;
  BNameHolder   : TNameHolder;
Begin
 Result:='';

 while ANodeIdx<TvProject.Items.Count do
  begin
  BNode:=TvProject.Items[ANodeIdx]; inc(ANodeIdx);
  BNameHolder:=TNameHolder(BNode.Data);
  if (BNameHolder<>nil) and (BNameHolder.ObjectClass=AClass) then begin Result:=BNameHolder.FullName; break; end;
  end;
End;

Procedure TPrjTree.CollectParamsE ( AList : TStringList );
Var
  BNodeIdx      : Integer;
  BNode         : TTreeNode;
  BNameHolder   : TNameHolder;
Begin
 BNodeIdx:=0;
 while BNodeIdx<TvProject.Items.Count do
  begin
  BNode:=TvProject.Items[BNodeIdx];
  repeat
  BNameHolder:=TNameHolder(BNode.Data);
  if BNameHolder=nil then break;
  case BNameHolder.ObjectClass of
    ocHrwF: AList.Append('HrwF '+BNameHolder.FullName);
    ocCpuF: AList.Append('CpuF '+BNameHolder.FullName);
    ocMemF: AList.Append('MemF '+BNameHolder.FullName);
    ocPerF: AList.Append('PerF '+BNameHolder.FullName);
    ocThrF: AList.Append('ThrF '+BNameHolder.FullName);
    ocIncF: AList.Append('IncF '+RelFilename(FPrjPath,BNameHolder.FullName));
    ocOutF: AList.Append('OutF '+RelFilename(FPrjPath,BNameHolder.FullName));
    ocSrcF: AList.Append('SrcF '+RelFilename(FPrjPath,BNameHolder.FullName));
    ocLocF: AList.Append('LocF '+RelFilename(FPrjPath,BNameHolder.FullName));
    ocTstF: AList.Append('TstF '+RelFilename(FPrjPath,BNameHolder.FullName));
    ocRvcF: AList.Append('RvcF '+BNameHolder.FullName);
    ocLnkF: AList.Append('LnkF '+BNameHolder.FullName);
    ocModF: AList.Append('ModF '+RelFilename(FPrjPath,BNameHolder.FullName));
  end; // case
  until TRUE;
  inc(BNodeIdx);
  end;
 if CbGroupTree.Checked then AList.Append('GroupTree True') else AList.Append('GroupTree False');
End;

{Function TPrjTree.CollectParamsF : string;
Var
  BNodeIdx      : Integer;
  BFullName     : string;
  BMemSegCfg    : string;
  BSegName      : string;
  BSegFlags     : byte;
  BHwBase,
  BHwSize       : Cardinal;
  BHwWidth      : Byte;
Begin
 BMemSegCfg:='';
 BNodeIdx:=0;
 repeat
 BFullName:=CollectItemsF(ocMemF,BNodeIdx);
 if BFullName='' then break;
 BSegName:=ReadParamStr(BFullName);
 BSegFlags:=Hex32ToInt(ReadParamStr(BFullName));
 BHwBase:=Hex32ToInt(ReadParamStr(BFullName));
 BHwSize:=Hex32ToInt(ReadParamStr(BFullName));
 BHwWidth:=Hex32ToInt(ReadParamStr(BFullName));
 BMemSegCfg:=BMemSegCfg+MemSegParamsToStr(BSegName,BSegFlags,BHwBase,BHwSize,BHwWidth);
 until FALSE;

 // These parameters are read in TMsProcess.ProcessBuild
 Result:=FPrjName+#13+
         CollectItemsFNoSp(ocCpuF)+#13+
         BMemSegCfg+#13+
         'code data'+#13+
         '1000 FE'+#13+
         CollectItemsF(ocRvcF)+#13+
         CollectItems(ocSrcF)+#13+
         CollectItems(ocIncF)+#13+
         CollectItems(ocTstF)+#13+
         CollectItems(ocOutF);
End;}

Procedure TPrjTree.ViewAny ( Const AMessage : string );
Begin
 if Assigned(FViewAny) then FViewAny(AMessage);
End;

Function TPrjTree.AddTreeItem ( AParent : TTreeNode; AClass : TNhObjectClass; Const ACaption : string; AImageIdx : Integer ) : TTreeNode;
Var
  BNameHolder   : TNameHolder;
Begin
 Result:=TvProject.Items.AddChild(AParent,ACaption);
 BNameHolder:=TNameHolder.Create;
 BNameHolder.Init(AClass,ACaption);
 Result.Data:=BNameHolder;
 Result.ImageIndex:=AImageIdx; Result.SelectedIndex:=AImageIdx;
End;

Function TPrjTree.AddTreeItem ( AParent : TTreeNode; AClass : TNhObjectClass; Const AFullName, ACaption : string; AImageIdx : Integer ) : TTreeNode;
Var
  BNameHolder   : TNameHolder;
Begin
 Result:=TvProject.Items.AddChild(AParent,ACaption);
 BNameHolder:=TNameHolder.Create;
 BNameHolder.Init(AClass,AFullName);
 Result.Data:=BNameHolder;
 Result.ImageIndex:=AImageIdx; Result.SelectedIndex:=AImageIdx;
End;

Procedure TPrjTree.TreeXchgNode ( ANodeA, ANodeB : TTreeNode );
Var
  BNameHolderA,
  BNameHolderB  : TNameHolder;
  BDummyTextA,
  BDummyTextB   : string;
Begin
 repeat
 if ANodeA=nil then break;
 if ANodeB=nil then break;
 BNameHolderA:=TNameHolder(ANodeA.Data); if BNameHolderA=nil then break;
 BNameHolderB:=TNameHolder(ANodeB.Data); if BNameHolderB=nil then break;
 BDummyTextA:=ANodeA.Text; BDummyTextB:=ANodeB.Text;
 ANodeA.Data:=BNameHolderB; ANodeA.Text:=BDummyTextB;
 ANodeB.Data:=BNameHolderA; ANodeB.Text:=BDummyTextA;
 until TRUE;
End;

// Tree section

Procedure TPrjTree.PrjAddFiles ( AList : TStrings );
Var
  BFileIdx      : Integer;
  BFullName     : string;
  BNodeIdx      : Integer;
  BNode         : TTreeNode;
  BHolder       : TNameHolder;
  BPath         : string;
Begin
 BFileIdx:=0;
 while BFileIdx<AList.Count do
  begin
  BFullName:=AList.Strings[BFileIdx];
  if BFullName='' then break;
  BNodeIdx:=0;
  while BNodeIdx<TvProject.Items.Count do
   begin
   BHolder:=TNameHolder(TvProject.Items[BNodeIdx].Data);
   if BHolder<>nil then
    begin
    if (BHolder.ObjectClass=ocSrcF) and (LowerCase(BHolder.FullName)=LowerCase(BFullName)) then begin VpMesOk(Self,'Warning','File '+BFullName+' is already in the list'); break; end;
    end;
   inc(BNodeIdx);
   end;
  if BNodeIdx=TvProject.Items.Count then // i.e. file is not yet in the list
   begin
   BPath:=ExtractFilePath(BFullName);
   BNodeIdx:=0;
   while BNodeIdx<TvProject.Items.Count do
    begin
    BHolder:=TNameHolder(TvProject.Items[BNodeIdx].Data);
    if BHolder<>nil then
     begin
     if (BHolder.ObjectClass=ocGrpG) and (AbsFilename(FPrjPath,BHolder.FullName)=BPath) then break;
     end;
    inc(BNodeIdx);
    end;
   if BNodeIdx<TvProject.Items.Count then
    begin
    BNode:=TvProject.Items[BNodeIdx];
    BHolder:=TNameHolder(BNode.Data);
    if BHolder=nil then break;
    AddTreeItem(BNode,ocSrcF,BFullName,ExtractFilename(BFullName),SrcIdxByFileType(BFullName));
    end
   else
    begin
    if FNodeSrc=nil then break;
    AddTreeItem(FNodeSrc,ocSrcF,BFullName,RelFilename(FPrjPath,BFullName),SrcIdxByFileType(BFullName));
    end;
   end;
  inc(BFileIdx);
  end;
End;

Procedure TPrjTree.ModAddFiles ( AList : TStrings );
Var
  BFileIdx      : Integer;
  BFullName     : string;
  BNodeIdx      : Integer;
  BHolder       : TNameHolder;
Begin
 BFileIdx:=0;
 while BFileIdx<AList.Count do
  begin
  BFullName:=AList.Strings[BFileIdx];
  if BFullName='' then break;
  BNodeIdx:=0;
  while BNodeIdx<TvProject.Items.Count do
   begin
   BHolder:=TNameHolder(TvProject.Items[BNodeIdx].Data);
   if BHolder<>nil then
    begin
    if (BHolder.ObjectClass=ocSrcF) and (LowerCase(BHolder.FullName)=LowerCase(BFullName)) then begin VpMesOk(Self,'Warning','File '+BFullName+' is already in the list'); break; end;
    end;
   inc(BNodeIdx);
   end;
  if BNodeIdx=TvProject.Items.Count then // i.e. file is not yet in the list
   begin
   if FNodeMod=nil then break;
   AddTreeItem(FNodeMod,ocModF,BFullName,RelFilename(FPrjPath,BFullName),38);
   end;
  inc(BFileIdx);
  end;
End;

Procedure TPrjTree.FillSrcTree;
Var
  BExpanded : boolean;
Begin
 repeat
 if FNodeSrc=nil then break;
 BExpanded:=FNodeSrc.Expanded;
 FNodeSrc.DeleteChildren;
 if CbGroupTree.Checked then FillSrcTree_Tree
 else FillSrcTree_Flat;
 if BExpanded then FNodeSrc.Expand(TRUE);
 until TRUE;
End;

Procedure TPrjTree.FillSrcTree_Flat;
Var
  BSrcFiles,
  BRelName      : string;
Begin
 BSrcFiles:=FSrcFiles;
 repeat
 BRelName:=ReadParamStr(BSrcFiles);
 if BRelName='' then break;
 AddTreeItem(FNodeSrc,ocSrcF,AbsFilename(FPrjPath,BRelName),BRelName,SrcIdxByFileType(BRelName));
 until FALSE;
End;

Procedure TPrjTree.FillSrcTree_Tree;
Var
  BList         : TStringList;
  BSrcFiles     : string;
  BPath,
  BPathA        : string;
  BGrpIdx       : Integer;
  BRelName      : string;
  BNode         : TTreeNode;
Begin
 BList:=TStringList.Create;

 BSrcFiles:=FSrcFiles;
 repeat
 BRelName:=ReadParamStr(BSrcFiles);
 if BRelName='' then break;
 BPath:=ExtractFilePath(BRelName);
 if BPath='' then
  begin
  AddTreeItem(FNodeSrc,ocSrcF,AbsFilename(FPrjPath,BRelName),BRelName,SrcIdxByFileType(BRelName));
  end
 else if BList.IndexOf(BPath)=-1 then BList.Append(BPath);
 until FALSE;

 BGrpIdx:=0;
 while BGrpIdx<BList.Count do
  begin
  BPath:=BList.Strings[BGrpIdx];
  BNode:=AddTreeItem(FNodeSrc,ocGrpG,BPath,17);
  // Add subnodes
  BSrcFiles:=FSrcFiles;
  repeat
  BRelName:=ReadParamStr(BSrcFiles);
  if BRelName='' then break;
  BPathA:=ExtractFilePath(BRelName);
  if BPathA=BPath then
   begin
   AddTreeItem(BNode,ocSrcF,AbsFilename(FPrjPath,BRelName),ExtractFilename(BRelName),SrcIdxByFileType(BRelName));
   end;
  until FALSE;

  inc(BGrpIdx);
  end;

 BList.Free;
End;

// TvProject section

Procedure TPrjTree.TvProjectDblClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder=nil then break;
 if BNameHolder.ObjectClass in [ocSrcF] then
 else break;
 ViewAny('va '+BNameHolder.FullName);
 until TRUE;
End;

Function TPrjTree.GetNodePosOpti ( ANode : TTreeNode ) : TPoint;
Var
  BRect         : TRect;
  BPoint        : TPoint;
Begin
 BRect:=ANode.DisplayRect(TRUE);
 BPoint.X:=BRect.Left; BPoint.Y:=BRect.Bottom;
 Result:=TvProject.ClientToScreen(BPoint);
End;

Procedure TPrjTree.TvProjectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  BNameHolder   : TNameHolder;
  BCanEdit      : boolean;
  BPoint        : TPoint;
Begin
 BCanEdit:=FALSE;

 repeat
 FActionNode:=TvProject.GetNodeAt(X,Y);
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder=nil then break;
 BPoint:=GetNodePosOpti(FActionNode);

 BCanEdit:=BNameHolder.ObjectClass in [ocThrF, ocCpuF, ocMemF, ocPerF, ocLnkF];

 if (Button=mbRight) and (not (ssShift in Shift)) and (not (ssCtrl in Shift)) and (not (ssAlt in Shift)) then
  begin
  case BNameHolder.ObjectClass of
    // Hardware
    ocHrwG: PmHrwG.Popup(BPoint.X,BPoint.Y);
    ocHrwF: PmHrwF.Popup(BPoint.X,BPoint.Y);
    // Digital
    ocCpuG: PmCpuG.Popup(BPoint.X,BPoint.Y);
    ocCpuF: PmCpuF.Popup(BPoint.X,BPoint.Y);
    ocMemG: PmMemG.Popup(BPoint.X,BPoint.Y);
    ocMemF: PmMemF.Popup(BPoint.X,BPoint.Y);
    ocPerG: PmPerG.Popup(BPoint.X,BPoint.Y);
    ocPerF: PmPerF.Popup(BPoint.X,BPoint.Y);
    // Firmware
    ocThrG: PmThrG.Popup(BPoint.X,BPoint.Y);
    ocThrF: PmThrF.Popup(BPoint.X,BPoint.Y);
    ocIncG: PmIncG.Popup(BPoint.X,BPoint.Y);
    ocIncF: PmIncF.Popup(BPoint.X,BPoint.Y);
    ocOutG: PmOutG.Popup(BPoint.X,BPoint.Y);
    ocOutF: PmOutF.Popup(BPoint.X,BPoint.Y);
    ocSrcG: PmSrcG.Popup(BPoint.X,BPoint.Y);
    ocGrpG: PmGrpG.Popup(BPoint.X,BPoint.Y);
    ocSrcF: PmSrcF.Popup(BPoint.X,BPoint.Y);
    ocLocG: PmLocG.Popup(BPoint.X,BPoint.Y);
    ocLocF: PmLocF.Popup(BPoint.X,BPoint.Y);
    ocTstG: PmTstG.Popup(BPoint.X,BPoint.Y);
    ocTstF: PmTstF.Popup(BPoint.X,BPoint.Y);
    // Tools
    ocRvcG: PmRvcG.Popup(BPoint.X,BPoint.Y);
    ocRvcF: PmRvcF.Popup(BPoint.X,BPoint.Y);
    ocLnkG: PmLnkG.Popup(BPoint.X,BPoint.Y);
    ocLnkF: PmLnkF.Popup(BPoint.X,BPoint.Y);
    // Model
    ocModG: PmModG.Popup(BPoint.X,BPoint.Y);
    ocModF: PmModF.Popup(BPoint.X,BPoint.Y);

  {if BNameHolder.ObjType=otTstA then
   begin
   MTstMoveUp.Enabled:=BNode.getPrevSibling<>nil;
   MTstMoveDn.Enabled:=BNode.getNextSibling<>nil;
   PmTstA.Popup(BPoint.X,BPoint.Y);
   break;
   end;}
  end; // case
  end;

 until TRUE;

 TvProject.ReadOnly:=not BCanEdit;
End;

Procedure TPrjTree.TvProjectEdited(Sender: TObject; Node: TTreeNode; var S: string);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if Node=nil then break;
 BNameHolder:=TNameHolder(Node.Data);
 if BNameHolder=nil then break;
 case BNameHolder.ObjectClass of
   ocThrF: BNameHolder.Init(S);
   ocCpuF: BNameHolder.Init(S);
   ocMemF: BNameHolder.Init(S);
   ocPerF: BNameHolder.Init(S);
   ocLnkF: BNameHolder.Init(S);
   ocModF: BNameHolder.Init(S);
 end;
 until TRUE;
End;

Procedure TPrjTree.TvProjectDeletion(Sender: TObject; Node: TTreeNode);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 FActionNode:=nil;
 BNameHolder:=TNameHolder(Node.Data);
 if BNameHolder=nil then break;
 BNameHolder.Free;
 until TRUE;
End;

Procedure TPrjTree.CbGroupTreeClick(Sender: TObject);
Begin
 repeat
 if FInConstruction then break;
 FSrcFiles:=CollectItems(ocSrcF);
 FillSrcTree;
 until TRUE;
End;

// Submenu section

Procedure TPrjTree.MAnyF_MoveUpClick(Sender: TObject);
Begin
 if FActionNode<>nil then TreeXchgNode(FActionNode,FActionNode.GetPrevSibling);
End;

Procedure TPrjTree.MAnyF_MoveDnClick(Sender: TObject);
Begin
 if FActionNode<>nil then TreeXchgNode(FActionNode,FActionNode.GetNextSibling);
End;

Procedure TPrjTree.AddSrcNew ( Const APath : string );
Var
  BDialog       : TOpenDialog;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Create a new source file';
 BDialog.Filter:=CAllSourceFiles;
 BDialog.FilterIndex:=0;
 BDialog.Options:=BDialog.Options+[ofEnableSizing,ofViewDetail];
 BDialog.InitialDir:=APath;
 BDialog.DefaultExt:='';

 if BDialog.Execute then PrjAddFiles(BDialog.Files);

 BDialog.Free;
End;

Procedure TPrjTree.AddSrcOld ( Const APath : string );
Var
  BDialog       : TOpenDialog;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Open source file';
 BDialog.Filter:=CAllSourceFiles;
 BDialog.FilterIndex:=0;
 BDialog.Options:=BDialog.Options+[ofAllowMultiSelect,ofEnableSizing,ofViewDetail];
 BDialog.InitialDir:=APath;
 BDialog.DefaultExt:='';

 if BDialog.Execute then PrjAddFiles(BDialog.Files);

 BDialog.Free;
End;

Procedure TPrjTree.AddDllOld ( Const APath : string );
Var
  BDialog       : TOpenDialog;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Open source file';
 BDialog.Filter:='Library files (*.dll; *.so)|*.dll; *.so|All files (*.*)|*.*';
 BDialog.FilterIndex:=0;
 BDialog.Options:=BDialog.Options+[ofAllowMultiSelect,ofEnableSizing,ofViewDetail];
 BDialog.InitialDir:=APath;
 BDialog.DefaultExt:='';

 if BDialog.Execute then ModAddFiles(BDialog.Files);

 BDialog.Free;
End;

Procedure TPrjTree.MHrwG_AppendClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TOpenDialog;
  BNameIdx      : Integer;
  BFilename     : string;
Begin
 BDialog:=TOpenDialog.Create(Self);
 BDialog.Title:='Miramelle IDE: Append System configuration file';
 BDialog.Filter:='Json files (*.json)|*.json|All files (*.*)|*.*';
 BDialog.Options:=[ofAllowMultiSelect,ofEnableSizing,ofViewDetail];
 BDialog.InitialDir:=FRecentFolder;
 BDialog.DefaultExt:='';
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocHrwG then break;
 if BDialog.Execute=FALSE then break;
 BNameIdx:=0;
 while BNameIdx<BDialog.Files.Count do
  begin;
  BFilename:=BDialog.Files[BNameIdx];
  AddTreeItem(FNodeHrw,ocHrwF,AbsFilename(FPrjPath,BFilename),RelFilename(FPrjPath,BFilename),SrcIdxByFileType(BFilename));
  inc(BNameIdx);
  end;
 FRecentFolder:=BDialog.InitialDir;
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MHrwF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocHrwF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MSrcG_AddNewClick(Sender: TObject);
Var
  BPath         : string;
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocSrcG then break;
 BPath:='';
 if FRecentFolder<>'' then BPath:=FRecentFolder
 else if FPrjPath<>'' then BPath:=FPrjPath;
 AddSrcNew(BPath);
 until TRUE;
End;

Procedure TPrjTree.MSrcG_AddOldClick(Sender: TObject);
Var
  BPath         : string;
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocSrcG then break;
 BPath:='';
 if FRecentFolder<>'' then BPath:=FRecentFolder
 else if FPrjPath<>'' then BPath:=FPrjPath;
 AddSrcOld(BPath);
 until TRUE;
End;

Procedure TPrjTree.MGrpG_AddNewClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BPath         : string;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder=nil then break;
 if BNameHolder.ObjectClass<>ocGrpG then break;
 BPath:=AbsFilename(FPrjPath,BNameHolder.FullName);
 AddSrcNew(BPath);
 until TRUE;
End;

Procedure TPrjTree.MGrpG_AddOldClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BPath         : string;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder=nil then break;
 if BNameHolder.ObjectClass<>ocGrpG then break;
 BPath:=AbsFilename(FPrjPath,BNameHolder.FullName);
 AddSrcOld(BPath);
 until TRUE;
End;

Procedure TPrjTree.MSrcF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocSrcF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MCpuG_AppendClick(Sender: TObject);
Begin
 AddTreeItem(FNodeCpu,ocCpuF,'ssee','ssee',-1);
End;

Procedure TPrjTree.MCpuF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocCpuF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MMemG_AppendClick(Sender: TObject);
Begin
 AddTreeItem(FNodeMem,ocMemF,'Mem 00000000 1000','Mem 00000000 1000',22);
End;

Procedure TPrjTree.MMemF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocMemF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MPerG_AppendClick(Sender: TObject);
Begin
 AddTreeItem(FNodePer,ocPerF,'Perif UPerif 0x0010','Perif UPerif 0x0010',22);
End;

Procedure TPrjTree.MPerF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocPerF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MThrG_AppendClick(Sender: TObject);
Begin
 AddTreeItem(FNodeThr,ocThrF,'MyThread','MyThread',-1);
End;

Procedure TPrjTree.MThrF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocThrF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MIncG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TSelectDirectoryDialog;
  BPath         : string;
Begin
 BDialog:=TSelectDirectoryDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Select Include path';
 BDialog.InitialDir:=FRecentFolder;
 BDialog.DefaultExt:='';
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocIncG then break;
 if BDialog.Execute=FALSE then break;
 BPath:=BDialog.Filename;
 AddTreeItem(FNodeInc,ocIncF,AbsFilename(FPrjPath,BPath),RelFilename(FPrjPath,BPath),24);
 FRecentFolder:=BDialog.InitialDir;
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MIncF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocIncF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MOutG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TSelectDirectoryDialog;
  BPath         : string;
Begin
 BDialog:=TSelectDirectoryDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Select source path';
 BDialog.InitialDir:=FRecentFolder;
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocOutG then break;
 if BDialog.Execute=FALSE then break;
 BPath:=BDialog.Filename;
 AddTreeItem(FNodeOut,ocOutF,AbsFilename(FPrjPath,BPath),RelFilename(FPrjPath,BPath),26);
 FRecentFolder:=BDialog.InitialDir;
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MOutF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocOutF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MLocG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TSelectDirectoryDialog;
  BPath         : string;
Begin
 BDialog:=TSelectDirectoryDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Select source seach path (for ELF file)';
 BDialog.InitialDir:=FRecentFolder;
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocLocG then break;
 if BDialog.Execute=FALSE then break;
 BPath:=BDialog.Filename;
 AddTreeItem(FNodeOut,ocLocF,AbsFilename(FPrjPath,BPath),RelFilename(FPrjPath,BPath),17);
 FRecentFolder:=BDialog.InitialDir;
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MLocF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocLocF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MSrcF_OpenClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder=nil then break;
 if BNameHolder.ObjectClass<>ocSrcF then break;
 ViewAny('va '+BNameHolder.FullName);
 until TRUE;
End;

Procedure TPrjTree.MTstG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TSelectDirectoryDialog;
  BPath         : string;
Begin
 BDialog:=TSelectDirectoryDialog.Create(Self);
 BDialog.Title:='Mirabelle IDE: Select UnitTest path';
 BDialog.InitialDir:=FRecentFolder;
 BDialog.DefaultExt:='';
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocTstG then break;
 if BDialog.Execute=FALSE then break;
 BPath:=BDialog.Filename;
 AddTreeItem(FNodeTst,ocTstF,AbsFilename(FPrjPath,BPath),RelFilename(FPrjPath,BPath),34);
 FRecentFolder:=BDialog.InitialDir;
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MTstF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocTstF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MRvcG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BDialog       : TWndExtCompDialog;
  BParams       : string;
Begin
 BDialog:=TWndExtCompDialog.Create(Self);
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocRvcG then break;
 BDialog.SetParams('');
 BDialog.ShowModal;
 BParams:=BDialog.GetParams;
 if BParams='' then break;
 AddTreeItem(FNodeRvc,ocRvcF,BParams,BParams,33);
 until TRUE;
 BDialog.Free;
End;

Procedure TPrjTree.MRvcF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocRvcF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MLnkG_AppendClick (Sender: TObject);
Var
  BNameHolder   : TNameHolder;
  BParams       : string;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocLnkG then break;
 BParams:='';
 AddTreeItem(FNodeLnk,ocLnkF,BParams,BParams,35);
 until TRUE;
End;

Procedure TPrjTree.MLnkF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocLnkF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

Procedure TPrjTree.MModG_AppendClick (Sender: TObject);
Var
  BPath         : string;
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocModG then break;
 BPath:='';
 if FRecentFolder<>'' then BPath:=FRecentFolder
 else if FPrjPath<>'' then BPath:=FPrjPath;
 AddDllOld(BPath);
 until TRUE;
End;

procedure TPrjTree.MOutG_Append1Click(Sender: TObject);
begin

end;

Procedure TPrjTree.MModF_RemoveClick(Sender: TObject);
Var
  BNameHolder   : TNameHolder;
Begin
 repeat
 if FActionNode=nil then break;
 BNameHolder:=TNameHolder(FActionNode.Data);
 if BNameHolder.ObjectClass<>ocModF then break;
 TvProject.Items.Delete(FActionNode); FActionNode:=nil;
 until TRUE;
End;

end.

