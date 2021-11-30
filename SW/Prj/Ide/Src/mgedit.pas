unit MgEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls,
  AfItem_sd, ConComI, DbgInfo_sd;

type
  TPcEditMouse = record
    FDown   : boolean;
    FX,
    FY      : Integer;
  end;

  { TMgEdit }
  TMgEdit = class(TFrame)
    IlTabs: TImageList;
    procedure PcEditCloseTabClicked(Sender: TObject);
    procedure PcEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PcEditMouseLeave(Sender: TObject);
    procedure PcEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PcEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FParent             : TWinControl;
    FPcEdit             : TPageControl;
    FPcEditDragTabIdx   : Integer;
    FOnUpdateUndoRedo   : TOnUpdateUndoRedo;
    FDbgOnSrcLineChange : TDbgOnSrcLineChange;

    FPcEditChangeLock   : Integer;
    FPcEditMouse        : TPcEditMouse;

    Function GetAfItem ( ATabIdx : Integer ) : TAfItem;
    Function ClosePage ( APageIdx : Integer; AForceIfError : boolean ) : boolean;

    Procedure PcEditChange ( Sender: TObject );
    Procedure PcEditStartDrag ( ASender : TObject; Var ADragObject : TDragObject );
    Procedure PcEditDragOver ( ASender, ASource : TObject; AX, AY : Integer; AState: TDragState; Var AAccept : Boolean );
    Procedure PcEditDragDrop ( ASender, ASource : TObject; AX, AY : Integer );

    Function CreateNew ( Const AFullName : string ) : boolean;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TWinControl; AOnUpdateUndoRedo : TOnUpdateUndoRedo; ADbgOnSrcLineChange : TDbgOnSrcLineChange );
    Procedure Done;

    Function AddOpen ( Const AFullName : string; ACreate : boolean; AAtEnd : boolean ) : TAfItem;
    Procedure CloseActive;
    Procedure CloseAll;
    Procedure SaveActive;
    Function SaveDirty : boolean;
    Function IsRunDirty : boolean;
    Procedure ResetRunDirty;
    Procedure DestroyErrorHint;
    Procedure RefreshChangedYN;

    Procedure DebugModeSet;
    Procedure DebugModeClr;
    Procedure SetDbgLine ( AAfItem : TAfItem; AInfoLine : TDbgInfoLine; ATextLine, ATextPos : Integer );
    Procedure LocalizeError ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string );

    Function GetOpenList ( Const APrjPath : string ) : string;
    Function GetActiveFile ( Const APrjPath : string ) : string;
    Procedure SetActiveFile ( Const APrjPath, AFilename : string );
    Function ActiveItem : TAfItem;
  end;

implementation

Uses
  ConComS, MMesBase_sd;

{$R *.lfm}

// Editor

Constructor TMgEdit.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TMgEdit.Destroy;
Begin
 Inherited;
End;

Procedure TMgEdit.Init ( AParent : TWinControl; AOnUpdateUndoRedo : TOnUpdateUndoRedo; ADbgOnSrcLineChange : TDbgOnSrcLineChange );
Begin
 FOnUpdateUndoRedo:=AOnUpdateUndoRedo;
 FDbgOnSrcLineChange:=ADbgOnSrcLineChange;
 FParent:=AParent;
 FParent.InsertControl(Self);
 Align:=alClient;
 FPcEdit:=TPageControl.Create(Self); InsertControl(FPcEdit);
 FPcEdit.Align:=alClient;
 FPcEdit.Images:=ilTabs;
 FPcEdit.OnChange:=@PcEditChange;
 FPcEdit.OnStartDrag:=@PcEditStartDrag;
 FPcEdit.OnDragOver:=@PcEditDragOver;
 FPcEdit.OnDragDrop:=@PcEditDragDrop;
 FPcEdit.OnMouseDown:=@PcEditMouseDown;
 FPcEdit.OnMouseUp:=@PcEditMouseUp;
 FPcEdit.OnMouseLeave:=@PcEditMouseLeave;
 FPcEdit.OnMouseMove:=@PcEditMouseMove;
 //FPcEdit.OnCloseTabClicked:=@PcEditCloseTabClicked;
 FPcEdit.DragMode:=dmManual;
 //FPcEdit.Options:=[nboShowCloseButtons];
End;

Procedure TMgEdit.Done;
Begin
 CloseAll;
 RemoveControl(FPcEdit); FPcEdit.Free;
 FParent.RemoveControl(Self);
End;

procedure TMgEdit.PcEditChange(Sender: TObject);
begin
 if FPcEditChangeLock=0 then
  begin
  DestroyErrorHint;
  RefreshChangedYN;
  end;
end;

Procedure TMgEdit.PcEditStartDrag ( ASender : TObject; Var ADragObject : TDragObject );
Var
  BPgCtrl       : TPageControl;
Begin
 BPgCtrl:=ASender as TPageControl;
 FPcEditDragTabIdx:=BPgCtrl.PageIndex;
End;

Procedure TMgEdit.PcEditDragOver ( ASender, ASource : TObject; AX, AY : Integer; AState: TDragState; Var AAccept : Boolean );
Var
  BPgCtrl       : TPageControl;
Begin
 BPgCtrl:=ASender as TPageControl;
 AAccept:=(ASender=ASource) and (BPgCtrl.IndexOfTabAt(AX,AY)>=0);
End;

Procedure TMgEdit.PcEditDragDrop ( ASender, ASource : TObject; AX, AY : Integer );
Var
  BDropIndex     : Integer;
  BPgCtrl        : TPageControl;
Begin
 BPgCtrl:=ASender as TPageControl;
 repeat
 if FPcEditDragTabIdx<0 then break;
 if FPcEditDragTabIdx>=BPgCtrl.PageCount then break;
 if BPgCtrl<>FPcEdit then break;
 if ASender<>ASource then break;
 BDropIndex:=BPgCtrl.IndexOfTabAt(AX,AY);
 if BDropIndex>=0 then
  begin
  if BDropIndex<>FPcEditDragTabIdx then BPgCtrl.Pages[FPcEditDragTabIdx].PageIndex:=BDropIndex;
  BPgCtrl.TabIndex:=BDropIndex;
  end;
 until TRUE;
End;

Procedure TMgEdit.PcEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 if ((ssShift in Shift)=FALSE) and (Button=mbLeft) then
  begin
  FPcEditMouse.FDown:=TRUE;
  FPcEditMouse.FX:=X;
  FPcEditMouse.FY:=Y;
  end;
End;

Procedure TMgEdit.PcEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  BDX   : Integer;
Begin
 if ((ssShift in Shift)=FALSE) and FPcEditMouse.FDown then
  begin
  BDX:=Abs(X-FPcEditMouse.FX);
  if BDX>3 then FPcEdit.BeginDrag(FALSE);
  end;
End;

Procedure TMgEdit.PcEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
 FPcEditMouse.FDown:=FALSE;
End;

Procedure TMgEdit.PcEditMouseLeave(Sender: TObject);
Begin
 FPcEditMouse.FDown:=FALSE;
End;

Procedure TMgEdit.PcEditCloseTabClicked(Sender: TObject);
Begin
 CloseActive;
End;

Function TMgEdit.GetAfItem ( ATabIdx : Integer ) : TAfItem;
Var
  BSheet    : TTabSheet;
Begin
 Result:=nil;
 repeat
 BSheet:=FPcEdit.Pages[ATabIdx];
 if BSheet=nil then break;
 if BSheet.ControlCount=0 then break;
 Result:=TAfItem(Pointer(BSheet.Controls[0]));
 until TRUE;
End;

Function TMgEdit.ActiveItem : TAfItem;
Begin
 Result:=nil;
 repeat
 if FPcEdit.PageCount=0 then break;
 if FPcEdit.ActivePageIndex<0 then break;
 Result:=GetAfItem(FPcEdit.ActivePageIndex);
 until TRUE;
End;

Function TMgEdit.SaveDirty : boolean;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 Result:=TRUE;
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  if BAfItem<>nil then begin BAfItem.SaveDirty; if BAfItem.IsDirty then Result:=FALSE; end;
  inc(BTabIdx);
  end;
End;

Function TMgEdit.IsRunDirty : boolean;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 Result:=FALSE;
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  if BAfItem.IsRunDirty then break;
  inc(BTabIdx);
  end;
 Result:=BTabIdx<FPcEdit.PageCount;
End;

Procedure TMgEdit.ResetRunDirty;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  BAfItem.ResetRunDirty;
  inc(BTabIdx);
  end;
End;

Procedure TMgEdit.DestroyErrorHint;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  if BAfItem<>nil then BAfItem.DestroyErrorHint;
  inc(BTabIdx);
  end;
End;

Procedure TMgEdit.RefreshChangedYN;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  if BAfItem<>nil then BAfItem.RefreshChangedYN;
  inc(BTabIdx);
  end;
End;

Function TMgEdit.CreateNew ( Const AFullName : string ) : boolean;
Var
  BList     : TStringList;
Begin
 Result:=TRUE;
 BList:=TStringList.Create;
 try
   BList.SaveToFile(AFullName);
 except
   Result:=FALSE;
 end;
 BList.Free;
End;

Function TMgEdit.AddOpen ( Const AFullName : string; ACreate : boolean; AAtEnd : boolean ) : TAfItem;
Var
  BPath         : string;
  BAfIndex      : Integer;
  BAfItem       : TAfItem;
  BSheet        : TTabSheet;
Begin
 inc(FPcEditChangeLock);
 Result:=nil;
 repeat
 BAfIndex:=0;
 while BAfIndex<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BAfIndex);
  if BAfItem.FullName=AFullName then begin Result:=BAfItem; break; end;
  inc(BAfIndex);
  end;
 if Result<>nil then
  begin
  if BAfIndex<FPcEdit.PageCount then FPcEdit.PageIndex:=BAfIndex;
  break;
  end;

 if FileExists(AFullName)=FALSE then
  begin
  if ACreate=FALSE then break;
  BPath:=ExtractFilePath(AFullName);
  if DirectoryExists(BPath)=FALSE then
   begin
   if VpMesYN(Self,'Warning','Folder'+#13+BPath+#13+'does not exist.'+#13+'Would you like to create it?')<>0 then break;
   if CreateDir(BPath)=FALSE then begin VpMesOk(Self,'Error','Cannot create directory'+#13+BPath); break; end;
   end;
  if VpMesYN(Self,'Warning','File'+#13+AFullName+#13+'does not exist.'+#13+'Would you like to create it?')<>0 then break;
  if CreateNew(AFullName)=FALSE then VpMesOK(Self,'Warning','Cannot create file'+#13+AFullName);
  end;
 BSheet:=FPcEdit.AddTabSheet; BSheet.PageControl:=FPcEdit;
 if AAtEnd then BAfIndex:=FPcEdit.PageCount-1
 else if FPcEdit.PageCount=0 then BAfIndex:=0
 else BAfIndex:=FPcEdit.PageIndex+1;
 Result:=TAfItem.Create(Self); BSheet.PageIndex:=BAfIndex;
 Result.Init(BSheet,AFullName,FOnUpdateUndoRedo,FDbgOnSrcLineChange);
 FPcEdit.PageIndex:=BAfIndex;
 until TRUE;
 dec(FPcEditChangeLock);
End;

Function TMgEdit.ClosePage ( APageIdx : Integer; AForceIfError : boolean ) : boolean;
Var
  BAfItem   : TAfItem;
  BSheet    : TTabSheet;
Begin
 inc(FPcEditChangeLock);
 Result:=FALSE;
 repeat
 if APageIdx>=FPcEdit.PageCount then break;
 BAfItem:=GetAfItem(APageIdx);
 if (BAfItem<>nil) and BAfItem.IsDirty then
  begin
  if AForceIfError then BAfItem.SaveDirty
  else
   begin
   if VpMesYN(Self,'Warning','File'+#13+BAfItem.FullName+'Is not saved.'+#13+'Would you like to save it?')=0 then
    begin
    BAfItem.SaveDirty;
    if BAfItem.IsDirty then
     begin
     if VpMesYN(Self,'Warning','Error saving file'+#13+BAfItem.FullName+'Would you like to close it anyway?')<>0 then break;
     end;
    end;
   end;
  end;
 BAfItem.Done; BAfItem.Free;
 BSheet:=FPcEdit.Pages[APageIdx]; BSheet.Free;
 Result:=TRUE;
 until TRUE;
 dec(FPcEditChangeLock);
End;

Procedure TMgEdit.CloseActive;
Var
  BAfIndex  : Integer;
Begin
 repeat
 if FPcEdit.PageIndex<0 then break;
 BAfIndex:=FPcEdit.PageIndex;
 if ClosePage(BAfIndex,FALSE)=FALSE then
 else
  begin
  if BAfIndex>=FPcEdit.PageCount then BAfIndex:=FPcEdit.PageCount-1;
  if BAfIndex>=0 then FPcEdit.PageIndex:=BAfIndex;
  end;
 until TRUE;
End;

Procedure TMgEdit.CloseAll;
Var
  BAfIndex  : Integer;
Begin
 BAfIndex:=0;
 while FPcEdit.PageCount<>0 do
  begin
  ClosePage(BAfIndex,TRUE);
  end;
 FPcEdit.ActivePage:=nil;
End;

Procedure TMgEdit.SaveActive;
Var
  BAfItem   : TAfItem;
Begin
 repeat
 if FPcEdit.PageCount=0 then break;
 if FPcEdit.PageIndex<0 then break;
 BAfItem:=GetAfItem(FPcEdit.PageIndex);
 if BAfItem.IsDirty then begin BAfItem.SaveDirty; BAfItem.UpdateLabels; end;
 until TRUE;
End;

Function TMgEdit.GetOpenList ( Const APrjPath : string ) : string;
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
Begin
 Result:='';
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  if BAfItem<>nil then
   begin
   if Result<>'' then Result:=Result+#32;
   Result:=Result+RelFilename(APrjPath,BAfItem.FullName);
   end;
  inc(BTabIdx);
  end;
End;

Function TMgEdit.GetActiveFile ( Const APrjPath : string ) : string;
Var
  BAfItem   : TAfItem;
Begin
 Result:='';
 repeat
 BAfItem:=ActiveItem;
 if BAfItem=nil then break;
 Result:=RelFilename(APrjPath,BAfItem.FullName);
 until TRUE;
End;

Procedure TMgEdit.SetActiveFile ( Const APrjPath, AFilename : string );
Var
  BTabIdx   : Integer;
  BAfItem   : TAfItem;
  BFilename : string;
Begin
 repeat
 if AFilename='' then break;
 BTabIdx:=0;
 while BTabIdx<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BTabIdx);
  BFilename:=RelFilename(APrjPath,BAfItem.FullName);
  if BFilename=AFilename then break;
  inc(BTabIdx);
  end;
 if BTabIdx<FPcEdit.PageCount then FPcEdit.ActivePageIndex:=BTabIdx;
 until TRUE;
End;

Procedure TMgEdit.DebugModeSet;
Var
  BIndex    : Integer;
  BAfItem   : TAfItem;
Begin
 BIndex:=0;
 while BIndex<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BIndex);
  if BAfItem<>nil then BAfItem.IsDebugMode:=TRUE;
  inc(BIndex);
  end;
End;

Procedure TMgEdit.DebugModeClr;
Var
  BIndex    : Integer;
  BAfItem   : TAfItem;
Begin
 BIndex:=0;
 while BIndex<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BIndex);
  if BAfItem<>nil then begin BAfItem.IsDebugMode:=FALSE; BAfItem.SetDbgLine(nil,-1,-1); end;
  inc(BIndex);
  end;
End;

Procedure TMgEdit.SetDbgLine ( AAfItem : TAfItem; AInfoLine : TDbgInfoLine; ATextLine, ATextPos : Integer );
Var
  BIndex    : Integer;
  BAfItem   : TAfItem;
Begin
 BIndex:=0;
 while BIndex<FPcEdit.PageCount do
  begin
  BAfItem:=GetAfItem(BIndex);
  repeat
  if BAfItem=nil then break;
  if BAfItem=AAfItem then BAfItem.SetDbgLine(AInfoLine,ATextLine,ATextPos)
  else BAfItem.SetDbgLine(nil,-1,-1);
  until TRUE;
  inc(BIndex);
  end;
End;

Procedure TMgEdit.LocalizeError ( Const AFilename : string; ATextL, ATextP : Integer; Const AComment : string );
Var
  BLine,
  BPos      : Integer;
  BAfItem   : TAfItem;
Begin
 DestroyErrorHint;

 repeat
 BAfItem:=AddOpen(AFilename,FALSE,TRUE);
 if BAfItem=nil then break;
 BLine:=ATextL; BPos:=ATextP;
 if BPos=0 then
  begin
  if BLine>0 then
   begin
   dec(BLine);
   if BAfItem.Edit.Lines.Count>BLine then BPos:=Length(BAfItem.Edit.Lines.Strings[BLine]);
   end;
  end
 else dec(BPos);
 BAfItem.GotoLinePos(BLine,BPos);
 BAfItem.ViewErrorHint(AComment);
 until TRUE;

End;

end.

