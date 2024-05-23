unit AfItem_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, ExtCtrls, ComCtrls, SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterAny, SynHighlighterPas, SynHighlighterCpp,
  SynHighlighterPython, SynEditMiscClasses,
  ConComI, DbgInfo_sd, WasmProcess_sd;

type
  TOnUpdateUndoRedo = Procedure ( ACanUndo, ACanRedo : boolean ) of object;

  TZHighlighterAsm = class(TSynAnySyn)
  private
    FKeyWordsA,
    FKeyWordsB  : String;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Function IsKeyword ( Const AKeyword : String ) : boolean; Override;
  end;

  TZHighlighterSrv = class(TSynAnySyn)
  private
    FKeyWordsA,
    FKeyWordsB  : String;
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
    Function IsKeyword ( Const AKeyword : String ) : boolean; Override;
  end;

  TZHighlighterPas = class(TSynPasSyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  TZHighlighterCpp = class(TSynCppSyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  TZHighlighterRust = class(TSynAnySyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  TZHighlighterPy = class(TSynPythonSyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  TZHighlighterWasm = class(TSynAnySyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  TZHighlighterVhd = class(TSynAnySyn)
  private
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;
  end;

  { TAfItem }

  TAfItem = class(TFrame)
  private
    FParent     : TTabSheet;
    FFullName   : string;
    FShortName,
    FExt        : string;

    FOnUpdateUndoRedo   : TOnUpdateUndoRedo;
    FDbgOnSrcLineChange : TDbgOnSrcLineChange;

    FEdit           : TSynEdit;
    FHighlighter    : TSynCustomHighlighter;
    FStatBar        : TMsStatusBar;

    FDbgLine,
    FDbgPos         : Integer;

    FErrorWnd       : THintWindow;
    FErrorHint      : string;

    FIsDebugMode    : boolean;
    FLastCaretX,
    FLastCaretY     : Integer;
    FRunDirty       : boolean; // Needed to determine if file has to be recompiled. Even if changed and saved, FRunDirty can stay TRUE till next compilation
    FFileAge        : Integer;
    FSelfChanged    : boolean;

    Procedure EditChange ( ASender : TObject );
    Procedure EditClick ( ASender : TObject );
    Procedure EditKeyDown ( ASender : TObject; var AKey : Word; AShift : TShiftState );

    Procedure SViewHint;
    Procedure SaveCaretPos;
    Procedure IsCaretPosChanged;
    Procedure EditLineMarkup ( ASender : TObject; ALine : integer; Var ASpecial : boolean; AMarkup : TSynSelectedColor );
    Procedure BeforeSave;
    Function CheckExistsSave : boolean;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TTabSheet; Const AFullName : string; AOnUpdateUndoRedo : TOnUpdateUndoRedo; AOnDbgLineChange : TDbgOnSrcLineChange );
    Procedure Done;

    Function IsDirty : boolean;
    Procedure SaveDirty;
    Function IsRunDirty : boolean; // Shows if file has to be recompiled
    Procedure ResetRunDirty;       // Sets FRunDirty := FALSE
    Procedure GotoLinePos ( ALine, APos : Integer );
    Procedure ViewErrorHint ( AText : string );
    Procedure DestroyErrorHint;
    Procedure RefreshChangedYN;
    Procedure SetDbgLine ( ADbgInfoLine : TDbgInfoLine; ATextLine, ATextPos : Integer );
    Procedure UpdateLabels;

    property FullName : string read FFullName;
    property IsDebugMode : boolean read FIsDebugMode write FIsDebugMode;
    property Edit : TSynEdit read FEdit;
  end;

implementation

{$R *.lfm}

Uses
  ConComL, MMesBase_sd, AsmHelper_sd;

Procedure SetAttri ( AAttri : TSynHighlighterAttributes; AColorF : Cardinal; AStyle : TFontStyles );
Begin
 AAttri.Foreground:=AColorF;
 AAttri.Style:=AStyle;
End;

// Highlighters

{*** TZHighlighterAsm ***}

Constructor TZHighlighterAsm.Create ( AOwner : TComponent );
Var
  BObjectsAsm   : string;
Begin
 Inherited;
 FKeyWordsA:=' seg db dw dd dq public extern include align incdata as ';
 FKeyWordsB:=' '+CCmdNamesMs+' ';
 BObjectsAsm:='zl al bl cl dl el fl gl  mh ah bh ch dh eh fh gh  zx ax bx cx dx ex fx gx  mw aw bw cw dw ew fw gw  zwx awx bwx cwx dwx ewx fwx gwx esp ar br cr dr er fr gr  iq aq bq cq dq eq fq gq eip';
 while BObjectsAsm<>'' do Objects.Append(UpperCase(ReadParamStr(BObjectsAsm)));
 Comments:=[csAsmStyle];
End;

Destructor TZHighlighterAsm.Destroy;
Begin
 Inherited;
End;

Function TZHighlighterAsm.IsKeyword ( Const AKeyword : String ): boolean;
Var
  BDataS        : string;
Begin
 BDataS:=' '+LowerCase(AKeyword)+' ';
 Result:=(Pos(BDataS,FKeyWordsA)<>0) or (Pos(BDataS,FKeyWordsB)<>0);
End;

{*** TZHighlighterSrv ***}

Constructor TZHighlighterSrv.Create ( AOwner : TComponent );
Var
  BObjectsSrv   : string;
Begin
 Inherited;
 FKeyWordsA:=' align file option text data bss globl type size ident comm ';
 FKeyWordsB:=' '+CCmdNamesRV+' '+CCmdPseudoRV+' ';
 BObjectsSrv:='x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 eip';
 while BObjectsSrv<>'' do Objects.Append(UpperCase(ReadParamStr(BObjectsSrv)));
 BObjectsSrv:='zero ra sp gp tp t0 t1 t2 s0 s1 a0 a1 a2 a3 a4 a5';
 while BObjectsSrv<>'' do Objects.Append(UpperCase(ReadParamStr(BObjectsSrv)));
 Comments:=[csAsmStyle];
End;

Destructor TZHighlighterSrv.Destroy;
Begin
 Inherited;
End;

Function TZHighlighterSrv.IsKeyword ( Const AKeyword : String ): boolean;
Var
  BDataS        : string;
Begin
 BDataS:=' '+LowerCase(AKeyword)+' ';
 Result:=(Pos(BDataS,FKeyWordsA)<>0) or (Pos(BDataS,FKeyWordsB)<>0);
End;

{*** TZHighlighterPas ***}

Constructor TZHighlighterPas.Create ( AOwner : TComponent );
Begin
 Inherited;
 SetAttri(CommentAttri,$808080,[]);
 SetAttri(SymbolAttri,$0000C0,[]);
 SetAttri(NumberAttri,$008000,[]);
 SetAttri(StringAttri,$F04000,[]);
End;

Destructor TZHighlighterPas.Destroy;
Begin
 Inherited;
End;

{*** TZHighlighterCpp ***}

Constructor TZHighlighterCpp.Create ( AOwner : TComponent );
Begin
 Inherited;
 SetAttri(CommentAttri,$808080,[]);
 SetAttri(SymbolAttri,$0000C0,[]);
 SetAttri(NumberAttri,$008000,[]);
End;

Destructor TZHighlighterCpp.Destroy;
Begin
 Inherited;
End;

{*** TZHighlighterRust ***}

Constructor TZHighlighterRust.Create ( AOwner : TComponent );
Var
  BKeyWords     : string;
  BParamS       : string;
Begin
 Inherited;
 BKeywords:='let mut fn return '+
            'i8 i16 i32 u8 u16 u32 f32';
 repeat
 BParamS:=ReadParamStr(BKeywords);
 if BParamS='' then break;
 KeyWords.Append(UpperCase(BParamS));
 until FALSE;
 StringDelim:=sdDoubleQuote;
 Comments:=[csCStyle];
End;

Destructor TZHighlighterRust.Destroy;
Begin
 Inherited;
End;

{*** TZHighlighterWasm ***}

Constructor TZHighlighterWasm.Create ( AOwner : TComponent );
Var
  BKeyWords     : string;
  BParamS       : string;
Begin
 Inherited;
 BKeywords:='let mut fn return '+
            'i8 i16 i32 u8 u16 u32 f32';
 repeat
 BParamS:=ReadParamStr(BKeywords);
 if BParamS='' then break;
 KeyWords.Append(UpperCase(BParamS));
 until FALSE;
 StringDelim:=sdDoubleQuote;
 Comments:=[csCStyle];
End;

Destructor TZHighlighterWasm.Destroy;
Begin
 Inherited;
End;

{*** TZHighlighterPy ***}

Constructor TZHighlighterPy.Create ( AOwner : TComponent );
Begin
 Inherited;
 KeyWords.AddObject('external',TObject(Ord(tkKey)));
 KeyWords.AddObject('true',TObject(Ord(tkKey)));
 KeyWords.AddObject('false',TObject(Ord(tkKey)));
 KeyWords.AddObject('byte',TObject(Ord(tkNonKeyword)));
 KeyWords.AddObject('bool',TObject(Ord(tkNonKeyword)));
End;

Destructor TZHighlighterPy.Destroy;
Begin
 Inherited;
End;

{*** TZHighlighterVhd ***}

Constructor TZHighlighterVhd.Create ( AOwner : TComponent );
Var
  BKeywordsVhd  : string;
Begin
 Inherited;
 BKeywordsVhd:='module endmodule always assign wire reg input output inout parameter localparam if else posedge negedge begin end case endcase or task endtask for initial forever';
 while BKeywordsVhd<>'' do KeyWords.Append(UpperCase(ReadParamStr(BKeywordsVhd)));
 StringDelim:=sdDoubleQuote;
 Comments:=[csCStyle];
 SetAttri(CommentAttri,$808080,[]);
 SetAttri(SymbolAttri,$0000C0,[]);
 SetAttri(NumberAttri,$008000,[]);
End;

Destructor TZHighlighterVhd.Destroy;
Begin
 Inherited;
End;

// Editor

Constructor TAfItem.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TAfItem.Destroy;
Begin
 Inherited;
End;

Type
  TSpecialType = (stNone, stWasm);

Procedure TAfItem.Init ( AParent : TTabSheet; Const AFullName : string; AOnUpdateUndoRedo : TOnUpdateUndoRedo; AOnDbgLineChange : TDbgOnSrcLineChange );
Var
  BFilename     : string;
  BSpecialType  : TSpecialType;
  BErrorCode    : string;
Begin
 Name:='';
 FOnUpdateUndoRedo:=AOnUpdateUndoRedo;
 FDbgOnSrcLineChange:=AOnDbgLineChange;
 FParent:=AParent;
 FParent.InsertControl(Self); Align:=alClient;
 FFullName:=AFullName;

 BFileName:=ExtractFileName(FFullName);
 FShortName:=ReadTillC(BFileName,'.');
 FExt:=LowerCase(BFileName);
 FParent.Caption:=FShortName;

 BSpecialType:=stNone;
 FStatBar:=TMsStatusBar.Create(Self); InsertControl(FStatBar); FStatBar.Align:=alBottom;
 FEdit:=TSynEdit.Create(Self); InsertControl(FEdit); FEdit.Align:=alClient;
 FEdit.OnChange:=@EditChange;
 FEdit.OnClick:=@EditClick;
 FEdit.OnKeyDown:=@EditKeyDown;
 FEdit.Font.Size:=10;
 FEdit.Options:=FEdit.Options-[eoSmartTabs]+[eoTabsToSpaces]+[eoBracketHighlight];
 FEdit.OnSpecialLineMarkup:=@EditLineMarkup;
 if StrInList(FExt,'asm') then begin FParent.ImageIndex:=1; FHighlighter:=TZHighlighterAsm.Create(FEdit); end
 else if StrInList(FExt,'s srv i') then begin FParent.ImageIndex:=7; FHighlighter:=TZHighlighterSrv.Create(FEdit); end
 else if StrInList(FExt,'pas') then begin FParent.ImageIndex:=2; FHighlighter:=TZHighlighterPas.Create(FEdit); end
 else if StrInList(FExt,'c cpp ci') then begin FParent.ImageIndex:=3; FHighlighter:=TZHighlighterCpp.Create(FEdit); end
 else if StrInList(FExt,'py') then begin FParent.ImageIndex:=4; FHighlighter:=TSynPythonSyn.Create(FEdit); end
 else if StrInList(FExt,'rs') then begin FParent.ImageIndex:=5; FHighlighter:=TZHighlighterRust.Create(FEdit); end
 else if StrInList(FExt,'wasm') then begin BSpecialType:=stWasm; FParent.ImageIndex:=8; FHighlighter:=TZHighlighterWasm.Create(FEdit); FEdit.ReadOnly:=TRUE; end // ToDo: ## Add disassembly
 else if StrInList(FExt,'v') then begin FParent.ImageIndex:=6; FHighlighter:=TZHighlighterVhd.Create(FEdit); end
 else FParent.ImageIndex:=0;
 FEdit.Highlighter:=FHighlighter;

 repeat
 case BSpecialType of
   stWasm:
     begin
     if WasmParse(FFullName,FEdit.Lines,BErrorCode)=FALSE then
      begin
      VpMesOk(FParent,'Error','Error reading file'+#13+FFullName+#13+BErrorCode);
      end;
     try
       FFileAge:=FileAge(FFullName);
     except
       VpMesOk(FParent,'Error','Cannot read file'+#13+FFullName);
     end;
     end;
   else
     begin
     try
       FEdit.Lines.LoadFromFile(FFullName);
       FFileAge:=FileAge(FFullName);
     except
       VpMesOk(FParent,'Error','Cannot read file'+#13+FFullName);
     end;
     end;
 end; // case
 until TRUE;

 FStatBar.SetFilename(FFullName);
 FDbgLine:=-1;
 UpdateLabels;
End;

Procedure TAfItem.Done;
Begin
 FEdit.Highlighter:=nil;
 if FHighlighter<>nil then begin FHighlighter.Free; FHighlighter:=nil; end;
 RemoveControl(FEdit); FEdit.Free;
 RemoveControl(FStatBar); FStatBar.Free;
 FParent.RemoveControl(Self)
End;

Function TAfItem.IsDirty : boolean;
Begin
 Result:=FEdit.Modified;
End;

Procedure TAfItem.SaveDirty;
Begin
 repeat
 if FEdit.Modified=FALSE then break;
 try
   FEdit.Lines.SaveToFile(FFullName);
   FFileAge:=FileAge(FFullName);
 except
   VpMesOk(FParent,'Error','Cannot save file'+#13+FFullName);
   break;
 end;
 FEdit.Modified:=FALSE;
 FRunDirty:=TRUE;
 until TRUE;
End;

Function TAfItem.IsRunDirty : boolean;
Begin
 Result:=IsDirty or FRunDirty;
End;

Procedure TAfItem.ResetRunDirty;
Begin
 FRunDirty:=FALSE;
End;

Procedure TAfItem.GotoLinePos ( ALine, APos : Integer );
Var
  BLine,
  BPos          : Integer;
Begin
 BLine:=ALine;
 BPos:=APos;

 if BLine>=FEdit.Lines.Count then
  begin
  if FEdit.Lines.Count=0 then begin BLine:=0; BPos:=0; end
  else begin BLine:=FEdit.Lines.Count-1; BPos:=Length(FEdit.Lines.Strings[BLine]); end;
  end;

 FEdit.CaretX:=BPos+1;
 FEdit.CaretY:=BLine;
 FEdit.SetFocus;
End;

Procedure TAfItem.UpdateLabels;
Var
  BCaretPos,
  BModified,
  BIsInsert : string;
Begin
 BCaretPos:=IntToStr(FEdit.CaretY)+': '+IntToStr(FEdit.CaretX);
 if FEdit.Modified then BModified:='Modified' else BModified:='';
 if FEdit.InsertMode then BIsInsert:='INS' else BisInsert:='OVR';
 FStatBar.SetParams(BCaretPos,BModified,BIsInsert);
End;

// Events
Procedure TAfItem.EditChange ( ASender : TObject );
Begin
 DestroyErrorHint;
 UpdateLabels;
 if Assigned(FOnUpdateUndoRedo) then FOnUpdateUndoRedo(FEdit.CanUndo,FEdit.CanRedo);
 IsCaretPosChanged;
End;

Procedure TAfItem.EditClick ( ASender : TObject );
Begin
 DestroyErrorHint;
 UpdateLabels;
 IsCaretPosChanged;
End;

Procedure TAfItem.EditKeyDown ( ASender : TObject; var AKey : Word; AShift : TShiftState );
Begin
 DestroyErrorHint;
 UpdateLabels;
 IsCaretPosChanged;
End;

Procedure TAfItem.SetDbgLine ( ADbgInfoLine : TDbgInfoLine; ATextLine, ATextPos : Integer );
Begin
 repeat
 if ADbgInfoLine=nil then
  begin
  if FDbgLine<>-1 then
   begin
   FDbgLine:=-1; FDbgPos:=0;
   FEdit.Invalidate;
   end;
  break;
  end;
 if (FDbgLine<>ATextLine) or (FDbgPos<>ATextPos) then
  begin
  FDbgLine:=ATextLine; FDbgPos:=ATextPos;
  FEdit.CaretY:=FDbgLine; FEdit.CaretX:=FDbgPos;
  FEdit.Invalidate;
  end;
 until TRUE;
End;

Procedure TAfItem.EditLineMarkup ( ASender : TObject; ALine : integer; Var ASpecial : boolean; AMarkup : TSynSelectedColor );
Begin
 repeat
 if ALine<>FDbgLine then break;
 ASpecial:=TRUE;
 AMarkup.Foreground:=$000000;
 AMarkup.Background:=$80E0F0;
 until TRUE;
End;


Procedure TAfItem.ViewErrorHint ( AText : string );
Begin
 FErrorHint:=AText;
 //OzhPostMessageA(Handle,CViewHint,Integer(Self),0);
 SViewHint;
End;

Procedure TAfItem.SViewHint;
Var
  BText         : string;
  BPoint        : TPoint;
  BRect         : TRect;
  BHeight       : Integer;

Begin
 DestroyErrorHint;

 BHeight:=FEdit.Canvas.TextHeight('Ayla')+2;
 BText:=FErrorHint;
 if BText<>'' then
  begin
  BPoint.X:=FEdit.CaretXPix;
  BPoint.Y:=FEdit.CaretYPix;
  BPoint:=FEdit.ClientToScreen(BPoint);
  FErrorWnd:=THintWindow.Create(Self);
  BRect:=FErrorWnd.CalcHintRect(200,BText,nil);
  BRect.Top:=BRect.Top+BPoint.y+BHeight;
  BRect.Bottom:=BRect.Bottom+BPoint.y+BHeight;
  BRect.Left:=BRect.Left+BPoint.x;
  BRect.Right:=BRect.Right+BPoint.x;
  FErrorWnd.ActivateHint(BRect,BText);
  end;
End;

Procedure TAfItem.DestroyErrorHint;
Begin
 if FErrorWnd<>nil then
  begin
  FErrorWnd.OnClose:=nil;
  FErrorWnd.Free;
  FErrorWnd:=nil;
  end;
End;

Procedure TAfItem.RefreshChangedYN;
Var
  BFileAge      : Integer;
  BUpdate       : boolean;
Begin
 inc(FSelfChanged);

 BUpdate:=FALSE;

 repeat
 if CheckExistsSave then break;

 BFileAge:=FileAge(FFullName);
 if BFileAge=FFileAge then break;

 if VpMesYN(Self,'Warning','File'+#13+FFullName+#13+'was changed on disk.'+#13+'Would you like to reread it?')<>0 then break;

 BUpdate:=TRUE;

 FEdit.Lines.Clear;

 try
  FEdit.Lines.LoadFromFile(FFullName);
  FFileAge:=BFileAge;
 except
 end;

 until TRUE;

 dec(FSelfChanged);

 if BUpdate then FEdit.Invalidate;
End;


Procedure TAfItem.SaveCaretPos;
Begin
 FLastCaretX:=FEdit.CaretX; FLastCaretY:=FEdit.CaretY;
End;

Procedure TAfItem.IsCaretPosChanged;
Begin
 if FLastCaretY<>FEdit.CaretY then
  begin
  if (IsDirty=FALSE) and FIsDebugMode then
   begin
   FDbgLine:=FEdit.CaretY;
   FEdit.Invalidate;
   if Assigned(FDbgOnSrcLineChange) then FDbgOnSrcLineChange(FFullName,FEdit.CaretY);
   end;
  end;
 SaveCaretPos;
End;

Procedure TAfItem.BeforeSave;
Var
  i             : Integer;
  BDummyS       : string;
  BLength       : word;
  BPos          : word;

Begin
 i:=0;
 while i<FEdit.Lines.Count do
  begin
  BDummyS:=FEdit.Lines[i];
  BLength:=Length(BDummyS);
  BPos:=BLength;

  repeat
  if BPos=0 then break;
  if BDummyS[BPos]<>' ' then break;
  dec(BPos);
  until FALSE;

  if BPos<BLength then
   begin
   Delete(BDummyS,BPos+1,BLength-BPos);
   FEdit.Lines[i]:=BDummyS;
   end;

  inc(i);
  end;
End;

Function TAfItem.CheckExistsSave : boolean;
Begin
 Result:=FALSE;
 if FileExists(FFullName)=FALSE then
  begin
  VpMesOK(Self,'Warning','File'+#13+FFullName+#13+'is not found in the location');
  try
   BeforeSave;
   FEdit.Lines.SaveToFile(FFullName);
   FFileAge:=FileAge(FFullName);
  except
  end;
  Result:=TRUE;
  end;
End;

end.


