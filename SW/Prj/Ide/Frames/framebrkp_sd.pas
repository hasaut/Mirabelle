unit FrameBrkp_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, Buttons,
  StdCtrls;

type

  { TWndBrkpSd }

  TOnBrkpChange = Procedure ( Const AList : string ) of object;
  TOnShowBrkp = Procedure ( AAddr : Cardinal ) of object;

  TWndBrkpSd = class(TFrame)
    BtDel: TBitBtn;
    BtAdd: TBitBtn;
    EdAddr: TEdit;
    ImageList1: TImageList;
    LvList: TListView;
    PnlBot: TPanel;
    procedure BtAddClick(Sender: TObject);
    procedure BtDelClick(Sender: TObject);
    procedure LvListDblClick(Sender: TObject);
    procedure LvListResize(Sender: TObject);
  private
    FSheet      : TTabSheet;
    FBrkpChange : TOnBrkpChange;
    FResizeLock : boolean;
    FOnShowBrkp : TOnShowBrkp;

    Procedure UpdateTabName;
    Procedure AppendAddr ( AAddr : Cardinal );
    Function ItemByAddr ( AAddr : Cardinal ) : TListItem;
    //Function GetSrcLine ( ALine : TDbgInfoLine ) : string;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet );
    Procedure Done;

    Procedure Clear;
    Procedure SetAddrList ( Const AAddrList : string );
    Function CollectAddrList : string;

    //Procedure SetIpLine ( ALine : TDbgInfoLine );

    property OnBrkpChange : TOnBrkpChange read FBrkpChange write FBrkpChange;
    property OnShowBrkp : TOnShowBrkp read FOnShowBrkp write FOnShowBrkp;
  end;

implementation

{$R *.lfm}

Uses
  ConComL;

Constructor TWndBrkpSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndBrkpSd.Destroy;
Begin
 Inherited;
End;

Procedure TWndBrkpSd.Init ( ASheet : TTabSheet );
Begin
 FResizeLock:=TRUE;
 FSheet:=ASheet;
 FSheet.InsertControl(Self);
 LvList.Align:=alClient;
 FResizeLock:=FALSE;
 Align:=alClient;
 BtAdd.Height:=EdAddr.Height;
 BtDel.Height:=EdAddr.Height;
 PnlBot.ClientHeight:=EdAddr.Height+4;
 EdAddr.Text:='';
 Clear;
End;

Procedure TWndBrkpSd.Done;
Begin
 Clear;
 FResizeLock:=TRUE;
 FSheet.RemoveControl(Self);
End;

Procedure TWndBrkpSd.Clear;
Begin
 LvList.Clear;
 UpdateTabName;
End;

Procedure TWndBrkpSd.LvListResize(Sender: TObject);
Var
  BWidth    : Integer;
Begin
 repeat
 if FResizeLock then break;
 LvList.Columns[0].Width:=50+LvList.Canvas.TextWidth('000000');
 BWidth:=LvList.ClientWidth-LvList.Columns[0].Width;
 LvList.Columns[1].Width:=Round(0.3*BWidth);
 LvList.Columns[2].Width:=Round(0.3*BWidth);
 LvList.Columns[3].Width:=LvList.ClientWidth-LvList.Columns[0].Width-LvList.Columns[1].Width-LvList.Columns[2].Width-20;
 until TRUE;
End;

Procedure TWndBrkpSd.UpdateTabName;
Begin
 FSheet.Caption:='Break points ('+IntToStr(LvList.Items.Count)+')';
End;

Procedure TWndBrkpSd.AppendAddr ( AAddr : Cardinal );
Var
  BItem     : TListItem;
  BAddrS    : string;
  BItemIdx  : Integer;
Begin
 BAddrS:=IntToHex(AAddr,6);
 BItemIdx:=0;
 while BItemIdx<LvList.Items.Count do
  begin
  BItem:=LvList.Items[BItemIdx];
  if BItem.Caption=BAddrS then break;
  inc(BItemIdx);
  end;
 if BItemIdx>=LvList.Items.Count then
  begin
  BItem:=LvList.Items.Add;
  BItem.Caption:=BAddrS;
  end;
 UpdateTabName;
End;

Procedure TWndBrkpSd.SetAddrList ( Const AAddrList : string );
Var
  BAddrList : string;
  BAddrS    : string;
  BAddr     : Cardinal;
  BItemIdx  : Integer;
  BItem     : TListItem;
Begin
 BItemIdx:=0;
 while BItemIdx<LvList.Items.Count do
  begin;
  BItem:=LvList.Items[BItemIdx];
  BAddrList:=AAddrList;
  repeat
  BAddrS:=ReadParamStr(BAddrList);
  if BAddrS='' then break;
  if BItem.Caption=BAddrS then break;
  until FALSE;
  if BAddrS='' then LvList.Items.Delete(BItemIdx)
  else inc(BItemIdx);
  end;

 BAddrList:=AAddrList;
 repeat
 BAddrS:=ReadParamStr(BAddrList);
 if BAddrS='' then break;
 if HexToDWordCheck(BAddrS,BAddr) then AppendAddr(BAddr);
 until FALSE;
End;

Function TWndBrkpSd.CollectAddrList : string;
Var
  BItemIdx  : Integer;
  BItem     : TListItem;
  BAddrS    : string;
  BAddr     : Cardinal;
Begin
 Result:='';
 BItemIdx:=0;
 while BItemIdx<LvList.Items.Count do
  begin
  BItem:=LvList.Items[BItemIdx];
  BAddrS:=BItem.Caption;
  if HexToDWordCheck(BAddrS,BAddr) then
   begin
   if Result<>'' then Result:=Result+#32;
   Result:=Result+IntToHex(BAddr,6);
   end;
  inc(BItemIdx);
  end;
End;

Procedure TWndBrkpSd.BtAddClick(Sender: TObject);
Var
  BAddr     : Cardinal;
  BAddrList : string;
Begin
 repeat
 if HexToDWordCheck(EdAddr.Text,BAddr)=FALSE then break;
 BAddrList:=CollectAddrList;
 if BAddrList<>'' then BAddrList:=BAddrList+#32;
 BAddrList:=BAddrList+IntToHex(BAddr,6);
 SetAddrList(BAddrList);
 if Assigned(FBrkpChange) then FBrkpChange(BAddrList);
 until TRUE;
End;

Procedure TWndBrkpSd.BtDelClick(Sender: TObject);
Var
  BItemIdx  : Integer;
  BItem     : TListItem;
  BAddrList : string;
Begin
 BItemIdx:=0;
 while BItemIdx<LvList.Items.Count do
  begin
  BItem:=LvList.Items[BItemIdx];
  if BItem.Checked then LvList.Items.Delete(BItemIdx)
  else inc(BItemIdx);
  end;
 BAddrList:=CollectAddrList;
 SetAddrList(BAddrList);
 if Assigned(FBrkpChange) then FBrkpChange(BAddrList);
End;

Function TWndBrkpSd.ItemByAddr ( AAddr : Cardinal ) : TListItem;
Var
  BItemIdx  : Integer;
  BItem     : TListItem;
  BAddr     : Cardinal;
Begin
 Result:=nil;
 BItemIdx:=0;
 while BItemIdx<LvList.Items.Count do
  begin
  BItem:=LvList.Items[BItemIdx];
  HexToDWordCheck(BItem.Caption,BAddr);
  if BAddr=AAddr then begin Result:=BItem; break; end;
  inc(BItemIdx);
  end;
End;

{Function TWndBrkpSd.GetSrcLine ( ALine : TDbgInfoLine ) : string;
Var
  BList     : TStringList;
Begin
 BList:=nil;
 Result:='';
 repeat
 if ALine.SrcFile='' then break;
 if ALine.SrcIdxL<=0 then break;
 if FileExists(ALine.SrcFile)=FALSE then break;
 BList:=TStringList.Create;
 try
   BList.LoadFromFile(ALine.SrcFile);
 except
   break;
 end;
 if ALine.SrcIdxL>=BList.Count then break;
 Result:=BList.Strings[ALine.SrcIdxL-1];
 until TRUE;
 if BList<>nil then BList.Free;
End;

Procedure TWndBrkpSd.SetIpLine ( ALine : TDbgInfoLine );
Var
  BItem     : TListItem;
  BTypeS,
  BAddrS,
  BDataS,
  BOrigS    : string;
  BAddr     : Cardinal;
  BSegType  : TAsmSegType;
Begin
 repeat
 if ALine=nil then break;
 SplitLineLst(ALine.Readable,BTypeS,BAddrS,BDataS,BOrigS,BSegType);
 if HexToDWordCheck(BAddrS,BAddr)=FALSE then break;
 BItem:=ItemByAddr(BAddr); if BItem=nil then break;
 BItem.SubItems.Clear;
 BItem.SubItems.Append(ALine.ModuleName);
 BItem.SubItems.Append(BOrigS);
 BItem.SubItems.Append(GetSrcLine(ALine));
 until TRUE;
End;}

Procedure TWndBrkpSd.LvListDblClick(Sender: TObject);
Var
  BLineIdx  : Integer;
  BItem     : TListItem;
  BAddr     : Cardinal;
Begin
 repeat
 BLineIdx:=LvList.ItemIndex;
 if BLineIdx<0 then break;
 BItem:=LvList.Items[BLineIdx];
 HexToDWordCheck(BItem.Caption,BAddr);
 if Assigned(FOnShowBrkp) then FOnShowBrkp(BAddr);
 until TRUE;
End;


end.

