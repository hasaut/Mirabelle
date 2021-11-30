unit FrameMemA_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  TOnWndMemAReqData = Procedure ( Const ASegName : string ) of Object;

  { TSdWndMemA }

  TSdWndMemA = class(TFrame)
    MemBinView: TMemo;
  private
    FTextHex    : TStringList;
    FBinData    : string;
    FParent     : TWinControl;
    FSegStart,
    FSegSize    : Cardinal;
    FSegName    : string;

    FOnReqData  : TOnWndMemAReqData;

    Procedure ReqData;
    Procedure ParseBinData;

  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string );
    Procedure Deinit;

    Procedure SetActive ( AActive : boolean );
    Procedure GrayData;
    Procedure SetData ( Const AData : string );

    property SegName : string read FSegName;
    property OnReqData : TOnWndMemAReqData read FOnReqData write FOnReqData;

  end;

  TWndMemAList = array of TSdWndMemA;

Function WndMemAListAppend ( Var AList : TWndMemAList; AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string ) : TSdWndMemA;
Procedure WndMemAListClear ( Var AList : TWndMemAList );

implementation

{$R *.lfm}

Function WndMemAListAppend ( Var AList : TWndMemAList; AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string ) : TSdWndMemA;
Var
  BIndex    : Integer;
Begin
 Result:=TSdWndMemA.Create(AParent); Result.Name:='';
 BIndex:=Length(AList); SetLength(AList,BIndex+1); AList[BIndex]:=Result;
 Result.Init(AParent,ASegStart,ASegSize,ASegName);
End;

Procedure WndMemAListClear ( Var AList : TWndMemAList );
Var
  BIndex    : Integer;
  BWndMemA  : TSdWndMemA;
Begin
 BIndex:=0;
 while BIndex<Length(AList) do
  begin
  BWndMemA:=AList[BIndex];
  BWndMemA.Deinit; BWndMemA.Free;
  inc(BIndex);
  end;
 AList:=nil;
End;

Constructor TSdWndMemA.Create ( AOwner : TComponent );
Begin
 Inherited;
 FTextHex:=TStringList.Create;
End;

Destructor TSdWndMemA.Destroy;
Begin
 FTextHex.Free;
 Inherited;
End;

Procedure TSdWndMemA.Init ( AParent : TWinControl; ASegStart, ASegSize : Cardinal; Const ASegName : string );
Begin
 FParent:=AParent; FParent.InsertControl(Self);
 Visible:=FALSE; Align:=alClient;
 FSegStart:=ASegStart;
 FSegSize:=ASegSize;
 FSegName:=ASegName;
 MemBinView.Align:=alClient; MemBinView.Lines.Clear;
End;

Procedure TSdWndMemA.Deinit;
Begin
 FParent.RemoveControl(Self);
End;

Procedure TSdWndMemA.SetActive ( AActive : boolean );
Begin
 Visible:=AActive;
 if AActive then begin GrayData; ReqData; end;
End;

Procedure TSdWndMemA.GrayData;
Begin
 MemBinView.Font.Color:=$C0C0C0;
End;

Procedure TSdWndMemA.ReqData;
Begin
 if Assigned(FOnReqData) then FOnReqData(FSegName);
End;

Procedure TSdWndMemA.SetData ( Const AData : string );
Begin
 FBinData:=AData;
 ParseBinData;
End;

Function FormatLineData ( AAddr : Cardinal; Const AData : string ) : string;
Var
  BCharIdx  : Integer;
  BChar     : char;
  BPartTxt  : string;
  BDataB    : byte;
Begin
 Result:=IntToHex(AAddr,5)+': ';
 BPartTxt:='';
 BCharIdx:=0;
 while BCharIdx<Length(AData) do
  begin
  BChar:=AData[1+BCharIdx]; BDataB:=Ord(BChar);
  Result:=Result+IntToHex(BDataB,2)+' ';
  if (32<=BDataB) and (BDataB<127) then BPartTxt:=BPartTxt+BChar
  else BPartTxt:=BPartTxt+'.';
  inc(BCharIdx);
  end;
 while BCharIdx<16 do
  begin
  Result:=Result+'   ';
  inc(BCharIdx);
  end;
 Result:=Result+'| '+BPartTxt;
End;

Procedure TSdWndMemA.ParseBinData;
Var
  BBinData      : string;
  BLineIdx      : Cardinal;
  BLineData     : string;
  BCopyLen      : Integer;
Begin
 FTextHex.Clear;
 BBinData:=FBinData;
 BLineIdx:=0;
 while BBinData<>'' do
  begin
  BCopyLen:=Length(BBinData); if BCopyLen>16 then BCopyLen:=16;
  BLineData:=Copy(BBinData,1,BCopyLen); Delete(BBinData,1,BCopyLen);
  FTextHex.Append(FormatLineData(FSegStart+BLineIdx*16,BLineData));
  inc(BLineIdx);
  end;
 MemBinView.Lines.Assign(FTextHex);
 MemBinView.Font.Color:=$000000;
End;


end.

