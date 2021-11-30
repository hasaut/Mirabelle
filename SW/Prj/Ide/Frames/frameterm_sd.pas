unit FrameTerm_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Buttons, AviMath, AviTypes;

type

  { TWndTermSd }

  TWndTermSd = class(TFrame)
    BtClr: TBitBtn;
    MeList: TMemo;
    procedure BtClrClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure LvListResize(Sender: TObject);
  private
    FSheet  : TTabSheet;

    FCalibC,
    FCalibM         : TVect3s;

    FCompDataB,
    FCompDataF,
    FCompDataEma,
    FCompDataNorm   : TVect3s;

    Procedure UpdateTabName;
    Procedure ProcessStem ( Const AReadS : string );
  public
    Constructor Create ( AOwner : TComponent ); Override;
    Destructor Destroy; Override;

    Procedure Init ( ASheet : TTabSheet );
    Procedure Done;

    Procedure Clear;
    Procedure AppendAny ( Const AMessage : string );
  end;

implementation

Uses
  ConComL, Math;

{$R *.lfm}

Constructor TWndTermSd.Create ( AOwner : TComponent );
Begin
 Inherited;
End;

Destructor TWndTermSd.Destroy;
Begin
 Inherited;
End;

Procedure TWndTermSd.Init ( ASheet : TTabSheet );
Begin
 Name:='';
 FSheet:=ASheet;
 FSheet.InsertControl(Self);
 MeList.Align:=alLeft;
 Align:=alClient;
 Clear;
End;

Procedure TWndTermSd.Done;
Begin
 FSheet.RemoveControl(Self);
End;

Procedure TWndTermSd.Clear;
Begin
 MeList.Clear;
 UpdateTabName;
End;

procedure TWndTermSd.LvListResize(Sender: TObject);
begin
end;

Procedure TWndTermSd.FrameResize(Sender: TObject);
Begin
 BtClr.Left:=ClientWidth-BtClr.Width-8;
 MeList.Width:=ClientWidth-BtClr.Width-16;
End;

procedure TWndTermSd.BtClrClick(Sender: TObject);
begin
 Clear;
end;

Procedure TWndTermSd.UpdateTabName;
Begin
 FSheet.Caption:='Terminal ('+IntToStr(MeList.Lines.Count)+')';
End;

Procedure TWndTermSd.AppendAny ( Const AMessage : string );
Var
  BMessage  : string;
  BCmd      : char;
Begin
 BMessage:=AMessage;

 repeat
 if BMessage='' then break;
 BCmd:=BMessage[1]; Delete(BMessage,1,1);
 case BCmd of
  't': begin
       if Pos('~stem ',BMessage)=1 then ProcessStem(BMessage)
       else MeList.Lines.Append(BMessage);
       end;
  else MeList.Lines.Append('?: '+BMessage);
 end;
 UpdateTabName;
 until TRUE;
End;

Function ReadVect3s ( Var AReadS : string; Out AVect3s : TVect3s ) : boolean;
Var
  BDataC    : Cardinal;
  BDataF    : Single absolute BDataC;
  BParamS   : string;
Begin
 Result:=FALSE; AVect3s:=ZVect3s;
 repeat
 // FX
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3s.FX:=BDataF;
 // FY
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3s.FY:=BDataF;
 // FZ
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3s.FZ:=BDataF;
 // Ret
 Result:=TRUE;
 until TRUE;
End;

Type
  TVect3i = record
    FX,
    FY,
    FZ      : smallint;
  end;

Const
  ZVect3i : TVect3i =
    (
     FX:0;
     FY:0;
     FZ:0;
    );

Function ReadVect3i ( Var AReadS : string; Out AVect3i : TVect3i ) : boolean;
Var
  BDataC    : Cardinal;
  BParamS   : string;
Begin
 Result:=FALSE; AVect3i:=ZVect3i;
 repeat
 // FX
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3i.FX:=BDataC;
 // FY
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3i.FY:=BDataC;
 // FZ
 BParamS:=ReadParamStr(AReadS); if BParamS='' then break;
 if HexToDWordCheck(BParamS,BDataC)=FALSE then break;
 AVect3i.FZ:=BDataC;
 // Ret
 Result:=TRUE;
 until TRUE;
End;

Function CmpVect3s ( Const ADataA, ADataB : TVect3s ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if SameValue(ADataA.FX,ADataB.FX)=FALSE then break;
 if SameValue(ADataA.FY,ADataB.FY)=FALSE then break;
 if SameValue(ADataA.FZ,ADataB.FZ)=FALSE then break;
 Result:=TRUE;
 until TRUE;
End;

Procedure TWndTermSd.ProcessStem ( Const AReadS : string );
Var
  BReadS        : string;
  BParamS       : string;
  BDataRaw      : TVect3i;
  BCompDataB,
  BCompDataF,
  BCompDataEma,
  BCompDataNorm : TVect3s;
  BVectA        : TVect3s;
  BVectLen      : Single;
Begin
 BReadS:=AReadS;
 repeat
 ReadParamStr(BReadS);
 BParamS:=ReadParamStr(BReadS);
 if BParamS='c' then
  begin
  FCalibC:=ZVect3s; FCalibM:=ZVect3s;
  FCompDataB:=ZVect3s; FCompDataF:=ZVect3s; FCompDataEma:=ZVect3s;
  if ReadVect3s(BReadS,FCalibC)=FALSE then break;
  if ReadVect3s(BReadS,FCalibM)=FALSE then break;
  break;
  end;
 if BParamS='d' then
  begin
  if ReadVect3i(BReadS,BDataRaw)=FALSE then break;
  if ReadVect3s(BReadS,BCompDataB)=FALSE then break;
  if ReadVect3s(BReadS,BCompDataF)=FALSE then break;
  if ReadVect3s(BReadS,BCompDataEma)=FALSE then break;
  if ReadVect3s(BReadS,BCompDataNorm)=FALSE then break;
  FCompDataB.FX:=BDataRaw.FX;
  FCompDataB.FY:=BDataRaw.FY;
  FCompDataB.FZ:=BDataRaw.FZ;
  BVectA:=SubVect(FCompDataB,FCalibC);
  FCompDataF:=MulVect(BVectA,FCalibM);

  FCompDataEma.FX:=0.8*FCompDataEma.FX+0.2*FCompDataF.FX;
  FCompDataEma.FY:=0.8*FCompDataEma.FY+0.2*FCompDataF.FY;
  FCompDataEma.FZ:=0.8*FCompDataEma.FZ+0.2*FCompDataF.FZ;

  FCompDataNorm:=FCompDataEma;
  BVectLen:=VectLen(FCompDataNorm);
  if IsZero(BVectLen) then break;
  //BCompData.FZ:=0.5*BCompData.FZ;
  NormVect(FCompDataNorm);

  if CmpVect3s(FCompDataEma,BCompDataEma)=FALSE then
   begin
   Sleep(0);
   end;
  break;
  end;
 until TRUE;
End;

end.

