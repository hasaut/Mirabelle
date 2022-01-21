unit AviMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AviTypes, Math;

Const
  CSensZ : TSensData =
   (
    FIdx:0;
    FSensA:(FX:0.0; FY:0.0; FZ:9.8);
    FSensG:(FX:0.0; FY:0.0; FZ:0.0);
    FSensM:(FX:1.0; FY:0.0; FZ:0.0);
    FSensT:23.0;
    FCntA:40; FCntG:40; FCntM:8; FCntT:8;
    FBaroA:(FDataT:15.0; FDataP:926.0; FDataH:0.0; FCntT:3; FCntP:3);
    FBaroB:(FDataT:15.0; FDataP:926.0; FDataH:0.0; FCntT:3; FCntP:3);
    FGyroGrav:(FX:0.0; FY:0.0; FZ:1.0);
    FSpdHEma:0.0;
    FAltB2G:0.0;
    FRH:85.0
   );

  // Math
  // b^x = exp(x*ln(b))
  // Pressure from Altitude (in hPa), simple
  // p = 1013.25 (1 - 2.25577E-5 * h) ^ 5.25588
  // Pressure to Altitude (in hPa), simple
  // h = 44307.69396*(1 - (p/1013.25)^0.190284)

Const
  CAccG = 9.80665;
  CAtmA : array [0..4] of TAviFloat = (-0.0065, 0, 0.001, 0.0028, 0);
  CAltStd : array [0..4] of TAviFloat = (11000, 20000, 32000, 47000, Infinity);
  CAirR = 287.00;
  CPix2 = pi*2;
  CAxisX : TVect3s = (FX:1.0; FY:0.0; FZ:0.0);
  CAxisY : TVect3s = (FX:0.0; FY:1.0; FZ:0.0);
  CAxisZ : TVect3s = (FX:0.0; FY:0.0; FZ:1.0);
  CGravAcc : TVect3s = (FX:0.0; FY:0.0; FZ:-9.8065);
  CGravDbg : TVect3s = (FX:0.0; FY:0.0; FZ:-0.01);

Function BPowerX ( AB, AX : TAviFloat ) : TAviFloat;
Function InsideBounds ( AValue, ABoundA, ABoundB : TAviFloat ) : boolean;
Function InsideBounds ( Const AValue, ABoundA, ABoundB : TVect2s ) : boolean;
Procedure CorrectAngle2pi ( Var AAngle : TAviFloat );
Procedure CorrectAngleMpipi ( Var AAngle : TAviFloat );
Function SubAngles ( AAngleA, AAngleB : TAviFloat ) : TAviFloat;

Function VectLen ( Const AVect : TVect3s ) : TAviFloat;
Procedure NormVect ( Const AVect : TVect3s; Out ADst : TVect3d );
Procedure NormVect ( Var AVect : TVect3s );
Procedure NormVect ( Var AVect : TVect3s; Out ALen : TAviFloat );
Procedure NormVect ( Var AVect : TVect2d );
Function NormVect ( Var AVect : TVect2s ) : boolean;
Function AddVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function SubVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function SubVect ( Const AVectA, AVectB : TVect2s ) : TVect2s;
Procedure ScaleVect ( Var AVect : TVect2s; ALen : TAviFloat );
Procedure ScaleVect ( Var AVect : TVect3s; ALen : TAviFloat );
Function AvgVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function MulVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function ScaleVectA ( Var AVect : TVect3s; ALen : TAviFloat ) : TVect3s;

Function IsZeroA ( AData : TAviFloat ) : boolean;
Function IsVectZero ( Const AVect : TVect3s ) : boolean;

Function SqrS ( AData : TAviFloat ) : TAviFloat;
Function SqrS ( Const AData : TVect3s ) : TVect3s;
Function VSinCos ( AAngle : TAviFloat ) : TVect2s;
Procedure GetSin ( Var ASinCos : TVect2s; ADir : Integer );

Function SamePoint ( Const APointA, APointB : TVect3s ) : boolean;
Function SamePoint ( Const APointA, APointB : TVect3s; ADelta : TAviFloat ) : boolean;
Function SamePoint ( Const APointA, APointB : TVect2s ) : boolean;

Procedure NormAngle ( Var AAngle : TAviFloat );
Procedure NormAngle ( Var AAngle : TVect3s );

Procedure PointRotate ( Var APoint : TVect2s; Const ASinCos : TVect2s );
Procedure PointRotateN ( Var APoint : TVect2s; Const ASinCos : TVect2s );
Procedure PointRotateX ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Procedure PointRotateY ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Procedure PointRotateZ ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Procedure PointRotate ( Var APoint : TVect3s; Const AAxis : TVect3s; Const ASinCos : TVect2s );
Procedure BasisRotate ( Var ABasis : TBasisXYZ; Const AAxis : TVect3s; Const AAngle : TAviFloat );

Function DotProduct ( Const AVectA, AVectB : TVect3s ) : TAviFloat;
Function DotProduct ( Const AVectA, AVectB : TVect2s ) : TAviFloat;
Function CrossProduct ( Const AVectA, AVectB : TVect3s ) : TVect3s;

Function GetDistancePPSquare ( Const APointA, APointB : TVect3s ) : TAviFloat;
Function GetDistancePP ( Const APointA, APointB : TVect3s ) : TAviFloat;
Function GetDistancePP ( Const APointA, APointB : TVect2s ) : TAviFloat;
Function GetInterPR ( Const APlane : TPlane3s; Const ARay : TRay3s; Out AInter : TVect3s; Out AT : TAviFloat ) : boolean;

Procedure PtrToBasis ( Var APoint : TVect3s; Const ABasis : TBasisXYZ );
Procedure PtrFromBasis ( Var APoint : TVect3s; Const ABasis : TBasisXYZ );

//Function HdngToComp ( ALat : TAviFloat; Const AHdng : THdng ) : TVect3s;
Function AltToBaro ( AAlt : TAviFloat ) : TAviFloat;
Function BaroToAlt ( ABaro : TAviFloat ) : TAviFloat;
Function GetAirDens ( Const ABaro : TBaroData; ARH : TAviFloat ) : TAviFloat;

Procedure AltStdToTpd ( AAlt : TAviFloat; Out AT, AP, AD : TAviFloat );
Procedure RefineBasis ( Var ABasis : TBasisXYZ );
Procedure GetRotAxis ( Const ABasis : TBasisXYZ; Const ASpdRot : TVect3s; Out ARotAxis : TVect3s; Out AAngle : TAviFloat );

Function  HdngToBasis ( Const AHdng : THdng ) : TBasisXYZ;
//Function GetWingForcesA ( Const AWing : TAviWing; Const ABodyWind : TVect2s; AOwnAoa : TAviFloat; AAirDens : TAviFloat; Var AStall : boolean ) : TVect2s;
// ASpdRot is given like for the Right wing
//Function GetWingForcesB ( Const AWing : TAviWing; Const ABodyWind : TVect2s; Const ASpdRot : TVect3s; AOwnAoa : TAviFloat; AAirDens : TAviFloat; Var AStall : boolean ) : TVect2s;

implementation

// b^x = exp(x*ln(b))
Function BPowerX ( AB, AX : TAviFloat ) : TAviFloat;
Begin
 Result:=exp(AX*ln(AB));
End;

Function InsideBounds ( AValue, ABoundA, ABoundB : TAviFloat ) : boolean;
Var
  BBoundA,
  BBoundB       : TAviFloat;
Begin
 if ABoundA<ABoundB then begin BBoundA:=ABoundA; BBoundB:=ABoundB; end
 else begin BBoundA:=ABoundB; BBoundB:=ABoundA; end;
 Result:=((BBoundA<=AValue) or SameValue(BBoundA,AValue,CAviZeroResolution))
     and ((AValue<BBoundB) or SameValue(AValue,BBoundB,CAviZeroResolution));
End;

Function InsideBounds ( Const AValue, ABoundA, ABoundB : TVect2s ) : boolean;
Begin
 Result:=InsideBounds(AValue.FX,ABoundA.FX,ABoundB.FX) and InsideBounds(AValue.FY,ABoundA.FY,ABoundB.FY);
End;

Procedure CorrectAngle2pi ( Var AAngle : TAviFloat );
Begin
 while AAngle<0 do AAngle:=AAngle+2*pi;
 while AAngle>(2*pi) do AAngle:=AAngle-2*pi;
End;

Procedure CorrectAngleMpipi ( Var AAngle : TAviFloat );
Begin
 while AAngle<-pi do AAngle:=AAngle+pi;
 while AAngle>pi do AAngle:=AAngle-pi;
End;

Function SubAngles ( AAngleA, AAngleB : TAviFloat ) : TAviFloat;
Begin
 Result:=AAngleA-AAngleB;
 if Result>pi then Result:=Result-2*pi
 else if Result<-pi then Result:=Result+2*pi;
 CorrectAngleMpipi(Result);
End;

Function VectLen ( Const AVect : TVect3s ) : TAviFloat;
Begin
 Result:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ));
End;

Procedure NormVect ( Const AVect : TVect3s; Out ADst : TVect3d );
Begin
 ADst.FD:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ));
 if IsZero(ADst.FD,CAviZeroResolution) then
  begin
  ADst.FX:=0;
  ADst.FY:=0;
  ADst.FZ:=0;
  end
 else
  begin
  ADst.FX:=AVect.FX/ADst.FD;
  ADst.FY:=AVect.FY/ADst.FD;
  ADst.FZ:=AVect.FZ/ADst.FD;
  end;
End;

Procedure NormVect ( Var AVect : TVect3s );
Var
  BD    : TAviFloat;
Begin
 BD:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ));
 if IsZero(BD,CAviZeroResolution) then
  begin
  AVect.FX:=0;
  AVect.FY:=0;
  AVect.FZ:=0;
  end
 else
  begin
  AVect.FX:=AVect.FX/BD;
  AVect.FY:=AVect.FY/BD;
  AVect.FZ:=AVect.FZ/BD;
  end;
End;

Procedure NormVect ( Var AVect : TVect3s; Out ALen : TAviFloat );
Var
  BD    : TAviFloat;
Begin
 BD:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ));
 if IsZero(BD,CAviZeroResolution) then
  begin
  AVect.FX:=0;
  AVect.FY:=0;
  AVect.FZ:=0;
  ALen:=0;
  end
 else
  begin
  AVect.FX:=AVect.FX/BD;
  AVect.FY:=AVect.FY/BD;
  AVect.FZ:=AVect.FZ/BD;
  ALen:=BD;
  end;
End;

Function AddVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=AVectA.FX+AVectB.FX;
 Result.FY:=AVectA.FY+AVectB.FY;
 Result.FZ:=AVectA.FZ+AVectB.FZ;
End;

Function SubVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=AVectA.FX-AVectB.FX;
 Result.FY:=AVectA.FY-AVectB.FY;
 Result.FZ:=AVectA.FZ-AVectB.FZ;
End;

Function SubVect ( Const AVectA, AVectB : TVect2s ) : TVect2s;
Begin
 Result.FX:=AVectA.FX-AVectB.FX;
 Result.FY:=AVectA.FY-AVectB.FY;
End;

Procedure ScaleVect ( Var AVect : TVect2s; ALen : TAviFloat );
Begin
 AVect.FX:=AVect.FX*ALen;
 AVect.FY:=AVect.FY*ALen;
End;

Procedure ScaleVect ( Var AVect : TVect3s; ALen : TAviFloat );
Begin
 AVect.FX:=AVect.FX*ALen;
 AVect.FY:=AVect.FY*ALen;
 AVect.FZ:=AVect.FZ*ALen;
End;

Function ScaleVectA ( Var AVect : TVect3s; ALen : TAviFloat ) : TVect3s;
Begin
 Result:=AVect;
 ScaleVect(Result,ALen);
End;

Function AvgVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=0.5*(AVectA.FX+AVectB.FX);
 Result.FY:=0.5*(AVectA.FY+AVectB.FY);
 Result.FZ:=0.5*(AVectA.FZ+AVectB.FZ);
End;

Function MulVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=AVectA.FX*AVectB.FX;
 Result.FY:=AVectA.FY*AVectB.FY;
 Result.FZ:=AVectA.FZ*AVectB.FZ;
End;

Function IsZeroA ( AData : TAviFloat ) : boolean;
Begin
 Result:=Abs(AData)<0.0000001;
End;

Function IsVectZero ( Const AVect : TVect3s ) : boolean;
Begin
 Result:=IsZero(AVect.FX,CAviZeroResolution) and IsZero(AVect.FY,CAviZeroResolution) and IsZero(AVect.FZ,CAviZeroResolution);
End;

Function SqrS ( AData : TAviFloat ) : TAviFloat;
Begin
 Result:=AData*Abs(AData);
End;

Function SqrS ( Const AData : TVect3s ) : TVect3s;
Begin
 Result.FX:=SqrS(AData.FX);
 Result.FY:=SqrS(AData.FY);
 Result.FZ:=SqrS(AData.FZ);
End;

Procedure NormVect ( Var AVect : TVect2d );
Begin
 AVect.FD:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY));
 if IsZero(AVect.FD,CAviZeroResolution) then begin AVect.FX:=0; AVect.FY:=0; end
 else begin AVect.FX:=AVect.FX/AVect.FD; AVect.FY:=AVect.FY/AVect.FD; end;
End;

Function NormVect ( Var AVect : TVect2s ) : boolean;
Var
  BD            : TAviFloat;
Begin
 Result:=FALSE;
 repeat
 BD:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY));
 if IsZero(BD,CAviZeroResolution) then break;
 AVect.FX:=AVect.FX/BD;
 AVect.FY:=AVect.FY/BD;
 Result:=TRUE;
 until TRUE;
End;

Function VSinCos ( AAngle : TAviFloat ) : TVect2s;
Var
  BSin,
  BCos          : Extended;
Begin
 sincos(AAngle,BSin,BCos);
 Result.FY:=BSin;
 Result.FX:=BCos;
End;

Procedure GetSin ( Var ASinCos : TVect2s; ADir : Integer );
Begin
 ASinCos.FY:=-Sqrt(1-Sqr(ASinCos.FX))*ADir;
End;

Function SamePoint ( Const APointA, APointB : TVect3s ) : boolean;
Begin
 Result:=SameValue(APointA.FX,APointB.FX,CAviZeroResolution) and
         SameValue(APointA.FY,APointB.FY,CAviZeroResolution) and
         SameValue(APointA.FZ,APointB.FZ,CAviZeroResolution);
End;

Function SamePoint ( Const APointA, APointB : TVect3s; ADelta : TAviFloat ) : boolean;
Begin
 Result:=SameValue(APointA.FX,APointB.FX,ADelta)
     and SameValue(APointA.FY,APointB.FY,ADelta)
     and SameValue(APointA.FZ,APointB.FZ,ADelta);
End;

Function SamePoint ( Const APointA, APointB : TVect2s ) : boolean;
Begin
 Result:=SameValue(APointA.FX,APointB.FX,CAviZeroResolution) and SameValue(APointA.FY,APointB.FY,CAviZeroResolution);
End;

Procedure NormAngle ( Var AAngle : TAviFloat );
Begin
 while AAngle>pi do AAngle:=AAngle-CPix2;
 while AAngle<(-pi) do AAngle:=AAngle+CPix2;
End;

Procedure NormAngle ( Var AAngle : TVect3s );
Begin
 NormAngle(AAngle.FX);
 NormAngle(AAngle.FY);
 NormAngle(AAngle.FZ);
End;

Function DotProduct ( Const AVectA, AVectB : TVect3s ) : TAviFloat;
Begin
 Result:=AVectA.FX*AVectB.FX+AVectA.FY*AVectB.FY+AVectA.FZ*AVectB.FZ;
End;

Function DotProduct ( Const AVectA, AVectB : TVect2s ) : TAviFloat;
Begin
 Result:=AVectA.FX*AVectB.FX+AVectA.FY*AVectB.FY;
End;

Function CrossProduct ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=AVectA.FY*AVectB.FZ-AVectA.FZ*AVectB.FY;
 Result.FY:=AVectA.FZ*AVectB.FX-AVectA.FX*AVectB.FZ;
 Result.FZ:=AVectA.FX*AVectB.FY-AVectA.FY*AVectB.FX;
End;

Function GetDistancePPSquare ( Const APointA, APointB : TVect3s ) : TAviFloat;
Begin
 Result:=Sqr(APointA.FX-APointB.FX)+Sqr(APointA.FY-APointB.FY)+Sqr(APointA.FZ-APointB.FZ);
End;

Function GetDistancePP ( Const APointA, APointB : TVect3s ) : TAviFloat;
Begin
 Result:=Sqrt(Sqr(APointA.FX-APointB.FX)+Sqr(APointA.FY-APointB.FY)+Sqr(APointA.FZ-APointB.FZ));
End;

Function GetDistancePP ( Const APointA, APointB : TVect2s ) : TAviFloat;
Begin
 Result:=Sqr(APointA.FX-APointB.FX)+Sqr(APointA.FY-APointB.FY);
End;

Function GetInterPR ( Const APlane : TPlane3s; Const ARay : TRay3s; Out AInter : TVect3s; Out AT : TAviFloat ) : boolean;
Var
  BDen, BT      : Double;

Begin
 Result:=FALSE;

 repeat
 BDen:=DotProduct(APlane.FDir,ARay.FDir);
 if IsZero(BDen,CAviZeroResolution) then break;
 BT:=(-DotProduct(APlane.FDir,ARay.FStart)+APlane.FD)/BDen;
 AInter.FX:=ARay.FDir.FX*BT+ARay.FStart.FX;
 AInter.FY:=ARay.FDir.FY*BT+ARay.FStart.FY;
 AInter.FZ:=ARay.FDir.FZ*BT+ARay.FStart.FZ;
 AT:=BT;
 //AT:=GetDistancePP(AInter,ARay.FStart);
 Result:=TRUE;
 until TRUE;
End;

Procedure PointRotate ( Var APoint : TVect2s; Const ASinCos : TVect2s );
Var
  BPoint        : TVect2s;
Begin
 BPoint.FX:= APoint.FX*ASinCos.FX+APoint.FY*ASinCos.FY;
 BPoint.FY:=-APoint.FX*ASinCos.FY+APoint.FY*ASinCos.FX;
 APoint:=BPoint;
End;

Procedure PointRotateN ( Var APoint : TVect2s; Const ASinCos : TVect2s );
Var
  BPoint        : TVect2s;
Begin
 BPoint.FX:= APoint.FX*ASinCos.FX-APoint.FY*ASinCos.FY;
 BPoint.FY:= APoint.FX*ASinCos.FY+APoint.FY*ASinCos.FX;
 APoint:=BPoint;
End;

Procedure PointRotateX ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Var
  BNewY,
  BNewZ         : TAviFloat;
Begin
 BNewY:=APoint.FY*ASinCos.FX-APoint.FZ*ASinCos.FY;
 BNewZ:=APoint.FY*ASinCos.FY+APoint.FZ*ASinCos.FX;
 APoint.FY:=BNewY;
 APoint.FZ:=BNewZ;
End;

Procedure PointRotateY ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Var
  BNewX,
  BNewZ         : TAviFloat;
Begin
 BNewX:=APoint.FX*ASinCos.FX+APoint.FZ*ASinCos.FY;
 BNewZ:=-APoint.FX*ASinCos.FY+APoint.FZ*ASinCos.FX;
 APoint.FX:=BNewX;
 APoint.FZ:=BNewZ;
End;

Procedure PointRotateZ ( Var APoint : TVect3s; Const ASinCos : TVect2s );
Var
  BNewX,
  BNewY         : TAviFloat;
Begin
 BNewX:=APoint.FX*ASinCos.FX-APoint.FY*ASinCos.FY;
 BNewY:=APoint.FX*ASinCos.FY+APoint.FY*ASinCos.FX;
 APoint.FX:=BNewX;
 APoint.FY:=BNewY;
End;

Procedure PointRotate ( Var APoint : TVect3s; Const AAxis : TVect3s; Const ASinCos : TVect2s );
Var
  BAxis         : TVect3s;
  BPoint        : TVect3s;
  BDF           : TAviFloat;
Begin
 BAxis:=AAxis;
 NormVect(BAxis);
 BPoint:=APoint;
 BDF:=DotProduct(BPoint,BAxis);
 APoint.FX:=BAxis.FX*BDF*(1-ASinCos.FX)+BPoint.FX*ASinCos.FX+(BAxis.FZ*BPoint.FY-BAxis.FY*BPoint.FZ)*ASinCos.FY;
 APoint.FY:=BAxis.FY*BDF*(1-ASinCos.FX)+BPoint.FY*ASinCos.FX+(BAxis.FX*BPoint.FZ-BAxis.FZ*BPoint.FX)*ASinCos.FY;
 APoint.FZ:=BAxis.FZ*BDF*(1-ASinCos.FX)+BPoint.FZ*ASinCos.FX+(BAxis.FY*BPoint.FX-BAxis.FX*BPoint.FY)*ASinCos.FY;
End;

Procedure BasisRotate ( Var ABasis : TBasisXYZ; Const AAxis : TVect3s; Const AAngle : TAviFloat );
Var
  BSinCos       : TVect2s;
Begin
 BSinCos:=VSinCos(AAngle);
 PointRotate(ABasis.FAxisX,AAxis,BSinCos);
 PointRotate(ABasis.FAxisY,AAxis,BSinCos);
 PointRotate(ABasis.FAxisZ,AAxis,BSinCos);
 RefineBasis(ABasis);
End;

Procedure PtrFromBasis ( Var APoint : TVect3s; Const ABasis : TBasisXYZ );
Var
  BResult       : TVect3s;
Begin
 BResult.FX:=APoint.FX*ABasis.FAxisX.FX+APoint.FY*ABasis.FAxisY.FX+APoint.FZ*ABasis.FAxisZ.FX;
 BResult.FY:=APoint.FX*ABasis.FAxisX.FY+APoint.FY*ABasis.FAxisY.FY+APoint.FZ*ABasis.FAxisZ.FY;
 BResult.FZ:=APoint.FX*ABasis.FAxisX.FZ+APoint.FY*ABasis.FAxisY.FZ+APoint.FZ*ABasis.FAxisZ.FZ;
 APoint:=BResult;
End;

Procedure PtrToBasis ( Var APoint : TVect3s; Const ABasis : TBasisXYZ );
Var
  BResult       : TVect3s;
Begin
 BResult.FX:=APoint.FX*ABasis.FAxisX.FX+APoint.FY*ABasis.FAxisX.FY+APoint.FZ*ABasis.FAxisX.FZ;
 BResult.FY:=APoint.FX*ABasis.FAxisY.FX+APoint.FY*ABasis.FAxisY.FY+APoint.FZ*ABasis.FAxisY.FZ;
 BResult.FZ:=APoint.FX*ABasis.FAxisZ.FX+APoint.FY*ABasis.FAxisZ.FY+APoint.FZ*ABasis.FAxisZ.FZ;
 APoint:=BResult;
End;



{Function HdngToComp ( ALat : TAviFloat; Const AHdng : THdng ) : TVect3s;
Var
  BGrav         : TVect3s;
  BRotVect      : TVect3s;
  BRotAngl      : TVect2s;
Begin
 // Adjust to latitude
 Result:=CAxisX;
 PointRotateY(Result,VSinCos(-DegToRad(ALat)));
 PointRotateZ(Result,VSinCos(DegToRad(AHdng.FDir)));
 // Adjust to Grav
 repeat
 BGrav:=AHdng.FGrav;
 NormVect(BGrav);
 if SamePoint(BGrav,CAxisZ) then break;
 BRotVect:=CrossProduct(BGrav,CAxisZ);
 NormVect(BRotVect);
 BRotAngl.FX:=DotProduct(BGrav,CAxisZ);
 GetSin(BRotAngl);
 PointRotate(Result,BRotVect,BRotAngl);
 until TRUE;
End;}

// p = 1013.25 (1 - 2.25577E-5 * h) ^ 5.25588
Function AltToBaro ( AAlt : TAviFloat ) : TAviFloat;
Begin
 Result:=1013.25*exp(5.25588*ln(1-AAlt*2.25577E-5));
End;

//BDummyF:=(1-Exp(0.190284*Ln(BP/1013.25)))*44307.69396
// h = 44307.69396*(1 - (p/1013.25)^0.190284)
Function BaroToAlt ( ABaro : TAviFloat ) : TAviFloat;
Begin
 Result:=44307.69396*(1-exp(0.190284*ln(ABaro/1013.25)));
End;

Function GetAirDens ( Const ABaro : TBaroData; ARH : TAviFloat ) : TAviFloat;
Var
  BTk           : TAviFloat;
  BPSat,
  BPv, BPd      : TAviFloat;
Begin
 BTk:=ABaro.FDataT+273.15;
 BPSat:=6.1078*BPowerX(10,7.5*ABaro.FDataT/(ABaro.FDataT+237.3));
 BPv:=ARH*BPSat; BPd:=ABaro.FDataP*100-BPv;
 Result:=(BPd*0.028964+BPv*0.018016)/(8.314*BTk);
End;

Procedure AltStdToTpd ( AAlt : TAviFloat; Out AT, AP, AD : TAviFloat );
Var
  BAlt          : array [0..1] of TAviFloat;
  BT            : array [0..1] of TAviFloat;
  BP            : array [0..1] of TAviFloat;
  BLayIdx       : Integer;
Begin
 BT[0]:=273.15+15; BP[0]:=101325; BAlt[0]:=0.0;

 BLayIdx:=0;
 while BLayIdx<5 do
  begin
  BAlt[1]:=AAlt;
  if (IsInfinite(CAltStd[BLayIdx])=FALSE) and (BAlt[1]>CAltStd[BLayIdx]) then BAlt[1]:=CAltStd[BLayIdx];
  if BLayIdx in [1,4] then
   begin
   BT[1]:=BT[0];
   BP[1]:=BP[0]*exp(-CAccG/(CAirR*BT[1])*(BAlt[1]-BAlt[0]));
   end
  else
   begin
   BT[1]:=BT[0]+CAtmA[BLayIdx]*(BAlt[1]-BAlt[0]);
   BP[1]:=BP[0]*BPowerX(BT[1]/BT[0],-CAccG/(CAtmA[BLayIdx]*CAirR));
   end;
  if SameValue(AAlt,BAlt[1],CAviZeroResolution) then break;
  BAlt[0]:=BAlt[1]; BT[0]:=BT[1]; BP[0]:=BP[1];
  inc(BLayIdx);
  end;

 AT:=BT[1];
 AP:=BP[1];
 AD:=BP[1]/(BT[1]*CAirR);
End;

Procedure RefineBasis ( Var ABasis : TBasisXYZ );
Var
  BAxisX        : TVect3s;
Begin
 ABasis.FAxisY:=CrossProduct(ABasis.FAxisZ,ABasis.FAxisX); NormVect(ABasis.FAxisY);
 BAxisX:=CrossProduct(ABasis.FAxisY,ABasis.FAxisZ); NormVect(BAxisX);
 if SamePoint(ABasis.FAxisX,BAxisX)=FALSE then
  begin
  ABasis.FAxisX:=BAxisX;
  NormVect(ABasis.FAxisZ);
  end;
End;

Procedure GetRotAxis ( Const ABasis : TBasisXYZ; Const ASpdRot : TVect3s; Out ARotAxis : TVect3s; Out AAngle : TAviFloat );
Var
  BAxis         : TRotAxis;
  BDummyLen     : TAviFloat;
Begin
 if IsVectZero(ASpdRot) then
  begin
  BAxis:=raNone;
  if Abs(ASpdRot.FX)>Abs(ASpdRot.FY) then
   begin
   if Abs(ASpdRot.FZ)>Abs(ASpdRot.FX) then BAxis:=raZ
   else BAxis:=raX;
   end
  else
   begin
   if Abs(ASpdRot.FZ)>Abs(ASpdRot.FY) then BAxis:=raZ
   else BAxis:=raY;
   end;
  ARotAxis:=CAxisX; AAngle:=0;
  case BAxis of
    raX: begin AAngle:=ASpdRot.FX; ARotAxis:=ABasis.FAxisX; end;
    raY: begin AAngle:=ASpdRot.FY; ARotAxis:=ABasis.FAxisY; end;
    raZ: begin AAngle:=ASpdRot.FZ; ARotAxis:=ABasis.FAxisZ; end;
  end;
  PtrFromBasis(ARotAxis,ABasis);
  end
 else
  begin
  ARotAxis:=ASpdRot;
  NormVect(ARotAxis,BDummyLen);
  PtrFromBasis(ARotAxis,ABasis);
  AAngle:=-BDummyLen;
  end;
End;

Function  HdngToBasis ( Const AHdng : THdng ) : TBasisXYZ;
Var
  BHdng         : THdng;
  BRotVect      : TVect3s;
  BRotAngl      : TVect2s;
Begin
 BHdng:=AHdng;
 NormVect(BHdng.FGrav);

 Result.FAxisX:=CAxisX; Result.FAxisZ:=CAxisZ;
 if SamePoint(BHdng.FGrav,CAxisZ) then
  begin
  PointRotateZ(Result.FAxisX,VSinCos(-DegToRad(AHdng.FDir-90)));
  end
 else
  begin
  BRotVect:=CrossProduct(CAxisZ,BHdng.FGrav); NormVect(BRotVect);
  BRotAngl.FX:=DotProduct(BHdng.FGrav,CAxisZ); GetSin(BRotAngl,1);
  PointRotate(Result.FAxisX,BRotVect,BRotAngl);
  PointRotate(Result.FAxisZ,BRotVect,BRotAngl);
  BRotAngl:=VSinCos(-DegToRad(AHdng.FDir));
  PointRotateZ(Result.FAxisX,BRotAngl);
  PointRotateZ(Result.FAxisZ,BRotAngl);
  end;
 RefineBasis(Result);
End;



end.

