Unit AviMath;

Interface

Uses
  SysLib;

Type
  TAviFloat = real;

  TVect3s = record
    FX,
    FY,
    FZ          : TAviFloat;
  end;

Var
  ZVect3s       : TVect3s; External;
  EVect3s       : TVect3s; External;
  CAxisX        : TVect3s; External;
  CAxisY        : TVect3s; External;
  CAxisZ        : TVect3s; External;

Function Sqr ( AData : TAviFloat ) : TAviFloat;
Function Sqrt ( AData : TAviFloat ) : TAviFloat;
Function Abs ( AData : TAviFloat ) : TAviFloat;
Function Round ( AData : TAviFloat ) : Integer;
Function SameValue ( ADataA, ADataB : TAviFloat ) : boolean;
Function IsZero ( AData : TAviFloat ) : boolean;

Function AddVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function SubVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Procedure ScaleVect ( Var AVect : TVect3s; ALen : TAviFloat );
Function ScaleVectA ( Var AVect : TVect3s; ALen : TAviFloat ) : TVect3s;
Function MulVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Function VectLen ( Const AVect : TVect3s ) : TAviFloat;
Function VectLenSquare ( Const AVect : TVect3s ) : TAviFloat;
Procedure NormVect ( Var AVect : TVect3s );
Function DotProduct ( Const AVectA, AVectB : TVect3s ) : TAviFloat;

Function GetDistancePPSquare ( Const APointA, APointB : TVect3s ) : TAviFloat;

Implementation

Function Sqr ( AData : TAviFloat ) : TAviFloat;
Begin
 Result:=AData*AData;
End;

Function Sqrt ( AData : TAviFloat ) : TAviFloat; External;

Function Abs ( AData : TAviFloat ) : TAviFloat; External;
Function Round ( AData : TAviFloat ) : Integer; External;


Function SameValue ( ADataA, ADataB : TAviFloat ) : boolean;
Begin
 Result:=Abs(ADataA-ADataB)<0.00001;
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

Function MulVect ( Const AVectA, AVectB : TVect3s ) : TVect3s;
Begin
 Result.FX:=AVectA.FX*AVectB.FX;
 Result.FY:=AVectA.FY*AVectB.FY;
 Result.FZ:=AVectA.FZ*AVectB.FZ;
End;

Function VectLen ( Const AVect : TVect3s ) : TAviFloat;
Begin
 Result:=Sqrt(Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ));
End;

Function VectLenSquare ( Const AVect : TVect3s ) : TAviFloat;
Begin
 Result:=Sqr(AVect.FX)+Sqr(AVect.FY)+Sqr(AVect.FZ);
End;

Procedure NormVect ( Var AVect : TVect3s );
Var
  BD    : TAviFloat;
Begin
 BD:=VectLen(AVect);
 if IsZero(BD) then
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

Function GetDistancePPSquare ( Const APointA, APointB : TVect3s ) : TAviFloat;
Begin
 Result:=Sqr(APointA.FX-APointB.FX)+Sqr(APointA.FY-APointB.FY)+Sqr(APointA.FZ-APointB.FZ);
End;

Function IsZero ( AData : TAviFloat ) : boolean;
Begin
 Result:=Abs(AData)<0.000001;
End;

Function DotProduct ( Const AVectA, AVectB : TVect3s ) : TAviFloat;
Begin
 Result:=AVectA.FX*AVectB.FX+AVectA.FY*AVectB.FY+AVectA.FZ*AVectB.FZ;
End;

end.

