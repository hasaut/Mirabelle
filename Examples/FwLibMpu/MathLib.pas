Unit MathLib;

Interface

Const
  pi = 3.1415926535897932384626433;
  CHalfPi = 0.5*pi;

Procedure MathTest;
Procedure MsSinCos ( AData : real; Var ASin, ACos : real );
Function MsAbs ( AData : real ) : real;
Function MsRound ( AData : Real ) : Integer;
Function MsSqr ( AData : real ) : real;
Function MsSqrt ( AData : real ) : real;
Function MsAtan2 ( AY, AX : real ) : real;

Implementation

Procedure MsSinCos ( AData : real; Var ASin, ACos : real ); External;
Function MsAbs ( AData : real ) : real; External;
Function MsRound ( AData : Real ) : Integer; External;
Function MsAtan2 ( AY, AX : real ) : real; External;

Procedure MathTest;
Var
  BX    : real;
  BSin,
  BCos  : real;
Begin
 repeat
 BX:=pi;
 BSin:=0; BCos:=0;
 MsSinCos(BX,BSin,BCos);
 if BSin=0 then break;
 until TRUE;
End;

Function MsSqr ( AData : real ) : real;
Begin
 Result:=AData*AData;
End;

Function MsSqrt ( AData : real ) : real; External;

end.


