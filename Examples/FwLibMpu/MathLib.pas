Unit MathLib;

Interface

Const
  pi = 3.1415926535897932384626433;

Procedure MathTest;
Procedure SinCos ( AData : real; Var ASin, ACos : real );

Implementation

Procedure SinCos ( AData : real; Var ASin, ACos : real ); External;

Procedure MathTest;
Var
  BX    : real;
  BSin,
  BCos  : real;
Begin
 repeat
 BX:=pi;
 BSin:=0; BCos:=0;
 SinCos(BX,BSin,BCos);
 if BSin=0 then break;
 until TRUE;
End;

end.


