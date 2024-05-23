Unit ConComI;

Interface

Uses
  AviMath;

Function AvgColor ( AColorA, AColorB : Cardinal; APos : TAviFloat ) : Cardinal;

Implementation

Function AvgColor ( AColorA, AColorB : Cardinal; APos : TAviFloat ) : Cardinal;
Var
  BPos          : TAviFloat;
  BColorBA,
  BColorBB      : byte;
Begin
 Result:=0;
 BPos:=1-APos;
 BColorBA:=AColorA shr 16; BColorBB:=AColorB shr 16;
 Result:=(Result shl 8) or (Round((BColorBA*APos)+(BColorBB*BPos)) and $FF);
 BColorBA:=AColorA shr 8; BColorBB:=AColorB shr 8;
 Result:=(Result shl 8) or (Round((BColorBA*APos)+(BColorBB*BPos)) and $FF);
 BColorBA:=AColorA; BColorBB:=AColorB;
 Result:=(Result shl 8) or (Round((BColorBA*APos)+(BColorBB*BPos)) and $FF);
End;

end.

