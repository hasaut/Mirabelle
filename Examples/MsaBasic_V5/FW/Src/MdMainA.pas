Unit MdMainA;

Interface

Uses
  SysLib;

Procedure MainA;

Implementation

Var
  FActive       : boolean;

Procedure MainA;
Begin
 FActive:=TRUE;
 repeat
 WDReset;
 until FActive=FALSE;
End;

Procedure MainA_old;
Var
  BLedY         : byte;
Begin
 LedRgbSet($02);

 BLedY:=0;
 FActive:=TRUE;
 repeat
 LedYSet(BLedY);
 //Writeln('Proba');
 //SysStop();
 inc(BLedY);
 WDReset();
 //if ProbaC(BData)=0 then break;
 until FActive=FALSE;

 SysStop;
End;

end.

