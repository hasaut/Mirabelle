Unit ApplMain;

Interface

Uses
  SysLib;

Procedure ApplMainProc;

Implementation

Var
  FExecEnd      : boolean;

Procedure ProbaA; External;

Function ProbaB ( AData : Cardinal ) : Cardinal;
Begin
 Result:=AData;
End;

Function ProbaC ( AData : Cardinal ) : Cardinal;
Begin
 Result:=AData;
End;

Procedure ApplMainProc;
Var
  BLedY         : byte;
  BData         : Cardinal;
Begin
 LedRgbSet($02);

 BData:=ProbaB(0);

 BLedY:=0;
 FExecEnd:=FALSE;
 repeat
 LedYSet(BLedY);
 //Writeln('Proba');
 //SysStop();
 inc(BLedY);
 WDReset();
 //if ProbaC(BData)=0 then break;
 until FExecEnd;

 SysStop;
End;

end.

