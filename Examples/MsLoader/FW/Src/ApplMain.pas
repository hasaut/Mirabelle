Unit ApplMain;

Interface

Uses
  SysLib;

Procedure ApplMainProc;

Implementation

Var
  FExecEnd      : boolean;

Procedure ProcApplTimerWait; External;
Function ProcApplFlashCheck ( AAddr, ASize : Cardinal ) : boolean; External;
Procedure ProcApplReconfig ( AAddr : Cardinal ); External;

Function StartupGet : byte; External;
Procedure StartupSet ( AFlags : byte ); External;

Procedure ApplMainProc;
Var
  BLed  : byte;
Begin
 FExecEnd:=FALSE;
 LedYSet($00);
 LedRgbSet($04);

 repeat
 if (StartupGet and $01)=0 then break; // Stay in loader mode (inverted because pin is read as '1')
 if ProcApplFlashCheck($800000,$700000)=FALSE then begin LedRgbSet($01); break; end;
 StartupSet($10);
 LedRgbSet($07);
 ProcApplTimerWait();
 ProcApplTimerWait();
 ProcApplReconfig($800000);
 LedRgbSet($00);
 SysStop();
 until FExecEnd;

 BLed:=0;
 repeat
 LedYSet(BLed);
 inc(BLed);
 ProcApplTimerWait();
 until FExecEnd;

 SysStop;
End;

end.

