Unit SysLib;

Interface

Procedure WDReset;
Procedure LedYSet ( AData : byte );
Procedure LedRgbSet ( AData : byte );
Procedure PinDbgSet ( AData : byte );

Procedure Writeln ( Const ADataS : string );
Function IntToStr ( AData : Integer ) : string;
Function FloatToStr ( AData : real ) : string;
Procedure DbgMark;
Procedure DbgSendByte ( AData : byte );
Procedure SysStop;
Procedure DbgSendStr ( Const AStr : string );
Procedure DbgSendHexT ( AData : byte );

Function GetClock : Cardinal;

Procedure TimerBStart ( ATime : word );
Procedure TimerBStop;
Function TimerBCheck : boolean;

Implementation

Procedure WDReset; External;
Procedure LedYSet ( AData : byte ); External;
Procedure LedRgbSet ( AData : byte ); External;
Procedure PinDbgSet ( AData : byte ); External;

Procedure Writeln ( Const ADataS : string ); External;
Function IntToStr ( AData : Integer ) : string; External;
Function FloatToStr ( AData : real ) : string; External;
Procedure DbgMark; External;
Procedure DbgSendByte ( AData : byte ); External;
Procedure DbgSendHexT ( AData : byte ); External;
Procedure SysStop; External;

Procedure TimerBStart ( ATime : word ); External;
Procedure TimerBStop; External;
Function TimerBCheck : boolean; External;

Procedure DbgSendStr ( Const AStr : string );
Var
  BIndex,
  BLen          : Integer;
  BData         : byte;
Begin
 BLen:=AStr[0];
 for BIndex:=1 to BLen do
  begin
  BData:=AStr[BIndex];
  DbgSendByte(BData);
  end;
End;

Function GetClock : Cardinal;
Begin
 Result:=0;
End;

end.

