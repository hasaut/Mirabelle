Unit StrMs;

Interface

Function Length ( Const ADataS : string ) : byte;
Procedure SetLength ( Var ADataS : string; ANewLen : byte );
Function ReadParamStr ( Var ADataS : string ) : string;
Function ReadTillC ( Var ADataS : string; ASeparator : char ) : string;
Function LowerCase ( Const ADataS : string ) : string;
Procedure LowerCaseV ( Var ADataS : string );
Function TryHexToInt ( Const ADataS : string; Var ADataI : Cardinal ) : boolean;
Function TryStrToInt ( Const ADataS : string; Var ADataI : Integer ) : boolean;
Procedure DelFirstSpace ( Var ADataS : string );
Procedure Delete ( Var ADataS : string; AStart, ACount : byte );
Procedure HexToBin ( Var ADataS : string );
Procedure BinToHex ( Var ADataS : string );
Function Copy ( Const ADataS : string; AStart, ACount : byte ) : string;
Function IntToHex ( AData : Cardinal; ADigits : byte ) : string;

Implementation

Function Length ( Const ADataS : string ) : byte; External;
Procedure SetLength ( Var ADataS : string; ANewLen : byte ); External;
Function ReadParamStr ( Var ADataS : string ) : string; External;
Function ReadTillC ( Var ADataS : string; ASeparator : char ) : string; External;
Function LowerCase ( Const ADataS : string ) : string; External;
Procedure LowerCaseV ( Var ADataS : string ); External;
Function TryHexToInt ( Const ADataS : string; Var ADataI : Cardinal ) : boolean; External;
Function TryStrToInt ( Const ADataS : string; Var ADataI : Integer ) : boolean; External;
Procedure DelFirstSpace ( Var ADataS : string ); External;
Procedure Delete ( Var ADataS : string; AStart, ACount : byte ); External;
Procedure HexToBin ( Var ADataS : string ); External;
Procedure BinToHex ( Var ADataS : string ); External;
Function Copy ( Const ADataS : string; AStart, ACount : byte ) : string; External;
Function IntToHex ( AData : Cardinal; ADigits : byte ) : string; External;

end.

