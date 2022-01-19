unit ComPortBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TComPortBase = class(TObject)
  private
  protected
    FLastError  : Integer;

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function GetErrorDesc : string;

    property LastError : Integer read FLastError;
  end;

Const
  dcb_Binary = $00000001;
  dcb_ParityCheck = $00000002;
  dcb_OutxCtsFlow = $00000004;
  dcb_OutxDsrFlow = $00000008;
  dcb_DtrControlMask = $00000030;
  dcb_DtrControlDisable = $00000000;
  dcb_DtrControlEnable = $00000010;
  dcb_DtrControlHandshake = $00000020;
  dcb_DsrSensivity = $00000040;
  dcb_TXContinueOnXoff = $00000080;
  dcb_OutX = $00000100;
  dcb_InX = $00000200;
  dcb_ErrorChar = $00000400;
  dcb_NullStrip = $00000800;
  dcb_RtsControlMask = $00003000;
  dcb_RtsControlDisable = $00000000;
  dcb_RtsControlEnable = $00001000;
  dcb_RtsControlHandshake = $00002000;
  dcb_RtsControlToggle = $00003000;
  dcb_AbortOnError = $00004000;
  dcb_Reserveds = $FFFF8000;

Const
  CComErrAlreadyOwned = 9991;
  CComErrAlreadyInUse = 9992;
  CComErrWrongParameter = 9993;
  CComErrPortNotOpen = 9994;
  CComErrNoDeviceAnswer =  9995;
  CComErrMaxBuffer = 9996;
  CComErrTimeout = 9997;
  CComErrNotRead = 9998;
  CComErrFrame = 9999;
  CComErrOverrun = 10000;
  CComErrRxOver = 10001;
  CComErrRxParity = 10002;
  CComErrTxFull = 10003;

implementation

Constructor TComPortBase.Create;
Begin
 Inherited;
End;

Destructor TComPortBase.Destroy;
Begin
 Inherited;
End;

Function TComPortBase.GetErrorDesc : string;
begin
  Result:= '';
  case FLastError of
    0:                     Result:='OK';
    CComErrAlreadyOwned:   Result:='Port owned by other process';
    CComErrAlreadyInUse:   Result:='Instance already in use';
    CComErrWrongParameter: Result:='Wrong parameter at call';
    CComErrPortNotOpen:    Result:='Instance not yet connected';
    CComErrNoDeviceAnswer: Result:='No device answer detected';
    CComErrMaxBuffer:      Result:='Maximal buffer length exceeded';
    CComErrTimeout:        Result:='Timeout during operation';
    CComErrNotRead:        Result:='Reading of data failed';
    CComErrFrame:          Result:='Receive framing error';
    CComErrOverrun:        Result:='Receive Overrun Error';
    CComErrRxOver:         Result:='Receive Queue overflow';
    CComErrRxParity:       Result:='Receive Parity Error';
    CComErrTxFull:         Result:='Tranceive Queue is full';
    else                   Result:=SysErrorMessage(FLastError);
  end;
end;


end.

