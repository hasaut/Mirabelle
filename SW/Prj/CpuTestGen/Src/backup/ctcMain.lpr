program ctcMain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TcProc;

Var
  HTcProc = TTcProc;

begin
 HTcProc:=TTcProc.Create;
 HTcProc.Build;
 HTcProc.Free;
end.

