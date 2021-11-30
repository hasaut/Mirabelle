program md_cli;

{$mode objfpc}{$H+}

uses
  Classes, SdMain_cli,
  ctypes, sysutils;

const
  SIGINT   =  2;
  //SIGTERM  = 15;
  SIGBREAK = 21;

Type
  signal_func = procedure(sig: cint); cdecl;

function signal(sig: cint; func: signal_func): signal_func; cdecl; external 'msvcrt' Name 'signal';

Var
  HSdMain  : TSdMain;

Procedure ProcessSigint ( sig : cint ); cdecl;
Begin
 signal(SIGINT, @ProcessSigint);
 if HSdMain<>nil then HSdMain.ProcessSignal(sig);
End;

Procedure ProcessSigterm ( sig : cint ); cdecl;
begin
 signal(SIGBREAK, @ProcessSigterm);
 if HSdMain<>nil then HSdMain.ProcessSignal(sig);
end;

{$R *.res}

Begin
 HSdMain:=TSdMain.Create;
 signal(SIGINT, @ProcessSigint);
 signal(SIGBREAK, @ProcessSigterm);
 System.ExitCode:=HSdMain.Process;
 HSdMain.Free;
End.

