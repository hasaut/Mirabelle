program md_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, SdMain_gui, ctypes, SysUtils, ExtCompDialog_sd;

const
  LIB_NAME = 'msvcrt';
  SIGINT   =  2;
  //SIGILL   =  4;
  //SIGFPE   =  8;
  //SIGSEGV  = 11;
  //SIGTERM  = 15;
  SIGBREAK = 21;
  //SIGABRT  = 22;

Type
  signal_func = procedure(sig: cint); cdecl;

function signal(sig: cint; func: signal_func): signal_func; cdecl; external LIB_NAME Name 'signal';

Procedure ProcessSigint ( sig : cint ); cdecl;
Begin
 signal(SIGINT, @ProcessSigint);
 if SdMainForm<>nil then SdMainForm.ProcessSignal(sig);
End;

Procedure ProcessSigterm ( sig : cint ); cdecl;
begin
 signal(SIGBREAK, @ProcessSigterm);
 if SdMainForm<>nil then SdMainForm.ProcessSignal(sig);
end;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSdMainForm, SdMainForm);
  signal(SIGINT, @ProcessSigint);
  signal(SIGBREAK, @ProcessSigterm);
  Application.Run;
end.

