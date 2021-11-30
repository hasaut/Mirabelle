program md_gui;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, SdMain_gui, BaseUnix;

const
  LIB_NAME = 'c';

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
 signal(SIGTERM, @ProcessSigterm);
 if SdMainForm<>nil then SdMainForm.ProcessSignal(sig);
end;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.StopOnException:=FALSE;
  Application.CreateForm(TSdMainForm, SdMainForm);
  signal(SIGINT, @ProcessSigint);
  signal(SIGTERM, @ProcessSigterm);
  Application.Run;
end.


