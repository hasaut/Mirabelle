program md_cli;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SdMain_cli,
  BaseUnix;

const
  SIGINT   =  2;
  //SIGTERM  = 15;
  SIGBREAK = 21;

const
  LIB_NAME = 'c';

Type
  signal_func = procedure(sig: cint); cdecl;

function signal(sig: cint; func: signal_func): signal_func; cdecl; external LIB_NAME Name 'signal';

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

// --ProjectFile /home/ozh/smb/ZukH/Projects/Mlx81143BA_dp_v4x/FW/Mlx81143BA_dp_v4x.prj  --Startup exit --flash 0x800000 0x700000 /home/ozh/smb/ZukHL/Projects/Mlx81143BA_dp_v4x/AltCX/output_files/Mlx81143BA_dp_v4x_auto.rpd --flash 0xF00000 0x100000 /home/ozh/smb/ZukH/Projects/Mlx81143BA_dp_v4x/FW/Out/Mlx81143BA_dp_v4x_code.hex

(*
program MlxDebug_cli;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, MdCliBatch,
  BaseUnix;

const
  LIB_NAME = 'c';

Type
  signal_func = procedure(sig: cint); cdecl;

function signal(sig: cint; func: signal_func): signal_func; cdecl; external LIB_NAME Name 'signal';

Var
  HMdBatch  : TMdBatch;

Procedure ProcessSigint ( sig : cint ); cdecl;
Begin
 signal(SIGINT, @ProcessSigint);
 if HMdBatch<>nil then HMdBatch.ProcessSignal(sig);
End;

Procedure ProcessSigterm ( sig : cint ); cdecl;
begin
 signal(SIGTERM, @ProcessSigterm);
 if HMdBatch<>nil then HMdBatch.ProcessSignal(sig);
end;

{$R *.res}

Begin
 HMdBatch:=TMdBatch.Create;
 signal(SIGINT, @ProcessSigint);
 signal(SIGTERM, @ProcessSigterm);
 System.ExitCode:=HMdBatch.Process;
 HMdBatch.Free;
End.

*)

