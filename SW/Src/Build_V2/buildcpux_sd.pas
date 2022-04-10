unit BuildCpuX_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, AsmMcuMs_sd, AsmMcuRV_sd, HexMcuRV_sd, AsmMcuWA_sd, HexMcuWA_sd, BuildStack_sd;

Type
  TBuildCpuX = class(TBuildStack)
  protected
    Function CreateUnit ( Const AFilename : string ) : TAsmBase; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

implementation

Constructor TBuildCpuX.Create;
Begin
 Inherited;
End;

Destructor TBuildCpuX.Destroy;
Begin
 Inherited;
End;

Function TBuildCpuX.CreateUnit ( Const AFilename : string ) : TAsmBase;
Var
  BExtL         : string;
Begin
 BExtL:=LowerCase(ExtractFileExt(AFilename));
 if BExtL='.s' then Result:=TAsmMcuRV.Create
 else if BExtL='.srv' then Result:=TAsmMcuRV.Create
 else if BExtL='.i' then Result:=TAsmMcuRV.Create
 else if BExtL='.hex' then Result:=THexMcuRV.Create
 else if BExtL='.wasm' then Result:=THexMcuWA.Create
 else if BExtL='.pas' then Result:=TPasMcuMs.Create
 else if (BExtL='.ci') or (BextL='.h') then Result:=TCppMcuMs.Create
 else if BExtL='.c' then Result:=TCppMcuRV.Create
 else if BExtL='.py' then Result:=TPyMcuMs.Create
 else if BExtL='.rs' then Result:=TRustMcuMs.Create
 else if BExtL='.cll' then Result:=TLlirMcuMs.Create
 else Result:=TAsmMcuMs.Create;
 if Result.Module<>nil then Result.Module.PrjParams:=FPrjParams;
End;

end.

