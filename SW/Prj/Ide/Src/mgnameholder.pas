unit MgNameHolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TNhObjectClass =
    (
     ocTop,
       ocHrwG,
         ocHrwF,
       ocDigG,
         ocCpuG,
           ocCpuF,
         ocMemG,
           ocMemF,
         ocPerG,
           ocPerF,
       ocFrwG,
         ocThrG,
           ocThrF,
         ocIncG,
           ocIncF,
         ocOutG,
           ocOutF,
         ocSrcG, ocGrpG,
           ocSrcF,
         ocTstG,
           ocTstF,
       ocTleG,
         ocRvcG,
           ocRvcF,
         ocLnkG,
           ocLnkF,
       ocDbgG,
         ocDbgF
    );

{
TvProject.Items.Clear;
FTopNode:=TvProject.Items.Add(nil,FConfigFileName);
FCfgNode:=TvProject.Items.AddChild(FTopNode,'Settings');
  FSegNode:=TvProject.Items.AddChild(FCfgNode,'Memory segments');
  FHexNode:=TvProject.Items.AddChild(FCfgNode,'Memory init files');
  FGdbNode:=TvProject.Items.AddChild(FCfgNode,'GDB dump');
  FDirNode:=TvProject.Items.AddChild(FCfgNode,'Source search path');
FSrcNode:=TvProject.Items.AddChild(FTopNode,'Source files');
FAsmNode:=TvProject.Items.AddChild(FTopNode,'Disassembler');
}

  TNameHolder = class(TObject)
  private
    FObjectClass    : TNhObjectClass;
    FFullName       : string;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( AObjectClass : TNhObjectClass; Const AFullName : string );
    Procedure Init ( Const AFullName : string );

    property ObjectClass : TNhObjectClass read FObjectClass;
    property FullName : string read FFullName;
  end;

implementation

Constructor TNameHolder.Create;
Begin
 Inherited;
End;

Destructor TNameHolder.Destroy;
Begin
 Inherited;
End;

Procedure TNameHolder.Init ( AObjectClass : TNhObjectClass; Const AFullName : string );
Begin
 FObjectClass:=AObjectClass;
 FFullName:=AFullName;
End;

Procedure TNameHolder.Init ( Const AFullName : string );
Begin
 FFullName:=AFullName;
End;

end.

