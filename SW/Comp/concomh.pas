unit ConComH;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TFileTree = class;
  TFileTreeList = array of TFileTree;

  TFileTree = class(TObject)
  private
    FQuName     : string;
    FChildList  : TFileTreeList;

    Procedure ClearChildList;
    Function AppendChild : TFileTree;
    Function FindChild ( Const AName : string ) : TFileTree;
    Procedure FindAppendChildA ( Const APath : string );
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure FindAppendChild ( Const APath : string );
    Procedure Optimize;

    property QuName : string read FQuName write FQuName;
    property ChildList : TFileTreeList read FChildList;
  end;

implementation

Uses
  ConComL, ConComS;

Constructor TFileTree.Create;
Begin
 Inherited;
End;

Destructor TFileTree.Destroy;
Begin
 ClearChildList;
 Inherited;
End;

Procedure TFileTree.ClearChildList;
Var
  BChildIdx     : Integer;
Begin
 BChildIdx:=0;
 while BChildIdx<Length(FChildList) do
  begin
  FChildList[BChildIdx].Free;
  inc(BChildIdx);
  end;
 FChildList:=nil;
End;

Function TFileTree.AppendChild : TFileTree;
Var
  BChildIdx : Integer;
Begin
 Result:=TFileTree.Create;
 BChildIdx:=Length(FChildList); SetLength(FChildList,BChildIdx+1); FChildList[BChildIdx]:=Result;
End;

Function TFileTree.FindChild ( Const AName : string ) : TFileTree;
Var
  BChildIdx : Integer;
  BChild    : TFileTree;
Begin
 Result:=nil;
 BChildIdx:=0;
 while BChildIdx<Length(FChildList) do
  begin
  BChild:=FChildList[BChildIdx];
  if BChild.QuName=AName then begin Result:=BChild; break; end;
  inc(BChildIdx);
  end;
End;

Procedure TFileTree.FindAppendChild ( Const APath : string );
Var
  BPath     : string;
  BCharIdx  : Integer;
Begin
 BPath:=APath;
 BCharIdx:=1; // In Linux, we cannot delete the first slash like /home/...
 while BCharIdx<Length(BPath) do
  begin
  if BPath[1+BCharIdx]=CFileSlash then BPath[1+BCharIdx]:=' ';
  inc(BCharIdx);
  end;
 FindAppendChildA(BPath);
End;

Procedure TFileTree.FindAppendChildA ( Const APath : string );
Var
  BPath     : string;
  BQuName   : string;
  BChild    : TFiletree;
Begin
 BPath:=APath;
 BQuName:=ReadParamStr(BPath); DelFirstSpace(BPath);
 BChild:=FindChild(BQuName);
 if BChild=nil then begin BChild:=AppendChild; BChild.QuName:=BQuName; end;
 if BPath<>'' then BChild.FindAppendChildA(BPath);
End;

Procedure TFileTree.Optimize;
Var
  BChild    : TFileTree;
Begin
 repeat
 if Length(FChildList)>1 then break;
 if Length(FChildList)=0 then break;
 BChild:=FChildList[0];
 if Length(BChild.FChildList)=0 then break;
 BChild.Optimize;
 if FQuName='' then break;
 FQuName:=FQuName+CFileSlash+BChild.QuName;
 FChildList:=Copy(BChild.FChildList);
 BChild.FChildList:=nil;
 BChild.Free;
 until TRUE;
End;

end.

