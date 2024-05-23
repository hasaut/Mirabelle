unit ClassList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  Generic TClassList<TClassType> = class(TObject)
  private
    FItemList    : array of TClassType;

    Function GetItem ( AIndex : SizeInt ) : TClassType;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Clear;
    Function Count : SizeInt;
    Procedure Append ( AItem : TClassType );

    property Items[AIndex : SizeInt] : TClassType read GetItem;
  end;

  Generic TVector<TItemType> = class(TObject)
  private
    FItemList    : array of TItemType;

    Function GetItem ( AIndex : SizeInt ) : TItemType;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Clear;
    Function Count : SizeInt;
    Procedure Append ( AItem : TItemType );

    property Items[AIndex : SizeInt] : TItemType read GetItem;
  end;

implementation

Constructor TClassList.Create;
Begin
 Inherited;
End;

Destructor TClassList.Destroy;
Begin
 Clear;
 Inherited;
End;

Procedure TClassList.Clear;
Var
  BIndex        : SizeInt;
Begin
 BIndex:=0;
 while BIndex<Length(FItemList) do
  begin
  FItemList[BIndex].Free;
  inc(BIndex);
  end;
 FItemList:=nil;
End;

Function TClassList.Count : SizeInt;
Begin
 Result:=Length(FItemList);
End;

Procedure TClassList.Append ( AItem : TClassType );
Var
  BIndex        : SizeInt;
Begin
 BIndex:=Length(FItemList);
 SetLength(FItemList,BIndex+1);
 FItemList[BIndex]:=AItem;
End;

Function TClassList.GetItem ( AIndex : SizeInt ) : TClassType;
Begin
 Result:=FItemList[AIndex];
End;

Constructor TVector.Create;
Begin
 Inherited;
End;

Destructor TVector.Destroy;
Begin
 Clear;
 Inherited;
End;

Procedure TVector.Clear;
Begin
 FItemList:=nil;
End;

Function TVector.Count : SizeInt;
Begin
 Result:=Length(FItemList);
End;

Procedure TVector.Append ( AItem : TItemType );
Var
  BIndex        : SizeInt;
Begin
 BIndex:=Length(FItemList);
 SetLength(FItemList,BIndex+1);
 FItemList[BIndex]:=AItem;
End;

Function TVector.GetItem ( AIndex : SizeInt ) : TItemType;
Begin
 Result:=FItemList[AIndex];
End;

end.

