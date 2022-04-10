unit LlvmItems_sd;

{$mode objfpc}{$H+}

Interface

uses
  Classes, SysUtils;

Type
  TVarListAppend = Function ( Const AName : string; ADataType : TLlvmType; ADeclPlace : TVarDeclPlace; ACaseSensitive : boolean ) : TLlvmVar of object;


  TLlvmBaseType = (lbtNone, lbtB1, lbtU8, lbtI8, lbtU16, lbtI16, lbtU32, lbtI32, lbtF32, lbtRecord);
  TVarDeclPlace = (vdpNone, vdpPrivate, vdpPublic, vdpParam, vdpParamC, vdpParamV, vdpRetVal, vdpLocal, vdpTmp);

  TLlvmVar = class;
  TLlvmVarList = array of TLlvmVar;

  TLlvmType = class(TObject)
  private
    FName,
    FNameS      : string;
    FBaseType   : TLlvmBaseType;
    FFieldList  : TLlvmVarList;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( Const AName : string; ABaseType : TLlvmBaseType );

    property Name : string read FName;
    property FieldList : TLlvmVarList read FFieldList;
  end;

  TLlvmTypeList = array of TLlvmType;

  TLlvmVar = class(TObject)
  private
    FName,
    FNameS      : string;
    FDataType   : TLlvmType;
    FDeclPlace  : TVarDeclPlace;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Init ( Const AName : string; ADataType : TLlvmType; APlace : TVarDeclPlace );

    property Name : string read FName;
    property DataType : TLlvmType read FDataType;
    property DeclPlace : TVarDeclPlace read FDeclPlace;
  end;


Procedure TypeListClear ( Var ATypeList : TLlvmTypeList );
Procedure TypeListAppend ( Var ATypeList : TLlvmTypeList; AType : TLlvmType );
Function TypeListSearch ( Const ATypeList : TLlvmTypeList; Const AName : string; ACaseSensitive : boolean ) : TLlvmType;

Procedure VarListClear ( Var AVarList : TLlvmVarList );
Procedure VarListAppend ( Var AVarList : TLlvmVarList; AVar : TLlvmVar );
Function VarListAppend ( Var AVarList : TLlvmVarList; Const AName : string; ADataType : TLlvmType; ADeclPlace : TVarDeclPlace; ACaseSensitive : boolean ) : TLlvmVar;
Function VarListSearch ( Const AVarList : TLlvmVarList; Const AName : string; ACaseSensitive : boolean ) : TLlvmVar;


Implementation

// Common

Procedure TypeListClear ( Var ATypeList : TLlvmTypeList );
Var
  BIndex    : Integer;
Begin
 BIndex:=0; while BIndex<Length(ATypeList) do begin ATypeList[BIndex].Free; inc(BIndex); end;
 ATypeList:=nil;
End;

Procedure TypeListAppend ( Var ATypeList : TLlvmTypeList; AType : TLlvmType );
Var
  BIndex    : Integer;
Begin
 BIndex:=Length(ATypeList); SetLength(ATypeList,BIndex+1); ATypeList[BIndex]:=AType;
End;

Function TypeListSearch ( Const ATypeList : TLlvmTypeList; Const AName : string; ACaseSensitive : boolean ) : TLlvmType;
Var
  BIndex    : Integer;
  BName     : string;
  BType     : TLlvmType;
Begin
 Result:=nil;
 if ACaseSensitive then BName:=AName else BName:=LowerCase(AName);
 BIndex:=0;
 while BIndex<Length(ATypeList) do
  begin
  BType:=ATypeList[BIndex];
  if ACaseSensitive then
   begin
   if BType.FName=BName then begin Result:=BType; break; end;
   end
  else
   begin
   if BType.FNameS=BName then begin Result:=BType; break; end;
   end;
  inc(BIndex);
  end;
End;

Procedure VarListClear ( Var AVarList : TLlvmVarList );
Var
  BIndex    : Integer;
Begin
 BIndex:=0; while BIndex<Length(AVarList) do begin AVarList[BIndex].Free; inc(BIndex); end;
 AVarList:=nil;
End;

Procedure VarListAppend ( Var AVarList : TLlvmVarList; AVar : TLlvmVar );
Var
  BIndex    : Integer;
Begin
 BIndex:=Length(AVarList); SetLength(AVarList,BIndex+1); AVarList[BIndex]:=AVar;
End;

Function VarListAppend ( Var AVarList : TLlvmVarList; Const AName : string; ADataType : TLlvmType; ADeclPlace : TVarDeclPlace; ACaseSensitive : boolean ) : TLlvmVar;
Begin
 Result:=nil;
 repeat
 if VarListSearch(AVarList,AName,ACaseSensitive)<>nil then break;
 Result:=TLlvmVar.Create;
 Result.Init(AName,ADataType,ADeclPlace);
 until TRUE;
End;

Function VarListSearch ( Const AVarList : TLlvmVarList; Const AName : string; ACaseSensitive : boolean ) : TLlvmVar;
Var
  BIndex    : Integer;
  BName     : string;
  BVar      : TLlvmVar;
Begin
 Result:=nil;
 if ACaseSensitive then BName:=AName else BName:=LowerCase(AName);
 BIndex:=0;
 while BIndex<Length(AVarList) do
  begin
  BVar:=AVarList[BIndex];
  if ACaseSensitive then
   begin
   if BVar.FName=BName then begin Result:=BVar; break; end;
   end
  else
   begin
   if BVar.FNameS=BName then begin Result:=BVar; break; end;
   end;
  inc(BIndex);
  end;
End;


// TLlvmType

Constructor TLlvmType.Create;
Begin
 Inherited;
End;

Destructor TLlvmType.Destroy;
Begin
 VarListClear(FFieldList);
 Inherited;
End;

Procedure TLlvmType.Init ( Const AName : string; ABaseType : TLlvmBaseType );
Begin
 FName:=AName; FNameS:=LowerCase(AName);
 FBaseType:=ABaseType;
End;

// TLlvmVar

Constructor TLlvmVar.Create;
Begin
 Inherited;
End;

Destructor TLlvmVar.Destroy;
Begin
 Inherited;
End;

Procedure TLlvmVar.Init ( Const AName : string; ADataType : TLlvmType; APlace : TVarDeclPlace );
Begin
 FName:=AName; FNameS:=LowerCase(AName);
 FDataType:=ADataType;
 FDeclPlace:=APlace;
End;

end.

