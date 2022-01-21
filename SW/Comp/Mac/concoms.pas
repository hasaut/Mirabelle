unit ConComS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConComL, SynEditTypes;

Function AbsFilename ( ABasePath, ARelName : string ) : string;
Function RelFilename ( ABasePath, AAbsName : string ) : string;
Function AssembleFullName ( APath, AName, AExt : string ) : string;
Procedure ForceCheckFileExtA ( Var AFullName : string; Const AExt : string );
Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;
Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string ); Overload;
Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string; AStartIndex : Integer ); Overload;

implementation

Const
  CFileSlash         = '/';

Function AbsFilename ( ABasePath, ARelName : string ) : string;
Var
  BRelName : string;
  BPath    : string;
  BLen     : Integer;
  BIndex   : Integer;

Begin
 repeat
 BRelName:=ARelName;
 DelFirstSpace(BRelName);

 if (BRelName<>'') and (BRelName[1] in ['/','\']) then
  begin
  Result:=BRelName;
  break;
  end;

 BPath:=ABasePath;

 while BRelName<>'' do
  begin
  if Copy(BRelName,1,2)<>'..' then break;
  Delete(BRelName,1,2); DelFirstSlash(BRelName);
  BLen:=Length(BPath);
  BIndex:=BLen;
  while BIndex>0 do
   begin
   if (BPath[BIndex] in ['\','/']) then break;
   dec(BIndex);
   end;
  Delete(BPath,BIndex,BLen-BIndex+1);
  end;

 Result:=BPath+'/'+BRelName;
 until TRUE;
End;

Function RelFilename ( ABasePath, AAbsName : string ) : string;
Var
  BPathPrev,
  BNamePrev,
  BPathThis,
  BNameThis   : string;
  BPathCmp,
  BNameCmp    : string;
  BBackIndex  : Integer;
Begin
 Result:=AAbsName;
 repeat
 if (ABasePath='') or (AAbsName='') then break;
 if (ABasePath[1]<>'/') or (AAbsName[1]<>'/') then break;
 BPathThis:=ABasePath; BNameThis:=AAbsName;
 Delete(BPathThis,1,1); Delete(BNameThis,1,1);
 BPathPrev:=BPathThis; BNamePrev:=BNameThis;
 BBackIndex:=0;
 while (BPathThis<>'') and (BNameThis<>'') do
  begin
  BPathCmp:=ReadTill(BPathThis,'/'); BNameCmp:=ReadTill(BNameThis,'/');
  if BPathCmp<>BNameCmp then break;
  BPathPrev:=BPathThis; BNamePrev:=BNameThis;
  end;
 while BPathPrev<>'' do
  begin
  ReadTill(BPathPrev,'/');
  inc(BBackIndex);
  end;
 //Result:='/';
 Result:='';
 while BBackIndex<>0 do
  begin
  Result:=Result+'../';
  dec(BBackIndex);
  end;
 Result:=Result+BNamePrev;
 until TRUE;
End;

Function AssembleFullName ( APath, AName, AExt : string ) : string;
Begin
 Result:=APath;
 if Result<>'' then Result:=Result+CFileSlash;
 Result:=Result+AName;
 if AExt<>'' then Result:=Result+'.'+AExt;
End;

Procedure ForceCheckFileExtA ( Var AFullName : string; Const AExt : string );
Var
  BPath,
  BName,
  BExt    : string;
Begin
 repeat
 BPath:=''; BName:=''; BExt:='';
 SplitFilename(AFullName,BPath,BName,BExt);
 if BExt<>'' then break;
 AFullName:=AssembleFullName(BPath,BName,AExt);
 until TRUE;
End;

Function AddSearchAgain ( ASearchOptions : TSynSearchOptions ) : TSynSearchOptions;
Begin
 Result:=ASearchOptions-[ssoEntireScope]{+[ssoFindContinue]}; // Seems to be a bug for Linux
End;

Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string );
Var
  BDummyS       : string;
  BPos          : Integer;
Begin
 repeat
 BDummyS:=AList.Values[AMask];
 if BDummyS='' then break;
 BPos:=Pos('\',BDummyS);
 while BPos<>0 do
  begin
  BDummyS[BPos]:='/';
  BPos:=Pos('\',BDummyS);
  end;
 AList.Values[AMask]:=BDummyS;
 until TRUE;
End;

Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string; AStartIndex : Integer );
Var
  BDummyS       : string;
  BPos          : Integer;
  BIndex        : Integer;
  BMask         : string;
Begin
 BIndex:=AStartIndex;
 repeat
 BMask:=AMask+IntToStr(BIndex);
 BDummyS:=AList.Values[BMask];
 if BDummyS='' then break;
 BPos:=Pos('\',BDummyS);
 while BPos<>0 do
  begin
  BDummyS[BPos]:='/';
  BPos:=Pos('\',BDummyS);
  end;
 AList.Values[BMask]:=BDummyS;
 inc(BIndex);
 until FALSE;
End;

end.

