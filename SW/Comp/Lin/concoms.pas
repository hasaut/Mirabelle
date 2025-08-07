unit ConComS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConComL;

Type
  TOsIs    = (oiWin, oiLin, ioMac);

Function AbsFilename ( Const ABasePath, ARelName : string ) : string;
Function RelFilename ( Const ABasePath, AAbsName : string ) : string;
Function AssembleFullName ( Const APath, AName, AExt : string ) : string;
Procedure ForceCheckFileExtA ( Var AFullName : string; Const AExt : string );
Function ReplacePathSlash ( Const APath : string ) : string;
Procedure ReplacePathSlashVar ( Var APath : string );
Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string ); Overload;
//Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string; AStartIndex : Integer ); Overload;
Procedure AppendLastSlash ( Var S : string );

Const
  COsIs        = oiLin;
  CFileSlash   = '/';
  CForceEol    = #13;

implementation

{Function AbsFilename ( Const ABasePath, ARelName : string ) : string;
Var
  BRelName : string;
  BPath    : string;
  BLen     : Integer;
  BIndex   : Integer;
  BPos     : Integer;

Begin
 repeat
 BRelName:=ARelName;
 DelFirstSpace(BRelName);

 if (BRelName<>'') and (BRelName[1] in ['/','\']) then
  begin
  Result:=BRelName;
  break;
  end;
 if Pos('./',BRelName)=1 then
  begin
  Delete(BRelName,1,2);
  Result:=IncludeTrailingPathDelimiter(GetCurrentDir)+BRelName;
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

 repeat
 BPos:=Pos('//',Result);
 if BPos=0 then break;
 Delete(Result,BPos,1);
 until FALSE;

 until TRUE;
End; }

Function AbsFilename ( Const ABasePath, ARelName : string ) : string;
Var
  BName     : string;
  BPath     : string;
  BLen      : Integer;
  BIndex    : Integer;
  BPos      : Integer;
Begin
 repeat
 BName:=ARelName;
 BPath:=ABasePath;
 DelLastSlash(BPath);
 DelFirstSpace(BName);
 if ExtractFileDrive(BName)<>'' then
  begin
  Result:=BName;
  break;
  end;
 if Pos('./',BName)=1 then
  begin
  Delete(BName,1,2);
  Result:=IncludeTrailingPathDelimiter(GetCurrentDir)+BName;
  break;
  end;

 if (BName<>'') and (BName[1]='/') then begin Result:=BName; break; end;

 DelFirstSlash(BName);
 while BName<>'' do
  begin
  if Copy(BName,1,2)<>'..' then break;
  Delete(BName,1,2); DelFirstSlash(BName);
  BLen:=Length(BPath);
  BIndex:=BLen;
  while BIndex>0 do
   begin
   if BPath[BIndex]=CFileSlash then break;
   dec(BIndex);
   end;
  Delete(BPath,BIndex,BLen-BIndex+1);
  end;

 Result:=BPath+CFileSlash+BName;

 repeat
 BPos:=Pos('//',Result);
 if BPos=0 then break;
 Delete(Result,BPos,1);
 until FALSE;

 until TRUE;
End;

{Function RelFilename ( Const ABasePath, AAbsName : string ) : string;
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
  BPathCmp:=ReadTillC(BPathThis,'/'); BNameCmp:=ReadTillC(BNameThis,'/');
  if BPathCmp<>BNameCmp then break;
  BPathPrev:=BPathThis; BNamePrev:=BNameThis;
  end;
 while BPathPrev<>'' do
  begin
  ReadTillC(BPathPrev,'/');
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
End;}

Function RelFilename ( Const ABasePath : string; Const AAbsName : string ) : string;
Var
  BBasePath,
  BPath,
  BFilename : string;
Begin
 //Result:=ExtractRelativePath(ABasePath,ExtractFilePath(AAbsName))+ExtractFilename(AAbsName);
 Result:='';
 repeat
 BBasePath:=IncludeTrailingPathDelimiter(ABasePath);
 BPath:=ExtractFilePath(AAbsName);
 BFilename:=ExtractFilename(AAbsName);
 if BPath='' then
  begin
  Result:=AAbsName;
  break;
  end;
 if BPath[1]<>'/' then
  begin
  Result:=AAbsName;
  break;
  end;
 Result:=ExtractRelativePath(BBasePath,BPath)+BFilename;
 until TRUE;
End;

Function AssembleFullName ( Const APath, AName, AExt : string ) : string;
Begin
 Result:='';
 if APath<>'' then Result:=IncludeTrailingPathDelimiter(APath);
 Result:=Result+AName;
 if AExt<>'' then
  begin
  if AExt[1]='.' then Result:=Result+AExt
  else Result:=Result+'.'+AExt;
  end;
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

Function ReplacePathSlash ( Const APath : string ) : string;
Var
  BIndex    : Integer;
Begin
 Result:=APath;
 BIndex:=0;
 while BIndex<Length(Result) do
  begin
  if Result[1+BIndex]='\' then Result[1+BIndex]:='/';
  inc(BIndex);
  end;
End;

Procedure ReplacePathSlashVar ( Var APath : string );
Var
  BIndex    : Integer;
Begin
 BIndex:=0;
 while BIndex<Length(APath) do
  begin
  if APath[1+BIndex]='\' then APath[1+BIndex]:='/';
  inc(BIndex);
  end;
End;

Procedure ReplaceListSlash ( AList : TStringList; Const AMask : string );
Var
  BDummyS       : string;
  BPos          : Integer;
  BIndex        : Integer;
Begin
 BIndex:=0;
 while BIndex<AList.Count do
  begin
  BDummyS:=AList.Strings[BIndex];
  if Pos(AMask,BDummyS)=1 then
   begin
   BPos:=Pos('\',BDummyS);
   while BPos<>0 do
    begin
    BDummyS[BPos]:='/';
    BPos:=Pos('\',BDummyS);
    end;
   AList.Strings[BIndex]:=BDummyS;
   end;
  inc(BIndex);
  end;
End;
{
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
}
Procedure AppendLastSlash ( Var S : string );
Var
  BLen          : Integer;
Begin
 BLen:=Length(S);
 if BLen=0 then S:=S+'/'
 else if S[BLen]<>'/' then S:=S+'/';
End;

end.

