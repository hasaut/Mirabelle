unit LlvmDbg_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LlvmBase_sd, ConComL, AsmTypes_sd, LlvmLine_sd, ParsHelper_sd;

Function DbgFormatFlow ( Const ALine : string ) : string;
Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; AText : TStringList; AFormatData : boolean );
Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; Const AFlow : TFlowLineList; AFormatData : boolean );
Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; Const AFlow : TFlowLineList; AFormatData : boolean; Const AInfo : string );
Procedure DbgExtractVerifInfo ( ASrc : TStringList; AVerifInfo : TStringList );
Function DbgCheckVerifInfo ( AErrorList, AVerifList : TStringList ) : boolean;

implementation

Function DbgFormatFlow ( Const ALine : string ) : string;
Var
  BLine         : string;
  BReadS        : string;
Begin
 Result:='';
 repeat
 BLine:=ALine;
 BReadS:=ReadParamStr(BLine);
 if IsLabel(BReadS) then
  begin
  Result:='     '+BReadS;
  Result:=AddSpacesResR(Result,50);
  //Result:=Result+ReadParamStr(BLine);
  break;
  end;
 Result:='        '+BReadS;
 Result:=AddSpacesResR(Result,17);
 while BLine<>'' do
  begin
  BReadS:=ReadParamStr(BLine);
  if BReadS='' then break;
  if BReadS[1]=';' then
   begin
   //Result:=AddSpacesResR(Result,50);
   //Result:=Result+BReadS;
   break;
   end;
  Result:=Result+RemoveRecInside(BReadS)+' ';
  end;
 until TRUE;
End;

Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; AText : TStringList; AFormatData : boolean );
Var
  BLine         : TFlowLine;
  BText         : TStringList;
  BIndex        : Integer;
  BFilename     : string;
Begin
 BLine:=TFlowLine.Create;
 BText:=TStringList.Create;

 if AFormatData=FALSE then BText.Assign(AText)
 else
  begin
  for BIndex:=0 to AText.Count-1 do
   begin
   BLine.RdLine(AText.Strings[BIndex]);
   BText.Append(BLine.FormatNice);
   end;
  end;

 BFilename:=AProc.Module.DbgPath+AProc.Module.Name+'_'+AProc.Name+'_'+ANameSuffix+'.txt';

 try
   BText.SaveToFile(BFilename);
 except
 end;

 BLine.Free;
 BText.Free;
End;

Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; Const AFlow : TFlowLineList; AFormatData : boolean );
Var
  BText         : TStringList;
  BIndex        : Integer;
  BFilename     : string;
Begin
 BText:=TStringList.Create;

 for BIndex:=0 to Length(AFlow)-1 do
  begin
  if AFormatData then BText.Append(AFlow[BIndex].FormatNice)
  else BText.Append(AFlow[BIndex].Orig);
  end;

 BFilename:=AProc.Module.DbgPath+AProc.Module.Name+'_'+AProc.Name+'_'+ANameSuffix+'.txt';

 try
   BText.SaveToFile(BFilename);
 except
 end;

 BText.Free;
End;

Procedure DbgSave ( AProc : TLlvmProc; Const ANameSuffix : string; Const AFlow : TFlowLineList; AFormatData : boolean; Const AInfo : string );
Var
  BText         : TStringList;
  BIndex        : Integer;
  BFilename     : string;
Begin
 BText:=TStringList.Create;

 BText.Append(AInfo);

 for BIndex:=0 to Length(AFlow)-1 do
  begin
  if AFormatData then BText.Append(AddSpacesResL(IntToStr(BIndex),3)+': '+AFlow[BIndex].FormatMatr)
  else BText.Append(AFlow[BIndex].Orig);
  end;

 BFilename:=AProc.Module.DbgPath+'Dbg\'+AProc.Module.Name+'_'+AProc.Name+'_'+ANameSuffix+'.txt';

 try
   BText.SaveToFile(BFilename);
 except
 end;

 BText.Free;
End;

Procedure DbgExtractVerifInfo ( ASrc : TStringList; AVerifInfo : TStringList );
Var
  BIndex        : Integer;
  BReadS        : string;
  BPosA,
  BPosB         : Integer;
Begin
 for BIndex:=0 to ASrc.Count-1 do
  begin
  BReadS:=ASrc.Strings[BIndex];
  repeat
  BPosA:=Pos(CTagS+'vm',BReadS);
  if BPosA=0 then break;
  Delete(BReadS,1,BPosA-1);
  BPosB:=Pos(CTagM,BReadS);
  if BPosB<>5 then break;
  BPosB:=Pos(CTagE,BReadS);
  if BPosB=0 then break;
  AVerifInfo.Append(Copy(BReadS,1,BPosB));
  until TRUE;
  end;
End;

Function DbgCheckVerifInfo ( AErrorList, AVerifList : TStringList ) : boolean;
Var
  BVerifIdx,
  BErrorIdx     : Integer;
  BVerifS,
  BErrorS,
  BReadS        : string;
  BVerifSHolder : string;
  BFoundAll     : boolean;
Begin
 BVerifIdx:=0;
 while BVerifIdx<AVerifList.Count do
  begin
  BVerifS:=AVerifList.Strings[BVerifIdx];
  BErrorIdx:=0;
  while BErrorIdx<AErrorList.Count do
   begin
   BErrorS:=LowerCase(AErrorList.Strings[BErrorIdx]);
   BFoundAll:=TRUE;
   BVerifSHolder:=LowerCase(ParsExtractName(BVerifS));
   while BVerifSHolder<>'' do
    begin
    BReadS:=ReadParamStr(BVerifSHolder); if BReadS='' then break;
    if StrInList(BReadS,BErrorS)=FALSE then begin BFoundAll:=FALSE; break; end;
    end;
   if BFoundAll then break;
   inc(BErrorIdx);
   end;
  if BErrorIdx>=AErrorList.Count then break;
  inc(BVerifIdx);
  end;
 Result:=BVerifIdx=AVerifList.Count;
End;

end.

