unit AsmHelperWA_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmBase_sd, AsmTypes_sd, MemSeg_sd;

Type
  TAsmFlowLineWA = class(TAsmFlowLine)
  protected
    Function InvJmpA ( Const AJmp : string ) : string; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Parse ( Const AReadS : string; Const AFileName : string; ALineIdx : Integer ); Override;
    Procedure FixAddr ( Var ASegList : TMemSegList; Var ASeg : TMemSeg ); Override;
    Procedure UpdateRefs ( Const AInternList, AExternList : TAsmRefList; Var AIsAddrOor : boolean ); Override;
  end;

  TAsmHelperWA = class(TAsmBase)
  private
    Procedure ExtractDupOpti ( ALine : TAsmFlowLine; AParam : TAsmLineParam; Var AReadS : string; Out ADupCnt : Integer );
    Procedure CodeGenDB ( ALine : TAsmFlowLine );
    Procedure CodeGenDW ( ALine : TAsmFlowLine );
    Procedure CodeGenDD ( ALine : TAsmFlowLine );
  protected
    Function NewFlowLine : TAsmFlowLine; Override;
    Function IsPointer ( Const AStr : string; Out ALabel : string ) : boolean;
    Procedure CompileLine ( ALine : TAsmFlowLine );
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Compile; Override;
    Procedure CorrectAddrOor; Override;
  end;

implementation

Uses
  ConComL, ConComS;

{ *** TAsmFlowLineWA *** }

Constructor TAsmFlowLineWA.Create;
Begin
 Inherited;
End;

Destructor TAsmFlowLineWA.Destroy;
Begin
 Inherited;
End;

Procedure TAsmFlowLineWA.Parse ( Const AReadS : string; Const AFileName : string; ALineIdx : Integer );
Var
  BCmdL         : string;
  BParamA       : string;
  BTextPos      : Integer;
  BExec         : string;
Begin
 Inherited;

 repeat
 if FCmdIs<>acUnparsed then break;
 if Length(FParams)=0 then begin AppendError('e','Internal error: Params list is empty [R:TAsmFlowLineWA.Parse]'); break; end;

 BCmdL:=LowerCase(FParams[0].Name);

 if BCmdL='.seg' then
  begin
  FCmdIs:=acSegName;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then AppendError('e',FParams[0],'Segment name is absent [R:TAsmFlowLineWA.Parse]');
  AppendParam(BParamA,BTextPos);
  break;
  end;
 if BCmdL='.org' then
  begin
  FCmdIs:=acOrg;
  ReadParamA(' ',BParamA,BTextPos);if BParamA='' then begin AppendError('e',FParams[0],'ORG parameter is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  if TryStrToInt(FParams[1].Name,FAlign)=FALSE then AppendError('e',FParams[1],'Cannot convert ORG to integer [R:TAsmFlowLineWA.Parse]');
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;
 if BCmdL='align' then
  begin
  FCmdIs:=acAlign;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Align parameter is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  if TryStrToInt(FParams[1].Name,FAlign)=FALSE then AppendError('e',FParams[1],'Cannot convert Align to integer [R:TAsmFlowLineWA.Parse]');
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;
 if StrInList(BCmdL,'db dw dd') then
  begin
  FCmdIs:=acData;
  if BCmdL='db' then FAlign:=1
  else if BCmdL='dw' then FAlign:=2
  else {if BCmd='dd' then} FAlign:=4;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL='#stack' then
  begin
  FCmdIs:=acStack;
  FAlign:=4;
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Data is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  repeat
  ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
  AppendParam(BParamA,BTextPos);
  until FALSE;
  break;
  end;
 if BCmdL[Length(BCmdL)]=':' then
  begin
  FCmdIs:=acLabel;
  FLabelName:=FParams[0].Name; Delete(FLabelName,Length(FLabelName),1);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;
 if BCmdL='public' then
  begin
  FCmdIs:=acPublic;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Identifier name is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;
 if BCmdL='extern' then
  begin
  FCmdIs:=acExtern;
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'Identifier name is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;

 BExec:=FExec;
 ReadParamStr(BExec);
 if LowerCase(ReadParamStr(BExec))='equ' then
  begin
  FCmdIs:=acEqu;
  ReadParamA(' ',BParamA,BTextPos); AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA='' then begin AppendError('e',FParams[0],'EQU parameter is missing [R:TAsmFlowLineWA.Parse]'); break; end;
  AppendParam(BParamA,BTextPos);
  ReadParamA(' ',BParamA,BTextPos); if BParamA<>'' then AppendError('e',AppendParam(BParamA,BTextPos),'Extra parameter in line '+BParamA+' [R:TAsmFlowLineWA.Parse]');
  break;
  end;

 FCmdIs:=acCmd;
 repeat
 ReadParamA(',',BParamA,BTextPos); if BParamA='' then break;
 AppendParam(BParamA,BTextPos);
 until FALSE;

 until TRUE;
End;

Procedure TAsmFlowLineWA.FixAddr ( Var ASegList : TMemSegList; Var ASeg : TMemSeg );
Begin
 // acSegmC, acSegmD, acOrg, acAlign, acData, acLabel, acCmd
 repeat
 case FCmdIs of
   acSegName:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Seg name parameter is not defined'); break; end;
     ASeg:=MemSegSearch(ASegList,FParams[1].Name);
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined [R:TAsmFlowLineWA.FixAddr]'); break; end;
     end;
   acOrg:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Org parameter is not defined'); break; end;
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined'); break; end;
     if ASeg.FillSize>FAlign then begin AppendError('e',FParams[1],'Segment is already bigger than org parameter'); break; end;
     ASeg.FillSize:=FAlign;
     end;
   acAlign:
     begin
     if Length(FParams)<2 then begin AppendError('e',FParams[0],'Align parameter is not defined'); break; end;
     if ASeg=nil then begin AppendError('e',FParams[1],'Segment is not defined'); break; end;
     if FAlign<=0 then begin AppendError('e',FParams[1],'Align is too small'); break; end;
     ASeg.FillSize:=((ASeg.FillSize+(FAlign-1)) div FAlign)*FAlign;
     end;
   acData,
   acStack:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     if (FAlign>2) and ((ASeg.FillSize mod FAlign)<>0) then
      begin
      AppendError('w',FParams[0],'Data is misaligned, align is forced');
      ASeg.FillSize:=((ASeg.FillSize+(FAlign-1)) div FAlign)*FAlign;
      end;
     end;
   acLabel:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     end;
   acCmd:
     begin
     if ASeg=nil then begin AppendError('e',FParams[0],'Segment is not defined'); break; end;
     if (ASeg.FillSize mod 2)<>0 then
      begin
      AppendError('w',FParams[0],'Instructions are misaligned, align is forced');
      ASeg.FillSize:=((ASeg.FillSize+(2-1)) div 2)*2;
      end;
     end;
   else
     break;
   end;
 FMemSeg:=ASeg;
 FAddr:=ASeg.HwBase+ASeg.FillSize;
 if FUsed then ASeg.FillSize:=ASeg.FillSize+Length(FCodeBin);
 until TRUE;
End;

Procedure TAsmFlowLineWA.UpdateRefs ( Const AInternList, AExternList : TAsmRefList; Var AIsAddrOor : boolean );
Var
  BRefIdx       : Integer;
  BRefA,
  BRefB         : TAsmRef;
  BReadS,
  BRefName,
  BOpcode,
  BConstS       : string;
  BDstAddr      : Cardinal;
  BConst        : Integer;
  BJmpDelta     : Int64;
Begin
 FIsAddrOor:=FALSE;
 for BRefIdx:=0 to Length(FRefList)-1 do
  begin
  BRefA:=FRefList[BRefIdx];
  BRefName:=BRefA.Name;
  BReadS:=BRefA.Param.Name;
  ReadParamStr(BReadS);
  BOpcode:=ReadParamStr(BReadS);
  BConstS:=ReadParamStr(BReadS); BConst:=0;
  DelFirstSpace(BReadS);
  if BReadS<>'' then BRefA.AppendError('e','Extra parameter in line [R:TAsmFlowLineWA.UpdateRefs]');
  if (BOpcode<>'') and (BConstS='') then BRefA.AppendError('e','Constant is missing after operation code [R:TAsmFlowLineWA.UpdateRefs]');
  if BOpcode<>'' then
   begin
   if StrInList(BOpcode,'div shr shl add sub or and')=FALSE then BRefA.AppendError('e','Invalid operation '+BOpcode+' (only "div shr shl add sub or and" are allowed) [R:TAsmFlowLineWA.UpdateRefs]');
   if TryStrToInt(BConstS,BConst)=FALSE then BRefA.AppendError('e','Invalid constant '+BConstS+' [R:TAsmFlowLineWA.UpdateRefs]');
   end;
  repeat
  if BRefName='.' then BDstAddr:=Addr
  else
   begin
   BRefB:=RefListSearch(AInternList,BRefName);
   if BRefB=nil then BRefB:=RefListSearch(AExternList,BRefName);
   if BRefB=nil then BRefB:=RefListSearch(FAsmBase.HiddenLine.RefList,BRefName);
   if BRefB=nil then begin BRefA.AppendError('e','Reference '+BRefName+' not found [R:TAsmFlowLineWA.UpdateRefs]'); break; end;
   BDstAddr:=BRefB.ObjectAddr;
   end;
  if BOpcode='' then
  else if BOpcode='div' then
   begin
   if BConst=0 then BRefA.AppendError('e','Cannot divide by zero [R:TAsmFlowLineWA.UpdateRefs]')
   else BDstAddr:=BDstAddr div BConst;
   end
  else if BOpcode='shl' then BDstAddr:=BDstAddr shl BConst
  else if BOpcode='shr' then BDstAddr:=BDstAddr shr BConst
  else if BOpcode='add' then BDstAddr:=BDstAddr + BConst
  else if BOpcode='sub' then BDstAddr:=BDstAddr - BConst
  else if BOpcode='or'  then BDstAddr:=BDstAddr or  BConst
  else if BOpcode='and' then BDstAddr:=BDstAddr and BConst;
  BDstAddr:=BDstAddr+BRefA.FieldAddr;
  case BRefA.RefType of
    'j' : begin
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>255) or (BJmpDelta<-255) then
           begin
           AIsAddrOor:=TRUE;
           FIsAddrOor:=TRUE;
           //BRefA.AppendError('e','Relative label out of range (jmp offset: '+IntToStr(BJmpDelta)+')[R:TAsmFlowLineWA.UpdateRefs]');
           end;
          FCodeBin[1+BRefA.CodeBinPos]:=Chr(Ord(FCodeBin[1+BRefA.CodeBinPos]) or ((BJmpDelta and $F) shl 4));
          FCodeBin[2+BRefA.CodeBinPos]:=Chr(Ord(FCodeBin[2+BRefA.CodeBinPos]) or ((BJmpDelta and $F0) shr 4));
          if BJmpDelta<0 then FCodeBin[2+BRefA.CodeBinPos]:=Chr(Ord(FCodeBin[2+BRefA.CodeBinPos]) or $10);
          end;
    'l' : begin
          BJmpDelta:=(BDstAddr-FAddr) div 2;
          if (BJmpDelta>$7FFF) or (BJmpDelta<-$8000) then
           begin
           AIsAddrOor:=TRUE;
           FIsAddrOor:=TRUE;
           //BRefA.AppendError('e','Relative label out of range (jmp offset: '+IntToStr(BJmpDelta)+')[R:TAsmFlowLineWA.UpdateRefs]');
           end;
          if (BRefA.CodeBinPos+2)>Length(FCodeBin) then
           BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineWA.UpdateRefs]')
          else AddRefDW(FCodeBin,BRefA.CodeBinPos,Cardinal(BJmpDelta));
          end;
    'b' : BRefA.AppendError('e','Linker cannot assign an external 32bit address to a 8bit reference [R:TAsmFlowLineWA.UpdateRefs]');
    'w' : BRefA.AppendError('e','Linker cannot assign an external 32bit address to a 16bit reference [R:TAsmFlowLineWA.UpdateRefs]');
    'd' : begin // Same as 'R' ?
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then
           BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineWA.UpdateRefs]')
          else AddRefDD(FCodeBin,BRefA.CodeBinPos,BDstAddr);
          end;
    'c' : begin
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then
           BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineWA.UpdateRefs]')
          else AddRefDD(FCodeBin,BRefA.CodeBinPos,BDstAddr shr 1);
          end;
    'r' : begin
          if BDstAddr>$7FFF then
           begin
           AIsAddrOor:=TRUE;
           FIsAddrOor:=TRUE;
           end
          else
           begin
           if (BRefA.CodeBinPos+2)>Length(FCodeBin) then
            BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineWA.UpdateRefs]')
           else AddRefDW(FCodeBin,BRefA.CodeBinPos,BDstAddr);
           end;
          end;
    'R' : begin
          if (BRefA.CodeBinPos+4)>Length(FCodeBin) then
           BRefA.AppendError('e','Internal error: update reference goes out of binary length [R:TAsmFlowLineWA.UpdateRefs]')
          else AddRefDD(FCodeBin,BRefA.CodeBinPos,BDstAddr);
          end;
     else begin
          BRefA.AppendError('e','Internal error: invalid reference type #'+IntToStr(Ord(BRefA.RefType))+' [R:TAsmFlowLineWA.UpdateRefs]')
          end;
    end; // case
  until TRUE;
  end;
End;

Function TAsmFlowLineWA.InvJmpA ( Const AJmp : string ) : string;
Begin
 Result:='jmp_invalid';
 if AJmp='be' then Result:='bne'
 else if AJmp='bne' then Result:='be'
 else if AJmp='ba' then Result:='bbe'
 else if AJmp='bb' then Result:='bae'
 else if AJmp='bae' then Result:='bb'
 else if AJmp='bbe' then Result:='ba'
 else if AJmp='bg' then Result:='bse'
 else if AJmp='bs' then Result:='bge'
 else if AJmp='bge' then Result:='bs'
 else if AJmp='bse' then Result:='bg'
 else AppendError('e','Internal error: cannot invert JMP condition '+AJmp+' [R:TAsmFlowLineWA.InvJmp]');
End;

{ *** TAsmHelperWA *** }

Constructor TAsmHelperWA.Create;
Begin
 Inherited;
 FSymComment:=';';
 FSymMultiline:=#0;
End;

Destructor TAsmHelperWA.Destroy;
Begin
 Inherited;
End;

Function TAsmHelperWA.NewFlowLine : TAsmFlowLine;
Begin
 Result:=TAsmFlowLineWA.Create;
End;

Procedure TAsmHelperWA.CompileLine ( ALine : TAsmFlowLine );
Var
  BRef          : TAsmRef;
Begin
 repeat
 case ALine.CmdIs of
   acPublic:
     begin
     BRef:=RefListSearch(FPublicList,ALine.Params[1].Name);
     if BRef<>nil then ALine.AppendError('e',ALine.Params[1],'Declaration with this name already exists [R:TAsmHelperWA.Compile]');
     RefListAppend(FPublicList,ALine.Params[1],ALine.Params[1].Name);
     end;
   acExtern:
     begin
     BRef:=RefListSearch(FExternList,ALine.Params[1].Name);
     if BRef<>nil then ALine.AppendError('e',ALine.Params[1],'Declaration with this name already exists [R:TAsmHelperWA.Compile]');
     RefListAppend(FExternList,ALine.Params[1],ALine.Params[1].Name);
     end;
   acLabel:
     begin
     BRef:=RefListSearch(FLabelList,ALine.LabelName);
     if BRef<>nil then ALine.AppendError('e',ALine.Params[0],'Label already exists [R:TAsmHelperWA.Compile]');
     RefListAppend(FLabelList,ALine.Params[0],ALine.LabelName);
     end;
   acEqu:
     begin
     if FEquList.Values[ALine.Params[0].Name]<>'' then ALine.AppendError('e',ALine.Params[0],'Duplicated EQU [R:TAsmHelperWA.Compile]')
     else FEquList.Values[ALine.Params[0].Name]:=ALine.Params[2].Name;
     end;
   acData:
     begin
     if ALine.Align=1 then CodeGenDB(ALine)
     else if ALine.Align=2 then CodeGenDW(ALine)
     else if ALine.Align=4 then CodeGenDD(ALine)
     else ALine.AppendError('e',ALine.Params[0],'Internal error: Invalid align [R:TAsmHelperWA.Compile]');
     end;
   acStack:
     begin
     end;
   acCmd:
     begin
     CodeGenCmd(ALine);
     end;
   end; // case
 until TRUE;
End;

Procedure TAsmHelperWA.Compile;
Var
  BLineCnt,
  BLineIdx      : Integer;
  BPath         : string;
Begin
 Inherited;

 repeat
 if FParser=nil then
  begin
  FAsmName:=FSrcName;
  FAsmSrc.Assign(FTextSrc);
  end
 else
  begin
  if FModule=nil then begin AppendErrorA('e',0,0,FSrcName,'Internal error: Parser is defined but module is not [R:TAsmHelperWA.Compile]'); break; end;
  FModule.OnAppendError:=@AppendErrorA;
  FModule.Init(FSrcName,FPrjPath,FDstPath,FDefCodeSeg,FDefDataSeg);
  FParser.Parse(FModule,FALSE); if FModule.GetErrorCountA<>0 then break;
  FModule.Compile; if FModule.GetErrorCountA<>0 then break;
  FModule.GenAsmSrc(FAsmSrc); if FModule.GetErrorCountA<>0 then break;
  FAsmName:=AssembleFullName(FDstPath,FModule.Name,'asm');
  BPath:=ExtractFilePath(FAsmName);
  if DirectoryExists(BPath)=FALSE then
   begin
   if ForceDirectories(BPath)=FALSE then AppendErrorA('e',0,0,FSrcName,'Directory '+BPath+' does not exists and program is not able to create it');
   end;
  try
   FAsmSrc.SaveToFile(FAsmName);
  except
   AppendErrorA('e',0,0,FSrcName,'Cannot save file '+FAsmName);
  end;
  end;

 ParseList(FAsmSrc,FAsmName,'');

 if GetErrorCountA<>0 then break;
 BLineCnt:=Length(FFlowList);
 BLineIdx:=0;
 while BLineIdx<BLineCnt do
  begin
  CompileLine(FFlowList[BLineIdx]);
  inc(BLineIdx);
  end;
 until TRUE;
End;

Procedure TAsmHelperWA.ExtractDupOpti ( ALine : TAsmFlowLine; AParam : TAsmLineParam; Var AReadS : string; Out ADupCnt : Integer );
Var
  BReadS        : string;
  BDupS         : string;
  BDupCnt       : Integer;
Begin
 ADupCnt:=1;
 repeat
 BReadS:=AReadS;
 BDupS:=ReadParamStrInv(BReadS);
 if BDupS='' then break;
 if Pos('dup(',LowerCase(BDupS))<>1 then break;
 if BDupS[Length(BDupS)]<>')' then break;
 AReadS:=BReadS;
 Delete(BDupS,1,4);
 Delete(BDupS,Length(BDupS),1);
 if TryStrToInt(BDupS,BDupCnt)=FALSE then begin ALine.AppendError('e',AParam,'Cannot convert dup constant to integer [R:TAsmHelperWA.ExtractDupOpti]'); break; end;
 if BDupCnt>65536 then begin ALine.AppendError('e',AParam,'Dup counter is too big (65536 max) [R:TAsmHelperWA.ExtractDupOpti]'); break; end;
 if BDupCnt<1 then begin ALine.AppendError('e',AParam,'Dup counter is too small [R:TAsmHelperWA.ExtractDupOpti]'); break; end;
 ADupCnt:=BDupCnt;
 until TRUE;
End;

Procedure TAsmHelperWA.CodeGenDB ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-1 do ALine.AppendDataBinB(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>255 then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperWA.CodeGenB]')
   else if BDataI<-128 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperWA.CodeGenB]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinB(Byte(BDataI));
   end
  else
   begin
   ALine.AppendError('e',BParam,'Reference (if this is a reference) is too big for this data type [R:TAsmHelperWA.CodeGenB]');
   end;
  end;
End;

Procedure TAsmHelperWA.CodeGenDW ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-2 do ALine.AppendDataBinW(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>65535 then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperWA.CodeGenB]')
   else if BDataI<-32768 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperWA.CodeGenB]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinW(Word(BDataI));
   end
  else
   begin
   ALine.AppendError('e',BParam,'Reference (if this is a reference) is too big for this data type [R:TAsmHelperWA.CodeGenB]');
   end;
  end;
End;

Procedure TAsmHelperWA.CodeGenDD ( ALine : TAsmFlowLine );
Var
  BParamIdx     : Integer;
  BParam        : TAsmLineParam;
  BDataS        : string;
  BStrIdx       : Integer;
  BDataI        : Int64;
  BDataFS       : Single;
  BDataFD       : Double;
  BDupIdx,
  BDupCnt       : Integer;
Begin
 ALine.ClearDataBin;
 for BParamIdx:=1 to Length(ALine.Params)-1 do
  begin
  BParam:=ALine.Params[BParamIdx];
  BDataS:=BParam.Name;
  ExtractDupOpti(ALine,BParam,BDataS,BDupCnt);
  if (BDataS[1]=#39) and (Length(BDataS)>2) and (BDataS[Length(BDataS)]=#39) then
   begin
   for BDupIdx:=1 to BDupCnt do begin for BStrIdx:=2 to Length(BDataS)-2 do ALine.AppendDataBinD(Ord(BDataS[BStrIdx])); end;
   end
  else if IsIntegerEqu(BDataS,BDataI) then
   begin
   if BDataI>$FFFFFFFF then ALine.AppendError('e',BParam,'Constant is too big for this data type [R:TAsmHelperWA.CodeGenB]')
   else if BDataI<-$80000000 then ALine.AppendError('e',BParam,'Constant is too small for this data type [R:TAsmHelperWA.CodeGenB]');
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinD(Cardinal(BDataI));
   end
  else if IsFloatEqu(BDataS,BDataFD) then
   begin
   BDataFS:=BDataFD;
   for BDupIdx:=1 to BDupCnt do ALine.AppendDataBinD(Cardinal(BDataFS));
   end
  else
   begin
   ALine.AppendDataRefD(BParam);
   end;
  end;
End;

Function TAsmHelperWA.IsPointer ( Const AStr : string; Out ALabel : string ) : boolean;
Var
  BLen  : integer;
Begin
 Result:=FALSE;
 ALabel:='';
 repeat
 BLen:=Length(AStr);
 if BLen<2 then break;
 if (AStr[1]='[') and (AStr[BLen]=']') then
 else break;
 ALabel:=Copy(AStr,2,BLen-2);
 Result:=TRUE;
 until TRUE;
End;

Procedure TAsmHelperWA.CorrectAddrOor;
Var
  BLineIdx      : Integer;
  BLine         : TAsmFlowLine;
  BRef          : TAsmRef;
  BCodeBin      : string;
Begin
 BLineIdx:=0;
 while BLineIdx<Length(FFlowList) do
  begin
  BLine:=FFlowList[BLineIdx];
  repeat
  if BLine.IsAddrOor=FALSE then break;
  BRef:=BLine.RefList[0];
  if BRef=nil then begin BLine.AppendError('e','Internal error: ref is nil [R:TAsmHelperWA.CorrectAddrOor]'); break; end;
  case BRef.RefType of
   'j': begin // Becomes 'l'
        BRef.RefType:='l'; BRef.CodeBinPos:=2;
        // ONS    |110     ||   djjjj|jjjjffff|                 | bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv
        // ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
        BCodeBin:=BLine.CodeBin;
        if Length(BCodeBin)<2 then begin BLine.AppendError('e','Internal error: Size of codebin is wrong [R:TAsmHelperWA.CorrectAddrOor]'); break; end;
        BCodeBin[2]:=Chr($B0); BCodeBin[1]:=Chr(Ord(BCodeBin[1]) and $0F);
        BLine.CodeBin:=BCodeBin;
        BLine.AppendDataBinW(0);
        SaveCodeBin;
        end;
   'l': begin // Becomes 'd'
        BRef.RefType:='d';
        // ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU
        BCodeBin:=BLine.CodeBin;
        if Length(BCodeBin)<4 then begin BLine.AppendError('e','Internal error: Size of codebin is wrong [R:TAsmHelperWA.CorrectAddrOor]'); break; end;
        BCodeBin[2]:=Chr(Ord(BCodeBin[2]) or $04);
        BLine.CodeBin:=BCodeBin;
        BLine.AppendDataBinW(0);
        SaveCodeBin;
        end;
   'r': begin // Becomes 'R'
        BRef.RefType:='R';
        BCodeBin:=BLine.CodeBin;
        if Length(BCodeBin)<4 then begin BLine.AppendError('e','Internal error: Size of codebin is wrong [R:TAsmHelperWA.CorrectAddrOor]'); break; end;
        BCodeBin[2]:=Chr(Ord(BCodeBin[2]) or $04);
        BLine.CodeBin:=BCodeBin;
        // MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
        BLine.AppendDataBinW(0);
        SaveCodeBin;
        end;
   else begin
        BLine.AppendError('e','Internal error: Ref cannot be changed anymore [R:TAsmHelperWA.CorrectAddrOor]');
        break;
        end;
  end; // case
  until TRUE;
  inc(BLineIdx);
  end;
End;


end.

