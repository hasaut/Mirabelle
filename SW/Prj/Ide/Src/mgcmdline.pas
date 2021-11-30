unit MgCmdLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TStartupMode = (smRun, smStep, smExit);

  TCmdParams = class(TObject)
  private
    FCmdLine    : string;
    FLastError  : string;
    FPrjParams  : TStringList;

    FWrHelp         : boolean;
    FWrVersion      : boolean;
    FPrjFilename    : string;
    FIsSilent       : boolean;
    FStartupMode    : TStartupMode;
    FMaxAddr        : Cardinal;
    FMaxTime        : Cardinal;
    FOutMemDump,
    FOutTty         : string;
    FFlashList      : string;

    Function FilterItem ( Const AItemName : string; Const AReadSL : string; Var ACmdLine : string ) : boolean;
    Function FilterItems ( Const ACmdLineItems : string; Const AReadSL : string; Var ACmdLine : string ) : boolean;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function Parse ( Const ACmdLineItems : string ) : boolean;
    Procedure ExpandPrjParams ( APrjParams : TStringList );

    property LastError : string read FLastError;

    property WrHelp      : boolean  read FWrHelp;
    property WrVersion   : boolean  read FWrVersion;
    property PrjFilename : string   read FPrjFilename;
    property IsSilent    : boolean read FIsSilent;
    property StartupMode : TStartupMode read FStartupMode write FStartupMode;
    property MaxTime     : Cardinal read FMaxTime;
    property OutMemDump  : string read FOutMemDump;
    property OutTty      : string read FOutTty;
    property FlashList   : string read FFlashList;
  end;

implementation

Uses
  ConComL;

{ *** TCmdParams *** }

Constructor TCmdParams.Create;
Begin
 Inherited;
 FPrjParams:=TStringList.Create;
End;

Destructor TCmdParams.Destroy;
Begin
 FPrjParams.Free;
 Inherited;
End;

Function TCmdParams.FilterItem ( Const AItemName : string; Const AReadSL : string; Var ACmdLine : string ) : boolean;
Var
  BItemNameL    : string;
  BParam        : string;
Begin
 Result:=FALSE;
 repeat
 BItemNameL:=LowerCase(AItemName);
 if ('--'+BItemNameL)<>AReadSL then break;
 BParam:=ReadParamStr(ACmdLine);
 FPrjParams.Append(AItemName+' '+BParam);
 Result:=TRUE;
 until TRUE;
End;

Function TCmdParams.FilterItems ( Const ACmdLineItems : string; Const AReadSL : string; Var ACmdLine : string ) : boolean;
Var
  BItems    : string;
  BItemA    : string;
Begin
 Result:=FALSE;
 BItems:=ACmdLineItems;
 repeat
 BItemA:=ReadParamStr(BItems);
 if BItemA='' then break;
 if FilterItem(BItemA,AReadSL,ACmdLine) then begin Result:=TRUE; break; end;
 until FALSE;
End;

Function TCmdParams.Parse ( Const ACmdLineItems : string ) : boolean;
Var
  BParamIdx     : Integer;
  BCmdLine      : string;
  BReadS,
  BReadSL       : string;
  BStartupS,
  BMaxAddrS,
  BMaxTimeS     : string;
  BDummyS       : string;
Begin
 Result:=FALSE;

 FCmdLine:=''; FLastError:=''; FPrjParams.Clear;

 BParamIdx:=1;
 while BParamIdx<=ParamCount do begin FCmdLine:=FCmdLine+ParamStr(BParamIdx)+' '; inc(BParamIdx); end;

 FWrHelp:=FCmdLine=''; FWrVersion:=FALSE;
 FPrjFilename:=''; FIsSilent:=FALSE;

 FStartupMode:=smRun; FMaxAddr:=0; FMaxTime:=0; FOutMemDump:=''; FOutTty:=''; FFlashList:='';

 BStartupS:=''; BMaxAddrS:=''; BMaxTimeS:='';

 repeat
 BCmdLine:=FCmdLine;
 repeat
 BReadS:=ReadParamStr(BCmdLine); BReadSL:=LowerCase(BReadS); if BReadS='' then break;
 if (BReadSL='-h') or (BReadSL='-help') or (BReadSL='--help') then FWrHelp:=TRUE
 else if (BReadSL='-version') or (BReadSL='--version') then FWrVersion:=TRUE
 else if (BReadSL='-s') then FIsSilent:=TRUE
 else if BReadSL='--projectfile' then
  begin
  if FPrjFilename<>'' then begin FLastError:='Project file can be specified only once'; break; end;
  FPrjFilename:=ExpandFilename(ReadParamStr(BCmdLine));
  end
 else if FilterItems(ACmdLineItems,BReadSL,BCmdLine) then
 else if BReadSL='--startup' then
  begin
  if BStartupS<>'' then begin FLastError:='Startup mode can be specified only once'; break; end;
  BStartupS:=LowerCase(ReadParamStr(BCmdLine));
  end
 else if BReadSL='--maxaddr' then
  begin
  if BMaxAddrS<>'' then begin FLastError:='MaxAddr can be specified only once'; break; end;
  BMaxAddrS:=LowerCase(ReadParamStr(BCmdLine));
  end
 else if BReadSL='--maxtime' then
  begin
  if BMaxTimeS<>'' then begin FLastError:='MaxTime can be specified only once'; break; end;
  BMaxTimeS:=LowerCase(ReadParamStr(BCmdLine));
  end
 else if BReadSL='--outmemdump' then
  begin
  if FOutMemDump<>'' then begin FLastError:='Output mem dump can be specified only once'; break; end;
  FOutMemDump:=ReadParamStr(BCmdLine);
  end
 else if BReadSL='--outtty' then
  begin
  if FOutTty<>'' then begin FLastError:='Output TTY can be specified only once'; break; end;
  FOutTty:=ReadParamStr(BCmdLine);
  end
 else if BReadSL='--flash' then
  begin
  if FFlashList<>'' then FFlashList:=FFlashList+'; ';
  BDummyS:=ReadParamStr(BCmdLine); if BDummyS='' then begin FLastError:='Flash address is not specified'; break; end; FFlashList:=FFlashList+BDummyS+' ';
  BDummyS:=ReadParamStr(BCmdLine); if BDummyS='' then begin FLastError:='Flash size is not specified'; break; end; FFlashList:=FFlashList+BDummyS+' ';
  BDummyS:=ReadParamStr(BCmdLine); if BDummyS='' then begin FLastError:='Flash file is not specified'; break; end; FFlashList:=FFlashList+BDummyS;
  end
 else begin FLastError:='Parameter string "'+BReadS+'" is not recognized'; break; end;
 until FALSE;

 if FLastError<>'' then break;

 if BStartupS='step' then FStartupMode:=smStep
 else if BStartupS='run' then FStartupMode:=smRun
 else if BStartupS='exit' then FStartupMode:=smExit;

 if BMaxAddrS<>'' then
  begin
  if TryStrToInt0x(BMaxAddrS,FMaxAddr)=FALSE then begin FLastError:='Cannot convert MaxAddr to integer'; break; end;
  end;

 if BMaxTimeS<>'' then
  begin
  if TryStrToInt0x(BMaxTimeS,FMaxTime)=FALSE then begin FLastError:='Cannot convert MaxTime to integer'; break; end;
  end;

 Result:=TRUE;
 until TRUE;
End;

Procedure TCmdParams.ExpandPrjParams ( APrjParams : TStringList );
Var
  BLineIdx  : Integer;
Begin
 BLineIdx:=0;
 while BLineIdx<FPrjParams.Count do
  begin
  APrjParams.Append(FPrjParams.Strings[BLineIdx]);
  inc(BLineIdx);
  end;
End;

end.

