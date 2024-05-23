unit ElfFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsmTypes_sd, ClassList;

Const
  SHT_NULL           = $00; // Section header table entry unused;
  SHT_PROGBITS	     = $01; // Program data
  SHT_SYMTAB	     = $02; // Symbol table
  SHT_STRTAB	     = $03; // String table
  SHT_RELA	     = $04; // Relocation entries with addends
  SHT_HASH	     = $05; // Symbol hash table
  SHT_DYNAMIC	     = $06; // Dynamic linking information
  SHT_NOTE	     = $07; // Notes
  SHT_NOBITS	     = $08; // Program space with no data (bss)
  SHT_REL	     = $09; // Relocation entries, no addends
  SHT_SHLIB	     = $0A; // Reserved
  SHT_DYNSYM	     = $0B; // Dynamic linker symbol table
  SHT_INIT_ARRAY     = $0E; // Array of constructors
  SHT_FINI_ARRAY     = $0F; // Array of destructors
  SHT_PREINIT_ARRAY  = $10; // Array of pre-constructors
  SHT_GROUP	     = $11; // Section group
  SHT_SYMTAB_SHNDX   = $12; // Extended section indices
  SHT_NUM	     = $13; // Number of defined types.

  PT_NULL    = 0;
  PT_LOAD    = 1;
  PT_DYNAMIC = 2;
  PT_INTERP  = 3;
  PT_NOTE    = 4;
  PT_SHLIB   = 5;
  PT_PHDR    = 6;
  PT_TLS     = 7;
  PT_LOOS    = $60000000;
  PT_HIOS    = $6fffffff;
  PT_LOPROC  = $70000000;
  PT_HIPROC  = $7fffffff;

Type
  TAddr = Cardinal;
  TElfFile = class;

  TOnSetDbgParams  = Procedure ( AAddrB, AAddrE : TAddr; Const ASrcFile : string; ALine, APos : Cardinal ) of Object;
  //TOnPropDbgParams = Procedure ( AAddrB, AAddrE : TAddr; Const ASrcFile : string; ALine, APos : Cardinal ) of Object;

  TElfHdrBase = record
    FMagicNr     : Cardinal;
    FDataType    : byte;
    FEndian      : byte;
    FHrdVersion  : byte;
    FAbi         : byte;
    FPadding     : array [0..7] of byte;
    FObjFileType : word;
    FIsa         : word;
    FElfVersion  : array[0..3] of byte;
  end;

  TElfHdr32 = record
    FProgramEntry       : Cardinal;
    FProgHdrTablePos    : Cardinal;
    FSectHdrTablePos    : Cardinal;
  end;

  TElfHdr64 = record
    FProgramEntry       : QWord;
    FProgHdrTablePos    : QWord;
    FSectHdrTablePos    : QWord;
  end;

  TElfHdrTail = record
    FFlags              : Cardinal; // 36-39	48-51	Flags - architecture dependent; see note below
    FHdrSize            : Word;     // 40-41	52-53	Header size
    FProgEntrySize      : Word;     // 42-43	54-55	Size of an entry in the program header table
    FProgEntryNr        : Word;     // 44-45	56-57	Number of entries in the program header table
    FSectEntrySize      : Word;     // 46-47	58-59	Size of an entry in the section header table
    FSectEntryNr        : Word;     // 48-49	60-61	Number of entries in the section header table
    FNameIdx            : Word;     // 50-51	62-63	Index in section header table with the section names
  end;

  TElfProgHdr32 = record
    FType               : Cardinal;
    FOffset             : Cardinal;
    FAddrV              : Cardinal;
    FAddrP              : Cardinal;
    FFileSZ             : Cardinal;
    FMemSZ              : Cardinal;
    FFlags              : Cardinal;
    FAlign              : Cardinal;
  end;

  TElfProgHdr64 = record
    FType               : Cardinal;
    FFlags              : Cardinal;
    FOffset             : QWord;
    FAddrV              : QWord;
    FAddrP              : QWord;
    FFileSZ             : QWord;
    FMemSZ              : QWord;
    FAlign              : QWord;
  end;

  TElfSectHdr32 = record
    FName               : Cardinal;
    FType               : Cardinal;
    FFlags              : Cardinal;
    FAddr               : Cardinal;
    FOffset             : Cardinal;
    FSize               : Cardinal;
    FLink               : Cardinal;
    FInfo               : Cardinal;
    FAddrAlign          : Cardinal;
    FEntSize            : Cardinal;
  end;

  TElfSectHdr64 = record
    FName               : Cardinal;
    FType               : Cardinal;
    FFlags              : QWord;
    FAddr               : QWord;
    FOffset             : QWord;
    FSize               : QWord;
    FLink               : Cardinal;
    FInfo               : Cardinal;
    FAddrAlign          : QWord;
    FEntSize            : QWord;
  end;

  TElfSym32 = record
    FStName    : Cardinal;
    FStValue   : Cardinal;
    FStSize    : Cardinal;
    FStInfo    : Byte;
    FStOther   : Byte;
    FStShndx   : Word;
  end;

  TElfSym64 = record
    FStName    : Cardinal;
    FStInfo    : byte;
    FStOther   : byte;
    FStShndx   : word;
    FStValue   : QWord;
    FStSize    : QWord;
  end;

  TCuHeader32 = record
   FDataLen               : Cardinal;
   FVersion               : word;
   FHeaderLength          : Cardinal;
   FMminInstructionLength : byte;
   FDefaultIsStmt         : byte;
   FLineBase              : byte;
   FLineRange             : byte;
   FOpcodeBase            : byte;
   FStdOpcodeLen          : array [0..11] of byte;
  end;

  TCuHeader64 = record
   FDataLen               : QWord;
   FVersion               : Word;
   FHeaderLength          : QWord;
   FMminInstructionLength : byte;
   FDefaultIsStmt         : byte;
   FLineBase              : byte;
   FLineRange             : byte;
   FOpcodeBase            : byte;
   FStdOpcodeLen          : array [0..11] of byte;
  end;

  TLocInfoItem = record
   FLine,
   FPos     : Cardinal;
   FSrcFile : TIndex;
  end;

  TLocInfoList = array of TLocInfoItem;

  TElfProgItem = class(TObject)
  private
    FProgType   : Cardinal;
    FFlags      : Cardinal;
    FOffset     : QWord;
    FAddrV      : QWord;
    FAddrP      : QWord;
    FFileSZ     : QWord;
    FMemSZ      : QWord;
    FAlign      : QWord;

    FRawData    : PByte;

    FOnViewAny  : TOnViewAny;
    FLocInfo    : TLocInfoList;

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );
    Procedure SetSizes;
   public
     Constructor Create ( AOnViewAny : TOnViewAny; ARawData : PByte ); Virtual;
     Destructor Destroy; Override;

     Procedure Fill ( Const AProgHdr : TElfProgHdr32 );
     Procedure Fill ( Const AProgHdr : TElfProgHdr64 );

     Function IsLoadable : boolean;
     Function IsInside ( AAddr : TAddr ) : boolean;
     Function TrySetDbgParams ( AAddr : TAddr; AFileIdx : TIndex; ALine, APos : Cardinal ) : boolean;
     Function TryGetDbgParams ( AAddr : TAddr; Out AFileIdx : TIndex; Out ALine, APos : Cardinal ) : boolean;

     property AddrP : QWord read FAddrP;
     property FileSZ : QWord read FFileSZ;
     property RawData : PByte read FRawData;
   end;

  TElfSectItem = class(TObject)
  private
    FNameIdx            : Cardinal;
    FSectType           : Cardinal;
    FFlags              : QWord;
    FAddr               : QWord;
    FOffset             : QWord;
    FSize               : QWord;
    FLink               : Cardinal;
    FInfo               : Cardinal;
    FAddrAlign          : QWord;
    FEntSize            : QWord;

    FNameS      : string;
    FRawData    : PByte;
    FOnViewAny  : TOnViewAny;

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );
  public
    Constructor Create ( AOnViewAny : TOnViewAny ); Virtual;
    Destructor Destroy; Override;
    Procedure Fill ( Const ASectHdr : TElfSectHdr32 );
    Procedure Fill ( Const ASectHdr : TElfSectHdr64 );

    Function RdStrA ( AOffset : QWord ) : string;
    Function RdStr ( Var AOffset : QWord; Out AData : string ) : boolean;
    Function RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
    Function RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
    Function RdDataB ( Var AOffset : QWord; Out AData : byte ) : boolean;
    Function RdDataX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;

    property NameIdx : Cardinal read FNameIdx;
    property Offset : QWord read FOffset;
    property Size : QWord read FSize;
    property SectType : Cardinal read FSectType;
    property RawData : PByte read FRawData write FRawData;

    property NameS : string read FNameS write FNameS;
  end;

  TElfSymItem = class(TObject)
  private
    FName     : Cardinal;
    FInfo     : byte;
    FOther    : byte;
    FShndx    : Word;
    FValue    : QWord;
    FSize     : QWord;

    FSymType   : byte;
    FSymBind   : byte;
    FNameS     : string;

    Procedure SetInfo ( AInfo : byte );

  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Fill ( Const ARdItem : TElfSym32 );
    Procedure Fill ( Const ARdItem : TElfSym64 );

    property NameOffset : Cardinal read FName;
    property NameS : string read FNameS write FNameS;
    property Value : QWord read FValue;
    property SymType : byte read FSymType;
    property SymBind : byte read FSymBind;
  end;

  TDebugAbbrevParam = record
    FAttribute  : word;
    FForm       : word;
    FData       : Int64;
  end;

  TDebugAbbrevParamList = specialize TVector<TDebugAbbrevParam>;

  TDebugAbbrevItem = class(TObject)
  private
    FAbbrevSect         : TElfSectItem;
    FOnViewAny          : TOnViewAny;

    FNumber             : Cardinal;
    FTag                : Cardinal;
    FHasChildren        : boolean;
    FParamList          : TDebugAbbrevParamList;

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );

  public
    Constructor Create ( AOnViewAny : TOnViewAny; AAbbrevSect : TElfSectItem; ANumber : Cardinal ); Virtual;
    Destructor Destroy; Override;
    Function Parse ( Var ARdIdx : QWord ) : boolean;

    property Number : Cardinal read FNumber;
    property Tag : Cardinal read FTag;
    property HasChildren : boolean read FHasChildren;
    property ParamList : TDebugAbbrevParamList read FParamList;
  end;

  TDebugAbbrevList = specialize TClassList<TDebugAbbrevItem>;
  TByteVect = specialize TVector<byte>;

  TDebugInfoParam = class(TObject)
  private
    FAbbrev     : TDebugAbbrevParam;
    FDataS      : string;    // DataS = 0
    FDataA      : TByteVect; // DataA = 1
    FDataU      : QWord;     // DataU RefU RefAddr Flag Addr DataS
    FDataType   : byte;      //     2    3       4    5    6     7
  public
    Constructor Create ( AAbbrev : TDebugAbbrevParam ); Virtual;
    Destructor Destroy; Override;

    property Abbrev : TDebugAbbrevParam read FAbbrev;

    property DataType : byte read FDataType;
    property DataU : QWord read FDataU;
    property DataS : string read FDataS;
    property DataA : TByteVect read FDataA;

    Procedure SetDataS ( Const ADataS : string );
    Procedure ClearDataA;
    Procedure AppendDataA ( AData : byte );
    Procedure SetDataU ( AData : QWord );
    Procedure SetDataI ( AData : Int64 );
    Procedure SetRefU ( AData : QWord );
    Procedure SetRefAddr ( AData : QWord );
    Procedure SetOffsetU ( AData : QWord );
    Procedure SetFlag ( AFlag : byte );
    Procedure SetFlagPresent;
    Procedure SetAddr ( AAddr : QWord );

    Function Verbose : string;
  end;

  TDebugInfoItem = class;
  TDebugInfoList = specialize TClassList<TDebugInfoItem>;
  TDebugInfoParamList = specialize TClassList<TDebugInfoParam>;

  TDebugInfoItem = class(TObject)
  private
    FAbbrev     : TDebugAbbrevItem;
    FParent     : TDebugInfoItem;
    FChildList  : TDebugInfoList;
    FParamList  : TDebugInfoParamList;

  public
    Constructor Create ( AAbbrev : TDebugAbbrevItem ); Virtual;
    Destructor Destroy; Override;

    Function FindParam ( AAttribute : word ) : TDebugInfoParam;

    property Abbrev : TDebugAbbrevItem read FAbbrev;
    property Parent : TDebugInfoItem read FParent write FParent;
    property ChildList : TDebugInfoList read FChildList;
    property ParamList : TDebugInfoParamList read FParamList;
  end;


  TDebugInfoUnit = class(TObject)
  private
    FDebugInfoSect      : TElfSectItem;
    FBase               : QWord;
    FOffsetNextHdr      : QWord;
    FOnViewAny          : TOnViewAny;
    FAbbrevSect,
    FDebugStrSect,
    FDebugLineStrSect   : TElfSectItem;

    FDataLen            : QWord;
    FVersion            : word;
    FUnitType           : byte;
    FDebugAbbrevOffset  : Cardinal;
    FAddressSize        : byte;
    //FTypeSignature      : QWord;
    //FTypeOffset         : Cardinal;

    FAbbrevList         : TDebugAbbrevList;
    FInfoList           : TDebugInfoList;

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );
    Function RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
    Function RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
    Function RdDataB ( Var AOffset : QWord; Out AData : byte ) : boolean;
    Function RdDataX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;

    Function ParseAbbrev : boolean;
    Function ReadDebugInfoParam (  ADebugAbbrevParam : TDebugAbbrevParam; ADebugInfoParam : TDebugInfoParam; Var AOffset : QWord ) : boolean;


  public
    Constructor Create ( AOnViewAny : TOnViewAny; ADebugInfoSect, AAbbrevSect, ADebugStrSect, ADebugLineStrSect : TElfSectItem ); Virtual;
    Destructor Destroy; Override;

    Function Parse ( Var ARdIdx : QWord ) : boolean;

    property InfoList : TDebugInfoList read FInfoList;
  end;

  TElfProc = class(TObject)
  private
    FAddrS      : TAddr;
    FProcName   : string;
  public
    Constructor Create ( AAddr : TAddr; Const AProcName : string ); Virtual;
    Destructor Destroy; Override;

    property AddrS : TAddr read FAddrS;
    property ProcName : string read FProcName;
  end;

  TDecFsm = class(TObject)
  private
    FAddr           : TAddr;
    FOpIndex        : Cardinal;
    FFileName       : string;
    FLine,
    FColumn         : Cardinal;
    FIsStmt         : Byte;
    FBasicBlock,
    FEndSequence,
    FPrologueEnd,
    FEpilogueBegin  : boolean;
    FIsa            : Cardinal;
    FDiscrim        : Integer;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Clear ( AIsStmt : Byte; Const AFileName : string );
    Procedure NegateStmt;
    Procedure Move ( AAddr : TAddr; AOpIdx : Cardinal; ALineInc : Integer );
    Procedure MoveLine ( ALineInc : Integer );

    property Addr : TAddr read FAddr write FAddr;
    property OpIndex : Cardinal read FOpIndex write FOpIndex;
    property FileName : string read FFileName write FFileName;
    property Line : Cardinal read FLine write FLine;
    property Column : Cardinal read FColumn write FColumn;
    property BasicBlock : boolean read FBasicBlock write FBasicBlock;
    property PrologueEnd : boolean read FPrologueEnd write FPrologueEnd;
    property EpilogueBegin : boolean read FEpilogueBegin write FEpilogueBegin;
    property Discrim : Integer read FDiscrim write FDiscrim;

    (*
    ~TDecFsm ( void ) {}
    void Clear ( bool AIsStmt, const std::string &AFileName ) { FAddr=0; FOpIndex=0; FFile=AFileName; FLine=1; FColumn=0;  FIsStmt=AIsStmt; FBasicBlock=false; FEndSequence=false; FPrologueEnd=false; FEpilogueBegin=false; FIsa=0; FDiscrim=0; }
    // Set                                                              Get
    void SetAddr ( TAddr AAddr ) { FAddr=AAddr; }                       TAddr GetAddr ( void ) { return FAddr; }
    void SetOpIndex ( uint32_t AData ) { FOpIndex=AData; }              uint32_t GetOpIndex ( void ) { return FOpIndex; }
    void SetDiscrim ( uint32_t AData ) { FDiscrim=AData; }
    void SetFile ( const std::string &AFileName ) { FFile=AFileName; }   std::string GetFile ( void ) { return FFile; }
                                                                         uint32_t GetLine ( void ) { return FLine; }
    void SetColumn ( uint32_t AColumn ) { FColumn=AColumn; }             uint32_t GetColumn ( void ) { return FColumn; }
    void SetBasicBlock ( bool AData ) { FBasicBlock=AData; }
    void SetPrologueEnd ( bool AData ) { FPrologueEnd=AData; }
    void SetEpilogueBegin ( bool AData ) { FEpilogueBegin=AData; }

    void NegateStmt ( void ) { FIsStmt=!FIsStmt; }


    void Move ( TAddr AAddr, uint32_t AOpIndex, int32_t ALineInc ) { FAddr=AAddr; FOpIndex=AOpIndex; FLine=uint32_t(int32_t(FLine)+ALineInc); FBasicBlock=false; FEndSequence=false; FPrologueEnd=false; FEpilogueBegin=false; }
    void MoveLine ( int32_t ALineInc ) { FLine=uint32_t(int32_t(FLine)+ALineInc); } *)
  end;

  TPathFileFormatItem = record
    FContentType    : Cardinal;
    FFormCode       : Cardinal;
  end;
  TPathFileFormatList = specialize TVector<TPathFileFormatItem>;

  TDebugLineItem = class(TObject)
  private
    FElfFile            : TElfFile;
    FRawData            : PByte;
    FOnViewAny          : TOnViewAny;
    FOnSetDbgParams     : TOnSetDbgParams;

    FSizeBase   : byte;

    FDataLen                : QWord;
    FVersion                : word;
    FAddrSize               : byte;
    FSegmSelector           : byte;
    FHeaderLength           : QWord;
    FMinInstructionLength   : byte;
    FMaxOperPerInstruction  : byte;
    FDefaultIsStmt          : byte;
    FLineBase               : ShortInt;
    FLineRange              : byte;
    FOpcodeBase             : byte;
    FStdOpcodeLen           : array [0..11] of byte;

    FOffsetPathFile,
    FOffsetOpc,
    FOffsetNextHdr  : QWord;

    FPathList,
    FFileList       : TStringList;

    FDecFsm         : TDecFsm;
    FFsmEndSequence : boolean;

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );
    Procedure SetDbgParams ( AAddrB, AAddrE : TAddr );
    //Procedure PropDbgParams ( AAddr : TAddr );
    Function RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
    Function RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
    Function RdProgB ( Var AOffset : QWord; Out AData : byte ) : boolean;
    Function RdProgX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;
    Function RdStrZ ( Var AOffset : QWord; Out AData : string ) : boolean;

    Function Parse2 : boolean;
    Function Parse5 : boolean;
    Function ReadFormatList ( Var AOffset : QWord; AFormatList : TPathFileFormatList ) : boolean;
    Function ReadByFormat ( Var AOffset : QWord; AForm : Cardinal; Out AAnyDataS : string; Out AAnyDataQ : QWord ) : boolean;
    Function ReadPathList5 ( Var AOffset : QWord ) : boolean;
    Function ReadFileList5 ( Var AOffset : QWord ) : boolean;

  public
    Constructor Create ( AElfFile : TElfFile; AOnViewAny : TOnViewAny; ARawData : PByte; AOnSetDbgParams : TOnSetDbgParams; ASizeBase : byte ); Virtual;
    Destructor Destroy; Override;

    Procedure ReadHeader;
    Function Parse : boolean;

    Procedure AppendDbgInfoFile ( Var AFileList : string );
    Function IndexFlowLines : boolean;

    property DataLen : QWord read FDataLen;
    property OffsetNextHdr : QWord read FOffsetNextHdr;
  End;

  TElfProgList = specialize TClassList<TElfProgItem>;
  TElfSectList = specialize TClassList<TElfSectItem>;
  TStrTableList = specialize TClassList<TStringList>;
  TElfSymList = specialize TClassList<TElfSymItem>;
  TDebugLineList = specialize TClassList<TDebugLineItem>;
  TDebugUnitList = specialize TClassList<TDebugInfoUnit>;
  TElfProcList = specialize TClassList<TElfProc>;

  TElfFile = class(TObject)
  private
    FFilename   : string;
    FRawData    : TMemoryStream;
    FOnViewAny,
    FOnViewAnyE : TOnViewAny;
    //FDebugElf   : TStringList;

    LRawDataB   : PByte;

    FElfHdrBase         : TElfHdrBase;
    FProgramEntry       : Cardinal;
    FProgHdrTablePos    : Cardinal;
    FSectHdrTablePos    : Cardinal;
    FElfHdrTail         : TElfHdrTail;

    FProgList           : TElfProgList;
    FSectList           : TElfSectList;
    FStrTableList       : TStrTableList;
    FSymList            : TElfSymList;
    FDebugLineList      : TDebugLineList;
    FDebugUnitList      : TDebugUnitList;

    FElfProcList        : TElfProcList;

    FSectNames          : TElfSectItem;
    FGenStrTab          : TElfSectItem;
    FAbbrevSect         : TElfSectItem;
    FDebugStrSect,
    FDebugLineStrSect   : TElfSectItem;

    FFileList           : TStringList; // Final location, reported in the LST

    Procedure ViewAny ( Const AMessage : string; Const AReporter : string );
    Procedure ViewAnyFilt ( Const AMessage : string );
    Procedure Clear;

    Function RawPtr ( AOffset : QWord ) : PByte;
    Generic Function ParseProgList<TProgHdrX> ( AOffset : Int64; AHdrSize : word; ACount : Cardinal ) : boolean;
    Generic Function ParseSectList<TSectHdrX> ( AOffset : Int64; AHdrSize : word; ACount : Cardinal ) : boolean;
    Generic Function ParseSymList<TSymHdrX> ( AOffset : QWord; ASize : QWord ) : boolean;

    Function FindSectByName ( Const AName : string ) : TElfSectItem;
    Function GetCuHeaderSize : QWord;

    Function PopulateSectData : boolean;
    Function ParseStringTables : boolean;
    Function PopulateSectNames : boolean;
    Function ProcessSymTable : boolean;
    Function ResolveSymNames : boolean;
    Function ProcessDebugAbbrev : boolean;
    Function ProcessDebugStr : boolean;
    Function ProcessDebugLineStr : boolean;
    Function ProcessDebugInfo : boolean;
    Function ParseDbgLines : boolean;
    Function IndexFlowLines : boolean;

    Function IsAddrLoadable ( AAddr : TAddr ) : boolean;
    Function FileListSearchAppend ( Const AFileName : string ) : TIndex;
    Procedure SetDbgParamsA ( AAddr : TAddr; AFileIdx : TIndex; ALine, APos : Cardinal );
    Function GetDbgParamsA ( AAddr : TAddr; Out AFileIdx : TIndex; Out ALine, APos : Cardinal ) : boolean;
    Procedure SetDbgParams ( AAddrB, AAddrE : TAddr; Const ASrcFile : string; ALine, APos : Cardinal );

  public
    Constructor Create ( AOnViewAny : TOnViewAny; Const AFilename : string ); Virtual;
    Destructor Destroy; Override;

    Function Load : boolean;
    Function ParseAll : boolean;
    Procedure MapLocPath ( ALocList : TStringList );

    Procedure ResolveSrc ( AAddr : Cardinal; Var ASrcName : string; Var ASrcLine : Integer );

    Function FindSymName ( AAddr : TAddr; AType : byte ) : string;
    Function FindProcName ( AAddr : TAddr ) : string;
    Function IsProc ( AAddr : TAddr ) : boolean;

    property ProgList : TElfProgList read FProgList;
    property ProgramEntry : Cardinal read FProgramEntry;

    property DebugLineStrSect : TElfSectItem read FDebugLineStrSect;
  end;

implementation

Uses
  ConComL;

Generic Function RdMisaligned<TDataType> ( AData : PByte; Var AOffset : QWord ) : TDataType;
Var
  BIndex    : TIndex;
Begin
 Result:=0;

 BIndex:=0;
 while BIndex<SizeOf(TDataType) do
  begin
  Result:=Result or (QWord(AData[AOffset+BIndex]) << (8*BIndex));
  inc(BIndex);
  end;

 Inc(AOffset,BIndex);
End;

Function RdMisalignedQ ( AData : PByte; Var AOffset : QWord ) : QWord;
Begin
 Result:=Specialize RdMisaligned<QWord>(AData,AOffset);
End;

Function RdMisalignedD ( AData : PByte; Var AOffset : QWord ) : Cardinal;
Begin
 Result:=Specialize RdMisaligned<Cardinal>(AData,AOffset);
End;

Function RdMisalignedW ( AData : PByte; Var AOffset : QWord ) : word;
Begin
 Result:=Specialize RdMisaligned<word>(AData,AOffset);
End;

Function RdMisalignedB ( AData : PByte; Var AOffset : QWord ) : byte;
Begin
 Result:=AData[AOffset];
 inc(AOffset);
End;

// *** TElfProgItem ***

Constructor TElfProgItem.Create ( AOnViewAny : TOnViewAny; ARawData : PByte );
Begin
 Inherited Create;
 FOnViewAny:=AOnViewAny;
 FRawData:=ARawData;
 FLocInfo:=nil;
End;

Destructor TElfProgItem.Destroy;
Begin
End;

Procedure TElfProgItem.Fill ( Const AProgHdr : TElfProgHdr32 );
Begin
 FLocInfo:=nil;
 FProgType:=AProgHdr.FType;
 FFlags:=AProgHdr.FFlags;
 FOffset:=AProgHdr.FOffset;
 FAddrV:=AProgHdr.FAddrV;
 FAddrP:=AProgHdr.FAddrP;
 FFileSZ:=AProgHdr.FFileSZ;
 FMemSZ:=AProgHdr.FMemSZ;
 FAlign:=AProgHdr.FAlign;
 SetSizes;
End;

Procedure TElfProgItem.Fill ( Const AProgHdr : TElfProgHdr64 );
Begin
 FProgType:=AProgHdr.FType;
 FFlags:=AProgHdr.FFlags;
 FOffset:=AProgHdr.FOffset;
 FAddrV:=AProgHdr.FAddrV;
 FAddrP:=AProgHdr.FAddrP;
 FFileSZ:=AProgHdr.FFileSZ;
 FMemSZ:=AProgHdr.FMemSZ;
 FAlign:=AProgHdr.FAlign;
 SetSizes;
End;

Procedure TElfProgItem.SetSizes;
Begin
 FLocInfo:=nil;
 SetLength(FLocInfo,FFileSZ div 2);
End;

Procedure TElfProgItem.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage+' [R:TElfProgItem.'+AReporter+']');
End;

Function TElfProgItem.IsLoadable : boolean;
Begin
 Result:=FProgType=PT_LOAD;
End;

Function TElfProgItem.IsInside ( AAddr : TAddr ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if FProgType<>PT_LOAD then break;
 if AAddr<FAddrP then break;
 if AAddr>=(FAddrP+FFileSZ) then break;
 Result:=TRUE;
 until TRUE;
End;

Function TElfProgItem.TrySetDbgParams ( AAddr : TAddr; AFileIdx : TIndex; ALine, APos : Cardinal ) : boolean;
Var
  BIndex    : TIndex;
  BLocItem  : TLocInfoItem;
Begin
 Result:=FALSE;
 repeat
 if AAddr<FAddrP then break;
 BIndex:=(AAddr-FAddrP) div 2;
 if BIndex>=Length(FLocInfo) then break;
 BLocItem.FSrcFile:=AFileIdx;
 BLocItem.FLine:=ALine;
 BLocItem.FPos:=APos;
 FLocInfo[BIndex]:=BLocItem;
 Result:=TRUE;
 until TRUE;
End;

Function TElfProgItem.TryGetDbgParams ( AAddr : TAddr; Out AFileIdx : TIndex; Out ALine, APos : Cardinal ) : boolean;
Var
  BIndex    : TIndex;
  BLocItem  : TLocInfoItem;
Begin
 Result:=FALSE; AFileIdx:=0; ALine:=0; APos:=0;
 repeat
 if AAddr<FAddrP then break;
 BIndex:=(AAddr-FAddrP) div 2;
 if BIndex>=Length(FLocInfo) then break;
 BLocItem:=FLocInfo[BIndex];
 AFileIdx:=BLocItem.FSrcFile;
 ALine:=BLocItem.FLine;
 APos:=BLocItem.FPos;
 Result:=TRUE;
 until TRUE;
End;

// *** TElfSectItem ***

Constructor TElfSectItem.Create ( AOnViewAny : TOnViewAny );
Begin
 Inherited Create;
 FOnViewAny:=AOnViewAny;
End;

Destructor TElfSectItem.Destroy;
Begin
 Inherited;
End;

Procedure TElfSectItem.Fill ( Const ASectHdr : TElfSectHdr32 );
Begin
 FNameIdx:=ASectHdr.FName;
 FSectType:=ASectHdr.FType;
 FFlags:=ASectHdr.FFlags;
 FAddr:=ASectHdr.FAddr;
 FOffset:=ASectHdr.FOffset;
 FSize:=ASectHdr.FSize;
 FLink:=ASectHdr.FLink;
 FInfo:=ASectHdr.FInfo;
 FAddrAlign:=ASectHdr.FAddrAlign;
 FEntSize:=ASectHdr.FEntSize;
End;

Procedure TElfSectItem.Fill ( Const ASectHdr : TElfSectHdr64 );
Begin
 FNameIdx:=ASectHdr.FName;
 FSectType:=ASectHdr.FType;
 FFlags:=ASectHdr.FFlags;
 FAddr:=ASectHdr.FAddr;
 FOffset:=ASectHdr.FOffset;
 FSize:=ASectHdr.FSize;
 FLink:=ASectHdr.FLink;
 FInfo:=ASectHdr.FInfo;
 FAddrAlign:=ASectHdr.FAddrAlign;
 FEntSize:=ASectHdr.FEntSize;
End;

Procedure TElfSectItem.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage+' [R:TElfSectItem.'+AReporter+']');
End;

Function TElfSectItem.RdStrA ( AOffset : QWord ) : string;
Var
  BOffset       : QWord;
  BRdByte       : byte;
Begin
 Result:='';
 BOffset:=AOffset;
 while BOffset<FSize do
  begin
  BRdByte:=FRawData[BOffset];
  if BRdByte=0 then break;
  Result:=Result+Char(BRdByte);
  inc(BOffset);
  end;
End;

Function TElfSectItem.RdStr ( Var AOffset : QWord; Out AData : string ) : boolean;
Var
  BRdByte       : byte;
Begin
 Result:=FALSE; AData:='';
 while AOffset<FSize do
  begin
  BRdByte:=FRawData[AOffset]; Inc(AOffset);
  if BRdByte=0 then begin Result:=TRUE; break; end;
  AData:=AData+Char(BRdByte);
  end;
 if Result=FALSE then ViewAny('eError reading debug line info: reading after the end of file or LEB128u reading error','RdStr');
End;

Function TElfSectItem.RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
Var
  BData     : byte;
  BShift    : Cardinal;
Begin
 Result:=FALSE;

 AData:=0;
 BShift:=0;

 while AOffset<FSize do
  begin
  BData:=FRawData[AOffset]; Inc(AOffset);
  AData:=AData or (QWord(BData and $7F) shl BShift);
  if (BData and $80)=0 then begin Result:=TRUE; break; end;
  BShift:=BShift+7;
  end;
 if Result=FALSE then ViewAny('eError reading debug line info: reading after the end of file or LEB128u reading error','RdLebU');
End;

Function TElfSectItem.RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
Var
  BData     : byte;
  BShift    : Cardinal;
  BDataA    : QWord;
Begin
 Result:=FALSE;
 AData:=0; BDataA:=0;
 BShift:=0;

 while AOffset<FSize do
  begin
  BData:=FRawData[AOffset]; Inc(AOffset);
  BDataA:=BDataA or (QWord(BData and $7F) shl BShift);
  BShift:=BShift+7;
  if (BData and $80)=0 then
   begin
   if (BData and $40)<>0 then AData:=Int64(($FFFFFFFFFFFFFFFF shl BShift) or BDataA)
   else AData:=Int64(BDataA);
   Result:=TRUE;
   break;
   end;
  end;
 if Result=FALSE then ViewAny('eError reading debug line info: reading after the end of file or LEB128s reading error','RdLebS');
End;

Function TElfSectItem.RdDataB ( Var AOffset : QWord; Out AData : byte ) : boolean;
Begin
 Result:=FALSE;
 AData:=0;

 repeat
 if AOffset>=FSize then begin ViewAny('eError reading section: reading after the end of file','RdDataB'); break; end;
 AData:=FRawData[AOffset]; Inc(AOffset);
 Result:=TRUE;
 until TRUE;
End;

Function TElfSectItem.RdDataX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;
Var
  BIndex    : byte;
  BDataB    : byte;
Begin
 Result:=FALSE;
 AData:=0;

 BIndex:=0;
 while BIndex<ASize do
  begin
  if RdDataB(AOffset,BDataB)=FALSE then break;
  AData:=AData or (QWord(BDataB) shl (8*BIndex));
  inc(BIndex);
  end;
 Result:=BIndex=ASize;
End;

// *** TElfSymItem ***

Constructor TElfSymItem.Create;
Begin
 Inherited;
End;

Destructor TElfSymItem.Destroy;
Begin
 Inherited;
End;

Procedure TElfSymItem.SetInfo ( AInfo : byte );
Begin
 FInfo:=AInfo;
 FSymBind:=FInfo shr 4;
 FSymType:=FInfo and $0F;
End;

Procedure TElfSymItem.Fill ( Const ARdItem : TElfSym32 );
Begin
 FName:=ARdItem.FStName;
 FInfo:=ARdItem.FStInfo; SetInfo(FInfo);
 FOther:=ARdItem.FStOther;
 FShndx:=ARdItem.FStShndx;
 FValue:=ARdItem.FStValue;
 FSize:=ARdItem.FStSize;
End;

Procedure TElfSymItem.Fill ( Const ARdItem : TElfSym64 );
Begin
 FName:=ARdItem.FStName;
 FInfo:=ARdItem.FStInfo; SetInfo(FInfo);
 FOther:=ARdItem.FStOther;
 FShndx:=ARdItem.FStShndx;
 FValue:=ARdItem.FStValue;
 FSize:=ARdItem.FStSize;
End;

// *** TDebugAbbrevItem ***
Constructor TDebugAbbrevItem.Create ( AOnViewAny : TOnViewAny; AAbbrevSect : TElfSectItem; ANumber : Cardinal );
Begin
 Inherited Create;
 FOnViewAny:=AOnViewAny;
 FAbbrevSect:=AAbbrevSect;
 FNumber:=ANumber;

 FParamList:=TDebugAbbrevParamList.Create;
End;

Destructor TDebugAbbrevItem.Destroy;
Begin
 FParamList.Free;

 Inherited;
End;

Procedure TDebugAbbrevItem.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage+' [R:TDebugAbbrevItem.'+AReporter+']');
End;

Function TDebugAbbrevItem.Parse ( Var ARdIdx : QWord ) : boolean;
Var
  BRdData   : QWord;
  BParam    : TDebugAbbrevParam;
Begin
 Result:=FALSE;

 repeat
 if FAbbrevSect.RdLebU(ARdIdx,BRdData)=FALSE then break;
 FTag:=BRdData;
 if FAbbrevSect.RdLebU(ARdIdx,BRdData)=FALSE then break;
 FHasChildren:=(BRdData=1);
   repeat
   //BRdIdxA:=ARdIdx;
   if FAbbrevSect.RdLebU(ARdIdx,BRdData)=FALSE then break;
   BParam.FAttribute:=BRdData;
   if FAbbrevSect.RdLebU(ARdIdx,BRdData)=FALSE then break;
   BParam.FForm:=BRdData;
   BParam.FData:=0;
   if BParam.FForm=$21 then // Implicit Const
    begin
    if FAbbrevSect.RdLebS(ARdIdx,BParam.FData)=FALSE then break;
    end;
   if (BParam.FAttribute=0) and (BParam.FForm=0) then begin Result:=TRUE; break; end;
   FParamList.Append(BParam);
   //ViewAny('dParam: @'+IntToHex(BRdIdxA,4)+' '+IntToHex(BParam.FAttribute,4)+' '+IntToHex(BParam.FForm,2),'Parse');
   until FALSE;
 until TRUE;
End;

// *** TDebugInfoParam ***

Constructor TDebugInfoParam.Create ( AAbbrev : TDebugAbbrevParam );
Begin
 Inherited Create;
 FAbbrev:=AAbbrev;
 FDataA:=TByteVect.Create;
End;

Destructor TDebugInfoParam.Destroy;
Begin
 FDataA.Free;
 Inherited;
End;

Procedure TDebugInfoParam.SetDataS ( Const ADataS : string );
Begin
 FDataS:=ADataS; FDataType:=0;
End;

Procedure TDebugInfoParam.ClearDataA;
Begin
 FDataA.Clear;
End;

Procedure TDebugInfoParam.AppendDataA ( AData : byte );
Begin
 FDataA.Append(AData); FDataType:=1;
End;

Procedure TDebugInfoParam.SetDataU ( AData : QWord );
Begin
 FDataU:=AData; FDataType:=2;
End;

Procedure TDebugInfoParam.SetDataI ( AData : Int64 );
Begin
 FDataU:=QWord(AData); FDataType:=7;
End;

Procedure TDebugInfoParam.SetRefU ( AData : QWord );
Begin
 FDataU:=AData; FDataType:=3;
End;

Procedure TDebugInfoParam.SetRefAddr ( AData : QWord );
Begin
 FDataU:=AData; FDataType:=4;
End;

Procedure TDebugInfoParam.SetOffsetU ( AData : QWord );
Begin
 FDataU:=AData; FDataType:=8;
End;

Procedure TDebugInfoParam.SetFlag ( AFlag : byte );
Begin
 FDataU:=AFlag; FDataType:=5;
End;

Procedure TDebugInfoParam.SetFlagPresent;
Begin
 FDataType:=5;
End;

Procedure TDebugInfoParam.SetAddr ( AAddr : QWord );
Begin
 FDataU:=AAddr; FDataType:=6;
End;

Function TDebugInfoParam.Verbose : string;
Begin
 Result:=IntToHex(FDataType,2);
End;

// *** TDebugInfoItem ***

Constructor TDebugInfoItem.Create ( AAbbrev : TDebugAbbrevItem );
Begin
 Inherited Create;
 FAbbrev:=AAbbrev;
 FChildList:=TDebugInfoList.Create;
 FParamList:=TDebugInfoParamList.Create;
End;

Destructor TDebugInfoItem.Destroy;
Begin
 FParamList.Free;
 FChildList.Free;
 Inherited;
End;

Function TDebugInfoItem.FindParam ( AAttribute : word ) : TDebugInfoParam;
Var
  BParam        : TDebugInfoParam;
  BParamIdx     : TIndex;
Begin
 Result:=nil;

 BParamIdx:=0;
 while BParamIdx<FParamList.Count do
  begin
  BParam:=FParamList.Items[BParamIdx];
  if BParam.Abbrev.FAttribute=AAttribute then begin Result:=BParam; break; end;
  inc(BParamIdx);
  end;
End;

// *** TDebugInfoUnit ***

Constructor TDebugInfoUnit.Create ( AOnViewAny : TOnViewAny; ADebugInfoSect, AAbbrevSect, ADebugStrSect, ADebugLineStrSect : TElfSectItem );
Begin
 Inherited Create;
 FOnViewAny:=AOnViewAny; FDebugInfoSect:=ADebugInfoSect; FAbbrevSect:=AAbbrevSect; FDebugStrSect:=ADebugStrSect; FDebugLineStrSect:=ADebugLineStrSect;
 FAbbrevList:=TDebugAbbrevList.Create;
 FInfoList:=TDebugInfoList.Create;
End;

Destructor TDebugInfoUnit.Destroy;
Begin
 FInfoList.Free;
 FAbbrevList.Free;
 Inherited;
End;

Procedure TDebugInfoUnit.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage+' [R:TDebugInfoUnit.'+AReporter+']');
End;

Function TDebugInfoUnit.RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if FDebugInfoSect.RdLebU(AOffset,AData)=FALSE then begin ViewAny('eError reading debuginfo: reading after the end of file or LEB128u reading error','RdLebU'); break; end;
 if AOffset>FOffsetNextHdr then begin ViewAny('eError reading debuginfo: reading after the end of file or LEB128u reading error','RdLebU'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TDebugInfoUnit.RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if FDebugInfoSect.RdLebS(AOffset,AData)=FALSE then begin ViewAny('eError reading debuginfo: reading after the end of file or LEB128s reading error','RdLebS'); break; end;
 if AOffset>FOffsetNextHdr then begin ViewAny('eError reading debuginfo: reading after the end of file or LEB128s reading error','RdLebS'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TDebugInfoUnit.RdDataB ( Var AOffset : QWord; Out AData : byte ) : boolean;
Begin
 Result:=FALSE;
 repeat
 if AOffset>=FOffsetNextHdr then
  begin
  ViewAny('eError reading data: reading after the end of file','RdDataB');
  break;
  end;
 if FDebugInfoSect.RdDataB(AOffset,AData)=FALSE then begin ViewAny('eError reading data: reading after the end of file','RdDataB'); break; end;
 if AOffset>FOffsetNextHdr then begin ViewAny('eError reading data: reading after the end of file','RdDataB'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TDebugInfoUnit.RdDataX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;
Var
  BIndex    : byte;
  BDataB    : byte;
Begin
 Result:=FALSE;
 AData:=0;

 BIndex:=0;
 while BIndex<ASize do
  begin
  if RdDataB(AOffset,BDataB)=FALSE then break;
  AData:=AData or (QWord(BDataB) shl (8*BIndex));
  inc(BIndex);
  end;

 Result:=BIndex=ASize;
End;

Function TDebugInfoUnit.ParseAbbrev : boolean;
Var
  BRdIdx        : QWord;
  BRdData       : QWord;
  BAbbrevItem   : TDebugAbbrevItem;
  BIndexCheck   : QWord;
Begin
 Result:=FALSE;

 FAbbrevList.Clear;
 repeat
 if FAbbrevSect=nil then begin ViewAny('eAbbrev section is not defined','ParseAbbrev'); break; end;
 if FDebugAbbrevOffset>=FAbbrevSect.Size then begin ViewAny('eDebug info index fall out of abbrev section','ParseAbbrev'); break; end;
 BRdIdx:=FDebugAbbrevOffset; BIndexCheck:=1;
   repeat
   //BRdIdxA:=BRdIdx;
   if FAbbrevSect.RdLebU(BRdIdx,BRdData)=FALSE then break;
   if BRdData=0 then begin Result:=TRUE; break; end;
   if BRdData<>BIndexCheck then begin ViewAny('eAbbrev index consecutive check error','ParseAbbrev'); break; end;
   inc(BIndexCheck);
   BAbbrevItem:=TDebugAbbrevItem.Create(FOnViewAny,FAbbrevSect,BRdData); FAbbrevList.Append(BAbbrevItem);
   //ViewAny('dAbbrevItem: @'+IntToHex(BRdIdxA,4)+' '+IntToHex(BAbbrevItem.Number,4)+' '+IntToHex(BAbbrevItem.Tag,2),'ParseAbbrev');
   if BAbbrevItem.Parse(BRdIdx)=FALSE then break;
   until FALSE;
 until TRUE;
End;

Function TDebugInfoUnit.ReadDebugInfoParam ( ADebugAbbrevParam : TDebugAbbrevParam; ADebugInfoParam : TDebugInfoParam; Var AOffset : QWord ) : boolean;
Var
  BForm     : word;
  BDataQ    : QWord;
  BDataQI   : Int64;
  BRdIdx    : QWord;
  BRdDataC  : byte;
  BDataS    : string;
  BDataIdx,
  BDataLen  : TIndex;
Begin
 Result:=FALSE;

 repeat
 BForm:=ADebugInfoParam.Abbrev.FForm;
 case BForm of
   $01: // DW_FORM_addr
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     ADebugInfoParam.SetAddr(BDataQ);
     Result:=TRUE;
     end;
   $03: // DW_FORM_block2
     begin
     ADebugInfoParam.ClearDataA;
     if RdDataX(AOffset,2,BDataQ)=FALSE then break;
     BDataLen:=BDataQ;
     BDataIdx:=0;
     while BDataIdx<BDataLen do
      begin
      if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_sting','ReadDebugInfoParam'); break; end;
      ADebugInfoParam.AppendDataA(BRdDataC);
      inc(BDataIdx);
      end;
     Result:=(BDataIdx=BDataLen);
     end;
   $04: // DW_FORM_block4
     begin
     ADebugInfoParam.ClearDataA;
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     BDataLen:=BDataQ;
     BDataIdx:=0;
     while BDataIdx<BDataLen do
      begin
      if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_sting','ReadDebugInfoParam'); break; end;
      ADebugInfoParam.AppendDataA(BRdDataC);
      inc(BDataIdx);
      end;
     Result:=BDataIdx=BDataLen;
     end;
   $05: // DW_FORM_data2
     begin
     if RdDataX(AOffset,2,BDataQ)=FALSE then break;
     ADebugInfoParam.SetDataU(BDataQ);
     Result:=TRUE;
     end;
   $06: // DW_FORM_data4
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     ADebugInfoParam.SetDataU(BDataQ);
     Result:=TRUE;
     end;
   $07: // DW_FORM_data8
     begin
     if RdDataX(AOffset,8,BDataQ)=FALSE then break;
     ADebugInfoParam.SetDataU(BDataQ);
     Result:=TRUE;
     end;
   $08: // DW_FORM_string
     begin
     BDataS:='';
     repeat
     if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_sting','ReadDebugInfoParam'); break; end;
     if BRdDataC=0 then begin ADebugInfoParam.SetDataS(BDataS); Result:=TRUE; break; end;
     BDataS:=BDataS+char(BRdDataC);
     until FALSE;
     if Result=FALSE then break;
     end;
   $09: // DW_FORM_block
     begin
     ADebugInfoParam.ClearDataA;
     if RdLebU(AOffset,BDataQ)=FALSE then break;
     BDataLen:=BDataQ;
     BDataIdx:=0;
     while BDataIdx<BDataLen do
      begin
      if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_sting','ReadDebugInfoParam'); break; end;
      ADebugInfoParam.AppendDataA(BRdDataC);
      inc(BDataIdx);
      end;
     Result:=BDataIdx=BDataLen;
     end;
   $0A: // DW_FORM_block1
     begin
     ADebugInfoParam.ClearDataA;
     if RdDataX(AOffset,1,BDataQ)=FALSE then break;
     BDataLen:=BDataQ;
     BDataIdx:=0;
     while BDataIdx<BDataLen do
      begin
      if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_sting','ReadDebugInfoParam'); break; end;
      ADebugInfoParam.AppendDataA(BRdDataC);
      inc(BDataIdx);
      end;
     Result:=BDataIdx=BDataLen;
     end;
   $0B: // DW_FORM_data1
     begin
     if RdDataX(AOffset,1,BDataQ)=FALSE then break;
     ADebugInfoParam.SetDataU(BDataQ);
     Result:=TRUE;
     end;
   $0D: // DW_FORM_sdata
     begin
     if RdLebS(AOffset,BDataQI)=FALSE then break;
     ADebugInfoParam.SetDataI(BDataQI);
     Result:=TRUE;
     end;
   $0C: // DW_FORM_flag
     begin
     if RdDataX(AOffset,1,BDataQ)=FALSE then break;
     ADebugInfoParam.SetFlag(BDataQ);
     Result:=TRUE;
     end;
   $0F: // DW_FORM_udata
     begin
     if RdLebU(AOffset,BDataQ)=FALSE then break;
     ADebugInfoParam.SetDataU(BDataQ);
     Result:=TRUE;
     end;
   $0E: // DW_FORM_strp
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     BDataS:='';
     BRdIdx:=BDataQ;
     repeat
     if FDebugStrSect.RdDataB(BRdIdx,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_strp','ReadDebugInfoParam'); break; end;
     if BRdDataC=0 then begin ADebugInfoParam.SetDataS(BDataS); Result:=TRUE; break; end;
     BDataS:=BDataS+char(BRdDataC);
     until FALSE;
     if Result=FALSE then break;
     end;
   $10: // DW_FORM_ref_addr
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefAddr(BDataQ);
     Result:=TRUE;
     end;
   $11: // DW_FORM_ref1
     begin
     if RdDataX(AOffset,1,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefU(BDataQ);
     Result:=TRUE;
     end;
   $12: // DW_FORM_ref2
     begin
     if RdDataX(AOffset,2,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefU(BDataQ);
     Result:=TRUE;
     end;
   $13: // DW_FORM_ref4
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefU(BDataQ);
     Result:=TRUE;
     end;
   $14: // DW_FORM_ref8
     begin
     if RdDataX(AOffset,8,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefU(BDataQ);
     Result:=TRUE;
     end;
   $15: // DW_FORM_refu
     begin
     if RdLebU(AOffset,BDataQ)=FALSE then break;
     ADebugInfoParam.SetRefU(BDataQ);
     Result:=TRUE;
     end;
   $17: // DW_FORM_sec_offset
     begin
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     ADebugInfoParam.SetOffsetU(BDataQ);
     Result:=TRUE;
     end;
   $18: // DW_FORM_exprloc
     begin
     ADebugInfoParam.ClearDataA;
     if RdLebU(AOffset,BDataQ)=FALSE then break;
     BDataLen:=BDataQ;
     BDataIdx:=0;
     while BDataIdx<BDataLen do
      begin
      if RdDataB(AOffset,BRdDataC)=FALSE then begin ViewAny('eError reading DW_FORM_exprloc','ReadDebugInfoParam'); break; end;
      ADebugInfoParam.AppendDataA(BRdDataC);
      inc(BDataIdx);
      end;
     Result:=(BDataIdx=BDataLen);
     end;
   $19: // DW_FORM_flag_present
     begin
     ADebugInfoParam.SetFlagPresent;
     Result:=TRUE;
     end;
   $1F: // DW_FORM_line_strp
     begin
     if FDebugLineStrSect=nil then begin ViewAny('eDW_FORM_line_strp is requested in attrib section but .debug_line_str section is not located','ReadDebugInfoParam'); break; end;
     if RdDataX(AOffset,4,BDataQ)=FALSE then break;
     BRdIdx:=BDataQ;
     if FDebugLineStrSect.RdStr(BRdIdx,BDataS)=FALSE then begin ViewAny('eError reading DW_FORM_strp','ReadDebugInfoParam'); break; end;
     ADebugInfoParam.SetDataS(BDataS);
     Result:=TRUE;
     end;
   $21: // DW_FORM_implicit_const
     begin
     ADebugInfoParam.SetDataI(ADebugAbbrevParam.FData);
     Result:=TRUE;
     end;

  end; // case
 until TRUE;

 if Result=FALSE then
  begin
  ViewAny('eError parsing abbrev form = 0x'+IntToHex(BForm,2),'ReadDebugInfoParam');
  end;

End;


Function TDebugInfoUnit.Parse ( Var ARdIdx : QWord ) : boolean;
Var
  BDataQ        : QWord;
  BAbbrevIdx    : TIndex;
  BInfoItem     : TDebugInfoItem;
  BAbbrevItem   : TDebugAbbrevItem;
  BParamIdx     : TIndex;
  BActiveParent : TDebugInfoItem;
  BAbbrevParam  : TDebugAbbrevParam;
  BInfoParam    : TDebugInfoParam;
  //BRdIdxA       : QWord;
Begin
 Result:=FALSE;

 FBase:=ARdIdx;

 repeat
 FOffsetNextHdr:=FBase+4; // Temporarily, to avoid RdData fail
 if RdDataX(ARdIdx,4,BDataQ)=FALSE then break;
 FDataLen:=BDataQ;
 FOffsetNextHdr:=FBase+FDataLen+4;
 if FOffsetNextHdr>FDebugInfoSect.Size then begin ViewAny('eError in ELF file: DebugInfoItem goes out of segment size','Parse'); break; end;
 if RdDataX(ARdIdx,2,BDataQ)=FALSE then break; FVersion:=BDataQ;
 if FVersion>=5 then
  begin
  if RdDataX(ARdIdx,1,BDataQ)=FALSE then break; FUnitType:=BDataQ;
  if RdDataX(ARdIdx,1,BDataQ)=FALSE then break; FAddressSize:=BDataQ;
  if RdDataX(ARdIdx,4,BDataQ)=FALSE then break; FDebugAbbrevOffset:=BDataQ;
  end
{else if FVersion>=4 then
   begin
   if RdDataX(ARdIdx,4,BDataQ)=FALSE then break; FDebugAbbrevOffset=BDataQ;
   if RdDataX(ARdIdx,1,BDataQ)=FALSE then break; FAddressSize=BDataQ;
   if RdDataX(ARdIdx,4,BDataQ)=FALSE then break; FTypeSignature=BDataQ;
   if RdDataX(ARdIdx,2,BDataQ)=FALSE then break; FTypeOffset=BDataQ;
   end}
 else
  begin
  if RdDataX(ARdIdx,4,BDataQ)=FALSE then break; FDebugAbbrevOffset:=BDataQ;
  if RdDataX(ARdIdx,1,BDataQ)=FALSE then break; FAddressSize:=BDataQ;
  end;

 if ParseAbbrev=FALSE then break;
 BActiveParent:=nil;
   repeat
   if ARdIdx=FOffsetNextHdr then begin Result:=TRUE; break; end;
   //BRdIdxA:=ARdIdx;
   if RdLebU(ARdIdx,BDataQ)=FALSE then break;
   //ViewAny('dAbbrevIdx: @'+IntToHex(BRdIdxA,4)+' '+IntToHex(BDataQ,4),'Parse');
   if BDataQ=0 then // Go 1 level up
    begin
    if BActiveParent=nil then begin ViewAny('eHierarchy error','Parse'); break; end;
    BActiveParent:=BActiveParent.Parent;
    end
   else
    begin
    BAbbrevIdx:=BDataQ-1;
    if BAbbrevIdx>=FAbbrevList.Count then begin ViewAny('eAbbrev index out of range','Parse'); break; end;
    BAbbrevItem:=FAbbrevList.Items[BAbbrevIdx];
    BInfoItem:=TDebugInfoItem.Create(BAbbrevItem);
    if BActiveParent=nil then FInfoList.Append(BInfoItem) else BActiveParent.ChildList.Append(BInfoItem);
    BInfoItem.Parent:=BActiveParent;
    if BAbbrevItem.HasChildren then BActiveParent:=BInfoItem;
    BParamIdx:=0;
    while BParamIdx<BAbbrevItem.ParamList.Count do
     begin
     BAbbrevParam:=BAbbrevItem.ParamList.Items[BParamIdx];
     BInfoParam:=TDebugInfoParam.Create(BAbbrevParam); BInfoItem.ParamList.Append(BInfoParam);
     //BRdIdxA:=ARdIdx;
     if ReadDebugInfoParam(BAbbrevParam,BInfoParam,ARdIdx)=FALSE then begin ViewAny('eDebugInfoParam reading error','Parse'); break; end;
     //ViewAny('dDebugInfoParam: @'+IntToHex(BRdIdxA,4)+' '+BInfoParam.Verbose,'');
     inc(BParamIdx);
     end;
    if BParamIdx<>BAbbrevItem.ParamList.Count then break;
    end;
   until FALSE;
 if Result=FALSE then begin ViewAny('eDebug items parse error','Parse'); break; end;
 if BActiveParent<>nil then begin ViewAny('eHierarchy error','Parse'); Result:=FALSE; break; end;
 until TRUE;
End;

// *** TElfProc ***

Constructor TElfProc.Create ( AAddr : TAddr; Const AProcName : string );
Begin
 Inherited Create;
 FAddrS:=AAddr;
 FProcName:=AProcName;
End;

Destructor TElfProc.Destroy;
Begin
 Inherited;
End;

// *** TDecFsm ***

Constructor TDecFsm.Create;
Begin
 Inherited Create;
End;

Destructor TDecFsm.Destroy;
Begin
 Inherited Destroy;
End;

Procedure TDecFsm.Clear ( AIsStmt : Byte; Const AFileName : string );
Begin
 FAddr:=0; FOpIndex:=0; FFileName:=AFileName; FLine:=1; FColumn:=0;  FIsStmt:=AIsStmt;
 FBasicBlock:=FALSE; FEndSequence:=FALSE; FPrologueEnd:=FALSE; FEpilogueBegin:=FALSE;
 FIsa:=0; FDiscrim:=0;
End;

Procedure TDecFsm.NegateStmt;
Begin
 FIsStmt:=not FIsStmt;
End;

Procedure TDecFsm.Move ( AAddr : TAddr; AOpIdx : Cardinal; ALineInc : Integer );
Begin
 FAddr:=AAddr; FOpIndex:=AOpIdx; FLine:=Cardinal(Integer(FLine)+ALineInc);
 FBasicBlock:=FALSE; FEndSequence:=FALSE; FPrologueEnd:=FALSE; FEpilogueBegin:=FALSE;
End;

Procedure TDecFsm.MoveLine ( ALineInc : Integer );
Begin
 FLine:=Cardinal(Integer(FLine)+ALineInc);
End;

// *** TDebugLineItem ***

Constructor TDebugLineItem.Create ( AElfFile : TElfFile; AOnViewAny : TOnViewAny; ARawData : PByte; AOnSetDbgParams : TOnSetDbgParams; ASizeBase : byte );
Begin
 Inherited Create;

 FElfFile:=AElfFile; // Needed to access predefined sections
 FOnViewAny:=AOnViewAny; FRawData:=ARawData; FOnSetDbgParams:=AOnSetDbgParams;
 FSizeBase:=ASizeBase;

 FPathList:=TStringList.Create;
 FFileList:=TStringList.Create;
 FDecFsm:=TDecFsm.Create;
End;

Destructor TDebugLineItem.Destroy;
Begin
 FDecFsm.Free;
 FFileList.Free;
 FPathList.Free;
 Inherited;
End;

Procedure TDebugLineItem.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 if Assigned(FOnViewAny) then FOnViewAny(AMessage+' [R:TDebugLineItem.'+AReporter+']');
End;

Procedure TDebugLineItem.SetDbgParams ( AAddrB, AAddrE : TAddr );
Begin
 if Assigned(FOnSetDbgParams) then FOnSetDbgParams(AAddrB,AAddrE,FDecFsm.FileName,FDecFsm.Line,FDecFsm.Column);
End;

Function TDebugLineItem.RdLebU ( Var AOffset : QWord; Out AData : QWord ) : boolean;
Var
  BShift    : Cardinal;
  BData     : byte;
Begin
 Result:=FALSE;
 BShift:=0;

 AData:=0;
 while AOffset<FOffsetNextHdr do
  begin
  BData:=FRawData[AOffset]; inc(AOffset);
  AData:=AData or (QWord(BData and $7F) shl BShift);
  if (BData and $80)=0 then begin Result:=TRUE; break; end;
  Inc(BShift,7);
  end;
 if Result=FALSE then ViewAny('eError reading debug line info: reading after the end of file or LEB128u reading error','RdLebU');
End;

Function TDebugLineItem.RdLebS ( Var AOffset : QWord; Out AData : Int64 ) : boolean;
Var
  BShift    : Cardinal;
  BData     : byte;
  BDataA    : QWord;
Begin
 Result:=FALSE;
 BShift:=0;
 BDataA:=0;

 AData:=0;
 while AOffset<FOffsetNextHdr do
  begin
  BData:=FRawData[AOffset]; Inc(AOffset);
  BDataA:=BDataA or (QWord(BData and $7F) shl BShift);
  Inc(BShift,7);
  if (BData and $80)=0 then
   begin
   if (BData and $40)<>0 then AData:=Int64($FFFFFFFFFFFFFFFF shl BShift) or BDataA
   else AData:=Int64(BDataA);
   Result:=TRUE;
   break;
   end;
  end;

 if Result=FALSE then ViewAny('eError reading debug line info: reading after the end of file or LEB128s reading error','RdLebS');
End;

Function TDebugLineItem.RdProgB ( Var AOffset : QWord; Out AData : byte ) : boolean;
Begin
 Result:=FALSE;
 AData:=0;

 repeat
 if AOffset>=FOffsetNextHdr then begin ViewAny('eError reading debug line info: reading after the end of file','RdProgB'); break; end;
 AData:=FRawData[AOffset]; inc(AOffset);
 Result:=TRUE;
 until TRUE;
End;

Function TDebugLineItem.RdProgX ( Var AOffset : QWord; ASize : byte; Out AData : QWord ) : boolean;
Var
  BIndex    : byte;
  BDataB    : byte;
Begin
 Result:=FALSE;
 AData:=0;

 BIndex:=0;
 while BIndex<ASize do
  begin
  if RdProgB(AOffset,BDataB)=FALSE then break;
  AData:=AData or (QWord(BDataB) shl (8*BIndex));
  inc(BIndex);
  end;
 Result:=BIndex=ASize;
End;

Function TDebugLineItem.RdStrZ ( Var AOffset : QWord; Out AData : string ) : boolean;
Var
  BData     : byte;
Begin
 Result:=FALSE;
 AData:='';

 repeat
 if AOffset>=FOffsetNextHdr then begin ViewAny('eError reading debug line info: reading after the end of file','RdStr'); break; end;
 BData:=FRawData[AOffset]; inc(AOffset);
 if BData=0 then begin Result:=TRUE; break; end;
 if (BData<32) or (BData>=127) then begin ViewAny('eError reading debug line info: incorrect char symbol','RdStr'); break; end;
 AData:=AData+char(BData);
 until FALSE;
End;

Procedure TDebugLineItem.ReadHeader;
Var
  BOffset   : QWord;
  BIndexA   : byte;
Begin
 BOffset:=0;
 // Unit length
 if FSizeBase=1 then FDataLen:=RdMisalignedD(FRawData,BOffset) else FDataLen:=RdMisalignedQ(FRawData,BOffset); FOffsetNextHdr:=FDataLen+BOffset;
 // Version
 FVersion:=RdMisalignedW(FRawData,BOffset);
 // AddrSize / SegmSelector (V5 only)
 if FVersion<5 then begin FAddrSize:=4; FSegmSelector:=0; end
 else begin FAddrSize:=RdMisalignedB(FRawData,BOffset); FSegmSelector:=RdMisalignedB(FRawData,BOffset); end;
 // Header length
 if FSizeBase=1 then FHeaderLength:=RdMisalignedD(FRawData,BOffset) else FHeaderLength:=RdMisalignedQ(FRawData,BOffset);
 FOffsetOpc:=FHeaderLength+BOffset;
 // Min instruction length
 FMinInstructionLength:=RdMisalignedB(FRawData,BOffset);
 // Max oper per instruction (Version higher than 2)
 if FVersion<=3 then FMaxOperPerInstruction:=1
 else FMaxOperPerInstruction:=RdMisalignedB(FRawData,BOffset);
 // Default_si_stmt
 FDefaultIsStmt:=RdMisalignedB(FRawData,BOffset);
 // LineBase, LineRange, OpcodeBase, OpcodeLen
 FLineBase:=ShortInt(RdMisalignedB(FRawData,BOffset));
 FLineRange:=RdMisalignedB(FRawData,BOffset);
 FOpcodeBase:=RdMisalignedB(FRawData,BOffset);
 BIndexA:=1;
 while BIndexA<FOpcodeBase do
  begin
  FStdOpcodeLen[BIndexA-1]:=FRawData[BOffset]; Inc(BOffset);
  inc(BIndexA);
  end;
 while BIndexA<=12 do
  begin
  FStdOpcodeLen[BIndexA-1]:=0;
  inc(BIndexA);
  end;
 FOffsetPathFile:=BOffset;
End;

Function TDebugLineItem.Parse : boolean;
Begin
 if FVersion<5 then Result:=Parse2
 else Result:=Parse5;
End;

Function TDebugLineItem.Parse2 : boolean;
Var
  BRdLine       : string;
  BRdByte       : Byte;
  BOffsetLoc    : QWord;
  BDirIdx,
  BTime,
  BSize         : QWord;
  BPath         : string;
  BFileName     : string;
  BLen          : TIndex;
Begin
 Result:=FALSE;

 FPathList.Clear; FPathList.Append('');
 FFileList.Clear;

 repeat
 //ViewAny('iCU','');
 // Rd Directories
 BRdLine:=''; BOffsetLoc:=FOffsetPathFile;
 while BOffsetLoc<FOffsetNextHdr do
  begin
  BRdByte:=FRawData[BOffsetLoc]; Inc(BOffsetLoc);
  if BRdByte=0 then
   begin
   if BRdLine='' then begin Result:=TRUE; break; end;
   //ViewAny('iPath: "+BRdLine,"');
   FPathList.Append(BRdLine); BRdLine:='';
   end
  else BRdLine:=BRdLine+Char(BRdByte);
  end;
 if Result=FALSE then begin ViewAny('eError parsing .debug_line section','Parse'); break; end;
 Result:=FALSE;
 if BOffsetLoc>=FOffsetNextHdr then begin ViewAny('eError parsing .debug_line section, reading outside of section size','Parse'); break; end;

 // Rd Files
 BRdLine:='';
 while BOffsetLoc<FOffsetNextHdr do
  begin
  BRdByte:=FRawData[BOffsetLoc]; Inc(BOffsetLoc);
  if BRdByte=0 then
   begin
   if BRdLine='' then begin Result:=TRUE; break; end;
   if RdLebU(BOffsetLoc,BDirIdx)=FALSE then break;
   if RdLebU(BOffsetLoc,BTime)=FALSE then break;
   if RdLebU(BOffsetLoc,BSize)=FALSE then break;
   if BDirIdx>=FPathList.Count then begin ViewAny('eDirectory index out of range','Parse'); break; end;
   BPath:=FPathList.Strings[BDirIdx];
   BLen:=Length(BPath);
   if (BLen<>0) and (BPath[BLen]<>'/') then BPath:=BPath+'/';
   BFileName:=BPath+BRdLine;
   //ViewAny('iFile: "+BFileName,"');
   FFileList.Append(BFileName); BRdLine:='';
   end
  else BRdLine:=BRdLine+Char(BRdByte);
  end;
 if Result=FALSE then begin ViewAny('eError parsing .debug_line section','Parse'); break; end;
 Result:=FALSE;
 if BOffsetLoc>=FOffsetNextHdr then begin ViewAny('eError parsing .debug_line section, reading outside of section size','Parse'); break; end;
 if BOffsetLoc<>FOffsetOpc then begin ViewAny('eError parsing .debug_line section, internal error or damaged header','Parse'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TDebugLineItem.ReadFormatList ( Var AOffset : QWord; AFormatList : TPathFileFormatList ) : boolean;
Var
  BDataQ        : QWord;
  BFormatCnt    : byte;
  BFormatItem   : TPathFileFormatItem;
  BIndexB       : byte;
Begin
 Result:=FALSE;
 AFormatList.Clear;

 repeat
 if RdProgB(AOffset,BFormatCnt)=FALSE then break;
 BIndexB:=0;
 while BIndexB<BFormatCnt do
  begin
  if RdLebU(AOffset,BDataQ)=FALSE then break; BFormatItem.FContentType:=BDataQ;
  if RdLebU(AOffset,BDataQ)=FALSE then break; BFormatItem.FFormCode:=BDataQ;
  AFormatList.Append(BFormatItem);
  inc(BIndexB);
  end;
 if BIndexB<>BFormatCnt then break;
 if AOffset>=FOffsetNextHdr then begin ViewAny('eError parsing .debug_line section, reading outside of section size','ReadFormatList'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TDebugLineItem.ReadByFormat ( Var AOffset : QWord; AForm : Cardinal; Out AAnyDataS : string; Out AAnyDataQ : QWord ) : boolean;
Var
  BRdIdx    : QWord;
Begin
 Result:=FALSE; AAnyDataS:=''; AAnyDataQ:=0;
 repeat
 case AForm of
   $05: // DW_FORM_data2
     begin
     if RdProgX(AOffset,2,AAnyDataQ)=FALSE then break;
     Result:=TRUE;
     end;
   $06: // DW_FORM_data4
     begin
     if RdProgX(AOffset,4,AAnyDataQ)=FALSE then break;
     Result:=TRUE;
     end;
   $0B: // DW_FORM_data1
     begin
     if RdProgX(AOffset,1,AAnyDataQ)=FALSE then break;
     Result:=TRUE;
     end;
   $0F: // DW_FORM_udata
     begin
     if RdLebU(AOffset,AAnyDataQ)=FALSE then break;
     Result:=TRUE;
     end;
   $1F: // DW_FORM_line_strp
     begin
     if FElfFile.DebugLineStrSect=nil then begin ViewAny('eDW_FORM_line_strp is requested in attrib section but .debug_line_str section is not located','ReadDebugInfoParam'); break; end;
     if RdProgX(AOffset,4,BRdIdx)=FALSE then break;
     if FElfFile.DebugLineStrSect.RdStr(BRdIdx,AAnyDataS)=FALSE then begin ViewAny('eError reading DW_FORM_strp','ReadDebugInfoParam'); break; end;
     Result:=TRUE;
     end;
   end; // case
 until TRUE;
 if Result=FALSE then
  begin
  ViewAny('eUnknown DW_FORM_* code: 0x'+IntToHex(AForm,2),'ReadPathFileFormat');
  end;
End;

Function TDebugLineItem.ReadPathList5 ( Var AOffset : QWord ) : boolean;
Var
  BFormatList   : TPathFileFormatList;
  BFormatItem   : TPathFileFormatItem;
  BPathFileCnt,
  BPathFileIdx  : QWord;
  BFormatIdx    : SizeInt;
  BAnyDataS     : string;
  BAnyDataQ     : QWord;
Begin
 Result:=FALSE;
 BFormatList:=TPathFileFormatList.Create;

 repeat
 if ReadFormatList(AOffset,BFormatList)=FALSE then break;

 if RdLebU(AOffset,BPathFileCnt)=FALSE then break;
 BPathFileIdx:=0;
 while BPathFileIdx<BPathFileCnt do
  begin
  BFormatIdx:=0;
  while BFormatIdx<BFormatList.Count do
   begin
   BFormatItem:=BFormatList.Items[BFormatIdx];
   case BFormatItem.FContentType of
     $01: // DW_LNCT_path
       begin
       if ReadByFormat(AOffset,BFormatItem.FFormCode,BAnyDataS,BAnyDataQ)=FALSE then break;
       FPathList.Append(BAnyDataS);
       end;
   end; // case
   inc(BFormatIdx);
   end;
  if BFormatIdx<>BFormatList.Count then break;
  if AOffset>=FOffsetNextHdr then begin ViewAny('eError parsing .debug_line section, reading outside of section size','ReadPathFileList5'); break; end;
  inc(BPathFileIdx);
  end;
 if BPathFileIdx<>BPathFileCnt then break;
 Result:=TRUE;
 until TRUE;

 BFormatList.Free;
End;

Function TDebugLineItem.ReadFileList5 ( Var AOffset : QWord ) : boolean;
Var
  BFormatList   : TPathFileFormatList;
  BFormatItem   : TPathFileFormatItem;
  BPathFileCnt,
  BPathFileIdx  : QWord;
  BFormatIdx    : SizeInt;
  BAnyDataS     : string;
  BAnyDataQ     : QWord;
  BPathS,
  BFileS        : string;
Begin
 Result:=FALSE;
 BFormatList:=TPathFileFormatList.Create;

 repeat
 if ReadFormatList(AOffset,BFormatList)=FALSE then break;

 if RdLebU(AOffset,BPathFileCnt)=FALSE then break;
 BPathFileIdx:=0;
 while BPathFileIdx<BPathFileCnt do
  begin
  BPathS:=''; BFileS:='';
  BFormatIdx:=0;
  while BFormatIdx<BFormatList.Count do
   begin
   BFormatItem:=BFormatList.Items[BFormatIdx];
   case BFormatItem.FContentType of
     $01: // DW_LNCT_path
       begin
       if ReadByFormat(AOffset,BFormatItem.FFormCode,BAnyDataS,BAnyDataQ)=FALSE then break;
       BFileS:=BAnyDataS;
       end;
     $02: // DW_LNCT_directory_index
       begin
       if ReadByFormat(AOffset,BFormatItem.FFormCode,BAnyDataS,BAnyDataQ)=FALSE then break;
       if BAnyDataQ>=FPathList.Count then begin ViewAny('eError parsing .debug_line section, directory index out of range','ReadPathFileList5'); break; end;
       BPathS:=FPathList.Strings[BAnyDataQ];
       end;
   end; // case
   inc(BFormatIdx);
   end;
  if BFormatIdx<>BFormatList.Count then break;
  if AOffset>FOffsetNextHdr then begin ViewAny('eError parsing .debug_line section, reading outside of section size','ReadPathFileList5'); break; end;
  if BFileS='' then begin ViewAny('eError parsing .debug_line section, file name is not specified','ReadPathFileList5'); break; end;
  FFileList.Append(IncludeTrailingPathDelimiterA(BPathS,'/')+BFileS);
  inc(BPathFileIdx);
  end;
 if BPathFileIdx<>BPathFileCnt then break;
 Result:=TRUE;
 until TRUE;

 BFormatList.Free;
End;

Function TDebugLineItem.Parse5 : boolean;
Var
  BOffsetLoc    : QWord;
Begin
 Result:=FALSE;

 FPathList.Clear; FPathList.Append('');
 FFileList.Clear;

 BOffsetLoc:=FOffsetPathFile;
 repeat
 if ReadPathList5(BOffsetLoc)=FALSE then break;
 if ReadFileList5(BOffsetLoc)=FALSE then break;
 if BOffsetLoc<>FOffsetOpc then begin ViewAny('eError parsing .debug_line section, internal error or damaged header','Parse5'); break; end;
 Result:=TRUE;
 until TRUE;

End;

Procedure TDebugLineItem.AppendDbgInfoFile ( Var AFileList : string );
Var
  BFileIdx  : Integer;
Begin
 BFileIdx:=0;
 while BFileIdx<FFileList.Count do
  begin
  if AFileList<>'' then AFileList:=AFileList+' ';
  AFileList:=AFileList+FFileList.Strings[BFileIdx];
  inc(BFileIdx);
  end;
End;

Function TDebugLineItem.IndexFlowLines : boolean;
Var
  BOffsetLoc    : QWord;
  BOpcode       : byte;
  BOpSize       : QWord;
  BOpData       : QWord;
  BDataS        : string;
  BFileDirIdx,
  BFileModifTime,
  BFileSize     : QWord;
  BFileMain     : string;
  BFileNameA    : string;
  BAdjustOpcode,
  BOperAdvance  : byte;
  BAddrNext     : TAddr;
  BOpIndexNext  : Cardinal;
  BLineInc      : Integer;
  BDataI        : Int64;
  BDataU        : QWord;
  BAddrPropB    : TAddr; // Prop beginning address
Begin
 Result:=FALSE;
 BOpData:=0;

 FFsmEndSequence:=FALSE;
 BFileMain:=''; if FFileList.Count<>0 then BFileMain:=FFileList.Strings[0];
 FDecFsm.Clear(FDefaultIsStmt,BFileMain); BAddrPropB:=0;

 //ViewAny('i *** '+BFileMain+' *** ','');
 BOffsetLoc:=FOffsetOpc;
 while BOffsetLoc<FOffsetNextHdr do
  begin
  if RdProgB(BOffsetLoc,BOpcode)=FALSE then break;
  if BOpcode=0 then // Ext opcodes
   begin
   if RdLebU(BOffsetLoc,BOpSize)=FALSE then break;
   if BOpSize<1 then begin ViewAny('eError reading debug line index: Opc data is too small','IndexFlowLines'); break; end;
   if RdProgB(BOffsetLoc,BOpcode)=FALSE then break;
   case BOpcode of
     1: begin // DW_LNE_end_sequence
        FFsmEndSequence:=TRUE;
        FDecFsm.Clear(FDefaultIsStmt,BFileMain);
        //ViewAny('iExt DW_LNE_end_sequence','IndexFlowLines');
        //ViewAny('i','IndexFlowLines');
        Result:=TRUE;
        end;
     2: begin // DW_LNE_set_address
        if RdProgX(BOffsetLoc,Byte(BOpSize-1),BOpData)=FALSE then break;
        //if FDecFsm.Addr<>0 then PropDbgParams(TAddr(BOpData));
        FDecFsm.Addr:=TAddr(BOpData); FDecFsm.OpIndex:=0;
        BAddrPropB:=FDecFsm.Addr;
        //ViewAny('iExt DW_LNE_set_address 0x'+IntToHex(FDecFsm.Addr,6),'IndexFlowLines');
        Result:=TRUE;
        end;
     3: begin // DW_LNE_define_file
        if RdStrZ(BOffsetLoc,BDataS)=FALSE then break;
        if RdLebU(BOffsetLoc,BFileDirIdx)=FALSE then break;
        if RdLebU(BOffsetLoc,BFileModifTime)=FALSE then break;
        if RdLebU(BOffsetLoc,BFileSize)=FALSE then break;
        if BFileDirIdx=0 then BFileNameA:=BDataS
        else if (BFileDirIdx+1)>=FPathList.Count then begin ViewAny('eELF parsing error: DirIdx out of range','IndexFlowLines'); break; end
        else BFileNameA:=IncludeTrailingPathDelimiter(FPathList.Strings[BFileDirIdx-1])+BDataS;
        FDecFsm.FileName:=BFileNameA;
        //ViewAny('iExt DW_LNE_define_file '+BFileNameA,'IndexFlowLines');
        Result:=TRUE;
        end;
     4: begin // DW_LNE_set_discriminator
        if RdLebU(BOffsetLoc,BOpData)=FALSE then break;
        FDecFsm.Discrim:=Integer(BOpData);
        //ViewAny('iExt DW_LNE_set_discriminator '+IntToStr(BOpData),'IndexFlowLines');
        Result:=TRUE;
        end;
     else
       ViewAny('eExt opcode 0x'+IntToHex(BOpcode,2)+' is not implemented or not tested','IndexFlowLines');
    end; // case
   if Result=FALSE then break;
   Result:=FALSE;
   end
  else if BOpcode<FOpcodeBase then // Std opcodes
   begin
   case BOpcode of
     1: begin // DW_LNS_copy
        SetDbgParams(BAddrPropB,FDecFsm.Addr); BAddrPropB:=FDecFsm.Addr;
        FDecFsm.BasicBlock:=FALSE; FDecFsm.PrologueEnd:=FALSE; FDecFsm.EpilogueBegin:=FALSE;
        //ViewAny('iStd DW_LNS_copy','IndexFlowLines');
        Result:=TRUE;
        end;
     3: begin // DW_LNS_advance_line
        if RdLebS(BOffsetLoc,BDataI)=FALSE then break;
        FDecFsm.MoveLine(BDataI);
        //ViewAny('iStd DW_LNS_advance_line '+IntToStr(BDataI),'IndexFlowLines');
        Result:=TRUE;
        end;
     4: begin // DW_LNS_set_file
        if RdLebU(BOffsetLoc,BDataU)=FALSE then break;
        if BDataU=0 then begin ViewAny('eELF parsing error: FileIndex is zero','IndexFlowLines'); break; end;
        if BDataU>FFileList.Count then begin ViewAny('eELF parsing error: FileIndex out of range','IndexFlowLines'); break; end;
        BFileNameA:=FFileList[Cardinal(BDataU)-1];
        FDecFsm.FileName:=BFileNameA;
        //ViewAny('iStd DW_LNS_set_file *** '+BFileNameA+' ***','IndexFlowLines');
        Result:=TRUE;
        end;
     5: begin // DW_LNS_set_column
        if RdLebU(BOffsetLoc,BDataU)=FALSE then break;
        FDecFsm.Column:=BDataU;
        //ViewAny('iStd DW_LNS_set_column '+IntToStr(BDataU),'IndexFlowLines');
        Result:=TRUE;
        end;
     6: begin // DW_LNS_negate_stmt
        FDecFsm.NegateStmt;
        //ViewAny('iStd DW_LNS_negate_stmt','IndexFlowLines');
        Result:=TRUE;
        end;
     9: begin // DW_LNS_fixed_advance_pc
        if RdProgX(BOffsetLoc,2,BDataU)=FALSE then break;
        BAddrNext:=FDecFsm.Addr+BDataU;
        //PropDbgParams(BAddrNext);
        FDecFsm.Addr:=BAddrNext;
        FDecFsm.OpIndex:=0;
        //ViewAny('iStd DW_LNS_fixed_advance_pc +'+IntToStr(BDataU)+' Addr: 0x'+IntToHex(BAddrNext,6),'IndexFlowLines');
        Result:=TRUE;
        end;
     else
        ViewAny('eStd opcode 0x'+IntToHex(BOpcode,2)+' is not implemented or not tested','IndexFlowLines');
     end; // case
    if Result=FALSE then break;
    Result:=FALSE;
   end
  else // Spc opcodes
   begin
   BAdjustOpcode:=BOpcode-FOpcodeBase;
   //ViewAny('iSpc '+IntToHex(BAdjustOpcode,2),'IndexFlowLines');
   BOperAdvance:=BAdjustOpcode div FLineRange;
   BAddrNext:=FDecFsm.Addr+FMinInstructionLength*((FDecFsm.OpIndex+BOperAdvance) div FMaxOperPerInstruction);
   BOpIndexNext:=(FDecFsm.OpIndex+BOperAdvance) mod FMaxOperPerInstruction;
   BLineInc:=FLineBase+(BAdjustOpcode mod FLineRange);
   FDecFsm.Move(BAddrNext,BOpIndexNext,BLineInc);
   SetDbgParams(BAddrPropB,FDecFsm.Addr); BAddrPropB:=FDecFsm.Addr;
   end;
  if FFsmEndSequence then
   begin
   if BOffsetLoc=FOffsetNextHdr then begin Result:=TRUE; break; end;
   FFsmEndSequence:=FALSE; // Next sequence
   end;
  end;
 if BOffsetLoc<>FOffsetNextHdr then begin ViewAny('eUnclosed DebugLine sequence','IndexFlowLines'); Result:=FALSE; end;
End;

// *** TElfFile ***

Constructor TElfFile.Create ( AOnViewAny : TOnViewAny; Const AFilename : string );
Begin
 Inherited Create;
 FOnViewAnyE:=AOnViewAny; FOnViewAny:=@ViewAnyFilt;
 FFilename:=AFilename;
 FRawData:=TMemoryStream.Create;
 FProgList:=TElfProgList.Create;
 FSectList:=TElfSectList.Create;
 FStrTableList:=TStrTableList.Create;
 FSymList:=TElfSymList.Create;
 FDebugLineList:=TDebugLineList.Create;
 FDebugUnitList:=TDebugUnitList.Create;
 FElfProcList:=TElfProcList.Create;
 FFileList:=TStringList.Create;
 //FDebugElf:=TStringList.Create;
End;

Destructor TElfFile.Destroy;
Begin
 Clear;
 //FDebugElf.Free;
 FFileList.Free;
 FElfProcList.Free;
 FDebugUnitList.Free;
 FDebugLineList.Free;
 FSymList.Free;
 FStrTableList.Free;
 FSectList.Free;
 FProgList.Free;
 FRawData.Free;
 Inherited;
End;

Procedure TElfFile.Clear;
Begin
 FSectNames:=nil;
 FGenStrTab:=nil;

 FProgList.Clear;
 FSectList.Clear;
 FStrTableList.Clear;
 FSymList.Clear;
 FDebugUnitList.Clear;
 FElfProcList.Clear;
End;

Procedure TElfFile.ViewAny ( Const AMessage : string; Const AReporter : string );
Begin
 ViewAnyFilt(AMessage+' [R:TElfFile.'+AReporter+']');
End;

Function RemoveReporter ( Const AMessage : string ) : string;
Var
  BIndex    : Integer;
Begin
 BIndex:=Pos('[R:',AMessage);
 if BIndex=0 then Result:=AMessage
 else Result:=Copy(AMessage,1,BIndex-1);
 DelFirstLastSpace(Result);
End;

Procedure TElfFile.ViewAnyFilt ( Const AMessage : string );
Var
  BDataCmpA,
  BDataCmpB : string;
Begin
 if (AMessage<>'') and (AMessage[1]='d') then
  begin
  BDataCmpA:='';
  //if FDebugElf.Count>0 then begin BDataCmpA:=FDebugElf.strings[0]; FDebugElf.Delete(0); end;
  BDataCmpB:=RemoveReporter(AMessage);
  if BDataCmpA<>BDataCmpB then
   begin
   ViewAny('eComparisonError '+BDataCmpA+' '+BDataCmpB,'');
   end;
  end
 else
  begin
  if Assigned(FOnViewAnyE) then FOnViewAnyE(AMessage);
  end;
End;

Function TElfFile.Load : boolean;
Begin
 Result:=FALSE;

 repeat
 try
   FRawData.LoadFromFile(FFilename);
 except
   ViewAny('eCannot load file ['+FFilename+']','Load');
   break;
 end;
 LRawDataB:=PByte(FRawData.Memory);
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.RawPtr ( AOffset : QWord ) : PByte;
Begin
 Result:=@(PByte(FRawData.Memory)[AOffset]);
End;

Generic Function TElfFile.ParseProgList<TProgHdrX> ( AOffset : Int64; AHdrSize : word; ACount : Cardinal ) : boolean;
Var
  BIndex        : Cardinal;
  BProgHdr      : TProgHdrX;
  BOffset       : Int64;
  BProgItem     : TElfProgItem;
Begin
 BOffset:=AOffset;
 BIndex:=0;
 while BIndex<ACount do
  begin
  if (BOffset+SizeOf(BProgHdr))>FRawData.Size then begin ViewAny('eReading after the end of file, item: '+IntToStr(BIndex),'ParseProgList'); break; end;
  FRawData.Position:=BOffset;
  FRawData.Read(Pointer(@BProgHdr)^,SizeOf(BProgHdr));
  if Int64(BProgHdr.FOffset)+Int64(BProgHdr.FFileSZ)>FRawData.Size then begin ViewAny('eReading after the end of file, item: '+IntToStr(BIndex),'ParseProgList'); break; end;
  BProgItem:=TElfProgItem.Create(FOnViewAny,RawPtr(BProgHdr.FOffset)); BProgItem.Fill(BProgHdr);
  FProgList.Append(BProgItem);
  BOffset:=BOffset+AHdrSize;
  inc(BIndex);
  end;

 Result:=BIndex=ACount;
End;

Generic Function TElfFile.ParseSectList<TSectHdrX> ( AOffset : Int64; AHdrSize : word; ACount : Cardinal ) : boolean;
Var
  BIndex        : Cardinal;
  BSectHdr      : TSectHdrX;
  BOffset       : Int64;
  BSectItem     : TElfSectItem;
Begin
 BOffset:=AOffset;
 BIndex:=0;
 while BIndex<ACount do
  begin
  if (BOffset+SizeOf(BSectHdr))>FRawData.Size then begin ViewAny('eReading after the end of file, item: '+IntToStr(BIndex),'ParseSectList'); break; end;
  FRawData.Position:=BOffset;
  FRawData.Read(Pointer(@BSectHdr)^,SizeOf(BSectHdr));
  BSectItem:=TElfSectItem.Create(FOnViewAny); BSectItem.Fill(BSectHdr);
  FSectList.Append(BSectItem);
  Inc(BOffset,AHdrSize);
  inc(BIndex);
  end;

 Result:=BIndex=ACount;
End;

Generic Function TElfFile.ParseSymList<TSymHdrX> ( AOffset : QWord; ASize : QWord ) : boolean;
Var
  BItemIdx,
  BItemCnt      : QWord;
  BOffset       : QWord;
  BSymHdr       : TSymHdrX;
  BSymItem      : TElfSymItem;
Begin
 Result:=FALSE;
 BItemCnt:=ASize div SizeOf(TSymHdrX);
 BOffset:=AOffset;

 BItemIdx:=0;
 while BItemIdx<BItemCnt do
  begin
  if (BOffset+SizeOf(TSymHdrX))>FRawData.Size then begin ViewAny('eReading after the end of file, item: '+IntToStr(BItemIdx),'ParseSymList'); break; end;
  FRawData.Position:=BOffset;
  FRawData.Read(Pointer(@BSymHdr)^,SizeOf(BSymHdr));
  BSymItem:=TElfSymItem.Create; BSymItem.Fill(BSymHdr);
  FSymList.Append(BSymItem);
  inc(BOffset,SizeOf(BSymHdr));
  inc(BItemIdx);
  end;
 Result:=BItemIdx=BItemCnt;
End;

Function TElfFile.PopulateSectData : boolean;
Var
  BSectIdx      : TIndex;
  BSectItem     : TElfSectItem;
Begin
 BSectIdx:=0;
 while BSectIdx<FSectList.Count do
  begin
  BSectItem:=FSectList.Items[BSectIdx];
  if BSectItem.Size<>0 then
   begin
   if (Int64(BSectItem.Offset)+Int64(BSectItem.Size))>FRawData.Size then begin ViewAny('eSection size expand outside the file size','PopulateSectData'); break; end;
   BSectItem.RawData:=RawPtr(BSectItem.Offset);
   end;
  inc(BsectIdx);
  end;
 Result:=BSectIdx=FSectList.Count;
End;

Function TElfFile.ParseStringTables : boolean;
Var
  BSectIdx      : TIndex;
  BSectItem     : TElfSectItem;
  BStrTable     : TStringList;
  BRdByte       : byte;
  BOffset       : QWord;
  BDataLen,
  BDataIdx      : TIndex;
  BRdLine       : string;
Begin
 Result:=TRUE;
 FSectNames:=nil;
 BSectIdx:=0;
 while BSectIdx<FSectList.Count do
  begin
  BSectItem:=FSectList.Items[BSectIdx];
  if BSectItem.SectType=SHT_STRTAB then
   begin
   BStrTable:=TStringList.Create; FStrTableList.Append(BStrTable);
   if BSectIdx=FElfHdrTail.FNameIdx then FSectNames:=BSectItem;
   BRdLine:='';
   BOffset:=BSectItem.Offset; BDataLen:=BSectItem.Size;
   BDataIdx:=0;
   while BDataIdx<BDataLen do
    begin
    if BOffset>=FRawData.Size then begin ViewAny('eReading data after the end of file','ParseStringTable'); Result:=FALSE; break; end;
    BRdByte:=LRawDataB[BOffset]; Inc(BOffset);
    if BRdByte=0 then begin BStrTable.Append(BRdLine); BRdLine:=''; end
    else BRdLine:=BRdLine+Char(BRdByte);
    Inc(BDataIdx);
    end;
   if BRdLine<>'' then BStrTable.Append(BRdLine);
   end;
  inc(BSectIdx);
  end;
End;


Function TElfFile.PopulateSectNames : boolean;
Var
  BSectIdx      : TIndex;
  BSectItem     : TElfSectItem;
  BNameOffset   : Cardinal;
Begin
 Result:=FALSE;
 repeat
 if FSectNames=nil then begin ViewAny('eSection names segment is not defined or absent','PopulateSectNames'); break; end;
 BSectIdx:=0;
 while BSectIdx<FSectList.Count do
  Begin
  BSectItem:=FSectList.Items[BSectIdx];
  BNameOffset:=BSectItem.NameIdx;
  if BNameOffset>=FSectNames.Size then begin ViewAny('eSection name index out of range','PopulateSectNames'); break; end;
  BSectItem.NameS:=FSectNames.RdStrA(BNameOffset);
   //ViewAny('i "+BSectItem->GetNameS()+" "+std::to_string(BSectItem->GetType()),'');
  inc(BSectIdx);
  end;
 if BSectIdx<>FSectList.Count then break;
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.FindSectByName ( Const AName : string ) : TElfSectItem;
Var
  BSectIdx      : TIndex;
  BSectItem     : TElfSectItem;
Begin
 Result:=nil;

 BSectIdx:=0;
 while BSectIdx<FSectList.Count do
  begin
  BSectItem:=FSectList.Items[BSectIdx];
  if BSectItem.NameS=AName then begin Result:=BSectItem; break; end;
  inc(BSectIdx);
  end;
End;

Function TElfFile.GetCuHeaderSize : QWord;
Begin
 if FElfHdrBase.FDataType=1 then Result:=SizeOf(TCuHeader32) else Result:=SizeOf(TCuHeader64);
End;

Function TElfFile.ProcessSymTable : boolean;
Var
  BSectItem     : TElfSectItem;
Begin
 Result:=FALSE;
 repeat
 BSectItem:=FindSectByName('.symtab'); if BSectItem=nil then begin ViewAny('eSymbol table is not found inside the ELF file','ProcessSymTable'); break; end;
 if FElfHdrBase.FDataType=1 then
  begin
  if specialize ParseSymList<TElfSym32>(BSectItem.Offset,BSectItem.Size)=FALSE then break;
  end
 else if FElfHdrBase.FDataType=2 then
  begin
  if specialize ParseSymList<TElfSym64>(BSectItem.Offset,BSectItem.Size)=FALSE then break;
  end
 else begin ViewAny('eInvalid data type','ProcessSymTable'); break; end;
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.ResolveSymNames : boolean;
Var
  BSymIdx       : TIndex;
  BSymItem      : TElfSymItem;
  BNameOffset   : QWord;
Begin
 Result:=FALSE;
 repeat
 FGenStrTab:=FindSectByName('.strtab'); if FGenStrTab=nil then begin ViewAny('eString table is not found inside the ELF file','ResolveSymNames'); break; end;
 //ViewAny('i ','');
 //ViewAny('i Symbol table ','');
 BSymIdx:=0;
 while BSymIdx<FSymList.Count do
  begin
  BSymItem:=FSymList.Items[BSymIdx];
  BNameOffset:=BSymItem.NameOffset;
  if BNameOffset>=FGenStrTab.Size then begin ViewAny('eSection name index out of range','ResolveSymNames'); break; end;
  BSymItem.NameS:=FGenStrTab.RdStrA(BNameOffset);
  //ViewAny('i '+BSymItem->VerboseAll(),'');
  inc(BSymIdx);
  end;
 if BSymIdx<>FSymList.Count then break;
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.ProcessDebugAbbrev : boolean;
Var
  BSectItem     : TElfSectItem;
Begin
 Result:=FALSE;
 FAbbrevSect:=nil;

 repeat
 BSectItem:=FindSectByName('.debug_abbrev'); if BSectItem=nil then begin ViewAny('edebug_abbrev section is not found inside the ELF file','ProcessDebugAbbrev'); break; end;
 FAbbrevSect:=BSectItem;
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.ProcessDebugStr : boolean;
Var
  BSectItem     : TElfSectItem;
Begin
 Result:=FALSE;
 FDebugStrSect:=nil;

 repeat
 BSectItem:=FindSectByName('.debug_str'); if BSectItem=nil then begin ViewAny('edebug_str section is not found inside the ELF file','ProcessDebugStr'); break; end;
 FDebugStrSect:=BSectItem;
 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.ProcessDebugLineStr : boolean;
Begin
 FDebugLineStrSect:=FindSectByName('.debug_line_str');
 Result:=TRUE;
End;

Function TElfFile.ProcessDebugInfo : boolean;
Var
  BSectItem     : TElfSectItem;
  BRdIdx        : QWord;
  BUnit         : TDebugInfoUnit;
  BError        : boolean;
  BUnitIdx      : TIndex;
  BInfoIdx,
  BProcIdx      : TIndex;
  BInfoUnit,
  BInfoProc     : TDebugInfoItem;
  BAbbrevUnit,
  BAbbrevProc   : TDebugAbbrevItem;
  BParamAddr,
  BParamName    : TDebugInfoParam;
  BProcAddr     : TAddr;
  BProcName     : string;
  BElfProc      : TElfProc;
Begin
 Result:=FALSE;

 repeat
 BSectItem:=FindSectByName('.debug_info'); if BSectItem=nil then begin ViewAny('eDebugInfo section is not found inside the ELF file','ProcessDebugInfo'); break; end;
 BRdIdx:=0;
 while BRdIdx<BSectItem.Size do
  begin
  BUnit:=TDebugInfoUnit.Create(FOnViewAny,BSectItem,FAbbrevSect,FDebugStrSect,FDebugLineStrSect); FDebugUnitList.Append(BUnit);
  if BUnit.Parse(BRdIdx)=FALSE then break;
  if BRdIdx=BSectItem.Size then begin Result:=TRUE; break; end;
  end;

 if Result=FALSE then break;

 BError:=FALSE;
 FElfProcList.Clear;
 BUnitIdx:=0;
 while BUnitIdx<FDebugUnitList.Count do
  begin
  BUnit:=FDebugUnitList.Items[BUnitIdx];
  BInfoIdx:=0;
  while BInfoIdx<BUnit.InfoList.Count do
   begin
   BInfoUnit:=BUnit.InfoList.Items[BInfoIdx];
   BAbbrevUnit:=BInfoUnit.Abbrev;
   if BAbbrevUnit.Tag=$11 then // CompileUnit
    begin
    BProcIdx:=0;
    while BProcIdx<BInfoUnit.ChildList.Count do
     begin
     BInfoProc:=BInfoUnit.ChildList.Items[BProcIdx];
     BAbbrevProc:=BInfoProc.Abbrev;
     if BAbbrevProc.Tag=$2E then // Subprogram
      begin
      // Proc addr
      BParamAddr:=BInfoProc.FindParam($11); // DW_AT_low_pc
      BParamName:=BInfoProc.FindParam($03); // DW_AT_name
      if (BParamAddr<>nil) and (BParamName<>nil) then
       begin
       if BParamAddr.DataType<>6 then begin BError:=TRUE; ViewAny('eError in ELF file: the base address of subroutine is indicated but has an incorrect type','ProcessDebugInfo'); break; end;
       BProcAddr:=TAddr(BParamAddr.DataU);
       if BParamName.DataType<>0 then begin BError:=TRUE; ViewAny('eError in ELF file: the name of subroutine is indicated but has an incorrect type','ProcessDebugInfo'); break; end;
       BProcName:=BParamName.DataS;
       if BProcAddr=$00000000 then
       else if IsAddrLoadable(BProcAddr)=FALSE then
       else begin BElfProc:=TElfProc.Create(BProcAddr,BProcName); FElfProcList.Append(BElfProc); {ViewAny('i'+IntToHex(BProcAddr,6)+': '+BProcName,'ProcessDebugInfo');} end;
       end;
      end;
     inc(BProcIdx);
     end;
    end;
   if BError then break;
   inc(BInfoIdx);
   end;
  if BError then break;
  inc(BUnitIdx);
  end;
 Result:=BUnitIdx=FDebugUnitList.Count;
 until TRUE;
End;

Function TElfFile.ParseDbgLines : boolean;
Var
  BDbgLines     : TElfSectItem;
  BOffsetAll,
  BOffsetLoc,
  BSize         : TIndex;
  BLineItem     : TDebugLineItem;
Begin
 Result:=FALSE;

 repeat
 BDbgLines:=FindSectByName('.debug_line'); if BDbgLines=nil then begin ViewAny('eDebug Lines section is not found inside the ELF file','ParseDbgLines'); break; end;
 BSize:=BDbgLines.Size;
 BOffsetAll:=BDbgLines.Offset; BOffsetLoc:=0;
 while BOffsetLoc<BSize do
  begin
  if (BOffsetAll+BOffsetLoc+GetCuHeaderSize())>FRawData.Size then begin ViewAny('eReading after the end of the ELF file','ParseDbgLines'); break; end;
  BLineItem:=TDebugLineItem.Create(Self,FOnViewAny,@LRawDataB[BOffsetAll+BOffsetLoc],@SetDbgParams,FElfHdrBase.FDataType); FDebugLineList.Append(BLineItem); BLineItem.ReadHeader;
  if (BOffsetAll+BOffsetLoc+BLineItem.DataLen)>FRawData.Size then begin ViewAny('eReading after the end of the ELF file','ParseDbgLines'); break; end;
  if BLineItem.Parse=FALSE then break;
  BOffsetLoc:=BOffsetLoc+BLineItem.OffsetNextHdr;
  end;
 if BOffsetLoc<>BSize then break;

 Result:=TRUE;
 until TRUE;
End;

Function TElfFile.IndexFlowLines : boolean;
Var
  BDbgIdx   : TIndex;
  BDbgItem  : TDebugLineItem;
Begin
 BDbgIdx:=0;
 while BDbgIdx<FDebugLineList.Count do
  begin
  BDbgItem:=FDebugLineList.Items[BDbgIdx];
  if BDbgItem.IndexFlowLines=FALSE then break;
  inc(BDbgIdx);
  end;

 Result:=BDbgIdx=FDebugLineList.Count;
End;


Function TElfFile.IsAddrLoadable ( AAddr : TAddr ) : boolean;
Var
   BProgIdx     : TIndex;
   BProgItem    : TElfProgItem;
Begin
 Result:=FALSE;
 BProgIdx:=0;
 while BProgIdx<FProgList.Count do
  begin
  BProgItem:=FProgList.Items[BProgIdx];
  if BProgItem.IsInside(AAddr) then begin Result:=TRUE; break; end;
  inc(BProgIdx);
  end;
End;

Function TElfFile.FileListSearchAppend ( Const AFileName : string ) : TIndex;
Begin
 Result:=0;
 while Result<FFileList.Count do
  begin
  if FFileList.Strings[Result]=AFileName then break;
  inc(Result);
  end;
 if Result>=FFileList.Count then FFileList.Append(AFileName);
End;

Procedure TElfFile.SetDbgParamsA ( AAddr : TAddr; AFileIdx : TIndex; ALine, APos : Cardinal );
Var
   BProgIdx     : TIndex;
   BProgItem    : TElfProgItem;
Begin
 BProgIdx:=0;
 while BProgIdx<FProgList.Count do
  begin
  BProgItem:=FProgList.Items[BProgIdx];
  if BProgItem.TrySetDbgParams(AAddr,AFileIdx,ALine,APos) then break;
  inc(BProgIdx);
  end;
End;

Function TElfFile.GetDbgParamsA ( AAddr : TAddr; Out AFileIdx : TIndex; Out ALine, APos : Cardinal ) : boolean;
Var
   BProgIdx     : TIndex;
   BProgItem    : TElfProgItem;
Begin
 Result:=FALSE;
 AFileIdx:=0; ALine:=0; APos:=0;
 BProgIdx:=0;
 while BProgIdx<FProgList.Count do
  begin
  BProgItem:=FProgList.Items[BProgIdx];
  if BProgItem.TryGetDbgParams(AAddr,AFileIdx,ALine,APos) then begin Result:=TRUE; break; end;
  inc(BProgIdx);
  end;
End;

Procedure TElfFile.SetDbgParams ( AAddrB, AAddrE : TAddr; Const ASrcFile : string; ALine, APos : Cardinal );
Var
  BFileIdx     : TIndex;
  BAddr        : TAddr;
Begin
 BFileIdx:=FileListSearchAppend(ASrcFile);
 BAddr:=AAddrE;
 repeat
 SetDbgParamsA(BAddr,BFileIdx,ALine,APos);
 if BAddr<2 then break;
 if AAddrB=0 then break;
 dec(BAddr,2); if BAddr<=AAddrB then break;
 until FALSE;
End;

Function TElfFile.ParseAll : boolean;
Var
  BElfHdr32     : TElfHdr32;
  BElfHdr64     : TElfHdr64;
Begin
 Result:=FALSE;
 FRawData.Position:=0;
 //FDebugElf.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(FFilename))+'DebugElf.txt');
 repeat
 if FRawData.Size<SizeOf(TElfHdrBase) then begin ViewAny('eFile size is too small','ParseAll'); break; end;
 FRawData.Read(FElfHdrBase,SizeOf(TElfHdrBase));
 if FElfHdrBase.FMagicNr<>$464C457F then begin ViewAny('eIncorrect file type','ParseAll'); break; end;
 if FElfHdrBase.FEndian<>1 then begin ViewAny('eOnly LittleEndian is supported','ParseAll'); break; end;
 if FElfHdrBase.FDataType=1 then
  begin
  if FRawData.Size<SizeOf(TElfHdrBase)+SizeOf(TElfHdr32)+SizeOf(TElfHdrTail) then begin  ViewAny('File size is too small','ParseAll'); break; end;
  FRawData.Read(Pointer(@BElfHdr32)^,SizeOf(TElfHdr32));
  FProgramEntry:=BElfHdr32.FProgramEntry;
  FProgHdrTablePos:=BElfHdr32.FProgHdrTablePos;
  FSectHdrTablePos:=BElfHdr32.FSectHdrTablePos;
  FRawData.Read(Pointer(@FElfHdrTail)^,SizeOf(TElfHdrTail));
  if specialize ParseProgList<TElfProgHdr32>(FProgHdrTablePos,FElfHdrTail.FProgEntrySize,FElfHdrTail.FProgEntryNr)=FALSE then break;
  if specialize ParseSectList<TElfSectHdr32>(FSectHdrTablePos,FElfHdrTail.FSectEntrySize,FElfHdrTail.FSectEntryNr)=FALSE then break;
  end
 else if FElfHdrBase.FDataType=2 then
  begin
  if FRawData.Size<SizeOf(TElfHdrBase)+SizeOf(TElfHdr32)+SizeOf(TElfHdrTail) then begin  ViewAny('File size is too small','ParseAll'); break; end;
  FRawData.Read(Pointer(@BElfHdr64)^,SizeOf(TElfHdr64));
  FProgramEntry:=BElfHdr64.FProgramEntry;
  FProgHdrTablePos:=BElfHdr64.FProgHdrTablePos;
  FSectHdrTablePos:=BElfHdr64.FSectHdrTablePos;
  FRawData.Read(Pointer(@FElfHdrTail)^,SizeOf(TElfHdrTail));
  if specialize ParseProgList<TElfProgHdr64>(FProgHdrTablePos,FElfHdrTail.FProgEntrySize,FElfHdrTail.FProgEntryNr)=FALSE then break;
  if specialize ParseSectList<TElfSectHdr64>(FSectHdrTablePos,FElfHdrTail.FSectEntrySize,FElfHdrTail.FSectEntryNr)=FALSE then break;
  end
 else
  begin
  ViewAny('eInvalid data length','ParseAll');
  break;
  end;
 if PopulateSectData=FALSE then break;
 if ParseStringTables=FALSE then break;
 if PopulateSectNames=FALSE then break;
 if ProcessSymTable=FALSE then break;
 if ResolveSymNames=FALSE then break;
 if ProcessDebugAbbrev=FALSE then break;
 if ProcessDebugStr=FALSE then break;
 if ProcessDebugLineStr=FALSE then break;
 if ProcessDebugInfo=FALSE then break;
 if ParseDbgLines=FALSE then break;
 if IndexFlowLines=FALSE then break;
 {
 if UploadProgData=FALSE then break;}

 Result:=TRUE;
 until TRUE;
End;

Procedure TElfFile.MapLocPath ( ALocList : TStringList );
Var
  BSrcList  : TStringList;
  BExtList  : string;
  BPathA,
  BNameA,
  BExtA     : string;
  BReadS    : string;
  BFullName : string;
  BFileIdx  : TIndex;
  BPathIdx  : TIndex;
Begin
 BSrcList:=TStringList.Create;

 // Collect all possible extensions
 BExtList:='';
 BFileIdx:=0;
 while BFileIdx<FFileList.Count do
  begin
  repeat
  BReadS:=FFileList.Strings[BFileIdx]; BFullName:=ReadParamStr(BReadS); if BFullName='' then break;
  SplitFilename(BFullName,BPathA,BNameA,BExtA); if BExtA='' then break;
  if StrInList(BExtA,BExtList)=FALSE then BExtList:=BExtList+BExtA+' ';
  until TRUE;
  inc(BFileIdx);
  end;
 Trim(BExtList);
 // Obtain file list
 BPathIdx:=0;
 while BPathIdx<ALocList.Count do
  begin
  FileList(ALocList.Strings[BPathIdx],BExtList,BSrcList);
  inc(BPathIdx);
  end;
 // Map each file
 BFileIdx:=0;
 while BFileIdx<FFileList.Count do
  begin
  BReadS:=FFileList.Strings[BFileIdx]; BFullName:=ReadParamStr(BReadS);
  FFileList.Strings[BFileIdx]:=BFullName+' '+BestFileFit(BFullName,BSrcList);
  inc(BFileIdx);
  end;

 BSrcList.Free;
End;

Procedure TElfFile.ResolveSrc ( AAddr : Cardinal; Var ASrcName : string; Var ASrcLine : Integer );
Var
  BFileIdx  : TIndex;
  BLine,
  BPos      : Cardinal;
  BReadS,
  BFileName : string;
Begin
 ASrcName:=''; ASrcLine:=0;
 repeat
 if GetDbgParamsA(AAddr,BFileIdx,BLine,BPos)=FALSE then break;
 if BFileIdx>=FFileList.Count then break;
 BReadS:=FFileList.Strings[BFileIdx];
 ReadParamStr(BReadS); BFileName:=ReadParamStr(BReadS);
 if BLine>0 then Dec(BLine); // In ELF it starts from 1
 ASrcName:=BFileName; ASrcLine:=BLine;
 until TRUE;
End;

Function TElfFile.FindSymName ( AAddr : TAddr; AType : byte ) : string;
Var
  BSymIdx   : TIndex;
  BSymItem  : TElfSymItem;
Begin
 Result:='';

 BSymIdx:=0;
 while BSymIdx<FSymList.Count do
  begin
  BSymItem:=FSymList.Items[BSymIdx];
  if (BSymItem.SymType=AType) and (BSymItem.Value=AAddr) then
   begin
   Result:=BSymItem.NameS;
   break;
   end;
  inc(BSymIdx);
  end;
End;

Function TElfFile.FindProcName ( AAddr : TAddr ) : string;
Var
  BProcIdx  : TIndex;
  BElfProc  : TElfProc;
Begin
 Result:='';

 BProcIdx:=0;
 while BProcIdx<FElfProcList.Count do
  begin
  BElfProc:=FElfProcList.Items[BProcIdx];
  if BElfProc.AddrS=AAddr then
   begin
   Result:=BElfProc.ProcName;
   break;
   end;
  inc(BProcIdx);
  end;
End;

Function TElfFile.IsProc ( AAddr : TAddr ) : boolean;
Var
  BProcIdx  : TIndex;
  BElfProc  : TElfProc;
Begin
 Result:=FALSE;

 BProcIdx:=0;
 while BProcIdx<FElfProcList.Count do
  begin
  BElfProc:=FElfProcList.Items[BProcIdx];
  if BElfProc.AddrS=AAddr then
   begin
   Result:=TRUE;
   break;
   end;
  inc(BProcIdx);
  end;
End;

end.

