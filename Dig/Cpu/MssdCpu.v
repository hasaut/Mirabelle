module MssdCpu #(parameter CIoSpace = 32'h300, CIrqCnt = 16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AExecEn, input ACoreEn,
  output [31:0] ACodeAddr, input [63:0] ACodeMiso, output ACodeReq, input ACodeAck,
  output [31:0] ADataAddr, input [63:0] ADataMiso, output [63:0] ADataMosi, output [3:0] ADataWrSize, ADataRdSize, input ADataAck,
  output [15:0] APortAddr, input [63:0] APortMiso, output [63:0] APortMosi, output [3:0] APortWrSize, APortRdSize, input APortAck,
  input ASysCoreSel, output [31:3] AContPtrMiso, output AIsIsr, input AContPtrWrEn,
  output ASetIrqSwtBase,
  output [CIrqCnt-1:0] AIrqBusyList, input [CIrqCnt-1:0] AIrqToProcess, // Avoid IRQ re-entering
  input [63:0] ARegMosi, output [63:0] ARegMiso, input [7:0] ARegRdIdx, ARegWrIdx,
  output ATEnd, ATrap, output [31:0] AIpThis, output ACmdDecReady,
  // Sys Req/Ack {unlock, lock, end, Swt}
  output [2:0] ASysReq, input ASysAck, output ASiLock,
  output AUnityReq, input AUnityAck,
  output AIrqEn
 );

 function IsColRowConflict ( input [11:0] AColRowA, AColRowB );
   IsColRowConflict = (|(AColRowA[11:8] & AColRowB[11:8])) & (|(AColRowA[7:0] & AColRowB[7:0]));
 endfunction

 localparam CVliwLen = 119;
 localparam CVliwNil = {CVliwLen{1'b0}};

 // SysCtrl
 wire [31:3] FContPtr, BContPtr; // Pointer to a current context
 wire [CIrqCnt-1:0] FIrqBusyList, BIrqBusyList; // Avoid IRQ re-enterance
 // Exec state (multi-state commands)
 localparam CStateLen = 10;
 localparam CStateNil = {CStateLen{1'b0}};
 wire [CStateLen-1:0] FState, BState;
 wire [15:0] FPplList, BPplList;
 wire [2:0] FSysReq, BSysReq;
 wire FSiLock, BSiLock;
 wire FStateNZ, BStateNZ;
 wire [CVliwLen-1:0] FVliw, BVliw;
 wire [23:1] FIpCache, BIpCache;
 wire FKeepIpCache, BKeepIpCache;
 wire [7:0] FLoopDData, BLoopDData;

 MsDffList #(.CRegLen(29+CIrqCnt+CStateLen+16+3+1+1+CVliwLen+23+1+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BContPtr, BIrqBusyList, BState, BPplList, BSysReq, BSiLock, BStateNZ, BVliw, BIpCache, BKeepIpCache, BLoopDData}),
   .ADataO({FContPtr, FIrqBusyList, FState, FPplList, FSysReq, FSiLock, FStateNZ, FVliw, FIpCache, FKeepIpCache, FLoopDData})
  );

 // CmdDecoder
 //          Final         Decoded    | if Conflict then Final:=0 else Final:=Decoded;
 wire  [1:0] MCmdLen;

 // VLIW
 wire  [3:0] LCond,        MCond;
 wire        LLoadEipImm,  MLoadEipImm;
 wire  [1:0] LTrap,        MTrap;
 wire  [4:0] LSysReq,      MSysReq;
 wire  [5:0] LRegIdxS,     MRegIdxS;
 wire  [5:0] LRegIdxU,     MRegIdxU;
 wire  [1:0] LWwConst,     MWwConst;
 wire [31:0] LConst,       MConst;
 wire  [4:0] LMlsc,        MMlsc;
 wire  [2:0] LLoopD,       MLoopD;      // BusB BusA
 wire  [7:0] LMuxSrc,      MMuxSrc;
 wire  [2:1] LSelIp,       MSelIp;
 wire  [5:0] LRegIdxR,     MRegIdxR;
 wire        LDstFlagWr,   MDstFlagWr;
 wire        LAluSignExt,  MAluSignExt;
 wire  [3:0] LAluSelA,     MAluSelA;
 wire  [7:0] LAluSelU,     MAluSelU;
 wire  [3:0] LAluSelS,     MAluSelS;
 wire  [3:0] LAluSelT,     MAluSelT;
 wire  [6:0] LAluSelF,     MAluSelF;
 wire  [1:0] LMioWrRdEn,   MMioWrRdEn;
 wire  [1:0] LMioSize,     MMioSize;
 wire  [2:0] LMioSignExt,  MMioSignExt;
 wire        LUnityReq,    MUnityReq;

 // Bus A, B
 wire [63:0] BBusAData; wire [11:0] BBusALoad; wire [3:0] BBusFData; wire BBusFLoad;
 wire [63:0] BBusBData; wire [11:0] BBusBLoad, BBusBPend; wire BDevBPendAny;

 // Buses
 wire [7:0] BRegWrIdx = ASysCoreSel ? ARegWrIdx : 8'h0;
 wire [7:0] BRegRdIdx = ASysCoreSel ? ARegRdIdx : 8'h0;
 wire BRegWrIdxNZ = |BRegWrIdx;
 wire [11:0] BRegWrIdxA = BBusALoad;
 wire [11:0] BRegWrIdxB = BBusBLoad | {{4{BRegWrIdxNZ}}, BRegWrIdx};
 wire [63:0] BRegWrBusA = BBusAData;
 wire [63:0] BRegWrBusB = BBusBData | (BRegWrIdxNZ ? ARegMosi : 64'h0);

 wire [23:1] BEipWrBus;

 wire [31:0] BMioAddr;

 // Registers
 wire [511:0] LRegsThis;
 wire [23:1] LEipThis  = LRegsThis[23:1];
 wire  [7:0] LFlagsThis = LRegsThis[31:24];

 // State
 assign BStateNZ = |FState;
 // SiLock
 assign BSiLock = (FSiLock | LSysReq[2]) & ~LSysReq[3];

 // Flags
 wire [3:0] LFlagsVNZC = LFlagsThis[3:0];
 wire [3:0] BFlagsVNZC = BBusFLoad ? BBusFData : LFlagsVNZC;
 wire BFlWrEnA = (BRegWrIdxA[11:8]==4'b0010) & BRegWrIdxA[0];
 wire BFlagWrAny = |{BRegWrIdx[0], BFlWrEnA};
 wire [7:0] BFlagWrBus =
  (BRegWrIdx[0] ? BRegWrBusB[31:24] : 8'h0) |
  (BFlWrEnA ? BRegWrBusA[15:8] : 8'h0) |
  (BFlagWrAny ? 8'h0 : {LFlagsThis[7:4], BFlagsVNZC});
 //wire BFlagWrReq = BBusFLoad | BFlagWrAny;
 wire [1:0] LCpuType = LFlagsThis[5:4];


 // EIP
 wire BCmdValid;
 wire [23:1] BIpLoadBus;
 wire BStepNextNZ = |BState;
 wire BEipUpdateA = &{BRegWrIdxA[10:8], BRegWrIdxA[0]};
 wire BEipUpdateB = &{BRegWrIdxB[10:8], BRegWrIdxB[0]};
 //wire BEipLoadImpass = BEipUpdateA | BEipUpdateB;
 wire BJmpEn; MsCondAnalyzer UJmpEn ( .ACond(LCond), .AFlags(LFlagsVNZC), .AJmpEn(BJmpEn) );
 wire BEipJmpUpdate = LLoadEipImm & BJmpEn;
 wire [23:1] BEipNext = LEipThis + {21'h0, (BStepNextNZ | ~BCmdValid) ? 2'h0 : MCmdLen};
 wire BEipUpdateAny = |{BEipUpdateB, BEipUpdateA, BEipJmpUpdate};
 assign BEipWrBus =
  (BEipUpdateB ? BRegWrBusB[23:1] : 23'h0) |  // Tail
  (BEipUpdateA ? BRegWrBusA[23:1] : 23'h0) |  // Tail
  (BEipJmpUpdate ? BIpLoadBus : 23'h0) |  // Mux
  (BEipUpdateAny ? 23'h0 : BEipNext);         // Mux
 assign BIpCache = BEipJmpUpdate ? BEipNext : FIpCache;
 assign BKeepIpCache = BEipJmpUpdate | (FKeepIpCache & ~BCmdValid);

 // SysReq
 assign BSysReq = (FSysReq & {3{~ASysAck}}) | {LSysReq[2:1], BJmpEn & LSysReq[0]}; // Lock End Swt

 // CmdQueue
 wire [23:1] BQueLda; // Load address
 wire [127:0] BQueTopD;
 MsCmdQue UCmdQue
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AQueLda(BQueLda), .AQueTop(BQueTopD),
   .ACodeMiso(ACodeMiso), .ACodeAck(ACodeAck),
   .AEipUpdate(BEipUpdateAny), .AEipWrBus(BEipWrBus)
  );

 // CmdQue mux
 wire [3:0] BQueLenA = BQueLda[4:1]-LEipThis[4:1];
 wire [1:0] BQueLen = (|BQueLenA[3:2]) ? 2'h3 : BQueLenA[1:0];
 wire BQueLenNZ = |BQueLen;

 assign ACodeReq = ACoreEn & (BEipUpdateAny | (((BQueLda[4:3]-BEipNext[4:3])<2'h2) & ~ACodeAck));
 assign ACodeAddr = BEipUpdateAny ? {8'h0, BEipWrBus[23:3], 3'h0} : {8'h0, BQueLda[23:3], 3'h0};
 wire [95:0] BQueTopC = LEipThis[3] ? {BQueTopD[31:0], BQueTopD[127:64]} : BQueTopD[95:0];
 wire [63:0] BQueTopB = LEipThis[2] ? BQueTopC[95:32] : BQueTopC[63:0];
 wire [47:0] BQueTopA = LEipThis[1] ? BQueTopB[63:16] : BQueTopB[47:0];
 wire [47:0] BQueTop = BQueTopA;

 // Decoder
 wire BCmdLenValid;
 wire BMioPendAny; wire [CStateLen-1:0] BStepNextM;
 MsCmdDec UCmdDec
  (
   .AQueTop(BQueTop), .AIpThis(LEipThis), .ACpuType(LCpuType),
   // CmdLen
   .ACmdLen(MCmdLen),
   // Vliw
   .ACond(MCond), .ALoadEipImm(MLoadEipImm),
   .ATrap(MTrap), .ASysReq(MSysReq),
   .ARegIdxS(MRegIdxS), .ARegIdxU(MRegIdxU),
   .AWwConst(MWwConst), .AConst(MConst), .AMlsc(MMlsc), .ALoopD(MLoopD), .AMuxSrc(MMuxSrc), .ASelIp(MSelIp), .ARegIdxR(MRegIdxR), .ADstFlagWr(MDstFlagWr),
   .AAluSignExt(MAluSignExt), .AAluSelA(MAluSelA), .AAluSelU(MAluSelU), .AAluSelS(MAluSelS), .AAluSelT(MAluSelT), .AAluSelF(MAluSelF),
   .AMioWrRdEn(MMioWrRdEn), .AMioSize(MMioSize), .AMioSignExt(MMioSignExt),
   // Multi-Step
   .ACmdLenValid(BCmdLenValid), .AStepThis(FState), .AStepNext(BStepNextM),
   .APplListThis(FPplList), .APplListNext(BPplList),
   .AUnityReq(MUnityReq), .AUnityAck(AUnityAck),
   .AMemPend(BMioPendAny)
  );

 wire [11:0] MColRowS; MsDecColRow UColRowMS ( .ARegIdx(MRegIdxS), .ASelIp(MSelIp[2]), .AColRow(MColRowS) );
 wire [11:0] MColRowU; MsDecColRow UColRowMU ( .ARegIdx(MRegIdxU), .ASelIp(MSelIp[1]), .AColRow(MColRowU) );
 wire [11:0] MColRowR; MsDecColRow UColRowMR ( .ARegIdx(MRegIdxR), .ASelIp(1'b0),      .AColRow(MColRowR) );

 wire [11:0] LColRowS; MsDecColRow UColRowLS ( .ARegIdx(LRegIdxS), .ASelIp(LSelIp[2]), .AColRow(LColRowS) );
 wire [11:0] LColRowU; MsDecColRow UColRowLU ( .ARegIdx(LRegIdxU), .ASelIp(LSelIp[1]), .AColRow(LColRowU) );
 wire [11:0] LColRowR; MsDecColRow UColRowLR ( .ARegIdx(LRegIdxR), .ASelIp(1'b0),      .AColRow(LColRowR) );

 wire BPendAnyA = |BDevBPendAny;
 wire [11:0] BPendMask = BPendAnyA ? BBusBPend : 12'h0;
 wire [2:0] BConflictColRow =
  {
   IsColRowConflict(BPendMask, MColRowS),
   IsColRowConflict(BPendMask, MColRowU),
   IsColRowConflict(BPendMask, MColRowR)
  };
 wire BRReqDevB = |{MAluSelU, MAluSelF, MMioWrRdEn}; // RReq = "Resource request"
 wire BPendDevB = |BDevBPendAny;
 wire BConflictAny = |{BConflictColRow, BRReqDevB & BPendDevB, BEipUpdateAny, BPendMask==12'h701, BPendAnyA & (|MSysReq), LSysReq};
 assign BCmdLenValid = (BQueLen>=MCmdLen) & BQueLenNZ;
 assign BCmdValid = BStateNZ | (&{BCmdLenValid, ~BConflictAny, AExecEn, ACoreEn});
 assign BState = BCmdValid ? BStepNextM : CStateNil;

 assign
  {
   LCond, LLoadEipImm, LTrap, LSysReq,
   LRegIdxS, LRegIdxU, LWwConst, LConst, LMlsc, LLoopD, LMuxSrc, LSelIp, LRegIdxR, LDstFlagWr,
   LAluSignExt, LAluSelA, LAluSelU, LAluSelS, LAluSelT, LAluSelF,
   LMioWrRdEn, LMioSize, LMioSignExt,
   LUnityReq
  } = FVliw;
 assign BVliw = BCmdValid ?
  {
   MCond, MLoadEipImm, MTrap, MSysReq,
   MRegIdxS, MRegIdxU, MWwConst, MConst, MMlsc, MLoopD, MMuxSrc, MSelIp, MRegIdxR, MDstFlagWr,
   MAluSignExt, MAluSelA, MAluSelU, MAluSelS, MAluSelT, MAluSelF,
   MMioWrRdEn, MMioSize, MMioSignExt,
   MUnityReq
  } : CVliwNil;

 // Sys
 assign BContPtr = (ASysCoreSel & AContPtrWrEn) ? ADataMiso[31:3] : FContPtr;
 assign BIrqBusyList = (ASysCoreSel & AContPtrWrEn) ? AIrqToProcess : FIrqBusyList;

 // Registers
 MssdRegTask URegTask
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(BRegWrBusA), .ARegWrIdxA(BRegWrIdxA),
   .ARegWrBusB(BRegWrBusB), .ARegWrIdxB(BRegWrIdxB),
   .AFlagWrBus(BFlagWrBus),
   .AEipWrBus(BEipWrBus),
   .ARegsThis(LRegsThis)
  );

 // RegIdxX takes in account ARegRdIdx
 wire BRegRdIdxNZ = |BRegRdIdx;
 wire [11:0] BRegRdIdxS = LMioWrRdEn[0] ? 12'h0 : LColRowS;
 wire [11:0] BRegRdIdxU = {{4{BRegRdIdxNZ}}, BRegRdIdx} | LColRowU;

 // Reg muxes
 wire [63:0] BRegMuxS;
 MssdRegMux URegMuxS
  (
   .ARegFile({LRegsThis[511:32], 8'h0, LMuxSrc[3] ? FIpCache : LEipThis, 1'b0}), .ARegRdIdx(BRegRdIdxS),
   .AMux(BRegMuxS)
  );

 wire [63:0] BRegMuxUX;
 MssdRegMux URegMuxU
  (
   .ARegFile({LRegsThis[511:32], 32'h0}), .ARegRdIdx(BRegRdIdxU),
   .AMux(BRegMuxUX)
  );

 wire [63:0] BRegMuxU =
   // Alu and Addr
   BRegMuxUX |
   {56'h0, (BRegRdIdxU==12'h201) ? LRegsThis[31:24] : 8'h0} |
   {32'h0, LSelIp[1] ? {8'h0, LEipThis, 1'b0} : 8'h0} |
   // Sys
   {32'h0, BRegRdIdx[0] ? LRegsThis[31:0] : 32'h0};

 wire [31:0] BMlsc = {{28{LMlsc[4]}}, LMlsc[3:0]};

 wire [1:0] BWwS = (LMuxSrc[0] ? LRegIdxS[5:4] : 2'h0) | ((|LMuxSrc[2:1]) ? LWwConst : 2'h0);
 wire [1:0] BWwU = LRegIdxU[5:4];
 wire [1:0] BWwR = LRegIdxR[5:4];

 wire [31:0] BAluXMuxS;
 wire [31:0] BAluXMuxSA = (LMuxSrc[0] ? BRegMuxS[31:0] : 32'h0) |
                          (LMuxSrc[1] ? LConst : 32'h0) |
                          (LMuxSrc[2] ? BMlsc : 32'h0);
 MsSignExt UAluXMuxS ( .AData(BAluXMuxSA), .AWw(BWwS), .ASignExt(LAluSignExt), .AResult(BAluXMuxS) );

 wire [31:0] BAluXMuxUA = (LMuxSrc[4] ? BRegMuxU[31:0] : 32'h0) |
                          (LLoopD[1] ? {24'h0, FLoopDData} : 32'h0);
 wire [31:0] BAluXMuxU;
 MsSignExt UAluXMuxU ( .AData(BAluXMuxUA), .AWw(BWwU), .ASignExt(LAluSignExt), .AResult(BAluXMuxU) );

 wire [63:0] BMioWrDataA = LMuxSrc[5] ? {32'h0, LConst} : (BRegMuxS | {56'h0, LLoopD[2] ? FLoopDData : 8'h0});
 wire [63:0] BMioWrData;
 assign BMioWrData[63:32] = BMioWrDataA[63:32]; MsSignExt UMioWrData ( .AData(BMioWrDataA[31:0]), .AWw(LMioSignExt[1:0]), .ASignExt(LMioSignExt[2]), .AResult(BMioWrData[31:0]) );
 wire [31:0] BMioAddrS = ({32{LMuxSrc[7]}} & BMlsc) | ({32{LMuxSrc[6]}} & LConst);
 assign      BMioAddr = BMioAddrS + BRegMuxU[31:0];
 wire [23:0] BIpLoadBusA = BRegMuxS[23:0] + LConst[23:0];
 assign BIpLoadBus = BIpLoadBusA[23:1];

 // Alu A+S+T
 wire [31:0] BAluADataA; wire [11:0] BAluALoadA;
 MssdAluFast UAluFast
  (
   .AMuxS(BAluXMuxS), .AMuxU(BAluXMuxU), .AWwS(BWwS), .AWwU(BWwU), .AFlagC(LFlagsVNZC[0]),
   .AAluSelA(LAluSelA), .AAluSelS(LAluSelS), .AAluSelT(LAluSelT),
   .ADstColRow(LColRowR), .ADstFlagWr(LDstFlagWr),
   .AAluData(BAluADataA), .AAluLoad(BAluALoadA),
   .AFlagsData(BBusFData), .AFlagsLoad(BBusFLoad)
  );

 MssdAlignToCol UBusAData ( .AData({32'h0, BAluADataA}), .ADstCol(BAluALoadA[11:8]), .AResult(BBusAData) );
 assign BBusALoad = BAluALoadA;

 // Alu U+F
 wire [31:0] BAluBDataA; wire [11:0] BAluBLoad, BAluBPend; wire BAluBPendAny;
 MssdAluSlow UAluSlow
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BAluXMuxS), .AMuxU(BAluXMuxU), .AWwS(BWwS), .AWwU(BWwU), .AWwR(BWwR),
   .AAluSelU(LAluSelU), .AAluSelF(LAluSelF),
   .ADstColRow(LColRowR),
   .ABusBData(BAluBDataA), .ABusBLoad(BAluBLoad), .ABusBPend(BAluBPend), .APendAny(BAluBPendAny)
  );

 // Mem
 wire [63:0] BMioBDataA; wire [11:0] BMioBLoad, BMioBPend; wire BMioBPendAny;
 wire [31:0] BExtAddr; wire [63:0] BExtMosi, BExtMiso; wire [3:0] BExtWrSize, BExtRdSize; wire BExtAck;
 wire [11:0] BLoopDColRow = (|{LLoopD[2], LLoopD[0]} ? 12'h100 : 12'h0);
 MsMioCtrl #(.CAddrLen(32)) UMioCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExtAddr(BExtAddr), .AExtMosi(BExtMosi), .AExtMiso(BExtMiso), .AExtWrSize(BExtWrSize), .AExtRdSize(BExtRdSize), .AExtReady(BExtAck),
   .AWrRdEn(LMioWrRdEn), .ARegIdx(LColRowS | BLoopDColRow), .AArgSize(LMioSize), .ASignExt(LMioSignExt),
   .AAddr(BMioAddr), .AData(BMioWrData),
   .ABusBData(BMioBDataA), .ABusBLoad(BMioBLoad), .ABusBPend(BMioBPend), .APendAny(BMioBPendAny)
  );
 assign BMioPendAny = |BMioBPendAny;

 wire BIsIoAccess = BExtAddr<CIoSpace;
 assign {ADataAddr, ADataMosi, ADataWrSize, ADataRdSize} = BIsIoAccess ? {32'h0, 64'h0, 4'h0, 4'h0} : {BExtAddr, BExtMosi, BExtWrSize, BExtRdSize};
 assign {APortAddr, APortMosi, APortWrSize, APortRdSize} = BIsIoAccess ? {BExtAddr[15:0], BExtMosi, BExtWrSize, BExtRdSize} : {16'h0, 64'h0, 4'h0, 4'h0};
 assign BExtMiso = BIsIoAccess ? APortMiso : ADataMiso;
 // assign BExtAck  = BIsIoAccess ? APortAck : ADataAck; <- this line generates a combinatorial loop, because BIsIoAccess depends on a register which is being written
 assign BExtAck  = APortAck | ADataAck;  // <- It is safe to do like this, because access is controlled by the same module. Otherwise, it is necessary to implement 2 Mio units: one for data, another one for ports

 // LoopD
 assign BLoopDData =
  ((BMioBLoad==12'h100) ? BMioBDataA[7:0] : 8'h0) |
  (LLoopD[1] ? BAluADataA[7:0] : 8'h0);


 assign BBusBLoad = BAluBLoad | BMioBLoad;
 assign BBusBPend = BAluBPend | BMioBPend;
 assign BDevBPendAny = BAluBPendAny | BMioBPendAny;
 MssdAlignToCol UBusBData ( .AData({32'h0, BAluBDataA} | BMioBDataA), .ADstCol(BBusBLoad[11:8]), .AResult(BBusBData) );

 // Ext
 assign AContPtrMiso = ASysCoreSel ? FContPtr : 29'h0;
 assign AIsIsr = ASysCoreSel & LRegsThis[30];
 assign ASetIrqSwtBase = LSysReq[4];
 assign AIrqBusyList = FIrqBusyList;
 assign ARegMiso = BRegRdIdxNZ ? BRegMuxU : 64'h0;
 assign AIpThis = {8'h0, LEipThis, 1'b0};
 assign ACmdDecReady = &{BCmdLenValid, ~BConflictAny};
 assign ATEnd = LTrap[1];
 assign ATrap = LTrap[0];
 assign ASysReq = FSysReq;
 assign ASiLock = FSiLock;
 assign AUnityReq = LUnityReq;

 wire BBlockIrqA = BPendAnyA | BStateNZ;
 wire BBlockIrqB = (|FSysReq);
 assign AIrqEn = &{LRegsThis[31], ~BBlockIrqA, ~BBlockIrqB, ~FKeepIpCache};
endmodule

// *********************
// *** #Flow Section ***
// *********************

module MsCondAnalyzer ( input [3:0] ACond, input [3:0] AFlags, output AJmpEn );
 localparam IFlagV = 3;
 localparam IFlagN = 2;
 localparam IFlagZ = 1;
 localparam IFlagC = 0;

 wire BSmaller = AFlags[IFlagN] ^ AFlags[IFlagV];

 assign AJmpEn = // sjmp jbe jc jnc jz jnz ja jsy
  ((ACond==4'h0) ?  1'b1                             : 1'b0) | // sjmp
  ((ACond==4'h1) ?  AFlags[IFlagZ] |  AFlags[IFlagC] : 1'b0) | // jbe
  ((ACond==4'h2) ?                    AFlags[IFlagC] : 1'b0) | // jc
  ((ACond==4'h3) ?                   ~AFlags[IFlagC] : 1'b0) | // jnc
  ((ACond==4'h4) ?  AFlags[IFlagZ]                   : 1'b0) | // jz
  ((ACond==4'h5) ? ~AFlags[IFlagZ]                   : 1'b0) | // jnz
  ((ACond==4'h6) ? ~AFlags[IFlagZ] & ~AFlags[IFlagC] : 1'b0) | // ja
  //((ACond==4'h7) ? ~BMuxDAE                          : 1'b0) | // djnz
  ((ACond==4'h8) ? ~AFlags[IFlagZ] & ~BSmaller       : 1'b0) | // jg
  ((ACond==4'h9) ?  AFlags[IFlagZ] | ~BSmaller       : 1'b0) | // jge
  ((ACond==4'hA) ?                    BSmaller       : 1'b0) | // js
  ((ACond==4'hB) ?  AFlags[IFlagZ] |  BSmaller       : 1'b0) | // jse
  ((ACond==4'hC) ?  AFlags[IFlagN]                   : 1'b0) | // jn
  ((ACond==4'hD) ?  AFlags[IFlagV]                   : 1'b0) | // jv
  ((ACond==4'hE) ? ~AFlags[IFlagN]                   : 1'b0) | // jnn
  ((ACond==4'hF) ? ~AFlags[IFlagV]                   : 1'b0);  // jnv
endmodule

// *********************
// *** #Regs Section ***
// *********************

module MssdRegRwx
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [63:0] ARegWrBusA, input [3:0] AColWrEnA, input ARowWrEnA,
  input [63:0] ARegWrBusB, input [3:0] AColWrEnB, input ARowWrEnB,
  output [63:0] ADataThis
 );

 wire [3:0] BRegWrEnA = ARowWrEnA ? AColWrEnA : 4'h0;
 wire [3:0] BRegWrEnB = ARowWrEnB ? AColWrEnB : 4'h0;

 MsRegX #(.CDataLen(32)) URegR
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA[63:32]), .ARegWrEnA(BRegWrEnA[3]),
   .ARegWrBusB(ARegWrBusB[63:32]), .ARegWrEnB(BRegWrEnB[3]),
   .ADataThis(ADataThis[63:32])
  );

 MsRegX #(.CDataLen(16)) URegW
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA[31:16]), .ARegWrEnA(BRegWrEnA[2]),
   .ARegWrBusB(ARegWrBusB[31:16]), .ARegWrEnB(BRegWrEnB[2]),
   .ADataThis(ADataThis[31:16])
  );

 MsRegX #(.CDataLen(8)) URegH
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA[15:8]), .ARegWrEnA(BRegWrEnA[1]),
   .ARegWrBusB(ARegWrBusB[15:8]), .ARegWrEnB(BRegWrEnB[1]),
   .ADataThis(ADataThis[15:8])
  );

 MsRegX #(.CDataLen(8)) URegL
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA[7:0]), .ARegWrEnA(BRegWrEnA[0]),
   .ARegWrBusB(ARegWrBusB[7:0]), .ARegWrEnB(BRegWrEnB[0]),
   .ADataThis(ADataThis[7:0])
  );

endmodule

module MssdRegTask
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [63:0] ARegWrBusA, input [11:0] ARegWrIdxA,
  input [63:0] ARegWrBusB, input [11:0] ARegWrIdxB,
  input [7:0] AFlagWrBus,
  input [23:1] AEipWrBus,
  output [511:0] ARegsThis
 );

 MssdRegRwx URegsRwx[7:1]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA), .AColWrEnA(ARegWrIdxA[11:8]), .ARowWrEnA(ARegWrIdxA[7:1]),
   .ARegWrBusB(ARegWrBusB), .AColWrEnB(ARegWrIdxB[11:8]), .ARowWrEnB(ARegWrIdxB[7:1]),
   .ADataThis(ARegsThis[511:64])
  );

 MsRegX #(.CDataLen(32)) URegSP
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARegWrBusA(ARegWrBusA[63:32]), .ARegWrEnA(ARegWrIdxA[11] & ARegWrIdxA[0]),
   .ARegWrBusB(ARegWrBusB[63:32]), .ARegWrEnB(ARegWrIdxB[11] & ARegWrIdxB[0]),
   .ADataThis(ARegsThis[63:32])
  );

 wire [23:1] FEip, BEip; assign BEip = AEipWrBus;
 wire [7:0] FFlags, BFlags; assign BFlags = AFlagWrBus;
 MsDffList #(.CRegLen(23+8)) URegFlEip
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BEip, BFlags}),
   .ADataO({FEip, FFlags})
  );

 assign ARegsThis[31:0] = {FFlags, FEip, 1'b0};
endmodule

// ********************
// *** #Sel Section ***
// ********************

module MsDecColRow ( input [5:0] ARegIdx, input ASelIp, output [11:0] AColRow );
 wire [7:0] BColDec; MsDec3x8a UColDec ( .ADataI({ARegIdx[5:4], ARegIdx[3]}), .ADataO(BColDec) );
 wire [7:0] BRowDec; MsDec3x8a URowDec ( .ADataI(ARegIdx[2:0]), .ADataO(BRowDec) );
 wire [3:0] BColIdx =
  (BColDec[7] ? 4'b0000 : 4'h0) |
  (BColDec[6] ? 4'b1111 : 4'h0) |
  (BColDec[5] ? 4'b1000 : 4'h0) |
  (BColDec[4] ? 4'b0111 : 4'h0) |
  (BColDec[3] ? 4'b0100 : 4'h0) |
  (BColDec[2] ? 4'b0011 : 4'h0) |
  (BColDec[1] ? 4'b0010 : 4'h0) |
  ((BColDec[0] & (|{ASelIp, ARegIdx[2:0]})) ? 4'b0001 : 4'h0);
 assign AColRow = {BColIdx, BRowDec[7:1], (ASelIp | BColDec[5] | BColDec[1]) & BRowDec[0]};
endmodule

module MssdAlignToAlu ( input [63:0] AData, input [3:0] ACol, output [63:0] AResult );
 assign AResult =
   ((ACol==4'hF) ? {       AData[63: 0]} : 64'h0) |
   ((ACol==4'h8) ? {32'h0, AData[63:32]} : 64'h0) |
   ((ACol==4'h7) ? {32'h0, AData[31: 0]} : 64'h0) |
   ((ACol==4'h4) ? {48'h0, AData[31:16]} : 64'h0) |
   ((ACol==4'h3) ? {48'h0, AData[15: 0]} : 64'h0) |
   ((ACol==4'h2) ? {56'h0, AData[15: 8]} : 64'h0) |
   ((ACol==4'h1) ? {56'h0, AData[ 7: 0]} : 64'h0);
endmodule

module MssdAlignToCol ( input [63:0] AData, input [3:0] ADstCol, output [63:0] AResult );
 assign AResult =
   ((ADstCol==4'hF) ? AData[63:0] : 32'h0) |
   ((ADstCol==4'h8) ? {AData[31:0], 32'h0} : 32'h0) |
   ((ADstCol==4'h7) ? {32'h0, AData[31:0]} : 32'h0) |
   ((ADstCol==4'h4) ? {32'h0, AData[15:0], 16'h0} : 32'h0) |
   ((ADstCol==4'h3) ? {32'h0, 16'h0, AData[15:0]} : 32'h0) |
   ((ADstCol==4'h2) ? {32'h0, 16'h0, AData[7:0], 8'h0} : 32'h0) |
   ((ADstCol==4'h1) ? {32'h0, 16'h0, 8'h0, AData[7:0]} : 32'h0);
endmodule

module MssdRegMux ( input [511:0] ARegFile, input [11:0] ARegRdIdx, output [63:0] AMux );
 wire [63:0] BMuxA; MsSelectRow #(.CRowCnt(8), .CColCnt(64)) UMuxA ( .ADataI(ARegFile), .AMask(ARegRdIdx[7:0]), .ADataO(BMuxA) );
 wire [63:0] BMuxB; MssdAlignToAlu UMuxB ( .AData(BMuxA), .ACol(ARegRdIdx[11:8]), .AResult(BMuxB) );
 assign AMux = BMuxB;
endmodule

module MssdColToSize ( input [3:0] ACol, output [3:0] ASize );
 assign ASize =
  ((ACol==4'hF) ? 4'h8 : 4'h0) |
  ((ACol==4'h8) ? 4'h4 : 4'h0) |
  ((ACol==4'h7) ? 4'h4 : 4'h0) |
  ((ACol==4'h4) ? 4'h2 : 4'h0) |
  ((ACol==4'h3) ? 4'h2 : 4'h0) |
  ((ACol==4'h2) ? 4'h1 : 4'h0) |
  ((ACol==4'h1) ? 4'h1 : 4'h0);
endmodule


// ************************
// *** #AluFast Section ***
// ************************

module MssdAluFast
 (
  input [31:0] AMuxS, AMuxU, input [1:0] AWwS, AWwU, input AFlagC,
  input [3:0] AAluSelA, // xor and sub add
  input [3:0] AAluSelS, // asr rol shr shl
  input [3:0] AAluSelT, // btx bts btr bt
  input [11:0] ADstColRow, input ADstFlagWr,
  output [31:0] AAluData, output [11:0] AAluLoad,
  output [3:0] AFlagsData, output AFlagsLoad
 );

 // AluA
 wire [31:0] BBusADataA; wire [11:0] BBusALoadA;
 wire  [3:0] BBusFDataA; wire BBusFLoadA;
 MssdAluA UAluA
  (
   .ADataS(AMuxS), .ADataU(AMuxU), .AWwS(AWwS), .AWwU(AWwU), .AOper(AAluSelA), .ADstColRow(ADstColRow), .ADstFlagWr(ADstFlagWr),
   .ABusAData(BBusADataA), .ABusALoad(BBusALoadA),
   .ABusFData(BBusFDataA), .ABusFLoad(BBusFLoadA)
  );

 // AluS
 wire [31:0] BBusADataS; wire [11:0] BBusALoadS;
 wire  [3:0] BBusFDataS; wire BBusFLoadS;
 MssdAluS UAluS
  (
   .ADataS(AMuxS), .ADataU(AMuxU), .AOper(AAluSelS), .ADstColRow(ADstColRow), .ADstFlagWr(ADstFlagWr),
   .ABusAData(BBusADataS), .ABusALoad(BBusALoadS),
   .ABusFData(BBusFDataS), .ABusFLoad(BBusFLoadS)
  );

 // AluT
 wire [31:0] BBusADataT; wire [11:0] BBusALoadT;
 wire  [3:0] BBusFDataT; wire BBusFLoadT;
 MssdAluT UAluT
  (
   .ADataS(AMuxS), .ADataD(AMuxU), .AOper(AAluSelT), .ADstColRow(ADstColRow), .ADstFlagWr(ADstFlagWr),
   .AFlagC(AFlagC),
   .ABusAData(BBusADataT), .ABusALoad(BBusALoadT),
   .ABusFData(BBusFDataT), .ABusFLoad(BBusFLoadT)
  );

 wire [31:0] BBusDData = BBusADataA | BBusADataS | BBusADataT;
 wire [11:0] BBusDLoad = BBusALoadA | BBusALoadS | BBusALoadT;
 wire  [3:0] BBusFData = BBusFDataA | BBusFDataS | BBusFDataT;
 wire        BBusFLoad = BBusFLoadA | BBusFLoadS | BBusFLoadT;

 assign AAluData = BBusDData; assign AAluLoad = BBusDLoad;
 assign AFlagsData = BBusFData; assign AFlagsLoad = BBusFLoad;
endmodule

module MssdAluGetSign ( input [31:0] AData, input [3:0] ASizeDec, output ASign );
 assign ASign = |{AData[31] & ASizeDec[2], AData[15] & ASizeDec[1], AData[7] & ASizeDec[0]};
endmodule

// ## AluA

module MssdAluA
 (
  input [31:0] ADataS, ADataU, input [1:0] AWwS, AWwU, input [3:0] AOper, input [11:0] ADstColRow, input ADstFlagWr,
  output [31:0] ABusAData, output [11:0] ABusALoad,
  output  [3:0] ABusFData, output ABusFLoad
 );

 // For Mirabelle AOper is one-hot {xor, and, sub, add}
 // For Risc-V, if bit "sub" is set, then it is possible that bit "and" signifies to take "C" flag for the result (SLTXX commands)
 // In this case bit "xor" indicates if bit "C" in the result has to be inverted

 wire [31:0] BResXor = ADataS ^ ADataU;
 wire [31:0] BResAnd = ADataS & ADataU;
 wire [31:0] BDataSA = {32{AOper[1]}} ^ ADataS;
 wire [32:0] BResAdd = {1'b0, BDataSA} + {1'b0, ADataU} + {31'h0, AOper[1]};

 wire [3:0] BSizeDecS; MsDec2x4a USizeDecS ( .ADataI(AWwS), .ADataO(BSizeDecS) );
 wire [3:0] BSizeDecU; MsDec2x4a USizeDecU ( .ADataI(AWwU), .ADataO(BSizeDecU) );
 wire [3:0] BSizeDecR; MssdColToSize USizeDecR ( .ACol(ADstColRow[11:8]), .ASize(BSizeDecR) );

 wire BResDataNZ = |ABusAData;
 wire BSignS; MssdAluGetSign USignS ( .AData(BDataSA),   .ASizeDec(BSizeDecS), .ASign(BSignS) );
 wire BSignU; MssdAluGetSign USignU ( .AData(ADataU),    .ASizeDec(BSizeDecU), .ASign(BSignU) );
 wire BSignR; MssdAluGetSign USignR ( .AData(ABusAData), .ASizeDec(BSizeDecR), .ASign(BSignR) );
 wire BOverflowA = ~(BSignS ^ BSignU) & (BSignU ^ BResAdd[31]);
 wire BBitT = AOper[3] ? BResAdd[31] ^ BOverflowA : ~BResAdd[32];

 wire BOperNZ = |AOper;
 wire BCmdAddSub = (AOper[0] | AOper[1]) & ~AOper[2];

 assign ABusAData =
   ((AOper[1] & AOper[2]) ? {31'h0, BBitT} : 32'h0) |
   ((AOper[3] & ~AOper[1]) ? BResXor : 32'h0) |
   ((AOper[2] & ~AOper[1]) ? BResAnd : 32'h0) |
   (BCmdAddSub ? BResAdd[31:0] : 32'h0);
 assign ABusALoad = BOperNZ ? ADstColRow : 12'h0;

 wire BOverflow = ~(BSignS ^ BSignU) & (BSignU ^ BSignR);
 assign ABusFData = BOperNZ ? {BCmdAddSub & BOverflow, BSignR, ~BResDataNZ, BCmdAddSub & (AOper[1] ^ BResAdd[32])} : 4'h0;
 assign ABusFLoad = BOperNZ ? ADstFlagWr : 1'h0;
endmodule

module MssdAluS
 (
  input [31:0] ADataS, ADataU,
  input [3:0] AOper, input [11:0] ADstColRow, input ADstFlagWr,
  output [31:0] ABusAData, output [11:0] ABusALoad,
  output  [3:0] ABusFData, output ABusFLoad
 );

 wire BOperNZ = |AOper;
 wire [3:0] BSizeDec; MssdColToSize USizeDec ( .ACol(ADstColRow[11:8]), .ASize(BSizeDec) );

 wire [31:0] BDataUAlignL =
   (BSizeDec[2] ?  ADataU               : 32'h0) |
   (BSizeDec[1] ? {ADataU[15:0], 16'h0} : 32'h0) |
   (BSizeDec[0] ? {ADataU[ 7:0], 24'h0} : 32'h0);

 wire [63:0] BDataShift =
  (AOper[3] ? {{32{ADataU[31]}}, ADataU} : 64'h0) | // asr
  (AOper[2] ? {ADataU, BDataUAlignL} : 64'h0) |     // rol
  (AOper[1] ? {32'h0, ADataU} : 64'h0) |            // shr
  (AOper[0] ? {ADataU, 32'h0} : 64'h0);             // shl

 wire [4:0] BShiftA = (AOper[0] | AOper[2]) ? ADataS[4:0] : ~ADataS[4:0] + 5'h1;

 wire [ 64:0] BDataResF = {1'b0, BDataShift};
 wire [ 48:0] BDataResE = BShiftA[4] ? BDataResF[48:0] : BDataResF[ 64:16];
 wire [ 40:0] BDataResD = BShiftA[3] ? BDataResE[40:0] : BDataResE[ 48: 8];
 wire [ 36:0] BDataResC = BShiftA[2] ? BDataResD[36:0] : BDataResD[ 40: 4];
 wire [ 34:0] BDataResB = BShiftA[1] ? BDataResC[34:0] : BDataResC[ 36: 2];
 wire [ 33:0] BDataResA = BShiftA[0] ? BDataResB[33:0] : BDataResB[ 34: 1];

 wire BShiftANZ = |BShiftA;
 assign ABusAData = BShiftANZ ? BDataResA[32:1] : (BOperNZ ? ADataU : 32'h0);
 assign ABusALoad = BOperNZ ? ADstColRow : 12'h0;

 wire BFlagCA = (AOper[1] | AOper[3]) ? BDataResA[0] :
  (BSizeDec[2] ? BDataResA[33] : 1'b0) |
  (BSizeDec[1] ? BDataResA[17] : 1'b0) |
  (BSizeDec[0] ? BDataResA[ 9] : 1'b0);

 wire BFlagC = BFlagCA & BShiftANZ;

 wire BResDataNZ = |ABusAData;
 wire BSignR =
  (BSizeDec[2] ? ABusAData[31] : 1'b0) |
  (BSizeDec[1] ? ABusAData[15] : 1'b0) |
  (BSizeDec[0] ? ABusAData[ 7] : 1'b0);
 assign ABusFData = BOperNZ ? {1'b0, BSignR, ~BResDataNZ, BFlagC} : 4'h0;
 assign ABusFLoad = BOperNZ ? ADstFlagWr : 1'h0;
endmodule

module MssdAluT
 (
  input [31:0] ADataS, input [31:0] ADataD, input [3:0] AOper, input [11:0] ADstColRow, input ADstFlagWr,
  input AFlagC,
  output [31:0] ABusAData, output [11:0] ABusALoad,
  output  [3:0] ABusFData, output ABusFLoad
 );

 wire [31:0] BBitMask; MsDec5x32a UBitMask ( .ADataI(ADataS[4:0]), .ADataO(BBitMask) );

 wire BOperNZ = |AOper;
 wire [31:0] BResData =
   //(AOper[3] ? (ADataD & ~BBitMask) | ({32{AFlagC}} & BBitMask) : 32'h0) | // btx
   (AOper[2] ?  ADataD |  BBitMask : 32'h0) |                              // bts
   (AOper[1] ?  ADataD & ~BBitMask : 32'h0) |                              // btr
   (AOper[0] ?  ADataD : 32'h0);                                           // bt

 assign ABusAData = BResData;
 assign ABusALoad = BOperNZ ? ADstColRow : 12'h0;

 wire BFlagC = |{ADataD & BBitMask};

 assign ABusFData = BOperNZ ? {1'b0, 1'b0, 1'b0, BFlagC} : 4'h0;
 assign ABusFLoad = BOperNZ ? ADstFlagWr : 1'h0;
endmodule

// ************************
// *** #AluSlow Section ***
// ************************

module MssdAluSlow
 (
  input AClkH, AResetHN, AClkHEn,
  input [31:0] AMuxS, AMuxU,
  input [1:0] AWwS, AWwU, AWwR,
  input [7:0] AAluSelU,
  input [6:0] AAluSelF,
  input [11:0] ADstColRow,
  output [31:0] ABusBData, output [11:0] ABusBLoad, ABusBPend, output APendAny
 );

 wire [31:0] BMulDivDataH, BMulDivDataR; wire BMulDivWrEn;
 wire [31:0] BMulDivDataSU, BMulDivDataDU; wire [2:0] BMulDivSizeU; wire [1:0] BMulDivStartU;
 wire [2:0] BMulDivNegDataU;

 wire [31:0] BAluURes; wire BAluUAck;
 MssdAluU UAluU
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataS(AMuxS), .ADataU(AMuxU),
   .AWwS(AWwS), .AWwU(AWwU), .AWwR(AWwR),
   .AOper(AAluSelU), .ADstColRow(ADstColRow),
   .AMulDivDataS(BMulDivDataSU), .AMulDivDataD(BMulDivDataDU), .AMulDivSize(BMulDivSizeU), .AMulDivStart(BMulDivStartU),
   .AMulDivNegData(BMulDivNegDataU),
   .AMulDivDataH(BMulDivDataH), .AMulDivDataR(BMulDivDataR), .AMulDivWrEn(BMulDivWrEn),
   .AAluRes(BAluURes), .AAluAck(BAluUAck)
  );

 wire [31:0] BMulDivDataSF, BMulDivDataDF; wire [1:0] BMulDivStartF;
 wire [31:0] BAluFRes; wire BAluFAck;
 MsFpuSA UFpu
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(AMuxS), .AMuxU(AMuxU),
   .AAluSel(AAluSelF), // round trunc itf div mul sub add
   .AMulDivDataS(BMulDivDataSF), .AMulDivDataD(BMulDivDataDF), .AMulDivStart(BMulDivStartF),
   .AMulDivDataH(BMulDivDataH), .AMulDivDataR(BMulDivDataR), .AMulDivWrEn(BMulDivWrEn),
   .AFpuRes(BAluFRes), .AFpuAck(BAluFAck)
  );

 MsMulDiv UMulDiv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataS(BMulDivDataSU | BMulDivDataSF), .ADataD(BMulDivDataDU | BMulDivDataDF), .ASizeI(BMulDivSizeU), .ASizeF(|BMulDivStartF), .AStart(BMulDivStartU | BMulDivStartF),
   .ANegData(BMulDivNegDataU),
   .ADataH(BMulDivDataH), .ADataR(BMulDivDataR), .ABusy(), .ABusBWrEn(BMulDivWrEn)
  );

 MsBusBCtrl UBusB
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrRdEn({1'b0, |{AAluSelU, AAluSelF}}), .ARegIdx(ADstColRow),
   .AExtMiso(BAluURes | BAluFRes), .AExtAck(BAluUAck | BAluFAck),
   .AWrRdActive(),
   .ABusBData(ABusBData), .ABusBLoad(ABusBLoad), .ABusBPend(ABusBPend),
   .APendAny(APendAny)
  );

endmodule

module MssdAluU
 (
  input AClkH, AResetHN, AClkHEn,
  input [31:0] ADataS, ADataU,
  input [1:0] AWwS, AWwU, AWwR,
  input [7:0] AOper, // urem srem udiv sdiv mulhu mulhsu mulh mul
  input [11:0] ADstColRow,
  // Common MulDiv
  output [31:0] AMulDivDataS, AMulDivDataD, output [2:0] AMulDivSize, output [1:0] AMulDivStart,
  output [2:0] AMulDivNegData,
  input [31:0] AMulDivDataH, AMulDivDataR, input AMulDivWrEn,
  // Output
  output [31:0] AAluRes, output AAluAck
 );

 wire FBusy, BBusy;
 wire FBusH, BBusH;
 wire FZeroDiv, BZeroDiv;

 MsDffList #(.CRegLen(1+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BBusy, BBusH, BZeroDiv}),
   .ADataO({FBusy, FBusH, FZeroDiv})
  );

 //wire [15:0] BOper; MsDec4x16a UOper ( .ADataI(AOper), .ADataO(BOper) );
 wire [7:0] BOper = AOper;

 localparam auUrem = 7;
 localparam auSrem = 6;
 localparam auUdiv = 5;
 localparam auSdiv = 4;
 localparam auMhuu = 3;
 localparam auMhsu = 2;
 localparam auMhss = 1;
 localparam auMul  = 0;

 assign AMulDivStart =
   {
    |{BOper[auUrem], BOper[auUdiv], BOper[auSrem], BOper[auSdiv]},
    |{BOper[auMhuu], BOper[auMhsu], BOper[auMhss], BOper[auMul] }
   };
 wire BMulDivStartNZ = |AMulDivStart;
 //wire [1:0] BWwSrcDiv = ((AWwS==2'h2) | (AWwU==2'h2)) ? 2'h2 : (((AWwS==2'h1) | (AWwU==2'h1)) ? 2'h1 : 2'h0);
 //wire [1:0] BWwSrcMul = ((AWwS==2'h2) | (AWwU==2'h2) | (AWwR==2'h2)) ? 2'h2 : (((AWwS==2'h1) | (AWwU==2'h1) | (AWwR==2'h1)) ? 2'h1 : 2'h0);
 wire [1:0] BWwSrc = ((AWwS==2'h2) | (AWwU==2'h2) | (AWwR==2'h2)) ? 2'h2 : (((AWwS==2'h1) | (AWwU==2'h1) | (AWwR==2'h1)) ? 2'h1 : 2'h0);
 wire [1:0] BSizeA = (AMulDivStart[1] ? BWwSrc : 2'h0) |
                     (AMulDivStart[0] ? BWwSrc : 2'h0);
 wire [3:0] BSize; MsDec2x4a USize ( .ADataI(BSizeA), .ADataO(BSize) );
 wire BDataSNZ = |ADataS;
 assign BZeroDiv = BMulDivStartNZ ? AMulDivStart[1] & ~BDataSNZ : FZeroDiv;

 // MulDiv
 assign AMulDivDataS = BMulDivStartNZ ? ADataS : 32'h0;
 assign AMulDivDataD = BMulDivStartNZ ? ADataU : 32'h0;
 assign AMulDivSize  = BMulDivStartNZ ? BSize[2:0] : 3'h0;
 assign AMulDivNegData =
  ((|{BOper[auMhuu], BOper[auUrem], BOper[auUdiv]}) ? 3'h0 : 3'h0) |
  ((|{BOper[auMhsu]}) ? {ADataU[31], 1'b0, ADataU[31]} : 3'h0) |
  ((|{BOper[auSrem]}) ? {ADataU[31], ADataS[31], ADataU[31]} : 3'h0) |
  ((|{BOper[auMhss], BOper[auSdiv], BOper[auMul]}) ? {ADataS[31] ^ ADataU[31], ADataS[31], ADataU[31]} : 3'h0);

 assign BBusy = BMulDivStartNZ | (FBusy & ~AMulDivWrEn);
 wire BBusBWrEn = FBusy &  AMulDivWrEn;

 assign BBusH = (BMulDivStartNZ & (|{BOper[auUrem], BOper[auSrem], BOper[auMhuu], BOper[auMhsu], BOper[auMhss]})) | (FBusH & ~AMulDivWrEn);

 // Final assignment
 //assign AAluRes = BBusBWrEn ? (FZeroDiv ? 32'hFFFFFFFF : AMulDivDataR) : (AOper[2] ? FResid : 32'h0);
 assign AAluRes = BBusBWrEn ? (FBusH ? AMulDivDataH : AMulDivDataR) : 32'h0;
 assign AAluAck = BBusBWrEn;
endmodule

// Core CmdDec
// 8428 304
// 8581 424

