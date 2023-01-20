module MsCpuCtrl #(parameter CCoreCnt=8'h2, CStartAddr=32'h0000, CVersion=8'h8, CIrqCnt=8)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AExecEn, input ADbgStep,
  input [CCoreCnt*3-1:0] ASysReq, output [CCoreCnt-1:0] ASysAck,
  output [CCoreCnt-1:0] ASysCoreSel, input [31:3] AContPtrMiso, input AIsIsr, output AContPtrWrEn, input [CCoreCnt-1:0] ASiLock,
  input [CIrqCnt-1:0] AIrqBusyList, output [CIrqCnt-1:0] AIrqToProcess, // Avoid re-entering
  input [CCoreCnt-1:0] ASetIrqSwtBase,
  input [CCoreCnt-1:0] AIrqEn, input [CIrqCnt-1:0] AIrq,
  output [CCoreCnt-1:0] ACoreEn, // Individual Enable signal, can be zero during IRQ
  output [63:0] ARegMosi, input [63:0] ARegMiso, output [11:0] ARegWrIdx, ARegRdIdx,
  output [31:3] ARomAddr, input [63:0] ARomMiso, output ARomRdEn,
  output [31:0] ARamAddr, input [63:0] ARamMiso, output [63:0] ARamMosi, output [3:0] ARamWrSize, ARamRdSize, input ARamAck,
  output [7:0] ATest
 );

 localparam CCoreNil = {CCoreCnt{1'b0}};

 localparam CStateMLen = 7;
 localparam CStateMNil = {CStateMLen{1'b0}};

 localparam IStExecB = 6;
 localparam IStExecA = 5;
 localparam IStInitE = 4;
 localparam IStInitD = 3;
 localparam IStInitC = 2;
 localparam IStInitB = 1;
 localparam IStInitA = 0; // Set RomAddr, CoreIdx

 localparam CStateSysLen = 25;
 localparam CStateSysNil = {CStateSysLen{1'b0}};
 localparam IStIrqEndA = 24;
 localparam IStIrqSwtH = 23;
 localparam IStIrqSwtG = 22;
 localparam IStIrqSwtF = 21;
 localparam IStIrqSwtE = 20;
 localparam IStIrqSwtD = 19;
 localparam IStIrqSwtC = 18;
 localparam IStIrqSwtB = 17;
 localparam IStIrqSwtA = 16;
 localparam IStIrqReqD = 15;
 localparam IStIrqReqC = 14;
 localparam IStIrqReqB = 13;
 localparam IStIrqReqA = 12;
 localparam IStSysLock = 11;
 localparam IStSysEndA = 10;
 localparam IStSysSwtH =  9;
 localparam IStSysSwtG =  8;
 localparam IStSysSwtF =  7;
 localparam IStSysSwtE =  6;
 localparam IStSysSwtD =  5;
 localparam IStSysSwtC =  4;
 localparam IStSysSwtB =  3;
 localparam IStSysSwtA =  2;
 localparam IStSysReqC =  1;
 localparam IStSysReqA =  0;

 wire [1:0] FSubcoreIdxBase, BSubcoreIdxBase;
 wire [1:0] FSubcoreIdxStop, BSubcoreIdxStop;

 wire [CStateMLen-1:0] FStateM, BStateM;
 wire [CStateSysLen-1:0] FStateSys, BStateSys;
 wire [CCoreCnt-1:0] FCoreEn, BCoreEn;
 wire [2:0] FSysReqFn, BSysReqFn;
 wire [63:0] FRomMiso, BRomMiso;
 wire [3:0] FCoreIdxA, BCoreIdxA;
 wire [CCoreCnt-1:0] FSysReqSrc, BSysReqSrc; // Which core initiates a SysReq (can be multiple)
 wire [CCoreCnt-1:0] FSysCoreSel, BSysCoreSel;
 wire [7:0] FQueMask, BQueMask,
            FTailIdx, BTailIdx,
            FHeadIdx, BHeadIdx;
 wire [3:0] FRegIdx, BRegIdx;
 wire [63:0] FRamMosi, BRamMosi;
 wire [63:0] FRamMiso, BRamMiso;
 wire [CCoreCnt-1:0] FSiLock, BSiLock;

 wire [31:2] FIrqBase, BIrqBase;
 wire [31:2] FSwtBase, BSwtBase;

 wire [CIrqCnt-1:0] FIrqIn, BIrqIn;
 wire [CIrqCnt-1:0] FIrqAll, BIrqAll;
 wire [CIrqCnt-1:0] FIrqThis, BIrqThis;

 wire [CCoreCnt-1:0] FCoreCandidate, BCoreCandidate;

 MsDffList #(.CRegLen(2+2+CStateMLen+CStateSysLen+CCoreCnt+3+64+4+CCoreCnt*2+3*8+4+64+64+CCoreCnt+30+30+3*CIrqCnt+CCoreCnt)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSubcoreIdxBase, BSubcoreIdxStop, BStateM, BStateSys, BCoreEn, BSysReqFn, BRomMiso, BCoreIdxA, BSysReqSrc, BSysCoreSel, BQueMask, BTailIdx, BHeadIdx, BRegIdx, BRamMosi, BRamMiso, BSiLock, BIrqBase, BSwtBase, BIrqIn, BIrqAll, BIrqThis, BCoreCandidate}),
   .ADataO({FSubcoreIdxBase, FSubcoreIdxStop, FStateM, FStateSys, FCoreEn, FSysReqFn, FRomMiso, FCoreIdxA, FSysReqSrc, FSysCoreSel, FQueMask, FTailIdx, FHeadIdx, FRegIdx, FRamMosi, FRamMiso, FSiLock, FIrqBase, FSwtBase, FIrqIn, FIrqAll, FIrqThis, FCoreCandidate})
  );

 assign BRomMiso = FStateM[IStInitC] ? ARomMiso : FRomMiso;

 assign {BIrqBase, BSwtBase} = (|ASetIrqSwtBase) ? {ARegMiso[63:34], ARegMiso[31:2]} : {FIrqBase, FSwtBase};

 // Common
 wire BStateMNZ = |FStateM;
 wire BStateSysNZ = |FStateSys;
 wire BSysReqSrcNZ = |FSysReqSrc;
 wire BPendNZ = |{BStateSysNZ, BStateSys[IStSysReqA]};

 wire [CStateMLen-1:0] BGoM, BStayM; assign BStateM = BGoM | BStayM;
 wire [CStateSysLen-1:0] BGoSys, BStaySys; assign BStateSys = BGoSys | BStaySys;

// assign BSubcoreIdxBase = {FSubcoreIdxBase[0], FSubcoreIdxBase[1] ? AExecEn : ~FSubcoreIdxBase[0] & FStateM[IStExecB]};
// assign BSubcoreIdxStop = {FSubcoreIdxStop[0], FSubcoreIdxBase[1] ? FStateM[IStExecB] : (FStateM[IStExecB] ? ~FSubcoreIdxBase[0] & FStateM[IStExecB]  : ~FSubcoreIdxStop[0] & BPendNZ)};
 assign BSubcoreIdxBase = {FSubcoreIdxBase[0], ~FSubcoreIdxBase[0] & FStateM[IStExecB]};
 assign BSubcoreIdxStop = {FSubcoreIdxStop[0], FStateM[IStExecB] ? BSubcoreIdxBase[0] : ~FSubcoreIdxStop[0] & BPendNZ};

 // FSM Main
 assign BGoM[IStInitA] = ~BStateMNZ & (AExecEn | ADbgStep);               assign BStayM[IStInitA] = 1'b0;
 assign BGoM[IStInitB] =  FStateM[IStInitA] |
                         (FStateM[IStInitE] & ~FSysCoreSel[CCoreCnt-1]);  assign BStayM[IStInitB] = 1'b0;
 assign BGoM[IStInitC] =  FStateM[IStInitB];                              assign BStayM[IStInitC] = 1'b0;
 assign BGoM[IStInitD] =  FStateM[IStInitC];                              assign BStayM[IStInitD] = 1'b0;
 assign BGoM[IStInitE] =  FStateM[IStInitD] & ~FSysCoreSel[CCoreCnt-1];   assign BStayM[IStInitE] = 1'b0;
 assign BGoM[IStExecA] = (FStateM[IStInitD] &  FSysCoreSel[CCoreCnt-1]) |
                         (FStateM[IStInitE] &  FSysCoreSel[CCoreCnt-1]);  assign BStayM[IStExecA] = 1'b0;
 assign BGoM[IStExecB] =  FStateM[IStExecA];                              assign BStayM[IStExecB] = FStateM[IStExecB];

 assign BCoreIdxA = FCoreIdxA + {3'h0, FStateM[IStInitD] | FStateM[IStInitE]};

 wire [CCoreCnt-1:0] BIrqAck = FStateSys[IStIrqEndA] ? FSysCoreSel : CCoreNil;

 wire BSysAckNZ = |ASysAck;
 MsMatrOrRow #(.CRowCnt(CCoreCnt), .CColCnt(3)) USysReqSrc ( .ADataI(BSysAckNZ ? {CCoreCnt{1'b0}} : ASysReq), .ADataO(BSysReqSrc) );

 // IRQ
 // ** Irq
 assign BIrqIn = AIrq;
 assign BIrqAll = (FIrqIn & ~AIrqBusyList) | (FIrqAll & ~(FStateSys[IStIrqEndA] ? FIrqThis : {CIrqCnt{1'b0}})); // Accumulates requests and keeps them till IStIrqEndA. Then clears the corresponding bit
 wire BIrqAllNZ  = |FIrqAll;
 wire BIrqThisNZ = |FIrqThis;
 wire BIrqLatch = BIrqAllNZ & ~BIrqThisNZ; // Avoid IRQ change during execution
 wire [CIrqCnt-1:0] BIrqAllPrty; // Momentarily prioritizes the new request (i.e. when no other requests are pending)
 MsPrioritize #(.CLineCnt(CIrqCnt)) UPrtyIrq
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BIrqLatch ? FIrqAll : {CIrqCnt{1'b0}}), .ADataO(BIrqAllPrty)
  );

 // FIrqThis is latched and prioritized IRQ. Only 1 bit is set. Cleared by IStIrqEndA.
 assign BIrqThis = BIrqLatch ? BIrqAllPrty : (FStateSys[IStIrqEndA] ? {CIrqCnt{1'b0}} : FIrqThis); // Only 1 IRQ is latched
 wire [4:0] BIrqThisIdx; MsIdxOf32a UIrqThisIdx ( .ADataI({{(32-CIrqCnt){1'b0}}, FIrqThis}), .ADataO(BIrqThisIdx) );

 // We give priority to IRQ over SWT, otherwise if at least one of cores performs SWT frequently, interrupts will not be processed
 // even if there is a free CPU core.

 // Select a core which is able to process an IRQ
 wire [CCoreCnt-1:0] BCoreCandidateA;
 MsPrioritize #(.CLineCnt(CCoreCnt)) UCoreToProcessIrq
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI((~BStateSysNZ & BIrqThisNZ) ? AIrqEn : {CCoreCnt{1'b0}}), .ADataO(BCoreCandidateA)
  );

 wire BGoIrqProc = |BCoreCandidateA;
 // Avoiding IRQ racing condition: FCoreCandidate will keep value for 2 cycles and see if AIrqEn is still there
 assign BCoreCandidate = BGoSys[IStIrqReqA] ? BCoreCandidateA : (BGoSys[IStIrqReqB] ? FCoreCandidate : CCoreNil);
 wire BIrqConfirmed = |(FCoreCandidate & AIrqEn); // Avoid race condition if IRQ comes at the same moment CPU start executing "long" commands
 wire [CCoreCnt-1:0] BIrqCancel = ((|FStateSys[IStIrqReqB:IStIrqReqA]) & ~BIrqConfirmed) ? FCoreCandidate : CCoreNil;

 assign BCoreEn =
  (FStateM[IStExecA] ? {CCoreCnt{1'b1}} : CCoreNil) |
  (FStateM[IStExecB] ? (FCoreEn & ~FSysReqSrc & ~BCoreCandidate) | ASysAck | BIrqAck | BIrqCancel : CCoreNil);


 // FSM Sys
 wire [CCoreCnt-1:0] BSysReqIdx;
 MsPrioritize #(.CLineCnt(CCoreCnt)) USysReqIdx
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(FSysReqSrc), .ADataO(BSysReqIdx)
  );

 wire [CCoreCnt:0] BCoreE = {CCoreNil, 1'b1};
 wire [CCoreCnt:0] BSysCoreSelShlE = {FSysCoreSel, 1'b0};
 assign BSysCoreSel =
  (FStateM[IStInitA] ? BCoreE[CCoreCnt-1:0] : CCoreNil) |
  (FStateM[IStInitD] ? BSysCoreSelShlE[CCoreCnt-1:0] : CCoreNil) |
  (BGoSys[IStSysReqA] ? BSysReqIdx : CCoreNil) |
  (FStateSys[IStIrqReqA] ? FCoreCandidate : CCoreNil) |
  ((|{FStateM[IStInitB], FStateM[IStInitC], BStateSysNZ}) ? FSysCoreSel : CCoreNil);

 wire [2:0] BSysReqFnA; MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(3)) USysReqFnA ( .ADataI(ASysReq), .AMask(BSysCoreSel), .ADataO(BSysReqFnA) );
 assign BSysReqFn = BGoSys[IStSysReqA] ? BSysReqFnA : FSysReqFn;

 wire [15:0] BRegIdxS; MsDec4x16a URegIdxS ( .ADataI(FRegIdx), .ADataO(BRegIdxS) );
 wire LLastRegGpr = BRegIdxS[7];
 wire LLastRegMpu = BRegIdxS[11];

 // SWT/END/SYS part
 assign BGoSys[IStSysReqA] = ~BStateSysNZ & BSysReqSrcNZ & ~BGoIrqProc & FStateM[IStExecB];   assign BStaySys[IStSysReqA] = 1'b0;
 // Load Ctrl
 assign BGoSys[IStSysReqC] =  FStateSys[IStSysReqA] & (|FSysReqFn[1:0]);                      assign BStaySys[IStSysReqC] = FStateSys[IStSysReqC] & ~ARamAck;
 // Save context
 assign BGoSys[IStSysSwtA] = (FStateSys[IStSysReqC] &  ARamAck & FSysReqFn[0]) |
                             (FStateSys[IStSysSwtB] &  ARamAck & ~LLastRegGpr);               assign BStaySys[IStSysSwtA] = 1'b0;
 assign BGoSys[IStSysSwtB] =  FStateSys[IStSysSwtA];                                          assign BStaySys[IStSysSwtB] = FStateSys[IStSysSwtB] & ~ARamAck;
 // Save ContPtr (only if the thread is not IRQ)
 assign BGoSys[IStSysSwtC] =  FStateSys[IStSysSwtB] &  ARamAck &  LLastRegGpr & ~AIsIsr;      assign BStaySys[IStSysSwtC] = FStateSys[IStSysSwtC] & ~ARamAck;
 // Load ContPtr
 assign BGoSys[IStSysSwtD] = (FStateSys[IStSysReqC] &  ARamAck & FSysReqFn[1]) |
                             (FStateSys[IStSysSwtB] &  ARamAck &  LLastRegGpr &  AIsIsr) |
                             (FStateSys[IStSysSwtC] &  ARamAck);                              assign BStaySys[IStSysSwtD] = FStateSys[IStSysSwtD] & ~ARamAck;
 assign BGoSys[IStSysSwtE] =  FStateSys[IStSysSwtD] &  ARamAck;                               assign BStaySys[IStSysSwtE] = 1'b0;
 // Load context
 assign BGoSys[IStSysSwtF] =  FStateSys[IStSysSwtE] |
                             (FStateSys[IStSysSwtG] & ~LLastRegMpu);                          assign BStaySys[IStSysSwtF] = FStateSys[IStSysSwtF] & ~ARamAck;
 assign BGoSys[IStSysSwtG] = (FStateSys[IStSysSwtF] &  ARamAck);                              assign BStaySys[IStSysSwtG] = 1'b0;
 // Save Ctrl
 assign BGoSys[IStSysSwtH] = (FStateSys[IStSysSwtG] &  LLastRegMpu);                          assign BStaySys[IStSysSwtH] = FStateSys[IStSysSwtH] & ~ARamAck;
 // Sys ACK
 assign BGoSys[IStSysEndA] = (FStateSys[IStSysSwtH] &  ARamAck);                              assign BStaySys[IStSysEndA] = 1'b0;
 // Lock
 assign BGoSys[IStSysLock] =  FStateSys[IStSysReqA] & FSysReqFn[2];                           assign BStaySys[IStSysLock] = 1'b0;
 // IRQ part
 assign BGoSys[IStIrqReqA] = ~BStateSysNZ & BGoIrqProc & FStateM[IStExecB];                   assign BStaySys[IStIrqReqA] = 1'b0;
 // Avoiding race condition, 2 clock cycles
 assign BGoSys[IStIrqReqB] =  FStateSys[IStIrqReqA] & BIrqConfirmed;                          assign BStaySys[IStIrqReqB] = 1'b0;
 // Load Ctrl
 assign BGoSys[IStIrqReqC] =  FStateSys[IStIrqReqB] & BIrqConfirmed;                          assign BStaySys[IStIrqReqC] = FStateSys[IStIrqReqC] & ~ARamAck;
 // Dec head address // At the same time as IStIrqSwtA
 assign BGoSys[IStIrqReqD] =  FStateSys[IStIrqReqC] &  ARamAck;                               assign BStaySys[IStIrqReqD] = 1'b0;
 // Save context
 assign BGoSys[IStIrqSwtA] = (FStateSys[IStIrqReqC] &  ARamAck) |
                             (FStateSys[IStIrqSwtB] &  ARamAck & ~LLastRegGpr);               assign BStaySys[IStIrqSwtA] = 1'b0;
 assign BGoSys[IStIrqSwtB] =  FStateSys[IStIrqSwtA];                                          assign BStaySys[IStIrqSwtB] = FStateSys[IStIrqSwtB] & ~ARamAck;
 // Save ContPtr (but do not increment address: it is in the beginning at the list)
 assign BGoSys[IStIrqSwtC] =  FStateSys[IStIrqSwtB] &  ARamAck &  LLastRegGpr;                assign BStaySys[IStIrqSwtC] = FStateSys[IStIrqSwtC] & ~ARamAck;
 // Load ContPtr (from Irq table, as an index of IRQ)
 assign BGoSys[IStIrqSwtD] =  FStateSys[IStIrqSwtC] &  ARamAck;                               assign BStaySys[IStIrqSwtD] = FStateSys[IStIrqSwtD] & ~ARamAck;
 assign BGoSys[IStIrqSwtE] =  FStateSys[IStIrqSwtD] &  ARamAck;                               assign BStaySys[IStIrqSwtE] = 1'b0;
 // Load context
 assign BGoSys[IStIrqSwtF] =  FStateSys[IStIrqSwtE] |
                             (FStateSys[IStIrqSwtG] & ~LLastRegMpu);                          assign BStaySys[IStIrqSwtF] = FStateSys[IStIrqSwtF] & ~ARamAck;
 assign BGoSys[IStIrqSwtG] =  FStateSys[IStIrqSwtF] &  ARamAck;                               assign BStaySys[IStIrqSwtG] = 1'b0;
 // Save Ctrl
 assign BGoSys[IStIrqSwtH] =  FStateSys[IStIrqSwtG] &  LLastRegMpu ;                          assign BStaySys[IStIrqSwtH] = FStateSys[IStIrqSwtH] & ~ARamAck;
 // Irq ACK (clear flags)
 assign BGoSys[IStIrqEndA] =  FStateSys[IStIrqSwtH] &  ARamAck ;                              assign BStaySys[IStIrqEndA] = 1'b0;

 wire BRegIdxKeep = |{
                      FStateSys[IStSysSwtB:IStSysSwtA],
                      FStateSys[IStSysSwtG:IStSysSwtF],
                      FStateSys[IStIrqSwtB:IStIrqSwtA],
                      FStateSys[IStIrqSwtG:IStIrqSwtF]
                     };
 wire BRegIdxNext = |{
                      FStateSys[IStSysSwtB] & ARamAck & ~LLastRegGpr,
                      FStateSys[IStSysSwtG] & ~LLastRegMpu,
                      FStateSys[IStIrqSwtB] & ARamAck & ~LLastRegGpr,
                      FStateSys[IStIrqSwtG] & ~LLastRegMpu
                     };
 wire BRegIdxZero = |{
                      FStateSys[IStSysSwtB] & ARamAck &  LLastRegGpr,
                      FStateSys[IStSysSwtG] & LLastRegMpu,
                      FStateSys[IStIrqSwtB] & ARamAck &  LLastRegGpr,
                      FStateSys[IStIrqSwtG] & LLastRegMpu
                     };
 assign BRegIdx = (BRegIdxKeep & ~BRegIdxZero) ? FRegIdx+{3'h0, BRegIdxNext} : 3'h0;

 assign BRamMiso = ((FStateSys[IStSysSwtF] | FStateSys[IStIrqSwtF]) & ARamAck) ? ARamMiso : FRamMiso;
 assign BRamMosi =  (FStateSys[IStSysSwtA] | FStateSys[IStIrqSwtA]) ? ARegMiso : FRamMosi;

 assign BQueMask = ((FStateSys[IStSysReqC] | FStateSys[IStIrqReqC]) &  ARamAck) ? ARamMiso[23:16] : FQueMask;
 assign BTailIdx = ((FStateSys[IStSysReqC] | FStateSys[IStIrqReqC]) &  ARamAck) ? ARamMiso[15: 8] : FQueMask & (FTailIdx+{7'h0, FStateSys[IStSysSwtC] &  ARamAck});
 assign BHeadIdx = ((FStateSys[IStSysReqC] | FStateSys[IStIrqReqC]) &  ARamAck) ? ARamMiso[ 7: 0] : FQueMask & (FHeadIdx+({7'h0, FStateSys[IStSysSwtD] &  ARamAck} | {8{FStateSys[IStIrqReqD]}}));

 assign BSiLock =   FStateSys[IStSysLock] ? FSysCoreSel : FSiLock & ASiLock;

 // Outs
 assign ASysAck =
  ((FStateSys[IStSysEndA] | FStateSys[IStSysLock]) ? FSysCoreSel : CCoreNil);

 assign ACoreEn = FCoreEn;

 // Outs.Regs
 wire [7:0] BCoreCnt = CCoreCnt[7:0];
 assign ARegMosi =
  (FStateM[IStInitD] ? {CVersion, 8'h0, BCoreCnt, 4'h0, FCoreIdxA, FRomMiso[31: 0]} : 64'h0) |
  (FStateM[IStInitE] ? {CVersion, 8'h0, BCoreCnt, 4'h0, FCoreIdxA, FRomMiso[63:32]} : 64'h0) |
  ((FStateSys[IStSysSwtG] | FStateSys[IStIrqSwtG]) ? FRamMiso : 64'h0);

 assign ARegWrIdx =
  ((FStateM[IStInitD] | FStateM[IStInitE]) ? 12'h1 : 12'h0) |
  ((FStateSys[IStSysSwtG] | FStateSys[IStIrqSwtG]) ? BRegIdxS[11:0] : 12'h0);

 assign ARegRdIdx =
  ((|ASetIrqSwtBase) ? 12'h2 : 12'h0) |
  ((FStateSys[IStSysSwtA] | FStateSys[IStIrqSwtA]) ? BRegIdxS[11:0] : 12'h0);

 // Outs.Rom
 assign ARomAddr = FStateM[IStInitB] ? {CStartAddr[31:6], FCoreIdxA[3:1]} : 29'h0;
 assign ARomRdEn = FStateM[IStInitB];

 // Outs.Ram
 wire [31:2] BSwtAddr = FSwtBase+30'h1;
 wire [31:2] BIrqAddr = FIrqBase;
 wire [31:3] BContAddr = AContPtrMiso+{25'h0, BRegIdx};
 assign ARamAddr =
  ((|{BStateSys[IStSysReqC], BStateSys[IStSysSwtH], BStateSys[IStIrqReqC], BStateSys[IStIrqSwtH]}) ? {FSwtBase, 2'h0}  : 32'h0) |
  ((|{BStateSys[IStSysSwtB], BStateSys[IStSysSwtF], BStateSys[IStIrqSwtB], BStateSys[IStIrqSwtF]}) ? {BContAddr, 3'h0} : 32'h0) |
  ((|{BStateSys[IStSysSwtC]}) ? {BSwtAddr+{21'h0, FTailIdx}, 2'h0} : 32'h0) |
  ((|{BStateSys[IStSysSwtD], BStateSys[IStIrqSwtC]}) ? {BSwtAddr+{21'h0, FHeadIdx}, 2'h0} : 32'h0) |
  ((|{BStateSys[IStIrqSwtD]}) ? {BIrqAddr+{24'h0, BIrqThisIdx}, 2'h0} : 32'h0);

 assign ARamMosi =
  ((BStateSys[IStSysSwtB] | BStateSys[IStIrqSwtB]) ? BRamMosi : 64'h0) |
  ((BStateSys[IStSysSwtC] | BStateSys[IStIrqSwtC]) ? {32'h0, AContPtrMiso, 3'h0} : 64'h0) |
  ((BStateSys[IStSysSwtH] | BStateSys[IStIrqSwtH]) ? {32'h0, 8'h0, FQueMask, FTailIdx, FHeadIdx} : 64'h0);

 assign ARamWrSize = {BStateSys[IStSysSwtB] | BStateSys[IStIrqSwtB], BStateSys[IStSysSwtC] | BStateSys[IStSysSwtH] | BStateSys[IStIrqSwtC] | BStateSys[IStIrqSwtH], 2'h0};
 assign ARamRdSize = {BStateSys[IStSysSwtF] | BStateSys[IStIrqSwtF], BStateSys[IStSysSwtD] | BStateSys[IStSysReqC] | BStateSys[IStIrqSwtD] | BStateSys[IStIrqReqC], 2'h0};

 // Outs.Ctrl/ContPtrs
 assign ASysCoreSel = FSysCoreSel | ASetIrqSwtBase;
 assign AContPtrWrEn  = (FStateSys[IStSysSwtD] | FStateSys[IStIrqSwtD]) &  ARamAck;
 assign AIrqToProcess = FStateSys[IStIrqSwtD] ? FIrqThis : {CIrqCnt{1'b0}};

 //assign ATest = {ASysCoreSel, AContPtrWrEn, AIrqBusyList[0], FStateSys[IStIrqEndA], FIrqThis[0], FIrqAll[0], FIrqIn[0]};
 assign ATest = {ASysCoreSel, AIrqEn, FStateSys[IStIrqEndA], FIrqThis[0], FIrqAll[0], FIrqIn[0]};

endmodule

module MsUnityCtrl #(parameter CLineCnt=2)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CLineCnt-1:0] AUnityReq, output [CLineCnt-1:0] AUnityAck
 );

 wire [CLineCnt-1:0] FUnityIdx, BUnityIdx;
 MsDffList #(.CRegLen(CLineCnt)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BUnityIdx}),
   .ADataO({FUnityIdx})
  );

 wire [CLineCnt-1:0] BUnityIdxA;
 MsPrioritize #(.CLineCnt(CLineCnt)) USysReqIdx
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AUnityReq), .ADataO(BUnityIdxA)
  );

 wire BDataKeep = |(AUnityReq & FUnityIdx);
 assign BUnityIdx = BDataKeep ? FUnityIdx : BUnityIdxA;

 assign AUnityAck = FUnityIdx;
endmodule

module MsShlMemMask ( input [2:0] AAddr, input [7:0] AMaskI, output [7:0] AMaskO );
 wire [7:0] BMaskC = AAddr[2] ? {AMaskI[3:0], 4'h0} : AMaskI;
 wire [7:0] BMaskB = AAddr[1] ? {BMaskC[5:4], 2'h0, BMaskC[1:0], 2'h0} : BMaskC;
 wire [7:0] BMaskA = AAddr[0] ? {BMaskB[6], 1'b0, BMaskB[4], 1'b0, BMaskB[2], 1'b0, BMaskB[0], 1'b0} : BMaskB;
 assign AMaskO = BMaskA;
endmodule

module MsShlMemData ( input [2:0] AAddr, input [63:0] ADataI, output [63:0] ADataO );
 wire [63:0] BDataC = AAddr[2] ? {ADataI[31:0], 32'h0} : ADataI;
 wire [63:0] BDataB = AAddr[1] ? {BDataC[47:32], 16'h0, BDataC[15:0], 16'h0} : BDataC;
 wire [63:0] BDataA = AAddr[0] ? {BDataB[55:48], 8'h0, BDataB[39:32], 8'h0, BDataB[23:16], 8'h0, BDataB[7:0], 8'h0} : BDataB;
 assign ADataO = BDataA;
endmodule

module MsShrMemData ( input [2:0] AAddr, input [63:0] ADataI, output [63:0] ADataO );
 wire [63:0] BDataC = AAddr[2] ? {32'h0, ADataI[63:32]} : ADataI;
 wire [63:0] BDataB = AAddr[1] ? {16'h0, BDataC[63:48], 16'h0, BDataC[31:16]} : BDataC;
 wire [63:0] BDataA = AAddr[0] ? {8'h0, BDataB[63:56], 8'h0, BDataB[47:40], 8'h0, BDataB[31:24], 8'h0, BDataB[15:8]} : BDataB;
 assign ADataO = BDataA;
endmodule

module MsMemCtrl #(parameter CCoreCnt=2, CDataCnt=3, CRomBase=32'h1000, CRomSize=32'h1000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CCoreCnt*32-1:0] ACodeAddr, output [63:0] ACodeMiso, input [CCoreCnt-1:0] ACodeReq, output [CCoreCnt-1:0] ACodeAck,
  input [CDataCnt*32-1:0] ADataAddr, output [63:0] ADataMiso, input [CDataCnt*64-1:0] ADataMosi, input [CDataCnt*4-1:0] ADataWrSize, ADataRdSize, output [CDataCnt-1:0] ADataAck,
  input [CCoreCnt*16-1:0] APortAddr, output [63:0] APortMiso, input [CCoreCnt*64-1:0] APortMosi, input [CCoreCnt*4-1:0] APortWrSize, APortRdSize, output [CCoreCnt-1:0] APortAck,
  output [31:3] ARomAddr, input [63:0] ARomMiso, output ARomRdEn,
  output [31:3] ARamAddr, input [63:0] ARamMiso, output [63:0] ARamMosi, output [7:0] ARamWrEn, ARamRdEn,
  output [15:0] AIoAddr, input [63:0] AIoMiso, output [63:0] AIoMosi, output [3:0] AIoWrSize, AIoRdSize, input AIoBusy
 );

 localparam CCoreNil = {CCoreCnt{1'b0}};

 // Local vars
 wire [CCoreCnt-1:0] FCodeIdx, BCodeIdx;
 wire [CDataCnt-1:0] FDataIdx, BDataIdx;
 wire FDataInRom, BDataInRom;
 wire [2:0] FDataScaleAddr, BDataScaleAddr;
 wire [7:0] FDataRdMask, BDataRdMask;
 wire [CCoreCnt-1:0] FPortIdx, BPortIdx;
 wire [15:0] FIoAddr, BIoAddr;
 wire [63:0] FIoMosi, BIoMosi;
 wire [3:0] FIoWrSize, BIoWrSize;
 wire [3:0] FIoRdSize, BIoRdSize;

 MsDffList #(.CRegLen(CCoreCnt+CDataCnt+1+3+8+CCoreCnt+16+64+4+4)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCodeIdx, BDataIdx, BDataInRom, BDataScaleAddr, BDataRdMask, BPortIdx, BIoAddr, BIoMosi, BIoWrSize, BIoRdSize}),
   .ADataO({FCodeIdx, FDataIdx, FDataInRom, FDataScaleAddr, FDataRdMask, FPortIdx, FIoAddr, FIoMosi, FIoWrSize, FIoRdSize})
  );

 // Code
 MsPrioritize #(.CLineCnt(CCoreCnt)) UCodeIdx
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(ACodeReq), .ADataO(BCodeIdx)
  );
 wire [31:0] BCodeAddr; MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(32)) UCodeAddr ( .ADataI(ACodeAddr), .AMask(BCodeIdx), .ADataO(BCodeAddr) );

 // Data
 wire [CDataCnt-1:0] BDataWrReq; MsMatrOrRow #(.CRowCnt(CDataCnt), .CColCnt(4)) UDataWrReq ( .ADataI(ADataWrSize), .ADataO(BDataWrReq) );
 wire [CDataCnt-1:0] BDataRdReq; MsMatrOrRow #(.CRowCnt(CDataCnt), .CColCnt(4)) UDataRdReq ( .ADataI(ADataRdSize), .ADataO(BDataRdReq) );
 wire [CDataCnt-1:0] BDataReq = BDataWrReq | BDataRdReq;
 MsPrioritize #(.CLineCnt(CDataCnt)) UDataIdx
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BDataReq), .ADataO(BDataIdx)
  );
 wire [31:0] BDataAddr; MsSelectRow #(.CRowCnt(CDataCnt), .CColCnt(32)) UDataAddr ( .ADataI(ADataAddr), .AMask(BDataIdx), .ADataO(BDataAddr) );
 wire [63:0] BDataMosi; MsSelectRow #(.CRowCnt(CDataCnt), .CColCnt(64)) UDataMosi ( .ADataI(ADataMosi), .AMask(BDataIdx), .ADataO(BDataMosi) );
 wire [3:0] BDataWrSize; MsSelectRow #(.CRowCnt(CDataCnt), .CColCnt(4)) UDataWrSize ( .ADataI(ADataWrSize), .AMask(BDataIdx), .ADataO(BDataWrSize) );
 wire [3:0] BDataRdSize; MsSelectRow #(.CRowCnt(CDataCnt), .CColCnt(4)) UDataRdSize ( .ADataI(ADataRdSize), .AMask(BDataIdx), .ADataO(BDataRdSize) );

 // Code/Data
 assign BDataInRom = (|BDataIdx) & (BDataAddr>=CRomBase) & (BDataAddr<(CRomBase+CRomSize));
 assign BDataScaleAddr = BDataAddr[2:0];
 wire [7:0] BDataWrMask = {{4{BDataWrSize[3]}}, {2{|BDataWrSize[3:2]}}, |BDataWrSize[3:1], |BDataWrSize};
 assign     BDataRdMask = {{4{BDataRdSize[3]}}, {2{|BDataRdSize[3:2]}}, |BDataRdSize[3:1], |BDataRdSize};
 wire [63:0] BDataWrMaskA; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) UDataWrMaskA ( .ADataI(BDataInRom ? 8'h0 : BDataWrMask), .ADataO(BDataWrMaskA) );
 wire [63:0] BDataMiso;
 wire [63:0] BDataRdMaskA; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) UDataRdMaskA ( .ADataI(FDataRdMask), .ADataO(BDataRdMaskA) );
 assign ADataMiso = BDataMiso & BDataRdMaskA;
 assign ACodeMiso = (|ACodeAck) ? ARomMiso : 64'h0;

 // Ram
 assign ARamAddr = BDataInRom ? 29'h0 : BDataAddr[31:3];
 MsShlMemData URamMosi ( .AAddr(BDataScaleAddr), .ADataI(BDataMosi & BDataWrMaskA), .ADataO(ARamMosi) );
 MsShlMemMask URamWrEn ( .AAddr(BDataScaleAddr), .AMaskI(BDataInRom ? 8'h0 : BDataWrMask), .AMaskO(ARamWrEn) );
 MsShlMemMask URamRdEn ( .AAddr(BDataScaleAddr), .AMaskI(BDataInRom ? 8'h0 : BDataRdMask), .AMaskO(ARamRdEn) );
 MsShrMemData URamMiso ( .AAddr(FDataScaleAddr), .ADataI(FDataInRom ? ARomMiso : ARamMiso), .ADataO(BDataMiso) );
 assign ADataAck = FDataIdx;

 // Rom
 assign ARomAddr = BDataInRom ? BDataAddr[31:3] : BCodeAddr[31:3];
 assign ARomRdEn = BDataInRom ? (|BDataRdSize) : (|BCodeIdx);
 assign ACodeAck = FDataInRom ? CCoreNil : FCodeIdx;

 // Port
 wire [CCoreCnt-1:0] BPortWrReq; MsMatrOrRow #(.CRowCnt(CCoreCnt), .CColCnt(4)) UPortWrReq ( .ADataI(APortWrSize), .ADataO(BPortWrReq) );
 wire [CCoreCnt-1:0] BPortRdReq; MsMatrOrRow #(.CRowCnt(CCoreCnt), .CColCnt(4)) UPortRdReq ( .ADataI(APortRdSize), .ADataO(BPortRdReq) );
 wire [CCoreCnt-1:0] BPortReq = BPortWrReq | BPortRdReq;
 wire [CCoreCnt-1:0] BPortIdxA;
 MsPrioritize #(.CLineCnt(CCoreCnt)) UPortIdx
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BPortReq), .ADataO(BPortIdxA)
  );

 assign BPortIdx = AIoBusy ? FPortIdx : BPortIdxA;
 MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(16)) UIoAddr ( .ADataI(APortAddr), .AMask(BPortIdx), .ADataO(BIoAddr) );
 MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(64)) UIoMosi ( .ADataI(APortMosi), .AMask(BPortIdx), .ADataO(BIoMosi) );
 MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(4)) UIoWrSize ( .ADataI(APortWrSize), .AMask(BPortIdx), .ADataO(BIoWrSize) );
 MsSelectRow #(.CRowCnt(CCoreCnt), .CColCnt(4)) UIoRdSize ( .ADataI(APortRdSize), .AMask(BPortIdx), .ADataO(BIoRdSize) );

 // Out
 assign APortAck = AIoBusy ? {CCoreCnt{1'b0}} : FPortIdx;
 assign APortMiso = AIoMiso;

 assign AIoAddr = FIoAddr;
 assign AIoMosi = FIoMosi;
 assign AIoWrSize = FIoWrSize;
 assign AIoRdSize = FIoRdSize;

endmodule

module MsRegX #(parameter CDataLen = 8)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CDataLen-1:0] ARegWrBusA, input ARegWrEnA,
  input [CDataLen-1:0] ARegWrBusB, input ARegWrEnB,
  output [CDataLen-1:0] ADataThis
 );

 wire [CDataLen-1:0] BWrData =
  ({CDataLen{ARegWrEnA}} & ARegWrBusA) |
  ({CDataLen{ARegWrEnB}} & ARegWrBusB);

 wire [CDataLen-1:0] FRegData, BRegData;
 MsDffList #(.CRegLen(CDataLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BRegData),
   .ADataO(FRegData)
  );

 assign BRegData = (ARegWrEnA | ARegWrEnB) ? BWrData : FRegData;
 assign ADataThis = FRegData;
endmodule

// ***********************
// *** #CmdQue Section ***
// ***********************

module MsCmdQue
 (
  input AClkH, input AResetHN, input AClkHEn,
  output [23:1] AQueLda, output [127:0] AQueTop,
  input [63:0] ACodeMiso, input ACodeAck,
  input AEipUpdate, input [23:1] AEipWrBus
 );

 wire [23:1] FQueLda, BQueLda;
 wire [127:0] FQueBuf, BQueBuf;
 MsDffList #(.CRegLen(23+128)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BQueLda, BQueBuf}),
   .ADataO({FQueLda, FQueBuf})
  );

 assign BQueLda = AEipUpdate ? AEipWrBus : (ACodeAck ? {FQueLda[23:3]+21'h1, 2'h0} : FQueLda);
 assign BQueBuf =
  {
   (ACodeAck &  FQueLda[3]) ? ACodeMiso : FQueBuf[127:64],
   (ACodeAck & ~FQueLda[3]) ? ACodeMiso : FQueBuf[ 63: 0]
  };

 assign AQueLda = FQueLda;
 assign AQueTop = FQueBuf;
endmodule

// ********************
// *** #Mio Section ***
// ********************

module MsMioCtrl #(parameter CAddrLen=32)
 (
  input AClkH, input AResetHN, input AClkHEn,
  output [CAddrLen-1:0] AExtAddr, output [63:0] AExtMosi, input [63:0] AExtMiso, output [3:0] AExtWrSize, AExtRdSize, input AExtReady,
  input [1:0] AWrRdEn, input [11:0] ARegIdx, input [1:0] AArgSize, input [2:0] ASignExt,
  input [CAddrLen-1:0] AAddr, input [63:0] AData,
  output [63:0] ABusBData, output [11:0] ABusBLoad, output [11:0] ABusBPend,
  output APendAny
 );

 wire [CAddrLen-1:0] FAddr, BAddr;
 wire [63:0] FData, BData;
 wire [1:0] FArgSize, BArgSize;
 wire [2:0] FSignExt, BSignExt;

 MsDffList #(.CRegLen(CAddrLen+64+2+3)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BAddr, BData, BArgSize, BSignExt}),
   .ADataO({FAddr, FData, FArgSize, FSignExt})
  );

 wire [1:0] BWrRdActive;

 wire BWrRdEnNZ = |AWrRdEn;
 //wire BWrRdActiveNZ = |BWrRdActive;

 assign BArgSize = BWrRdEnNZ ? AArgSize : FArgSize; // BWrRdActiveNZ
 assign BAddr = BWrRdEnNZ ? AAddr : FAddr;
 assign BData = (AWrRdEn[1] ? AData : 64'h0) | (AExtReady ? 64'h0 : FData);
 assign BSignExt = BWrRdEnNZ ? ASignExt : FSignExt;

 wire [3:0] BArgSizeDec; MsDec2x4a UArgSizeDec ( .ADataI(BArgSize), .ADataO(BArgSizeDec) );

 wire [63:0] BExtMiso;
 assign BExtMiso[63:32] = AExtMiso[63:32];
 MsSignExt UExtMiso ( .AData(AExtMiso[31:0]), .AWw(FSignExt[1:0]), .ASignExt(FSignExt[2]), .AResult(BExtMiso[31:0]) );

 MsBusBCtrl #(.CDataLen(64)) UBusB
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrRdEn(AWrRdEn), .ARegIdx(ARegIdx),
   .AExtMiso(BExtMiso), .AExtAck(AExtReady),
   .AWrRdActive(BWrRdActive), 
   .ABusBData(ABusBData), .ABusBLoad(ABusBLoad), .ABusBPend(ABusBPend),
   .APendAny(APendAny)
  );

 assign AExtAddr = BAddr;
 assign AExtMosi = BData;
 assign AExtWrSize = BWrRdActive[1] ? BArgSizeDec : 3'h0;
 assign AExtRdSize = BWrRdActive[0] ? BArgSizeDec : 3'h0;
endmodule

// *********************
// *** #BusB Section ***
// *********************

module MsBusBCtrl_20210426 #(parameter CDataLen=32)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] AWrRdEn, input [11:0] ARegIdx,
  input [CDataLen-1:0] AExtMiso, input AExtAck,
  output [1:0] AWrRdActive,
  output [CDataLen-1:0] ABusBData, output [11:0] ABusBLoad, ABusBPend,
  output APendAny
 );

 localparam CRegIdxNil = 12'h0;

 wire  [1:0] FState, BState;
 wire  [1:0] FOper, BOper;
 wire        FPendAny, BPendAny;
 wire [11:0] FLoadRegIdx, BLoadRegIdx;
 wire [11:0] FPendRegIdx, BPendRegIdx;
 wire [CDataLen-1:0] FExtMiso, BExtMiso;

 MsDffList #(.CRegLen(2+2+1+2*12+CDataLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BState, BOper, BPendAny, BLoadRegIdx, BPendRegIdx, BExtMiso}),
   .ADataO({FState, FOper, FPendAny, FLoadRegIdx, FPendRegIdx, FExtMiso})
  );

 wire BWrRdEnNZ = |AWrRdEn;

 assign BState[0] = BWrRdEnNZ | (FState[0] & ~AExtAck);
 assign BState[1] = FState[0] & AExtAck;

 wire BPendA = FState[0] & ~AExtAck;

 assign BPendAny = BWrRdEnNZ | (BPendA & FPendAny);

 assign BOper = AWrRdEn | (BPendA ? FOper : 2'h0);

 assign BPendRegIdx = (AWrRdEn[1] ? ARegIdx : CRegIdxNil) |
                      (AWrRdEn[0] ? ARegIdx : CRegIdxNil) |
                      (BPendA ? FPendRegIdx : CRegIdxNil);
 assign BLoadRegIdx = (BState[1] & FOper[0]) ? FPendRegIdx : CRegIdxNil;

 assign BExtMiso = (AExtAck & FOper[0]) ? AExtMiso : FExtMiso;

 assign AWrRdActive = BState[0] ? BOper : 2'h0;

 assign ABusBData = FState[1] ? FExtMiso : {CDataLen{1'b0}};
 assign ABusBLoad = FLoadRegIdx;
 assign ABusBPend = FPendRegIdx;
 assign APendAny = FPendAny;
endmodule

module MsBusBCtrl #(parameter CDataLen=32)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] AWrRdEn, input [11:0] ARegIdx,
  input [CDataLen-1:0] AExtMiso, input AExtAck,
  output [1:0] AWrRdActive,
  output [CDataLen-1:0] ABusBData, output [11:0] ABusBLoad, ABusBPend,
  output APendAny
 );

 localparam CRegIdxNil = 12'h0;

 wire  [1:0] FOper, BOper;
 wire        FPendAny, BPendAny;
 wire [11:0] FLoadRegIdx, BLoadRegIdx;
 wire [11:0] FPendRegIdx, BPendRegIdx;

 MsDffList #(.CRegLen(2+1+2*12)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BOper, BPendAny, BLoadRegIdx, BPendRegIdx}),
   .ADataO({FOper, FPendAny, FLoadRegIdx, FPendRegIdx})
  );

 wire BWrRdEnNZ = |AWrRdEn;

 assign BPendAny = (BWrRdEnNZ | FPendAny) & ~AExtAck;

 wire BKeepAll = FPendAny & ~AExtAck;

 assign BOper =
  (BWrRdEnNZ ? AWrRdEn : 2'h0) |
  (BKeepAll ? FOper : 2'h0);

 assign BPendRegIdx = (BWrRdEnNZ ? ARegIdx : CRegIdxNil) |
                      (BKeepAll ? FPendRegIdx : CRegIdxNil);
 assign BLoadRegIdx = ((AWrRdEn[0] & AExtAck) ? ARegIdx : CRegIdxNil) |
                      ((FOper[0] & AExtAck) ? FPendRegIdx : CRegIdxNil);

 assign AWrRdActive = BOper;

 assign ABusBData = ((AWrRdEn[0] | FOper[0]) & AExtAck) ? AExtMiso : {CDataLen{1'b0}};
 assign ABusBLoad = BLoadRegIdx;
 assign ABusBPend = BPendRegIdx;
 assign APendAny  = BPendAny;
endmodule

module MsSignExt ( input [31:0] AData, input [1:0] AWw, input ASignExt, output [31:0] AResult );
 wire [31:0] BMaskOr =
   ((AWw==2'h1) ? {{16{ASignExt & AData[15]}}, 16'h0} : 32'h0) |
   ((AWw==2'h0) ? {{24{ASignExt & AData[ 7]}},  8'h0} : 32'h0);
 wire [31:0] BMaskAnd =
  ((AWw==2'h1) ? 32'h0000FFFF : 32'hFFFFFFFF) &
  ((AWw==2'h0) ? 32'h000000FF : 32'hFFFFFFFF);
 assign AResult = (AData & BMaskAnd) | BMaskOr;
endmodule

// ***********************
// *** #MulDiv Section ***
// ***********************

/*
 There are 3 common registers for all operations. But data is loaded differently.
 Addition is performed on A and B registers (somewhat differently for mul and div).
 For mul we don't care about the sigh: it will be correct for signed and unsigned mul.
 For div, S part is always negative (because we use SUB and not ADD). We play with D
 negation to obtain a correct result. In fact, we negate D if S is originally negative
 and not need to be negated.

 DivI                         MulI
          D                              S
          |                              |
 AAAA <- CCCC <- Res          AAAA Res->CCCC->
 BBBB                         BBBB
  |                            |
  S                            D


 DivF
  D       0
  |       |
 AAAA <- CCCC <- Res
 BBBB
  |
  S

*/

module MsMulDiv
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [31:0] ADataS, input [31:0] ADataD, input [2:0] ASizeI, input ASizeF, input [1:0] AStart,
  input [2:0] ANegData,
  output [31:0] ADataH, ADataR, output ABusy, output ABusBWrEn // ADataH = Hi part for Mul, Resid for div
 );

 wire [31:0] FDataC, BDataC;
 wire [32:0] FDataB, BDataB;
 wire [31:0] FDataA, BDataA;
 wire [2:0] FStep, BStep;
 wire [1:0] FOper, BOper;
 wire [2:0] FSizeI, BSizeI;
 wire FSizeF, BSizeF;
 wire [1:0] FNegRes, BNegRes; // DataH:DataR

 MsDffList #(.CRegLen(32+33+32+3+2+3+1+2)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDataC, BDataB, BDataA, BStep, BOper, BSizeI, BSizeF, BNegRes}),
   .ADataO({FDataC, FDataB, FDataA, FStep, FOper, FSizeI, FSizeF, FNegRes})
  );

 wire BStartNZ = |AStart;
 wire BStepNZ = |FStep;
 wire BOperNZ = |FOper;

 wire [31:0] BDataS = (ANegData[1] ^ AStart[1]) ? ~ADataS+32'h1 : ADataS;
 wire [31:0] BDataD =  ANegData[0]              ? ~ADataD+32'h1 : ADataD;

 wire [3:0] BStepCnt = ASizeF ? (AStart[1] ? 4'h6 : 4'h5) : {ASizeI, 1'b0}-4'h1;

 wire BDataSNZ = |ADataS;
 wire BZeroDiv = AStart[1] & ~BDataSNZ;

 assign BOper = BStepNZ ? FOper : AStart;
 assign BStep = BStartNZ ? BStepCnt[2:0] : FStep-{2'h0, BStepNZ};
 assign BSizeI = BStartNZ ? ASizeI : (BStepNZ ? FSizeI : 3'h0);
 assign BSizeF = BStartNZ ? ASizeF : (BStepNZ ? FSizeF : 1'h0);
 assign BNegRes = BStartNZ ? {2{ANegData[2]}} & {1'b1, (AStart[1] & ~BZeroDiv) | AStart[0]} : (BStepNZ ? FNegRes : 2'h0);
 //assign BNegRes = BStartNZ ? {2{ANegData[2]}} & {1'b1, |AStart} : (BStepNZ ? FNegRes : 2'h0);


 wire [3:0] BDivBitN;
 wire [35:0] BAuxEU =
  (FOper[1] ? {FDataA, FDataC[31:28]} : 36'h0) |
  (FOper[0] ? {4'h0, FDataA} : 36'h0);
 wire [35:0] BAuxDT = BAuxEU + ((FOper[1] | (FOper[0] & FDataC[3])) ? {                 FDataB, 3'h0} : 36'h0); wire [35:0] BAuxDU = (FOper[1] & BDivBitN[3]) ? BAuxEU : BAuxDT;
 wire [35:0] BAuxCT = BAuxDU + ((FOper[1] | (FOper[0] & FDataC[2])) ? {{1{FDataB[32]}}, FDataB, 2'h0} : 36'h0); wire [35:0] BAuxCU = (FOper[1] & BDivBitN[2]) ? BAuxDU : BAuxCT;
 wire [35:0] BAuxBT = BAuxCU + ((FOper[1] | (FOper[0] & FDataC[1])) ? {{2{FDataB[32]}}, FDataB, 1'h0} : 36'h0); wire [35:0] BAuxBU = (FOper[1] & BDivBitN[1]) ? BAuxCU : BAuxBT;
 wire [35:0] BAuxAT = BAuxBU + ((FOper[1] | (FOper[0] & FDataC[0])) ? {{3{FDataB[32]}}, FDataB      } : 36'h0); wire [35:0] BAuxAU = (FOper[1] & BDivBitN[0]) ? BAuxBU : BAuxAT;

 assign BDivBitN = {BAuxDT[35], BAuxCT[35], BAuxBT[35], BAuxAT[35]};

 assign BDataA =
   ((AStart[1] & ASizeF) ? {9'h0, BDataD[23:1]} : 32'h0) |
   ((BStepNZ & FOper[1]) ? BAuxAU[31:0] : 32'h0) |
   ((BStepNZ & FOper[0]) ? BAuxAU[35:4] : 32'h0);

 assign BDataB =
  (AStart[1] ? {BDataSNZ, BDataS} : 33'h0) |
  (AStart[0] ? {1'b0, BDataD} : 33'h0) |
  (BStepNZ ? FDataB : 33'h0);

 wire [31:0] BDataDDiv =
  (ASizeF ? {BDataD[0], 31'h0} : 32'h0) |
  (ASizeI[2] ?  BDataD               : 32'h0) |
  (ASizeI[1] ? {BDataD[15:0], 16'h0} : 32'h0) |
  (ASizeI[0] ? {BDataD[ 7:0], 24'h0} : 32'h0);

 wire [31:0] BDataCD = {FDataC[27:0], ~BDivBitN};
 wire [31:0] BDataCM = {BAuxAU[3:0], FDataC[31:4]};

 assign BDataC =
  (AStart[1] ? BDataDDiv : 32'h0) |
  (AStart[0] ? BDataS : 32'h0) |
  ((BStepNZ & FOper[1]) ? BDataCD : 32'h0) |
  ((BStepNZ & FOper[0]) ? BDataCM : 32'h0);

 wire [31:0] BDataRMul =
  //(FSizeF ? {7'h0, BAuxAT[27:3]} : 32'h0) |
  (FSizeF ? {3'h0, BAuxAT[27:0], 1'b0} : 32'h0) |
  (FSizeI[2] ?         BDataCM         : 32'h0) |
  (FSizeI[1] ? {16'h0, BDataCM[31:16]} : 32'h0) |
  (FSizeI[0] ? {24'h0, BDataCM[31:24]} : 32'h0);

 wire [31:0] BDataR =
  (FOper[1] ? BDataCD : 32'h0) |
  (FOper[0] ? BDataRMul : 32'h0);
 wire [31:0] BDataH =
  (FOper[1] ? BAuxAU[31:0] : 32'h0) |
  (FOper[0] ? BAuxAU[35:4] : 32'h0);
 wire BDataRNZ = |BDataR;

 assign ADataH = ABusBWrEn ? (FNegRes[1] ? ~BDataH + {31'h0, ~BDataRNZ | FOper[1]} : BDataH) : 32'h0;
 assign ADataR = ABusBWrEn ? (FNegRes[0] ? ~BDataR + 32'h1 : BDataR) : 32'h0;
 assign ABusy = BOperNZ;
 assign ABusBWrEn = BOperNZ & ~BStepNZ;
endmodule

