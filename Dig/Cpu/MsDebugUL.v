// Lite version of MsDebug. Flash load only

module MsDebugU #(parameter CAddrBase=16'h0000, CMcuType=8'h08, CCoreCnt=8'h2, CBrkCnt=8'h8, CProgBaudLen=3, CProgBaudDiv=3'h7, CRomBase=32'h0000, CRomSize=32'h0000, CProgStart=24'h800000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  // CPU
  output ADbgExecEn, output ADbgResetS, output ADbgClkHEn, output ADbgStep,
  output [CCoreCnt-1:0] ADbgCoreIdx, output [7:0] ADbgRegIdx, input [63:0] ARegMiso,
  input [CCoreCnt-1:0] ATEndList, ATrapList, input [CCoreCnt*32-1:0] AIpThis, input [CCoreCnt-1:0] ACmdDecReady,
  // Mem
  output AMemAccess, output [31:3] AMemAddr, input [63:0] AMemMiso, output [63:0] AMemMosi, output [1:0] AMemWrRdEn,
  // Dbg bridge
  input ADbgRx, output ADbgTx, output ADbgTxFlush,
  // Sync
  input ASync1M, input ASync1K,
  // Flash
  input ALoadFW,
  output AFlashMaster, input AFlashMiso, output AFlashMosi, output AFlashSck, output AFlashNCS,
  // Fpga reflash
  input [3:0] AStartupI, output [3:0] AStartupO, output [3:0] AStartupE,
  // Test
  output [7:0] ATest
 );

 wire [11:0] BDbioAddrU, BDbioAddrF;
 wire [63:0] BDbioMosiU, BDbioMosiF;
 wire [63:0] BDbioMiso;
 wire [3:0] BDbioMosiIdxU, BDbioMosiIdxF;
 wire [3:0] BDbioMisoIdxU, BDbioMisoIdxF;
 wire BDbioMosi1stU, BDbioMosi1stF;
 wire BDbioMiso1stU, BDbioMiso1stF;
 wire BDbioDataLenNZU;
 wire BDbioIdxReset;
 wire BDbgAttReq;

 wire BLdrActive;
 wire BLdrSbActive; // SendBack active (used to block UartFlush during SendBack where pauses may happen)

 wire [7:0] BLdrSbData; wire BLdrSbNow; // Data to be sent back immediately
 wire [7:0] BTestUart;
 wire [7:0] BTestFsm;

 TestBridgeUartA UTestBridge
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADbgRx(ADbgRx), .ADbgTx(ADbgTx), .ADbgTxFlush(ADbgTxFlush),
   .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ADbioAddr(BDbioAddrU), .ADbioMosi(BDbioMosiU), .ADbioMiso(BDbioMiso), .ADbioMosiIdx(BDbioMosiIdxU), .ADbioMisoIdx(BDbioMisoIdxU), .ADbioMosi1st(BDbioMosi1stU), .ADbioMiso1st(BDbioMiso1stU), .ADbioDataLenNZ(BDbioDataLenNZU), .ADbioIdxReset(BDbioIdxReset),
   .ADbioSbData(BLdrSbData), .ADbioSbNow(BLdrSbNow), .ADbioSbActive(BLdrSbActive),
   .ADbgAttReq(BDbgAttReq),
   .ATest(BTestUart)
  );
 wire BTestFsmReady;
 MsTestFsm UTestFsm
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ALoadFW(ALoadFW), .ALdrActive(BLdrActive),
   .ADbioAddr(BDbioAddrF), .ADbioMosi(BDbioMosiF), .ADbioMosiIdx(BDbioMosiIdxF), .ADbioMisoIdx(BDbioMisoIdxF), .ADbioMosi1st(BDbioMosi1stF), .ADbioMiso1st(BDbioMiso1stF),
   .AReady(BTestFsmReady),
   .ATest(BTestFsm)
  );

 wire [11:0] BDbioAddr = BTestFsmReady ? BDbioAddrU : BDbioAddrF;
 wire [63:0] BDbioMosi = BTestFsmReady ? BDbioMosiU : BDbioMosiF;
 wire [3:0] BDbioMosiIdx = BTestFsmReady ? BDbioMosiIdxU : BDbioMosiIdxF;
 wire [3:0] BDbioMisoIdx = BTestFsmReady ? BDbioMisoIdxU : BDbioMisoIdxF;
 wire BDbioMosi1st = BTestFsmReady ? BDbioMosi1stU : BDbioMosi1stF;
 wire BDbioMiso1st = BTestFsmReady ? BDbioMiso1stU : BDbioMiso1stF;
 wire BDbioDataLenNZ = BTestFsmReady ? BDbioDataLenNZU : 1'b0;

 // Functional group access
 wire BDbioTestUCS = (BDbioAddr[11:8]==4'h0);
 wire BDbioTestLCS = (BDbioAddr[11:8]==4'h1);

 wire BDbioIdxResetU, BDbioIdxResetL; assign BDbioIdxReset = |{BDbioIdxResetU, BDbioIdxResetL};
 wire [63:0] BDbioMisoU, BDbioMisoL;  assign BDbioMiso = BDbioMisoU | BDbioMisoL;

 wire [31:3] BMemAddrU, BMemAddrL;
 wire [63:0] BMemMosiU, BMemMosiL;
 wire [1:0] BMemWrRdEnU; wire BMemWrEnL;
 wire BMemAccessU, BMemAccessL;

 wire [7:0] BTestBU;
 MsTestBU #(.CAddrBase(CAddrBase), .CBrkCnt(CBrkCnt), .CMcuType(CMcuType), .CCoreCnt(CCoreCnt)) UTestU
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASync1M(ASync1M),
   .AIoAddr(AIoAddr), .AIoMiso(AIoMiso), .AIoMosi(AIoMosi), .AIoWrSize(AIoWrSize),  .AIoRdSize(AIoRdSize), .AIoAddrAck(AIoAddrAck), .AIoAddrErr(AIoAddrErr),
   .ADbioAddr(BDbioAddr[7:0]), .ADbioMosi(BDbioMosi), .ADbioMiso(BDbioMisoU), .ADbioMosiIdx(BDbioTestUCS ? BDbioMosiIdx : 4'h0), .ADbioMisoIdx(BDbioTestUCS ? BDbioMisoIdx : 4'h0), .ADbioMosi1st(BDbioTestUCS & BDbioMosi1st), .ADbioMiso1st(BDbioTestUCS & BDbioMiso1st), .ADbioDataLenNZ(BDbioTestUCS & BDbioDataLenNZ), .ADbioIdxReset(BDbioIdxResetU),
   .ADbgExecEn(ADbgExecEn), .ADbgResetS(ADbgResetS), .ADbgClkHEn(ADbgClkHEn), .ADbgStep(ADbgStep),
   .AMemAccess(BMemAccessU), .AMemAddr(BMemAddrU), .AMemMiso(AMemMiso), .AMemMosi(BMemMosiU), .AMemWrRdEn(BMemWrRdEnU),
   .AIpThis(AIpThis), .ACmdDecReady(ACmdDecReady),
   .ADbgCoreIdx(ADbgCoreIdx), .ADbgRegIdx(ADbgRegIdx), .ARegMiso(ARegMiso),
   .ATEndList(ATEndList), .ATrapList(ATrapList), .AAttReq(BDbgAttReq),
   .AStartupI(AStartupI), .AStartupO(AStartupO), .AStartupE(AStartupE),
   .ATest(BTestBU)
  );

 wire [7:0] BTestLdr;
 MsTestLdr #(.CBaudLen(CProgBaudLen), .CBaudDiv(CProgBaudDiv), .CRomBase(CRomBase), .CRomSize(CRomSize), .CProgStart(CProgStart)) UTestLdr
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADbioAddr(BDbioAddr[7:0]), .ADbioMosi(BDbioMosi), .ADbioMiso(BDbioMisoL), .ADbioMosiIdx(BDbioTestLCS ? BDbioMosiIdx : 4'h0), .ADbioMisoIdx(BDbioTestLCS ? BDbioMisoIdx : 4'h0), .ADbioMosi1st(BDbioTestLCS & BDbioMosi1st), .ADbioMiso1st(BDbioTestLCS & BDbioMiso1st), .ADbioDataLenNZ(BDbioTestLCS & BDbioDataLenNZ), .ADbioIdxReset(BDbioIdxResetL),
   .ADbioSbData(BLdrSbData), .ADbioSbNow(BLdrSbNow),
   .AMemAccess(BMemAccessL), .AMemAddr(BMemAddrL), .AMemMiso(AMemMiso), .AMemMosi(BMemMosiL), .AMemWrEn(BMemWrEnL),
   .ASpiMaster(AFlashMaster), .ASpiMiso(AFlashMiso), .ASpiMosi(AFlashMosi), .ASpiSck(AFlashSck), .ASpiNCS(AFlashNCS),
   .AActive(BLdrActive), .ASbActive(BLdrSbActive),
   .ATest(BTestLdr)
  );


 assign {AMemAddr, AMemMosi, AMemWrRdEn} =
   (BMemAccessU ? {BMemAddrU, BMemMosiU, BMemWrRdEnU    } : {29'h0, 64'h0, 2'h0}) |
   (BMemAccessL ? {BMemAddrL, BMemMosiL, BMemWrEnL, 1'b0} : {29'h0, 64'h0, 2'h0});
 assign AMemAccess = BMemAccessU | BMemAccessL;

 assign ATest = BTestBU;
endmodule

// Register A is sent first, then B and so on. StateWord at the end
module MsTestBU #(parameter CAddrBase=16'h0000, CBrkCnt=8'h8, CMcuType=8'h08, CCoreCnt=8'h2)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ASync1M,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  input [7:0] ADbioAddr, input [63:0] ADbioMosi, output [63:0] ADbioMiso, input [3:0] ADbioMosiIdx, input [3:0] ADbioMisoIdx, input ADbioMosi1st, input ADbioMiso1st, input ADbioDataLenNZ, output ADbioIdxReset,
  output ADbgExecEn, output ADbgResetS, output ADbgClkHEn, output ADbgStep,
  output AMemAccess, output [31:3] AMemAddr, input [63:0] AMemMiso, output [63:0] AMemMosi, output [1:0] AMemWrRdEn,
  output [CCoreCnt-1:0] ADbgCoreIdx, output [7:0] ADbgRegIdx, input [63:0] ARegMiso,
  input [CCoreCnt*32-1:0] AIpThis, input [CCoreCnt-1:0] ACmdDecReady,
  input [CCoreCnt-1:0] ATEndList, ATrapList, output AAttReq,
  input [3:0] AStartupI, output [3:0] AStartupO, output [3:0] AStartupE,
  output [7:0] ATest
 );

 localparam CCoreNil = {CCoreCnt{1'b0}};

 localparam CAddrCpuCtrlWr   = 8'h00;  // BMemAccess 3xRFU RFU BDbgExecEn BDbgResetS BDbgClkHEn
 localparam CAddrCpuStepWr   = 8'h01;
 localparam CAddrMemDataWr   = 8'h02;
 localparam CAddrMemAddrWr   = 8'h03;
 localparam CAddrBrkListWr   = 8'h05;
 localparam CAddrBrkThisWr   = 8'h06;
 localparam CAddrStartupWr   = 8'h07;

 localparam CAddrCpuStatRd   = 8'h00;
 localparam CAddrCpuInfoRd   = 8'h01;
 localparam CAddrMemDataRd   = 8'h02;
 localparam CAddrMemAddrRd   = 8'h03;
 localparam CAddrCpuRegsRd   = 8'h04;
 localparam CAddrTtyDataRd   = 8'h07;

 wire FMemAccess, BMemAccess;
 wire FDbgExecEn, BDbgExecEn;
 wire FDbgResetS, BDbgResetS;
 wire FDbgClkHEn, BDbgClkHEn;
 wire FDbgStep, BDbgStep;
 wire [31:3] FMemAddr, BMemAddr;
 wire [CBrkCnt*32-1:0] FBrkList, BBrkList;
 wire [CBrkCnt-1:0] FBrkIdx, BBrkIdx;
 wire [31:0] FBrkThis, BBrkThis;
 wire [CCoreCnt-1:0] FBreakReq, BBreakReq;
 wire FIsStop, BIsStop;
 wire FAttReq, BAttReq;
 wire [CCoreCnt-1:0] FBreakList, BBreakList;
 wire [CCoreCnt-1:0] FTrapList, BTrapList;
 wire [CCoreCnt-1:0] FTEndList, BTEndList;
 wire FTtySendHasData, BTtySendHasData;
 wire [CCoreCnt-1:0] FDbgCoreIdx, BDbgCoreIdx;
 wire [7:0] FDbgRegIdx, BDbgRegIdx;
 wire [3:0] FStartupI, BStartupI;
 wire [3:0] FStartupO, BStartupO;
 wire [3:0] FStartupE, BStartupE;

 MsDffList #(.CRegLen(1+1+1+1+1+29+CBrkCnt*32+CBrkCnt+32+CCoreCnt+1+1+CCoreCnt*3+1+CCoreCnt+8+3*4)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BMemAccess, BDbgExecEn, BDbgResetS, BDbgClkHEn, BDbgStep, BMemAddr, BBrkList, BBrkIdx, BBrkThis, BBreakReq, BIsStop, BAttReq, BBreakList, BTrapList, BTEndList, BTtySendHasData, BDbgCoreIdx, BDbgRegIdx, BStartupI, BStartupO, BStartupE}),
   .ADataO({FMemAccess, FDbgExecEn, FDbgResetS, FDbgClkHEn, FDbgStep, FMemAddr, FBrkList, FBrkIdx, FBrkThis, FBreakReq, FIsStop, FAttReq, FBreakList, FTrapList, FTEndList, FTtySendHasData, FDbgCoreIdx, FDbgRegIdx, FStartupI, FStartupO, FStartupE})
  );

 // Common Io/Tty
 wire BTtySendHasSpace; wire [15:0] BTtySendSize;

 // IO
 // IobDbgTestCtrl       equ 0xFC ; // RD: 4'h0 2'h0 TermSpace 1'b0
 // IobDbgTermData       equ 0xFE

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h000000C9)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );


 assign AIoMiso =
  (BIoAccess[IoSizeB+IoOperR+0] ? {32'h0, 8'h0, 4'h0, 1'b0, 1'b0, BTtySendHasSpace, 1'b0} : 32'h0) |
  (BIoAccess[IoSizeB+IoOperR+3] ? {32'h0, 8'h0, 4'h0, FStartupI} : 32'h0);

 assign BStartupI = AStartupI;

 // Terminal
 wire [7:0] BTtySendBoei; // Send Buf_Out_Ext_In
 wire BTtySendFifoRd;
 MsFifo256b UTtySendFifo
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[7:0]), .AWrEn(BIoAccess[IoSizeB+IoOperW+2]),
   .ADataO(BTtySendBoei), .ARdEn(BTtySendFifoRd),
   .AHasData(BTtySendHasData), .AHasSpace(BTtySendHasSpace), .ADataSize(BTtySendSize)
  );

 // Common
 wire BIsStopNotConf; // Break not confirmed (the line just after jmp/call)
 wire BIsStopCancel;  // Break cancelled (if breakpoint is not reached)

 // DBIO
 wire BMemAddrInc;

 wire BCpuCtrlWr = (ADbioAddr==CAddrCpuCtrlWr) & ADbioMosiIdx[0]; assign {BMemAccess, BDbgExecEn, BDbgResetS, BDbgClkHEn} = BCpuCtrlWr ? {ADbioMosi[7], ADbioMosi[2:0]} : {FMemAccess, (FDbgExecEn & ~BIsStopNotConf) | BIsStopCancel, FDbgResetS, FDbgClkHEn};
 wire BCpuStepWr = (ADbioAddr==CAddrCpuStepWr) & ADbioMosiIdx[0]; assign BDbgStep = BCpuStepWr ? ADbioMosi[0] : 1'h0;
 wire BMemDataWr = (ADbioAddr==CAddrMemDataWr) & ADbioMosiIdx[3];
 wire BMemAddrWr = (ADbioAddr==CAddrMemAddrWr) & ADbioMosiIdx[2]; assign BMemAddr = BMemAddrWr ? ADbioMosi[31:3] : FMemAddr + {28'h0, BMemAddrInc};
 wire BBrkListWr = (ADbioAddr==CAddrBrkListWr) & ADbioMosiIdx[2]; assign BBrkIdx  = ((ADbioAddr==CAddrBrkListWr) & ADbioMosi1st) ? {{(CBrkCnt-1){1'b0}}, 1'b1} : (BBrkListWr ? {FBrkIdx[CBrkCnt-2:0], 1'b0} : FBrkIdx);
 wire BBrkThisWr = (ADbioAddr==CAddrBrkThisWr) & ADbioMosiIdx[2]; assign BBrkThis = BBrkThisWr ? ADbioMosi[31:0] : FBrkThis;
 wire BStartupWr = (ADbioAddr==CAddrStartupWr) & ADbioMosiIdx[0]; assign {BStartupE, BStartupO} = BStartupWr ? ADbioMosi[7:0] : (BIoAccess[IoSizeB+IoOperW+3] ? AIoMosi[7:0] : {FStartupE, FStartupO});

 MsVectMux1a #(.CVectLen(32)) UBrkList[CBrkCnt-1:0] ( .ADataA(FBrkList), .ADataB({CBrkCnt{ADbioMosi[31:0]}}), .ADataO(BBrkList), .AAddr(FBrkIdx) );

 // CPU and ROM
 wire BCpuStatRd = (ADbioAddr==CAddrCpuStatRd) & ADbioMiso1st;
 wire [1:0] BCpuInfoRdX = {2{ADbioAddr==CAddrCpuInfoRd}} & {ADbioMisoIdx[2], ADbioMiso1st};
 wire BCpuInfoRd = |BCpuInfoRdX;
 wire [1:0] BMemDataRdX = {2{ADbioAddr==CAddrMemDataRd}} & {ADbioMisoIdx[3], ADbioMiso1st};
 wire BMemDataRd = |BMemDataRdX;
 wire BMemAddrRd = (ADbioAddr==CAddrMemAddrRd) & ADbioMiso1st;
 wire [1:0] BCpuRegsRdX = {2{ADbioAddr==CAddrCpuRegsRd}} & {ADbioMisoIdx[3], ADbioMiso1st};
 wire BCpuRegsRd = |BCpuRegsRdX;
 // Terminal
 wire [1:0] BTtyDataRdX = {2{ADbioAddr==CAddrTtyDataRd}} & {ADbioMisoIdx[0], ADbioMiso1st};
 wire BTtyDataRd = |BTtyDataRdX;

 assign BMemAddrInc = BMemDataWr | (BMemDataRd & ADbioDataLenNZ);

 assign ADbioIdxReset =
  |{
    BCpuCtrlWr, BCpuStepWr, BMemDataWr, BMemAddrWr, BBrkListWr, BStartupWr,
    BCpuStatRd, BCpuInfoRd, BMemDataRd, BMemAddrRd, BCpuRegsRd, BTtyDataRd
   };

 wire [63:0] BCpuStat = {8'h0, {(8-CCoreCnt){1'b0}}, FTEndList, BTtySendSize, {(8-CCoreCnt){1'b0}}, FTrapList, {(8-CCoreCnt){1'b0}}, FBreakList, 8'h0, FMemAccess, 3'h0, FIsStop, FDbgExecEn, FDbgResetS, FDbgClkHEn};

 assign BTtySendFifoRd = BTtyDataRd & ADbioDataLenNZ;

 wire [7:0] BCoreCnt = CCoreCnt[7:0];
 wire [7:0] BMcuType = CMcuType;
 
 assign ADbioMiso =
  (BCpuStatRd ? BCpuStat : 64'h0) |
  (BCpuInfoRd ? {32'h0, 16'h0, BMcuType, BCoreCnt} : 64'h0) |
  (BMemDataRd ? AMemMiso : 64'h0) |
  (BMemAddrRd ? {32'h0, FMemAddr, 3'h0} : 64'h0) |
  (BCpuRegsRd ? ARegMiso : 64'h0) |
  (BTtySendFifoRd ? {56'h0, BTtySendBoei} : 64'h0);

 wire BDbgRegsReset = |{ADbioMosi1st, ADbioMiso1st & (ADbioAddr!=CAddrCpuRegsRd)};
 wire [CCoreCnt:0] BDbgCoreIdxShlE = {FDbgCoreIdx, 1'b0};
 assign BDbgCoreIdx = BCpuRegsRdX[0] ? {{(CCoreCnt-1){1'b0}}, 1'b1} : ((BCpuRegsRdX[1] & FDbgRegIdx[7]) ? BDbgCoreIdxShlE[CCoreCnt-1:0] : (BDbgRegsReset ? CCoreNil : FDbgCoreIdx));
 assign BDbgRegIdx  = BCpuRegsRdX[0] ? 8'h1 : (BCpuRegsRdX[1] ? (FDbgRegIdx[7] ? {7'h0, ~FDbgCoreIdx[CCoreCnt-1]} : {FDbgRegIdx[6:0], 1'b0}) : (BDbgRegsReset ? 8'h0 : FDbgRegIdx));

 // Break can be errorneously sensed after JMP. In this case CmdDecReady will disappear, when it reappears, BreakListA can be zero (i.e. IP has changed)
 wire [CCoreCnt-1:0] BBreakListA; MsBrkCmp #(.CBrkCnt(1+CBrkCnt)) UBreakList[CCoreCnt-1:0] ( .ABrkIp(AIpThis), .ABrkCmp({FBrkThis, FBrkList}), .AIsBreak(BBreakListA) );
 wire BBreakListNZ = |BBreakListA;
 wire BCmdDecReady = &ACmdDecReady;
 wire [CCoreCnt-1:0] BBreakAck = BCmdDecReady ? FBreakReq & BBreakListA : CCoreNil;
 wire BBreakAckNZ = |BBreakAck;
 wire BBreakReqNZ = |FBreakReq;
 
 assign BBreakReq = ({CCoreCnt{FDbgExecEn}} & BBreakListA) | (BCmdDecReady ? CCoreNil : FBreakReq);
 wire BTrapListNZ = |ATrapList;
 assign BIsStop = BBreakAckNZ | (FDbgExecEn & BTrapListNZ) | (FIsStop & ~BCpuStatRd);
 assign BIsStopNotConf = BBreakListNZ | BTrapListNZ;
 assign BIsStopCancel  = BBreakReqNZ & BCmdDecReady & ~BBreakAckNZ;

 assign BAttReq =
   (FAttReq & ~BCpuStatRd) |
   (BIsStop & ~FIsStop) |
   (BTtySendHasData & ~FTtySendHasData);
 assign BBreakList = BBreakAck | (BCpuStatRd ? CCoreNil : FBreakList);
 assign BTrapList = ATrapList | (BCpuStatRd ? CCoreNil : FTrapList);

 assign {ADbgExecEn, ADbgResetS, ADbgClkHEn} = {FDbgExecEn & ~BIsStopNotConf, FDbgResetS, FDbgClkHEn};
 assign ADbgStep = FDbgStep;

 assign AMemAccess = FMemAccess;
 assign AMemAddr = FMemAddr[31:3];
 assign AMemMosi = BMemDataWr ? ADbioMosi : 64'h0;
 assign AMemWrRdEn = {BMemDataWr, 1'b1};

 assign AAttReq = FAttReq;

 assign ADbgCoreIdx = BDbgCoreIdx;
 assign ADbgRegIdx  = BDbgRegIdx;

 assign {AStartupO, AStartupE} = {FStartupO, FStartupE};

 assign ATest = {FAttReq, FIsStop, BBreakListNZ, BTrapListNZ, BIsStopCancel, BBreakAckNZ, FDbgExecEn, |FBreakReq};
endmodule

module MsBrkCmpA ( input [31:0] ABrkIp, input [31:0] ABrkCmp, output AIsBreak );
 wire BBrkCmpNZ = |ABrkCmp;
 assign AIsBreak = BBrkCmpNZ & (ABrkCmp==ABrkIp);
endmodule

module MsBrkCmp #(parameter CBrkCnt=9) ( input [31:0] ABrkIp, input [CBrkCnt*32-1:0] ABrkCmp, output AIsBreak );
 wire [CBrkCnt-1:0] BBreakList; MsBrkCmpA UBreakList[CBrkCnt-1:0] ( .ABrkIp({CBrkCnt{ABrkIp}}), .ABrkCmp(ABrkCmp), .AIsBreak(BBreakList) );
 assign AIsBreak = |BBreakList;
endmodule

module MsTestLdr #(parameter CBaudLen=3, CBaudDiv=3'h7, CRomBase=32'h0000, CRomSize=32'h0000, CProgStart=24'h800000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] ADbioAddr, input [63:0] ADbioMosi, output [63:0] ADbioMiso, input [3:0] ADbioMosiIdx, ADbioMisoIdx, input ADbioMosi1st, ADbioMiso1st, input ADbioDataLenNZ, output ADbioIdxReset,
  output [7:0] ADbioSbData, output ADbioSbNow, // Data to be sent back immediately
  output AMemAccess, output [31:3] AMemAddr, input [63:0] AMemMiso, output [63:0] AMemMosi, output AMemWrEn,
  output ASpiMaster, input ASpiMiso, output ASpiMosi, output ASpiSck, output ASpiNCS,
  output AActive, output ASbActive,
  output [7:0] ATest
 );

 // Interface
 localparam CAddrCtrlWr = 8'h00;
 localparam CAddrStatRd = 8'h00;
 localparam CAddrProgTr = 8'h01;

 // FSM
 localparam CStLen        = 19;

 localparam IStForiMux    =  0;
 localparam IStForiStart  =  1;
 localparam IStForiCmdA   =  2;
 localparam IStForiAddrC  =  3;
 localparam IStForiAddrB  =  4;
 localparam IStForiAddrA  =  5;
 localparam IStForiDataI  =  6;
 localparam IStForiDataA  =  7;
 localparam IStForiDataB  =  8;
 localparam IStForiRomWr  =  9;
 localparam IStForiIncA   = 10;
 localparam IStForiEnd    = 11;
 localparam IStFlMux      = 12;
 localparam IStFlStart    = 13;
 localparam IStFlLoopA    = 14;
 localparam IStFlLoopB    = 15;
 localparam IStFlLastA    = 16;
 localparam IStFlEnd      = 17;
 localparam IStMuxDone    = 18;

 wire [CStLen-1:0] FState, BState;
 wire FStateNZ, BStateNZ;
 wire [2:0] FByteIdx, BByteIdx;
 wire [31:3] FMemAddr, BMemAddr;
 wire [63:0] FMemMosi, BMemMosi;
 wire FSpiMaster, BSpiMaster;
 wire FSpiCS, BSpiCS;
 wire FActive, BActive;
 wire FSbActive, BSbActive;

 MsDffList #(.CRegLen(CStLen+1+3+29+64+1+1+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BState, BStateNZ, BByteIdx, BMemAddr, BMemMosi, BSpiMaster, BSpiCS, BActive, BSbActive}),
   .ADataO({FState, FStateNZ, FByteIdx, FMemAddr, FMemMosi, FSpiMaster, FSpiCS, FActive, FSbActive})
  );

 // Common
 wire [1:0] BLdrCmd;
 wire [15:0] BLdrStat = {4'h0, FStateNZ, 3'h0, FMemAddr[31:24]};

 // Aux
 wire [7:0] BSpiMosi; wire [7:0] BSpiMiso; wire BSpiSend, BSpiBusy, BSpiRecv;
 wire BByteIdxNZ = |FByteIdx;
 wire BMemAddrEnd = FMemAddr>=CRomSize[31:3];

 // DBIO
 wire BDbioCtrlWr = (ADbioAddr==CAddrCtrlWr) & ADbioMosiIdx[1]; assign BLdrCmd = BDbioCtrlWr ? ADbioMosi[1:0] : 2'h0;
 wire BDbioStatRd = (ADbioAddr==CAddrStatRd) & ADbioMiso1st;
 wire BDbioProgWr = (ADbioAddr==CAddrProgTr) & ADbioMosiIdx[0];

 assign ADbioIdxReset = |{BDbioCtrlWr, BDbioStatRd, BDbioProgWr};

 assign ADbioMiso =
  (BDbioStatRd ? {48'h0, BLdrStat} : 64'h0);

 assign ADbioSbData = ADbioSbNow ? BSpiMiso : 8'h0;

 assign ADbioSbNow = (|{FState[IStFlEnd:IStFlLoopA]}) & BSpiRecv;

 // FSM
 assign BStateNZ = |FState;
 wire BDbioProgStart = (ADbioAddr==CAddrProgTr) & ADbioMosi1st;

 wire [CStLen-1:0] BGo;                                              wire [CStLen-1:0] BStay;
 // Part 1. Read flash, program ROM (Read command = 0x03)
 assign BGo[IStForiMux]   = ~BStateNZ & (BLdrCmd==2'h1);             assign BStay[IStForiMux]   = 1'b0;
 assign BGo[IStForiStart] =  FState[IStForiMux];                     assign BStay[IStForiStart] = 1'b0;
 // Send command
 assign BGo[IStForiCmdA]  =  FState[IStForiStart];                   assign BStay[IStForiCmdA]  = FState[IStForiCmdA] & BSpiBusy;
 // Send 3 bytes of address
 assign BGo[IStForiAddrC] =  FState[IStForiCmdA]  & ~BSpiBusy;       assign BStay[IStForiAddrC] = FState[IStForiAddrC] & BSpiBusy;
 assign BGo[IStForiAddrB] =  FState[IStForiAddrC] & ~BSpiBusy;       assign BStay[IStForiAddrB] = FState[IStForiAddrB] & BSpiBusy;
 assign BGo[IStForiAddrA] =  FState[IStForiAddrB] & ~BSpiBusy;       assign BStay[IStForiAddrA] = FState[IStForiAddrA] & BSpiBusy;
 // Start reading data
 assign BGo[IStForiDataI] =  FState[IStForiAddrA] & ~BSpiBusy;       assign BStay[IStForiDataI] = 1'b0;
 // Read data byte
 assign BGo[IStForiDataA] =  FState[IStForiDataI] |
                            (FState[IStForiDataB] &  BByteIdxNZ) |
                            (FState[IStForiIncA]  & ~BMemAddrEnd);   assign BStay[IStForiDataA] = FState[IStForiDataA] & BSpiBusy;
 assign BGo[IStForiDataB] =  FState[IStForiDataA] & ~BSpiBusy;       assign BStay[IStForiDataB] = 1'b0;
 // Write data to ROM
 assign BGo[IStForiRomWr] =  FState[IStForiDataB] & ~BByteIdxNZ;     assign BStay[IStForiRomWr] = 1'b0;
 // Inc address
 assign BGo[IStForiIncA]  =  FState[IStForiRomWr];                   assign BStay[IStForiIncA]  = 1'b0;
 // End of Fori (= "Flash Out Rom In")
 assign BGo[IStForiEnd]   =  FState[IStForiIncA]  &  BMemAddrEnd;    assign BStay[IStForiEnd]   = 1'b0;
 // Part 2. Send/Recv SPI commands
 assign BGo[IStFlMux]     = ~BStateNZ & BDbioProgStart;              assign BStay[IStFlMux]     = 1'b0;
 assign BGo[IStFlStart]   =  FState[IStFlMux];                       assign BStay[IStFlStart]   = 1'b0;
 // Send/Recv Loop
 assign BGo[IStFlLoopA]   = (FState[IStFlStart] & ADbioDataLenNZ) |
                            (FState[IStFlLoopB] & ADbioDataLenNZ);   assign BStay[IStFlLoopA]   = FState[IStFlLoopA] & (ADbioAddr==CAddrProgTr) & ~BDbioProgWr;

 assign BGo[IStFlLoopB]   =  FState[IStFlLoopA] & BDbioProgWr;       assign BStay[IStFlLoopB]   = 1'b0;
 // Last byte of FL
 assign BGo[IStFlLastA]   =  FState[IStFlLoopB] & ~ADbioDataLenNZ;   assign BStay[IStFlLastA]   = FState[IStFlLastA] & ~BSpiRecv;
 // End of FL
 assign BGo[IStFlEnd]     = (FState[IStFlStart] & ~ADbioDataLenNZ) |
                            (FState[IStFlLastA] & BSpiRecv);         assign BStay[IStFlEnd]     = 1'b0;
 // Common (FlashMux)
 assign BGo[IStMuxDone]   = |{FState[IStForiEnd], FState[IStFlEnd]}; assign BStay[IStMuxDone]   = 1'b0;

 // State
 assign BState = BGo | BStay;
 assign BActive = |BState;
 assign BSbActive = |BState[IStFlEnd:IStFlStart];

 // Submodules
 SpiFsmMS #(.CBaudLen(CBaudLen), .CBaudRate(CBaudDiv)) USpiFsm
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AFsmMosi(BSpiMosi), .AFsmMiso(BSpiMiso), .AFsmSend(BSpiSend), .AFsmBusy(BSpiBusy), .AFsmRecv(BSpiRecv),
   .ASpiMiso(ASpiMiso), .ASpiMosi(ASpiMosi), .ASpiSck(ASpiSck)
  );

 assign BSpiSend = |{BGo[IStForiCmdA], BGo[IStForiAddrA:IStForiAddrC], BGo[IStForiDataA], BGo[IStFlLoopB]};

 wire [23:0] LProgStart = CProgStart;
 assign BSpiMosi =
  (BGo[IStForiAddrC] ? LProgStart[23:16] : 8'h0) |
  (BGo[IStForiAddrB] ? LProgStart[15: 8] : 8'h0) |
  (BGo[IStForiAddrA] ? LProgStart[ 7: 0] : 8'h0) |
  (BGo[IStForiCmdA]  ? 8'h03 : 8'h0) |
  (BGo[IStForiDataA] ? 8'hFF : 8'h0) |
  (BGo[IStFlLoopB]   ? ADbioMosi[7:0] : 8'h0);

 // Functional
 assign BByteIdx = FByteIdx + {2'h0, BGo[IStForiDataB]};
 assign BMemAddr = FState[IStForiStart] ? 29'h0 : FMemAddr + {28'h0, BGo[IStForiIncA]};
 assign BMemMosi = FState[IStForiEnd] ? 64'h0 : (FState[IStForiDataB] ? {BSpiMiso, FMemMosi[63:8]} : FMemMosi);

 // SPI
 assign BSpiMaster = |{BState[IStForiMux], BState[IStFlMux], BSpiCS, BState[IStMuxDone]};
 assign BSpiCS =
  |{
    BState[IStForiEnd:IStForiCmdA],
    BState[IStFlEnd:IStFlStart]
   };

 // External
 assign AMemAddr = FMemAddr+CRomBase[31:3];
 assign AMemMosi = FMemMosi;
 assign AMemWrEn = FState[IStForiRomWr];
 assign AMemAccess = FStateNZ;

 assign ASpiMaster = FSpiMaster;
 assign ASpiNCS = ~FSpiCS;

 assign AActive = FActive;
 assign ASbActive = FSbActive;

 assign ATest =
  {
   BSpiSend,
   BSpiRecv,
   ADbioDataLenNZ,
   FState[IStForiDataA],
   FState[IStForiDataB],
   FState[IStForiRomWr],
   FState[IStForiIncA],
   FState[IStForiEnd]
  };
endmodule


