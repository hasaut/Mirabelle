// UART version of MsDebug (almost completely copy-paste of FTDI version)

module MsDebugU #(parameter CAddrBase=16'h0000, CMcuType=8'h08, CRegCnt=8, CCoreCnt=2, CBrdVers=8'h5, CHwVers=32'h0, CBrkCnt=8, CMemCodeBase=32'h0000, CMemCodeSize=32'h0000, CProgBootOffs=32'h300000, CUartLedTailLen=4)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] AProgBaudRate,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  // CPU
  output wire ADbgExecEn, output wire ADbgResetS, output wire ADbgClkHEn, output wire ADbgStep,
  output wire [CCoreCnt-1:0] ADbgCoreIdx, output wire [CRegCnt-1:0] ADbgRegIdx, input wire [63:0] ADbgRegMiso,
  input wire [CCoreCnt-1:0] ATEndList, ATrapList, input wire [CCoreCnt*32-1:0] AIpThis, input wire [CCoreCnt-1:0] ACmdDecReady,
  // ADC
  input wire AAdcAttReq, output wire AAdcAttAck, input wire [15:0] AAdcDataLen,
  input wire [63:0] AAdcMiso, output wire [63:0] AAdcMosi, output wire AAdcWrEn, AAdcRdEn,
  input wire AClkStopAdc,
  // Mem
  output wire AMemAccess, output wire [31:3] AMemAddr, input wire [63:0] AMemMiso, output wire [63:0] AMemMosi, output wire [1:0] AMemWrRdEn,
  // Dbg bridge
  input wire ADbgRx, output wire ADbgTx, output wire ADbgTxFlush, output wire ADbgTxLed, ADbgRxLed,
  // Sync
  input wire ASync1M, input wire ASync1K,
  // Flash
  input wire ALoadFW,
  output wire AFlashMaster, input wire AFlashMiso, output wire AFlashMosi, output wire AFlashSck, output wire AFlashNCS,
  // RSU
  input wire ARsuReady, input wire [31:0] ARsuBootAddr,
  // Fpga reflash
  input wire [3:0] AStartupI, output wire [3:0] AStartupO, output wire [3:0] AStartupE,
  // Test
  output wire [15:0] ATest
 );

 wire [11:0] BDbioAddrU, BDbioAddrF;
 wire [63:0] BDbioMosiU, BDbioMosiF;
 wire [63:0] BDbioMiso;
 wire [3:0] BDbioMosiIdxU, BDbioMosiIdxF;
 wire [3:0] BDbioMisoIdxU, BDbioMisoIdxF;
 wire BDbioMosi1stU, BDbioMosi1stF;
 wire BDbioMiso1stU, BDbioMiso1stF;
 wire [15:0] BDbioDataLenU;
 wire BDbioDataLenNZU;
 wire BDbioIdxReset;
 wire BDbgAttReq;

 wire BLdrActive;
 wire BLdrSbActive; // SendBack active (used to block UartFlush during SendBack where pauses may happen)

 wire [7:0] BLdrSbData; wire BLdrSbNow; // Data to be sent back immediately
 wire [15:0] BTestUart;
 wire [7:0] BTestFsm;
 wire [39:0] BLogTimer;

 TestBridgeUartA #(.CUartTailLen(CUartLedTailLen)) UTestBridge
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADbgRx(ADbgRx), .ADbgTx(ADbgTx), .ADbgTxFlush(ADbgTxFlush), .ADbgTxLed(ADbgTxLed), .ADbgRxLed(ADbgRxLed),
   .ASync1M(ASync1M), .ASync1K(ASync1K), .ALogTimer(BLogTimer),
   .ADbioAddr(BDbioAddrU), .ADbioMosi(BDbioMosiU), .ADbioMiso(BDbioMiso), .ADbioMosiIdx(BDbioMosiIdxU), .ADbioMisoIdx(BDbioMisoIdxU), .ADbioMosi1st(BDbioMosi1stU), .ADbioMiso1st(BDbioMiso1stU), .ADbioDataLen(BDbioDataLenU), .ADbioDataLenNZ(BDbioDataLenNZU), .ADbioIdxReset(BDbioIdxReset),
   .ADbioSbData(BLdrSbData), .ADbioSbNow(BLdrSbNow), .ADbioSbActive(BLdrSbActive),
   .ADbgAttReq(BDbgAttReq), .AAdcAttReq(AAdcAttReq), .AAdcAttAck(AAdcAttAck), .AAdcDataLen(AAdcDataLen),
   .ATest(BTestUart)
  );
 wire BTestFsmReady;
 MsTestFsm UTestFsm
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARsuReady(ARsuReady),
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
 wire [15:0] BDbioDataLen = BTestFsmReady ? BDbioDataLenU : 16'h0;
 wire BDbioDataLenNZ = BTestFsmReady ? BDbioDataLenNZU : 1'b0;

 // Functional group access
 wire BDbioTestUCS = (BDbioAddr[11:8]==4'h0);
 wire BDbioTestLCS = (BDbioAddr[11:8]==4'h1);
 wire BDbioTestACS = (BDbioAddr[11:8]==4'h7);

 wire BDbioIdxResetU, BDbioIdxResetL, BDbioIdxResetA; assign BDbioIdxReset = |{BDbioIdxResetU, BDbioIdxResetL, BDbioIdxResetA};
 wire [63:0] BDbioMisoU, BDbioMisoL, BDbioMisoA;  assign BDbioMiso = BDbioMisoU | BDbioMisoL | BDbioMisoA;

 wire [31:3] BMemAddrU, BMemAddrL;
 wire [63:0] BMemMosiU, BMemMosiL;
 wire [1:0] BMemWrRdEnU; wire BMemWrEnL;
 wire BMemAccessU, BMemAccessL;

 wire [7:0] BTestBU;
 MsTestBU #(.CAddrBase(CAddrBase), .CBrkCnt(CBrkCnt), .CMcuType(CMcuType), .CRegCnt(CRegCnt), .CCoreCnt(CCoreCnt), .CBrdVers(CBrdVers), .CHwVers(CHwVers)) UTestU
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASync1M(ASync1M), .AClkStopAdc(AClkStopAdc),
   .AIoAddr(AIoAddr), .AIoMiso(AIoMiso), .AIoMosi(AIoMosi), .AIoWrSize(AIoWrSize),  .AIoRdSize(AIoRdSize), .AIoAddrAck(AIoAddrAck), .AIoAddrErr(AIoAddrErr),
   .ADbioAddr(BDbioAddr[7:0]), .ADbioMosi(BDbioMosi), .ADbioMiso(BDbioMisoU), .ADbioMosiIdx(BDbioTestUCS ? BDbioMosiIdx : 4'h0), .ADbioMisoIdx(BDbioTestUCS ? BDbioMisoIdx : 4'h0), .ADbioMosi1st(BDbioTestUCS & BDbioMosi1st), .ADbioMiso1st(BDbioTestUCS & BDbioMiso1st), .ADbioDataLenNZ(BDbioTestUCS & BDbioDataLenNZ), .ADbioIdxReset(BDbioIdxResetU),
   .ADbgExecEn(ADbgExecEn), .ADbgResetS(ADbgResetS), .ADbgClkHEn(ADbgClkHEn), .ADbgStep(ADbgStep),
   .AMemAccess(BMemAccessU), .AMemAddr(BMemAddrU), .AMemMiso(AMemMiso), .AMemMosi(BMemMosiU), .AMemWrRdEn(BMemWrRdEnU),
   .AIpThis(AIpThis), .ACmdDecReady(ACmdDecReady),
   .ADbgCoreIdx(ADbgCoreIdx), .ADbgRegIdx(ADbgRegIdx), .ADbgRegMiso(ADbgRegMiso),
   .ATEndList(ATEndList), .ATrapList(ATrapList), .AAttReq(BDbgAttReq),
   .AStartupI(AStartupI), .AStartupO(AStartupO), .AStartupE(AStartupE),
   .ALogTimer(BLogTimer),
   .ATest(BTestBU)
  );

 wire [7:0] BTestLdr;
 MsTestLdr #(.CMemCodeBase(CMemCodeBase), .CMemCodeSize(CMemCodeSize)) UTestLdr
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AProgBaudRate(AProgBaudRate),
   .ADbioAddr(BDbioAddr[7:0]), .ADbioMosi(BDbioMosi), .ADbioMiso(BDbioMisoL), .ADbioMosiIdx(BDbioTestLCS ? BDbioMosiIdx : 4'h0), .ADbioMisoIdx(BDbioTestLCS ? BDbioMisoIdx : 4'h0), .ADbioMosi1st(BDbioTestLCS & BDbioMosi1st), .ADbioMiso1st(BDbioTestLCS & BDbioMiso1st), .ADbioDataLen(BDbioTestLCS ? BDbioDataLen : 16'h0), .ADbioDataLenNZ(BDbioTestLCS & BDbioDataLenNZ), .ADbioIdxReset(BDbioIdxResetL),
   .ADbioSbData(BLdrSbData), .ADbioSbNow(BLdrSbNow),
   .AMemAccess(BMemAccessL), .AMemAddr(BMemAddrL), .AMemMiso(AMemMiso), .AMemMosi(BMemMosiL), .AMemWrEn(BMemWrEnL),
   .ASpiMaster(AFlashMaster), .ASpiMiso(AFlashMiso), .ASpiMosi(AFlashMosi), .ASpiSck(AFlashSck), .ASpiNCS(AFlashNCS),
   .ALdrAddr(ARsuBootAddr+CProgBootOffs),
   .AActive(BLdrActive), .ASbActive(BLdrSbActive),
   .ATest(BTestLdr)
  );

 wire [7:0] BTestAdc;
 MsTestAdc UTestAdc
  (
   .ADbioAddr(BDbioAddr[7:0]), .ADbioMosi(BDbioMosi), .ADbioMiso(BDbioMisoA), .ADbioMosiIdx(BDbioTestACS ? BDbioMosiIdx : 4'h0), .ADbioMisoIdx(BDbioTestACS ? BDbioMisoIdx : 4'h0), .ADbioMosi1st(BDbioTestACS & BDbioMosi1st), .ADbioMiso1st(BDbioTestACS & BDbioMiso1st), .ADbioDataLenNZ(BDbioTestACS & BDbioDataLenNZ), .ADbioIdxReset(BDbioIdxResetA),
   .AAdcMiso(AAdcMiso), .AAdcMosi(AAdcMosi), .AAdcWrEn(AAdcWrEn), .AAdcRdEn(AAdcRdEn),
   .ATest(BTestAdc)
  );

 assign {AMemAddr, AMemMosi, AMemWrRdEn} =
   (BMemAccessU ? {BMemAddrU, BMemMosiU, BMemWrRdEnU    } : {29'h0, 64'h0, 2'h0}) |
   (BMemAccessL ? {BMemAddrL, BMemMosiL, BMemWrEnL, 1'b0} : {29'h0, 64'h0, 2'h0});
 assign AMemAccess = BMemAccessU | BMemAccessL;

 //assign ATest = {BTestUart[7:0], AClkH, ADbgTxFlush, ADbgRx, ADbgTx, BTestAdc[3:0]}; //{AAdcAttReq, AAdcAttAck, AAdcWrEn, AAdcRdEn, ADbgTx, ADbgRx, AAdcDataLen[1:0]};
 assign ATest = 
  {
   ALoadFW, 2'h0, AFlashMaster, AFlashMiso, AFlashMosi, AFlashSck, AFlashNCS,
   BTestLdr
  };

endmodule



