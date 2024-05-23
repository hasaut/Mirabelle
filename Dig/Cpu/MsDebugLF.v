// FTDI version of MsDebug (almost completely copy-paste of UART version)

module MsDebugF #(parameter CAddrBase=16'h0000, CMcuType=8'h08, CRegCnt=8, CCoreCnt=2, CBrdVers=8'h5, CBrkCnt=8, CProgBaudLen=3, CProgBaudDiv=3'h7, CRomBase=32'h0000, CRomSize=32'h0000, CProgBootOffs=32'h300000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  // CPU
  output ADbgExecEn, output ADbgResetS, output ADbgClkHEn, output ADbgStep,
  output [CCoreCnt-1:0] ADbgCoreIdx, output [CRegCnt-1:0] ADbgRegIdx, input [63:0] ADbgRegMiso,
  input [CCoreCnt-1:0] ATEndList, ATrapList, input [CCoreCnt*32-1:0] AIpThis, input [CCoreCnt-1:0] ACmdDecReady,
  // ADC
  input AAdcAttReq, output AAdcAttAck, input [15:0] AAdcDataLen,
  input [63:0] AAdcMiso, output [63:0] AAdcMosi, output AAdcWrEn, AAdcRdEn,
  // Mem
  output AMemAccess, output [31:3] AMemAddr, input [63:0] AMemMiso, output [63:0] AMemMosi, output [1:0] AMemWrRdEn,
  // Dbg bridge
  input [7:0] ADbgDataI, output [7:0] ADbgDataO, output ADbgDataOE, input ADbgRF, ADbgTE, output ADbgWr, ADbgRd, ADbgSiwu, output [1:0] ADbgLed,
  // Sync
  input ASync1M, input ASync1K,
  // Flash
  input ALoadFW,
  output AFlashMaster, input AFlashMiso, output AFlashMosi, output AFlashSck, output AFlashNCS,
  // RSU
  input ARsuReady, input [31:0] ARsuBootAddr,
  // Fpga reflash
  input [3:0] AStartupI, output [3:0] AStartupO, output [3:0] AStartupE,
  // Test
  output [15:0] ATest
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

 TestBridgeFtdi UTestBridge
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADbgDataI(ADbgDataI), .ADbgDataO(ADbgDataO), .ADbgDataOE(ADbgDataOE), .ADbgRF(ADbgRF), .ADbgTE(ADbgTE), .ADbgWr(ADbgWr), .ADbgRd(ADbgRd), .ADbgSiwu(ADbgSiwu), .ADbgLed(ADbgLed),
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
 MsTestBU #(.CAddrBase(CAddrBase), .CBrkCnt(CBrkCnt), .CMcuType(CMcuType), .CRegCnt(CRegCnt), .CCoreCnt(CCoreCnt), .CBrdVers(CBrdVers)) UTestU
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASync1M(ASync1M),
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
 MsTestLdr #(.CBaudLen(CProgBaudLen), .CBaudDiv(CProgBaudDiv), .CRomBase(CRomBase), .CRomSize(CRomSize)) UTestLdr
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
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

 assign ATest = {BTestBU, AClkH, 1'b0, ADbgWr, ADbgRd, BTestAdc[3:0]}; //{AAdcAttReq, AAdcAttAck, AAdcWrEn, AAdcRdEn, ADbgTx, ADbgRx, AAdcDataLen[1:0]};

endmodule



