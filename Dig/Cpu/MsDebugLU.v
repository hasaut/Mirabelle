// UART version of MsDebug (almost completely copy-paste of FTDI version)

module MsDebugU #(parameter CAddrBase=16'h0000, CMcuType=8'h08, CRegCnt=8, CCoreCnt=8'h2, CBrdVers=8'h5, CBrkCnt=8'h8, CProgBaudLen=3, CProgBaudDiv=3'h7, CRomBase=32'h0000, CRomSize=32'h0000, CProgBootOffs=32'h300000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  // CPU
  output ADbgExecEn, output ADbgResetS, output ADbgClkHEn, output ADbgStep,
  output [CCoreCnt-1:0] ADbgCoreIdx, output [CRegCnt-1:0] ADbgRegIdx, input [63:0] ADbgRegMiso,
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
  // RSU
  input ARsuReady, input [31:0] ARsuBootAddr,
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
 wire [15:0] BDbioDataLenU;
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
   .ADbioAddr(BDbioAddrU), .ADbioMosi(BDbioMosiU), .ADbioMiso(BDbioMiso), .ADbioMosiIdx(BDbioMosiIdxU), .ADbioMisoIdx(BDbioMisoIdxU), .ADbioMosi1st(BDbioMosi1stU), .ADbioMiso1st(BDbioMiso1stU), .ADbioDataLen(BDbioDataLenU), .ADbioDataLenNZ(BDbioDataLenNZU), .ADbioIdxReset(BDbioIdxReset),
   .ADbioSbData(BLdrSbData), .ADbioSbNow(BLdrSbNow), .ADbioSbActive(BLdrSbActive),
   .ADbgAttReq(BDbgAttReq),
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

 wire BDbioIdxResetU, BDbioIdxResetL; assign BDbioIdxReset = |{BDbioIdxResetU, BDbioIdxResetL};
 wire [63:0] BDbioMisoU, BDbioMisoL;  assign BDbioMiso = BDbioMisoU | BDbioMisoL;

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

 assign {AMemAddr, AMemMosi, AMemWrRdEn} =
   (BMemAccessU ? {BMemAddrU, BMemMosiU, BMemWrRdEnU    } : {29'h0, 64'h0, 2'h0}) |
   (BMemAccessL ? {BMemAddrL, BMemMosiL, BMemWrEnL, 1'b0} : {29'h0, 64'h0, 2'h0});
 assign AMemAccess = BMemAccessU | BMemAccessL;

 assign ATest = BTestLdr;
endmodule



