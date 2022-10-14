module TryComp
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ADataI, output ADataO
 );

 localparam CCoreCnt = 2;
 localparam CIrqCnt = 8;

 // Mosi
 wire BExecEn, BDbgStep;
 wire [63:0] BRomMiso;
 wire [63:0] BRamMiso;
 wire [63:0] BIoMiso; wire BIoBusy;
 wire [CCoreCnt-1:0] BDbgCoreIdx; wire [7:0] BDbgRegRdIdx;
 wire [CIrqCnt-1:0] BIrq;
 // Miso
 wire [31:3] BRomAddr;  wire BRomRdEn;
 wire [31:3] BRamAddr; wire [63:0] BRamMosi; wire [7:0] BRamWrEn, BRamRdEn;
 wire [15:0] BIoAddr; wire [63:0] BIoMosi; wire [3:0] BIoWrSize, BIoRdSize;
 wire [63:0] BRegMiso;
 wire [CCoreCnt-1:0] BTEnd, BTrap; wire [CCoreCnt*32-1:0] BIpThis; wire [CCoreCnt-1:0] BCmdDecReady;

 MsTmpCompB #(.CLenI(29+1+29+64+8+8+16+64+4+4+64+CCoreCnt*2+CCoreCnt*32+CCoreCnt+1), .CLenO(1+1+1+64+64+64+1+CCoreCnt+8+CIrqCnt)) UTmpComp
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BRomAddr, BRomRdEn, BRamAddr, BRamMosi, BRamWrEn, BRamRdEn, BIoAddr, BIoMosi, BIoWrSize, BIoRdSize, BRegMiso, BTEnd, BTrap, BIpThis, BCmdDecReady, ADataI}),
   .ADataO({ADataO, BExecEn, BDbgStep, BRomMiso, BRamMiso, BIoMiso, BIoBusy, BDbgCoreIdx, BDbgRegRdIdx, BIrq})
  );

 MsProcTop #(.CCoreCnt(CCoreCnt), .CRomBase(32'h2000), .CRomSize(32'h1000), .CIrqCnt(CIrqCnt)) UProc
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn(BExecEn), .ADbgStep(BDbgStep),
   .ARomAddr(BRomAddr), .ARomMiso(BRomMiso), .ARomRdEn(BRomRdEn),
   .ARamAddr(BRamAddr), .ARamMosi(BRamMosi), .ARamMiso(BRamMiso), .ARamWrEn(BRamWrEn), .ARamRdEn(BRamRdEn),
   .AIoAddr(BIoAddr), .AIoMosi(BIoMosi), .AIoMiso(BIoMiso), .AIoWrSize(BIoWrSize), .AIoRdSize(BIoRdSize), .AIoBusy(BIoBusy),
   .ADbgCoreIdx(BDbgCoreIdx), .ADbgRegRdIdx(BDbgRegRdIdx), .ARegMiso(BRegMiso),
   .ATEnd(BTEnd), .ATrap(BTrap), .AIpThis(BIpThis), .ACmdDecReady(BCmdDecReady),
   .AIrq(BIrq),
   .ATest()
  );

endmodule

/*
module MsProcTop #(parameter CCoreCnt=2, CRomBase=32'h0000, CRomSize=32'h0000, CIrqCnt=8)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AExecEn, input ADbgStep,
  output [31:3] ARomAddr, input [63:0] ARomMiso, output ARomRdEn,
  output [31:3] ARamAddr, input [63:0] ARamMiso, output [63:0] ARamMosi, output [7:0] ARamWrEn, ARamRdEn,
  output [15:0] AIoAddr, input [63:0] AIoMiso, output [63:0] AIoMosi, output [3:0] AIoWrSize, AIoRdSize, input AIoBusy,

  input [CCoreCnt-1:0] ADbgCoreIdx, input [7:0] ADbgRegRdIdx, output [63:0] ARegMiso,
  output [CCoreCnt-1:0] ATEnd, ATrap, output [CCoreCnt*32-1:0] AIpThis, output [CCoreCnt-1:0] ACmdDecReady,
  input [CIrqCnt-1:0] AIrq,
  output [7:0] ATest
 );


*/

