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

 wire [CCoreCnt*3-1:0] BSysReq; wire [CCoreCnt-1:0] BSysAck; wire [CCoreCnt-1:0] BSiLock;
 wire [CCoreCnt-1:0] BSysCoreSel;
 wire [31:3] BContPtrMiso; wire [CCoreCnt-1:0] BIsIsr; wire BContPtrWrEn;
 wire [CCoreCnt-1:0] BSetIrqSwtBase;
 wire [CCoreCnt-1:0] BCoreEn;
 wire [63:0] BRegMosi, BRegMiso; wire [7:0] BRegWrIdx, BRegRdIdx; // Common
 wire [31:3] BCtrlRomAddr; wire BCtrlRomRdEn;
 wire [31:0] BCtrlRamAddr; wire [63:0] BCtrlRamMosi; wire [3:0] BCtrlRamWrSize, BCtrlRamRdSize; wire BCtrlRamAck;

 wire [CCoreCnt-1:0] BIrqEn;
 wire [CIrqCnt-1:0] BIrqBusyList, BIrqToProcess; // Vector of IRQs

 MsCpuCtrl #(.CCoreCnt(CCoreCnt), .CStartAddr(CRomBase), .CVersion(8'h8), .CIrqCnt(CIrqCnt)) UCpuCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn(AExecEn), .ADbgStep(ADbgStep),
   .ASysReq(BSysReq), .ASysAck(BSysAck), .ASiLock(BSiLock),
   .ASysCoreSel(BSysCoreSel), .AContPtrMiso(BContPtrMiso), .AIsIsr(|BIsIsr), .AContPtrWrEn(BContPtrWrEn),
   .AIrqBusyList(BIrqBusyList), .AIrqToProcess(BIrqToProcess),
   .ASetIrqSwtBase(BSetIrqSwtBase),
   .AIrqEn(BIrqEn), .AIrq(AIrq),
   .ACoreEn(BCoreEn),
   .ARegMosi(BRegMosi), .ARegMiso(BRegMiso), .ARegWrIdx(BRegWrIdx), .ARegRdIdx(BRegRdIdx),
   .ARomAddr(BCtrlRomAddr), .ARomMiso(ARomMiso), .ARomRdEn(BCtrlRomRdEn),
   .ARamAddr(BCtrlRamAddr), .ARamMiso(ARamMiso), .ARamMosi(BCtrlRamMosi), .ARamWrSize(BCtrlRamWrSize), .ARamRdSize(BCtrlRamRdSize), .ARamAck(BCtrlRamAck),
   .ATest()
  );

 wire [CCoreCnt-1:0] BUnityReq, BUnityAck;
 MsUnityCtrl #(.CLineCnt(CCoreCnt)) UUnityCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AUnityReq(BUnityReq), .AUnityAck(BUnityAck)
  );

 localparam CDataCnt = 1+CCoreCnt;

 wire [CCoreCnt*32-1:0] BCodeAddr;
 wire [63:0] BCodeMiso;
 wire [CCoreCnt-1:0] BCodeReq, BCodeAck;
 wire [CDataCnt*32-1:0] BDataAddr;
 wire [CDataCnt*64-1:0] BDataMosi;
 wire [63:0] BDataMiso;
 wire [CDataCnt*4-1:0] BDataWrSize, BDataRdSize;
 wire [CDataCnt-1:0] BDataAck;
 wire [CCoreCnt*16-1:0] BPortAddr;
 wire [CCoreCnt*64-1:0] BPortMosi;
 wire [63:0] BPortMiso;
 wire [CCoreCnt*4-1:0] BPortWrSize, BPortRdSize;
 wire [CCoreCnt-1:0] BPortAck;

 wire [31:3] BRomAddr; wire BRomRdEn;

 wire [CCoreCnt*64-1:0] BRegMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(64)) URegMiso ( .ADataI(BRegMisoA), .ADataO(BRegMiso) );

 MsMemCtrl #(.CCoreCnt(CCoreCnt), .CDataCnt(CDataCnt), .CRomBase(CRomBase), .CRomSize(CRomSize)) UMemCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACodeAddr(BCodeAddr), .ACodeMiso(BCodeMiso), .ACodeReq(BCodeReq), .ACodeAck(BCodeAck),
   .ADataAddr(BDataAddr), .ADataMiso(BDataMiso), .ADataMosi(BDataMosi), .ADataWrSize(BDataWrSize), .ADataRdSize(BDataRdSize), .ADataAck(BDataAck),
   .APortAddr(BPortAddr), .APortMiso(BPortMiso), .APortMosi(BPortMosi), .APortWrSize(BPortWrSize), .APortRdSize(BPortRdSize), .APortAck(BPortAck),
   .ARomAddr(BRomAddr), .ARomMiso(ARomMiso), .ARomRdEn(BRomRdEn),
   .ARamAddr(ARamAddr), .ARamMiso(ARamMiso), .ARamMosi(ARamMosi), .ARamWrEn(ARamWrEn), .ARamRdEn(ARamRdEn),
   .AIoAddr(AIoAddr), .AIoMosi(AIoMosi), .AIoMiso(AIoMiso), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize), .AIoBusy(AIoBusy)
  );

 assign ARomAddr = BRomAddr | BCtrlRomAddr;
 assign ARomRdEn = BRomRdEn | BCtrlRomRdEn;

 wire [CCoreCnt*32-1:CCoreCnt*3] BContPtrMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(29)) UContPtrMiso ( .ADataI(BContPtrMisoA), .ADataO(BContPtrMiso) );

 wire [CCoreCnt*32-1:0] BDataAddrA;
 wire [CCoreCnt*64-1:0] BDataMosiA;
 wire [CCoreCnt*4-1:0] BDataWrSizeA, BDataRdSizeA;
 wire [CCoreCnt-1:0] BDataAckA;

 wire [CCoreCnt*CIrqCnt-1:0] BIrqBusyListA;  MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(CIrqCnt)) UIrqBusyList ( .ADataI(BIrqBusyListA), .ADataO(BIrqBusyList) );

 MssdCpu #(.CIoSpace(32'h300), .CIrqCnt(CIrqCnt)) UCpu [CCoreCnt-1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn({CCoreCnt{AExecEn | ADbgStep}}), .ACoreEn(BCoreEn),
   .ACodeAddr(BCodeAddr), .ACodeMiso(BCodeMiso), .ACodeReq(BCodeReq), .ACodeAck(BCodeAck),
   .ADataAddr(BDataAddrA), .ADataMiso(BDataMiso), .ADataMosi(BDataMosiA), .ADataWrSize(BDataWrSizeA), .ADataRdSize(BDataRdSizeA), .ADataAck(BDataAckA),
   .APortAddr(BPortAddr), .APortMiso(BPortMiso), .APortMosi(BPortMosi), .APortWrSize(BPortWrSize), .APortRdSize(BPortRdSize), .APortAck(BPortAck),
   .ASysCoreSel(BSysCoreSel | ADbgCoreIdx), .AContPtrMiso(BContPtrMisoA), .AIsIsr(BIsIsr), .AContPtrWrEn(BContPtrWrEn),
   .ASetIrqSwtBase(BSetIrqSwtBase),
   .AIrqBusyList(BIrqBusyListA), .AIrqToProcess(BIrqToProcess),
   .ARegMosi(BRegMosi), .ARegMiso(BRegMisoA), .ARegRdIdx(BRegRdIdx | ADbgRegRdIdx), .ARegWrIdx(BRegWrIdx),
   .ATEnd(ATEnd), .ATrap(ATrap), .AIpThis(AIpThis), .ACmdDecReady(ACmdDecReady),
   .ASysReq(BSysReq), .ASysAck(BSysAck), .ASiLock(BSiLock),
   .AUnityReq(BUnityReq), .AUnityAck(BUnityAck),
   .AIrqEn(BIrqEn)
  );

 assign ATest = {|BUnityReq, |BUnityAck, &BUnityReq, &BUnityAck, BUnityReq, BUnityAck};

 assign BDataAddr = {BCtrlRamAddr, BDataAddrA};
 assign BDataMosi = {BCtrlRamMosi, BDataMosiA};
 assign BDataWrSize = {BCtrlRamWrSize, BDataWrSizeA};
 assign BDataRdSize = {BCtrlRamRdSize, BDataRdSizeA};
 assign {BCtrlRamAck, BDataAckA} = BDataAck;

 assign ARegMiso = BRegMiso;
endmodule


