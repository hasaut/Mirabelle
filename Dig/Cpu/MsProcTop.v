module MsProcTop #(parameter CCoreCnt=2, CRomBase=32'h0000, CRomSize=32'h0000, CIoSpace=16'h0300, CIrqCnt=8)
 (
  input AClkH, AResetHN, AClkHEn,
  input AExecEn, ADbgStep,
  output [31:3] ARomAddr, input [63:0] ARomMiso, output ARomRdEn,
  output [31:3] ARamAddr, input [63:0] ARamMiso, output [63:0] ARamMosi, output [7:0] ARamWrEn, ARamRdEn,
  output [15:0] AIoAddr, input [63:0] AIoMiso, output [63:0] AIoMosi, output [3:0] AIoWrSize, AIoRdSize, input AIoBusy, input AIoSrq,
  input [CCoreCnt-1:0] ADbgCoreIdx, input [11:0] ADbgRegRdIdx, output [63:0] ADbgRegMiso,
  output [CCoreCnt-1:0] ATEnd, ATrap, output [CCoreCnt*32-1:0] AIpThis, output [CCoreCnt-1:0] ACmdDecReady,
  input [CIrqCnt-1:0] AIrq,
  output [7:0] ATest
 );

 wire [CCoreCnt*3-1:0] BSysReq; wire [CCoreCnt-1:0] BSysAck; wire [CCoreCnt-1:0] BSiLock;
 wire [CCoreCnt-1:0] BSysCoreSel;
 wire [31:3] BContPtrMiso; wire [CCoreCnt-1:0] BIsIsr; wire BContPtrWrEn;
 wire [CCoreCnt-1:0] BSetIrqSwtBase;
 wire [CCoreCnt-1:0] BCoreEn;
 wire [63:0] BRegMosi, BRegMiso; wire [11:0] BRegWrIdx, BRegRdIdx; // Common
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
   .AIrqEn(BIrqEn), .AIrq(AExecEn ? AIrq : {CIrqCnt{1'b0}}),
   .ACoreEn(BCoreEn),
   .ARegMosi(BRegMosi), .ARegMiso(BRegMiso), .ARegWrIdx(BRegWrIdx), .ARegRdIdx(BRegRdIdx),
   .ARomAddr(BCtrlRomAddr), .ARomMiso(ARomMiso), .ARomRdEn(BCtrlRomRdEn),
   .ARamAddr(BCtrlRamAddr), .ARamMiso(ARamMiso), .ARamMosi(BCtrlRamMosi), .ARamWrSize(BCtrlRamWrSize), .ARamRdSize(BCtrlRamRdSize), .ARamAck(BCtrlRamAck),
   .ATest(ATest)
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
 wire [CCoreCnt-1:0] BPortAck, BPortSrq;

 wire [31:3] BRomAddr; wire BRomRdEn;

 wire [CCoreCnt*64-1:0] BRegMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(64)) URegMiso ( .ADataI(BRegMisoA), .ADataO(BRegMiso) );

 MsMemCtrl #(.CCoreCnt(CCoreCnt), .CDataCnt(CDataCnt), .CRomBase(CRomBase), .CRomSize(CRomSize)) UMemCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACodeAddr(BCodeAddr), .ACodeMiso(BCodeMiso), .ACodeReq(BCodeReq), .ACodeAck(BCodeAck),
   .ADataAddr(BDataAddr), .ADataMiso(BDataMiso), .ADataMosi(BDataMosi), .ADataWrSize(BDataWrSize), .ADataRdSize(BDataRdSize), .ADataAck(BDataAck),
   .APortAddr(BPortAddr), .APortMiso(BPortMiso), .APortMosi(BPortMosi), .APortWrSize(BPortWrSize), .APortRdSize(BPortRdSize), .APortAck(BPortAck), .APortSrq(BPortSrq),
   .ARomAddr(BRomAddr), .ARomMiso(ARomMiso), .ARomRdEn(BRomRdEn),
   .ARamAddr(ARamAddr), .ARamMiso(ARamMiso), .ARamMosi(ARamMosi), .ARamWrEn(ARamWrEn), .ARamRdEn(ARamRdEn),
   .AIoAddr(AIoAddr), .AIoMosi(AIoMosi), .AIoMiso(AIoMiso), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize), .AIoBusy(AIoBusy), .AIoSrq(AIoSrq)
  );

 assign ARomAddr = BRomAddr | BCtrlRomAddr;
 assign ARomRdEn = BRomRdEn | BCtrlRomRdEn;

 wire [CCoreCnt*32-1:CCoreCnt*3] BContPtrMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(29)) UContPtrMiso ( .ADataI(BContPtrMisoA), .ADataO(BContPtrMiso) );

 wire [CCoreCnt*256-1:0] BMpuRegs;
 wire [CCoreCnt*32-1:0] BCodeAddrCpu;
 wire [CCoreCnt*32-1:0] BDataAddrCpu;
 wire [CCoreCnt*64-1:0] BDataMisoCpu, BDataMosiCpu;
 wire [CCoreCnt*4-1:0] BDataWrSizeCpu, BDataRdSizeCpu;
 wire [CCoreCnt-1:0] BDataAckCpu;

 wire [CCoreCnt*CIrqCnt-1:0] BIrqBusyListA;  MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(CIrqCnt)) UIrqBusyList ( .ADataI(BIrqBusyListA), .ADataO(BIrqBusyList) );

 MssdCpu #(.CIrqCnt(CIrqCnt)) UCpu [CCoreCnt-1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn({CCoreCnt{AExecEn | ADbgStep}}), .ACoreEn(BCoreEn),
   .AMpuRegs(BMpuRegs),
   .ACodeAddr(BCodeAddrCpu), .ACodeMiso(BCodeMiso), .ACodeReq(BCodeReq), .ACodeAck(BCodeAck),
   .ADataAddr(BDataAddrCpu), .ADataMiso(BDataMisoCpu), .ADataMosi(BDataMosiCpu), .ADataWrSize(BDataWrSizeCpu), .ADataRdSize(BDataRdSizeCpu), .ADataAck(BDataAckCpu),
   .ASysCoreSel(BSysCoreSel | ADbgCoreIdx), .AContPtrMiso(BContPtrMisoA), .AIsIsr(BIsIsr), .AContPtrWrEn(BContPtrWrEn),
   .ASetIrqSwtBase(BSetIrqSwtBase),
   .AIrqBusyList(BIrqBusyListA), .AIrqToProcess(BIrqToProcess), .ASrq(BPortSrq),
   .ARegMosi(BRegMosi), .ARegMiso(BRegMisoA), .ARegRdIdx(BRegRdIdx | ADbgRegRdIdx), .ARegWrIdx(BRegWrIdx),
   .ATEnd(ATEnd), .ATrap(ATrap), .AIpThis(AIpThis), .ACmdDecReady(ACmdDecReady),
   .ASysReq(BSysReq), .ASysAck(BSysAck), .ASiLock(BSiLock),
   .AUnityReq(BUnityReq), .AUnityAck(BUnityAck),
   .AIrqEn(BIrqEn)
  );

 wire [CCoreCnt*32-1:0] BDataAddrMpu;

 MssdMpuCtrl UMpuCtrl[CCoreCnt-1:0]
  (
   .AMpuRegs(BMpuRegs),
   .ACodeAddrCpu(BCodeAddrCpu), .ACodeReqAny(|BCodeReq), .ACodeAddrMpu(BCodeAddr), .ACodeAddrErr(),
   .ADataAddrCpu(BDataAddrCpu), .ADataReqAny(|{BDataWrSizeCpu, BDataRdSizeCpu}), .ADataAddrMpu(BDataAddrMpu), .ADataAddrErr()
  );

 wire [CCoreCnt*32-1:0] BDataAddrMpd;
 wire [CCoreCnt*64-1:0] BDataMosiMpd;
 wire [CCoreCnt*4-1:0] BDataWrSizeMpd, BDataRdSizeMpd;
 wire [CCoreCnt-1:0] BDataAckMpd;

 MssdMemPerDec #(.CIoSpace(CIoSpace)) UMemPerDec[CCoreCnt-1:0]
  (
   .AExtAddr(BDataAddrMpu), .AExtMiso(BDataMisoCpu), .AExtMosi(BDataMosiCpu), .AExtWrSize(BDataWrSizeCpu), .AExtRdSize(BDataRdSizeCpu), .AExtAck(BDataAckCpu),
   .AContPtrWrEn(BSysCoreSel & {CCoreCnt{BContPtrWrEn}}),
   .AMemAddr(BDataAddrMpd), .AMemMiso(BDataMiso), .AMemMosi(BDataMosiMpd), .AMemWrSize(BDataWrSizeMpd), .AMemRdSize(BDataRdSizeMpd), .AMemAck(BDataAckMpd),
   .APerAddr(BPortAddr), .APerMiso(BPortMiso), .APerMosi(BPortMosi), .APerWrSize(BPortWrSize), .APerRdSize(BPortRdSize), .APerAck(BPortAck)
  );

 //assign ATest = {|BUnityReq, |BUnityAck, &BUnityReq, &BUnityAck, BUnityReq, BUnityAck};

 assign BDataAddr = {BCtrlRamAddr, BDataAddrMpd};
 assign BDataMosi = {BCtrlRamMosi, BDataMosiMpd};
 assign BDataWrSize = {BCtrlRamWrSize, BDataWrSizeMpd};
 assign BDataRdSize = {BCtrlRamRdSize, BDataRdSizeMpd};
 assign {BCtrlRamAck, BDataAckMpd} = BDataAck;

 assign ADbgRegMiso = BRegMiso;
endmodule

module MssdMpuItem
 (
  input [63:0] AMpuRegs,
  input [31:0] AAddrCpu, output [31:0] AAddrAdd, output AInUse, AIsHit
 );

 wire LCmpGran = AMpuRegs[63];
 wire [19:0] LNewAddrA = AMpuRegs[59:40];
 wire [19:0] LCmpAddrE = AMpuRegs[39:20];
 wire [19:0] LCmpAddrS = AMpuRegs[19: 0];

 wire BInUse = |LCmpAddrE;
 wire [19:0] BAddrToCmp = LCmpGran ? AAddrCpu[31:12] : AAddrCpu[23:4];
 wire [31:0] BAddrToAdd = LCmpGran ? {LNewAddrA, 12'h0} : {8'h0, LNewAddrA, 4'h0};
 wire BIsHit = (BAddrToCmp>=LCmpAddrS) & (BAddrToCmp<LCmpAddrE);

 assign AAddrAdd = (BInUse & BIsHit) ? BAddrToAdd : 32'h0;
 
 assign AInUse = BInUse;
 assign AIsHit = BIsHit;
endmodule

module MssdMpuCtrl
 (
  input [4*64-1:0] AMpuRegs,
  input [31:0] ACodeAddrCpu, input ACodeReqAny, output [31:0] ACodeAddrMpu, output ACodeAddrErr,
  input [31:0] ADataAddrCpu, input ADataReqAny, output [31:0] ADataAddrMpu, output ADataAddrErr
 );

 wire BCodeInUse, BCodeIsHit; wire [31:0] BCodeAddrAdd;
 MssdMpuItem UMpuCode ( .AMpuRegs(AMpuRegs[63:0]), .AAddrCpu(ACodeAddrCpu), .AAddrAdd(BCodeAddrAdd), .AInUse(BCodeInUse), .AIsHit(BCodeIsHit) );
 assign ACodeAddrErr = ACodeReqAny & BCodeInUse & ~BCodeIsHit;

 assign ACodeAddrMpu = ACodeAddrCpu + BCodeAddrAdd;

 wire [4*32-1:0] BDataAddrAdd;
 wire [3:0] BDataInUse, BDataIsHit;
 MssdMpuItem UMpuData[3:0] ( .AMpuRegs(AMpuRegs), .AAddrCpu(ADataAddrCpu), .AAddrAdd(BDataAddrAdd), .AInUse(BDataInUse), .AIsHit(BDataIsHit) );
 wire BDataInUseNZ = |BDataInUse;
 wire BDataIsHitNZ = |BDataIsHit;
 assign ADataAddrErr = ADataReqAny & BDataInUseNZ & ~BDataIsHitNZ;

 wire [31:0] BDataAddrAddSel;
 MsMatrOrCol #(.CRowCnt(4), .CColCnt(32)) UDataAddrMpu ( .ADataI(BDataAddrAdd), .ADataO(BDataAddrAddSel) );
 assign ADataAddrMpu = ADataAddrCpu + BDataAddrAddSel;

endmodule

module MssdMemPerDec #(parameter CIoSpace=16'h0300)
 (
  input [31:0] AExtAddr, output [63:0] AExtMiso, input [63:0] AExtMosi, input [3:0] AExtWrSize, AExtRdSize, output AExtAck,
  input AContPtrWrEn,
  output [31:0] AMemAddr, input [63:0] AMemMiso, output [63:0] AMemMosi, output [3:0] AMemWrSize, AMemRdSize, input AMemAck,
  output [15:0] APerAddr, input [63:0] APerMiso, output [63:0] APerMosi, output [3:0] APerWrSize, APerRdSize, input APerAck
 );

 wire BIsIoAccess = AExtAddr<{16'h0, CIoSpace};

 assign {AMemAddr, AMemMosi, AMemWrSize, AMemRdSize} = BIsIoAccess ? {32'h0, 64'h0, 4'h0, 4'h0} : {AExtAddr, AExtMosi, AExtWrSize, AExtRdSize};
 assign {APerAddr, APerMosi, APerWrSize, APerRdSize} = BIsIoAccess ? {AExtAddr[15:0], AExtMosi, AExtWrSize, AExtRdSize} : {16'h0, 64'h0, 4'h0, 4'h0};
 assign AExtMiso = //BIsIoAccess ? APerMiso : AMemMiso;
   ((AMemAck | AContPtrWrEn) ? AMemMiso : 64'h0) |
   (APerAck ? APerMiso : 64'h0);
 // assign AExtAck  = BIsIoAccess ? APortAck : ADataAck; <- this line generates a combinatorial loop, because BIsIoAccess depends on a register which is being written
 assign AExtAck  = APerAck | AMemAck;  // <- It is safe to do like this, because access is controlled by the same module. Otherwise, it is necessary to implement 2 Mio units: one for data, another one for ports
endmodule


