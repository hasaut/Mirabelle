module MsProcTop #(parameter CCoreCnt=2, CMemCodeBase=32'h0000, CMemCodeSize=32'h0000, CIoSpace=16'h0300, CIrqCnt=8)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire AExecEn,
  output wire [31:3] AMemCodeAddr, input wire [63:0] AMemCodeMiso, output wire AMemCodeRdEn,
  output wire [31:3] AMemDataAddr, input wire [63:0] AMemDataMiso, output wire [63:0] AMemDataMosi, output wire [7:0] AMemDataWrEn, AMemDataRdEn,
  output wire [15:0] AIoSpaceAddr, input wire [63:0] AIoSpaceMiso, output wire [63:0] AIoSpaceMosi, output wire [3:0] AIoSpaceWrSize, AIoSpaceRdSize, input wire AIoSpaceBusy, input wire AIoSpaceSrq,
  input wire [CCoreCnt-1:0] ADbgCoreIdx, input wire [11:0] ADbgRegRdIdx, output wire [63:0] ADbgRegMiso,
  output wire [CCoreCnt-1:0] ATEnd, ATrap, output wire [CCoreCnt*32-1:0] AIpThis, output wire [CCoreCnt-1:0] ACmdDecReady,
  input wire [CIrqCnt-1:0] AIrq,
  output wire [CCoreCnt*3-1:0] AErrList,
  output wire [7:0] ATest
 );

 wire [CCoreCnt*3-1:0] BSysReq; wire [CCoreCnt-1:0] BSysAck; wire [CCoreCnt-1:0] BSiLock;
 wire [CCoreCnt-1:0] BSysCoreSel;
 wire [31:3] BContPtrMiso; wire [CCoreCnt-1:0] BIsIsr; wire BContPtrWrEn;
 wire [CCoreCnt-1:0] BSetIrqSwtBase;
 wire [CCoreCnt-1:0] BCoreEn;
 wire [63:0] BRegMosi, BRegMiso; wire [11:0] BRegWrIdx, BRegRdIdx; // Common
 wire [31:3] BCtrlCodeAddr; wire BCtrlCodeRdEn;
 wire [31:0] BCtrlDataAddr; wire [63:0] BCtrlDataMosi; wire [3:0] BCtrlDataWrSize, BCtrlDataRdSize; wire BCtrlDataAck;

 wire [CCoreCnt-1:0] BIrqEn;
 wire [CIrqCnt-1:0] BIrqBusyList, BIrqToProcess; // Vector of IRQs

 MsCpuCtrl #(.CCoreCnt(CCoreCnt), .CStartAddr(CMemCodeBase), .CVersion(8'h8), .CIrqCnt(CIrqCnt)) UCpuCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn(AExecEn),
   .ASysReq(BSysReq), .ASysAck(BSysAck), .ASiLock(BSiLock),
   .ASysCoreSel(BSysCoreSel), .AContPtrMiso(BContPtrMiso), .AIsIsr(|BIsIsr), .AContPtrWrEn(BContPtrWrEn),
   .AIrqBusyList(BIrqBusyList), .AIrqToProcess(BIrqToProcess),
   .ASetIrqSwtBase(BSetIrqSwtBase),
   .AIrqEn(BIrqEn), .AIrq(AExecEn ? AIrq : {CIrqCnt{1'b0}}),
   .ACoreEn(BCoreEn),
   .ARegMosi(BRegMosi), .ARegMiso(BRegMiso), .ARegWrIdx(BRegWrIdx), .ARegRdIdx(BRegRdIdx),
   .AMemCodeAddr(BCtrlCodeAddr), .AMemCodeMiso(AMemCodeMiso), .AMemCodeRdEn(BCtrlCodeRdEn),
   .AMemDataAddr(BCtrlDataAddr), .AMemDataMiso(AMemDataMiso), .AMemDataMosi(BCtrlDataMosi), .AMemDataWrSize(BCtrlDataWrSize), .AMemDataRdSize(BCtrlDataRdSize), .AMemDataAck(BCtrlDataAck),
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

 wire [31:3] BMemCodeAddr; wire BMemCodeRdEn;

 wire [CCoreCnt*64-1:0] BRegMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(64)) URegMiso ( .ADataI(BRegMisoA), .ADataO(BRegMiso) );

 MsMemCtrl #(.CCoreCnt(CCoreCnt), .CDataCnt(CDataCnt), .CMemCodeBase(CMemCodeBase), .CMemCodeSize(CMemCodeSize)) UMemCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACodeAddr(BCodeAddr), .ACodeMiso(BCodeMiso), .ACodeReq(BCodeReq), .ACodeAck(BCodeAck),
   .ADataAddr(BDataAddr), .ADataMiso(BDataMiso), .ADataMosi(BDataMosi), .ADataWrSize(BDataWrSize), .ADataRdSize(BDataRdSize), .ADataAck(BDataAck),
   .APortAddr(BPortAddr), .APortMiso(BPortMiso), .APortMosi(BPortMosi), .APortWrSize(BPortWrSize), .APortRdSize(BPortRdSize), .APortAck(BPortAck), .APortSrq(BPortSrq),
   .AMemCodeAddr(BMemCodeAddr), .AMemCodeMiso(AMemCodeMiso), .AMemCodeRdEn(BMemCodeRdEn),
   .AMemDataAddr(AMemDataAddr), .AMemDataMiso(AMemDataMiso), .AMemDataMosi(AMemDataMosi), .AMemDataWrEn(AMemDataWrEn), .AMemDataRdEn(AMemDataRdEn),
   .AIoSpaceAddr(AIoSpaceAddr), .AIoSpaceMosi(AIoSpaceMosi), .AIoSpaceMiso(AIoSpaceMiso), .AIoSpaceWrSize(AIoSpaceWrSize), .AIoSpaceRdSize(AIoSpaceRdSize), .AIoSpaceBusy(AIoSpaceBusy), .AIoSpaceSrq(AIoSpaceSrq)
  );

 assign AMemCodeAddr = BMemCodeAddr | BCtrlCodeAddr;
 assign AMemCodeRdEn = BMemCodeRdEn | BCtrlCodeRdEn;

 wire [CCoreCnt*32-1:CCoreCnt*3] BContPtrMisoA; MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(29)) UContPtrMiso ( .ADataI(BContPtrMisoA), .ADataO(BContPtrMiso) );

 wire [CCoreCnt*256-1:0] BMpuRegs;
 wire [CCoreCnt*32-1:0] BCodeAddrCpu;
 wire [CCoreCnt*32-1:0] BDataAddrCpu;
 wire [CCoreCnt*64-1:0] BDataMisoCpu, BDataMosiCpu;
 wire [CCoreCnt*4-1:0] BDataWrSizeCpu, BDataRdSizeCpu;
 wire [CCoreCnt-1:0] BDataAckCpu;

 wire [CCoreCnt*CIrqCnt-1:0] BIrqBusyListA;  MsMatrOrCol #(.CRowCnt(CCoreCnt), .CColCnt(CIrqCnt)) UIrqBusyList ( .ADataI(BIrqBusyListA), .ADataO(BIrqBusyList) );

 wire [CCoreCnt-1:0] BErrCpuDec;
 MssdCpu #(.CIrqCnt(CIrqCnt)) UCpu [CCoreCnt-1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn({CCoreCnt{AExecEn}}), .ACoreEn(BCoreEn),
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
   .AIrqEn(BIrqEn),
   .AErrDec(BErrCpuDec)
  );

 wire [CCoreCnt*32-1:0] BDataAddrMpu;
 wire [CCoreCnt-1:0] BErrMpuCode, BErrMpuData;
 MssdMpuCtrl UMpuCtrl[CCoreCnt-1:0]
  (
   .AMpuRegs(BMpuRegs),
   .ACodeAddrCpu(BCodeAddrCpu), .ACodeReqAny(|BCodeReq), .ACodeAddrMpu(BCodeAddr), .ACodeAddrErr(BErrMpuCode),
   .ADataAddrCpu(BDataAddrCpu), .ADataReqAny(|{BDataWrSizeCpu, BDataRdSizeCpu}), .ADataAddrMpu(BDataAddrMpu), .ADataAddrErr(BErrMpuData)
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

 assign BDataAddr = {BCtrlDataAddr, BDataAddrMpd};
 assign BDataMosi = {BCtrlDataMosi, BDataMosiMpd};
 assign BDataWrSize = {BCtrlDataWrSize, BDataWrSizeMpd};
 assign BDataRdSize = {BCtrlDataRdSize, BDataRdSizeMpd};
 assign {BCtrlDataAck, BDataAckMpd} = BDataAck;

 // Error list (transpose ErrMatrix)
 MsMatrTrans #(.CRowCnt(3), .CColCnt(CCoreCnt)) UErrList ( .ADataI({BErrMpuData, BErrMpuCode, BErrCpuDec}), .ADataO(AErrList) );

 
 assign ADbgRegMiso = BRegMiso;
endmodule

module MssdMpuItem
 (
  input wire [63:0] AMpuRegs,
  input wire [31:0] AAddrCpu, output wire [31:0] AAddrAdd, output wire AInUse, AIsHit
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
  input wire [4*64-1:0] AMpuRegs,
  input wire [31:0] ACodeAddrCpu, input wire ACodeReqAny, output wire [31:0] ACodeAddrMpu, output wire ACodeAddrErr,
  input wire [31:0] ADataAddrCpu, input wire ADataReqAny, output wire [31:0] ADataAddrMpu, output wire ADataAddrErr
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
  input wire [31:0] AExtAddr, output wire [63:0] AExtMiso, input wire [63:0] AExtMosi, input wire [3:0] AExtWrSize, AExtRdSize, output wire AExtAck,
  input wire AContPtrWrEn,
  output wire [31:0] AMemAddr, input wire [63:0] AMemMiso, output wire [63:0] AMemMosi, output wire [3:0] AMemWrSize, AMemRdSize, input wire AMemAck,
  output wire [15:0] APerAddr, input wire [63:0] APerMiso, output wire [63:0] APerMosi, output wire [3:0] APerWrSize, APerRdSize, input wire APerAck
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


