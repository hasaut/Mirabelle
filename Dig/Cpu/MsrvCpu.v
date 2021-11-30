module MsrvCpu #(parameter CIoSpace = 32'h300)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] ACpuInfoIdx, output [15:0] ACpuInfo,
  output [31:0] ACodeAddr, input [63:0] ACodeMiso, output ACodeReq, input ACodeAck,
  output [31:0] ADataAddr, input [31:0] ADataMiso, output [31:0] ADataMosi, output [2:0] ADataWrSize, ADataRdSize, input ADataAck,
  output [15:0] APortAddr, input [31:0] APortMiso, output [31:0] APortMosi, output [2:0] APortWrSize, APortRdSize, input APortAck,
  input [1:0] ASysCoreIdx, output [31:2] ACtrlMiso, output [31:3] ALabelMiso, input ALabelWrEn,
  input [2*4-1:0] APhase, // TESZ
  input [63:0] ARegMosi, output [63:0] ARegMiso, input [7:0] ARegRdIdx, ARegWrIdx,
  output [1:0] AIsStepPend,
  output [1:0] ATrap, output [31:0] AEipWrBus,
  // Sys Req/Ack {unlock, lock, end, Swt}
  output [2*3-1:0] ASysReq, input [1:0] ASysAck, output [1:0] ASiLock,
  output [1:0] AUnityReq, input [1:0] AUnityAck,
  output [1:0] AIsIrqEn
 );

 function IsColRowConflict ( input [13:0] AColRowA, AColRowB );
   IsColRowConflict = (|(AColRowA[13:8] & AColRowB[13:8])) & (|(AColRowA[7:0] & AColRowB[7:0]));
 endfunction

 // SysCtrl
 wire [31:2] FCtrlAddr, BCtrlAddr;
 wire [31:3] FLabelP, BLabelP;
 wire [31:3] FLabelQ, BLabelQ;
 // Exec state (multi-state commands)
 localparam CStateLen = 5;
 localparam CStateNil = {CStateLen{1'b0}};
 wire [CStateLen-1:0] FStateP, BStateP;
 wire [CStateLen-1:0] FStateQ, BStateQ;
 wire [2:0] FSysReqP, BSysReqP;
 wire [2:0] FSysReqQ, BSysReqQ;
 wire [1:0] FSiLock, BSiLock;
 wire FUnityReqP, BUnityReqP;
 wire FUnityReqQ, BUnityReqQ;

 MsDffList #(.CRegLen(30+2*29+2*CStateLen+2*3+2+2)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrlAddr, BLabelP, BLabelQ, BStateP, BStateQ, BSysReqP, BSysReqQ, BSiLock, BUnityReqP, BUnityReqQ}),
   .ADataO({FCtrlAddr, FLabelP, FLabelQ, FStateP, FStateQ, FSysReqP, FSysReqQ, FSiLock, FUnityReqP, FUnityReqQ})
  );

 // CmdDecoder
 //          Final         Decoded    | if Conflict then Final:=0 else Final:=Decoded;
 wire  [1:0] LCmdLen,      MCmdLen;

 // VLIW
 wire  [6:0] LCond,        MCond;
 wire        LMuxEip,      MMuxEip;
 wire  [1:0] LLoadEip,     MLoadEip;
 wire        LTrap,        MTrap;
 wire  [4:0] LSysReq,      MSysReq;
 wire [13:0] LSelColRowS,  MSelColRowS;
 wire [13:0] LSelColRowU,  MSelColRowU;
 wire        LSignExt,     MSignExt;
 wire [31:0] LConst,       MConst;
 wire  [1:0] LLoopD,       MLoopD;
 wire [13:0] LDstColRow,   MDstColRow;
 wire  [3:0] LAluSelA,     MAluSelA;
 wire  [7:0] LAluSelU,     MAluSelU;
 wire  [2:0] LAluSelS,     MAluSelS;
 wire  [6:0] LAluSelF,     MAluSelF;
 wire  [1:0] LMioWrRdEn,   MMioWrRdEn;
 wire  [2:0] LMioSize,     MMioSize;
 wire        LUnityReq,    MUnityReq;

 wire  [6:0] FCond;
 wire        FMuxEip;
 wire  [1:0] FLoadEip;
 wire [23:1] FEipConst;
 wire [13:0] FDstColRow;

 MsPhaseDelT #(.CDataLen(7+1+2+23+14)) UPhaseDelT
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({LCond, LMuxEip, LLoadEip, LConst[23:1], LDstColRow}),
   .ADataO({FCond, FMuxEip, FLoadEip, FEipConst,    FDstColRow})
  );

 // Bus A, B
 wire [31:0] BBusAData; wire [13:0] BBusALoad; wire [3:0] BBusFData;
 wire [31:0] BBusBData; wire [13:0] BBusBLoad, BBusBPend; wire [1:0] BDevBPendAny;

 // Registers
 wire [1:0] BSrcCpuIdx = {|{APhase[7], APhase[5:4]}, |{APhase[3], APhase[1:0]}};
 wire [511:0] BRegsNextQ, BRegsNextP;
 wire [511:0] BRegsNext = (BSrcCpuIdx[1] ? BRegsNextQ : 512'h0) |
                          (BSrcCpuIdx[0] ? BRegsNextP : 512'h0);
 wire [511:0] BRegsThisQ, BRegsThisP;
 wire [511:0] BRegsThis = (BSrcCpuIdx[1] ? BRegsThisQ : 512'h0) |
                          (BSrcCpuIdx[0] ? BRegsThisP : 512'h0);
 wire [23:1] BEipThis = BRegsThis[23:1];

 // Buses
 wire BPhaseZNZ = APhase[4] | APhase[0];
 wire [7:0] BRegWrIdx = BPhaseZNZ ? ARegWrIdx : 8'h0;
 wire [7:0] BRegRdIdx = BPhaseZNZ ? ARegRdIdx : 8'h0;
 wire BRegWrIdxNZ = |BRegWrIdx;
 wire [13:0] BRegWrIdxA = BBusALoad | {3'h0, {3{BRegWrIdxNZ}}, BRegWrIdx} | (FMuxEip ? FDstColRow : 14'h0);
 wire [13:0] BRegWrIdxB = BBusBLoad | {{3{BRegWrIdxNZ}}, 3'h0, BRegWrIdx};
 wire [31:0] BRegWrBusA = (FMuxEip ? {8'h0, BEipThis, 1'b0} : BBusAData) | (BRegWrIdxNZ ? ARegMosi[31: 0] : 32'h0);
 wire [31:0] BRegWrBusB = BBusBData                                      | (BRegWrIdxNZ ? ARegMosi[63:32] : 32'h0);

 wire [23:1] BEipWrBus;

 // Decoder
 wire [1:0] BCpuIdxM = {APhase[5], APhase[1]}; // Mux phase
 wire [1:0] BCpuIdxE = {APhase[6], APhase[2]}; // Exe phase
 // State
 wire [CStateLen-1:0] BStepThis =
  (BCpuIdxM[1] ? FStateQ : CStateNil) |
  (BCpuIdxM[0] ? FStateP : CStateNil);
 wire [CStateLen-1:0] BStepNext;
 assign BStateQ = BCpuIdxM[1] ? BStepNext : FStateQ;
 assign BStateP = BCpuIdxM[0] ? BStepNext : FStateP;
 wire [1:0] BStepThisNZ_inv = {|FStateP, |FStateQ}; // Used to detect memory conflict (if CoreP is in Step state, then CoreQ cannot access Memory)
 // SysReq
 wire [2:0] BSysReqThis =
  (BCpuIdxM[1] ? FSysReqQ : 3'h0) |
  (BCpuIdxM[0] ? FSysReqP : 3'h0);
 wire [2:0] BSysReqNext;
 assign BSysReqQ = BCpuIdxM[1] ? BSysReqNext : (ASysAck[1] ? 3'h0 : FSysReqQ);
 assign BSysReqP = BCpuIdxM[0] ? BSysReqNext : (ASysAck[0] ? 3'h0 : FSysReqP);
 // SiLock
 assign BSiLock = (FSiLock | (LSysReq[2] ? BCpuIdxM : 2'h0)) & (LSysReq[3] ? ~BCpuIdxM : 2'h3);
 // UnityReq
 assign BUnityReqQ = BCpuIdxM[1] ? LUnityReq : FUnityReqQ;
 assign BUnityReqP = BCpuIdxM[0] ? LUnityReq : FUnityReqP;

 // EIP
 // Eip is updated in Tail phase
 wire BStepNextNZ = |BStepNext;
 wire BEipUpdateA = &{BRegWrIdxA[10:8]==3'h7, BRegWrIdxA[0]};
 wire BEipUpdateB = &{BRegWrIdxB[10:8]==3'h7, BRegWrIdxB[0]};
 wire BJmpEn; MsrvCondAnalyzer UJmpEn ( .ACond(FCond), .AFlags(BBusFData), .AJmpEn(BJmpEn) );
 wire BEipJmpUpdate = FLoadEip[1] & BJmpEn;
 wire [23:1] BEipNext = BEipThis + {21'h0, BStepNextNZ ? 2'h0 : LCmdLen};
 wire BEipUpdateAny = |{BEipUpdateB, BEipUpdateA, BEipJmpUpdate, FLoadEip[0]};
 wire BEipLoadImpass = BEipUpdateAny;
 assign BEipWrBus =
  (BEipUpdateB ? BRegWrBusB[23:1] : 23'h0) |  // Tail
  (BEipUpdateA ? BRegWrBusA[23:1] : 23'h0) |  // Tail
  (BEipJmpUpdate ? FEipConst[23:1] : 23'h0) |    // Tail
  (FLoadEip[0] ? BBusAData[23:1] : 23'h0) |
  (BEipUpdateAny ? 23'h0 : BEipNext);         // Mux
 assign BSysReqNext = BSysReqThis | {LSysReq[2:0]}; // Lock End Swt

 // 2x CmdQueue
 wire [23:1] BQueLdaQ, BQueLdaP; // Load address
 wire [127:0] BQueTopQ, BQueTopP;
 MsCmdQue UCmdQue [1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AQueLda({BQueLdaQ, BQueLdaP}), .AQueTop({BQueTopQ, BQueTopP}),
   .APhase(APhase),
   .ACodeMiso(ACodeMiso), .ACodeAck(ACodeAck),
   .AEipUpdate(BEipUpdateAny), .AEipWrBus(BEipWrBus)
  );

 // CmdQue mux
 wire [23:1] BQueLda = BQueLdaQ | BQueLdaP;
 wire [127:0] BQueTopD = BQueTopQ | BQueTopP;
 wire [3:0] BQueLenA = BQueLda[4:1]-BEipThis[4:1];
 wire [1:0] BQueLen = (|BQueLenA[3:2]) ? 2'h3 : BQueLenA[1:0];
 wire BQueLenNZ = |BQueLen;

 assign ACodeReq = (APhase[5] | APhase[1]) & (BEipUpdateAny | ((BQueLda[4:3]-BEipNext[4:3])<2'h2));
 assign ACodeAddr = BEipUpdateAny ? {8'h0, BEipWrBus[23:3], 3'h0} : {8'h0, BQueLda[23:3], 3'h0};
 wire [79:0] BQueTopC = BEipThis[3] ? {BQueTopD[15:0], BQueTopD[127:64]} : BQueTopD[79:0];
 wire [47:0] BQueTopB = BEipThis[2] ? BQueTopC[79:32] : BQueTopC[47:0];
 wire [31:0] BQueTopA = BEipThis[1] ? BQueTopB[47:16] : BQueTopB[31:0];
 wire [31:0] BQueTop = BQueTopA;

 // Decoder
 wire BMemPendAny;

 MsrvCmdDec UCmdDec
  (
   .AQueTop(BQueTop), .AIpThis(BEipThis),
   // CmdLen
   .ACmdLen(MCmdLen),
   // Vliw
   .ACond(MCond), .AMuxEip(MMuxEip), .ALoadEip(MLoadEip),
   .ATrap(MTrap), .ASysReq(MSysReq),
   .ASelColRowS(MSelColRowS), .ASelColRowU(MSelColRowU), .ASignExt(MSignExt),
   .AConst(MConst), .ALoopD(MLoopD), .ADstColRow(MDstColRow),
   .AAluSelA(MAluSelA), .AAluSelU(MAluSelU), .AAluSelS(MAluSelS), .AAluSelF(MAluSelF),
   .AMioWrRdEn(MMioWrRdEn), .AMioSize(MMioSize),
   // Multi-Step
   .AStepThis(BStepThis), .AStepNext(BStepNext),
   .AUnityReq(MUnityReq), .AUnityAck(|(BCpuIdxM & AUnityAck)),
   .AMemPend(BMemPendAny)
  );

 wire BPendAnyA = |(BCpuIdxM & BDevBPendAny);
 wire [13:0] BPendMask = BPendAnyA ? BBusBPend : 14'h0;
 wire [2:0] BConflictColRow =
  {
   IsColRowConflict(BPendMask, MSelColRowS),
   IsColRowConflict(BPendMask, MSelColRowU),
   IsColRowConflict(BPendMask, MDstColRow)
  };
 wire BRReqDevB = |{MAluSelU, MAluSelF[6], MMioWrRdEn}; // RReq = "Resource request"
 wire BPendDevB = |BDevBPendAny;
 wire BStepConflict = (|(BStepThisNZ_inv & BCpuIdxM)) & (|MMioWrRdEn);
 wire BConflictAny = |{BConflictColRow, BRReqDevB & BPendDevB, BEipLoadImpass, BPendAnyA & (|MSysReq), BSysReqThis, BStepConflict};
 wire BCmdLenValid = (BQueLen>=MCmdLen) & BQueLenNZ;
 wire BCmdValid = BCmdLenValid & ~BConflictAny & (|BCpuIdxM);

 assign
  {
   LCmdLen,
   LCond, LMuxEip, LLoadEip, LTrap, LSysReq,
   LSelColRowS, LSelColRowU, LSignExt, LConst, LLoopD, LDstColRow,
   LAluSelA, LAluSelU, LAluSelS, LAluSelF,
   LMioWrRdEn, LMioSize,
   LUnityReq
  } = BCmdValid ?
  {
   MCmdLen,
   MCond, MMuxEip, MLoadEip, MTrap, MSysReq,
   MSelColRowS, MSelColRowU, MSignExt, MConst, MLoopD, MDstColRow,
   MAluSelA, MAluSelU, MAluSelS, MAluSelF,
   MMioWrRdEn, MMioSize,
   MUnityReq
  } : 123'h0;

 // Sys
 assign BCtrlAddr = LSysReq[4] ? BRegsThis[95:66] : FCtrlAddr;
 assign BLabelQ = (ASysCoreIdx[1] & ALabelWrEn) ? ADataMiso[31:3] : FLabelQ;
 assign BLabelP = (ASysCoreIdx[0] & ALabelWrEn) ? ADataMiso[31:3] : FLabelP;

 // Registers
 MsrvRegTask URegTask [1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .APhase(APhase),
   .ARegWrBusA(BRegWrBusA), .ARegWrIdxA(BRegWrIdxA),
   .ARegWrBusB(BRegWrBusB), .ARegWrIdxB(BRegWrIdxB),
   .AEipWrBus(BEipWrBus),
   .ARegsNext({BRegsNextQ, BRegsNextP}), .ARegsThis({BRegsThisQ, BRegsThisP})
  );

 // RegIdxX takes in account ARegRdIdx
 wire BRegRdIdxNZ = |BRegRdIdx;
 wire [13:0] BRegRdIdxS = {{3{BRegRdIdxNZ}}, 3'h0, BRegRdIdx} | (LMioWrRdEn[0] ? 14'h0 : LSelColRowS);
 wire [13:0] BRegRdIdxU = {3'h0, {3{BRegRdIdxNZ}}, BRegRdIdx} | LSelColRowU;

 // Reg muxes
 wire [31:0] BRegMuxS;
 MsrvRegMux URegMuxS ( .ARegFile({BRegsNext[511:32], 8'h0, BEipNext, 1'b0}), .ARegRdIdx(BRegRdIdxS), .AMux(BRegMuxS) );

 wire [31:0] BRegMuxU;
 MsrvRegMux URegMuxU ( .ARegFile({BRegsNext[511:32], 8'h0, BEipThis, 1'b0}), .ARegRdIdx(BRegRdIdxU), .AMux(BRegMuxU) );

 wire BSelS_Reg = |LSelColRowS[13:8];
 wire [31:0] BMuxS_LConst = BSelS_Reg ? BRegMuxS : LConst;
 wire [31:0] BAluAMuxS  = BMuxS_LConst;
 wire [31:0] BAluAMuxU  = BRegMuxU | {24'h0, LLoopD[0] ? BBusBData[7:0] : 8'h0};
 wire [31:0] BMioAddrS  = LConst;
 wire [31:0] BMioWrData = BRegMuxS | {24'h0, LLoopD[1] ? BBusAData[7:0] : 8'h0};

 // Alu A+S+T
 MsrvAluFast UAluFast
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BAluAMuxS), .AMuxU(BAluAMuxU),
   .AAluSelA(LAluSelA), .AAluSelS(LAluSelS),
   .ADstColRow(LDstColRow),
   .AAluData(BBusAData), .AAluLoad(BBusALoad),
   .AFlagsData(BBusFData)
  );

 // Alu U+F
 wire [31:0] BAluBDataA; wire [13:0] BAluBLoad, BAluBPend; wire [1:0] BAluBPendAny;
 MsrvAluSlow UAluSlow
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BMuxS_LConst), .AMuxU(BRegMuxU), .ACpuIdxM(BCpuIdxM), .ACpuIdxE(BCpuIdxE), // 20945
   .AAluSelU(LAluSelU), .AAluSelF(LAluSelF),
   .ADstColRow(LDstColRow),
   .ABusBData(BAluBDataA), .ABusBLoad(BAluBLoad), .ABusBPend(BAluBPend), .APendAny(BAluBPendAny)
  );

 // Mio
 wire [31:0] BMioAddr = BMioAddrS+BRegMuxU;
 wire BIsIoAccess = BMioAddr<CIoSpace;
 wire [1:0] BMemWrRdEn = BIsIoAccess ? 2'h0 : LMioWrRdEn;
 wire [1:0] BIoWrRdEn  = BIsIoAccess ? LMioWrRdEn : 2'h0;
 wire [1:0] BMioSignExtA = {2{LSignExt}} &
  (
   ((LMioSize==3'h2) ? 2'h3 : 2'h0) |
   ((LMioSize==2'h1) ? 2'h1 : 2'h0)
  );

 // Mem
 wire [31:0] BMemBDataA; wire [13:0] BMemBLoad, BMemBPend; wire [1:0] BMemBPendAny; wire [1:0] BMemBSignExtL, BMemBSignExtP;
 MsMioCtrl #(.CAddrLen(32), .CRegIdxLen(16)) UMemCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExtAddr(ADataAddr), .AExtMosi(ADataMosi), .AExtMiso(ADataMiso), .AExtWrSize(ADataWrSize), .AExtRdSize(ADataRdSize), .AExtReady(ADataAck),
   .AWrRdEn(BMemWrRdEn), .ARegIdx({BMioSignExtA, LSelColRowS}), .AArgSize(LMioSize),
   .AAddr(BMioAddr), .AData(BMioWrData), .ACpuIdxM(BCpuIdxM), .ACpuIdxE(BCpuIdxE),
   .ABusBData(BMemBDataA), .ABusBLoad({BMemBSignExtL, BMemBLoad}), .ABusBPend({BMemBSignExtP, BMemBPend}), .APendAny(BMemBPendAny)
  );
 assign BMemPendAny = |BMemBPendAny;

 // Io
 wire [31:0] BIoBDataA; wire [13:0] BIoBLoad, BIoBPend; wire [1:0] BIoBPendAny; wire [1:0] BIoBSignExtL, BIoBSignExtP;
 MsMioCtrl #(.CAddrLen(16), .CRegIdxLen(16)) UPortCtrl
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExtAddr(APortAddr), .AExtMosi(APortMosi), .AExtMiso(APortMiso), .AExtWrSize(APortWrSize), .AExtRdSize(APortRdSize), .AExtReady(APortAck),
   .AWrRdEn(BIoWrRdEn), .ARegIdx({BMioSignExtA, LSelColRowS}), .AArgSize(LMioSize),
   .AAddr(BMioAddr[15:0]), .AData(BMioWrData), .ACpuIdxM(BCpuIdxM), .ACpuIdxE(BCpuIdxE),
   .ABusBData(BIoBDataA), .ABusBLoad({BIoBSignExtL, BIoBLoad}), .ABusBPend({BIoBSignExtP, BIoBPend}), .APendAny(BIoBPendAny)
  );

 wire [31:0] BMioDataA = BMemBDataA | BIoBDataA;
 wire [31:0] BMioDataB;
 wire [13:0] BMioLoad = BMemBLoad | BIoBLoad;
 wire [2:0] BMioSignExt = BMemBSignExtL | BIoBSignExtL;
 MsrvSignExt UMioDataB ( .AData(BMioDataA), .ACol(BMioSignExt), .ASignExt(|BMioSignExt), .AResult(BMioDataB) );

 assign BBusBLoad = BAluBLoad | BMioLoad;
 assign BBusBPend = BAluBPend | BMemBPend | BIoBPend;
 assign BDevBPendAny = BAluBPendAny | BMemBPendAny | BIoBPendAny;
 assign BBusBData = BAluBDataA | BMioDataB;

 // Ext
 assign ACpuInfo =
  (ACpuInfoIdx[1] ? 16'h2 : 16'h0) |
  (ACpuInfoIdx[0] ? 16'h2 : 16'h0);

 assign ACtrlMiso = (|ASysCoreIdx) ? FCtrlAddr : 30'h0;
 assign ALabelMiso = (ASysCoreIdx[1] ? FLabelQ : 29'h0) |
                     (ASysCoreIdx[0] ? FLabelP : 29'h0);
 assign ARegMiso = BRegRdIdxNZ ? {BRegMuxS, BRegMuxU} : 64'h0;
 assign AEipWrBus = {8'h0, BEipWrBus, 1'b0};
 assign AIsStepPend = {|FStateQ, |FStateP} | BDevBPendAny;
 assign ATrap = BCpuIdxM & {2{LTrap}};
 assign ASysReq = {FSysReqQ, FSysReqP};
 assign ASiLock = FSiLock;
 assign AUnityReq = {FUnityReqQ, FUnityReqP};
 assign AIsIrqEn = 2'h0;
endmodule

// *********************
// *** #Flow Section ***
// *********************

module MsrvCondAnalyzer ( input [6:0] ACond, input [3:0] AFlags, output AJmpEn );
 localparam IFlagV = 3;
 localparam IFlagN = 2;
 localparam IFlagZ = 1;
 localparam IFlagC = 0;

 wire BSmaller = AFlags[IFlagN] ^ AFlags[IFlagV];

/*
imm[12|10:5] rs2 rs1 000 imm[4:1|11] 1100 011 BEQ
imm[12|10:5] rs2 rs1 001 imm[4:1|11] 1100 011 BNE
imm[12|10:5] rs2 rs1 100 imm[4:1|11] 1100 011 BLT
imm[12|10:5] rs2 rs1 101 imm[4:1|11] 1100 011 BGE
imm[12|10:5] rs2 rs1 110 imm[4:1|11] 1100 011 BLTU
imm[12|10:5] rs2 rs1 111 imm[4:1|11] 1100 011 BGEU
*/

 assign AJmpEn = // sjmp jbe jc jnc jz jnz ja jsy
  (ACond[0] ?  1'b1                             : 1'b0) | // sjmp
  (ACond[1] ?  AFlags[IFlagZ]                   : 1'b0) | // jz
  (ACond[2] ? ~AFlags[IFlagZ]                   : 1'b0) | // jnz
  (ACond[3] ?                    BSmaller       : 1'b0) | // js
  (ACond[4] ?  AFlags[IFlagZ] | ~BSmaller       : 1'b0) | // jge
  (ACond[5] ?                    AFlags[IFlagC] : 1'b0) | // jc
  (ACond[6] ?  AFlags[IFlagZ] | ~AFlags[IFlagC] : 1'b0);  // jae
endmodule

// *********************
// *** #Regs Section ***
// *********************

module MsrvRegWhl
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AWrPhase,
  input [31:0] ARegWrBusA, input [5:0] AColWrEnA, input ARowWrEnA,
  input [31:0] ARegWrBusB, input [5:0] AColWrEnB, input ARowWrEnB,
  output [63:0] ADataNext, output [63:0] ADataThis
 );

 wire [5:0] BRegWrEnA = ARowWrEnA ? AColWrEnA : 6'h0;
 wire [5:0] BRegWrEnB = ARowWrEnB ? AColWrEnB : 6'h0;

 MsrvRegX URegX [1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(AWrPhase),
   .ARegWrBusA(ARegWrBusA), .ARegWrEnA(BRegWrEnA),
   .ARegWrBusB(ARegWrBusB), .ARegWrEnB(BRegWrEnB),
   .ADataNext(ADataNext), .ADataThis(ADataThis)
  );

endmodule

module MsrvRegX
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AWrPhase,
  input [31:0] ARegWrBusA, input [2:0] ARegWrEnA,
  input [31:0] ARegWrBusB, input [2:0] ARegWrEnB,
  output [31:0] ADataNext, output [31:0] ADataThis
 );

 MsRegX #(.CDataLen(16)) URegW
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(AWrPhase),
   .ARegWrBusA(ARegWrBusA[31:16]), .ARegWrEnA(ARegWrEnA[2]),
   .ARegWrBusB(ARegWrBusB[31:16]), .ARegWrEnB(ARegWrEnB[2]),
   .ADataNext(ADataNext[31:16]), .ADataThis(ADataThis[31:16])
  );

 MsRegX #(.CDataLen(8)) URegH
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(AWrPhase),
   .ARegWrBusA(ARegWrBusA[15:8]), .ARegWrEnA(ARegWrEnA[1]),
   .ARegWrBusB(ARegWrBusB[15:8]), .ARegWrEnB(ARegWrEnB[1]),
   .ADataNext(ADataNext[15:8]), .ADataThis(ADataThis[15:8])
  );

 MsRegX #(.CDataLen(8)) URegL
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(AWrPhase),
   .ARegWrBusA(ARegWrBusA[7:0]), .ARegWrEnA(ARegWrEnA[0]),
   .ARegWrBusB(ARegWrBusB[7:0]), .ARegWrEnB(ARegWrEnB[0]),
   .ADataNext(ADataNext[7:0]), .ADataThis(ADataThis[7:0])
  );

endmodule

module MsrvRegTask
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [3:0] APhase,
  input [31:0] ARegWrBusA, input [13:0] ARegWrIdxA,
  input [31:0] ARegWrBusB, input [13:0] ARegWrIdxB,
  input [23:1] AEipWrBus,
  output [511:0] ARegsNext, output [511:0] ARegsThis
 );

 MsrvRegWhl URegsRwx[7:1]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(APhase[0] | APhase[3]),
   .ARegWrBusA(ARegWrBusA), .AColWrEnA(ARegWrIdxA[13:8]), .ARowWrEnA(ARegWrIdxA[7:1]),
   .ARegWrBusB(ARegWrBusB), .AColWrEnB(ARegWrIdxB[13:8]), .ARowWrEnB(ARegWrIdxB[7:1]),
   .ADataNext(ARegsNext[511:64]), .ADataThis(ARegsThis[511:64])
  );

 MsrvRegX URegSP
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrPhase(APhase[0] | APhase[3]),
   .ARegWrBusA(ARegWrBusA), .ARegWrEnA(ARegWrIdxA[13:11] & {3{ARegWrIdxA[0]}}),
   .ARegWrBusB(ARegWrBusB), .ARegWrEnB(ARegWrIdxB[13:11] & {3{ARegWrIdxB[0]}}),
   .ADataNext(ARegsNext[63:32]), .ADataThis(ARegsThis[63:32])
  );

 wire [23:1] FEip, BEip; assign BEip = (APhase[0] | APhase[1] | APhase[3]) ? AEipWrBus : FEip;
 MsDffList #(.CRegLen(23)) URegEip
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BEip}),
   .ADataO({FEip})
  );

 assign ARegsNext[31:0] = {8'h0, BEip, 1'b0};
 assign ARegsThis[31:0] = {8'h0, FEip, 1'b0};
endmodule

// ********************
// *** #Sel Section ***
// ********************

module MsrvSignExt ( input [31:0] AData, input [2:0] ACol, input ASignExt, output [31:0] AResult );
 assign AResult = AData |
   ((ACol==3'h3) ? {{16{ASignExt & AData[15]}}, 16'h0} : 32'h0) |
   ((ACol==3'h1) ? {{24{ASignExt & AData[ 7]}},  8'h0} : 32'h0);
endmodule

module MsrvSelRegLine ( input [63:0] ARegLine, input [5:0] ACol, input ARow, output [31:0] AResult );
 wire [5:0] BSel = ARow ? ACol : 4'h0;
 assign AResult =
  {BSel[5] ? ARegLine[63:48] : 16'h0, BSel[4] ? ARegLine[47:40] : 8'h0, BSel[3] ? ARegLine[39:32] : 8'h0} |
  {BSel[2] ? ARegLine[31:16] : 16'h0, BSel[1] ? ARegLine[15: 8] : 8'h0, BSel[0] ? ARegLine[ 7: 0] : 8'h0};
endmodule

module MsrvSelRegColRow ( input [511:0] ARegFile, input [13:0] ARegRdIdx, output [31:0] AResult );
 wire [255:0] BMux;
 MsrvSelRegLine USelRegLine[7:0] ( .ARegLine(ARegFile), .ACol(ARegRdIdx[13:8]), .ARow(ARegRdIdx[ 7:0]), .AResult(BMux) );
 MsMatrOrCol #(.CRowCnt(8), .CColCnt(32)) UResultMux ( .ADataI(BMux), .ADataO(AResult) );
endmodule

module MsrvRegMux ( input [511:0] ARegFile, input [13:0] ARegRdIdx, output [31:0] AMux );
 wire [31:0] BMuxA; MsrvSelRegColRow UMuxA ( .ARegFile(ARegFile), .ARegRdIdx(ARegRdIdx), .AResult(BMuxA) );
 assign AMux = BMuxA;
endmodule

module MsrvColToSizeA ( input [2:0] ACol, output [2:0] ASize );
 assign ASize =
  ((ACol==3'h7) ? 3'h4 : 3'h0) |
  ((ACol==3'h3) ? 3'h2 : 3'h0) |
  ((ACol==3'h1) ? 3'h1 : 3'h0);
endmodule

module MsrvColToSize ( input [5:0] ACol, output [2:0] ASize );
 MsrvColToSizeA UColToSize( .ACol(ACol[5:3] | ACol[2:0]), .ASize(ASize) );
endmodule


// ************************
// *** #AluFast Section ***
// ************************

module MsrvAluFast
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [31:0] AMuxS, input [31:0] AMuxU,
  input [3:0] AAluSelA, // xor and sub add | 0110 = sltu, 1110 = slt
  input [2:0] AAluSelS, // asr shr shl
  input [13:0] ADstColRow,
  output [31:0] AAluData, output [13:0] AAluLoad,
  output [3:0] AFlagsData
 );

 wire FAnyOp, BAnyOp;
 wire [31:0] FMuxS, BMuxS;
 wire [31:0] FMuxU, BMuxU;
 wire [3:0] FAluSelA, BAluSelA;
 wire [2:0] FAluSelS, BAluSelS;
 wire [13:0] FDstColRow, BDstColRow;
 wire [31:0] FBusDData, BBusDData;
 wire [13:0] FBusDLoad, BBusDLoad;
 wire [3:0] FBusFData, BBusFData;

 MsDffList #(.CRegLen(1+32+32+4+3+14+32+14+4)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BAnyOp, BMuxS, BMuxU, BAluSelA, BAluSelS, BDstColRow, BBusDData, BBusDLoad, BBusFData}),
   .ADataO({FAnyOp, FMuxS, FMuxU, FAluSelA, FAluSelS, FDstColRow, FBusDData, FBusDLoad, FBusFData})
  );

 assign BAnyOp = |{AAluSelA, AAluSelS};
 assign BMuxS = BAnyOp ? AMuxS : 32'h0;
 assign BMuxU = BAnyOp ? AMuxU : 32'h0;
 assign BAluSelA = AAluSelA;
 assign BAluSelS = AAluSelS;
 assign BDstColRow = ADstColRow;

 // AluA
 wire [31:0] BBusADataA; wire [13:0] BBusALoadA;
 wire  [3:0] BBusFDataA; wire BBusFLoadA;
 MsrvAluA UAluA
  (
   .ADataS(FMuxS), .ADataD(FMuxU), .AOper(FAluSelA), .ADstColRow(FDstColRow),
   .ABusAData(BBusADataA), .ABusALoad(BBusALoadA),
   .ABusFData(BBusFDataA)
  );

 // AluS
 wire [31:0] BBusADataS; wire [13:0] BBusALoadS;
 MsrvAluS UAluS
  (
   .ADataS(FMuxS), .ADataD(FMuxU), .AOper(FAluSelS), .ADstColRow(FDstColRow),
   .ABusAData(BBusADataS), .ABusALoad(BBusALoadS)
  );

 assign BBusDData = BBusADataA | BBusADataS;
 assign BBusDLoad = BBusALoadA | BBusALoadS;
 assign BBusFData = BBusFDataA;

 assign AAluData = FBusDData; assign AAluLoad = FBusDLoad;
 assign AFlagsData = FBusFData;
endmodule

// ## AluA

module MsrvAluA
 (
  input [31:0] ADataS, input [31:0] ADataD, input [3:0] AOper, input [13:0] ADstColRow,
  output [31:0] ABusAData, output [13:0] ABusALoad,
  output  [3:0] ABusFData
 );

 // For Cam AOper is one-hot {xor, and, sub, add}
 // For Risc-V, if bit "sub" is set, then it is possible that bit "and" signifies to take "C" flag for the result (SLTXX commands)
 // In this case bit "xor" indicates if bit "C" in the result has to be inverted

 wire [31:0] BResXor = ADataS ^ ADataD;
 wire [31:0] BResAnd = ADataS & ADataD;
 wire [31:0] BDataSA = {32{AOper[1]}} ^ ADataS;
 wire [32:0] BResAdd = {1'b0, BDataSA} + {1'b0, ADataD} + {31'h0, AOper[1]};

 wire BResDataNZ = |ABusAData;
 wire BSignS = BDataSA[31];
 wire BSignD = ADataD[31];
 wire BOverflowA = ~(BSignS ^ BSignD) & (BSignD ^ BResAdd[31]);
 wire BBitT = AOper[3] ? BResAdd[31] ^ BOverflowA : ~BResAdd[32];

 wire BOperNZ = |AOper;
 assign ABusAData =
   ((AOper[1] & AOper[2]) ? {31'h0, BBitT} : 32'h0) |
   ((AOper[3] & ~AOper[1]) ? BResXor : 32'h0) |
   ((AOper[2] & ~AOper[1]) ? BResAnd : 32'h0) |
   (((AOper[0] | AOper[1]) & ~AOper[2]) ? BResAdd[31:0] : 32'h0);
 assign ABusALoad = BOperNZ ? ADstColRow : 12'h0;

 wire BSignR = ABusAData[31];
 wire BOverflow = ~(BSignS ^ BSignD) & (BSignD ^ BSignR);
 assign ABusFData = BOperNZ ? {BOverflow, BSignR, ~BResDataNZ, AOper[1] ^ BResAdd[32]} : 4'h0;
endmodule

module MsrvAluS
 (
  input [31:0] ADataS, input [31:0] ADataD, input [2:0] AOper, input [13:0] ADstColRow,
  output [31:0] ABusAData, output [13:0] ABusALoad
 );

 wire BOperNZ = |AOper;

 wire [63:0] BDataShift =
  (AOper[2] ? {{32{ADataD[31]}}, ADataD} : 64'h0) | // asr
  (AOper[1] ? {32'h0, ADataD} : 64'h0) |            // shr
  (AOper[0] ? {ADataD, 32'h0} : 64'h0);             // shl

 wire [4:0] BShiftA = AOper[0] ? ADataS[4:0] : ~ADataS[4:0] + 5'h1;

 wire [ 64:0] BDataResF = {1'b0, BDataShift};
 wire [ 48:0] BDataResE = BShiftA[4] ? BDataResF[48:0] : BDataResF[ 64:16];
 wire [ 40:0] BDataResD = BShiftA[3] ? BDataResE[40:0] : BDataResE[ 48: 8];
 wire [ 36:0] BDataResC = BShiftA[2] ? BDataResD[36:0] : BDataResD[ 40: 4];
 wire [ 34:0] BDataResB = BShiftA[1] ? BDataResC[34:0] : BDataResC[ 36: 2];
 wire [ 33:0] BDataResA = BShiftA[0] ? BDataResB[33:0] : BDataResB[ 34: 1];

 wire BShiftANZ = |BShiftA;
 assign ABusAData = BShiftANZ ? BDataResA[32:1] : (BOperNZ ? ADataD : 32'h0);
 assign ABusALoad = BOperNZ ? ADstColRow : 14'h0;
endmodule

// ************************
// *** #AluSlow Section ***
// ************************

module MsrvAluSlow
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [31:0] AMuxS, input [31:0] AMuxU,
  input [7:0] AAluSelU, // mulhu mulhsu mulh sdiv Rem udiv mul
  input [6:0] AAluSelF,
  input [13:0] ADstColRow, input [1:0] ACpuIdxM, ACpuIdxE,
  output [31:0] ABusBData, output [13:0] ABusBLoad, ABusBPend, output [1:0] APendAny
 );

 wire FAnyOp, BAnyOp;
 wire [31:0] FMuxS, BMuxS;
 wire [31:0] FMuxU, BMuxU;
 wire [7:0] FAluSelU, BAluSelU;
 wire [6:0] FAluSelF, BAluSelF;
 wire [13:0] FDstColRow, BDstColRow;
 wire [31:0] FBusDData, BBusDData;
 wire [13:0] FBusDLoad, BBusDLoad;

 MsDffList #(.CRegLen(1+32+32+8+7+14+32+14)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BAnyOp, BMuxS, BMuxU, BAluSelU, BAluSelF, BDstColRow, BBusDData, BBusDLoad}),
   .ADataO({FAnyOp, FMuxS, FMuxU, FAluSelU, FAluSelF, FDstColRow, FBusDData, FBusDLoad})
  );

 assign BAnyOp = |{AAluSelU, AAluSelF};
 assign BMuxS = BAnyOp ? AMuxS : 32'h0;
 assign BMuxU = BAnyOp ? AMuxU : 32'h0;
 assign BAluSelU = AAluSelU;
 assign BAluSelF = AAluSelF;
 assign BDstColRow = ADstColRow;

 wire [31:0] BMulDivDataR; wire BMulDivWrEn; wire [31:0] BMulDivDataH;
 wire [31:0] BMulDivDataSU, BMulDivDataDU; wire [2:0] BMulDivSizeU; wire [1:0] BMulDivStartU;
 wire [2:0] BMulDivNegDataU;

 wire [31:0] BAluURes; wire BAluUAck;
 MsrvAluU UAluU
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataS(FMuxS), .ADataD(FMuxU), .AOper(FAluSelU), .ADstColRow(FDstColRow),
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
   .AMuxS(FMuxS), .AMuxU(FMuxU),
   .AAluSel(FAluSelF),
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

 MsBusBCtrl #(.CRegIdxLen(14)) UBusB
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AWrRdEn({1'b0, BAnyOp}), .ARegIdx(ADstColRow), .ACpuIdxM(ACpuIdxM), .ACpuIdxE(ACpuIdxE),
   .AExtMiso(BAluURes | BAluFRes), .AExtAck(BAluUAck | BAluFAck),
   .AWrRdActive(),
   .ABusBData(ABusBData), .ABusBLoad(ABusBLoad), .ABusBPend(ABusBPend),
   .APendAny(APendAny)
  );

endmodule

module MsrvAluU
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [31:0] ADataS, input [31:0] ADataD,
  input [7:0] AOper, // srem mulhu mulhsu mulh sdiv urem udiv mul
  input [13:0] ADstColRow,
  // Common MulDiv
  output [31:0] AMulDivDataS, output [31:0] AMulDivDataD, output [2:0] AMulDivSize, output [1:0] AMulDivStart,
  output [2:0] AMulDivNegData,
  input [31:0] AMulDivDataH, AMulDivDataR, input AMulDivWrEn,
  // Output
  output [31:0] AAluRes, output AAluAck
 );

 wire FBusy, BBusy;
 wire FBusH, BBusH;

 MsDffList #(.CRegLen(1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BBusy, BBusH}),
   .ADataO({FBusy, FBusH})
  );

 wire BMulDivStartNZ = |AOper;  // mulzx gw,gl,gh. Should be set accordingly to gw (16 bit)

 // MulDiv
 assign AMulDivDataS = BMulDivStartNZ ? ADataS : 32'h0;
 assign AMulDivDataD = BMulDivStartNZ ? ADataD : 32'h0;
 assign AMulDivStart = {|{AOper[7], AOper[3], AOper[2], AOper[1]}, |{AOper[6], AOper[5], AOper[4], AOper[0]}};
 assign AMulDivSize  = BMulDivStartNZ ? 3'h4 : 3'h0;
 assign AMulDivNegData =
  ((|{AOper[6], AOper[2], AOper[1]}) ? 3'h0 : 3'h0) |
  ((|{AOper[5]}) ? {ADataD[31], 1'b0, ADataD[31]} : 3'h0) |
  ((|{AOper[7]}) ? {ADataD[31], ADataS[31], ADataD[31]} : 3'h0) |
  ((|{AOper[4], AOper[3], AOper[0]}) ? {ADataS[31] ^ ADataD[31], ADataS[31], ADataD[31]} : 3'h0);

 assign BBusy = BMulDivStartNZ | (FBusy & ~AMulDivWrEn);
 wire BBusBWrEn = FBusy &  AMulDivWrEn;

 assign BBusH = (BMulDivStartNZ & (|{AOper[7], AOper[6], AOper[5], AOper[4], AOper[2]})) | (FBusH & ~AMulDivWrEn);

 // Final assignment
 assign AAluRes = BBusBWrEn ? (FBusH ? AMulDivDataH : AMulDivDataR) : 32'h0;
 assign AAluAck = BBusBWrEn;
endmodule

