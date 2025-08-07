module FpuStackFifo #(parameter CAddrLen = 2, CDataLen = 8)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CDataLen-1:0] ADataI, input wire AWrEn,
  output wire [CDataLen-1:0] ADataS, ADataU, input wire [CAddrLen-1:0] ARdEn,
  input wire [CDataLen-1:0] ADataR, input wire AWrEnR, input wire [CAddrLen-1:0] AInsPos,
  input wire AClr, output wire AHasData, output wire AHasSpace, output wire [CAddrLen+1-1:0] ADataSize
 );

 localparam CRegCnt = (1<<CAddrLen);
 localparam CAddrE = {{(CAddrLen-1){1'b0}}, 1'b1};
 localparam CDataZ = {CDataLen{1'b0}};

 wire [CAddrLen+1-1:0] FWrIdx, BWrIdx;
 wire [CAddrLen+1-1:0] FRdIdx, BRdIdx;

 MsDffList #(.CRegLen((CAddrLen+1)*2)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx}),
   .ADataO({FWrIdx, FRdIdx})
  );

 wire [CAddrLen-1:0] BAddrI = FWrIdx[CAddrLen-1:0]+{{(CAddrLen-1){1'b0}}, AWrEnR};
 wire [CAddrLen-1:0] BAddrS = FRdIdx[CAddrLen-1:0] + CAddrE;
 wire [CAddrLen-1:0] BAddrU = FRdIdx[CAddrLen-1:0];

 wire [CRegCnt-1:0] BAddrDecI; MsDecAny #(.CAddrLen(CAddrLen)) UAddrDecI ( .AAddr(BAddrI), .ADataO(BAddrDecI) ); // Assume that DataR is inserted always below WrAddr
 wire [CRegCnt-1:0] BAddrDecS; MsDecAny #(.CAddrLen(CAddrLen)) UAddrDecS ( .AAddr(BAddrS), .ADataO(BAddrDecS) );
 wire [CRegCnt-1:0] BAddrDecU; MsDecAny #(.CAddrLen(CAddrLen)) UAddrDecU ( .AAddr(BAddrU), .ADataO(BAddrDecU) );

 // Insert will be suffixed as "J"
 wire [CAddrLen-1:0] BAddrJ = FRdIdx[CAddrLen-1:0] + AInsPos;
 wire [CRegCnt-1:0] BAddrDecJ; MsDecAny #(.CAddrLen(CAddrLen)) UAddrDecJ ( .AAddr(BAddrJ), .ADataO(BAddrDecJ) );

 // AddrDecW is the original WrIdx (without adding WrEnR). We must not shift PrevData to this location if both WrEn and WrEnR are active (otherwise we mistakenly copy the data from the old location)
 wire [CAddrLen-1:0] BAddrW = FWrIdx[CAddrLen-1:0];
 wire [CRegCnt-1:0] BAddrDecW; MsDecAny #(.CAddrLen(CAddrLen)) UAddrDecW ( .AAddr(BAddrW), .ADataO(BAddrDecW) );
 wire [CRegCnt-1:0] BMaskW; MsOrVectR #(.CVectLen(CRegCnt)) UMaskW ( .ADataI(BAddrDecW), .ADataO(BMaskW) );
 wire [CRegCnt-1:0] BMaskJ; MsOrVectR #(.CVectLen(CRegCnt)) UMaskJ ( .ADataI(BAddrDecJ), .ADataO(BMaskJ) );

 //      BAddrJ<BAddrW          |      BAddrJ==BAddrW         |        BAddrJ>BAddrW          |
 //  AddrDec  Mask  &    WrEn   |  AddrDec  Mask  &    WrEn   |    AddrDec  Mask  &    WrEn   |
 // I: 01000 01111 01111        | I: 00100 00111 00111        |   I: 00010 00011 00011        |
 // J: 00010 00011 11100 01100  | J: 00100 00111 11000 00000  |   J: 01000 01111 10000 10011  |
 wire [CRegCnt-1:0] BWrEnPrev = {CRegCnt{AWrEnR}} &
   (
    (BAddrJ<=BAddrW) ? BMaskW & ~BMaskJ : BMaskW | ~BMaskJ
   );



 wire [CRegCnt*CDataLen-1:0] BDataSX, BDataUX;
 wire [CRegCnt*CDataLen-1:0] BDataThis;
 wire [CRegCnt*CDataLen-1:0] BDataPrev = {BDataThis[(CRegCnt-1)*CDataLen-1:0], BDataThis[CRegCnt*CDataLen-1:(CRegCnt-1)*CDataLen]};
 FpuStackItem #(.CDataLen(CDataLen)) UFpuStack[CRegCnt-1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataIA(ADataI), .ADataIB(ADataR), .AWrEnA({CRegCnt{AWrEn}} & BAddrDecI), .AWrEnB({CRegCnt{AWrEnR}} & BAddrDecJ),
   .ADataOB(BDataSX), .ADataOA(BDataUX), .ARdEnB(BAddrDecS), .ARdEnA(BAddrDecU),
   .ADataPrev(BDataPrev), .ADataThis(BDataThis), .AWrEnPrev(BWrEnPrev)
  );

 wire [CDataLen-1:0] BDataS, BDataU; 
 MsMatrOrCol #(.CRowCnt(CRegCnt), .CColCnt(CDataLen)) UDataS ( .ADataI(BDataSX), .ADataO(ADataS) );
 MsMatrOrCol #(.CRowCnt(CRegCnt), .CColCnt(CDataLen)) UDataU ( .ADataI(BDataUX), .ADataO(ADataU) );

 assign BWrIdx = AClr ? {(CAddrLen+1){1'b0}} : FWrIdx + {{CAddrLen{1'b0}}, AWrEn} + {{CAddrLen{1'b0}}, AWrEnR};
 assign BRdIdx = AClr ? {(CAddrLen+1){1'b0}} : FRdIdx + ARdEn;

 wire [CAddrLen+1-1:0] BDataSize = FWrIdx - FRdIdx;

 assign AHasData = ~(FWrIdx==FRdIdx);
 assign AHasSpace = (FWrIdx==FRdIdx) | (FWrIdx[CAddrLen-1:0]!=FRdIdx[CAddrLen-1:0]);
 assign ADataSize = BDataSize;
endmodule


module FpuStackItem #(parameter CDataLen=0)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CDataLen-1:0] ADataIA, ADataIB, input wire AWrEnA, AWrEnB,
  output wire [CDataLen-1:0] ADataOB, ADataOA, input wire ARdEnB, ARdEnA,
  input wire [CDataLen-1:0] ADataPrev, output wire [CDataLen-1:0] ADataThis, input wire AWrEnPrev

 );

 localparam CDataNil = {CDataLen{1'b0}};

 wire [CDataLen-1:0] FData, BData;
 MsDffList #(.CRegLen(CDataLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BData}),
   .ADataO({FData})
  );

 wire BWrEnNZ = |{AWrEnB, AWrEnA, AWrEnPrev};
 assign BData =
  (AWrEnB ? ADataIB : CDataNil) |
  (AWrEnA ? ADataIA : CDataNil) |
  (AWrEnPrev ? ADataPrev : CDataNil) |
  (BWrEnNZ ? CDataNil : FData);

 assign ADataOB = ARdEnB ? FData : CDataNil;
 assign ADataOA = ARdEnA ? FData : CDataNil;
 assign ADataThis = FData;
endmodule

module FpuSSum #(parameter CExpLen=8, CMantLen=28)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [1+CExpLen+CMantLen-1:0] AMuxS, AMuxU, input wire [1:0] AStart,
  output wire [1+CExpLen+CMantLen-1:0] AFpuRes, output wire [1:0] AFpuAck
 );

 localparam CDataLen = 1+CExpLen+CMantLen;
 localparam CDataZ = {CDataLen{1'b0}};
 localparam CExpZ  = {CExpLen{1'b0}};
 localparam CExpF  = {CExpLen{1'b1}};
 localparam CMantZ = {CMantLen{1'b0}};
 localparam CMantF = {CMantLen{1'b1}};

 // Implementation
 wire [1:0] FStart, BStart;
 wire [CDataLen-1:0] FDataS, BDataS;
 wire [CDataLen-1:0] FDataD, BDataD;

 MsDffList #(.CRegLen(2+CDataLen+CDataLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BStart, BDataS, BDataD}),
   .ADataO({FStart, FDataS, FDataD})
  );

 assign BStart = AStart;
 wire BStartAny = |BStart;

 assign BDataD = BStartAny ? AMuxU : CDataZ;
 assign BDataS = BStartAny ? AMuxS : CDataZ;

 // Which one is bigger by module?
 wire BCmpModA = FDataS[CDataLen-2:0] >  FDataD[CDataLen-2:0];
 wire BCmpModE = FDataS[CDataLen-2:0] == FDataD[CDataLen-2:0];
 // BDataB is bigger than BDataA
 wire [CDataLen-1:0] BDataB, BDataA; assign {BDataB, BDataA} = BCmpModA ? {FDataS, FDataD} : {FDataD, FDataS};
 // Mantissa for B and A
 //wire BDataBNZ = |BDataB[30:0]; wire BDataANZ = |BDataA[30:0];
 wire [CMantLen-1:0] BMantB; wire [CExpLen-1:0] BExpB; assign {BExpB, BMantB} = BDataB[CDataLen-2:0];
 wire [CMantLen-1:0] BMantA; wire [CExpLen-1:0] BExpA; assign {BExpA, BMantA} = BDataA[CDataLen-2:0];
 // Is physical operation is add or sub
 wire BIsSub = FDataS[CDataLen-1] ^ FDataD[CDataLen-1] ^ FStart[1];

 // exp delta shows smaller operand shr steps
 wire [CExpLen-1:0] BExpDeltaA = BExpB-BExpA;
 wire BExpUdfA = BExpDeltaA>=CMantLen; // Shows that DataA is shifted completely and does not influence the result anymore
 wire [CExpLen-1:0] BExpDelta = BExpUdfA ? CExpF : BExpDeltaA;

 // AddSub section
 wire [CMantLen-1:0] BMantAShr; FpuMantShr #(.CExpLen(CExpLen), .CMantLen(CMantLen)) UMantAShr ( .ADataI(BMantA), .AShr(BExpDelta), .ADataO(BMantAShr) );
 wire [CMantLen+2-1:0] BMantRA = {1'b0, BMantB, BIsSub} + ({(CMantLen+2){BIsSub}}^{1'b0, BMantAShr, 1'b0});

 // Align
 wire [CMantLen-1:0] BMantRX;
 wire [CExpLen-1:0] BShlIdxRX;
 FpuAlignL #(.CExpLen(CExpLen), .CMantLen(CMantLen)) UMantRB ( .ADataI(BMantRA), .ADataO(BMantRX), .AIdx(BShlIdxRX) );

 wire [1:0] BOvfUdf; wire [CExpLen-1:0] BExpR;
 assign {BOvfUdf, BExpR} = {2'h3, BExpB} - {2'h0, BShlIdxRX} + {{(CExpLen+1){1'b0}}, 1'b1}; // <- Add 1 here because BMantRX is shifted left 1 bit more
 wire BExpOvf = BOvfUdf==2'b00; // Can happen when BExpB==FF, BShlIdxRX==00; i.e. the value is too big
 wire BExpUdf = BOvfUdf==2'b10; // Means that BExpB was originally very small and operation did harm OvfUdf[0]; i.e. we obtain too small value

 wire BOrigSignE = FDataD[31]==FDataS[31];
 wire BSign = (BCmpModE & ~BOrigSignE) ? 1'b0 : BDataB[CDataLen-1];

 wire [CDataLen-1:0] BDataRF =
  {
   BSign,
   BExpUdf ? {(CDataLen-1){1'b0}} : (BExpOvf ? {CExpF, CMantZ} : {BExpR[CExpLen-1:0], BMantRX})
  };

 assign AFpuRes = BDataRF;
 assign AFpuAck = FStart;
endmodule

module FpuSMul #(parameter CExpLen=8, CMantLen=28)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [1+CExpLen+CMantLen-1:0] AMuxS, AMuxU, input wire AStart,
  output wire [1+CExpLen+CMantLen-1:0] AFpuRes, output wire AFpuAck
 );

 localparam CDataLen = 1+CExpLen+CMantLen;
 localparam CMulResLen = CMantLen+2;
 localparam CDataZ = {CDataLen{1'b0}};
 localparam CMantZ = {CMantLen{1'b0}};
 localparam CExpZ  = {CExpLen{1'b0}};
 localparam CExpF  = {CExpLen{1'b1}};
 localparam CExpCorr = {1'b1, {(CExpLen-2){1'b0}}, 1'b1};
 localparam CMulResZ = {CMulResLen{1'b0}};

 // Implementation
 wire FSignS, BSignS;
 wire FSignU, BSignU;
 wire [CExpLen-1:0] FExpS, BExpS;
 wire [CExpLen-1:0] FExpU, BExpU;
 wire [CMulResLen-1:0] FMulIRes, BMulIRes;
 wire FMulReady, BMulReady;

 MsDffList #(.CRegLen(2*1+2*CExpLen+CMulResLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSignS, BSignU, BExpS, BExpU, BMulIRes, BMulReady}),
   .ADataO({FSignS, FSignU, FExpS, FExpU, FMulIRes, FMulReady})
  );

 wire [CMantLen-1:0] BMantS, BMantU;
 assign {BSignS, BExpS, BMantS, BSignU, BExpU, BMantU} = AStart ? {AMuxS, AMuxU} : {FSignS, FExpS, CMantZ, FSignU, FExpU, CMantZ};

 wire [CMulResLen-1:0] BMulResA;
 FpuIMul #(.CMantLen(CMantLen)) UIMul
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataS(BMantS), .ADataD(BMantU), .AStart(AStart),
   .ADataR(BMulResA), .ABusy(), .AWrEn(BMulReady)
  );


 assign BMulIRes = BMulReady ? BMulResA : CMulResZ;

 wire [CMantLen+1-1:0] BMantXA = FMulIRes[CMulResLen-1] ? FMulIRes[CMulResLen-1:1] : FMulIRes[CMulResLen-2:0];
 wire [CMantLen-1:0] BMantX = BMantXA[CMantLen:1] + {{(CMantLen-1){1'b0}}, FMulIRes[0]};

 wire [CExpLen+2-1:0] BExp = {2'h0, FExpS} + {2'h0, FExpU} + CExpCorr + {9'h0, FMulIRes[CMulResLen-1]};
 wire BExpOvf = BExp[CExpLen+2-1];
 wire BExpUdf = (BExp[CExpLen+2-1:CExpLen+2-2]==2'h0) | (BExp=={2'h1, CExpZ});

 wire BMantXNZ = |BMantX;
 wire [CDataLen-1:0] BDataRF = {FSignS^FSignU, {(CDataLen-1){BMantXNZ & ~BExpUdf}} & (BExpOvf ? {CExpF, CMantZ} : {BExp[CExpLen-1:0], BMantX})};

 // Result
 assign AFpuRes = BDataRF;
 assign AFpuAck = FMulReady;
endmodule


