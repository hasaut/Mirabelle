module FpuTaylor #(parameter CExpLen=8, CMantLen=28)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [1+CExpLen+CMantLen-1:0] AMuxU, input wire [2:0] AStart,
  output wire [1+CExpLen+CMantLen-1:0] AFpuRes, output wire [2:0] AFpuAck, output wire AMux, ABusy,
  output wire [1+CExpLen+CMantLen-1:0] ANumDataS, ANumDataU, input wire [1+CExpLen+CMantLen-1:0] ANumDataR, output wire ANumReq, input wire ANumAck,
  output wire [1+CExpLen+CMantLen-1:0] ADenDataS, ADenDataU, input wire [1+CExpLen+CMantLen-1:0] ADenDataR, output wire ADenReq, input wire ADenAck,
  output wire [1+CExpLen+CMantLen-1:0] ASumDataS, ASumDataU, input wire [1+CExpLen+CMantLen-1:0] ASumDataR, output wire ASumReq, input wire ASumAck
 );

 localparam CDataLen = 1+CExpLen+CMantLen;
 localparam CMantNil = {CMantLen{1'b0}};

 localparam CMantE = {1'b1, {(CMantLen-1){1'b0}}};
 localparam CExpE  = {1'b0, {(CExpLen-1){1'b1}}};

 localparam CDataE = {1'b0, CExpE, CMantE};
 localparam CDataZ = {CDataLen{1'b0}};

 localparam CFnCnt = 3;
 localparam CFnNil = {CFnCnt{1'b0}};
 localparam IFnExp = 2;
 localparam IFnSin = 1;
 localparam IFnCos = 0;

 // Local vars
 wire [4:0] FStepIdx, BStepIdx;
 wire [5:0] FCoefAddr, BCoefAddr;
 wire [1:0] FCoefStep, BCoefStep;
 wire [1:0] FSignPatt, BSignPatt;
 wire [3:0] FLoad, BLoad;
 wire [CDataLen-1:0] FDataS, BDataS;
 wire [CDataLen-1:0] FDataU, BDataU;
 wire [CDataLen-1:0] FDataR, BDataR;
 wire FBusy, BBusy;
 wire FFpuAck, BFpuAck;
 wire FAluNumPend, BAluNumPend;
 wire FAluDenPend, BAluDenPend;
 wire FAluSumPend, BAluSumPend;
 wire [CFnCnt-1:0] FFpuFn, BFpuFn;

 MsDffList #(.CRegLen(5+6+2+2+4+3*CDataLen+1+1+3+CFnCnt)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BStepIdx, BCoefAddr, BCoefStep, BSignPatt, BLoad, BDataS, BDataU, BDataR, BBusy, BFpuAck, BAluNumPend, BAluDenPend, BAluSumPend, BFpuFn}),
   .ADataO({FStepIdx, FCoefAddr, FCoefStep, FSignPatt, FLoad, FDataS, FDataU, FDataR, FBusy, FFpuAck, FAluNumPend, FAluDenPend, FAluSumPend, FFpuFn})
  );

 // Init
 wire [3:0] BLoadIni; wire [4:0] BStepCntIni; wire [CDataLen-1:0] BDataRIni; wire [5:0] BCoefAddrIni; wire [1:0] BCoefStepIni; wire [1:0] BSignPattIni;
 assign {BLoadIni, BStepCntIni, BDataRIni, BCoefAddrIni, BCoefStepIni, BSignPattIni} =
  (AStart[IFnExp] ? {4'h0, 5'h10, CDataE, 6'h1, 2'h1, 2'h0} : {4'h0, 5'h0, 32'h0, 6'h0, 2'h0, 2'h0}) | // Exp(x)
  (AStart[IFnSin] ? {4'h9, 5'h08, CDataZ, 6'h1, 2'h2, 2'h1} : {4'h0, 5'h0, 32'h0, 6'h0, 2'h0, 2'h0}) | // Sin(x)
  (AStart[IFnCos] ? {4'hA, 5'h08, CDataE, 6'h2, 2'h2, 2'h2} : {4'h0, 5'h0, 32'h0, 6'h0, 2'h0, 2'h0});  // Cos(x)


 // Process
 wire BStartAny = |AStart;
 assign BFpuFn = BStartAny ? AStart : (FBusy ? FFpuFn : CFnNil);

 wire BStepIdxNZ = |FStepIdx;
 wire BLoadIniNZ = |BLoadIni;

 assign ANumReq = (BStartAny & BLoadIniNZ) | (BStepIdxNZ & ~FAluNumPend) | (BStepIdxNZ & ANumAck);
 assign ADenReq = BStepIdxNZ & (ANumAck | ~FAluNumPend);
 assign ASumReq = FBusy & ADenAck;

 assign BStepIdx  = BStartAny ? BStepCntIni : FStepIdx-{4'h0, ADenReq};
 assign BCoefAddr = BStartAny ? BCoefAddrIni : FCoefAddr + {3'h0, ADenReq ? FCoefStep : 2'h0};
 assign BCoefStep = BStartAny ? BCoefStepIni : FCoefStep;
 assign BSignPatt = BStartAny ? BSignPattIni : (ADenReq ? {FSignPatt[0], FSignPatt[1]} : FSignPatt);

 wire [CDataLen-1:0] BSerCoef; assign BSerCoef[CDataLen-1] = FSignPatt[1];
 FpuTaylorSerCoef USerCoef ( .AAddr(FCoefAddr), .ASerCoef(BSerCoef[CDataLen-2:0]) );

 assign BLoad = BStartAny ? BLoadIni : (ANumAck ? 4'h0 : FLoad);

 wire [3:0] LLoadDecS; MsDec2x4a ULoadDecS ( .ADataI(FLoad[3:2]), .ADataO(LLoadDecS) );
 wire [3:0] LLoadDecU; MsDec2x4a ULoadDecU ( .ADataI(FLoad[1:0]), .ADataO(LLoadDecU) );

 wire [CDataLen-1:0] BMuxS =
   (LLoadDecS[2] ? (ANumAck ? ANumDataR : FDataS) : CDataZ) |
   (LLoadDecS[1] ? FDataS : CDataZ) |
   (LLoadDecS[0] ? FDataS : CDataZ);
 wire [CDataLen-1:0] BMuxU =
   (LLoadDecU[2] ? (ANumAck ? ANumDataR : FDataU) : CDataZ) |
   (LLoadDecU[1] ? FDataU : CDataZ) |
   (LLoadDecU[0] ? (ANumAck ? ANumDataR : FDataU) : CDataZ);


 assign BDataS = BStartAny ? AMuxU : BMuxS;
 assign BDataU = BStartAny ? AMuxU : BMuxU;
 assign BDataR = BStartAny ? BDataRIni : (ASumAck ? ASumDataR : FDataR);

 assign BAluNumPend = ANumReq | (FAluNumPend & ~ANumAck);
 assign BAluDenPend = ADenReq | (FAluDenPend & ~ADenAck);
 assign BAluSumPend = ASumReq | (FAluSumPend & ~ASumAck);
 assign BBusy = |{BStartAny, BAluNumPend, BAluDenPend, BAluSumPend};
 assign BFpuAck = FBusy & ~BAluNumPend & ~BAluDenPend & ~BAluSumPend & FAluSumPend & ASumAck;

 /*FpuSMul #(.CExpLen(CExpLen), .CMantLen(CMantLen)) UFpuNum
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BDataS), .AMuxU(BDataU), .AStart(BAluNumStart),
   .AFpuRes(BAluNumRes), .AFpuAck(BAluNumAck)
  );*/
 assign {ANumDataS, ANumDataU} = {BDataS, BDataU};

 /*FpuSMul #(.CExpLen(CExpLen), .CMantLen(CMantLen)) UFpuDen
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BSerCoef), .AMuxU(BMuxU), .AStart(BAluDenStart),
   .AFpuRes(BAluDenRes), .AFpuAck(BAluDenAck)
  ); */
 assign {ADenDataS, ADenDataU} = {BSerCoef, BMuxU};

 /*
 wire [1:0] BAluSumAckA;
 FpuSSum #(.CExpLen(CExpLen), .CMantLen(CMantLen)) UFpuSum
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMuxS(BAluDenRes), .AMuxU(BDataR), .AStart({1'b0, BAluSumStart}),
   .AFpuRes(BAluSumRes), .AFpuAck(BAluSumAckA)
  );
 assign BAluSumAck = |BAluSumAckA; */
 assign {ASumDataS, ASumDataU} = {ADenDataR, BDataR};

 // Common part
 assign {AMux, ABusy} = {FBusy | BStartAny, FBusy}; // Avoid combinatorial loop over ACK
 assign AFpuAck = FFpuAck ? FFpuFn : CFnNil;
 assign AFpuRes = FDataR;
endmodule


