module IoAddrDec2a
 (
  input wire [1:0] AAddr, input wire [3:0] AWrSize, input wire [3:0] ARdSize,
  output wire [3:0] AWrEnQ, output wire [3:0] ARdEnQ,
  output wire [3:0] AWrEnD, output wire [3:0] ARdEnD,
  output wire [3:0] AWrEnW, output wire [3:0] ARdEnW,
  output wire [3:0] AWrEnB, output wire [3:0] ARdEnB,
  input wire [31:0] AAddrUsed, output wire AAddrAck
 );

 wire [3:0] AAddrDec; MsDec2x4a UAddrDec ( AAddr, AAddrDec );

 assign {AWrEnQ, ARdEnQ} = {{4{AWrSize[3]}}, {4{ARdSize[3]}}} & {AAddrDec, AAddrDec};
 assign {AWrEnD, ARdEnD} = {{4{AWrSize[2]}}, {4{ARdSize[2]}}} & {AAddrDec, AAddrDec};
 assign {AWrEnW, ARdEnW} = {{4{AWrSize[1]}}, {4{ARdSize[1]}}} & {AAddrDec, AAddrDec};
 assign {AWrEnB, ARdEnB} = {{4{AWrSize[0]}}, {4{ARdSize[0]}}} & {AAddrDec, AAddrDec};

 assign AAddrAck = |(AAddrUsed & {AWrEnQ, ARdEnQ, AWrEnD, ARdEnD, AWrEnW, ARdEnW, AWrEnB, ARdEnB});

endmodule

module IoIntf2a #(parameter CAddrBase=16'h0000, CAddrUsed=32'h00000000)
 (
  input wire [15:0] AAddr, input wire [3:0] AWrSize, input wire [3:0] ARdSize,
  output wire [3:0] AWrEnQ, output wire [3:0] ARdEnQ,
  output wire [3:0] AWrEnD, output wire [3:0] ARdEnD,
  output wire [3:0] AWrEnW, output wire [3:0] ARdEnW,
  output wire [3:0] AWrEnB, output wire [3:0] ARdEnB,
  output wire AAddrAck, output wire AAddrErr
 );

 wire BAddrCmp = (AAddr[15:2]==CAddrBase[15:2]);
 wire [3:0] BWrSize = {4{BAddrCmp}} & AWrSize;
 wire [3:0] BRdSize = {4{BAddrCmp}} & ARdSize;
 wire [3:0] BAddrDec; MsDec2x4a UAddrDec ( AAddr[1:0], BAddrDec );

 assign {AWrEnQ, ARdEnQ} = {{4{BWrSize[3]}}, {4{BRdSize[3]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnD, ARdEnD} = {{4{BWrSize[2]}}, {4{BRdSize[2]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnW, ARdEnW} = {{4{BWrSize[1]}}, {4{BRdSize[1]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnB, ARdEnB} = {{4{BWrSize[0]}}, {4{BRdSize[0]}}} & {BAddrDec, BAddrDec};

 wire BWrRdSizeNZ = |{AWrSize, ARdSize};
 assign AAddrAck = |(CAddrUsed & {AWrEnQ, ARdEnQ, AWrEnD, ARdEnD, AWrEnW, ARdEnW, AWrEnB, ARdEnB});
 assign AAddrErr = BWrRdSizeNZ & BAddrCmp & ~AAddrAck;

endmodule

module IoIntf2s #(parameter CAddrBase=16'h0000, CAddrUsed=32'h00000000)
 (
  input wire [15:0] AIoAddr, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize,
  output wire [31:0] AIoAccess,
  output wire AAddrAck, output wire AAddrErr
 );

 wire BAddrCmp = (AIoAddr[15:2]==CAddrBase[15:2]);
 wire [3:0] BWrSize = {4{BAddrCmp}} & AIoWrSize;
 wire [3:0] BRdSize = {4{BAddrCmp}} & AIoRdSize;
 wire [3:0] BAddrDec; MsDec2x4a UAddrDec ( AIoAddr[1:0], BAddrDec );

 assign AIoAccess = {8{BAddrDec}} &
  {
   {4{BWrSize[3]}}, {4{BRdSize[3]}}, {4{BWrSize[2]}}, {4{BRdSize[2]}}, {4{BWrSize[1]}}, {4{BRdSize[1]}}, {4{BWrSize[0]}}, {4{BRdSize[0]}}
  };

 wire BWrRdSizeNZ = |{AIoWrSize, AIoRdSize};
 assign AAddrAck = |(CAddrUsed & AIoAccess);
 assign AAddrErr = BWrRdSizeNZ & BAddrCmp & ~AAddrAck;
endmodule

module IoSizeToMask ( input wire [3:0] ASize, output wire [7:0] AMask );
 assign AMask = {{4{ASize[3]}}, {2{|ASize[3:2]}}, |ASize[3:1], |ASize};
endmodule

module PerifTimer #(parameter CDataLen=16)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CDataLen-1:0] AIoMosi, input wire AIoWrEn,
  input wire [1:0] ASyncSel, input wire ASync1M, input wire ASync1K,
  input wire ATimerReset, input wire ACountEn, output wire [CDataLen-1:0] ATimerThis, output wire ATimerNZ
 );

 // Local vars (Process)
 wire [CDataLen-1:0] FTOut, BTOut;
 wire [CDataLen-1:0] FTimer, BTimer;
 wire FTimerNZ, BTimerNZ;

 MsDffList #(.CRegLen(CDataLen+CDataLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BTOut, BTimer, BTimerNZ}),
   .ADataO({FTOut, FTimer, FTimerNZ})
  );

 assign BTOut = AIoWrEn ? AIoMosi : FTOut;

 wire [3:0] BSyncSel; MsDec2x4a USyncSel ( .ADataI(ASyncSel), .ADataO(BSyncSel) );

 wire BDecEn = |(BSyncSel[3:1] & {1'b1, ASync1M, ASync1K});
 assign BTimerNZ = |FTimer;

 assign BTimer = AIoWrEn ? AIoMosi : (ATimerReset ? FTOut : FTimer - {{(CDataLen-1){1'b0}}, BTimerNZ & BDecEn & ACountEn});

 assign ATimerThis = FTimer;
 assign ATimerNZ = FTimerNZ;
endmodule

module PerifGpio
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] AIoMosi, output wire [7:0] AIoMiso, input wire [1:0] AIoWrRdEn,
  input wire [3:0] AGpioI, output wire [3:0] AGpioO, output wire [3:0] AGpioE
 );

 wire [3:0] FGpioI, BGpioI;
 wire [3:0] FGpioO, BGpioO;
 wire [3:0] FGpioE, BGpioE;

 MsDffList #(.CRegLen(4+4+4)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BGpioI, BGpioO, BGpioE}),
   .ADataO({FGpioI, FGpioO, FGpioE})
  );

 assign BGpioI = AGpioI;
 assign {BGpioE, BGpioO} = AIoWrRdEn[1] ? AIoMosi : {FGpioE, FGpioO};
 assign AIoMiso = AIoWrRdEn[0] ? {4'h0, FGpioI} : 8'h0;

 assign {AGpioE, AGpioO} = {FGpioE, FGpioO};
endmodule

module MsFifoMx #(parameter CAddrLen=8, CDataLen=16)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CDataLen-1:0] ADataI, input wire AWrEn,
  output wire [CDataLen-1:0] ADataO, input wire ARdEn,
  input wire AClr, output wire AHasData, output wire AHasSpace, output wire [15:0] ADataSize
 );

 localparam CAddrNil = {(CAddrLen+1){1'b0}};

 wire [CAddrLen:0] FWrIdx, BWrIdx;
 wire [CAddrLen:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;

 MsDffList #(.CRegLen(CAddrLen+1+CAddrLen+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 RamSDP #(.CAddrLen(CAddrLen), .CDataLen(CDataLen)) URam
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrWr(FWrIdx[CAddrLen-1:0]), .AAddrRd(FRdIdx[CAddrLen-1:0]), .AMosi(ADataI), .AMiso(ADataO), .AWrEn(AWrEn)
  );

 assign BWrIdx = AClr ? CAddrNil : FWrIdx + {{CAddrLen{1'b0}}, AWrEn};
 assign BRdIdx = AClr ? CAddrNil : FRdIdx + {{CAddrLen{1'b0}}, ARdEn};

 wire [CAddrLen:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 //wire BAddrLE = FWrIdx[7:0]==FRdIdx[7:0];
 //wire BAddrHE = BAddrLE & (FWrIdx[8]==FRdIdx[8]);

 assign BHasData = BDataSizeNZ & ~ARdEn; // Avoid Comb-loop and enables Memory address feed from "FRdIdx" (and not "BRdIdx")

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[CAddrLen];
 assign ADataSize = {{(15-CAddrLen){1'b0}}, BDataSize};
endmodule

// Different from "Mx" in a way how HasData is reported, also "BRdIdx" (and not "FRdIdx") is used to address memory
module MsFifoMxA #(parameter CAddrLen=8, CDataLen=16)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CDataLen-1:0] ADataI, input wire AWrEn,
  output wire [CDataLen-1:0] ADataO, input wire ARdEn,
  input wire AClr, output wire AHasData, output wire AHasSpace, output wire [15:0] ADataSize
 );

 localparam CAddrNil = {(CAddrLen+1){1'b0}};

 wire [CAddrLen:0] FWrIdx, BWrIdx;
 wire [CAddrLen:0] FRdIdx, BRdIdx;

 MsDffList #(.CRegLen(CAddrLen+1+CAddrLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx}),
   .ADataO({FWrIdx, FRdIdx})
  );

 RamSDP #(.CAddrLen(CAddrLen), .CDataLen(CDataLen)) URam
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrWr(FWrIdx[CAddrLen-1:0]), .AAddrRd(BRdIdx[CAddrLen-1:0]), .AMosi(ADataI), .AMiso(ADataO), .AWrEn(AWrEn)
  );

 assign BWrIdx = AClr ? CAddrNil : FWrIdx + {{CAddrLen{1'b0}}, AWrEn};
 assign BRdIdx = AClr ? CAddrNil : FRdIdx + {{CAddrLen{1'b0}}, ARdEn};

 wire [CAddrLen:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 //wire BAddrLE = FWrIdx[7:0]==FRdIdx[7:0];
 //wire BAddrHE = BAddrLE & (FWrIdx[8]==FRdIdx[8]);

 assign AHasData  = BDataSizeNZ;
 assign AHasSpace = ~BDataSize[CAddrLen];
 assign ADataSize = {{(15-CAddrLen){1'b0}}, BDataSize};
endmodule

module MsFifoDff #(parameter CAddrLen = 2, CDataLen = 8)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [(CDataLen-1):0] ADataI, input wire AWrEn,
  output wire [(CDataLen-1):0] ADataO, input wire ARdEn,
  input wire AClr, output wire AHasData, output wire AHasSpace, output wire [15:0] ADataSize
 );

 localparam CRegCnt = (1<<CAddrLen);

 wire [(CDataLen*CRegCnt-1):0] FData, BData;
 wire [CAddrLen:0] FWrIdx, BWrIdx;
 wire [CAddrLen:0] FRdIdx, BRdIdx;

 MsDffList #(.CRegLen(CDataLen*CRegCnt+(CAddrLen+1)*2)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BData, BWrIdx, BRdIdx}),
   .ADataO({FData, FWrIdx, FRdIdx})
  );

 wire [CRegCnt-1:0] BWrIdxDec; MsDecAny #(.CAddrLen(CAddrLen)) UWrIdxDec ( .AAddr(FWrIdx[CAddrLen-1:0]), .ADataO(BWrIdxDec) );
 wire [(CDataLen*CRegCnt-1):0] BWrMux; MsSpreadVect #(.CVectLen(CRegCnt), .CSpreadLen(CDataLen)) UWrMux ( .ADataI({CRegCnt{AWrEn}} & BWrIdxDec), .ADataO(BWrMux) );
 MsMux1b UData[(CDataLen*CRegCnt-1):0] ( .ADataA(FData), .ADataB({CRegCnt{ADataI}}), .ADataO(BData), .AAddr(BWrMux) );

 assign BWrIdx = AClr ? {(CAddrLen+1){1'b0}} : FWrIdx + {{CAddrLen{1'b0}}, AWrEn};
 assign BRdIdx = AClr ? {(CAddrLen+1){1'b0}} : FRdIdx + {{CAddrLen{1'b0}}, ARdEn};

 wire [CAddrLen:0] BDataSize = FWrIdx - FRdIdx;

 MsMuxAny #(.CLenFinal(CDataLen), .CAddrLen(CAddrLen)) UDataO ( .ADataI(FData), .AAddr(FRdIdx[CAddrLen-1:0]), .ADataO(ADataO) );

 assign AHasData = ~(FWrIdx==FRdIdx);
 assign AHasSpace = (FWrIdx==FRdIdx) | (FWrIdx[CAddrLen-1:0]!=FRdIdx[CAddrLen-1:0]);
 assign ADataSize = {{(15-CAddrLen){1'b0}}, BDataSize};
endmodule

module PerifFifoMem #(parameter CAddrLen = 3)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [(CAddrLen-1):0] AAddrI, input wire [7:0] ADataI, input wire AWrEn,
  input wire [(CAddrLen-1):0] AAddrO, output wire [7:0] ADataO
 );

 localparam CByteCnt = 1<<CAddrLen;
 localparam CDataLen = 8*CByteCnt;

 wire [CDataLen-1:0] FData, BData;

 MsDffList #(.CRegLen(CDataLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BData}),
   .ADataO({FData})
  );

 wire [CByteCnt-1:0] BMaskA; MsDecAny #(.CAddrLen(CAddrLen)) UMaskA ( .AAddr(AAddrI), .ADataO(BMaskA) );
 wire [CDataLen-1:0] BMask; MsSpreadVect #(.CVectLen(CByteCnt), .CSpreadLen(8)) UMask ( .ADataI(AWrEn ? BMaskA : {CByteCnt{1'b0}}), .ADataO(BMask) );
 wire [CDataLen-1:0] BDataOr = {CByteCnt{ADataI}};

 assign BData = (BDataOr & BMask) | (FData & ~BMask);

 MsMuxAny #(.CLenFinal(8), .CAddrLen(CAddrLen)) UDataO ( .ADataI(FData), .AAddr(AAddrO), .ADataO(ADataO) );

endmodule

module PerifDataRol ( input wire [63:0] ADataI, input wire [7:0] AMaskI, input wire [2:0] AShCnt, output wire [63:0] ADataO, output wire [7:0] AMaskO );

 wire [63:0] BDataC = AShCnt[2] ? {ADataI[31:0], ADataI[63:32]} : ADataI;
 wire [63:0] BDataB = AShCnt[1] ? {BDataC[47:0], BDataC[63:48]} : BDataC;
 wire [63:0] BDataA = AShCnt[0] ? {BDataB[55:0], BDataB[63:56]} : BDataB;
 assign ADataO = BDataA;

 wire [7:0] BMaskC = AShCnt[2] ? {AMaskI[3:0], AMaskI[7:4]} : AMaskI;
 wire [7:0] BMaskB = AShCnt[1] ? {BMaskC[5:0], BMaskC[7:6]} : BMaskC;
 wire [7:0] BMaskA = AShCnt[0] ? {BMaskB[6:0], BMaskB[7:7]} : BMaskB;
 assign AMaskO = BMaskA;
endmodule

module PerifDataRor ( input wire [63:0] ADataI, input wire [2:0] AShCnt, output wire [63:0] ADataO );
 wire [63:0] BDataC = AShCnt[2] ? {ADataI[31:0], ADataI[63:32]} : ADataI;
 wire [63:0] BDataB = AShCnt[1] ? {BDataC[15:0], BDataC[63:16]} : BDataC;
 wire [63:0] BDataA = AShCnt[0] ? {BDataB[ 7:0], BDataB[63: 8]} : BDataB;
 assign ADataO = BDataA;
endmodule

// When different lines of FIFO are accessed, the address can overlap. So it will be +1 for certain lines
module PerifFifoAddrInc #(parameter CAddrLen=5) ( input wire [CAddrLen-1:0] AAddrI, input wire [7:0] AMask, output wire [8*(CAddrLen-3)-1:0] AAddrO );

 wire [11:0] BMuxC = AAddrI[2] ? {AMask, 4'h0} : {4'h0, AMask};
 wire [13:0] BMuxB = AAddrI[1] ? {BMuxC, 2'h0} : {2'h0, BMuxC};
 wire [14:0] BMuxA = AAddrI[0] ? {BMuxB, 1'h0} : {1'h0, BMuxB};

 wire [CAddrLen-4:0] BAddrA = AAddrI[CAddrLen-1:3];
 wire [CAddrLen-4:0] BAddrB = BAddrA + {{(CAddrLen-4){1'b0}}, 1'h1};

 assign AAddrO =
  {
   BAddrA,
   BMuxA[14] ? BAddrB : BAddrA,
   BMuxA[13] ? BAddrB : BAddrA,
   BMuxA[12] ? BAddrB : BAddrA,
   BMuxA[11] ? BAddrB : BAddrA,
   BMuxA[10] ? BAddrB : BAddrA,
   BMuxA[ 9] ? BAddrB : BAddrA,
   BMuxA[ 8] ? BAddrB : BAddrA
  };
endmodule

module FifoBufSendQ
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [63:0] ADataI, input wire [3:0] AWrSize,
  output wire [7:0] ADataO, input wire ARdEn,
  input wire AClr, output wire AHasData, output wire [3:0] AFreeSize
 );

 // Process
 wire [3:0] FWrIdx, BWrIdx;
 wire [3:0] FRdIdx, BRdIdx;
 wire [63:0] FBuf, BBuf;

 MsDffList #(.CRegLen(4+4+64)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BBuf}),
   .ADataO({FWrIdx, FRdIdx, FBuf})
  );

 wire [7:0] BWrMaskA; IoSizeToMask UWrMaskA ( .ASize(AWrSize), .AMask(BWrMaskA) );
 wire [63:0] BDataRol; wire [7:0] BMaskRol; PerifDataRol UDataRol ( .ADataI(ADataI), .AMaskI(BWrMaskA), .AShCnt(FWrIdx[2:0]), .ADataO(BDataRol), .AMaskO(BMaskRol) );
 wire [63:0] BWrMask; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) UWrMask ( .ADataI(BMaskRol), .ADataO(BWrMask) );

 assign BBuf = (BWrMask & BDataRol) | (~BWrMask & FBuf);

 wire [7:0] BRdMask; MsDec3x8a URdMask ( .ADataI(FRdIdx[2:0]), .ADataO(BRdMask) );
 MsSelectRow #( .CRowCnt(8), .CColCnt(8) ) UDataO ( .ADataI(FBuf), .AMask(BRdMask), .ADataO(ADataO) );

 assign BWrIdx = AClr ? 4'h0 : FWrIdx+AWrSize;
 assign BRdIdx = AClr ? 4'h0 : FRdIdx+{3'h0, ARdEn};

 wire [3:0] BDataSize = FWrIdx-FRdIdx;
 assign AHasData = |BDataSize;
 assign AFreeSize = 4'h8-BDataSize;
endmodule

module FifoBufRecvQ
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] ADataI, input wire AWrEn,
  output wire [63:0] ADataO, input wire [3:0] ARdSize,
  input wire AClr, output wire AHasSpace, output wire [3:0] AFillSize
 );

 // Process
 wire [3:0] FWrIdx, BWrIdx;
 wire [3:0] FRdIdx, BRdIdx;
 wire [63:0] FBuf, BBuf;

 MsDffList #(.CRegLen(4+4+64)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BBuf}),
   .ADataO({FWrIdx, FRdIdx, FBuf})
  );

 wire [7:0] BWrMaskA; MsDec3x8e UWrMaskA ( .ADataI(FWrIdx[2:0]), .AEn(AWrEn), .ADataO(BWrMaskA) );
 wire [63:0] BWrMask; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) UWrMask ( .ADataI(BWrMaskA), .ADataO(BWrMask) );

 assign BBuf = (BWrMask & {8{ADataI}}) | (~BWrMask & FBuf);

 wire [63:0] BDataO; PerifDataRor UDataO ( .ADataI(FBuf), .AShCnt(FRdIdx[2:0]), .ADataO(BDataO) );

 wire [7:0] BRdMaskA; IoSizeToMask URdMaskA ( .ASize(ARdSize), .AMask(BRdMaskA) );
 wire [63:0] BRdMask; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) URdMask ( .ADataI(BRdMaskA), .ADataO(BRdMask) );
 assign ADataO = BDataO & BRdMask;

 assign BWrIdx = AClr ? 4'h0 : FWrIdx+{3'h0, AWrEn};
 assign BRdIdx = AClr ? 4'h0 : FRdIdx+ARdSize;

 wire [3:0] BDataSize = FWrIdx-FRdIdx;
 assign AHasSpace = ~BDataSize[3];
 assign AFillSize = BDataSize;
endmodule

module PerifFifoSend #(parameter CAddrLen = 5)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [63:0] ADataI, input wire [3:0] AWrSize,
  output wire [7:0] ADataO, input wire ARdEn,
  input wire AClr, output wire AHasData, output wire [3:0] AFreeSize,
  output wire [3:0] ATest
 );

 wire [7:0] BDataBofi; wire BProcBofi; // BOFI = Buf-Out-Fifo-In
 wire BBufHasData, BFifoHasSpace;
 FifoBufSendQ UBufQ
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(ADataI), .AWrSize(AWrSize),
   .ADataO(BDataBofi), .ARdEn(BProcBofi),
   .AClr(AClr), .AHasData(BBufHasData), .AFreeSize(AFreeSize)
  );

 MsFifoMx #(.CAddrLen(CAddrLen), .CDataLen(8) ) UFifo
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BDataBofi), .AWrEn(BProcBofi),
   .ADataO(ADataO), .ARdEn(ARdEn),
   .AClr(AClr), .AHasData(AHasData), .AHasSpace(BFifoHasSpace), .ADataSize()
  );

 assign BProcBofi = BBufHasData & BFifoHasSpace;
 assign ATest = {|AWrSize, ARdEn, AFreeSize[3], BProcBofi};
endmodule

module PerifFifoRecv #(parameter CAddrLen = 5)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] ADataI, input wire AWrEn,
  output wire [63:0] ADataO, input wire [3:0] ARdSize,
  input wire AClr, output wire AHasSpace, output wire [3:0] AFillSize,
  output wire [3:0] ATest
 );

 wire [7:0] BDataBifo; wire BProcBifo; // BIFO = Buf-In-Fifo-Out
 wire BFifoHasData, BBufHasSpace;
 MsFifoMx #(.CAddrLen(CAddrLen), .CDataLen(8) ) UFifo
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(ADataI), .AWrEn(AWrEn),
   .ADataO(BDataBifo), .ARdEn(BProcBifo),
   .AClr(AClr), .AHasData(BFifoHasData), .AHasSpace(AHasSpace), .ADataSize()
  );

 FifoBufRecvQ UBufQ
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BDataBifo), .AWrEn(BProcBifo),
   .ADataO(ADataO), .ARdSize(ARdSize),
   .AClr(AClr), .AHasSpace(BBufHasSpace), .AFillSize(AFillSize)
  );

 assign BProcBifo = BFifoHasData & BBufHasSpace;
 assign ATest = {AWrEn, |ARdSize, AFillSize[3], BProcBifo};

endmodule

module PerifCfgBlock #(parameter CRegLen=8, CRegCnt=6)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [(CRegLen-1):0] ADataI, input wire AWrEn, input wire AAnyOtherAccess,
  output wire [(CRegLen*CRegCnt-1):0] ADataO
 );

 // Process
 wire [CRegCnt-1:0] FWrIdx, BWrIdx;

 MsDffList #(.CRegLen(CRegCnt)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BWrIdx}),
   .ADataO({FWrIdx})
  );

 MsBufReg #(.CDataWidth(CRegLen)) UDataO[(CRegCnt-1):0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AMosi(ADataI), .AWrEn({CRegCnt{AWrEn}} & FWrIdx), .ARegData(ADataO)
  );

 assign BWrIdx = AWrEn ? {FWrIdx[CRegCnt-2:0], 1'b0} : (AAnyOtherAccess ? {{(CRegCnt-1){1'b0}}, 1'b1} : FWrIdx);
endmodule

