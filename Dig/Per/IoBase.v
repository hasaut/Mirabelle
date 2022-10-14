module IoAddrDec2a
 (
  input [1:0] AAddr, input [3:0] AWrSize, input [3:0] ARdSize,
  output [3:0] AWrEnQ, output [3:0] ARdEnQ,
  output [3:0] AWrEnD, output [3:0] ARdEnD,
  output [3:0] AWrEnW, output [3:0] ARdEnW,
  output [3:0] AWrEnB, output [3:0] ARdEnB,
  input [31:0] AAddrUsed, output AAddrAck
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
  input [15:0] AAddr, input [3:0] AWrSize, input [3:0] ARdSize,
  output [3:0] AWrEnQ, output [3:0] ARdEnQ,
  output [3:0] AWrEnD, output [3:0] ARdEnD,
  output [3:0] AWrEnW, output [3:0] ARdEnW,
  output [3:0] AWrEnB, output [3:0] ARdEnB,
  output AAddrAck, output AAddrErr
 );

 wire BAddrCmp = (AAddr[15:2]==CAddrBase[15:2]);
 wire [3:0] BWrSize = {4{BAddrCmp}} & AWrSize;
 wire [3:0] BRdSize = {4{BAddrCmp}} & ARdSize;
 wire [3:0] BAddrDec; MsDec2x4a UAddrDec ( AAddr[1:0], BAddrDec );

 assign {AWrEnQ, ARdEnQ} = {{4{BWrSize[3]}}, {4{BRdSize[3]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnD, ARdEnD} = {{4{BWrSize[2]}}, {4{BRdSize[2]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnW, ARdEnW} = {{4{BWrSize[1]}}, {4{BRdSize[1]}}} & {BAddrDec, BAddrDec};
 assign {AWrEnB, ARdEnB} = {{4{BWrSize[0]}}, {4{BRdSize[0]}}} & {BAddrDec, BAddrDec};

 assign AAddrAck = |(CAddrUsed & {AWrEnQ, ARdEnQ, AWrEnD, ARdEnD, AWrEnW, ARdEnW, AWrEnB, ARdEnB});
 assign AAddrErr = BAddrCmp & ~AAddrAck;

endmodule

module IoIntf2s #(parameter CAddrBase=16'h0000, CAddrUsed=32'h00000000)
 (
  input [15:0] AIoAddr, input [3:0] AIoWrSize, input [3:0] AIoRdSize,
  output [31:0] AIoAccess,
  output AAddrAck, output AAddrErr
 );

 wire BAddrCmp = (AIoAddr[15:2]==CAddrBase[15:2]);
 wire [3:0] BWrSize = {4{BAddrCmp}} & AIoWrSize;
 wire [3:0] BRdSize = {4{BAddrCmp}} & AIoRdSize;
 wire [3:0] BAddrDec; MsDec2x4a UAddrDec ( AIoAddr[1:0], BAddrDec );

 assign AIoAccess = {8{BAddrDec}} &
  {
   {4{BWrSize[3]}}, {4{BRdSize[3]}}, {4{BWrSize[2]}}, {4{BRdSize[2]}}, {4{BWrSize[1]}}, {4{BRdSize[1]}}, {4{BWrSize[0]}}, {4{BRdSize[0]}}
  };

 assign AAddrAck = |(CAddrUsed & AIoAccess);
 assign AAddrErr = BAddrCmp & ~AAddrAck;
endmodule

module IoSizeToMask ( input [3:0] ASize, output [7:0] AMask );
 assign AMask = {{4{ASize[3]}}, {2{|ASize[3:2]}}, |ASize[3:1], |ASize};
endmodule

/*module IoIntf2a64d
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] AAddr, input [63:0] AMosi, output [63:0] AMiso, input [3:0] AWrSize, input [3:0] ARdSize,
  output [3:0] AWrEnQ, output [3:0] ARdEnQ,
  output [3:0] AWrEnD, output [3:0] ARdEnD,
  output [3:0] AWrEnW, output [3:0] ARdEnW,
  output [3:0] AWrEnB, output [3:0] ARdEnB,
  output [255:0] ADataMosiQ, output [127:0] ADataMosiD, output [63:0] ADataMosiW, output [31:0] ADataMosiB,
  input [255:0] ADataMisoQ, input [127:0] ADataMisoD, input [63:0] ADataMisoW, input [31:0] ADataMisoB
 );

 // Implementation

 IoAddrDec2a UIoAddrDec ( .AAddr(AAddr), .AWrSize(AWrSize), .ARdSize(ARdSize), .AWrEnQ(AWrEnQ), .ARdEnQ(ARdEnQ), .AWrEnD(AWrEnD), .ARdEnD(ARdEnD), .AWrEnW(AWrEnW), .ARdEnW(ARdEnW), .AWrEnB(AWrEnB), .ARdEnB(ARdEnB) );

 MsBufReg #(.CDataWidth(64)) UInputRegQ[3:0]
  (
   .AClkH({4{AClkH}}), .AResetHN({4{AResetHN}}), .AClkHEn({4{AClkHEn}}),
   .AMosi({4{AMosi[63:0]}}),
   .AWrEn(AWrEnQ),
   .ARegData(ADataMosiQ)
  );

 MsBufReg #(.CDataWidth(32)) UInputRegD[3:0]
  (
   .AClkH({4{AClkH}}), .AResetHN({4{AResetHN}}), .AClkHEn({4{AClkHEn}}),
   .AMosi({4{AMosi[31:0]}}),
   .AWrEn(AWrEnD),
   .ARegData(ADataMosiD)
  );

 MsBufReg #(.CDataWidth(16)) UInputRegW[3:0]
  (
   .AClkH({4{AClkH}}), .AResetHN({4{AResetHN}}), .AClkHEn({4{AClkHEn}}),
   .AMosi({4{AMosi[15:0]}}),
   .AWrEn(AWrEnW),
   .ARegData(ADataMosiW)
  );

 MsBufReg #(.CDataWidth(8)) UInputRegB[3:0]
  (
   .AClkH({4{AClkH}}), .AResetHN({4{AResetHN}}), .AClkHEn({4{AClkHEn}}),
   .AMosi({4{AMosi[7:0]}}),
   .AWrEn(AWrEnB),
   .ARegData(ADataMosiB)
  );

 assign AMiso =
  ({64{ARdEnQ[3]}} & ADataMisoQ[255:192]) |
  ({64{ARdEnQ[2]}} & ADataMisoQ[191:128]) |
  ({64{ARdEnQ[1]}} & ADataMisoQ[127: 64]) |
  ({64{ARdEnQ[0]}} & ADataMisoQ[ 63:  0]) |
  {32'h0, {32{ARdEnD[3]}} & ADataMisoD[127:96]} |
  {32'h0, {32{ARdEnD[2]}} & ADataMisoD[ 95:64]} |
  {32'h0, {32{ARdEnD[1]}} & ADataMisoD[ 63:32]} |
  {32'h0, {32{ARdEnD[0]}} & ADataMisoD[ 31: 0]} |
  {48'h0, {16{ARdEnW[3]}} & ADataMisoW[63:48]} |
  {48'h0, {16{ARdEnW[2]}} & ADataMisoW[47:32]} |
  {48'h0, {16{ARdEnW[1]}} & ADataMisoW[31:16]} |
  {48'h0, {16{ARdEnW[0]}} & ADataMisoW[15: 0]} |
  {56'h0, {8{ARdEnB[3]}} & ADataMisoB[31:24]} |
  {56'h0, {8{ARdEnB[2]}} & ADataMisoB[23:16]} |
  {56'h0, {8{ARdEnB[1]}} & ADataMisoB[15: 8]} |
  {56'h0, {8{ARdEnB[0]}} & ADataMisoB[ 7: 0]};
endmodule */

module PerifTimer #(parameter CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CDataLen-1:0] AIoMosi, input AIoWrEn,
  input [1:0] ASyncSel, input ASync1M, input ASync1K,
  input ATimerReset, input ACountEn, output [CDataLen-1:0] ATimerThis, output ATimerNZ
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
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] AIoMosi, output [7:0] AIoMiso, input [1:0] AIoWrRdEn,
  input [3:0] AGpioI, output [3:0] AGpioO, output [3:0] AGpioE
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
  input AClkH, input AResetHN, input AClkHEn,
  input [CDataLen-1:0] ADataI, input AWrEn,
  output [CDataLen-1:0] ADataO, input ARdEn,
  input AClr, output AHasData, output AHasSpace, output [15:0] ADataSize
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

 assign BHasData = BDataSizeNZ & ~ARdEn;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[CAddrLen];
 assign ADataSize = {{(15-CAddrLen){1'b0}}, BDataSize};
endmodule

module MsFifo4x #(parameter CDataLen = 8)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [(CDataLen-1):0] ADataI, input AWrEn,
  output [(CDataLen-1):0] ADataO, input ARdEn,
  input AClr, output AHasData, output AHasSpace, output [15:0] ADataSize
 );

 wire [(CDataLen*4-1):0] FData, BData;
 wire [2:0] FWrIdx, BWrIdx;
 wire [2:0] FRdIdx, BRdIdx;

 MsDffList #(.CRegLen(CDataLen*4+3+3)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BData, BWrIdx, BRdIdx}),
   .ADataO({FData, FWrIdx, FRdIdx})
  );

 wire [3:0] BWrIdxDec; MsDecAny #(.CAddrLen(2)) UWrIdxDec ( .AAddr(FWrIdx[1:0]), .ADataO(BWrIdxDec) );
 wire [(CDataLen*4-1):0] BWrMux; MsSpreadVect #(.CVectLen(4), .CSpreadLen(CDataLen)) UWrMux ( .ADataI({4{AWrEn}} & BWrIdxDec), .ADataO(BWrMux) );
 MsMux1b UData[(CDataLen*4-1):0] ( .ADataA(FData), .ADataB({4{ADataI}}), .ADataO(BData), .AAddr(BWrMux) );

 assign BWrIdx = AClr ? 3'h0 : FWrIdx + {2'h0, AWrEn};
 assign BRdIdx = AClr ? 3'h0 : FRdIdx + {2'h0, ARdEn};

 wire [2:0] BDataSize = FWrIdx - FRdIdx;

 MsMuxAny #(.CLenFinal(CDataLen), .CAddrLen(2)) UDataO ( .ADataI(FData), .AAddr(FRdIdx[1:0]), .ADataO(ADataO) );

 assign AHasData = ~(FWrIdx[2:0]==FRdIdx[2:0]);
 assign AHasSpace = (FWrIdx[2:0]==FRdIdx[2:0]) | (FWrIdx[1:0]!=FRdIdx[1:0]);
 assign ADataSize = {13'h0, BDataSize};
endmodule

module MsFifoDff #(parameter CAddrLen = 2, CDataLen = 8)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [(CDataLen-1):0] ADataI, input AWrEn,
  output [(CDataLen-1):0] ADataO, input ARdEn,
  input AClr, output AHasData, output AHasSpace, output [15:0] ADataSize
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

module MsFifo256b
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] ADataI, input AWrEn,
  output [7:0] ADataO, input ARdEn,
  output AHasData, output AHasSpace, output [15:0] ADataSize
 );

 wire [8:0] FWrIdx, BWrIdx;
 wire [8:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;


 MsDffList #(.CRegLen(9+9+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 RamSDP #(.CAddrLen(8), .CDataLen(8)) URam
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrWr(FWrIdx[7:0]), .AAddrRd(FRdIdx[7:0]), .AMosi(ADataI), .AMiso(ADataO), .AWrEn(AWrEn)
  );

 assign BWrIdx = FWrIdx + {8'h0, AWrEn};
 assign BRdIdx = FRdIdx + {8'h0, ARdEn};

 wire [8:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 //wire BAddrLE = FWrIdx[7:0]==FRdIdx[7:0];
 //wire BAddrHE = BAddrLE & (FWrIdx[8]==FRdIdx[8]);

 assign BHasData = BDataSizeNZ & ~ARdEn;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[8];
 assign ADataSize = {7'h0, BDataSize};
endmodule

module MsFifo256x #(parameter CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CDataLen-1:0] ADataI, input AWrEn,
  output [CDataLen-1:0] ADataO, input ARdEn,
  output AHasData, output AHasSpace, output [15:0] ADataSize
 );

 wire [8:0] FWrIdx, BWrIdx;
 wire [8:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;


 MsDffList #(.CRegLen(9+9+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 RamSDP #(.CAddrLen(8), .CDataLen(CDataLen)) URam
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrWr(FWrIdx[7:0]), .AAddrRd(FRdIdx[7:0]), .AMosi(ADataI), .AMiso(ADataO), .AWrEn(AWrEn)
  );

 assign BWrIdx = FWrIdx + {8'h0, AWrEn};
 assign BRdIdx = FRdIdx + {8'h0, ARdEn};

 wire [8:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 //wire BAddrLE = FWrIdx[7:0]==FRdIdx[7:0];
 //wire BAddrHE = BAddrLE & (FWrIdx[8]==FRdIdx[8]);

 assign BHasData = BDataSizeNZ & ~ARdEn;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[8];
 assign ADataSize = {7'h0, BDataSize};
endmodule

module MsFifo1K
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] ADataI, input AWrEn,
  output [7:0] ADataO, input ARdEn,
  output AHasData, output AHasSpace, output [15:0] ADataSize
 );

 wire [10:0] FWrIdx, BWrIdx;
 wire [10:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;

 MsDffList #(.CRegLen(11+11+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 RamSDP #(.CAddrLen(10), .CDataLen(8)) URam
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrWr(FWrIdx[9:0]), .AAddrRd(FRdIdx[9:0]), .AMosi(ADataI), .AMiso(ADataO), .AWrEn(AWrEn)
  );

 assign BWrIdx = FWrIdx + {10'h0, AWrEn};
 assign BRdIdx = FRdIdx + {10'h0, ARdEn};

 wire [10:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 //wire BAddrLE = FWrIdx[9:0]==FRdIdx[9:0];
 //wire BAddrHE = BAddrLE & (FWrIdx[10]==FRdIdx[10]);

 assign BHasData = BDataSizeNZ & ~ARdEn;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[10];
 assign ADataSize = {5'h0, BDataSize};
endmodule

module PerifFifoMem #(parameter CAddrLen = 3)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [(CAddrLen-1):0] AAddrI, input [7:0] ADataI, input AWrEn,
  input [(CAddrLen-1):0] AAddrO, output [7:0] ADataO
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

module PerifDataRol ( input [63:0] ADataI, input [7:0] AMaskI, input [2:0] AShCnt, output [63:0] ADataO, output [7:0] AMaskO );

 wire [63:0] BDataC = AShCnt[2] ? {ADataI[31:0], ADataI[63:32]} : ADataI;
 wire [63:0] BDataB = AShCnt[1] ? {BDataC[47:0], BDataC[63:48]} : BDataC;
 wire [63:0] BDataA = AShCnt[0] ? {BDataB[55:0], BDataB[63:56]} : BDataB;
 assign ADataO = BDataA;

 wire [7:0] BMaskC = AShCnt[2] ? {AMaskI[3:0], AMaskI[7:4]} : AMaskI;
 wire [7:0] BMaskB = AShCnt[1] ? {BMaskC[5:0], BMaskC[7:6]} : BMaskC;
 wire [7:0] BMaskA = AShCnt[0] ? {BMaskB[6:0], BMaskB[7:7]} : BMaskB;
 assign AMaskO = BMaskA;
endmodule

module PerifDataRor ( input [63:0] ADataI, input [2:0] AShCnt, output [63:0] ADataO );
 wire [63:0] BDataC = AShCnt[2] ? {ADataI[31:0], ADataI[63:32]} : ADataI;
 wire [63:0] BDataB = AShCnt[1] ? {BDataC[15:0], BDataC[63:16]} : BDataC;
 wire [63:0] BDataA = AShCnt[0] ? {BDataB[ 7:0], BDataB[63: 8]} : BDataB;
 assign ADataO = BDataA;
endmodule

// When different lines of FIFO are accessed, the address can overlap. So it will be +1 for certain lines
module PerifFifoAddrInc #(parameter CAddrLen=5) ( input [CAddrLen-1:0] AAddrI, input [7:0] AMask, output [8*(CAddrLen-3)-1:0] AAddrO );

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

module PerifFifoSend #(parameter CAddrLen = 5)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AResetSN,
  input [63:0] ADataI, input [3:0] AWrSize,
  output [7:0] ADataO, input ARdEn,
  output AHasData, output AHasSpace, output [15:0] AFreeSize
 );

 // Process
 wire [CAddrLen:0] FWrIdx, BWrIdx;
 wire [CAddrLen:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;

 MsDffList #(.CRegLen(1+CAddrLen+1+CAddrLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 wire [7:0] BWrMask; IoSizeToMask UWrMask ( .ASize(AWrSize), .AMask(BWrMask) );
 wire [63:0] BDataRol; wire [7:0] BMaskRol; PerifDataRol UDataRol ( .ADataI(ADataI), .AMaskI(BWrMask), .AShCnt(FWrIdx[2:0]), .ADataO(BDataRol), .AMaskO(BMaskRol) );
 wire [8*(CAddrLen-3)-1:0] BWrAddr; PerifFifoAddrInc #(.CAddrLen(CAddrLen)) UWrAddr ( .AAddrI(FWrIdx[CAddrLen-1:0]), .AMask(BWrMask), .AAddrO(BWrAddr) );

 wire [63:0] BDataMem;
 PerifFifoMem #(.CAddrLen(CAddrLen-3)) UFifoMem[7:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrI(BWrAddr), .ADataI(BDataRol), .AWrEn(BMaskRol),
   .AAddrO(FRdIdx[CAddrLen-1:3]), .ADataO(BDataMem)
  );

 wire [63:0] BDataRor; PerifDataRor UDataRor ( .ADataI(BDataMem), .AShCnt(FRdIdx[2:0]), .ADataO(BDataRor) );

 assign BWrIdx = AResetSN ? FWrIdx + {{(CAddrLen-3){1'b0}}, AWrSize} : {(CAddrLen+1){1'b0}};
 assign BRdIdx = AResetSN ? FRdIdx + {{CAddrLen{1'b0}}, ARdEn} : {(CAddrLen+1){1'b0}};

 wire [CAddrLen:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 assign ADataO = BDataRor[7:0];

 assign BHasData = BDataSizeNZ & ~ARdEn;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[CAddrLen];
 assign AFreeSize = {{(15-CAddrLen){1'b0}}, {1'b1, {(CAddrLen-1){1'b0}}}-BDataSize};
endmodule

module PerifFifoRecv #(parameter CAddrLen = 5)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AResetSN,
  input [7:0] ADataI, input AWrEn,
  output [63:0] ADataO, input [3:0] ARdSize,
  output AHasData, output AHasSpace, output [15:0] AFillSize
 );

 // Process
 wire [CAddrLen:0] FWrIdx, BWrIdx;
 wire [CAddrLen:0] FRdIdx, BRdIdx;
 wire FHasData, BHasData;

 MsDffList #(.CRegLen(1+CAddrLen+1+CAddrLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BWrIdx, BRdIdx, BHasData}),
   .ADataO({FWrIdx, FRdIdx, FHasData})
  );

 wire [63:0] BDataRol; wire [7:0] BMaskRol; PerifDataRol UDataRol ( .ADataI({56'h0, ADataI}), .AMaskI(8'h1), .AShCnt(FWrIdx[2:0]), .ADataO(BDataRol), .AMaskO(BMaskRol) );

 wire [7:0] BRdMask; IoSizeToMask URdMask ( .ASize(ARdSize), .AMask(BRdMask) );
 wire [8*(CAddrLen-3)-1:0] BRdAddr; PerifFifoAddrInc #(.CAddrLen(CAddrLen)) UWrAddr ( .AAddrI(FRdIdx[CAddrLen-1:0]), .AMask(BRdMask), .AAddrO(BRdAddr) );

 wire [63:0] BDataMem;
 PerifFifoMem #(.CAddrLen(CAddrLen-3)) UFifoMem[7:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AAddrI(FWrIdx[CAddrLen-1:3]), .ADataI(BDataRol), .AWrEn(BMaskRol),
   .AAddrO(BRdAddr), .ADataO(BDataMem)
  );

 wire [63:0] BDataRor; PerifDataRor UDataRor ( .ADataI(BDataMem), .AShCnt(FRdIdx[2:0]), .ADataO(BDataRor) );
 wire [63:0] BMaskRor; MsSpreadVect #(.CVectLen(8), .CSpreadLen(8)) UMaskRor ( .ADataI(BRdMask), .ADataO(BMaskRor) );

 assign BWrIdx = AResetSN ? FWrIdx + {{CAddrLen{1'b0}}, AWrEn} : {(CAddrLen+1){1'b0}};
 assign BRdIdx = AResetSN ? FRdIdx + {{(CAddrLen-3){1'b0}}, ARdSize} : {(CAddrLen+1){1'b0}};

 wire [CAddrLen:0] BDataSize = FWrIdx - FRdIdx;
 wire BDataSizeNZ = |BDataSize;

 assign ADataO = BDataRor & BMaskRor;

 assign BHasData = BDataSizeNZ;

 assign AHasData  = FHasData;
 assign AHasSpace =  ~BDataSize[CAddrLen];
 assign AFillSize = {{(15-CAddrLen){1'b0}}, BDataSize};
endmodule

module PerifCfgBlock #(parameter CRegLen=8, CRegCnt=6)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [(CRegLen-1):0] ADataI, input AWrEn, input AAnyOtherAccess,
  output [(CRegLen*CRegCnt-1):0] ADataO
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


