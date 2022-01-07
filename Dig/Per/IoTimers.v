// Most simple Timer, 16-bit, 1 compare
// counts up
// can generate IRQ

module IoTimer16A #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, AIoAddrErr,
  input ASync1M, input ASync1K, output AIrq,
  output [7:0] ATest
 );

 // Interface
 // IobCtrl   +0 ; // W: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU IrqAEn
 //              ; // R: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU CmpA
 // IowCmpA   +1
 // IobIrqR   +3 ; // Write bit 0 to reset IrqA
 // IowThis   +3 ; // RD: FCounter

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00002A91)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local variables
 wire  [7:0] FCtrl, BCtrl;
 wire [15:0] FCmpA, BCmpA;

 wire [15:0] FCounter, BCounter;
 wire        FCmpRes, BCmpRes;
 wire        FIrq, BIrq;
 wire        FCmpReset, BCmpReset;

 MsDffList #(.CRegLen(8+16+16+1+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrl, BCmpA, BCounter, BCmpRes, BIrq, BCmpReset}),
   .ADataO({FCtrl, FCmpA, FCounter, FCmpRes, FIrq, FCmpReset})
  );

 // Aliases
 wire [1:0] LTimerSrc = FCtrl[5:4];

 assign BCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[ 7:0] : FCtrl;
 assign BCmpA = BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FCmpA;

 assign BCmpReset = BIoAccess[IoSizeB+IoOperW+3] & AIoMosi[0];

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, FCounter} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+1] ? {48'h0, FCmpA} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FCtrl[7:2], 1'b0, FCmpRes} : 64'h0);

 // Functional
 wire BTimerSrcNZ = |LTimerSrc;
 wire [3:0] BSyncSel; MsDec2x4a USyncSel ( .ADataI(LTimerSrc), .ADataO(BSyncSel) );

 wire BIncEn = |(BSyncSel[3:1] & {1'b1, ASync1M, ASync1K});
 wire BCmpResA = (FCounter==FCmpA) & BIncEn;
 assign BCmpRes  = BCmpResA | (FCmpRes & ~FCmpReset);
 assign BIrq     = BCmpRes & FCtrl[0] & ~FCmpReset;

 assign BCounter = (BSyncSel[0] | BCmpResA) ? 16'h0 : FCounter + {15'h0, BIncEn};

 assign ATest = {AClkH, BTimerSrcNZ, BIncEn, BCmpResA, BCmpRes, FIrq, FCmpReset, FCmpRes};

 // External
 assign AIrq = FIrq;
endmodule

// Most simple Timer, 32-bit, 1 compare
// counts up
// can generate IRQ

module IoTimer32A #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, AIoAddrErr,
  input ASync1M, input ASync1K, output AIrq,
  output [7:0] ATest
 );

 // Interface
 // IobCtrl   +0 ; // W: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU IrqAEn
 //              ; // R: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU CmpA
 // IodCmpA   +1
 // IobIrqR   +3 ; // Write bit 0 to reset IrqA
 // IodThis   +3 ; // RD: FCounter

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h002A0091)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local variables
 wire  [7:0] FCtrl, BCtrl;
 wire [31:0] FCmpA, BCmpA;

 wire [31:0] FCounter, BCounter;
 wire        FCmpRes, BCmpRes;
 wire        FIrq, BIrq;

 MsDffList #(.CRegLen(8+32+32+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrl, BCmpA, BCounter, BCmpRes, BIrq}),
   .ADataO({FCtrl, FCmpA, FCounter, FCmpRes, FIrq})
  );

 // Aliases
 wire [1:0] LTimerSrc = FCtrl[5:4];

 assign BCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[ 7:0] : FCtrl;
 assign BCmpA = BIoAccess[IoSizeD+IoOperW+1] ? AIoMosi[31:0] : FCmpA;

 wire BCmpReset = BIoAccess[IoSizeB+IoOperW+3] & AIoMosi[0];

 assign AIoMiso =
  (BIoAccess[IoSizeD+IoOperR+3] ? {32'h0, FCounter} : 64'h0) |
  (BIoAccess[IoSizeD+IoOperR+1] ? {32'h0, FCmpA} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FCtrl[7:2], 1'b0, FCmpRes} : 64'h0);

 // Functional
 wire BTimerSrcNZ = |LTimerSrc;
 wire [3:0] BSyncSel; MsDec2x4a USyncSel ( .ADataI(LTimerSrc), .ADataO(BSyncSel) );

 wire BIncEn = |(BSyncSel[3:1] & {1'b1, ASync1M, ASync1K});
 wire BCmpResA = (FCounter==FCmpA) & BIncEn;
 assign BCmpRes  = BCmpResA | (FCmpRes & ~BCmpReset);
 assign BIrq     = BCmpRes & FCtrl[0] & ~BCmpReset;

 assign BCounter = (BSyncSel[0] | BCmpResA) ? 32'h0 : FCounter + {31'h0, BIncEn};

 assign ATest = {AClkH, BTimerSrcNZ, BIncEn, BCmpResA, BCmpRes, BIrq, FIrq, FCmpRes};

 // External
 assign AIrq = FIrq;
endmodule


