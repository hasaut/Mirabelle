module IoTestMux #(parameter CAddrBase=16'h0000, CAddrLen=12)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  input wire [511:0] ATestIn, output wire [15:0] ATest16p
 );

 // Interface
 // IobSMux = +1; // WR: 16x 64-in mux for each 8x-bit vector

 // Implementation
 // Domain H
 wire  [2:0] FChSelIdx, BChSelIdx;

 MsDffList #(.CRegLen(3)) ULocalVarsH
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BChSelIdx}),
   .ADataO({FChSelIdx})
  );

 // *** IO access ***
 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00000011)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 assign AIoMiso = 64'h0;

 wire BChWrEn = BIoAccess[IoSizeB+IoOperW+1];
 wire BChIdxR = ~BChWrEn & (|BIoAccess);

 assign BChSelIdx = BChIdxR ? 3'h0 : FChSelIdx+{2'h0, BChWrEn};

 // *** Channel Mux ***
 // 512 input bits give 512/8=64 8x-bit input channels
 // Scope can sample 128x signals simultaneously. This gives 128/8=16 multiplexers.
 // Each mux we must configure independently
 wire [7:0] BChSelDec; MsDec3x8a UChSelDec ( .ADataI(FChSelIdx), .ADataO(BChSelDec) );

 
 TestMux UTestMux[1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASelIdx(AIoMosi[5:0]), .ASelWrEn({2{BChWrEn}} & BChSelDec[1:0]),
   .ADataIn(ATestIn), .ADataOut(ATest16p),
   .AMuxIdx()
  );

endmodule

module TestMux
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [5:0] ASelIdx, input wire ASelWrEn,
  input wire [511:0] ADataIn, output wire [7:0] ADataOut,
  output wire [7:0] AMuxIdx // Saved value; needed to report MUX state to the PC to make nice plot
 );

 // Implementation
 wire [5:0] FSelIdx, BSelIdx;

 MsDffList #(.CRegLen(6)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(BSelIdx),
   .ADataO(FSelIdx)
  );

 assign BSelIdx = ASelWrEn ? ASelIdx : FSelIdx;

 wire [7:0] LDataIn [63:0];
 assign
  {
                                                                                 LDataIn[63], LDataIn[62], LDataIn[61], LDataIn[60],
   LDataIn[59], LDataIn[58], LDataIn[57], LDataIn[56], LDataIn[55], LDataIn[54], LDataIn[53], LDataIn[52], LDataIn[51], LDataIn[50],
   LDataIn[49], LDataIn[48], LDataIn[47], LDataIn[46], LDataIn[45], LDataIn[44], LDataIn[43], LDataIn[42], LDataIn[41], LDataIn[40],
   LDataIn[39], LDataIn[38], LDataIn[37], LDataIn[36], LDataIn[35], LDataIn[34], LDataIn[33], LDataIn[32], LDataIn[31], LDataIn[30],
   LDataIn[29], LDataIn[28], LDataIn[27], LDataIn[26], LDataIn[25], LDataIn[24], LDataIn[23], LDataIn[22], LDataIn[21], LDataIn[20],
   LDataIn[19], LDataIn[18], LDataIn[17], LDataIn[16], LDataIn[15], LDataIn[14], LDataIn[13], LDataIn[12], LDataIn[11], LDataIn[10],
   LDataIn[ 9], LDataIn[ 8], LDataIn[ 7], LDataIn[ 6], LDataIn[ 5], LDataIn[ 4], LDataIn[ 3], LDataIn[ 2], LDataIn[ 1], LDataIn[ 0]
  } = ADataIn;
 assign ADataOut = LDataIn[FSelIdx];
 assign AMuxIdx = {2'h0, FSelIdx};
endmodule

// ALM   M10K
// 702.2 8
