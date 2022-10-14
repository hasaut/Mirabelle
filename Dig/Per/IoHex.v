module IoHex #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  output [7:0] ALedY, output [2:0] ALedRgb, output ALedPwr,
  output AUnused
 );

 wire [2:0] FLedRgb, BLedRgb;
 wire [15:0] FDataHex, BDataHex;
 wire [7:0] FLedY, BLedY;
 wire FLedPwr, BLedPwr;

 MsDffList #(.CRegLen(3+16+8+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BLedRgb, BDataHex, BLedY, BLedPwr}),
   .ADataO({FLedRgb, FDataHex, FLedY, FLedPwr})
  );

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00001177)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 assign BDataHex = BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[15:0] : FDataHex;
 assign BLedY    = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[7:0] : FLedY;
 assign BLedRgb  = BIoAccess[IoSizeB+IoOperW+1] ? AIoMosi[2:0] : FLedRgb;
 assign BLedPwr  = BIoAccess[IoSizeB+IoOperW+2] ? AIoMosi[0]   : FLedPwr;

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+0] ? {48'h0, FDataHex} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FLedY} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, 5'h0, FLedRgb} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, 7'h0, FLedPwr} : 64'h0);

 assign ALedY = FLedY;
 assign ALedRgb = FLedRgb;
 assign ALedPwr = FLedPwr;

 assign AUnused = |{AIoMosi};
endmodule


