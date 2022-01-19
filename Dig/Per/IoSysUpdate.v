module IoSysUpdateL #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, output AIoAddrErr, output AIoBusy,
  output [7:0] ATest
 );

 // IobCtrl = +0; // WR: 7xRFU Reconfig
 // IobAddr = +2; // WR/RD: Addr[4:0]
 // IodData = +2;

 // Local variables
 wire [1:0] FRsuCtrl, BRsuCtrl;
 wire [4:0] FRsuAddr, BRsuAddr;
 wire [7:0] FRsuResetTimer, BRsuResetTimer;

 MsDffList #(.CRegLen(2+5+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BRsuCtrl, BRsuAddr, BRsuResetTimer}),
   .ADataO({FRsuCtrl, FRsuAddr, FRsuResetTimer})
  );

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00444410)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 // Common
 wire [28:0] BRsuMiso;
 wire BRsuBusy;

 // Interface WR
 assign BRsuCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[1:0] : FRsuCtrl;
 assign BRsuAddr = BIoAccess[IoSizeB+IoOperW+2] ? AIoMosi[4:0] : FRsuAddr;

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, 3'h0, FRsuAddr} : 64'h0) |
   (BIoAccess[IoSizeD+IoOperR+2] ? {32'h0, 3'h0, BRsuMiso} : 64'h0);

 // Process
 assign BRsuResetTimer = FRsuResetTimer + 8'h1;

 IpSysUpdate_S25FL128 USysUpdate
  (
   .clock(AClkH), .reset(~AResetHN),
   .busy(BRsuBusy),
   .data_in(AIoMosi[23:0]),
   .data_out(BRsuMiso),
   .param(FRsuAddr[2:0]),
   .read_param(BIoAccess[IoSizeD+IoOperR+2]),
   .read_source(FRsuAddr[4:3]),
   .reconfig(FRsuCtrl[0]),
   .reset_timer(FRsuResetTimer[7]),
   .write_param(BIoAccess[IoSizeD+IoOperW+2])
  );

 assign AIoBusy = BRsuBusy;

 assign ATest = {AIoBusy, 5'h0, FRsuCtrl};
endmodule

module IoSysUpdateG #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, output AIoAddrErr, output AIoBusy,
  output [7:0] ATest
 );

 // IobCtrl = +0; // WR: 7xRFU Reconfig
 // IobAddr = +2; // WR/RD: Addr[4:0]
 // IodData = +2;

 // Local variables
 wire [1:0] FRsuCtrl, BRsuCtrl;
 wire [4:0] FRsuAddr, BRsuAddr;
 wire [7:0] FRsuResetTimer, BRsuResetTimer;

 MsDffList #(.CRegLen(2+5+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BRsuCtrl, BRsuAddr, BRsuResetTimer}),
   .ADataO({FRsuCtrl, FRsuAddr, FRsuResetTimer})
  );

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00444410)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 // Common
 wire [31:0] BRsuMiso;
 wire BRsuBusy;

 // Interface WR
 assign BRsuCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[1:0] : FRsuCtrl;
 assign BRsuAddr = BIoAccess[IoSizeB+IoOperW+2] ? AIoMosi[4:0] : FRsuAddr;

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, 3'h0, FRsuAddr} : 64'h0) |
   (BIoAccess[IoSizeD+IoOperR+2] ? {32'h0, BRsuMiso} : 64'h0);

 // Process
 assign BRsuResetTimer = FRsuResetTimer + 8'h1;

 IpSysUpdate_MT25QL01G USysUpdate
  (
   .clock(AClkH), .reset(~AResetHN),
   .busy(BRsuBusy),
   .data_in(AIoMosi[31:0]),
   .data_out(BRsuMiso),
   .param(FRsuAddr[2:0]),
   .read_param(BIoAccess[IoSizeD+IoOperR+2]),
   .read_source(FRsuAddr[4:3]),
   .reconfig(FRsuCtrl[0]),
   .reset_timer(FRsuResetTimer[7]),
   .write_param(BIoAccess[IoSizeD+IoOperW+2])
  );

 assign AIoBusy = BRsuBusy;

 assign ATest = {AIoBusy, 5'h0, FRsuCtrl};
endmodule



