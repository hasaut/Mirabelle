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
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, AIoRdSize, output wire AIoAddrAck, AIoAddrErr, AIoBusy,
  output wire ARsuReady, output wire [31:0] ARsuBootAddr,
  output wire [7:0] ATest
 );

 // IobCtrl    = +0; // WR: 7xRFU Reconfig
 // IobOper    = +1; // WR: 6xRFU WrEn RdEn | RD: 7xRFU Busy
 // IobAddr    = +2; // WR/RD: Addr[4:0]
 // IodData    = +2;

 localparam CStLen      = 9;
 localparam IStReady    = 8;
 localparam IStRdAddrC  = 7;
 localparam IStRdAddrB  = 6;
 localparam IStRdAddrA  = 5;
 localparam IStSetMode  = 4;
 localparam IStRdStateC = 3;
 localparam IStRdStateB = 2;
 localparam IStRdStateA = 1;
 localparam IStStart    = 0;

 // Local variables
 wire [1:0] FRsuCtrl, BRsuCtrl;
 wire [2:0] FRsuParam, BRsuParam;
 wire [1:0] FRsuSrc, BRsuSrc;
 wire [7:0] FRsuResetTimer, BRsuResetTimer;
 wire FRsuBusy, BRsuBusy;
 wire [1:0] FRsuOper, BRsuOper;
 wire [31:0] FRsuMosi, BRsuMosi;
 wire [CStLen-1:0] FState, BState;
 wire [1:0] FRsuMode, BRsuMode;
 wire [29:0] FRsuAddr, BRsuAddr;
 wire FRsuReady, BRsuReady;

 MsDffList #(.CRegLen(2+3+2+8+1+2+32+CStLen+2+30+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BRsuCtrl, BRsuParam, BRsuSrc, BRsuResetTimer, BRsuBusy, BRsuOper, BRsuMosi, BState, BRsuMode, BRsuAddr, BRsuReady}),
   .ADataO({FRsuCtrl, FRsuParam, FRsuSrc, FRsuResetTimer, FRsuBusy, FRsuOper, FRsuMosi, FState, FRsuMode, FRsuAddr, FRsuReady})
  );

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00440076)) UIntf
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
 wire [CStLen-1:0] BGo, BStay;

 // Interface WR
 assign BRsuCtrl  = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[1:0] : FRsuCtrl;
 assign BRsuOper  = BIoAccess[IoSizeB+IoOperW+1] ? AIoMosi[1:0] : {1'b0, BGo[IStRdStateA] | BGo[IStRdAddrA]};
 assign BRsuMosi  = BIoAccess[IoSizeD+IoOperW+2] ? AIoMosi[31:0] : FRsuMosi;

 assign {BRsuSrc, BRsuParam} =
  (BGo[IStRdStateA] ? 5'h0 : 5'h0) |
  (BGo[IStRdAddrA] ? {(FRsuMode==2'h0) ? 2'h0 : 2'h2, 3'h4} : 5'h0) |
  (BIoAccess[IoSizeB+IoOperW+2] ? {AIoMosi[5:4], AIoMosi[2:0]} : 5'h0) |
  ((BGo[IStRdStateA] | BGo[IStRdAddrA] | BIoAccess[IoSizeB+IoOperW+2]) ? 5'h0 : {FRsuSrc, FRsuParam});

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, 7'h0, FRsuBusy} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, 2'h0, FRsuSrc, 1'b0, FRsuParam} : 64'h0) |
   (BIoAccess[IoSizeD+IoOperR+2] ? {32'h0, BRsuMiso} : 64'h0);

 // FSM
 wire BStateNZ = |FState;

 assign BGo[IStStart]    = ~BStateNZ & ~FRsuReady;           assign BStay[IStStart]    = 1'b0;
 assign BGo[IStRdStateA] = FState[IStStart];                 assign BStay[IStRdStateA] = 1'b0;
 assign BGo[IStRdStateB] = FState[IStRdStateA];              assign BStay[IStRdStateB] = FState[IStRdStateB] & BRsuBusy;
 assign BGo[IStRdStateC] = FState[IStRdStateB] & ~BRsuBusy;  assign BStay[IStRdStateC] = 1'b0;
 assign BGo[IStSetMode]  = FState[IStRdStateC];              assign BStay[IStSetMode]  = 1'b0;
 assign BGo[IStRdAddrA]  = FState[IStSetMode];               assign BStay[IStRdAddrA]  = 1'b0;
 assign BGo[IStRdAddrB]  = FState[IStRdAddrA];               assign BStay[IStRdAddrB]  = FState[IStRdAddrB] & BRsuBusy;
 assign BGo[IStRdAddrC]  = FState[IStRdAddrB] & ~BRsuBusy;   assign BStay[IStRdAddrC]  = 1'b0;
 assign BGo[IStReady]    = FState[IStRdAddrC];               assign BStay[IStReady]    = 1'b0;

 assign BState = BGo | BStay;

 assign BRsuMode  = FState[IStRdStateC] ? BRsuMiso[1:0] : FRsuMode;
 assign BRsuAddr  = FState[IStRdAddrC]  ? BRsuMiso[31:2] : FRsuAddr;
 assign BRsuReady = FState[IStReady] | FRsuReady;

 // Process
 assign BRsuResetTimer = FRsuResetTimer + 8'h1;

 IpSysUpdate_MT25QL01G USysUpdate
  (
   .clock(AClkH), .reset(~AResetHN),
   .busy(BRsuBusy),
   .data_in(FRsuMosi),
   .data_out(BRsuMiso),
   .param(FRsuParam),
   .read_param(FRsuOper[0]),
   .read_source(FRsuSrc),
   .reconfig(FRsuCtrl[0]),
   .reset_timer(FRsuResetTimer[7]),
   .write_param(FRsuOper[1])
  );

 assign AIoBusy = 1'b0;
 assign ARsuReady = FRsuReady;
 assign ARsuBootAddr = {2'h0, FRsuAddr};

 assign ATest = {FRsuOper[0], FRsuBusy, FRsuSrc, FRsuMode==2'h3, FRsuParam};
endmodule



