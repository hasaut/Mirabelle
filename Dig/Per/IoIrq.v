module IoIrq16A #(parameter CAddrBase=16'h0000, CIrqCnt=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, AIoRdSize, output AIoAddrAck, AIoAddrErr,
  input [CIrqCnt-1:0] AIrqI, output [CIrqCnt-1:0] AIrqO,
  output [7:0] ATest
 );

 // Interface
 // IowIrqEn  +0 ; // W: IrqEn
 //              ; // R: IrqEn

 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00001100)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local variables
 wire [CIrqCnt-1:0] FIrqEn, BIrqEn;

 MsDffList #(.CRegLen(16)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BIrqEn}),
   .ADataO({FIrqEn})
  );

 // Aliases
 assign BIrqEn = BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[CIrqCnt-1:0] : FIrqEn;

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+0] ? {{(64-CIrqCnt){1'b0}}, FIrqEn} : 64'h0);

 // External
 assign AIrqO = AIrqI & FIrqEn;
 assign ATest = 8'h0;
endmodule


