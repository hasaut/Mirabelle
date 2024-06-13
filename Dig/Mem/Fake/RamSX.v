module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CAddrLen-1:0] AAddr, input wire [CDataLen-1:0] AMosi, output wire [CDataLen-1:0] AMiso, input wire AWrEn, input wire ARdEn
 );

 wire [CDataLen-1:0] FSReg, BSReg;
 wire FRdEn, BRdEn;

 MsDffList #(.CRegLen(CDataLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSReg, BRdEn}),
   .ADataO({FSReg, FRdEn})
  );

 assign BRdEn = ARdEn;
 assign BSReg = FSReg ^ {FSReg[CDataLen-2:0], ^{AAddr, AMosi, AWrEn, ARdEn}};

 assign AMiso = {CDataLen{FRdEn}} & FSReg;

endmodule

module RamSDP #(parameter CAddrLen=13, CDataLen=128)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CAddrLen-1:0] AAddrWr, AAddrRd, input wire [CDataLen-1:0] AMosi, output wire [CDataLen-1:0] AMiso, input wire AWrEn
 );

 localparam ZData = {CDataLen{1'b0}};

 assign AMiso = ZData;
endmodule
