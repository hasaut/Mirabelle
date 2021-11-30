module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddr, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn, input ARdEn
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

