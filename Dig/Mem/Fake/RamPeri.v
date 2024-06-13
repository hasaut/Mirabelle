module Ram8a8d2
 (
  input wire AClkH, AResetB, AClkHEn,
  input wire [7:0] AAddrWr, AAddrRd,
  input wire [7:0] AMosi, output wire [7:0] AMiso,
  input wire AWrEn
 );

 wire [8-1:0] FSReg, BSReg;

 MsDffList #(.CRegLen(8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSReg}),
   .ADataO({FSReg})
  );

 assign BSReg = FSReg ^ {FSReg[8-2:0], ^{AAddrWr, AAddrRd, AMosi, AWrEn}};

 assign AMiso = FSReg;

endmodule


