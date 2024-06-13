module FtdiIoDec
 (
  input wire AClkS, input wire AClkL, input wire AResetN,
  input wire ADataI,
  output wire [7:0] ADataO
 );

 wire [7:0] FSReg, BSReg;
 MsDffList #(.CRegLen(8)) USReg
  (
   .AClkH(AClkS), .AResetHN(AResetN), .AClkHEn(1'b1),  
   .ADataI(BSReg),
   .ADataO(FSReg)
  );

 assign BSReg = {FSReg[6:0], ADataI};

 wire [7:0] FLReg, BLReg;
 MsDffList #(.CRegLen(8)) ULReg
  (
   .AClkH(AClkL), .AResetHN(AResetN), .AClkHEn(1'b1),  
   .ADataI(BLReg),
   .ADataO(FLReg)
  );

 assign BLReg = FSReg;

 assign ADataO = FLReg;
endmodule

