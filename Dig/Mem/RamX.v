module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddr, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn, input ARdEn
 );

 localparam ZData = {CDataLen{1'b0}};

 reg [CDataLen-1:0] BMiso;
 wire FRdEn, BRdEn;

 MsDffList #(.CRegLen(1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BRdEn}),
   .ADataO({FRdEn})
  );

 assign BRdEn = ARdEn;

 reg [CDataLen-1:0] FMem[2**CAddrLen-1:0];

 always @ (posedge AClkH)
  begin
  BMiso<=FMem[AAddr];
  if (AWrEn) FMem[AAddr] <= AMosi;
  end

 assign AMiso = {CDataLen{FRdEn}} & BMiso;

endmodule

// ****************************************************************
// *** Partial memory (i.e. not the full address space is used) ***
// ****************************************************************
module RamSX_Part #(parameter CAddrLen=13, CDataLen=128, CMemSize=2**CAddrLen)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddr, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn, input ARdEn
 );

 localparam ZData = {CDataLen{1'b0}};

 reg [CDataLen-1:0] BMiso;
 wire FRdEn, BRdEn;

 MsDffList #(.CRegLen(1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BRdEn}),
   .ADataO({FRdEn})
  );

 assign BRdEn = ARdEn;

 reg [CDataLen-1:0] FMem[CMemSize-1:0];

 always @ (posedge AClkH)
  begin
  BMiso<=FMem[AAddr];
  if (AWrEn) FMem[AAddr] <= AMosi;
  end

 assign AMiso = {CDataLen{FRdEn}} & BMiso;

endmodule

module RamDX #(parameter CAddrLen=11, CDataLen=8)
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [CAddrLen-1:0] AAddrA, input [CDataLen-1:0] AMosiA, output [CDataLen-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [CAddrLen-1:0] AAddrB, input [CDataLen-1:0] AMosiB, output [CDataLen-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {CDataLen{1'b0}};

 reg [CDataLen-1:0] BMisoA, BMisoB;
 wire FRdEnA, BRdEnA;
 wire FRdEnB, BRdEnB;

 MsDffList #(.CRegLen(1)) ULocalVarsA
  (
   .AClkH(AClkA), .AResetHN(AResetAN), .AClkHEn(AClkAEn), 
   .ADataI({BRdEnA}),
   .ADataO({FRdEnA})
  );

 MsDffList #(.CRegLen(1)) ULocalVarsB
  (
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkAEn), 
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 reg [CDataLen-1:0] FMem[2**CAddrLen-1:0] /* synthesis ramstyle = "no_rw_check" */;

 always @(posedge AClkA)
  begin
  BMisoA<=FMem[AAddrA];
  if (AWrEnA) FMem[AAddrA]<=AMosiA;
  end

 always @(posedge AClkB)
  begin
  BMisoB<=FMem[AAddrB];
  if (AWrEnB) FMem[AAddrB]<=AMosiB;
  end

 assign AMisoA = {CDataLen{FRdEnA}} & BMisoA;
 assign AMisoB = {CDataLen{FRdEnB}} & BMisoB;

endmodule

module RamSDP #(parameter CAddrLen=13, CDataLen=128)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddrWr, AAddrRd, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn
 );

 localparam ZData = {CDataLen{1'b0}};

 reg [CDataLen-1:0] BMiso;
 reg [CDataLen-1:0] FMem[2**CAddrLen-1:0];

 always @(posedge AClkH)
  begin
  BMiso<=FMem[AAddrRd];
  if (AWrEn) FMem[AAddrWr]<=AMosi;
  end

 assign AMiso = BMiso;
endmodule

