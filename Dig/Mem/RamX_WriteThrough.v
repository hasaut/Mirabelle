module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CAddrLen-1:0] AAddr, input wire [CDataLen-1:0] AMosi, output wire [CDataLen-1:0] AMiso, input wire AWrEn, input wire ARdEn
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
  if (AWrEn)
   begin
   FMem[AAddr]<=AMosi;
   BMiso<=AMosi;
   end
  else BMiso<=FMem[AAddr];
  end

 assign AMiso = {CDataLen{FRdEn}} & BMiso;

endmodule

// ****************************************************************
// *** Partial memory (i.e. not the full address space is used) ***
// ****************************************************************
module RamSX_Part #(parameter CAddrLen=13, CDataLen=128, CMemSize=2**CAddrLen)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CAddrLen-1:0] AAddr, input wire [CDataLen-1:0] AMosi, output wire [CDataLen-1:0] AMiso, input wire AWrEn, input wire ARdEn
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
  if (AWrEn)
   begin
   FMem[AAddr]<=AMosi;
   BMiso<=AMosi;
   end
  else BMiso<=FMem[AAddr];
  end

 assign AMiso = {CDataLen{FRdEn}} & BMiso;

endmodule

module RamDX #(parameter CAddrLen=11, CDataLen=8)
 (
  input wire AClkA, input wire AResetAN, input wire AClkAEn,
  input wire [CAddrLen-1:0] AAddrA, input wire [CDataLen-1:0] AMosiA, output wire [CDataLen-1:0] AMisoA, input wire AWrEnA, input wire ARdEnA,
  input wire AClkB, input wire AResetBN, input wire AClkBEn,
  input wire [CAddrLen-1:0] AAddrB, input wire [CDataLen-1:0] AMosiB, output wire [CDataLen-1:0] AMisoB, input wire AWrEnB, input wire ARdEnB
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
  if (AWrEnA)
   begin
   FMem[AAddrA]<=AMosiA;
   BMisoA<=AMosiA;
   end
  else BMisoA<=FMem[AAddrA];
  end

 always @(posedge AClkB)
  begin
  if (AWrEnB)
   begin
   FMem[AAddrB]<=AMosiB;
   BMisoB<=AMosiB;
   end
  else BMisoB<=FMem[AAddrB];
  end

 assign AMisoA = {CDataLen{FRdEnA}} & BMisoA;
 assign AMisoB = {CDataLen{FRdEnB}} & BMisoB;

endmodule

module RamSDP #(parameter CAddrLen=13, CDataLen=128)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CAddrLen-1:0] AAddrWr, AAddrRd, input wire [CDataLen-1:0] AMosi, output wire [CDataLen-1:0] AMiso, input wire AWrEn
 );

 localparam ZData = {CDataLen{1'b0}};

 reg [CDataLen-1:0] BMiso;
 reg [CDataLen-1:0] FMem[2**CAddrLen-1:0];

 always @(posedge AClkH)
  begin
  if (AWrEn)
   begin
   FMem[AAddrWr]<=AMosi;
   BMiso<=AMosi;
   end
  else BMiso<=FMem[AAddrRd];
  end

 assign AMiso = BMiso;
endmodule

