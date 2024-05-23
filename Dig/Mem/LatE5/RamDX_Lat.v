module RamDX_6a4d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [6-1:0] AAddrA, input [4-1:0] AMosiA, output [4-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [6-1:0] AAddrB, input [4-1:0] AMosiB, output [4-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {4{1'b0}};

 wire [4-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_6a4d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {4{ARdEnA}} & BMisoA;
 assign AMisoB = {4{FRdEnB}} & BMisoB;

endmodule

module RamDX_5a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [5-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [5-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_5a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_6a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [6-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [6-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_6a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_7a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [7-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [7-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
 wire FRdEnA, BRdEnA;
 wire FRdEnB, BRdEnB;

 MsDffList #(.CRegLen(1)) ULocalVarsA
  (
   .AClkH(AClkA), .AResetHN(AResetAN), .AClkHEn(AClkAEn),
   .ADataI({BRdEnA}),
   .ADataO({FRdEnA})
  );

 reg FDataDbgA, FDataDbgC;
 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  FDataDbgA<=1'b0;
  FDataDbgC<=1'b0;
  end
 else if (AClkAEn)
  begin
  FDataDbgA<=BRdEnA;
  FDataDbgC<=1'b1;
  end


 wire FDataDbgB;
 MsDffList #(.CRegLen(2)) ULocalVarsB
  (
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB, BRdEnA}),
   .ADataO({FRdEnB, FDataDbgB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_7a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

 //assign ATest = {AClkA, AResetAN, BRdEnA, FRdEnA, BMisoA[3:0]};

endmodule

module RamDX_8a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [8-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [8-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_8a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_9a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [9-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [9-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_9a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_10a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [10-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [10-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_10a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_11a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [11-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [11-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_11a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule

module RamDX_12a8d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [12-1:0] AAddrA, input [8-1:0] AMosiA, output [8-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [12-1:0] AAddrB, input [8-1:0] AMosiB, output [8-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {8{1'b0}};

 wire [8-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_12a8d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {8{ARdEnA}} & BMisoA;
 assign AMisoB = {8{FRdEnB}} & BMisoB;

endmodule


module RamDX_5a64d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [5-1:0] AAddrA, input [64-1:0] AMosiA, output [64-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [5-1:0] AAddrB, input [64-1:0] AMosiB, output [64-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {64{1'b0}};

 wire [64-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_5a64d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {64{ARdEnA}} & BMisoA;
 assign AMisoB = {64{FRdEnB}} & BMisoB;

endmodule

module RamDX_7a64d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [7-1:0] AAddrA, input [64-1:0] AMosiA, output [64-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [7-1:0] AAddrB, input [64-1:0] AMosiB, output [64-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {64{1'b0}};

 wire [64-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_7a64d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {64{ARdEnA}} & BMisoA;
 assign AMisoB = {64{FRdEnB}} & BMisoB;

endmodule

module RamDX_12a64d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [12-1:0] AAddrA, input [64-1:0] AMosiA, output [64-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [12-1:0] AAddrB, input [64-1:0] AMosiB, output [64-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {64{1'b0}};

 wire [64-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_12a64d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {64{ARdEnA}} & BMisoA;
 assign AMisoB = {64{FRdEnB}} & BMisoB;

endmodule

module RamDX_13a64d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [13-1:0] AAddrA, input [64-1:0] AMosiA, output [64-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [13-1:0] AAddrB, input [64-1:0] AMosiB, output [64-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam ZData = {64{1'b0}};

 wire [64-1:0] BMisoA, BMisoB;
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
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn),
   .ADataI({BRdEnB}),
   .ADataO({FRdEnB})
  );

 assign BRdEnA = ARdEnA;
 assign BRdEnB = ARdEnB;

 RamDX_13a64d_Lat UMem
  (
   .ClockA(AClkA), .ResetA(~AResetAN), .ClockEnA(AClkAEn),
   .AddressA(AAddrA), .DataInA(AMosiA), .QA(BMisoA), .WrA(AWrEnA),
   .ClockB(AClkB), .ResetB(~AResetBN), .ClockEnB(AClkBEn),
   .AddressB(AAddrB), .DataInB(AMosiB), .QB(BMisoB), .WrB(AWrEnB)
  );

 assign AMisoA = {64{ARdEnA}} & BMisoA;
 assign AMisoB = {64{FRdEnB}} & BMisoB;

endmodule




