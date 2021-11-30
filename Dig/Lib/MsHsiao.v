module MsHsiao32d7eEnc
 (
  input wire [17:0] AAddr,
  input wire [31:0] ADataI,
  output wire [38:0] ADataE
 );

 wire [6:0] BSyndrome;

 MsHsiao32d7eSyndrome USyndrome
  (
   .ADataI({AAddr, ADataI}),
   .ASyndrome(BSyndrome)
  );

 assign ADataE = {BSyndrome, ADataI};
endmodule

module MsHsiao32d7eSyndromeA
 (
  input wire [49:0] ADataI, ASensI,
  output wire ASyndrome
 );

 assign ASyndrome = ^(ADataI & ASensI);
endmodule

module MsHsiao32d7eSyndrome
 (
  input wire [49:0] ADataI,
  output wire [6:0] ASyndrome
 );

 MsHsiao32d7eSyndromeA USyndromeA [6:0]
  (
   .ADataI(ADataI),
   .ASensI({50'h054F7_C5C3_5996, 50'h22AED_9C3C_5553, 50'h161DE_5999_AC39, 50'h098BF_CA55_A3C9, 50'h3867A_35CC_9335, 50'h381F9_3333_5CCC, 50'h07F07_3333_3333}),
   .ASyndrome(ASyndrome)
  );

endmodule

module MsHsiao16d5eEnc
 (
  input wire [9:0] AAddr,
  input wire [15:0] ADataI,
  output wire [21:0] ADataE
 );

 wire [5:0] BSyndrome;

 MsHsiao16d5eSyndrome USyndrome
  (
   .ADataI({AAddr, ADataI}),
   .ASyndrome(BSyndrome)
  );

 assign ADataE = {BSyndrome, ADataI};
endmodule

module MsHsiao16d5eSyndromeA
 (
  input wire [25:0] ADataI, ASensI,
  output wire ASyndrome
 );

 assign ASyndrome = ^(ADataI & ASensI);
endmodule

module MsHsiao16d5eSyndrome
 (
  input wire [25:0] ADataI,
  output wire [5:0] ASyndrome
 );

 MsHsiao16d5eSyndromeA USyndromeA [5:0]
  (
   .ADataI(ADataI),
   .ASensI(
    {
     26'h0FECAC5,
     26'h2BDC5AA,
     26'h17BAC5A,
     26'h3375C35,
     26'h32F33CC,
     26'h0DF3333
    }),
   .ASyndrome(ASyndrome)
  );

endmodule

