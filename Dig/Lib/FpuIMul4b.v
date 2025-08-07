module FpuIMul #(parameter CMantLen=28)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CMantLen-1:0] ADataS, ADataD, input wire AStart,
  output wire [CMantLen+2-1:0] ADataR, output wire ABusy, output wire AWrEn
 );

 localparam CMantNil = {CMantLen{1'b0}};
 localparam CMulResLen = CMantLen+2;
 localparam CMulResNil = {CMulResLen{1'b0}};
 localparam CAuxLen = CMantLen+4;
 localparam CAuxNil = {CAuxLen{1'b0}};
 localparam CBit4Cnt = CMantLen/4;

 wire [CMantLen-1:0] FDataC, BDataC;
 wire [CMantLen-1:0] FDataB, BDataB;
 wire [CMantLen-1:0] FDataA, BDataA;
 wire [CBit4Cnt-1:0] FStep, BStep;
 wire FBusy, BBusy;

 MsDffList #(.CRegLen(3*CMantLen+CBit4Cnt+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDataC, BDataB, BDataA, BStep, BBusy}),
   .ADataO({FDataC, FDataB, FDataA, FStep, FBusy})
  );

 assign BBusy = |BStep;
 assign BStep = AStart ? {1'b1, {(CBit4Cnt-1){1'b0}}} : {1'b0, FStep[CBit4Cnt-1:1]};

 wire [CAuxLen-1:0] BAuxEU = {4'h0, FDataA};
 wire [CAuxLen-1:0] BAuxDU = BAuxEU + (FDataC[3] ? {      FDataB, 3'h0} : CAuxNil);
 wire [CAuxLen-1:0] BAuxCU = BAuxDU + (FDataC[2] ? {1'h0, FDataB, 2'h0} : CAuxNil);
 wire [CAuxLen-1:0] BAuxBU = BAuxCU + (FDataC[1] ? {2'h0, FDataB, 1'h0} : CAuxNil);
 wire [CAuxLen-1:0] BAuxAU = BAuxBU + (FDataC[0] ? {3'h0, FDataB      } : CAuxNil);

 assign BDataA = BBusy ? BAuxAU[CAuxLen-1:4] : CMantNil;
 assign BDataB = AStart ? ADataD : FDataB;
 assign BDataC = AStart ? ADataS : {4'h0, FDataC[CMantLen-1:4]};

 assign ADataR = AWrEn ? BAuxAU[CAuxLen-1:2] : CMulResNil;
 assign ABusy = FBusy;
 assign AWrEn = FStep[0];
endmodule




