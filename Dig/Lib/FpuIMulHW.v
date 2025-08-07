module FpuIMul #(parameter CMantLen=28)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [CMantLen-1:0] ADataS, ADataD, input wire AStart,
  output wire [CMantLen+2-1:0] ADataR, output wire ABusy, output wire AWrEn
 );

 localparam CMantNil = {CMantLen{1'b0}};
 localparam CMulResLen = CMantLen+2;
 localparam CMulResNil = {CMulResLen{1'b0}};

 wire [CMantLen-1:0] FDataS, BDataS;
 wire [CMantLen-1:0] FDataD, BDataD;
 wire [CMantLen*2-1:0] FDataR, BDataR;
 wire [1:0] FStep, BStep;
 wire FBusy, BBusy;

 MsDffList #(.CRegLen(2*CMantLen+CMantLen*2+2+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDataS, BDataD, BDataR, BStep, BBusy}),
   .ADataO({FDataS, FDataD, FDataR, FStep, FBusy})
  );

 assign BBusy = |BStep;
 assign BStep = AStart ? 2'b10 : {1'b0, FStep[1]};

 assign BDataS = AStart ? ADataS : CMantNil;
 assign BDataD = AStart ? ADataD : CMantNil;
 assign BDataR = FDataS*FDataD;

 assign ADataR = AWrEn ? FDataR[CMantLen*2-1:CMantLen-2] : CMulResNil;
 assign ABusy = FBusy;
 assign AWrEn = FStep[0];
endmodule




