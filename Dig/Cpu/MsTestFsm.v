module MsTestFsm
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire ARsuReady,
  input wire ALoadFW, input wire ALdrActive,
  output wire [11:0] ADbioAddr, output wire [63:0] ADbioMosi, output wire [3:0] ADbioMosiIdx, ADbioMisoIdx, output wire ADbioMosi1st, output wire ADbioMiso1st,
  output wire AReady,
  output wire [7:0] ATest
 );

 // Params
 localparam CStLen = 9;
 localparam CStNil = {CStLen{1'b0}};

 localparam IStFsmStartA = 0;
 localparam IStRsuWait   = 1;
 localparam IStRsuReady  = 2;
 localparam IStLdrStartA = 3;
 localparam IStLdrWait   = 4;
 localparam IStCpuStartA = 5;
 localparam IStCpuStartB = 6;
 localparam IStCpuDebugA = 7;
 localparam IStFsmReady  = 8;

 // Local vars
 // FSM
 wire [(CStLen-1):0] FState, BState;
 wire FLoadFW, BLoadFW;

 MsDffList #(.CRegLen(CStLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BState, BLoadFW}),
   .ADataO({FState, FLoadFW})
  );

 assign BLoadFW = ALoadFW;

 wire BStateNZ = |FState;

 // FSM
 assign BState[IStFsmStartA] = ~BStateNZ;
 assign BState[IStRsuWait]   =  FState[IStFsmStartA] |
                               (FState[IStRsuWait] & ~ARsuReady);
 assign BState[IStRsuReady]  =  FState[IStRsuWait] &  ARsuReady;
 assign BState[IStLdrStartA] =  FState[IStRsuReady] &  FLoadFW ;
 assign BState[IStLdrWait]   =  FState[IStLdrStartA] |
                               (FState[IStLdrWait] &  ALdrActive);
 assign BState[IStCpuStartA] = (FState[IStRsuReady] & ~FLoadFW) |
                               (FState[IStLdrWait] & ~ALdrActive);
 assign BState[IStCpuStartB] =  FState[IStCpuStartA];
 assign BState[IStCpuDebugA] =  FState[IStCpuStartB];
 assign BState[IStFsmReady]  =  FState[IStCpuDebugA] | FState[IStFsmReady];

 assign {ADbioAddr, ADbioMosi, ADbioMosiIdx, ADbioMisoIdx, ADbioMosi1st, ADbioMiso1st} =
  (FState[IStLdrStartA] ? {12'h100, 64'h0001, 4'h2, 4'h0, 1'b0, 1'b0} : 86'h0) |  // Loader, command code 1 = load from flash (to "ROM")
  (FState[IStCpuStartA] ? {12'h000, 64'h0003, 4'h1, 4'h0, 1'b0, 1'b0} : 86'h0) |  // See MsTestBU, CpuCtrl. We reset it in this line...
  (FState[IStCpuStartB] ? {12'h000, 64'h0005, 4'h1, 4'h0, 1'b0, 1'b0} : 86'h0);   // ... and start in this line
  //(FState[IStCpuStartB] ? {12'h000, 64'h0001, 4'h1, 4'h0, 1'b0, 1'b0} : 86'h0);

//  (FState[IStCpuStartA] ? {12'h000, 128'h0000, 5'h2, 5'h0, 1'b0, 1'b0} : 86'h0);// |
//  (FState[IStCpuDebugA] ? {12'h006, 128'h0017, 5'h4, 5'h0, 1'b0, 1'b0} : 86'h0);

 assign AReady = FState[IStFsmReady];
 assign ATest = {ALoadFW, FState[6:0]};
endmodule

