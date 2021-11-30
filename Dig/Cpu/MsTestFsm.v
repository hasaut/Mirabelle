module MsTestFsm
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ALoadFW, input ALdrActive,
  output [11:0] ADbioAddr, output [63:0] ADbioMosi, output [3:0] ADbioMosiIdx, ADbioMisoIdx, output ADbioMosi1st, output ADbioMiso1st,
  output AReady,
  output [7:0] ATest
 );

 // Params
 localparam CStLen = 7;
 localparam CStNil = {CStLen{1'b0}};

 localparam IStFsmStartA = 0;
 localparam IStLdrStartA = 1;
 localparam IStLdrWait   = 2;
 localparam IStCpuStartA = 3;
 localparam IStCpuStartB = 4;
 localparam IStCpuDebugA = 5;
 localparam IStFsmReady  = 6;

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
 assign BState[IStLdrStartA] =  FState[IStFsmStartA] &  FLoadFW ;
 assign BState[IStLdrWait]   =  FState[IStLdrStartA] |
                               (FState[IStLdrWait] &  ALdrActive);
 assign BState[IStCpuStartA] = (FState[IStFsmStartA] & ~FLoadFW) |
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
 assign ATest = {ALoadFW, FState};
endmodule

