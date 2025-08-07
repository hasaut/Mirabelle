module IoClkDivBB #(parameter CAddrBase=16'h0000, parameter CDivider=16'h1717, parameter CResetDelayW=16)
 (
  input wire AClkH, AResetHN, AClkHEn, AResetEN,
  input wire [15:0] AIoAddr, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, output wire AIoAddrAck, AIoAddrErr,
  input wire AClkI, AResetIN,
  output wire AClkO, AResetON,
  output wire [7:0] ATest
 );

 // IO
 localparam IoSizeW = 1*8;
 localparam IoOperW = 4;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00001000)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(4'h0),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Implementation
 wire [2:0] FStartupI, BStartupI; assign BStartupI = {FStartupI[1:0], 1'b1};
 wire [15:0] FDividerI, BDividerI;
 wire FDividerISet, BDividerISet; // Whether to use the default value or not
 
 wire [15:0] FDividerH, BDividerH;
 MsDffList #(.CRegLen(16)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDividerH}),
   .ADataO({FDividerH})
  );

 assign BDividerH = BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[15:0] : FDividerH;

 wire [15:0] BDividerHI; wire BWrDividerI;
 MsCrossDS #(.CRegLen(16)) UDividerHI
 (
  .AClkH(AClkI), .AResetHN(AResetIN), .AClkHEn(1'b1),
  .AClkS(AClkH), .AResetSN(AResetHN),
  .ADataI(BDividerH), .ADataO(BDividerHI),
  .AReqS(BIoAccess[IoSizeW+IoOperW+0]), .AAckH(BWrDividerI)
 );

 assign BDividerI    = BWrDividerI ? BDividerHI : (FDividerISet ? FDividerI : CDivider);
 assign BDividerISet = BWrDividerI | FDividerISet;

 // X Domain
 wire [7:0] FCntDec, BCntDec;
 wire [1:0] FClkO, BClkO;

 MsDffList #(.CRegLen(8+2+3+16+1)) ULocalVarsI
  (
   .AClkH(AClkI), .AResetHN(AResetIN), .AClkHEn(1'b1),
   .ADataI({BCntDec, BClkO, BStartupI, BDividerI, BDividerISet}),
   .ADataO({FCntDec, FClkO, FStartupI, FDividerI, FDividerISet})
  );

 wire BCntDecNZ = |FCntDec;
 wire [7:0] BLoadMux = FClkO[0] ? FDividerI[15:8] : FDividerI[7:0];

 //assign BCntDec = (BCntDecNZ & FStartupI[2]) ? FCntDec-8'h1 : BLoadMux;
 assign BCntDec = BCntDecNZ ? FCntDec-8'h1 : BLoadMux; // The very first clock will be too short because FDividerI (and thus BLoadMux) is zero
 assign BClkO = {FClkO[0], (FClkO[0] ^ ~BCntDecNZ) & FStartupI[2]}; // The very first clock is masked by FStartupI

 // ResetHN
 wire [CResetDelayW-1:0] FResetON, BResetON;
 wire FResetEN, BResetEN;
 MsDffList #(.CRegLen(CResetDelayW+1)) ULocalVarsO
  (
   .AClkH(AClkO), .AResetHN(AResetIN), .AClkHEn(1'b1),
   .ADataI({BResetON, BResetEN}),
   .ADataO({FResetON, FResetEN})
  );
 assign BResetEN = AResetEN;
 assign BResetON = FResetON + {{(CResetDelayW-1){1'b0}}, FResetEN & ~FResetON[CResetDelayW-1]};

 assign AClkO = FClkO[1];
 assign AResetON = FResetON[CResetDelayW-1];

 assign ATest = {AClkI, AClkO, AResetIN, AResetEN, AResetHN, |FDividerH, BCntDecNZ, BWrDividerI};
endmodule

module TimerClockSync #(parameter CDividerWidth=8, CDividerValue=8'hFF)
 (
  input wire AClkX, input wire AResetXN, input wire AClkXEn,
  input wire AClkH, AResetHN, AClkHEn,
  input wire ACascadeIn, output wire ACascadeOut,
  output wire ASync
 );

 reg [CDividerWidth-1:0] FDivider; wire [CDividerWidth-1:0] BDivider;
 reg FCascade; wire BCascade;

 always @(posedge AClkX or negedge AResetXN)
 if (AResetXN==1'b0)
  begin
  FDivider<=CDividerValue;
  FCascade<=1'b0;
  end
 else if (AClkXEn)
  begin
  FDivider<=BDivider;
  FCascade<=BCascade;
  end

 wire BDividerNZ = |FDivider;
 assign BDivider = BDividerNZ ? FDivider - {{(CDividerWidth-1){1'b0}}, ACascadeIn} : CDividerValue;
 assign BCascade = ACascadeIn & ~BDividerNZ;

 MsCrossSync UCrossSync
  (
   .AClkA(AClkX), .AResetAN(AResetXN), .AClkAEn(AClkXEn),
   .AReqA(~BDividerNZ),
   .AClkB(AClkH), .AResetBN(AResetHN), .AClkBEn(AClkHEn),
   .AAckB(ASync)
  );

 assign ACascadeOut = FCascade;

endmodule


module ClkDiv2a
 (
  input wire AClkD, input wire AResetDN,
  output wire AClkE, output wire AResetEN
 );

 reg [1:0] FClkE; wire BClkE;

 always @(posedge AClkD or negedge AResetDN)
 if (AResetDN==1'b0)
  begin
  FClkE<=2'h0;
  end
 else
  begin
  FClkE<={FClkE[0], BClkE};
  end

 assign BClkE = ~FClkE[0];

 reg FResetEN; wire BResetEN;

 always @(posedge AClkE or negedge AResetDN)
 if (AResetDN==1'b0)
  begin
  FResetEN<=1'b0;
  end
 else
  begin
  FResetEN<=1'b1;
  end

 assign AClkE = FClkE[1];
 assign AResetEN = FResetEN;
endmodule

module ClkDivX #(parameter CDivLen=2)
 (
  input wire AClkD, input wire AResetDN,
  output wire AClkE, output wire AResetEN
 );

 reg [(CDivLen-1):0] FClkDiv; wire [(CDivLen-1):0] BClkDiv;
 reg FClkE; wire BClkE;

 always @(posedge AClkD or negedge AResetDN)
 if (AResetDN==1'b0)
  begin
  FClkDiv<={CDivLen{1'b0}};
  FClkE<=1'b0;
  end
 else
  begin
  FClkDiv<=BClkDiv;
  FClkE<=BClkE;
  end

 wire [CDivLen:0] BClkDivA = {1'b0, FClkDiv} + {{CDivLen{1'b0}}, 1'b1};
 assign BClkDiv = BClkDivA[CDivLen-1:0];
 assign BClkE = FClkDiv[CDivLen-1];

 reg FResetEN; 

 always @(posedge AClkE or negedge AResetDN)
 if (AResetDN==1'b0)
  begin
  FResetEN<=1'b0;
  end
 else
  begin
  FResetEN<=1'b1;
  end

 assign AClkE = FClkE;
 assign AResetEN = FResetEN;
endmodule

module ClkDivM #(parameter CBitCnt=4, CHalfME=4'h9) // HalfME = Half-1
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire ACascadeI, output wire ACascadeO,
  output wire AClkOut
 );

 wire [CBitCnt-1:0] FDivider, BDivider;
 wire FClkOut, BClkOut;

 MsDffList #(.CRegLen(CBitCnt+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BDivider, BClkOut}),
   .ADataO({FDivider, FClkOut})
  );

 wire BDividerNZ = |FDivider;
 assign BDivider = BDividerNZ ? FDivider - {{(CBitCnt-1){1'b0}}, ACascadeI} : CHalfME;
 assign BClkOut = FClkOut ^ (ACascadeI & ~BDividerNZ);

 assign ACascadeO = ACascadeI & ~BDividerNZ & FClkOut;
 assign AClkOut = FClkOut;
endmodule

module ClkDivMX
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [11:0] AClkDiv,
  output wire ACascadeO,
  output wire AClkOut
 );

 /*
 12*n / (12*(n-1)+11)
 */

 wire [11:0] BClkDiv;
 DpCrossDataX #(.CDataWidth(12))  UCrossDataX
  (
   .AClkB(AClkH), .AResetBN(AResetHN), .AClkBEn(AClkHEn),
   .ADataA(AClkDiv),
   .ADataB(BClkDiv)
  );

 wire [7:0] FCnt, BCnt;
 wire [7:0] FSReg, BSReg;
 wire FClkOut, BClkOut;

 MsDffList #(.CRegLen(8+8+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),  
   .ADataI({BCnt, BSReg, BClkOut}),
   .ADataO({FCnt, FSReg, FClkOut})
  );

 wire [7:0] BFraDec =
  ((BClkDiv[3:1]==3'h0) ? 8'b00000000 : 8'h0) |
  ((BClkDiv[3:1]==3'h1) ? 8'b00001000 : 8'h0) |
  ((BClkDiv[3:1]==3'h2) ? 8'b10001000 : 8'h0) |
  ((BClkDiv[3:1]==3'h3) ? 8'b10100100 : 8'h0) |
  ((BClkDiv[3:1]==3'h4) ? 8'b10101010 : 8'h0) |
  ((BClkDiv[3:1]==3'h5) ? 8'b10110110 : 8'h0) |
  ((BClkDiv[3:1]==3'h6) ? 8'b11101110 : 8'h0) |
  ((BClkDiv[3:1]==3'h7) ? 8'b11101111 : 8'h0);

 wire [7:0] BClkDivME = BClkDiv[11:4]+8'hFF;

 wire BCntNZ = |FCnt;
 assign BSReg = BCntNZ ? FSReg : {~(|FSReg[7:1]), FSReg[7:1]};
 wire BMux = |(FSReg & BFraDec);
 assign BCnt = BCntNZ ? FCnt-8'h1 : (BMux ? BClkDiv[11:4] : BClkDivME);

 assign BClkOut = FClkOut ^ ~BCntNZ;

 assign ACascadeO = ~BCntNZ & FClkOut;
 assign AClkOut = FClkOut;
endmodule

