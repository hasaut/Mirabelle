module IoClkDivBB #(parameter CAddrBase=16'h0000, parameter CDivider=16'h1717, parameter CResetDelayW=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, input [63:0] AIoMosi, input [3:0] AIoWrSize, output AIoAddrAck, output AIoAddrErr,
  input AClkI, input AResetIN,
  output AClkO, output AResetON
 );

 localparam CResetDelayWSim = 4;

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
 reg [15:0] FDivider; wire [15:0] BDivider;
 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  FDivider<=CDivider;
  end
 else if (AClkHEn)
  begin
  FDivider<=BDivider;
  end

 assign BDivider = BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[15:0] : FDivider;

 // X Domain
 reg [7:0] FCntDec; wire [7:0] BCntDec;
 reg [1:0] FClkO; wire BClkO;

 always @(posedge AClkI or negedge AResetIN)
 if (AResetIN==1'b0)
  begin
  FCntDec<=CDivider[15:8];
  FClkO<=2'h0;
  end
 else
  begin
  FCntDec<=BCntDec;
  FClkO<={FClkO[0], BClkO};
  end

 wire BCntDecNZ = |FCntDec;
 wire [7:0] BLoadMux = FClkO[0] ? FDivider[15:8] : FDivider[7:0];

 assign BCntDec = BCntDecNZ ? FCntDec-8'h1 : BLoadMux;
 assign BClkO = FClkO[0] ^ ~BCntDecNZ;

 // ResetHN
 wire [CResetDelayWSim-1:0] FResetON, BResetON;
 MsDffList #(.CRegLen(CResetDelayWSim)) ULocalVars
  (
   .AClkH(AClkO), .AResetHN(AResetIN), .AClkHEn(1'b1),
   .ADataI({BResetON}),
   .ADataO({FResetON})
  );
 assign BResetON = FResetON+{{(CResetDelayWSim-1){1'b0}}, ~FResetON[CResetDelayWSim-1]};

 assign AClkO = AClkI;
 assign AResetON = FResetON[CResetDelayWSim-1];

endmodule

module TimerClockSync #(parameter CDividerWidth=8, CDividerValue=8'hFF)
 (
  input AClkX, input AResetXN, input AClkXEn,
  input AClkH, input AResetHN, input AClkHEn,
  input ACascadeIn, output ACascadeOut,
  output ASync
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
  input AClkD, input AResetDN,
  output AClkE, output AResetEN
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
  input AClkD, input AResetDN,
  output AClkE, output AResetEN
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

 assign BClkDiv = FClkDiv + {{(CDivLen-1){1'b0}}, 1'b1};
 assign BClkE = FClkDiv[CDivLen-1];

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

 assign AClkE = FClkE;
 assign AResetEN = FResetEN;
endmodule

module ClkDivM #(parameter CBitCnt=4, CHalfME=4'h9) // HalfME = Half-1
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ACascadeI, output ACascadeO,
  output AClkOut
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
  input AClkH, input AResetHN, input AClkHEn, 
  input [11:0] AClkDiv,
  output ACascadeO,
  output AClkOut
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

