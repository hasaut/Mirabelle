module UartCodec
 (
  input AClkH, input AResetN, input AClkHEn,
  input ARx, output ATx,
  output [7:0] ARecvData, output ARecvAck,
  input [7:0] ASendData, input ASendReq, output ASendRdy
 );

 parameter CBaudLen = 8;
 parameter CBaudDiv = 8'hFF;

 // Local variables
 // Send part
 reg [8:0] FSendReg;  wire [8:0] BSendReg;
 reg [CBaudLen:0] FSendBaud; wire [CBaudLen:0] BSendBaud; // Here and later do not indicate (CBaudLen-1)
 reg [3:0] FSendBit;  wire [3:0] BSendBit;
 // Recv part
 reg [1:0] FRxBuf;
 reg [1:0] FRecvState; wire [1:0] BRecvState;
 reg [7:0] FRecvReg;   wire [7:0] BRecvReg;
 reg [CBaudLen:0] FRecvBaud; wire [CBaudLen:0] BRecvBaud;
 reg [3:0] FRecvBit;   wire [3:0] BRecvBit;

 // Implementation
 always @(posedge AClkH or negedge AResetN)
 if (AResetN==1'b0)
  begin
  // Send part
  FSendReg<=9'h1FF;
  FSendBaud<={CBaudLen+1{1'b0}};
  FSendBit<=4'h0;
  // Recv part
  FRxBuf<=2'h0;
  FRecvState<=2'h0;
  FRecvReg<=8'h0;
  FRecvBaud<={CBaudLen+1{1'b0}};
  FRecvBit<=4'h0;
  end
 else if (AClkHEn)
  begin
  // Send part
  FSendReg<=BSendReg;
  FSendBaud<=BSendBaud;
  FSendBit<=BSendBit;
  // Recv part
  FRxBuf<={FRxBuf[0], ARx};
  FRecvState<=BRecvState;
  FRecvReg<=BRecvReg;
  FRecvBaud<=BRecvBaud;
  FRecvBit<=BRecvBit;
  end

 // Send part
 wire BSendBaudNZ = |FSendBaud;
 wire BSendBitNZ  = |FSendBit;

 wire BSendNextBit  = ~BSendBaudNZ &  BSendBitNZ;

 wire BSendReq = ASendReq & ~BSendBaudNZ & ~BSendBitNZ;
 assign BSendBaud = (BSendReq | BSendNextBit) ? {CBaudDiv, 1'b1} : FSendBaud-{{CBaudLen{1'b0}}, BSendBaudNZ};
 assign BSendReg  =  BSendReq ? {ASendData, 1'b0} : (BSendNextBit ? {1'b1, FSendReg[8:1]} : FSendReg);
 assign BSendBit  =  BSendReq ? 4'h9 : FSendBit-{3'h0, BSendNextBit};

 assign ASendRdy = ~BSendBaudNZ & ~BSendBitNZ;

 // Recv part
 wire BRecvStateNZ = |FRecvState;
 wire BRecvBaudNZ  = |FRecvBaud;
 wire BRecvBitNZ   = |FRecvBit;

 wire [1:0] BRecvGo;
 assign BRecvGo[0] = ~BRecvStateNZ & (FRxBuf==2'b10);
 assign BRecvGo[1] = FRecvState[0] & ~BRecvBaudNZ;

 assign BRecvState[0] = BRecvGo[0] | (FRecvState[0] & BRecvBaudNZ);
 assign BRecvState[1] = BRecvGo[1] | (FRecvState[1] & (BRecvBaudNZ | BRecvBitNZ));

 wire BRecvNextBit  = FRecvState[1] & ~BRecvBaudNZ &  BRecvBitNZ;
 wire BRecvNextByte = FRecvState[1] & ~BRecvBaudNZ & ~BRecvBitNZ;

 assign BRecvBaud =
  (BRecvGo[0] ? {1'b0, CBaudDiv} : {(CBaudLen+1){1'b0}}) |
  ((BRecvGo[1] | BRecvNextBit) ? {CBaudDiv, 1'b1} : {(CBaudLen+1){1'b0}}) |
  (FRecvBaud - {{CBaudLen{1'b0}}, BRecvBaudNZ});

 assign BRecvBit = ({4{BRecvGo[1]}} & 4'h8) | (FRecvBit - {3'h0, BRecvNextBit});

 assign BRecvReg = BRecvNextBit ? {FRxBuf[0], FRecvReg[7:1]} : FRecvReg;

 assign ARecvAck  = BRecvNextByte;
 assign ARecvData = FRecvReg;

 // Common part
 assign ATx = FSendReg[0];

endmodule

