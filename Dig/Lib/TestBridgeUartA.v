module TestBridgeUartA
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ADbgRx, output ADbgTx, output ADbgTxFlush,
  input ASync1M, ASync1K,
  output [11:0] ADbioAddr, output [63:0] ADbioMosi, input [63:0] ADbioMiso, output [3:0] ADbioMosiIdx, ADbioMisoIdx, output ADbioMosi1st, output ADbioMiso1st, output ADbioDataLenNZ, input ADbioIdxReset,
  input [7:0] ADbioSbData, input ADbioSbNow, input ADbioSbActive, // Data to be sent back immediately
  input ADbgAttReq,
  output [7:0] ATest
 );

 // Params
 localparam CStLen = 20;
 localparam CStNil = {CStLen{1'b0}};

 localparam IStSyncA =  0;
 localparam IStCommA =  1;
 localparam IStSyncB =  2;
 localparam IStSyncC =  3;
 localparam IStCommB =  4;
 localparam IStCommC =  5;
 localparam IStCommD =  6;
 localparam IStMosiA =  7;
 localparam IStMosiB =  8;
 localparam IStMosiC =  9;
 localparam IStMosiD = 10;
 localparam IStMosiE = 11;
 localparam IStMisoA = 12;
 localparam IStMisoB = 13;
 localparam IStMisoC = 14;
 localparam IStMisoD = 15;
 localparam IStMisoE = 16;
 localparam IStAttA  = 17;
 localparam IStAttB  = 18;
 localparam IStAttC  = 19;

 // Local vars
 // FSM
 wire [CStLen-1:0] FState, BState;
 // UART
 wire [15:0] FRecvCtrl, BRecvCtrl;
 wire [15:0] FBaud, BBaud;
 // Data
 wire [15:0] FDataLen, BDataLen;
 // TimeOut
 wire [9:0] FTimeOut, BTimeOut;
 // Dbio
 wire [11:0] FDbioAddr, BDbioAddr;
 wire [63:0] FDbioMosi, BDbioMosi;
 wire [63:0] FDbioMiso, BDbioMiso;
 wire [3:0] FDbioMosiIdx, BDbioMosiIdx;
 wire [3:0] FDbioMisoIdx, BDbioMisoIdx;
 wire FDbioMosi1st, BDbioMosi1st;
 // AttReq
 wire [6:0] FAttTimer, BAttTimer;

 MsDffList #(.CRegLen(CStLen+16+16+16+10+12+64+64+4+4+1+7)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BState, BRecvCtrl, BBaud, BDataLen, BTimeOut, BDbioAddr, BDbioMosi, BDbioMiso, BDbioMosiIdx, BDbioMisoIdx, BDbioMosi1st, BAttTimer}),
   .ADataO({FState, FRecvCtrl, FBaud, FDataLen, FTimeOut, FDbioAddr, FDbioMosi, FDbioMiso, FDbioMosiIdx, FDbioMisoIdx, FDbioMosi1st, FAttTimer})
  );

 // Common
 wire BTimeOutNZ = |FTimeOut;
 wire BStateNZ   = |FState;
 wire BDataLenNZ = |FDataLen;

 // Fifo (Send)
 wire [7:0] BUartSendData; wire BUartSendNow;
 wire BSendReqA, BSendAckA; wire [7:0] BSendDataA;
 wire BSendHasSpace;
 MsFifo4x #(.CDataLen(8)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BUartSendData), .AWrEn(BUartSendNow),
   .ADataO(BSendDataA), .ARdEn(BSendAckA),
   .AClr(1'b0), .AHasData(BSendReqA), .AHasSpace(BSendHasSpace), .ADataSize()
  );

 // UART
 wire [7:0] BUartRecvData; wire BUartRecvNow;
 wire [15:0] BBaudResult; wire BBaudUpdate;
 wire BSendBusy;

 UartACodec UUart
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACfg2Stop(1'b0), .ACfgTxEn(1'b1), .ACfgRxEn(BStateNZ | ~BBaudUpdate), //.ACfgTol(8'h3),
   .ABaudI(FBaud), .ABaudO(BBaudResult), .ABaudUpdate(BBaudUpdate),
   .AFifoSendData(BSendDataA), .AFifoSendReady(BSendReqA), .AFifoSendRd(BSendAckA), .ASendBusy(BSendBusy),
   .AFifoRecvData(BUartRecvData), .AFifoRecvWr(BUartRecvNow),
   .ARx(ADbgRx), .ATx(ADbgTx),
   .ATest()
  );

 // Flush
 UartFlush UFlush
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASync1M(ASync1M),
   .ABusy(BSendBusy | BSendReqA | ADbioSbActive), .AFlush(ADbgTxFlush),
   .ATest()
  );

 // Att
 wire BAttTimerNZ = |FAttTimer;
 assign BAttTimer = ADbgAttReq ? (FState[IStAttA] ? 7'd99 : FAttTimer-{6'h0, BAttTimerNZ & ASync1K}) : 7'h0;
 wire BAttReq = ADbgAttReq & ~BAttTimerNZ;

 // RecvCtrl dec
 wire BRecvCtrlRecv = (FRecvCtrl[15:12]==4'hC);
 wire BRecvCtrlSend = (FRecvCtrl[15:12]==4'h8);
 wire BRecvCtrlTran = (FRecvCtrl[15:12]==4'hD); // SendRecv

 // FSM
 assign BState[IStSyncA] = ~BStateNZ & BBaudUpdate;
 assign BState[IStCommA] = ~BStateNZ & BUartRecvNow & BUartRecvData[7];
 assign BState[IStAttA]  = ~BStateNZ & ~BState[IStSyncA] & ~BState[IStCommA] & BAttReq;
 // Sync
 assign BState[IStSyncB] = FState[IStSyncA] | (FState[IStSyncB] & ~BSendHasSpace);
 assign BState[IStSyncC] = FState[IStSyncB] & BSendHasSpace;
 // CommX
 assign BState[IStCommB] = FState[IStCommA] | (FState[IStCommB] & ~BUartRecvNow & BTimeOutNZ);
 assign BState[IStCommC] = (FState[IStCommB] & BUartRecvNow) | (FState[IStCommC] & ~BUartRecvNow & BTimeOutNZ);
 assign BState[IStCommD] = (FState[IStCommC] & BUartRecvNow) | (FState[IStCommD] & ~BUartRecvNow & BTimeOutNZ);
 // CommRecv
 assign BState[IStMosiA] = FState[IStCommD] & BUartRecvNow & (BRecvCtrlRecv | BRecvCtrlTran);
 assign BState[IStMosiB] = ((FState[IStMosiA] | FState[IStMosiC]) & BDataLenNZ) | (FState[IStMosiB] & ~BUartRecvNow & BTimeOutNZ);
 assign BState[IStMosiC] = FState[IStMosiB] & BUartRecvNow;
 assign BState[IStMosiD] = ((FState[IStMosiA] | FState[IStMosiC]) & ~BDataLenNZ) | (FState[IStMosiD] & ~BSendHasSpace);
 assign BState[IStMosiE] = FState[IStMosiD] & BSendHasSpace;
 // CommSend
 assign BState[IStMisoA] = (FState[IStCommD] & BUartRecvNow & BRecvCtrlSend) | (FState[IStMisoA] & ~BSendHasSpace);
 assign BState[IStMisoB] = FState[IStMisoA] & BSendHasSpace;
 assign BState[IStMisoC] = ((FState[IStMisoB] | FState[IStMisoD]) & BDataLenNZ) | (FState[IStMisoC] & ~BSendHasSpace);
 assign BState[IStMisoD] = FState[IStMisoC] & BSendHasSpace;
 assign BState[IStMisoE] = (FState[IStMisoB] | FState[IStMisoD]) & ~BDataLenNZ;
 // Attention
 assign BState[IStAttB]  = FState[IStAttA] | (FState[IStAttB] & ~BSendHasSpace);
 assign BState[IStAttC]  = FState[IStAttB] & BSendHasSpace;

 // Baud
 assign BBaud = FState[IStSyncA] ? BBaudResult : FBaud;

 // TimeOut
 assign BTimeOut = (FState[IStSyncA] | FState[IStCommA] | (BStateNZ & BUartRecvNow)) ? 10'd1000 : FTimeOut-{9'h0, BTimeOutNZ & ASync1K};

 assign BUartSendData =
  (FState[IStSyncC] ? 8'h55 : 8'h0) |
  (FState[IStAttC]  ? 8'hAA : 8'h0) |
  ((FState[IStMosiE] & ~FRecvCtrl[12]) ? 8'h00 : 8'h0) |
  (FState[IStMisoB] ? 8'h00 : 8'h0) |
  (FState[IStMisoD] ? FDbioMiso[7:0] : 8'h0) |
  (ADbioSbNow ? ADbioSbData : 8'h0);
 assign BUartSendNow = |{FState[IStSyncC], FState[IStAttC], FState[IStMosiE] & ~FRecvCtrl[12], FState[IStMisoB], FState[IStMisoD], ADbioSbNow};
 assign BRecvCtrl =
  {
   FState[IStCommA] ? BUartRecvData : FRecvCtrl[15:8],
   (FState[IStCommB] & BUartRecvNow) ?  BUartRecvData : FRecvCtrl[7:0]
  };

 wire BLenRecvL = FState[IStCommC] & BUartRecvNow;
 wire BLenRecvH = FState[IStCommD] & BUartRecvNow;
 wire BLenDec   = |{FState[IStMosiB] & BUartRecvNow, BState[IStMisoD]};
 assign BDataLen =
  (BLenRecvL ? {8'h0, BUartRecvData} : 16'h0) |
  (BLenRecvH ? {BUartRecvData, FDataLen[7:0]} : 16'h0) |
  (BLenDec   ? FDataLen-16'h1 : 16'h0) |
  ((BLenRecvL | BLenRecvH | BLenDec) ? 16'h0 : FDataLen);

 assign BDbioAddr = (FState[IStMisoA] | FState[IStMosiA]) ? FRecvCtrl[11:0] : FDbioAddr;

 wire [7:0] BDbioMosiIdxDecA; MsDec3x8a UDbioMosiIdxDecA ( .ADataI(FDbioMosiIdx[2:0]), .ADataO(BDbioMosiIdxDecA) );
 wire [7:0] BDbioMosiIdxDecB = FState[IStMosiC] ? BDbioMosiIdxDecA : 8'h0;
 assign BDbioMosi =
  {
   BDbioMosiIdxDecB[ 7] ? BUartRecvData : FDbioMosi[ 63: 56],
   BDbioMosiIdxDecB[ 6] ? BUartRecvData : FDbioMosi[ 55: 48],
   BDbioMosiIdxDecB[ 5] ? BUartRecvData : FDbioMosi[ 47: 40],
   BDbioMosiIdxDecB[ 4] ? BUartRecvData : FDbioMosi[ 39: 32],
   BDbioMosiIdxDecB[ 3] ? BUartRecvData : FDbioMosi[ 31: 24],
   BDbioMosiIdxDecB[ 2] ? BUartRecvData : FDbioMosi[ 23: 16],
   BDbioMosiIdxDecB[ 1] ? BUartRecvData : FDbioMosi[ 15:  8],
   BDbioMosiIdxDecB[ 0] ? BUartRecvData : FDbioMosi[  7:  0]
  };

 assign BDbioMiso = (FState[IStMisoB] | ADbioIdxReset) ? ADbioMiso : (FState[IStMisoD] ? {8'h0, FDbioMiso[63:8]} : FDbioMiso);

 assign BDbioMosiIdx = (FState[IStMosiA] | ADbioIdxReset) ? 4'h0 : FDbioMosiIdx + {3'h0, FState[IStMosiC]};

 assign BDbioMisoIdx = (FState[IStMisoA] | ADbioIdxReset) ? 4'h0 : FDbioMisoIdx + {3'h0, BState[IStMisoD]};

 assign BDbioMosi1st = FState[IStMosiA];

 assign ADbioMosi1st = FDbioMosi1st;
 assign ADbioMiso1st = FState[IStMisoB];
 assign ADbioAddr = FDbioAddr;
 assign ADbioMosi = FDbioMosi;
 assign ADbioMosiIdx = FDbioMosiIdx;
 assign ADbioMisoIdx = FDbioMisoIdx;
 assign ADbioDataLenNZ = BDataLenNZ;

 assign ATest = {4'h0, BStateNZ, BTimeOutNZ, FState[IStCommA], AResetHN};
endmodule

