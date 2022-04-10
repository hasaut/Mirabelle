module IoUartAB_F4b #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  input ASync1M, input ASync1K, output AIrq,
  input ARxPin, output ATxPin, output ASendBusy, input ASyncStart,
  output [7:0] ATest
 );

 // Interface
 // IobCtrl   +0 ; // WR: TxEn RxEn 2xTimerSrc 2Stop TimerAutoRstEn 2xIrqEn
 //                // RD: TxEn RxEn RFU TimerNZ SendBusy BaudUpdate CanWrite CanRead
 // IowBaud   +1 ; // WR/RD: Baud rate
 // IobData   +2 ; // WR/RD: Data
 // IowTOut   +3 ; // WR: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
 //              ; // RD: TimerThis

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h0000AA55)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local vars (Config)
 wire [7:0] FCtrl, BCtrl;
 wire [15:0] FBaud, BBaud;
 wire [1:0] FFlags, BFlags;
 // Local vars (Autobaud)
 wire [15:0] FBaudResult, BBaudResult;
 wire FBaudUpdate, BBaudUpdate;
 wire FSendBusyA, BSendBusyA;

 MsDffList #(.CRegLen(8+16+2+16+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrl, BBaud, BFlags, BBaudResult, BBaudUpdate, BSendBusyA}),
   .ADataO({FCtrl, FBaud, FFlags, FBaudResult, FBaudUpdate, FSendBusyA})
  );

 // Aliases
 wire L2Stop    = FCtrl[3];
 wire LTxEn     = FCtrl[7];
 wire LRxEn     = FCtrl[6];
 wire LTimerAre = FCtrl[2]; // Timer AutoResetEn
 wire [1:0] LIrqEn = FCtrl[1:0];

 // Interface
 // +0 IobCtrl  = 2'h0; // WR: TxEn RxEn 2xTimerSrc 2Stop RFU 2xIrqEn
 //                     // RD: TxEn RxEn RFU TimerNZ SendBusy BaudUpdate CanWrite CanRead
 // +1 IowBaud  = 2'h1; // WR/RD: Baud rate
 // +2 IobData  = 2'h2; // WR/RD: Data
 // +3 IowTOut  = 2'h3; // WR/RD: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
 assign BCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[7:0] : FCtrl;
 assign BBaud = BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FBaud;

 wire [7:0] BRecvFifo;
 wire BSendBusy;
 wire [15:0] BTimerThis; wire BTimerNZ;

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, BRecvFifo} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+1] ? {48'h0, FBaudResult} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FCtrl[7:5], BTimerNZ, BSendBusy, FBaudUpdate, FFlags} : 64'h0);


 // Fifo (Send)
 wire BSendReq, BSendAck; wire [7:0] BSendData;
 MsFifo4x #(.CDataLen(8)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[7:0]), .AWrEn(BIoAccess[IoSizeB+IoOperW+2]),
   .ADataO(BSendData), .ARdEn(BSendAck),
   .AHasData(BSendReq), .AHasSpace(BFlags[1]), .ADataSize()
  );

 // Fifo (Recv)
 wire [7:0] BRecvData; wire BRecvNow;
 MsFifo4x #(.CDataLen(8)) UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BRecvData), .AWrEn(BRecvNow),
   .ADataO(BRecvFifo), .ARdEn(BIoAccess[IoSizeB+IoOperR+2]),
   .AHasData(BFlags[0]), .AHasSpace(), .ADataSize()
  );

 // Baud part
 wire [15:0] BBaudResultA; wire BBaudUpdateA;
 assign BBaudUpdate = BBaudUpdateA | (FBaudUpdate & ~BIoAccess[IoSizeW+IoOperR+1]);
 assign BBaudResult = BBaudUpdateA ? BBaudResultA : FBaudResult;

 // Codec
 UartACodec UCodec
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACfg2Stop(L2Stop), .ACfgTxEn(LTxEn), .ACfgRxEn(LRxEn),
   .ABaudI(FBaud), .ABaudO(BBaudResultA), .ABaudUpdate(BBaudUpdateA),
   .AFifoSendData(BSendData), .AFifoSendReady(BSendReq), .AFifoSendRd(BSendAck), .ASendBusy(BSendBusy), .ASyncStart(ASyncStart),
   .AFifoRecvData(BRecvData), .AFifoRecvWr(BRecvNow),
   .ARx(ARxPin), .ATx(ATxPin),
   .ATest()
  );

 assign BSendBusyA = BSendBusy | BSendReq;

 // Process (Timer)
 PerifTimer #(.CDataLen(16)) UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FCtrl[5:4]), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(LTimerAre & (BSendBusy | BRecvNow)), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Common part
 assign ASendBusy = BSendBusy;
 assign AIrq = |(FFlags & LIrqEn);

 assign ATest = {ARxPin, ATxPin, 6'h0};
endmodule

module IoUartAB_F256b #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  input ASync1M, input ASync1K, output AIrq,
  input ARxPin, output ATxPin, output ATxFlush,
  output [7:0] ATest
 );

 // Interface
 // IobCtrl   +0 ; // WR: TxEn RxEn 2xTimerSrc 2Stop TimerAutoRstEn 2xIrqEn
 //                // RD: TxEn RxEn RFU TimerNZ SendBusy BaudUpdate CanWrite CanRead
 // IowBaud   +1 ; // WR/RD: Baud rate
 // IobData   +2 ; // WR/RD: Data
 // IowTOut   +3 ; // WR: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
 //              ; // RD: TimerThis

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h0000AA55)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local vars (Config)
 wire [7:0] FCtrl, BCtrl;
 wire [15:0] FBaud, BBaud;
 wire [1:0] FFlags, BFlags;
 // Local vars (Autobaud)
 wire [15:0] FBaudResult, BBaudResult;
 wire FBaudUpdate, BBaudUpdate;

 MsDffList #(.CRegLen(8+16+2+16+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrl, BBaud, BFlags, BBaudResult, BBaudUpdate}),
   .ADataO({FCtrl, FBaud, FFlags, FBaudResult, FBaudUpdate})
  );

 // Aliases
 wire L2Stop    = FCtrl[3];
 wire LTxEn     = FCtrl[7];
 wire LRxEn     = FCtrl[6];
 wire LTimerAre = FCtrl[2];
 wire [1:0] LIrqEn = FCtrl[1:0];

 // Interface
 // +0 IobCtrl  = 2'h0; // WR: TxEn RxEn 2xTimerSrc 2Stop RFU 2xIrqEn
 //                     // RD: TxEn RxEn RFU TimerNZ SendBusy BaudUpdate CanWrite CanRead
 // +1 IowBaud  = 2'h1; // WR/RD: Baud rate
 // +2 IobData  = 2'h2; // WR/RD: Data
 // +3 IowTOut  = 2'h3; // WR/RD: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
 assign BCtrl = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[7:0] : FCtrl;
 assign BBaud = BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FBaud;

 wire [7:0] BRecvFifo;
 wire BSendBusy;
 wire [15:0] BTimerThis; wire BTimerNZ;

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, BRecvFifo} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+1] ? {48'h0, FBaudResult} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FCtrl[7:5], BTimerNZ, BSendBusy, FBaudUpdate, FFlags} : 64'h0);


 // Fifo (Send)
 wire BSendReq, BSendAck; wire [7:0] BSendData;
 MsFifo256b UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[7:0]), .AWrEn(BIoAccess[IoSizeB+IoOperW+2]),
   .ADataO(BSendData), .ARdEn(BSendAck),
   .AHasData(BSendReq), .AHasSpace(BFlags[1]), .ADataSize()
  );

 // Fifo (Recv)
 wire [7:0] BRecvData; wire BRecvNow;
 MsFifo256b UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BRecvData), .AWrEn(BRecvNow),
   .ADataO(BRecvFifo), .ARdEn(BIoAccess[IoSizeB+IoOperR+2]),
   .AHasData(BFlags[0]), .AHasSpace(), .ADataSize()
  );

 // Baud part
 wire [15:0] BBaudResultA; wire BBaudUpdateA;
 assign BBaudUpdate = BBaudUpdateA | (FBaudUpdate & ~BIoAccess[IoSizeW+IoOperR+1]);
 assign BBaudResult = BBaudUpdateA ? BBaudResultA : FBaudResult;

 // Codec
 UartACodec UCodec
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ACfg2Stop(L2Stop), .ACfgTxEn(LTxEn), .ACfgRxEn(LRxEn),
   .ABaudI(FBaud), .ABaudO(BBaudResultA), .ABaudUpdate(BBaudUpdateA),
   .AFifoSendData(BSendData), .AFifoSendReady(BSendReq), .AFifoSendRd(BSendAck), .ASendBusy(BSendBusy), .ASyncStart(1'b1),
   .AFifoRecvData(BRecvData), .AFifoRecvWr(BRecvNow),
   .ARx(ARxPin), .ATx(ATxPin),
   .ATest()
  );

 // Process (Timer)
 PerifTimer UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FCtrl[5:4]), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(LTimerAre & (BSendBusy | BRecvNow)), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Flush
 UartFlush UFlush
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ASync1M(ASync1M),
   .ABusy(BSendBusy | BSendReq), .AFlush(ATxFlush),
   .ATest()
  );

 // Common part
 assign AIrq = |(FFlags & LIrqEn);

 assign ATest = {ARxPin, ATxPin, 2'h0, BRecvNow, BIoAccess[IoSizeB+IoOperR+2], FFlags[1:0]};
endmodule

// *** Codec part

module UartACodec
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ACfg2Stop, input ACfgTxEn, input ACfgRxEn,
  input [15:0] ABaudI, output [15:0] ABaudO, output ABaudUpdate, input ASyncStart,
  input [7:0] AFifoSendData, input AFifoSendReady, output AFifoSendRd, output ASendBusy,
  output [7:0] AFifoRecvData, output AFifoRecvWr,
  input ARx, output ATx,
  output [7:0] ATest
 );

 // Local variables
 // Send part
 wire  [8:0] FSendReg, BSendReg; // Inverted (due to default value 0 after reset)
 wire [12:0] FSendBaud, BSendBaud;
 wire  [3:0] FSendBitIdx, BSendBitIdx;
 // Recv part
 wire  [1:0] FRxBuf, BRxBuf;
 wire  [1:0] FRecvState, BRecvState;
 wire  [7:0] FRecvReg, BRecvReg;
 wire [12:0] FRecvBaud, BRecvBaud;
 wire  [3:0] FRecvBitIdx, BRecvBitIdx;

 MsDffList #(.CRegLen(9+13+4+2+2+8+13+4)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSendReg, BSendBaud, BSendBitIdx, BRxBuf, BRecvState, BRecvReg, BRecvBaud, BRecvBitIdx}),
   .ADataO({FSendReg, FSendBaud, FSendBitIdx, FRxBuf, FRecvState, FRecvReg, FRecvBaud, FRecvBitIdx})
  );

 // Common part
 assign BRxBuf = {FRxBuf[0], ARx};

 wire [7:0] BBaudCorrStatic =
  ((ABaudI[2:0]==3'h0) ? 8'b00001000 : 8'h0) |
  ((ABaudI[2:0]==3'h1) ? 8'b10001000 : 8'h0) |
  ((ABaudI[2:0]==3'h2) ? 8'b10100100 : 8'h0) |
  ((ABaudI[2:0]==3'h3) ? 8'b10101010 : 8'h0) |
  ((ABaudI[2:0]==3'h4) ? 8'b10110110 : 8'h0) |
  ((ABaudI[2:0]==3'h5) ? 8'b11101110 : 8'h0) |
  ((ABaudI[2:0]==3'h6) ? 8'b11101111 : 8'h0) |
  ((ABaudI[2:0]==3'h7) ? 8'b11111111 : 8'h0);

 // Send part
 wire BSendBaudNZ   = |FSendBaud;
 wire BSendBitIdxNZ = |FSendBitIdx;

 wire BSendNextBit  = ~BSendBaudNZ &  BSendBitIdxNZ;
 wire BSendNextByte = ~BSendBaudNZ & ~BSendBitIdxNZ & AFifoSendReady & ASyncStart;

 wire [12:0] BSendBaudA = ABaudI[15:3]-{12'h0, ~BBaudCorrStatic[BSendBitIdx[2:0]] & ~FSendBitIdx[3]};
 assign BSendBaud   = BSendNextByte ? ABaudI[15:3] : (BSendNextBit ? BSendBaudA : FSendBaud-{12'h0, BSendBaudNZ});
 assign BSendReg    = BSendNextByte ? {~AFifoSendData, 1'b1} : (BSendNextBit ? {1'b0, FSendReg[8:1]} : FSendReg); // This value must be inverted! (due to default value 0 after reset)
 assign BSendBitIdx = BSendNextByte ? 4'h9+{3'h0, ACfg2Stop} : FSendBitIdx-{3'h0, BSendNextBit};
 assign ASendBusy = BSendBaudNZ | BSendBitIdxNZ;

 assign AFifoSendRd = BSendNextByte;
 //assign ASendBusy = FSendBusy;

 // Recv part
 wire BRecvStateNZ  = |FRecvState;
 wire BRecvBaudNZ   = |FRecvBaud;
 wire BRecvBitIdxNZ = |FRecvBitIdx;

 wire [1:0] BRecvGo;
 assign BRecvGo[0] = ACfgRxEn & ~BRecvStateNZ & (FRxBuf==2'b10);
 assign BRecvGo[1] = ACfgRxEn & FRecvState[0] & ~BRecvBaudNZ;

 assign BRecvState[0] = BRecvGo[0] | (FRecvState[0] & ACfgRxEn & BRecvBaudNZ);
 assign BRecvState[1] = BRecvGo[1] | (FRecvState[1] & ACfgRxEn & (BRecvBaudNZ | BRecvBitIdxNZ));

 wire BRecvNextBit  = FRecvState[1] & ~BRecvBaudNZ &  BRecvBitIdxNZ;
 wire BRecvNextByte = FRecvState[1] & ~BRecvBaudNZ & ~BRecvBitIdxNZ;

 wire [12:0] BRecvBaudA = ABaudI[15:3]-{12'h0, ~BBaudCorrStatic[BRecvBitIdx[2:0]] & ~FRecvBitIdx[3]};
 assign BRecvBaud =
  (BRecvGo[0] ? {1'h0, ABaudI[15:4]} : 13'h0) |
  ((BRecvGo[1] | BRecvNextBit) ? BRecvBaudA : 13'h0) |
  (ACfgRxEn ? FRecvBaud - {12'h0, BRecvBaudNZ} : 13'h0);

 assign BRecvBitIdx =
  (BRecvGo[1] ? 4'h8 : 4'h0) |
  (ACfgRxEn ? FRecvBitIdx-{3'h0, BRecvNextBit} : 4'h0);

 assign BRecvReg = BRecvNextBit ? {FRxBuf[0], FRecvReg[7:1]} : FRecvReg;

 assign AFifoRecvData = FRecvReg;
 assign AFifoRecvWr   = BRecvNextByte;

 // Baud part
 wire [15:0] BBaudResultA;
 wire BBaudUpdateA;

 UartAutobaud55A UAutobaud
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARx(FRxBuf),
   .ABaudUpdate(ABaudUpdate), .ABaudResult(ABaudO),
   .ATest(ATest)
  );


 // Common part
 assign ATx = ~(FSendReg[0] & ACfgTxEn);
endmodule

module UartAutobaud55A
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] ARx,
  output ABaudUpdate, output [15:0] ABaudResult,
  output [7:0] ATest
 );

 // Local vars
 wire FActive, BActive;
 wire [12:0] FSLen, BSLen;
 wire [3:0] FBitIdx, BBitIdx;
 wire [15:0] FBaud, BBaud;
 wire [12:0] FCmp, BCmp;

 MsDffList #(.CRegLen(1+13+4+16+13)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BActive, BSLen, BBitIdx, BBaud, BCmp}),
   .ADataO({FActive, FSLen, FBitIdx, FBaud, FCmp})
  );

 wire BEdgeR = (ARx==2'b01);
 wire BEdgeF = (ARx==2'b10);
 wire BEdgeX = BEdgeR | BEdgeF;

 wire BSLenOvf = &FSLen;
 wire BBaudOvf = &FBaud;

 wire [13:0] BDeltaA = {1'h0, FCmp} - {1'h0, FSLen};
 wire [12:0] BDeltaM = BDeltaA[13] ? ~BDeltaA[12:0]+13'h1 : BDeltaA[12:0];
 wire BBitLenOK = (FBitIdx==4'h0) | // Ignore BitLen for 1st bit, because it is not yet measured
                  //((FSLen>{4'h0, ATol, 1'h0}) & (BDeltaM<{5'h0, ATol}));
                  ((FSLen>13'h4) & (BDeltaM<{7'h0, FCmp[12:2]}));
 //wire BBitLenViol = (FBitIdx!=4'h0) & (FSLen>(FCmp+{5'h0, ATol}));
 wire BBitLenViol = (FBitIdx!=4'h0) & (FSLen>(FCmp+{7'h0, FCmp[12:2]}));

 assign BSLen = BEdgeX ? 13'h1 : FSLen+{12'h0, ~BSLenOvf};

 wire BStart = ~FActive & BEdgeF;
 wire BStopAny = (BEdgeX & (~BBitLenOK | FBitIdx[3])) | BSLenOvf | BBaudOvf | BBitLenViol;

 assign BActive = BStart | (FActive & ~BStopAny);

 assign BBitIdx = FActive ? FBitIdx + {3'h0, BEdgeX} : 4'h0;
 assign BBaud   = BStart ? 16'h1 : FBaud + {15'h0, FActive & ~BStopAny & ~BBitIdx[3]};

 assign BCmp = {13{FActive}} &
   (
    ((BEdgeX & (FBitIdx==4'h0)) ? FBaud[12:0] : 13'h0) |
    ((BEdgeX & (FBitIdx==4'h1)) ? FBaud[13:1] : 13'h0) |
    ((BEdgeX & (FBitIdx==4'h3)) ? FBaud[14:2] : 13'h0) |
    ((BEdgeX & (FBitIdx==4'h7)) ? FBaud[15:3] : 13'h0) |
    ((~BEdgeX | (FBitIdx!=4'h0) | (FBitIdx==4'h1) | (FBitIdx==4'h3) | (FBitIdx==4'h7)) ? FCmp : 13'h0)
   );

 assign ABaudUpdate = FActive & BEdgeX & FBitIdx[3] & BBitLenOK & ~BSLenOvf & ~BBaudOvf;
 assign ABaudResult = FBaud - 16'h1;
 assign ATest = {ARx[0], BEdgeX, ABaudUpdate, FActive, BBitLenOK, BSLenOvf, BBaudOvf, BBitLenViol};
endmodule

module UartFlush
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ASync1M,
  input ABusy, output AFlush,
  output [7:0] ATest
 );

 // Local variables
 wire [1:0] FBusy, BBusy;
 wire [1:0] FTimer, BTimer;
 wire FTimerNZ, BTimerNZ;
 wire FFlush, BFlush;

 MsDffList #(.CRegLen(2+2+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BBusy, BTimer, BTimerNZ, BFlush}),
   .ADataO({FBusy, FTimer, FTimerNZ, FFlush})
  );

 // Process
 assign BBusy = {FBusy[0], ABusy};
 assign BTimerNZ = |FTimer;
 assign BTimer = (FBusy==2'b10) ? 2'h3 : FTimer - {1'h0, BTimerNZ & ASync1M};
 //assign BFlush = FFlush ^ ({FTimerNZ, BTimerNZ}==2'b10);
 assign BFlush = ~FTimerNZ;

 // Output
 assign AFlush = FFlush;

 assign ATest = {AClkH, ASync1M, FFlush, FTimerNZ, FBusy, FTimer};
endmodule


