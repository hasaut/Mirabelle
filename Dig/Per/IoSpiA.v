module IoSpi #(parameter CAddrBase=16'h0000, CFifoAddrLen=5)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  input wire ASync1M, input wire ASync1K, output wire AIrq, ASrq,
  input wire AExtTrig,
  output wire ACodecEn, output wire AIsMaster, output wire AHwNcs, output wire AReadyS, input wire ASckI, output wire ASckO, input wire ANcsI, output wire ANcsO, input wire AMiso, output wire AMosi,
  input wire [3:0] AGpioI, output wire [3:0] AGpioO, output wire [3:0] AGpioE,
  output wire [15:0] ATest
 );

 // Interface
 // IowCtrl   +0 ; // WR: ByteOrder 3xSdrl HwNcs NcsPol 2xMode | CodecEn IsMaster 2xTimerSrc ClkPersists RFU 2xIrqEn
 //                // RD: TrigTNZ   3xSdrl HwNcs NcsPol 2xMode | CodecEn IsMaster SendBusy TimerNZ 2xRFU CanWrite CanRead
 // IobTrig   +0 ; // WR: 7xRFU UseTrig (once triggered, the bit UseTrig will be cleared)
 // IobSckD   +1 ; // WR/RD: SCK delay (after NCS is active), in BaudRate
 // IowBaud   +1 ; // WR: Duty[7:0] Baud[7:0] | Duty indicates the position of rising edge and must be Baud/2+1 for 50% (for ex if Baud=3, then Duty=2)
 // IowSize   +1 ; // RD: 12x0 SendFreeSize[3:0] 12x0 RecvFillSize[3:0] // Max 8 bytes are indicated, but may be more depending on FIFO size
 // IodTxti   +1 ; // WR/RD: Transmission trigger interval; Used to perform the transmission with defined intervals.
 // IoqSdds   +1 ; // WR/RD: Slave default data to send (sent when master reads, but Slave FifoSend is empty)
 // IoxData   +2 ; // WR/RD: Data
 // IowTOut   +3 ; // WR: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
 //              ; // RD: TimerThis
 // IobGpio   +3 ; // WR: 4xGpioE 4xGpioO
                   // RD: 4xRFU   4xGpioI

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h6666FFFE)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Local vars (Config)
 wire FDebug, BDebug;
 wire [15:0] FCtrl, BCtrl;
 wire FUseTrig, BUseTrig;
 wire [ 7:0] FSckD, BSckD;
 wire [15:0] FBaud, BBaud;
 wire [1:0] FFlags, BFlags;
 wire [63:0] FSdds, BSdds;      // Slave default data to send
 wire FReadyS, BReadyS;
 wire [23:0] FTrigI, BTrigI;    // Transmission trigger interval
 wire [23:0] FTrigT, BTrigT;    // Transmission trigger timer
 wire FTrigTNZ, BTrigTNZ;       // Used to determine if data was written before transmission was triggered. Otherwise data will stuck in the buffer and will never be sent

 MsDffList #(.CRegLen(1+16+1+8+16+2+64+1+24+24+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDebug, BCtrl, BUseTrig, BSckD, BBaud, BFlags, BSdds, BReadyS, BTrigI, BTrigT, BTrigTNZ}),
   .ADataO({FDebug, FCtrl, FUseTrig, FSckD, FBaud, FFlags, FSdds, FReadyS, FTrigI, FTrigT, FTrigTNZ})
  );

 assign BDebug = BIoAccess[IoSizeW+IoOperR+1];

 // Aliases
 wire LByteOrder   = FCtrl[15];
 wire [2:0] LSdrl  = FCtrl[14:12]; // Slave defauld repeat length (for example, if SDRL==3, then data will repeat each 4 bytes)
 wire LHwNcs       = FCtrl[11];
 wire LNcsPol      = FCtrl[10];
 wire [1:0] LMode  = FCtrl[9:8];
 wire LCodecEn     = FCtrl[ 7];
 wire LIsMaster    = FCtrl[ 6];
 wire LSckPersists = FCtrl[ 3];
 wire [1:0] LIrqEn = FCtrl[1:0];

 // Interface
 assign BCtrl    =  BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[15:0] : FCtrl;
 assign BUseTrig = (BIoAccess[IoSizeB+IoOperW+0] & AIoMosi[0]) | (FUseTrig & ~AExtTrig);
 assign BSckD    =  BIoAccess[IoSizeB+IoOperW+1] ? AIoMosi[ 7:0] : FSckD;
 assign BBaud    =  BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FBaud;
 assign BTrigI   =  BIoAccess[IoSizeD+IoOperW+1] ? AIoMosi[23:0] : FTrigI;
 assign BSdds    =  BIoAccess[IoSizeQ+IoOperW+1] ? AIoMosi[63:0] : FSdds;

 wire [63:0] BRecvFifo;
 wire BSendBusy;
 wire [3:0] BSendFree, BRecvFill;
 wire [15:0] BTimerThis; wire BTimerNZ;
 wire [7:0] BGpioI;

 wire [3:0] BFifoWr = {BIoAccess[IoSizeQ+IoOperW+2], BIoAccess[IoSizeD+IoOperW+2], BIoAccess[IoSizeW+IoOperW+2], BIoAccess[IoSizeB+IoOperW+2]};
 wire [3:0] BFifoRd = {BIoAccess[IoSizeQ+IoOperR+2], BIoAccess[IoSizeD+IoOperR+2], BIoAccess[IoSizeW+IoOperR+2], BIoAccess[IoSizeB+IoOperR+2]};

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+0] ? {48'h0, FTrigTNZ, FCtrl[14:8], FCtrl[7:6], BSendBusy, BTimerNZ, FCtrl[3], 1'b0, FFlags} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, FSckD} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+1] ? {48'h0, 4'h0, BSendFree, 4'h0, BRecvFill} : 64'h0) |
  (BIoAccess[IoSizeD+IoOperR+1] ? {32'h0, 8'h0, FTrigI} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+3] ? {56'h0, BGpioI} : 64'h0) |
  (BFifoRd[3] ? (LByteOrder ? {BRecvFifo[7:0], BRecvFifo[15:8], BRecvFifo[23:16], BRecvFifo[31:24], BRecvFifo[39:32], BRecvFifo[47:40], BRecvFifo[55:48], BRecvFifo[63:56]} : BRecvFifo) : 64'h0) |
  (BFifoRd[2] ? (LByteOrder ? {32'h0, BRecvFifo[7:0], BRecvFifo[15:8], BRecvFifo[23:16], BRecvFifo[31:24]} : BRecvFifo) : 64'h0) |
  (BFifoRd[1] ? (LByteOrder ? {48'h0, BRecvFifo[7:0], BRecvFifo[15:8]} : BRecvFifo) : 64'h0) |
  (BFifoRd[0] ? (LByteOrder ? {56'h0, BRecvFifo[7:0]} : BRecvFifo) : 64'h0);

 // Fifo (Send)
 wire [63:0] BIoMosiD = AIoMosi;
 wire [63:0] BIoMosiR =
  (BFifoWr[3] ? {AIoMosi[7:0], AIoMosi[15:8], AIoMosi[23:16], AIoMosi[31:24], AIoMosi[39:32], AIoMosi[47:40], AIoMosi[55:48], AIoMosi[63:56]} : 64'h0) |
  (BFifoWr[2] ? {32'h0, AIoMosi[7:0], AIoMosi[15:8], AIoMosi[23:16], AIoMosi[31:24]} : 64'h0) |
  (BFifoWr[1] ? {48'h0, AIoMosi[7:0], AIoMosi[15:8]} : 64'h0) |
  (BFifoWr[0] ? {56'h0, AIoMosi[7:0]} : 64'h0);

 wire [63:0] BIoMosiX = LByteOrder ? BIoMosiR : BIoMosiD;

 wire [7:0] BFifoTest;

 wire BSendHasData, BSendPick; wire [7:0] BSendData;
 PerifFifoSend #(.CAddrLen(CFifoAddrLen)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BIoMosiX), .AWrSize(BFifoWr),
   .ADataO(BSendData), .ARdEn(BSendPick),
   .AClr(~LCodecEn), .AHasData(BSendHasData), .AFreeSize(BSendFree),
   .ATest(BFifoTest[7:4])
  );

 // Fifo (Recv)
 wire [7:0] BRecvData; wire BRecvNow;
 PerifFifoRecv #(.CAddrLen(CFifoAddrLen)) UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BRecvData), .AWrEn(BRecvNow),
   .ADataO(BRecvFifo), .ARdSize(BFifoRd),
   .AClr(~LCodecEn), .AHasSpace(), .AFillSize(BRecvFill),
   .ATest(BFifoTest[3:0])
  );

 // Flags
 assign BFlags = {|BSendFree, |BRecvFill};
  
 // Interval timer
 wire BTrigINZ = |FTrigI;
 assign BTrigTNZ = |FTrigT;
 wire BTrigTLoad = (BTrigINZ &  FUseTrig &  AExtTrig & BSendHasData) |
                   (BTrigINZ & ~FUseTrig & ~BTrigTNZ & BSendHasData);
 assign BTrigT = BTrigTLoad ? FTrigI : FTrigT-{23'h0, BTrigTNZ};

 // Codec
 wire BTxTrig = FUseTrig ? AExtTrig & BSendHasData : (BTrigINZ ? ~BTrigTNZ & BSendHasData : 1'b1);
 wire [7:0] BCodecTest;
 SpiAsyncCodec UCodec
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ANcsPol(LNcsPol), .ASpiMode(LMode), .ACodecEn(LCodecEn), .AIsMaster(LIsMaster), .ASckPersists(LSckPersists), .ABaud(FBaud),
   .ASckI(ASckI), .ANcsI(ANcsI), .ASckO(ASckO), .ANcsO(ANcsO), .AMiso(AMiso), .AMosi(AMosi),
   .ASdds(FSdds), .ASdrl(LSdrl), .ASckDelay(FSckD), .ATxTrig(BTxTrig),
   .ASendData(BSendData), .ASendHasData(BSendHasData), .ASendPick(BSendPick), .ASendBusy(BSendBusy),
   .ARecvData(BRecvData), .ARecvNow(BRecvNow),
   .ATest(BCodecTest)
  );

 // Process (Timer)
 PerifTimer UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FCtrl[5:4]), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(BSendBusy | BRecvNow), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Gpio
 PerifGpio UGpio
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[7:0]), .AIoMiso(BGpioI), .AIoWrRdEn({BIoAccess[IoSizeB+IoOperW+3], BIoAccess[IoSizeB+IoOperR+3]}),
   .AGpioI(AGpioI), .AGpioO(AGpioO), .AGpioE(AGpioE)
  );

 // Process
 assign BReadyS = BSendHasData;

 // Common part
 assign AIrq = |(FFlags & LIrqEn);
 assign ASrq = 1'b0;
 assign ACodecEn = LCodecEn;
 assign AIsMaster = LIsMaster;
 assign AHwNcs = LHwNcs;
 assign AReadyS = FReadyS;

 assign ATest =
  {
   //BFifoTest,
   //BCodecTest,
   AClkH, LCodecEn, |BFifoWr, |BRecvFill, BTxTrig, BSendHasData, BSendPick, BSendBusy,
   BRecvNow, |BFifoRd, {2{ANcsO}} | ~AGpioO[1:0], AMosi, AMiso, ASckO, ANcsO
  };
endmodule

module SpiAsyncCodec
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire ANcsPol, input wire [1:0] ASpiMode, input wire ACodecEn, AIsMaster, ASckPersists, input wire [15:0] ABaud,
  input wire ASckI, input wire ANcsI, output wire ASckO, output wire ANcsO, input wire AMiso, output wire AMosi,
  input wire [63:0] ASdds, input wire [2:0] ASdrl, input wire [7:0] ASckDelay, input wire ATxTrig,
  input wire [7:0] ASendData, input wire ASendHasData, output wire ASendPick, output wire ASendBusy,
  output wire [7:0] ARecvData, output wire ARecvNow,
  output wire [7:0] ATest
 );

 // Process (Mainly baud generation)
 localparam CStLen  = 3;
 localparam IStHead = 2;
 localparam IStData = 1;
 localparam IStTail = 0;

 wire [CStLen-1:0] FState, BState;
 wire [7:0] FBaudDiv, BBaudDiv;
 wire [2:0] FBitIdx, BBitIdx;
 wire [7:0] FSckDelay, BSckDelay;
 wire FRecvClkS, BRecvClkS; // Slow CLK (there are 2 CLKs: Slow and Fast. They are muxed to produce a RecvClk. Fast = ClkH, there is a special way to generate it. Slow is this one: a usual divider)
 wire FMasterCS, BMasterCS;
 wire FSendHasData, BSendHasData;
 wire FDataMsbBuf, BDataMsbBuf;

 MsDffList #(.CRegLen(CStLen+8+3+8+1+1+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BState, BBaudDiv, BBitIdx, BSckDelay, BRecvClkS, BMasterCS, BSendHasData, BDataMsbBuf}),
   .ADataO({FState, FBaudDiv, FBitIdx, FSckDelay, FRecvClkS, FMasterCS, FSendHasData, FDataMsbBuf})
  );

 //wire BBitIdxNZ  = |FBitIdx;
 wire BBitIdxE   = &FBitIdx;
 wire BBaudDivNZ = |FBaudDiv;
 wire BSckDelayNZ = |FSckDelay;

 wire BStateNZ = |FState;

 wire [CStLen-1:0] BStGo, BStStay;
 assign BStGo[IStHead] =  AIsMaster & FSendHasData & ATxTrig & ~BStateNZ;              assign BStStay[IStHead] = BBaudDivNZ |  BSckDelayNZ;
 assign BStGo[IStData] = (FState[IStHead] & ~BBaudDivNZ & ~BSckDelayNZ) |
                         (FState[IStData] &  BBitIdxE & ~BBaudDivNZ &  ASendHasData);  assign BStStay[IStData] = BBaudDivNZ | ~BBitIdxE;
 assign BStGo[IStTail] =  FState[IStData] &  BBitIdxE & ~BBaudDivNZ & ~ASendHasData;   assign BStStay[IStTail] = BBaudDivNZ;

 assign BState = BStGo | (FState & BStStay);

 assign BSckDelay = BStGo[IStHead] ? ASckDelay : FSckDelay - {7'h0, BSckDelayNZ & ~BBaudDivNZ};
 wire BLoadBaudDiv = ASckPersists ? ~BBaudDivNZ :
                                    (|BStGo[IStHead:IStTail]) | (|FState[IStHead:IStData] & ~BBaudDivNZ);
 assign BBaudDiv  = BLoadBaudDiv ? ABaud[7:0] : FBaudDiv - {7'h0, BBaudDivNZ};
 assign BBitIdx   = BStGo[IStData] ? 3'h0 : FBitIdx + {2'h0, ~BBaudDivNZ & FState[IStData]};
 assign BRecvClkS = ((ASckPersists | FState[IStData]) & BBaudDivNZ) ? (FBaudDiv==ABaud[15:8]) | FRecvClkS : 1'b0;
 assign BMasterCS = ASckPersists ? BState[IStData] : BStateNZ;
 assign BSendHasData = ASendHasData;
 assign BDataMsbBuf = ASendData[7];

 // CLK
 wire BBaudNZ = |ABaud;
 wire BRecvClkF;
 MsClkEnN URecvClkF
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AClkEn((ASckPersists | FState[IStData]) & ~BBaudNZ), .AClkO(BRecvClkF)
  );
 assign ASckO = (FRecvClkS | BRecvClkF) ^ ASpiMode[1];

 wire BRecvClk = (AIsMaster ? ASckO : ASckI) ^ ASpiMode[1] ^ ASpiMode[0];
 wire BSendClk = ~BRecvClk;

 wire BNcsI = (AIsMaster ? ANcsO : ANcsI) ^ ANcsPol;

 // Recv part
 wire [7:0] FRecvBuf, BRecvBuf;
 wire [2:0] FBitIdxR, BBitIdxR;
 wire FSendFromFifo, BSendFromFifo;

 MsDffList #(.CRegLen(8+3+1)) ULocalVarsS
  (
   .AClkH(BRecvClk), .AResetHN(ACodecEn & ~BNcsI), .AClkHEn(1'b1),
   .ADataI({BRecvBuf, BBitIdxR, BSendFromFifo}),
   .ADataO({FRecvBuf, FBitIdxR, FSendFromFifo})
  );

 wire BBitIdxRNZ = |FBitIdxR;
 wire BBitIdxRE  = &FBitIdxR;
 assign BRecvBuf = {FRecvBuf[6:0], AMiso};
 assign BBitIdxR = FBitIdxR + 3'h1;
 assign BSendFromFifo = BBitIdxRNZ ? FSendFromFifo : FSendHasData;

 MsCrossDS #(.CRegLen(8)) UDataO
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AClkS(BRecvClk), .AResetSN(ACodecEn),
   .ADataI(BRecvBuf), .ADataO(ARecvData),
   .AReqS(BBitIdxRE), .AAckH(ARecvNow)
  );

 // Send part
 wire [6:0] FSendBuf, BSendBuf;
 wire [2:0] FBitIdxS, BBitIdxS;
 wire [2:0] FSdri, BSdri;       // Slave default repeat index
 MsDffList #(.CRegLen(7+3+3)) USendBuf
  (
   .AClkH(BSendClk), .AResetHN(ACodecEn & ~BNcsI), .AClkHEn(1'b1),
   .ADataI({BSendBuf, BBitIdxS, BSdri}),
   .ADataO({FSendBuf, FBitIdxS, FSdri})
  );

 wire BBitIdxSNZ = |FBitIdxS;
 wire BSdriNZ = |FSdri;
 wire BFifoPickUp = ~BBitIdxSNZ & FSendFromFifo & ~BSdriNZ;

 MsCrossSync UPickUp
  (
   .AClkA(BSendClk), .AResetAN(ACodecEn), .AClkAEn(1'b1),
   .AClkB(AClkH), .AResetBN(AResetHN), .AClkBEn(AClkHEn),
   .AReqA(BFifoPickUp), .AAckB(ASendPick)
  );

 wire [31:0] BSddsMuxC = FSdri[2] ?     ASdds[63:32] :     ASdds[31: 0];
 wire [15:0] BSddsMuxB = FSdri[1] ? BSddsMuxC[31:16] : BSddsMuxC[15: 0];
 wire [ 7:0] BSddsMuxA = FSdri[0] ? BSddsMuxB[15: 8] : BSddsMuxB[ 7: 0];

 assign BBitIdxS = FBitIdxR;
 assign BSdri = (FBitIdxS==3'h1) ? ((FSdri==ASdrl) ? 3'h0 : FSdri + {2'h0, ~FSendFromFifo}) : FSdri;
 assign BSendBuf = BBitIdxSNZ ? {FSendBuf[5:0], 1'b0} : ((FSendFromFifo & ~BSdriNZ) ? ASendData[6:0] : BSddsMuxA[6:0]);

 assign AMosi = BBitIdxSNZ ? FSendBuf[6] : (FSendHasData ? FDataMsbBuf : ASdds[7]);
 assign ANcsO = ~(FMasterCS ^ ANcsPol);
 assign ASendBusy = FMasterCS;

 assign ATest = {AClkH, BRecvClk, ASckPersists, BLoadBaudDiv, 4'h0};
endmodule

module MsClkEnN
 (
  input AClkH, input AResetHN, input AClkHEn,
  input AClkEn, output AClkO
 );

 wire FClkA, BClkA;
 wire FClkB, BClkB;

 MsDffList #(.CRegLen(1)) UClkA
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BClkA}), .ADataO({FClkA})
  );

 MsDffList #(.CRegLen(1)) UClkB
  (
   .AClkH(~AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BClkB), .ADataO(FClkB)
  );

 assign BClkA = FClkB;
 assign BClkB = FClkB ^ AClkEn;

 assign AClkO = FClkA ^ FClkB;
endmodule

