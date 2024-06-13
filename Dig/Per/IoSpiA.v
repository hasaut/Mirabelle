module IoSpi #(parameter CAddrBase=16'h0000, CFifoAddrLen=5)
 (
  input wire AClkH, AResetHN, AClkHEn, input wire AScanI, output wire AScanO, input wire AScanE,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  input wire ASync1M, input wire ASync1K, output wire AIrq,
  output wire ACodecEn, output wire AIsMaster, output wire AHwNcs, output wire AReadyS, input wire ASckI, output wire ASckO, input wire ANcsI, output wire ANcsO, input wire AMiso, output wire AMosi,
  input wire [3:0] AGpioI, output wire [3:0] AGpioO, output wire [3:0] AGpioE,
  output wire [7:0] ATest
 );

 // Interface
 // IowCtrl   +0 ; // WR: RFU 3xSdrl HwNcs NcsPol 2xMode | CodecEn IsMaster 2xTimerSrc 2xRFU 2xIrqEn
 //                // RD: RFU 3xSdrl HwNcs NcsPol 2xMode | CodecEn IsMaster TimerNZ SendBusy 2xRFU CanWrite CanRead
 // IodSize   +0 ; // RD: SendFreeSize RecvFillSize
 // IowBaud   +1 ; // WR/RD: Baud rate
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
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'hCC44BBCC)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 // Scan
 localparam CScanLen     = 6;
 localparam IScanSelf    = 5;
 localparam IScanFifoTX  = 4;
 localparam IScanFifoRX  = 3;
 localparam IScanCodec   = 2;
 localparam IScanTimer   = 1;
 localparam IScanGpio    = 0;

 wire [CScanLen-1:0] BScanO, BScanI; assign {AScanO, BScanI} = {BScanO, AScanI};

 // Local vars (Config)
 wire [14:0] FCtrl, BCtrl;
 wire [15:0] FBaud, BBaud;
 wire [1:0] FFlags, BFlags;
 wire [63:0] FSdds, BSdds;      // Slave default data to send
 wire FReadyS, BReadyS;

 OzhDffList #(.CRegLen(15+16+2+64+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanSelf]), .AScanO(BScanO[IScanSelf]), .AScanE(AScanE),
   .ADataI({BCtrl, BBaud, BFlags, BSdds, BReadyS}),
   .ADataO({FCtrl, FBaud, FFlags, FSdds, FReadyS})
  );

 // Aliases
 wire [2:0] LSdrl  = FCtrl[14:12]; // Slave defauld repeat length (for example, if SDRL==3, then data will repeat each 4 bytes)
 wire LHwNcs       = FCtrl[11];
 wire LNcsPol      = FCtrl[10];
 wire [1:0] LMode  = FCtrl[9:8];
 wire LCodecEn     = FCtrl[ 7];
 wire LIsMaster    = FCtrl[ 6];
 wire [1:0] LIrqEn = FCtrl[1:0];

 // Interface
 assign BCtrl    = BIoAccess[IoSizeW+IoOperW+0] ? AIoMosi[14:0] : FCtrl;
 assign BBaud    = BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FBaud;
 assign BSdds    = BIoAccess[IoSizeQ+IoOperW+1] ? AIoMosi[63:0] : FSdds;

 wire [63:0] BRecvFifo;
 wire BSendBusy;
 wire [15:0] BSendFree, BRecvFill;
 wire [15:0] BTimerThis; wire BTimerNZ;
 wire [7:0] BGpioI;

 wire BAnyFifoWr = |{BIoAccess[IoSizeB+IoOperW+2], BIoAccess[IoSizeW+IoOperW+2], BIoAccess[IoSizeD+IoOperW+2], BIoAccess[IoSizeQ+IoOperW+2]};
 wire BAnyFifoRd = |{BIoAccess[IoSizeB+IoOperR+2], BIoAccess[IoSizeW+IoOperR+2], BIoAccess[IoSizeD+IoOperR+2], BIoAccess[IoSizeQ+IoOperR+2]};

 assign AIoMiso =
  (BIoAccess[IoSizeW+IoOperR+0] ? {48'h0, 1'h0, FCtrl[14:8], FCtrl[7:6], BTimerNZ, BSendBusy, 2'h0, FFlags} : 64'h0) |
  (BIoAccess[IoSizeD+IoOperR+0] ? {32'h0, BSendFree, BRecvFill} : 64'h0) |
  (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0) |
  (BIoAccess[IoSizeB+IoOperR+3] ? {56'h0, BGpioI} : 64'h0) |
  (BAnyFifoRd ? BRecvFifo : 64'h0);

 // Fifo (Send)
 wire BSendHasData, BSendPick; wire [7:0] BSendData;
 PerifFifoSend #(.CAddrLen(CFifoAddrLen)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanFifoTX]), .AScanO(BScanO[IScanFifoTX]), .AScanE(AScanE),
   .AResetSN(LCodecEn),
   .ADataI(AIoMosi), .AWrSize({4{BAnyFifoWr}} & AIoWrSize),
   .ADataO(BSendData), .ARdEn(BSendPick),
   .AHasData(BSendHasData), .AHasSpace(BFlags[1]), .AFreeSize(BSendFree)
  );

 // Fifo (Recv)
 wire [7:0] BRecvData; wire BRecvNow;
 PerifFifoRecv #(.CAddrLen(CFifoAddrLen)) UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanFifoRX]), .AScanO(BScanO[IScanFifoRX]), .AScanE(AScanE),
   .AResetSN(LCodecEn),
   .ADataI(BRecvData), .AWrEn(BRecvNow),
   .ADataO(BRecvFifo), .ARdSize({4{BAnyFifoRd}} & AIoRdSize),
   .AHasData(BFlags[0]), .AHasSpace(), .AFillSize(BRecvFill)
  );

 // Codec
 SpiAsyncCodec UCodec
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanCodec]), .AScanO(BScanO[IScanCodec]), .AScanE(AScanE),
   .ANcsPol(LNcsPol), .ASpiMode(LMode), .ACodecEn(LCodecEn), .AIsMaster(LIsMaster), .ABaud(FBaud),
   .ASckI(ASckI), .ANcsI(ANcsI), .ASckO(ASckO), .ANcsO(ANcsO), .AMiso(AMiso), .AMosi(AMosi),
   .ASdds(FSdds), .ASdrl(LSdrl),
   .ASendData(BSendData), .ASendHasData(BSendHasData), .ASendPick(BSendPick), .ASendBusy(BSendBusy),
   .ARecvData(BRecvData), .ARecvNow(BRecvNow)
  );

 // Process (Timer)
 PerifTimer UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanTimer]), .AScanO(BScanO[IScanTimer]), .AScanE(AScanE),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FCtrl[5:4]), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(BSendBusy | BRecvNow), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Gpio
 PerifGpio UGpio
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanGpio]), .AScanO(BScanO[IScanGpio]), .AScanE(AScanE),
   .AIoMosi(AIoMosi[7:0]), .AIoMiso(BGpioI), .AIoWrRdEn({BIoAccess[IoSizeB+IoOperW+3], BIoAccess[IoSizeB+IoOperR+3]}),
   .AGpioI(AGpioI), .AGpioO(AGpioO), .AGpioE(AGpioE)
  );

 // Process
 assign BReadyS = BSendHasData;

 // Common part
 assign AIrq = |(FFlags & LIrqEn);
 assign ACodecEn = LCodecEn;
 assign AIsMaster = LIsMaster;
 assign AHwNcs = LHwNcs;
 assign AReadyS = FReadyS;

 assign ATest = {ACodecEn, AIsMaster, ASckI, ASckO, ANcsI, ANcsO, AMiso, AMosi};
endmodule

module SpiAsyncCodec
 (
  input wire AClkH, AResetHN, AClkHEn, input wire AScanI, output wire AScanO, input wire AScanE,
  input wire ANcsPol, input wire [1:0] ASpiMode, input wire ACodecEn, input wire AIsMaster, input wire [15:0] ABaud,
  input wire ASckI, input wire ANcsI, output wire ASckO, output wire ANcsO, input wire AMiso, output wire AMosi,
  input wire [63:0] ASdds, input wire [2:0] ASdrl,
  input wire [7:0] ASendData, input wire ASendHasData, output wire ASendPick, output wire ASendBusy,
  output wire [7:0] ARecvData, output wire ARecvNow
 );

 // Scan
 localparam CScanLen    = 4;
 localparam IScanSelf   = 3;
 localparam IScanClk    = 2;
 localparam IScanPickUp = 1;
 localparam IScanDataO  = 0;

 wire [CScanLen-1:0] BScanO, BScanI; assign {AScanO, BScanI} = {BScanO, AScanI};

 // Process (Mainly baud generation)
 localparam CMasterStLen  = 3;
 localparam IMasterStHead = 2;
 localparam IMasterStData = 1;
 localparam IMasterStTail = 0;

 wire [CMasterStLen-1:0] FMasterState, BMasterState;
 wire [15:0] FBaudDiv, BBaudDiv;
 wire [2:0] FBitIdx, BBitIdx;
 wire FRecvClkS, BRecvClkS; // Slow CLK (there are 2 CLKs: Slow and Fast. They are muxed to produce a RecvClk. Fast = ClkH, there is a special way to generate it. Slow is this one: a usual divider)
 wire FMasterCS, BMasterCS;
 wire FSendHasData, BSendHasData;
 wire FDataMsbBuf, BDataMsbBuf;

 OzhDffList #(.CRegLen(CMasterStLen+16+3+1+1+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanSelf]), .AScanO(BScanO[IScanSelf]), .AScanE(AScanE),
   .ADataI({BMasterState, BBaudDiv, BBitIdx, BRecvClkS, BMasterCS, BSendHasData, BDataMsbBuf}),
   .ADataO({FMasterState, FBaudDiv, FBitIdx, FRecvClkS, FMasterCS, FSendHasData, FDataMsbBuf})
  );

 //wire BBitIdxNZ  = |FBitIdx;
 wire BBitIdxE   = &FBitIdx;
 wire BBaudDivNZ = |FBaudDiv;

 wire BMasterStateNZ = |FMasterState;

 wire [CMasterStLen-1:0] BMasterEnter;
 assign BMasterEnter[IMasterStHead] =  AIsMaster & FSendHasData & ~BMasterStateNZ;
 assign BMasterEnter[IMasterStData] = (FMasterState[IMasterStHead] & ~BBaudDivNZ) |
                                      (FMasterState[IMasterStData] &  BBitIdxE & ~BBaudDivNZ &  ASendHasData);
 assign BMasterEnter[IMasterStTail] =  FMasterState[IMasterStData] &  BBitIdxE & ~BBaudDivNZ & ~ASendHasData;

 wire [CMasterStLen-1:0] BMasterStay;
 assign BMasterStay[IMasterStHead] = FMasterState[IMasterStHead] &  BBaudDivNZ;
 assign BMasterStay[IMasterStData] = FMasterState[IMasterStData] & (BBaudDivNZ | ~BBitIdxE);
 assign BMasterStay[IMasterStTail] = FMasterState[IMasterStTail] &  BBaudDivNZ;

 assign BMasterState = BMasterEnter | BMasterStay;

 wire BLoadBaudDiv = (|BMasterEnter[IMasterStHead:IMasterStTail]) | (FMasterState[IMasterStData] & ~BBaudDivNZ);
 assign BBaudDiv  = BLoadBaudDiv ? ABaud : FBaudDiv - {15'h0, BBaudDivNZ & (|FMasterState[IMasterStHead:IMasterStTail])};
 assign BBitIdx   = BMasterEnter[IMasterStData] ? 3'h0 : FBitIdx + {2'h0, ~BBaudDivNZ & FMasterState[IMasterStData]};
 assign BRecvClkS = (FMasterState[IMasterStData] & BBaudDivNZ) ? (BBaudDiv=={1'b0, ABaud[15:1]}) | FRecvClkS : 1'b0;
 assign BMasterCS = |BMasterState;
 assign BSendHasData = ASendHasData;
 assign BDataMsbBuf = ASendData[7];

 // CLK
 wire BRecvClkF;
 MdClkEnN URecvClkF
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanClk]), .AScanO(BScanO[IScanClk]), .AScanE(AScanE),
   .AClkEn(FMasterState[IMasterStData] & ~(|ABaud)), .AClkO(BRecvClkF)
  );
 assign ASckO = (FRecvClkS | BRecvClkF) ^ ASpiMode[1];

 wire BRecvClk = (AIsMaster ? ASckO : ASckI) ^ ASpiMode[1] ^ ASpiMode[0];
 wire BSendClk = ~BRecvClk;

 wire BNcsI = (AIsMaster ? ANcsO : ANcsI) ^ ANcsPol;

 // Recv part
 wire [7:0] FRecvBuf, BRecvBuf;
 wire [2:0] FBitIdxR, BBitIdxR;
 wire FSendFromFifo, BSendFromFifo;

 OzhDffList #(.CRegLen(8+3+1)) ULocalVarsS
  (
   .AClkH(BRecvClk), .AResetHN(ACodecEn & ~BNcsI), .AClkHEn(1'b1), .AScanI(1'b0), .AScanO(), .AScanE(1'b0),
   .ADataI({BRecvBuf, BBitIdxR, BSendFromFifo}),
   .ADataO({FRecvBuf, FBitIdxR, FSendFromFifo})
  );

 wire BBitIdxRNZ = |FBitIdxR;
 wire BBitIdxRE  = &FBitIdxR;
 assign BRecvBuf = {FRecvBuf[6:0], AMiso};
 assign BBitIdxR = FBitIdxR + 3'h1;
 assign BSendFromFifo = BBitIdxRNZ ? FSendFromFifo : FSendHasData;

 MdCrossDS #(.CRegLen(8)) UDataO
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanDataO]), .AScanO(BScanO[IScanDataO]), .AScanE(AScanE),
   .AClkS(BRecvClk), .AResetSN(ACodecEn),
   .ADataI(BRecvBuf), .ADataO(ARecvData),
   .AReqS(BBitIdxRE), .AAckH(ARecvNow)
  );

 // Send part
 wire [6:0] FSendBuf, BSendBuf;
 wire [2:0] FBitIdxS, BBitIdxS;
 wire [2:0] FSdri, BSdri;       // Slave default repeat index
 OzhDffList #(.CRegLen(7+3+3)) USendBuf
  (
   .AClkH(BSendClk), .AResetHN(ACodecEn & ~BNcsI), .AClkHEn(1'b1), .AScanI(1'b0), .AScanO(), .AScanE(1'b0),
   .ADataI({BSendBuf, BBitIdxS, BSdri}),
   .ADataO({FSendBuf, FBitIdxS, FSdri})
  );

 wire BBitIdxSNZ = |FBitIdxS;
 wire BSdriNZ = |FSdri;
 wire BFifoPickUp = ~BBitIdxSNZ & FSendFromFifo & ~BSdriNZ;

 MdCrossSync UPickUp
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .AScanI(BScanI[IScanPickUp]), .AScanO(BScanO[IScanPickUp]), .AScanE(AScanE),
   .AClkS(BSendClk), .AResetSN(ACodecEn),
   .AReqS(BFifoPickUp), .AAckH(ASendPick)
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

endmodule


