module IoLin #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  input ASync1M, input ASync1K, output AIrq,
  output ATxEn, output ALinMode,
  input ARx, output ATx,
  output [7:0] ATest
 );


 // Interface
 // IowUartCtrl +0 ; // WR: 2xRFU 2xTimerSrc 2xRFU SenseColl Lin/Uart | TxEn RxEn 6xIrqEn
 //                  // RD: BusStatPend RFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr | StatePend TimerNZ SyncReady Collision SendEmpty RecvEmpty CanWrite CanRead
 // IowUartBaud +1 ; // Word access is baud rate
 // IowUartData +2 ;
 // IodTOut     +3 ; // Timer counts from this value down to Zero. Resets when next byte is received

 // IobFlagsBR  +0 ; // WR: Flags2_Reset: 2xRFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr
 // IobUartCfg  +1 ; // WR: ColWnd:AvgWnd TolBit BrkMin BrkMax BpdMin Master TxTail
 // IobPidCalc  +2 ; // WR/RD: PID calculator
 // IobIrqR     +3 ; // WR: IrqR IRQ reset
 // IobSendPid  +3 ; // RD: Resulting PID (including parity bits)

 // IodBusStatL +2 ; // WR: Set LIN bus LOW for specified duration (in us)
 // IoqState    +3 ; // RD: FsmState (for debug only)

 localparam IAddrCtrl     = 0;
 localparam IAddrBaud     = 1;
 localparam IAddrData     = 2;
 localparam IAddrTOut     = 3;
 localparam IAddrFlagsBR  = 0;
 localparam IAddrUartCfg  = 1;
 localparam IAddrPidCalc  = 2;
 localparam IAddrIrqR     = 3;
 localparam IAddrSendPid  = 3;
 localparam IAddrBusStatL = 2;
 localparam IAddrState    = 3;

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 wire [31:0] BIoAccess;
 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h88C877FC)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam CStLen         = 15;
 localparam CStNil         = 15'h0;
 localparam IStBreakStart  =  0; // 0001
 localparam IStBreakPend   =  1; // 0002
 localparam IStSyncStart   =  2; // 0004
 localparam IStSyncPend    =  3; // 0008
 localparam IStPidStart    =  4; // 0010
 localparam IStPidPend     =  5; // 0020
 localparam IStDataStart   =  6; // 0040
 localparam IStDataPend    =  7; // 0080
 localparam IStRepStart    =  8; // 0100
 localparam IStRepPend     =  9; // 0200
 localparam IStCollision   = 10; // 0400
 localparam IStListStart   = 11; // 0800
 localparam IStListPend    = 12; // 1000
 localparam IStListBaudDet = 13; // 2000
 localparam IStListData    = 14; // 4000

 // Flags description
 localparam IFlagSyncReady = 5;
 localparam IFlagCollision = 4;
 localparam IFlagSendEmpty = 3;
 localparam IFlagRecvEmpty = 2;
 localparam IFlagCanWrite  = 1;
 localparam IFlagCanRead   = 0;

 localparam CCmdSendSof    = 3'b100;
 localparam CCmdListen     = 3'b110;
 localparam CCmdStopListen = 3'b101;
 localparam CCmdSendRep    = 3'b111;

 // MOSI: 1_00PPPPPP = MasterSendSof (Break+Sync+Pid)
 //       1_10xxxxxx = SlaveListen
 //       1_01000000 = StopListen
 //       1_110VCCCC = Send CCCC times value V
 //       0_DDDDDDDD = Data
 //
 // MISO: 1_01000000 = (Brake+Sync) received and baud rate is set
 //       1_01EEEEEE = Error code (05 ListenCancelled, 06 InvalidCmd)
 //       0_DDDDDDDD = Data

 // Local variables
 wire [15:0] FCtrl, BCtrl; // 5xRFU TimeStep1M SenseColl Lin/Uart _ TxEn RxEn 6xIrqEn
 wire [15:0] FBaudRate, BBaudRate; // This is a duration of 8 bits, not of 1 bit
 wire FBaudWr, BBaudWr;

 wire [3:0] FFlagsA, BFlagsA;
 wire [5:0] FFlagsB, BFlagsB;

 // Common
 wire [(CStLen-1):0] FState, BState;
 wire FCollision, BCollision;
 wire FSyncReady, BSyncReady;
 wire FStatePend, BStatePend; // State is not Zero or pending transmission
 // Send part
 wire [5:0] FSendPid, BSendPid;
 wire FTx, BTx;    // FTx is needed to delay ATx for 1 clk for collision calculation
 wire FSendInProgress, BSendInProgress;
 wire FSendSenWnd, BSendSenWnd;
 // Recv part
 wire [1:0] FRxBuf, BRxBuf;
 wire [9:0] BRecvData;
 wire BRecvNextByte;
 wire BStopListen, BMispListen;
 wire BFifoRecvWrOvf; // Write to a full buffer (overflow condition)
 wire [2:0] BRecvAvgErr;
 // Baud detection
 wire [15:0] BBaudRes; wire BBaudDetRes;
 // Checksum
 //wire [7:0] FChs20v; wire [7:0] BChs20v; // 2.0
 //wire [7:0] FChs13v; wire [7:0] BChs13v; // 1.3
 wire [5:0] FPidCalc, BPidCalc; wire [7:0] LPidCalc;
 wire [7:0] LSendPid;
 // BusStat
 wire [23:0] FBusStatCnt, BBusStatCnt;
 wire FBusStatCntNZ, BBusStatCntNZ;
 // Tail
 wire FTxOutEn, BTxOutEn;
 wire [7:0] FTailCnt, BTailCnt;

 MsDffList #(.CRegLen(16+16+1+4+6+CStLen+1+1+1+6+1+1+1+2+6+24+1+1+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCtrl, BBaudRate, BBaudWr, BFlagsA, BFlagsB, BState, BCollision, BSyncReady, BStatePend, BSendPid, BTx, BSendInProgress, BSendSenWnd, BRxBuf, BPidCalc, BBusStatCnt, BBusStatCntNZ, BTxOutEn, BTailCnt}),
   .ADataO({FCtrl, FBaudRate, FBaudWr, FFlagsA, FFlagsB, FState, FCollision, FSyncReady, FStatePend, FSendPid, FTx, FSendInProgress, FSendSenWnd, FRxBuf, FPidCalc, FBusStatCnt, FBusStatCntNZ, FTxOutEn, FTailCnt})
  );

 // Config
 wire [7:0] BCfgSenLen; // 7:4 collision SenWnd, 3:0 average SenWnd | Range = (0.4375..0.0625)
 wire [7:0] BCfgBitTol; // 7:4 RFU, 3:0 Sync (0x55) jitter tolerance | Range = (0.4375..0.0625)
 wire [7:0] BCfgBrkMin; // 7:0 MinBrkLen | Range = (31.875..0.125)
 wire [7:0] BCfgBrkMax; // 7:0 MaxBrkLen | Range = (31.875..0.125)
 wire [7:0] BCfgBpdMax; // 7:0 Max Brk+Dlm Len | Range = (31.875..0.125)
 wire [7:0] BCfgMaster; // 6xRFU 2xStopBrk 2xStopData
 wire [7:0] BCfgTxTail; // TxTail Duration of tain H level

 // Aliases
 wire LStopB       = BCfgMaster[1]; // 2/1 Stop bit for Break
 wire LStopD       = BCfgMaster[0]; // 2/1 Stop bit for Data
 wire [1:0] LTimerSrc = FCtrl[13:12];
 wire LSenseColl   = FCtrl[ 9];
 wire LLinMode     = FCtrl[ 8];
 wire LTxEn        = FCtrl[ 7];
 wire LRxEn        = FCtrl[ 6];
 wire [5:0] LIrqEn = FCtrl[5:0]; // SyncRecv, Collision, SendEmpty, RecvEmpty, CanWrite, CanRead

 // Common Interface
 wire [8:0] BRecvFifo;
 wire [31:0] BTimerThis; wire BTimerNZ;

 // Interface WR
 assign BCtrl     = BIoAccess[IoSizeW+IoOperW+IAddrCtrl] ? AIoMosi[15:0] : FCtrl;
 assign BBaudRate = BIoAccess[IoSizeW+IoOperW+IAddrBaud] ? AIoMosi[15:0] : (BBaudDetRes ? BBaudRes : FBaudRate);
 assign BBaudWr   = BIoAccess[IoSizeW+IoOperW+IAddrBaud] | BBaudDetRes;
 wire BCollisionReset    = BIoAccess[IoSizeB+IoOperW+IAddrIrqR] & AIoMosi[IFlagCollision];
 wire BSyncReadyReset    = BIoAccess[IoSizeB+IoOperW+IAddrIrqR] & AIoMosi[IFlagSyncReady];
 wire [5:0] BFlagsBReset = BIoAccess[IoSizeB+IoOperW+IAddrFlagsBR] ? AIoMosi[5:0] : 6'h0;

 // Config
 PerifCfgBlock #(.CRegLen(8), .CRegCnt(7)) ULinCfg
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[7:0]), .AWrEn(BIoAccess[IoSizeB+IoOperW+3]), .AAnyOtherAccess(|BIoAccess),
   .ADataO({BCfgTxTail, BCfgMaster, BCfgBpdMax, BCfgBrkMax, BCfgBrkMin, BCfgBitTol, BCfgSenLen})
  );

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeW+IoOperR+IAddrCtrl] ? {48'h0, FBusStatCntNZ, 1'h0, FFlagsB, FStatePend, BTimerNZ, FSyncReady, FCollision, FFlagsA} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+IAddrBaud] ? {48'h0, FBaudRate} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+IAddrData] ? {48'h0, 7'h0, BRecvFifo} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+IAddrPidCalc] ? {56'h0, LPidCalc} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+IAddrSendPid] ? {56'h0, LSendPid} : 64'h0) |
   (BIoAccess[IoSizeQ+IoOperR+IAddrState] ? {{(64-CStLen){1'b0}}, FState} : 64'h0) |
   (BIoAccess[IoSizeD+IoOperR+IAddrTOut]  ? {32'h0, BTimerThis} : 64'h0);


 // Fifo (Send)
 wire BSendReq, BSendAck, BSendClr; wire [8:0] BFifoSTop;
 MsFifo4x #(.CDataLen(9)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[8:0]), .AWrEn(BIoAccess[IoSizeW+IoOperW+IAddrData]),
   .ADataO(BFifoSTop), .ARdEn(BSendAck),
   .AClr(BSendClr), .AHasData(BSendReq), .AHasSpace(BFlagsA[1]), .ADataSize()
  );
 assign BFlagsA[3] = BSendReq;

 // Fifo (Recv)
 wire [8:0] BFifoRecvWrData; wire BRecvNow;
 MsFifo4x #(.CDataLen(9)) UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BFifoRecvWrData), .AWrEn(BRecvNow),
   .ADataO(BRecvFifo), .ARdEn(BIoAccess[IoSizeW+IoOperR+IAddrData]),
   .AClr(1'b0), .AHasData(BFlagsA[0]), .AHasSpace(BFlagsA[2]), .ADataSize()
  );

 // BusStat
 assign BBusStatCntNZ = |FBusStatCnt;
 assign BBusStatCnt = BIoAccess[IoSizeD+IoOperW+IAddrBusStatL] ? AIoMosi[23:0] : FBusStatCnt-{23'h0, BBusStatCntNZ & ASync1M};

 // Common
 assign BFlagsB = ~BFlagsBReset & ~{6{FState[IStListStart]}} &
                  (FFlagsB | {BFifoRecvWrOvf, BRecvAvgErr, BRecvNextByte ? {BRecvData[0]==1'b1, BRecvData[9]==1'b0} : 2'h0});

 // PID calc
 assign BPidCalc = BIoAccess[IoSizeB+IoOperW+IAddrPidCalc] ? AIoMosi[5:0] : FPidCalc;
 assign LPidCalc = {~(FPidCalc[5] ^ FPidCalc[4]) ^ (FPidCalc[3] ^ FPidCalc[1]), (FPidCalc[4] ^ FPidCalc[2]) ^ (FPidCalc[1] ^ FPidCalc[0]), FPidCalc};

 // *** Send part
 assign BSendPid = BState[IStBreakStart] ? BFifoSTop[5:0] : FSendPid;
 assign LSendPid = {~(FSendPid[5] ^ FSendPid[4]) ^ (FSendPid[3] ^ FSendPid[1]), (FSendPid[4] ^ FSendPid[2]) ^ (FSendPid[1] ^ FSendPid[0]), FSendPid};

 wire BSendNextByte = BState[IStBreakStart] | BState[IStSyncStart] | BState[IStPidStart] | BState[IStDataStart] | BState[IStRepStart];
 wire [15:0] BSendData =
   (BState[IStBreakStart] ? 16'hE000 : 16'hFFFF) &
   (BState[IStSyncStart]  ? 16'hFEAA : 16'hFFFF) &
   (BState[IStPidStart]   ? {7'h7F, LSendPid, 1'b0} : 16'hFFFF) &
   (BState[IStDataStart]  ? {7'h7F, BFifoSTop[7:0], 1'b0} : 16'hFFFF) &
   (BState[IStRepStart]   ? {16{BFifoSTop[4]}} : 16'hFFFF);
 wire [3:0] BSendBitCnt =
   (BState[IStBreakStart] ? 4'hD+{3'h0, LStopB} : 4'h0) |
   (BState[IStSyncStart]  ? 4'h9+{3'h0, LStopD} : 4'h0) |
   (BState[IStPidStart]   ? 4'h9+{3'h0, LStopD} : 4'h0) |
   (BState[IStDataStart]  ? 4'h9+{3'h0, LStopD} : 4'h0) |
   (BState[IStRepStart]   ? BFifoSTop[3:0] : 4'h0);

 wire BTxNext;
 UartSend UUartSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ABaud(FBaudRate), .ABaudUpdate(FBaudWr),
   .ANextByte(BSendNextByte), .ABitCnt(BSendBitCnt), .AData(BSendData),
   .ACancel(BSendClr), .AInProgress(BSendInProgress), .ATx(BTx), .ATxNext(BTxNext),
   .ASenLen(BCfgSenLen[7:4]), .ASenWnd(BSendSenWnd)
  );

 assign BCollision = (LSenseColl & FSendSenWnd & FSendInProgress & (FTx ^ FRxBuf[0])) | (FCollision & ~BCollisionReset);
 assign BSyncReady = BBaudDetRes | (FSyncReady & ~BSyncReadyReset & ~FState[IStListStart]);
 assign BStatePend = (|BState) | BSendInProgress;

 // Command decoder and Send FIFO management
 assign BStopListen = BSendReq & (BFifoSTop[8:6]==CCmdStopListen);
 assign BMispListen = BSendReq & (BFifoSTop[8:6]==CCmdListen) & (FState!=CStNil); // Misplaced listen
 //wire BInvalidCmd = BSendReq & (BFifoSTop[8:6]==xxxxx);
 assign BSendAck = BState[IStBreakStart] | BState[IStDataStart] | BState[IStRepStart] | BState[IStListStart] |
                   BStopListen | BMispListen;
 assign BSendClr = ~LTxEn | FCollision;

 // Send checksum
 /*wire [8:0] BChs20vA = {1'b0, FChs20v} +
  (
   (BState[IStPidStart]  ? {1'b0, LSendPid} : 9'h0) |
   (BState[IStDataStart] ? {1'b0, BFifoSTop[7:0]} : 9'h0)
  );
 assign BChs20v = BState[IStBreakStart] ? 8'h0 : BChs20vA[7:0] + {7'h0, BChs20vA[8]};

 wire [8:0] BChs13vA = {1'b0, FChs13v} +
  (
   (BState[IStDataStart] ? {1'b0, BFifoSTop[7:0]} : 9'h0)
  );
 assign BChs13v = BState[IStBreakStart] ? 8'h0 : BChs13vA[7:0] + {7'h0, BChs13vA[8]};*/

 // *** Recv part
 UartRecv UUartRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ABaud(FBaudRate), .ABaudUpdate(FBaudWr),
   .ARx(FRxBuf), .AEn(LLinMode ? (FState==CStNil) | FState[IStListData] : LRxEn),
   .AData(BRecvData), .AInProgress(), .AByteEnd(BRecvNextByte),
   .ASenLen(BCfgSenLen[3:0]), .AAvgErr(BRecvAvgErr)
  );

 // Assembling Data and WrEn (for the reception FIFO)
 assign BRxBuf = {FRxBuf[0], ARx};
 assign BFifoRecvWrData =
   (FState[IStListBaudDet] ? 9'h140 : 9'h0) |
   (BStopListen ? 9'h145 : 9'h0) |
   (BMispListen ? 9'h147 : 9'h0) |
   //(BInvalidCmd ? 9'h146 : 9'h0) |
   ((BRecvNextByte & ~(FState[IStListBaudDet] | BStopListen | BMispListen)) ? {1'b0, BRecvData[8:1]} : 9'h0);
 wire BFifoRecvWrEnA = FState[IStListBaudDet] | BStopListen | BMispListen; // Command write (always, even if buf is full)
 assign BRecvNow  = BFifoRecvWrEnA | (BRecvNextByte & BFlagsA[2]);   // Data write (skip if buf is full)
 assign BFifoRecvWrOvf = ~BFlagsA[2] & (BFifoRecvWrEnA | BRecvNextByte); // Write causing overflow (when buf is full)

 // *** Baud detection
 LinBaudDet ULinBaudDet
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ARx(FRxBuf), .AEn(FState[IStListPend] | FState[IStListData]),
   .ABitTol(BCfgBitTol[3:0]),
   .ABrkMin(BCfgBrkMin), .ABrkMax(BCfgBrkMax), .ABpdMax(BCfgBpdMax),
   .ABaud(BBaudRes), .ABaudDetected(BBaudDetRes),
   .ATest()
  );

 // ** FSM **
 // SOF
 assign BState[IStBreakStart]  = (FState==CStNil) & BSendReq & (BFifoSTop[8:6]==CCmdSendSof) & ~FCollision;
 assign BState[IStBreakPend]   = FState[IStBreakStart] | (FState[IStBreakPend] & BSendInProgress & ~FCollision);
 assign BState[IStSyncStart]   = FState[IStBreakPend] & ~BSendInProgress & ~FCollision;
 assign BState[IStSyncPend]    = FState[IStSyncStart] | (FState[IStSyncPend] & BSendInProgress & ~FCollision);
 assign BState[IStPidStart]    = FState[IStSyncPend] & ~BSendInProgress & ~FCollision;
 assign BState[IStPidPend]     = FState[IStPidStart] | (FState[IStPidPend] & BSendInProgress & ~FCollision);
 // Data
 assign BState[IStDataStart]   = ((FState==CStNil) & BSendReq & ~BFifoSTop[8] & ~FCollision) |
                                 (FState[IStPidPend]  & ~BSendInProgress & BSendReq & ~BFifoSTop[8] & ~FCollision) |
                                 (FState[IStDataPend] & ~BSendInProgress & BSendReq & ~BFifoSTop[8] & ~FCollision);
 assign BState[IStDataPend]    = FState[IStDataStart] | (FState[IStDataPend] & BSendInProgress & ~FCollision);
 // Rep
 assign BState[IStRepStart]    = ((FState==CStNil) & BSendReq & (BFifoSTop[8:6]==CCmdSendRep) & ~FCollision) |
                                 (FState[IStRepPend] & ~BSendInProgress & BSendReq & (BFifoSTop[8:6]==CCmdSendRep) & ~FCollision);
 assign BState[IStRepPend]     = FState[IStRepStart] | (FState[IStRepPend] & BSendInProgress & ~FCollision);
 // Errors
 assign BState[IStCollision]   = FCollision;
 // Listener
 assign BState[IStListStart]   = (FState==CStNil) & BSendReq & (BFifoSTop[8:6]==CCmdListen) & ~FCollision;
 assign BState[IStListPend]    = FState[IStListStart] | (FState[IStListPend] & ~BStopListen & ~BBaudDetRes);
 assign BState[IStListBaudDet] = (FState[IStListPend] & ~BStopListen & BBaudDetRes);
 assign BState[IStListData]    = (FState[IStListBaudDet] & ~BStopListen) |
                                 (FState[IStListData] & ~BStopListen);

 // Timer
 PerifTimer #(.CDataLen(32)) UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[31:0]), .AIoWrEn(BIoAccess[IoSizeD+IoOperW+IAddrTOut]),
   .ASyncSel(LTimerSrc), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(FState[IStListStart] | BRecvNextByte), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Tail
 wire BTailCfgNZ = |BCfgTxTail;
 wire BTailCntNZ = |FTailCnt;
 wire BSendEdgeR = {BTx, BTxNext}==2'b01;
 assign BTxOutEn = BTailCfgNZ & (BSendEdgeR | BTailCntNZ);
 assign BTailCnt = BSendEdgeR ? BCfgTxTail : FTailCnt - {7'h0, BTailCntNZ};

 // Common part
 assign AIrq = |(FFlagsA & LIrqEn);
 assign ATx = BTx & ~FBusStatCntNZ;
 assign ATxEn = LTxEn & (~BTx | FTxOutEn);
 assign ALinMode = LLinMode;
 assign ATest = {ARx, ATx, FState[IStListPend], FState[IStListBaudDet], FState[IStListData], BBaudDetRes, BSendNextByte, BRecvNextByte};
endmodule

module LinDivToInc ( ADiv, AInc );
// Interface
 input [2:0] ADiv;
 output [7:0] AInc;
// Implementation
 assign AInc =
  ((ADiv[2:0]==3'h0) ? 8'b00001000 : 8'h0) |
  ((ADiv[2:0]==3'h1) ? 8'b10001000 : 8'h0) |
  ((ADiv[2:0]==3'h2) ? 8'b10100100 : 8'h0) |
  ((ADiv[2:0]==3'h3) ? 8'b10101010 : 8'h0) |
  ((ADiv[2:0]==3'h4) ? 8'b10110110 : 8'h0) |
  ((ADiv[2:0]==3'h5) ? 8'b11101110 : 8'h0) |
  ((ADiv[2:0]==3'h6) ? 8'b11101111 : 8'h0) |
  ((ADiv[2:0]==3'h7) ? 8'b11111111 : 8'h0);
endmodule

// *** Fractional baud rate corrector
// Calculates lengths for individual bits
module LinBaudCorr
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] ABaud, input ABaudUpdate,
  input AStart, input [3:0] ABitCnt, output [3:0] ABitIdx,
  input ACancel,
  input ARxMode,          // In RX mode Stop bit is half a bit
  output ANextBit, output AInProgress,
  input [3:0] ASenLen, // Specifies Delta for sensing collision TX or averaging RX
  output ASenWnd, output ASenWndStart
 );

 // Local var
 wire [ 7:0] FBaudCorr, BBaudCorr; // Baud corrector (ror after sending next bit)
 wire [ 3:0] FBitCnt,   BBitCnt;   // Bit counter
 wire [12:0] FBitLen,   BBitLen;   // Bit length
 wire [12:0] FIndLen,   BIndLen;   // Individual bit length
 wire [10:0] FDelta,    BDelta;    // Calculated delta. 11 bit, because max. value is 1/4 of baud for bit
 wire [10:0] FDeltaDec, BDeltaDec; // Helps calculate window
 wire FSenWndStart, BSenWndStart;

 MsDffList #(.CRegLen(8+4+13+13+11+11+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BBaudCorr, BBitCnt, BBitLen, BIndLen, BDelta, BDeltaDec, BSenWndStart}),
   .ADataO({FBaudCorr, FBitCnt, FBitLen, FIndLen, FDelta, FDeltaDec, FSenWndStart})
  );

 // Implementation
 wire [7:0] BBaudCorrStatic;
 LinDivToInc ULinDivToInc ( .ADiv(ABaud[2:0]), .AInc(BBaudCorrStatic) );

 wire BBitLenNZ = |FBitLen;
 wire BBitCntNZ = |FBitCnt;
 wire BDataPend =  BBitLenNZ | BBitCntNZ;
 wire BNextBit  = ~BBitLenNZ & BBitCntNZ;
 wire BDeltaDecNZ = |FDeltaDec;

 assign BBaudCorr = ABaudUpdate ? BBaudCorrStatic : ((AStart | BNextBit) ? {FBaudCorr[0], FBaudCorr[7:1]} : FBaudCorr);
 wire [12:0] BIndLenA = ABaud[15:3] - {12'h0, ~FBaudCorr[0]}; // Corrected (non-integer)

 wire [18:0] BDeltaA =
  (ASenLen[3] ? {      ABaud[15:0], 3'h0} : 19'h0) +
  (ASenLen[2] ? {1'h0, ABaud[15:0], 2'h0} : 19'h0) +
  (ASenLen[1] ? {2'h0, ABaud[15:0], 1'h0} : 19'h0) +
  (ASenLen[0] ? {3'h0, ABaud[15:0]      } : 19'h0);

 assign BDelta = BDeltaA[18:8];

 assign BIndLen = BIndLenA;
 assign BBitLen = {13{~ACancel}} &
   (
    (AStart ? FIndLen : 13'h0) |
    (BNextBit ? ((ARxMode & (FBitCnt==4'h1)) ? {1'h0, FIndLen[12:1]} : FIndLen) : 13'h0) |
    ((AStart | BNextBit) ? 13'h0 : FBitLen-{12'h0, BBitLenNZ})
   );

 assign BBitCnt = ACancel ? 4'h0 : (AStart ? ABitCnt : FBitCnt-{3'h0, BNextBit});

 assign BDeltaDec = ACancel ? 11'h0 : ((AStart | BNextBit) ? FDelta : FDeltaDec-{10'h0, BDeltaDecNZ});

 assign BSenWndStart = ACancel ? 1'h0 : (FDeltaDec==11'h1);

 assign ABitIdx = ABitCnt - FBitCnt;
 assign ANextBit = BNextBit;
 assign AInProgress = BDataPend;
 assign ASenWnd = ~BDeltaDecNZ & ((FBitLen>=FDelta) | (ARxMode & ~BBitCntNZ));
 assign ASenWndStart = FSenWndStart;
endmodule

// *** Uart transmitter. 13-bit shift reg allows sending Lin break
module UartSend
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] ABaud, input ABaudUpdate,
  input ANextByte, input [3:0] ABitCnt, input [15:0] AData,
  input ACancel, output AInProgress, output ATx, output ATxNext,
  input [3:0] ASenLen, output ASenWnd
 );

 // Local var
 wire [15:0] FSendReg, BSendReg;  // Send shift register

 MsDffList #(.CRegLen(16)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BSendReg}),
   .ADataO({FSendReg})
  );

 // Implementation
 wire BNextBit;
 LinBaudCorr ULinBaudCorr
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ABaud(ABaud), .ABaudUpdate(ABaudUpdate),
   .AStart(ANextByte), .ABitCnt(ABitCnt), .ABitIdx(),
   .ACancel(ACancel),
   .ARxMode(1'b0),
   .ANextBit(BNextBit), .AInProgress(AInProgress),
   .ASenLen(ASenLen), .ASenWnd(ASenWnd), .ASenWndStart()
  );

 assign BSendReg = ACancel ? 16'hFFFF : (ANextByte ? AData : (BNextBit ? {FSendReg[15], FSendReg[15:1]} : FSendReg));

 assign ATxNext = BSendReg[0];
 assign ATx = FSendReg[0];
endmodule

//          \_ 0 1 2 3 4 5 6 7 ~\
// BitLenZ  _|_|_|_|_|_|_|_|_|_|_
// FBitCnt  098877665544332211000
//
// *** Uart receiver
module UartRecv
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] ABaud, input ABaudUpdate,
  input [1:0] ARx, input AEn,            // ARx is not a line input but already a resynchronized shift reg
  output [9:0] AData, output AInProgress, output AByteEnd,
  input [3:0] ASenLen,
  output [2:0] AAvgErr // Data/Start/Stop. 1 when bit is not fully "0" or not fully "1"
 );

 // Local var
 wire [ 8:0] FDataReg, BDataReg;  // Shift register
 wire FInProgress, BInProgress;      // Used only to strobe byte end
 wire [12:0] FAvgCnt, BAvgCnt;
 wire [1:0] FAvgErr, BAvgErr;

 MsDffList #(.CRegLen(9+1+13+2)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDataReg, BInProgress, BAvgCnt, BAvgErr}),
   .ADataO({FDataReg, FInProgress, FAvgCnt, FAvgErr})
  );

 // Implementation
 wire BStart    = AEn & (ARx==2'b10) & ~BInProgress;
 wire [3:0] BBitIdx;
 wire BNextBit, BSenWnd, BSenWndStart;
 LinBaudCorr ULinBaudCorr
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ABaud(ABaud), .ABaudUpdate(ABaudUpdate),
   .AStart(BStart), .ABitCnt(4'h9), .ABitIdx(BBitIdx),
   .ACancel(~AEn),
   .ARxMode(1'b1),
   .ANextBit(BNextBit), .AInProgress(BInProgress),
   .ASenLen(ASenLen), .ASenWnd(BSenWnd), .ASenWndStart(BSenWndStart)
  );

 wire BSenWndA = BSenWnd & FInProgress;
 wire [12:0] BAvgCntP = FAvgCnt + {{12{BSenWndA & ~ARx[1]}}, BSenWndA};
 assign BAvgCnt  = (BStart | BNextBit) ? 13'h0FFF : BAvgCntP;
 assign BDataReg = BNextBit ? {BAvgCntP[12], FDataReg[8:1]} : FDataReg;
 assign BAvgErr  = (BStart | BNextBit) ? 2'h0 : {BSenWndStart ? ARx[1] : FAvgErr[1], BSenWndA ? ARx[1] : FAvgErr[0]};

 assign AData = {BAvgCntP[12], FDataReg[8:0]};
 assign AInProgress = BInProgress;
 assign AByteEnd = ~BInProgress & FInProgress;
 assign AAvgErr = {3{BInProgress}} & {(BBitIdx>4'h0) & (BBitIdx<4'h9), BBitIdx==4'h0, BBitIdx==4'h9} & {3{BAvgErr[1]^BAvgErr[0]}};
endmodule

// *** Lin LenDiv
// * Divides the length of pulse by value
module LinLenDiv
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ACntEn,       // Counter enable (otherwise hold value)
  input AClr,         // Clear / Restart
  input [7:0] ADiv,   // Fractional divider 16 - 0.125
  output [13:0] ARes  // Division result
 );

 // Implementation
 wire  [4:0] FCnt, BCnt;
 wire  [7:0] FInc, BInc;
 wire [13:0] FRes, BRes;

 MsDffList #(.CRegLen(5+8+14)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BCnt, BInc, BRes}),
   .ADataO({FCnt, FInc, FRes})
  );

 wire [7:0] BIncA;
 LinDivToInc ULinDivToInc ( .ADiv(ADiv[2:0]), .AInc(BIncA) );

 wire BResOvf = &FRes;
 wire BCmp = (FCnt==(ADiv[7:3]+{5{~FInc[7]}}));
 assign BCnt = AClr ? 5'h0 : (BCmp ? 5'h0 : FCnt+5'h1);
 assign BInc = AClr ? BIncA : (BCmp ? {FInc[6:0], FInc[7]} : FInc);
 assign BRes = AClr ? 14'h0 : FRes+{13'h0, ACntEn & BCmp & ~BResOvf};

 assign ARes = FRes;
endmodule

module LinSymLen
 (
  input [13:0] ASymLen,
  input  [3:0] ATol,
  output [13:0] AMax, output [13:0] AMin
 );

// Implementation
 wire [17:0] BMax = {ASymLen, 4'h0} +
  (ATol[3] ? {1'h0, ASymLen, 3'h0} : 18'h0) +
  (ATol[2] ? {2'h0, ASymLen, 2'h0} : 18'h0) +
  (ATol[1] ? {3'h0, ASymLen, 1'h0} : 18'h0) +
  (ATol[0] ? {4'h0, ASymLen      } : 18'h0);
 wire [17:0] BMin = {ASymLen, 4'h0} -
  (ATol[3] ? {1'h0, ASymLen, 3'h0} : 18'h0) -
  (ATol[2] ? {2'h0, ASymLen, 2'h0} : 18'h0) -
  (ATol[1] ? {3'h0, ASymLen, 1'h0} : 18'h0) -
  (ATol[0] ? {4'h0, ASymLen      } : 18'h0);
 assign AMax = BMax[17:4];
 assign AMin = BMin[17:4];
endmodule


// *** Lin Baud detector
module LinBaudDet
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [1:0] ARx, input AEn,            // ARx is not a line input but already a resynchronized shift reg
  input [3:0] ABitTol,                   // As a percentage of a calculated bit len. 4 bits. Range = (0.4375..0.0625)
  input [7:0] ABrkMin, input [7:0] ABrkMax, input [7:0] ABpdMax, // As a persantage of a calculated bit len. 8 bits. Range = (31.875..0.125)
  output [15:0] ABaud, output ABaudDetected,
  output [7:0] ATest
 );

 // Parameters
 localparam CStLen   = 7;
 localparam CStNil   = 7'h0;
 localparam IStBreak = 0;
 localparam IStDelim = 1;
 localparam IStDataA = 2;
 localparam IStDataB = 3;
 localparam IStDataC = 4;
 localparam IStDataE = 5;
 localparam IStDecis = 6;

 // LocalVar
 wire [(CStLen-1):0] FState, BState;
 // Baud rate is a length of 8 data_bit, it is 16-bit wide.
 // BitLen[14] (25000/2.400 ~10416, i.e. 2^14=16384 is enough)
 wire [13:0] FSymLen, BSymLen;       // Len of the symbol (any symbol) L or H
 wire [16:0] FLenAcc, BLenAcc;
 wire [13:0] FHslMaxP, BHslMaxP;
 wire [13:0] FHslMinP, BHslMinP;
 wire FBrkDlmPOk, BBrkDlmPOk;
 wire [15:0] FBaud8bS, BBaud8bS;
 wire  [1:0] FBcCnt, BBcCnt;         // Counter of BC pairs
 wire [13:0] FSymLenMin, BSymLenMin; // Bit length comparison Lower (Based on bit A)
 wire [13:0] FSymLenMax, BSymLenMax; // Bit length comparison Higher (Based on bit A)

 MsDffList #(.CRegLen(CStLen+14+17+14+14+1+16+2+14+14)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BState, BSymLen, BLenAcc, BHslMaxP, BHslMinP, BBrkDlmPOk, BBaud8bS, BBcCnt, BSymLenMin, BSymLenMax}),
   .ADataO({FState, FSymLen, FLenAcc, FHslMaxP, FHslMinP, FBrkDlmPOk, FBaud8bS, FBcCnt, FSymLenMin, FSymLenMax})
  );

 // Implementation
 wire BEdgeUp = (ARx==2'b01);
 wire BEdgeDn = (ARx==2'b10);
 wire BEdge = BEdgeUp | BEdgeDn;

 wire BSymLenOvf = &FSymLen;
 wire BDcsPoint = (FSymLen[13:0] == {1'h0, FSymLenMin[13:1]}); // Decision point

 // ** Brk/Dlm geometry
 wire [13:0] BBrkDivMinT, BBrkDivMaxT, BBpdDivMaxT;
 LinLenDiv UBrkDivMin ( .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .ACntEn(~ARx[1]), .AClr(BEdgeDn), .ADiv(ABrkMin), .ARes(BBrkDivMinT) );
 LinLenDiv UBrkDivMax ( .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .ACntEn(~ARx[1]), .AClr(BEdgeDn), .ADiv(ABrkMax), .ARes(BBrkDivMaxT) );
 LinLenDiv UBpdDivMax ( .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), .ACntEn(   1'b1), .AClr(BEdgeDn), .ADiv(ABpdMax), .ARes(BBpdDivMaxT) );
 // ** Comparisons
 wire [13:0] BHslMaxT = BBrkDivMinT;                                           // Min/Max header derived SymLen
 wire [13:0] BHslMinT = (BBrkDivMaxT<BBpdDivMaxT) ? BBrkDivMaxT : BBpdDivMaxT;
 wire BBrkDlmTOk = (BHslMaxT>=BHslMinT) & (FSymLen>=BHslMinT); // SymLen is used to verify a minimal Dlm len
 assign {BHslMaxP, BHslMinP, BBrkDlmPOk} = BEdgeDn ? {BHslMaxT, BHslMinT, BBrkDlmTOk} : {FHslMaxP, FHslMinP, FBrkDlmPOk};
 wire BSymLenOk = (FSymLen>=FSymLenMin) & (FSymLen<=FSymLenMax);
 wire BBrkLenOvf = (&BBrkDivMinT) | (&BBrkDivMaxT);
 wire BBpdLenOvf = (&BBpdDivMaxT) | (BBpdDivMaxT>BBrkDivMinT);

 // \____________/~~~\_/~\_/~\_/~\_/~\_/~\
 //     Break     Dlm A B C B C B C B C E
 //
 // ** State      Error Condition                   Function
 // Brk -> Dlm                                      Brk:=T
 // Dlm -> A            ( BrkDlmTOk)                Dlm:=T
 // Dlm -> Brk    *     (~BrkDlmTOk)
 // A -> B              ( SymLenOk)                 A:=S
 // A -> Dlm      *     (~SymLenOk)                 Brk:=T
 // B -> C              ( SymLenOk)                 B:=S
 // B -> A        *     (~SymLenOk) & ( BrkDlmTOk)  Brk:=T, Dlm:=T
 // B -> Brk      *     (~SymLenOk) & (~BrkDlmTOk)
 // C -> B norm         ( SymLenOk) & (BitCnt<>3)
 // C -> B rst    *     (~SymLenOk) & ( BrkDlmPOk)  Brk:=P, Dlm:=P, A:=S
 // C -> Dlm      *     (~SymLenOk) & (~BrkDlmPOk)  Brk:=S
 // C -> E              ( SymLenOk) & (BitCnt==3)
 // E -> A        *     (Edge) & ( BrkDlmTOk)       Brk:=T, Dlm:=T
 // E -> Brk      *     (Edge) & (~BrkDlmTOk)

 // ** FSM Change
 wire BGoBrkDlm  = AEn & BEdge & FState[IStBreak];
 wire BGoDlmA    = AEn & BEdge & FState[IStDelim] &  BBrkDlmTOk;
 wire BGoDlmBrk  = AEn & BEdge & FState[IStDelim] & ~BBrkDlmTOk;
 wire BGoAB      = AEn & BEdge & FState[IStDataA] &  BSymLenOk;
 wire BGoADlm    = AEn & BEdge & FState[IStDataA] & ~BSymLenOk;
 wire BGoBC      = AEn & BEdge & FState[IStDataB] &  BSymLenOk;
 wire BGoBA      = AEn & BEdge & FState[IStDataB] & ~BSymLenOk &  BBrkDlmTOk;
 wire BGoBBrk    = AEn & BEdge & FState[IStDataB] & ~BSymLenOk & ~BBrkDlmTOk;
 wire BGoCB_norm = AEn & BEdge & FState[IStDataC] &  BSymLenOk & (FBcCnt!=2'h3);
 wire BGoCB_rst  = AEn & BEdge & FState[IStDataC] & ~BSymLenOk &  BBrkDlmPOk;
 wire BGoCDlm    = AEn & BEdge & FState[IStDataC] & ~BSymLenOk & ~BBrkDlmPOk;
 wire BGoCE      = AEn & BEdge & FState[IStDataC] &  BSymLenOk & (FBcCnt==2'h3);
 wire BGoEA      = AEn & BEdge & FState[IStDataE] &  BBrkDlmTOk;
 wire BGoEBrk    = AEn & BEdge & FState[IStDataE] & ~BBrkDlmTOk;

 // ** FSM
 wire BStIsNil = (FState==CStNil);
 wire [(CStLen-1):0] BStateA; // Preliminary BState to optimize AEn
 assign BState = AEn ? BStateA : CStNil;
 assign BStateA[IStBreak] = ((FState==CStNil) & BEdgeDn) | BGoDlmBrk | BGoBBrk | BGoEBrk |
                            (FState[IStBreak] & ~BEdge & ~BBrkLenOvf);
 assign BStateA[IStDelim] =  BGoBrkDlm | BGoADlm | BGoCDlm |
                            (FState[IStDelim] & ~BEdge & ~BBpdLenOvf);
 assign BStateA[IStDataA] =  BGoDlmA | BGoBA | BGoEA |
                            (FState[IStDataA] & ~BEdge & ~BSymLenOvf);
 assign BStateA[IStDataB] =  BGoAB | BGoCB_norm | BGoCB_rst |
                            (FState[IStDataB] & ~BEdge & ~BSymLenOvf);
 assign BStateA[IStDataC] =  BGoBC |
                            (FState[IStDataC] & ~BEdge & ~BSymLenOvf);
 assign BStateA[IStDataE] =  BGoCE |
                            (FState[IStDataE] & ~BEdge & ~BDcsPoint);
 assign BStateA[IStDecis] = (FState[IStDataE] & ~BEdge &  BDcsPoint);

 // ** Counters
 assign BSymLen  = (BStIsNil | BEdge) ? 14'h0 : FSymLen + {13'h0, ~BSymLenOvf};
 assign BLenAcc  = (BStIsNil | BGoDlmA | BGoBA | BGoEA) ? 17'h0 : (BGoCB_rst ? {3'h0, FSymLen} : FLenAcc+17'h1);
 assign BBcCnt   = (BStIsNil | BGoAB | BGoCB_rst) ? 2'h0 : FBcCnt + {1'h0, BGoCB_norm};
 assign BBaud8bS =  BGoBC ? FLenAcc[15:0] : FBaud8bS;

 // ** SymLen comparison geometry
 wire [13:0] BBitLenNom =
  ((FBcCnt==2'h0) ? FLenAcc[14:1] : 14'h0) |
  ((FBcCnt==2'h1) ? FLenAcc[15:2] : 14'h0) |
  ((FBcCnt==2'h3) ? FLenAcc[16:3] : 14'h0);
 wire [13:0] BSymLenMaxA, BSymLenMinA;
 LinSymLen USymLen ( .ASymLen(BBitLenNom), .ATol(ABitTol[3:0]), .AMax(BSymLenMaxA), .AMin(BSymLenMinA) );

 assign {BSymLenMax, BSymLenMin} =
  (BGoDlmA | BGoBA | BGoEA) ? {BHslMaxT, BHslMinT} :
  (BGoCB_rst ? {BHslMaxP, BHslMinP} : ((BGoBC & (FBcCnt!=2'h2)) ? {BSymLenMaxA, BSymLenMinA} : {FSymLenMax, FSymLenMin}));
                                                   //  ^ Because it is difficult to divide FLenAcc by 3

 // IOs
 assign ABaud = FBaud8bS;
 assign ABaudDetected = FState[IStDecis];
 assign ATest = {ARx[0], FState};
endmodule

// V  Prj   Lin  Baud Calc
// A  13106 2134 1745 266
// B  11488 1342  974  55 - FSM + Tolerance calculator
// C   9865  534  197
// D   9974  575  228

// 953
