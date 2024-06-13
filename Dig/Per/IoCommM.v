module IoSpiM #(parameter CAddrBase=16'h0000)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  input wire ASync1M, ASync1K, output wire AIrq,
  output wire ASpiSck, input wire ASpiMiso, output wire ASpiMosi, output wire ASpiNCS,
  input wire [3:0] ASpiGpioI, output wire [3:0] ASpiGpioO, output wire [3:0] ASpiGpioE,
  input wire AClkDutStopped,
  output wire [15:0] ATest
 );

 // IobCtrl = +0; // WR: SpiCS MSB/LSB 2xTimerSrc Mode[1:0] 2xRFU
 //               // RD: SpiCS MSB/LSB Busy TOutNZ Mode[1:0] 2xRFU
 // IobBaud = +1;
 // IobData = +2;
 // IobGpio = +3; // W GpioE[3:0] GpioO[3:0]
 //               // R GpioI[3:0] GpioO[3:0]
 // IowTOut = +3;

 // Local variables
 wire FOutEn, BOutEn;
 wire FMsbLsb, BMsbLsb;
 wire [1:0] FTimerSrc, BTimerSrc;
 wire [1:0] FMode, BMode;
 wire FBusy, BBusy;
 wire [7:0] FSpiBaud, BSpiBaud;
 wire FSpiSck, BSpiSck;
 wire FSpiMiso, BSpiMiso;
 wire [7:0] FSpiDataO, BSpiDataO;
 wire [7:0] FSpiDataI, BSpiDataI;
 wire [3:0] FBitCount, BBitCount;
 wire [7:0] FBaudDiv, BBaudDiv;

 MsDffList #(.CRegLen(1+1+2+2+1+8+1+1+8+8+4+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BOutEn, BMsbLsb, BTimerSrc, BMode, BBusy, BSpiBaud, BSpiSck, BSpiMiso, BSpiDataO, BSpiDataI, BBitCount, BBaudDiv}),
   .ADataO({FOutEn, FMsbLsb, FTimerSrc, FMode, FBusy, FSpiBaud, FSpiSck, FSpiMiso, FSpiDataO, FSpiDataI, FBitCount, FBaudDiv})
  );

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h000088FF)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 // Interface WR
 assign {BOutEn, BMsbLsb, BTimerSrc, BMode} = BIoAccess[IoSizeB+IoOperW+0] ? AIoMosi[7:2] : {FOutEn, FMsbLsb, FTimerSrc, FMode};
 assign BSpiBaud  = BIoAccess[IoSizeB+IoOperW+1] ? AIoMosi[7:0] : FSpiBaud;

 wire [15:0] BTimerThis; wire BTimerNZ;
 wire [7:0] BGpioI;

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, FOutEn, AClkDutStopped, FBusy, BTimerNZ, FMode, 2'h0} : 64'h0) | //{56'h0, FOutEn, FMsbLsb, FBusy, BTimerNZ, FMode, 2'h0} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, FSpiBaud} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, FSpiDataI} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+3] ? {56'h0, BGpioI} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0);

 // Process
 assign BSpiMiso = ASpiMiso;

 wire BWrData  = BIoAccess[IoSizeB+IoOperW+2];
 wire BBaudDivNZ  = |FBaudDiv;
 wire BBitCountNZ = |FBitCount;
 wire BBitEnd     = BBusy & ~BBaudDivNZ & ~FBitCount[0];

 assign BBaudDiv  = BWrData ? FSpiBaud : (BBaudDivNZ ? FBaudDiv-{7'h0, ~AClkDutStopped} : (BBitCountNZ ? FSpiBaud : 8'h0));
 assign BBitCount = BWrData ? 4'hF : FBitCount-{3'h0, ~BBaudDivNZ & BBitCountNZ};
 assign BBusy = BBitCountNZ | BBaudDivNZ;

 wire [7:0] AIoMosiI = {AIoMosi[0], AIoMosi[1], AIoMosi[2], AIoMosi[3], AIoMosi[4], AIoMosi[5], AIoMosi[6], AIoMosi[7]};
 assign BSpiDataO = BWrData ? (FMsbLsb ? AIoMosi[7:0] : AIoMosiI[7:0]) : (BBitEnd ? {FSpiDataO[6:0], 1'b0} : FSpiDataO);

 // Data _77776666
 //    0 ___--__--
 //    1 _--__--__
 //    2 ---__--__
 //    3 -__--__--

 wire BSpiSckDef  = FMode[1];
 assign BSpiSck   = BWrData ? FMode[1]^FMode[0] : (BBusy ? FSpiSck^(~BBaudDivNZ) : BSpiSckDef);
 wire BDataLatch  = BBusy & ~BBaudDivNZ & FBitCount[0]; //BWrData | BBitEnd;
 assign BSpiDataI = BDataLatch ? (FMsbLsb ? {FSpiDataI[6:0], FSpiMiso} : {FSpiMiso, FSpiDataI[7:1]}) : FSpiDataI;

 // Process (Timer)
 PerifTimer UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FTimerSrc), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(1'b0), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Gpio
 PerifGpio UGpio
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[7:0]), .AIoMiso(BGpioI), .AIoWrRdEn({BIoAccess[IoSizeB+IoOperW+3], BIoAccess[IoSizeB+IoOperR+3]}),
   .AGpioI(ASpiGpioI), .AGpioO(ASpiGpioO), .AGpioE(ASpiGpioE)
  );

 // Outputs
 assign AIrq = 1'b0;
 assign ASpiSck = FSpiSck;
 assign ASpiMosi = FSpiDataO[7];
 assign ASpiNCS = ~FOutEn;
 assign ATest =
  {
   AClkH, BWrData, 6'h0,
   ASpiMosi, ASpiMiso, ASpiSck, ASpiNCS, 1'b0, BDataLatch, BBusy, BTimerNZ
  };
endmodule

// *********************
// *** IoSpi3w
// *********************
// *** supports 3-wire SPI
// *********************

module IoSpi3w
 (
  AClkH, AResetB, AClkHEn,
  AAddr, AMiso, AMosi, AWrEn, ARdEn,
  AIrq,
  ASpiSck, ASpiMiso, ASpiMosi,
  ASpiGpioI, ASpiGpioO,
  ASpiOutEn, // Sck, Mosi, GpioO[1:0]
  ATest
 );

 parameter CAbCtrl = 2'h0; // WR: W/#R, MSB/LSB, Mode[1:0], OutEn[3:0] (Sck, Mosi, GpioO[1:0])
                           // RD: Busy,  TOutNZ, Mode[1:0], OutEn[3:0] (Sck, Mosi, GpioO[1:0])
 parameter CAbBaud = 2'h1;
 parameter CAbData = 2'h2;
 parameter CAbGpio = 2'h3; // 3'h0, GpioI, 2'h0, GpioO[1:0]
 parameter CAwPres = 2'h2;
 parameter CAwTOut = 2'h3; // Read to reset

 // Interface
 input wire AClkH, AResetB, AClkHEn;
 input wire [1:0] AAddr; output wire [15:0] AMiso; input wire [15:0] AMosi;
 input wire [1:0] AWrEn, ARdEn;
 output wire AIrq;
 output wire ASpiSck; input wire ASpiMiso; output wire ASpiMosi;
 input wire ASpiGpioI; output wire [1:0] ASpiGpioO;
 output wire [3:0] ASpiOutEn;
 output wire [7:0] ATest;

 // Local variables
 reg [1:0] FMode; wire [1:0] BMode;
 reg FBusy; wire BBusy;
 reg FWrRd; wire BWrRd;
 reg FMsbLsb; wire BMsbLsb;
 reg [3:0] FSpiOutEn; wire [3:0] BSpiOutEn;
 reg [7:0] FSpiBaud;  wire [7:0] BSpiBaud;
 reg [2:0] FSpiGpioI; wire [2:0] BSpiGpioI;
 reg [1:0] FSpiGpioO; wire [1:0] BSpiGpioO;

 reg [15:0] FPresCfg; wire [15:0] BPresCfg;
 reg [15:0] FTOutCfg; wire [15:0] BTOutCfg;
 reg [15:0] FPres; wire [15:0] BPres;
 reg [15:0] FTOut; wire [15:0] BTOut;
 reg FTimerNZ; wire BTimerNZ;

 reg FSpiSck;  wire BSpiSck;
// reg FSpiMosi; wire BSpiMosi;
 reg FSpiMiso;
 reg [7:0] FSpiDataO; wire [7:0] BSpiDataO;
 reg [7:0] FSpiDataI; wire [7:0] BSpiDataI;
 reg [3:0] FBitCount; wire [3:0] BBitCount;
 reg [7:0] FBaudDiv;  wire [7:0] BBaudDiv;

 // Implementation
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==1'b0)
  begin
  FBusy<=1'b0;
  {FWrRd, FMsbLsb, FMode, FSpiOutEn}<=8'h0;
  FSpiBaud<=8'h0;
  FSpiGpioI<=3'h0; FSpiGpioO<=2'h0;
  FSpiSck<=1'b0;
  //FSpiMosi<=1'b0;
  FSpiMiso<=1'b0;
  FSpiDataO<=8'h0;
  FSpiDataI<=8'h0;
  FBitCount<=4'h0;
  FBaudDiv<=8'h0;
  // Timer
  FPresCfg<=16'h0;
  FTOutCfg<=16'h0;
  FPres<=16'h0;
  FTOut<=16'h0;
  FTimerNZ<=1'b0;
  end
 else if (AClkHEn)
  begin
  FBusy<=BBusy;
  {FWrRd, FMsbLsb, FMode, FSpiOutEn}<={BWrRd, BMsbLsb, BMode, BSpiOutEn};
  FSpiBaud<=BSpiBaud;
  FSpiGpioI<={FSpiGpioI[1:0], ASpiGpioI}; FSpiGpioO<=BSpiGpioO;
  FSpiSck<=BSpiSck;
  //FSpiMosi<=BSpiMosi;
  FSpiMiso<=ASpiMiso;
  FSpiDataO<=BSpiDataO;
  FSpiDataI<=BSpiDataI;
  FBitCount<=BBitCount;
  FBaudDiv<=BBaudDiv;
  // Timer
  FPresCfg<=BPresCfg;
  FTOutCfg<=BTOutCfg;
  FPres<=BPres;
  FTOut<=BTOut;
  FTimerNZ<=BTimerNZ;
  end

 // Aliases
 wire AWrEnB = (AWrEn==2'b01);
 wire ARdEnB = (ARdEn==2'b01);
 wire AWrEnW = (AWrEn==2'b11);
 wire ARdEnW = (ARdEn==2'b11);

 // Interface WR
 assign {BWrRd, BMsbLsb, BMode, BSpiOutEn} = (AWrEnB & (AAddr==CAbCtrl)) ? AMosi[7:0] : {FWrRd, FMsbLsb, FMode, FSpiOutEn};
 assign BSpiBaud  = (AWrEnB & (AAddr==CAbBaud)) ? AMosi[7:0] : FSpiBaud;
 assign BSpiGpioO = (AWrEnB & (AAddr==CAbGpio)) ? AMosi[1:0] : FSpiGpioO;
 assign BPresCfg = (AWrEnW & (AAddr==CAwPres)) ? AMosi[15:0] : FPresCfg;
 assign BTOutCfg = (AWrEnW & (AAddr==CAwTOut)) ? AMosi[15:0] : FTOutCfg;

 // Interface RD
 assign AMiso =
  {
   8'h0,
   ((ARdEnB & (AAddr==CAbCtrl)) ? {FBusy, FTimerNZ, FMode, FSpiOutEn} : 8'h0) |
   ((ARdEnB & (AAddr==CAbBaud)) ? FSpiBaud : 8'h0) |
   ((ARdEnB & (AAddr==CAbData)) ? FSpiDataI : 8'h0) |
   ((ARdEnB & (AAddr==CAbGpio)) ? {3'h0, FSpiGpioI[2], 2'h0, FSpiGpioO} : 8'h0)
  };

 // Process
 wire BWrData  = AWrEnB & (AAddr==CAbData);
 wire BBaudDivNZ  = |FBaudDiv;
 wire BBitCountNZ = |FBitCount;
 //wire BByteEnd    = ~BBaudDivNZ & ~BBitCountNZ;
 wire BBitEnd     = BBusy & ~BBaudDivNZ & ~FBitCount[0];

 assign BBaudDiv  = BWrData ? FSpiBaud : (BBaudDivNZ ? FBaudDiv-8'h1 : (BBitCountNZ ? FSpiBaud : 8'h0));
 assign BBitCount = BWrData ? 4'hF : FBitCount-{3'h0, ~BBaudDivNZ & BBitCountNZ};
 assign BBusy = BBitCountNZ | BBaudDivNZ;

 wire [7:0] AMosiI = {AMosi[0], AMosi[1], AMosi[2], AMosi[3], AMosi[4], AMosi[5], AMosi[6], AMosi[7]};
 assign BSpiDataO = BWrData ? (FMsbLsb ? AMosi[7:0] : AMosiI[7:0]) : (BBitEnd ? {FSpiDataO[6:0], 1'b0} : FSpiDataO);

 // Data _77776666
 //    0 ___--__--
 //    1 _--__--__
 //    2 ---__--__
 //    3 -__--__--

 wire BSpiSckDef  = FMode[1];
 assign BSpiSck   = BWrData ? FMode[1]^FMode[0] : (BBusy ? FSpiSck^(~BBaudDivNZ) : BSpiSckDef);
 wire BDataLatch  = BBusy & ~BBaudDivNZ & FBitCount[0]; //BWrData | BBitEnd;
 assign BSpiDataI = BDataLatch ? (FMsbLsb ? {FSpiDataI[6:0], FSpiMiso} : {FSpiMiso, FSpiDataI[7:1]}) : FSpiDataI;

 // Timer
 wire BTOutReset = ARdEnW & (AAddr==CAwTOut);
 wire BPresNZ = |FPres;
 wire BTOutNZ = |FTOut;
 assign BPres = BTOutReset ? FPresCfg : (BPresNZ ? FPres-16'h1 : {16{BTimerNZ}} & FPresCfg);
 assign BTOut = BTOutReset ? FTOutCfg : FTOut-{15'h0, BTOutNZ & ~BPresNZ};
 assign BTimerNZ = BPresNZ | BTOutNZ;

 // Outputs
 assign AIrq = 1'b0;
 assign ASpiSck = FSpiSck;
 assign ASpiMosi = FSpiDataO[7];
 assign ASpiGpioO = FSpiGpioO;
 assign ASpiOutEn = {FSpiOutEn[3], FSpiOutEn[2] & FBusy & FWrRd, FSpiOutEn[1:0]};
 assign ATest = 8'h0;

endmodule

// *** I2C Master ***
module IoI2cM #(parameter CAddrBase=16'h0000)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, input wire [3:0] AIoRdSize, output wire AIoAddrAck, output wire AIoAddrErr,
  input wire ASync1M, input wire ASync1K, output wire AIrq,
  input wire ASdaI, output wire ASdaO, output wire AScl, output wire AOutEn,
  input wire [3:0] AGpioI, output wire [3:0] AGpioO, output wire [3:0] AGpioE,
  output wire [7:0] ATest
 );

 // IobCtrl +0; // WR: W/#R RFU 2xTimerSrc OutEn RFU  Stop Start
 //             // RD: Busy 2xRFU TOutNZ 4xRFU
 // IobBaud +1;
 // IowData +2;   // 7xRFU ACK Data[7:0]
 // IobGpio +3; // W GpioE[3:0] GpioO[3:0]
 //             // R GpioI[3:0] GpioO[3:0]
 // IowTOut +3; // W TimeOut

 // Local variables
 wire FWrRd, BWrRd;
 wire [1:0] FTimerSrc, BTimerSrc;
 wire FOutEn, BOutEn;
 wire [7:0] FBaud, BBaud;


 MsDffList #(.CRegLen(1+2+1+8)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BWrRd, BTimerSrc, BOutEn, BBaud}),
   .ADataO({FWrRd, FTimerSrc, FOutEn, FBaud})
  );

 wire BFsmBusy;
 wire [8:0] BFsmDataRecv;

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h0000CCBB)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 // Interface WR
 assign {BWrRd, BTimerSrc, BOutEn} = BIoAccess[IoSizeB+IoOperW+0] ? {AIoMosi[7], AIoMosi[5:4], AIoMosi[3]} : {FWrRd, FTimerSrc, FOutEn};
 assign BBaud  = BIoAccess[IoSizeB+IoOperW+1] ? AIoMosi[7:0] : FBaud;

 wire [15:0] BTimerThis; wire BTimerNZ;
 wire [7:0] BGpioI;


 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, BFsmBusy, 2'h0, BTimerNZ, 4'h0} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, FBaud} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+2] ? {55'h0, BFsmDataRecv[8:0]} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+3] ? {56'h0, BGpioI} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+3] ? {48'h0, BTimerThis} : 64'h0);


 IoI2cM_Fsm UFsm
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ABaud(FBaud), .AOutEn(FOutEn),
   .AWrRd(FWrRd),
   .ASendS(BIoAccess[IoSizeB+IoOperW+0] & AIoMosi[0]),
   .ASendP(BIoAccess[IoSizeB+IoOperW+0] & AIoMosi[1]),
   .ASendD(BIoAccess[IoSizeW+IoOperW+2]),
   .ADataSend(AIoMosi[8:0]), .ADataRecv(BFsmDataRecv),
   .ASdaI(ASdaI), .ASdaO(ASdaO), .AScl(AScl),
   .ABusy(BFsmBusy),
   .ATest(ATest)
  );

 // Process (Timer)
 PerifTimer #(.CDataLen(16)) UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[15:0]), .AIoWrEn(BIoAccess[IoSizeW+IoOperW+3]),
   .ASyncSel(FTimerSrc), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(1'b0), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Gpio
 PerifGpio UGpio
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[7:0]), .AIoMiso(BGpioI), .AIoWrRdEn({BIoAccess[IoSizeB+IoOperW+3], BIoAccess[IoSizeB+IoOperR+3]}),
   .AGpioI(AGpioI), .AGpioO(AGpioO), .AGpioE(AGpioE)
  );

 // Outputs
 assign AOutEn = FOutEn;
 assign AIrq = 1'b0;

endmodule

module IoI2cM_Fsm
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] ABaud, input wire AOutEn,
  input wire AWrRd, input wire ASendS, input wire ASendP, input wire ASendD, input wire [8:0] ADataSend, output wire [8:0] ADataRecv,
  input wire ASdaI, output wire ASdaO, output wire AScl,
  output wire ABusy,
  output wire [7:0] ATest
 );

 localparam CStLen = 12;
 localparam CStNil = 12'h0;
 localparam IStStartA =  0;
 localparam IStStartB =  1;
 localparam IStStartC =  2;
 localparam IStStartD =  3;
 localparam IStDataA  =  4;
 localparam IStDataB  =  5;
 localparam IStDataC  =  6;
 localparam IStDataD  =  7;
 localparam IStDataE  =  8;
 localparam IStStopA  =  9;
 localparam IStStopB  = 10;
 localparam IStStopC  = 11;

 // Local variables
 wire [(CStLen-1):0] FState, BState;
 wire FSda, BSda;
 wire FScl, BScl;
 wire FSdaIA, BSdaIA;
 wire FSdaIB, BSdaIB;
 wire [3:0] FBitCount, BBitCount;
 wire [7:0] FBaudDiv, BBaudDiv;
 wire [8:0] FDataO, BDataO;
 wire [8:0] FDataI, BDataI;
 wire FBusy, BBusy;

 MsDffList #(.CRegLen(CStLen+1+1+1+1+4+8+9+9+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BState, BSda, BScl, BSdaIA, BSdaIB, BBitCount, BBaudDiv, BDataO, BDataI, BBusy}),
   .ADataO({FState, FSda, FScl, FSdaIA, FSdaIB, FBitCount, FBaudDiv, FDataO, FDataI, FBusy})
  );

 assign {BSdaIB, BSdaIA} = {FSdaIA, ASdaI};

 // FSM
 wire BStateNZ = |FState;
 wire BBaudDivNZ  = |FBaudDiv;
 wire BLastBit = (FBitCount==4'h8);
 wire [(CStLen-1):0] BEvent;
 assign BEvent[IStStartA] = ~BStateNZ & ASendS;
 assign BEvent[IStStartB] =  FState[IStStartA] & ~BBaudDivNZ;
 assign BEvent[IStStartC] =  FState[IStStartB] & ~BBaudDivNZ;
 assign BEvent[IStStartD] =  FState[IStStartC] & ~BBaudDivNZ;
 assign BEvent[IStDataA]  = ~BStateNZ & ASendD;
 assign BEvent[IStDataB]  =  FState[IStDataA] | (FState[IStDataE] & ~BBaudDivNZ & ~BLastBit);
 assign BEvent[IStDataC]  =  FState[IStDataB] & ~BBaudDivNZ;
 assign BEvent[IStDataD]  =  FState[IStDataC] & ~BBaudDivNZ;
 assign BEvent[IStDataE]  =  FState[IStDataD] & ~BBaudDivNZ;
 assign BEvent[IStStopA]  = ~BStateNZ & ASendP;
 assign BEvent[IStStopB]  =  FState[IStStopA] & ~BBaudDivNZ;
 assign BEvent[IStStopC]  =  FState[IStStopB] & ~BBaudDivNZ;

 assign BState = BEvent | (BBaudDivNZ ? FState : CStNil);

 // Process
 assign BDataO = ASendD ? {ADataSend[7:0], ADataSend[8]} : ((FState[IStDataE] & ~BBaudDivNZ) ? {FDataO[7:0], 1'b1} : FDataO);
 assign BDataI = (FState[IStDataD] & ~BBaudDivNZ) ? {FDataI[7:0], FSdaIB} : FDataI;

 wire BSdaA = AWrRd ? (BLastBit ? 1'b1 : FDataO[8]) : (BLastBit ? FDataO[8] : 1'b1);
 assign BSda = (FState[IStStartB] | FState[IStStopC]) ? 1'b1 :
               (FState[IStStartC] | FState[IStStartD] | FState[IStStopA] | FState[IStStopB]) ? 1'b0 :
               (FState[IStDataB] | FState[IStDataC] | FState[IStDataD] | FState[IStDataE]) ? BSdaA :
               (AWrRd & AOutEn) ? FSda : 1'b1;

 assign BScl = (FState[IStStartB] | FState[IStStartC] | FState[IStStopB] | FState[IStStopC] | FState[IStDataC] | FState[IStDataD]) ? 1'b1 :
               (FState[IStStartD] | FState[IStStopA] | FState[IStDataA] | FState[IStDataB] | FState[IStDataE]) ? 1'b0 :
                AOutEn ? FScl : 1'b1;

 assign BBaudDiv = ((|BEvent) & ~BEvent[IStDataA]) ? ABaud : FBaudDiv - {7'h0, BBaudDivNZ};
 assign BBitCount = FState[IStDataA] ? 4'h0 : FBitCount+{3'h0, FState[IStDataE] & ~BBaudDivNZ};
 assign BBusy = |BState;

 // Outputs
 assign {AScl, ASdaO} = {FScl, FSda};
 assign ABusy = FBusy;
 assign ADataRecv = {FDataI[0], FDataI[8:1]};
 assign ATest = {AScl, ASdaI, ASdaO, ABusy, AWrRd, ASendS, ASendP, ASendD};

endmodule

module SpiFsmMS #(parameter CBaudLen=3, CBaudRate=3'h7)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [7:0] AFsmMosi, output wire [7:0] AFsmMiso, input wire AFsmSend, output wire AFsmBusy, output wire AFsmRecv,
  input wire ASpiMiso, output wire ASpiMosi, output wire ASpiSck
 );

 wire [7:0] FDataS, BDataS;
 wire [3:0] FHalfBitIdx, BHalfBitIdx;
 wire FActive, BActive;
 wire FFsmRecv, BFsmRecv;
 wire [CBaudLen-1:0] FBaudDiv, BBaudDiv;

 MsDffList #(.CRegLen(8+4+1+1+CBaudLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BDataS, BHalfBitIdx, BActive, BFsmRecv, BBaudDiv}),
   .ADataO({FDataS, FHalfBitIdx, FActive, FFsmRecv, FBaudDiv})
  );

 wire BHalfBitIdxE = &FHalfBitIdx;
 wire BBaudDivNZ   = |FBaudDiv;
 wire BBitShift    = FActive & FHalfBitIdx[0] & ~BBaudDivNZ;

 assign BDataS = AFsmSend ? AFsmMosi : (BBitShift ? {FDataS[6:0], ASpiMiso} : FDataS);
 assign BHalfBitIdx = AFsmSend ? 4'h0 : FHalfBitIdx + {3'h0, FActive & ~BBaudDivNZ};
 assign BBaudDiv = (AFsmSend | (FActive & ~BBaudDivNZ & ~BHalfBitIdxE)) ? CBaudRate : FBaudDiv - {{(CBaudLen-1){1'b0}}, BBaudDivNZ};

 assign BFsmRecv = FActive & BHalfBitIdxE & ~BBaudDivNZ;
 assign BActive = AFsmSend | (FActive & ~BFsmRecv);

 assign AFsmBusy = FActive;
 assign AFsmRecv = FFsmRecv;
 assign AFsmMiso = FDataS;

 assign ASpiMosi = FDataS[7];
 assign ASpiSck = FHalfBitIdx[0];

endmodule

module IoMupetM_20200831
 (
  AClkH, AResetB, AClkHEn,
  AAddr, AMiso, AMosi, AWrEn, ARdEn,
  AIrq,
  AMupetSck, AMupetMiso, AMupetMosi,
  AMupetGpioI, AMupetGpioO,
  AMupetOutEn, // Sck, Mosi, GpioO[1:0]
  ATest
 );

 parameter CAbCtrl = 3'h0; // WR: RFU,  RFU,    2xRFU, OutEn[3:0] (Sck, Mosi, GpioO[1:0])
                           // RD: Busy, TOutNZ, 2xRFU, OutEn[3:0] (Sck, Mosi, GpioO[1:0])
 parameter CAbBaud = 3'h1; // Read to reset Mosi
 parameter CAbCrc  = 3'h2; // WR: Resets | RD: Crc-4
 parameter CAbGpio = 3'h3; // 3'h0, GpioI, 2'h0, GpioO[1:0]
 parameter CAwPres = 3'h2;
 parameter CAwTOut = 3'h3; // Read to reset timer. After reset, timer will count from TOut downto zero
 parameter CAbData8a = 3'h4;
 parameter CAbData4a = 3'h5;
 parameter CAbData2a = 3'h6;
 parameter CAbData1a = 3'h7;

 // Interface
 input wire AClkH, AResetB, AClkHEn;
 input wire [2:0] AAddr; output wire [15:0] AMiso; input wire [15:0] AMosi;
 input wire [1:0] AWrEn, ARdEn;
 output wire AIrq;
 output wire AMupetSck; input wire AMupetMiso; output wire AMupetMosi;
 input wire AMupetGpioI; output wire [1:0] AMupetGpioO;
 output wire [3:0] AMupetOutEn;
 output wire [7:0] ATest;

 // Local variables
 reg FBusy; wire BBusy;
 reg [3:0] FMupetOutEn; wire [3:0] BMupetOutEn;
 reg [7:0] FMupetBaud;  wire [7:0] BMupetBaud;
 reg [2:0] FMupetGpioI; wire [2:0] BMupetGpioI;
 reg [1:0] FMupetGpioO; wire [1:0] BMupetGpioO;

 reg [15:0] FPresCfg; wire [15:0] BPresCfg;
 reg [15:0] FTOutCfg; wire [15:0] BTOutCfg;
 reg [15:0] FPres; wire [15:0] BPres;
 reg [15:0] FTOut; wire [15:0] BTOut;
 reg FTimerNZ; wire BTimerNZ;

 reg FMupetSck;  wire BMupetSck;
// reg FMupetMosi; wire BMupetMosi;
 reg FMupetMiso;
 reg [7:0] FMupetDataO; wire [7:0] BMupetDataO;
 reg [7:0] FMupetDataI; wire [7:0] BMupetDataI;
 reg [3:0] FBitCount; wire [3:0] BBitCount;
 reg [7:0] FBaudDiv;  wire [7:0] BBaudDiv;

 reg [3:0] FCrc; wire [3:0] BCrc;

 // Implementation
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==1'b0)
  begin
  FBusy<=1'b0;
  FMupetOutEn<=4'h0;
  FMupetBaud<=8'h0;
  FMupetGpioI<=3'h0; FMupetGpioO<=2'h0;
  FMupetSck<=1'b0;
  //FMupetMosi<=1'b0;
  FMupetMiso<=1'b0;
  FMupetDataO<=8'h0;
  FMupetDataI<=8'h0;
  FBitCount<=4'h0;
  FBaudDiv<=8'h0;
  // Timer
  FPresCfg<=16'h0;
  FTOutCfg<=16'h0;
  FPres<=16'h0;
  FTOut<=16'h0;
  FTimerNZ<=1'b0;
  // CRC
  FCrc<=4'h0;
  end
 else if (AClkHEn)
  begin
  FBusy<=BBusy;
  FMupetOutEn<=BMupetOutEn;
  FMupetBaud<=BMupetBaud;
  FMupetGpioI<={FMupetGpioI[1:0], AMupetGpioI}; FMupetGpioO<=BMupetGpioO;
  FMupetSck<=BMupetSck;
  //FMupetMosi<=BMupetMosi;
  FMupetMiso<=AMupetMiso;
  FMupetDataO<=BMupetDataO;
  FMupetDataI<=BMupetDataI;
  FBitCount<=BBitCount;
  FBaudDiv<=BBaudDiv;
  // Timer
  FPresCfg<=BPresCfg;
  FTOutCfg<=BTOutCfg;
  FPres<=BPres;
  FTOut<=BTOut;
  FTimerNZ<=BTimerNZ;
  // CRC
  FCrc<=BCrc;
  end

 // Aliases
 wire AWrEnB = (AWrEn==2'b01);
 wire ARdEnB = (ARdEn==2'b01);
 wire AWrEnW = (AWrEn==2'b11);
 wire ARdEnW = (ARdEn==2'b11);

 // Interface WR
 assign BMupetOutEn = (AWrEnB & (AAddr==CAbCtrl)) ? AMosi[3:0] : FMupetOutEn;
 assign BMupetBaud  = (AWrEnB & (AAddr==CAbBaud)) ? AMosi[7:0] : FMupetBaud;
 assign BMupetGpioO = (AWrEnB & (AAddr==CAbGpio)) ? AMosi[1:0] : FMupetGpioO;
 assign BPresCfg = (AWrEnW & (AAddr==CAwPres)) ? AMosi[15:0] : FPresCfg;
 assign BTOutCfg = (AWrEnW & (AAddr==CAwTOut)) ? AMosi[15:0] : FTOutCfg;

 // Interface RD
 assign AMiso =
  {
   8'h0,
   ((ARdEnB & (AAddr==CAbCtrl)) ? {FBusy, FTimerNZ, 2'h0, FMupetOutEn} : 8'h0) |
   ((ARdEnB & (AAddr==CAbBaud)) ? FMupetBaud : 8'h0) |
   ((ARdEnB & (AAddr==CAbCrc))  ? {4'h0, FCrc} : 8'h0) |
   ((ARdEnB & (AAddr==CAbGpio)) ? {3'h0, FMupetGpioI[2], 2'h0, FMupetGpioO} : 8'h0) |
   ((ARdEnB & (AAddr==CAbData8a)) ? FMupetDataI : 8'h0) |
   ((ARdEnB & (AAddr==CAbData4a)) ? { 4'h0, FMupetDataI[3:0]} : 8'h0) |
   ((ARdEnB & (AAddr==CAbData2a)) ? { 6'h0, FMupetDataI[1:0]} : 8'h0) |
   ((ARdEnB & (AAddr==CAbData1a)) ? { 7'h0, FMupetDataI[0]} : 8'h0)
  };

 // Process
 wire [3:0] BWrData = {4{AWrEnB}} & {AAddr==CAbData8a, AAddr==CAbData4a, AAddr==CAbData2a, AAddr==CAbData1a};
 wire BWrDataNZ = |BWrData;
 wire BBaudDivNZ  = |FBaudDiv;
 wire BBitCountNZ = |FBitCount;
 //wire BByteEnd    = ~BBaudDivNZ & ~BBitCountNZ;
 wire BBitEnd     = BBusy & ~BBaudDivNZ & ~FBitCount[0];

 wire [3:0] BBitCountA =
  (BWrData[3] ? 4'hF : 4'h0) |
  (BWrData[2] ? 4'h7 : 4'h0) |
  (BWrData[1] ? 4'h3 : 4'h0) |
  (BWrData[0] ? 4'h1 : 4'h0);

 assign BBaudDiv  = BWrDataNZ ? FMupetBaud : (BBaudDivNZ ? FBaudDiv-8'h1 : (BBitCountNZ ? FMupetBaud : 8'h0));
 assign BBitCount = BWrDataNZ ? BBitCountA : FBitCount-{3'h0, ~BBaudDivNZ & BBitCountNZ};
 assign BBusy = BBitCountNZ | BBaudDivNZ;

 wire [7:0] BMupetDataOA =
  (BWrData[3] ? AMosi[7:0] : 8'h0) |
  (BWrData[2] ? {AMosi[3:0], 4'h0} : 8'h0) |
  (BWrData[1] ? {AMosi[1:0], 6'h0} : 8'h0) |
  (BWrData[0] ? {AMosi[0], 7'h0} : 8'h0);

 wire BMupetDataORst = ARdEnB & (AAddr==CAbBaud);

 assign BMupetDataO = BMupetDataORst ? 8'h0 : (BWrDataNZ ? BMupetDataOA : (BBitEnd ? {FMupetDataO[6:0], 1'b0} : FMupetDataO));

 // Data _77776666
 //    0 ___--__--  <- only this mode is used
 //    1 _--__--__
 //    2 ---__--__
 //    3 -__--__--

 assign BMupetSck   = BWrData ? 1'b0 : (BBusy ? FMupetSck^(~BBaudDivNZ) : 1'b0);
 wire BDataLatch  = BBusy & ~BBaudDivNZ & FBitCount[0];
 assign BMupetDataI = BDataLatch ? {FMupetDataI[6:0], FMupetMiso} : FMupetDataI;

 wire BCrcReset = AWrEnB & (AAddr==CAbCrc);
 wire [3:0] BCrcA = {1'b0, FCrc[3:1]} ^ ({4{FCrc[0] ^ FMupetDataO[7]}} & 4'b1001);
 assign BCrc = BCrcReset ? 4'h0 : (BDataLatch ? BCrcA : FCrc);

 // Timer
 wire BTOutReset = ARdEnW & (AAddr==CAwTOut);
 wire BPresNZ = |FPres;
 wire BTOutNZ = |FTOut;
 assign BPres = BTOutReset ? FPresCfg : (BPresNZ ? FPres-16'h1 : {16{BTimerNZ}} & FPresCfg);
 assign BTOut = BTOutReset ? FTOutCfg : FTOut-{15'h0, BTOutNZ & ~BPresNZ};
 assign BTimerNZ = BPresNZ | BTOutNZ;

 // Outputs
 assign AIrq = 1'b0;
 assign AMupetSck = FMupetSck;
 assign AMupetMosi = FMupetDataO[7];
 assign AMupetGpioO = FMupetGpioO;
 assign AMupetOutEn = FMupetOutEn;
 assign ATest = 8'h0;

endmodule

module IoMupetM #(parameter CAddrBase=16'h0000)
 (
  input wire AClkH, AResetHN, AClkHEn,
  input wire [15:0] AIoAddr, output wire [63:0] AIoMiso, input wire [63:0] AIoMosi, input wire [3:0] AIoWrSize, AIoRdSize, output wire AIoBusy, AIoAddrAck, AIoAddrErr,
  input wire ASync1M, ASync1K, output wire AIrq,
  output wire AMupetSck, input wire AMupetMiso, output wire AMupetMosi,
  input wire [3:0] AGpioI, output wire [3:0] AGpioO, AGpioE,
  output wire [7:0] ATest
 );

 // IobCtrl = +0; // WR: RFU UseTimer TimerSrc[1:0] 4xRFU
 //               // RD: 2xRFU Busy TOutNZ 2xRFU CanWrite CanRead
 // IowBaud = +1; // WR: Baud rate
                  // RD: Read to reset Mosi
 // IobCrc  = +1; // WR: Resets CRC block | RD: Crc4
 // IobData = +2; // WR/RD: Transmission/Reception data buffer
 // IodTOut = +3; // Read to reset timer. After reset, timer will count from TOut downto zero
 // IobGpio = +3; // WR: GpioE[3:0] GpioO[3:0]
 //               // RD: 4x0 GpioI[3:0]

 // Local variables
 wire [15:0] FMupetBaud, BMupetBaud;
 wire [1:0] FTimerSrc, BTimerSrc;
 wire FUseTimer, BUseTimer;

 wire FBusy, BBusy;
 wire [3:0] FTckState, BTckState;
 wire FMupetSck, BMupetSck;
 wire FMupetMosi, BMupetMosi;
 wire FMupetMiso, BMupetMiso;
 wire [3:0] FCrc, BCrc;
 wire [15:0] FBaudDiv, BBaudDiv;


 MsDffList #(.CRegLen(16+2+1+1+4+1+1+1+4+16)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BMupetBaud, BTimerSrc, BUseTimer, BBusy, BTckState, BMupetSck, BMupetMosi, BMupetMiso, BCrc, BBaudDiv}),
   .ADataO({FMupetBaud, FTimerSrc, FUseTimer, FBusy, FTckState, FMupetSck, FMupetMosi, FMupetMiso, FCrc, FBaudDiv})
  );

 // Common
 wire BSendMosi, BSendRdEn;
 wire BSendHasData, BSendHasSpace;
 wire BRecvWrEn; wire BRecvMisoA;
 wire BRecvHasData, BRecvHasSpace;

 // Interface
 wire [31:0] BIoAccess;

 IoIntf2s #(.CAddrBase(CAddrBase), .CAddrUsed(32'h008822FF)) UIntf
  (
   .AIoAddr(AIoAddr), .AIoWrSize(AIoWrSize), .AIoRdSize(AIoRdSize),
   .AIoAccess(BIoAccess),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );
 assign AIoBusy = 1'b0;

 localparam IoSizeQ = 3*8;
 localparam IoSizeD = 2*8;
 localparam IoSizeW = 1*8;
 localparam IoSizeB = 0*8;
 localparam IoOperW = 4;
 localparam IoOperR = 0;

 // Interface WR
 assign {BUseTimer, BTimerSrc} = BIoAccess[IoSizeB+IoOperW+0] ? {AIoMosi[6], AIoMosi[5:4]} : {FUseTimer, FTimerSrc};
 assign BMupetBaud  = BIoAccess[IoSizeW+IoOperW+1] ? AIoMosi[15:0] : FMupetBaud;

 wire [31:0] BTimerThis; wire BTimerNZ;
 wire [7:0] BGpioI;

 // Interface RD
 assign AIoMiso =
   (BIoAccess[IoSizeB+IoOperR+0] ? {56'h0, 2'h0, FBusy, BTimerNZ, 2'h0, BSendHasSpace, BRecvHasData} : 64'h0) |
   (BIoAccess[IoSizeW+IoOperR+1] ? {48'h0, FMupetBaud} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+1] ? {56'h0, 4'h0, FCrc} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+2] ? {56'h0, 7'h0, BRecvMisoA} : 64'h0) |
   (BIoAccess[IoSizeD+IoOperR+3] ? {32'h0, BTimerThis} : 64'h0) |
   (BIoAccess[IoSizeB+IoOperR+3] ? {56'h0, BGpioI} : 64'h0);


 // Process
 MsFifoDff #(.CAddrLen(4), .CDataLen(1)) UFifoSend
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(AIoMosi[0]), .AWrEn(BIoAccess[IoSizeB+IoOperW+2]),
   .ADataO(BSendMosi), .ARdEn(BSendRdEn),
   .AClr(1'b0), .AHasData(BSendHasData), .AHasSpace(BSendHasSpace), .ADataSize()
  );

 MsFifoDff #(.CAddrLen(4), .CDataLen(1)) UFifoRecv
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(FMupetMiso), .AWrEn(BRecvWrEn),
   .ADataO(BRecvMisoA), .ARdEn(BIoAccess[IoSizeB+IoOperR+2]),
   .AClr(1'b0), .AHasData(BRecvHasData), .AHasSpace(BRecvHasSpace), .ADataSize()
  );

 wire BBaudDivNZ = |FBaudDiv;
 //wire BBitPosMid = FBusy & ~BBaudDivNZ & FTckState[1];
 wire BBitPosEnd = FBusy & ~BBaudDivNZ & FTckState[3];
 wire BGoStart = ~FBusy & BSendHasData & (~FUseTimer | ~BTimerNZ);

 assign BSendRdEn = BGoStart | (BSendHasData & BBitPosEnd);
 assign BTckState = BBaudDivNZ ? FTckState : {FTckState[2:0], BSendRdEn};
 assign BMupetMosi = BSendRdEn ? BSendMosi : FMupetMosi & BBusy;
 assign BBusy = |BTckState;
 assign BMupetSck = |BTckState[2:1];
 assign BRecvWrEn = BBitPosEnd;
 assign BMupetMiso = AMupetMiso;

 wire [3:0] BCrcA = {1'b0, FCrc[3:1]} ^ ({4{FCrc[0] ^ FMupetMosi}} & 4'b1001);
 assign BCrc = BGoStart ? 4'h0 : (BBitPosEnd ? BCrcA : FCrc);

 assign BBaudDiv = (BSendRdEn | (~BBaudDivNZ & (|BTckState))) ? FMupetBaud : FBaudDiv-{15'h0, BBaudDivNZ};

 // Timer
 PerifTimer #(.CDataLen(32)) UTimer
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[31:0]), .AIoWrEn(BIoAccess[IoSizeD+IoOperW+3]),
   .ASyncSel(FTimerSrc), .ASync1M(ASync1M), .ASync1K(ASync1K),
   .ATimerReset(BIoAccess[IoSizeD+IoOperR+3]), .ACountEn(1'b1), .ATimerThis(BTimerThis), .ATimerNZ(BTimerNZ)
  );

 // Gpio
 PerifGpio UGpio
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AIoMosi(AIoMosi[7:0]), .AIoMiso(BGpioI), .AIoWrRdEn({BIoAccess[IoSizeB+IoOperW+3], BIoAccess[IoSizeB+IoOperR+3]}),
   .AGpioI(AGpioI), .AGpioO(AGpioO), .AGpioE(AGpioE)
  );

 // Outputs
 assign AIrq = 1'b0;
 assign AMupetSck = FMupetSck;
 assign AMupetMosi = FMupetMosi;
 assign ATest = {AClkH, AGpioO[0], BSendRdEn, BRecvWrEn, FBusy, FMupetSck, FMupetMosi, FMupetMiso};
endmodule


