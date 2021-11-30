;// DUT
IowDutCtrl      equ 0x00 ; // Wr: 2xRFU hpor hpori porb_vaux_d testwu iddq_out Clk10K_en | RFU PhyStat HPorB LPorB ScopeExit HvdigOk ClkEn1M ClkEn24M
                         ; // Rd: FCtrl[15:0]
IodVersion      equ 0x00 ; // Rd: Version[31:0]
IobDutRegAddr   equ 0x01 ; // Wr: Address of register
IodDutRegData   equ 0x01 ; // Wr/Rd: Register data (see following table)
IodDutMemAddr   equ 0x02 ; // Wr: Address of memory
IoqDutMemData   equ 0x03 ; // Wr/Rd: Memory data
IowClkDiv1M     equ 0x01 ; // WR: DivI[7:0] DivF[7:0] : Integer/Fractional dividers
IowDutNvop      equ 0x03 ; // WR: Reset NvopReq
                         ; // RD: {NvopReq, NvopAddr} // Addr for 64-bit data


;// PutChar (Debug interface from Amalthea)
IobPutCharCtrl equ 0x04 ; // WR: Nothing | RD: 7xRFU CanRead
IowPutCharAddr equ 0x05 ; // WR/RD: 2xRFU AddrToMonitor
IowPutCharData equ 0x06 ; // WR/RD: 7xRFU Addr[0:0] Data[7:0]

IowSynLAddr     equ 0x08 ; // WR/RD: 2xRFU AddrToMonitor
IoqSynLFifo     equ 0x0A ; // RD: 8xRFU Timer[23:0] PC[15:0] Addr[15:0]
IowSynLFifoSize equ 0x0B ; // RD: FifoSize[15:0]

IowCovAddr         equ 0x10 ; // WR/RD: Coverage mem address
IobCovData         equ 0x10 ; // WR/RD: 16x +2 run | 16x +more run // in order to understand if JXX was executed or not, we mark instruction for PC change by +2 and by +(more than 2) and later compare with disassembled code
IoqClkCnt          equ 0x11 ; // RD: Dut Clk Counter
IobClkCntR         equ 0x11 ; // WR: Write bit 0 to reset DutClkCounter
IodTailAddr        equ 0x12 ; // RD: Actual position, bit 31 is set if overflow
                            ; // WR: Set addr to read registers; Resets RegIdx
IoqTailAddr        equ 0x12 ; // RD: same as IodTailAddr but bits [63:32] indicate the max size; Resets the actual position
IowTailRegs        equ 0x12 ; // RD: Registers

IobMlxDbgCtrl      equ 0x14 ; // +0 IobCtrl      ;// WR: 5xRFU StepOneClk StepMode BreakEn | RD: 7x0 IsBreak
IodMlxDbgBreakThis equ 0x15 ; // +1 IodBreakThis ;// WR/RD: 1x break ptr (used for Step Over)
IodMlxDbgBreakList equ 0x16 ; // +2 IodBreakList ;// WR/RD: X break ptrs (up to 8)
IowMlxDbgRegs      equ 0x17 ; // +3 IowRegs      ;// RD: A PC Y X S {CX CVZN Z} DX DW
IobMlxDbgMaskClr   equ 0x17 ; // +3 IobMaskClr   ;// WR: 0x01 to clear ClkMask

IobAdcCtrl         equ 0x18 ; // +0 IobCtrl      ;// WR: 7xRFU ModeEn | RD: 6x0 IsRamHit IsBreakAdc
IodAdcRunTimer     equ 0x19 ; // +1 IodRunTimer  ;// RD: RunTimer (us based; counts UP when IsBreakAny==0; resets by IobMaskClr) Resets all the time when a new data is written to the port to start ADC. Used by model
IodAdcFifo         equ 0x1A ; // +2 IodAdcFifo   ;// WR: Adc FIFO data
IobAdcMaskClr      equ 0x1B ; // +3 IobMaskClr   ;// WR: 4xRFU ClrFifo ClrTimer ClrRamHit ClrBreakAdc
IowAdcRamAddr      equ 0x1B ; // +3 IowRamAddr   ;// WR: 2xRFU RamAddr[13:0]
IodAdcLogTimer     equ 0x1B ; // +1 IodLogTimer  ;// RD: LogTimer (us based; counts UP when IsBreakAny==0; resets by IobMaskClr) Normally resets once in the beginning and then runs all the time. Used to log data

;// ClkDiv
IowClkDivIda    equ 0x28
IowClkDivMlx    equ 0x24

;// TimerA 16-bit
IobTimerACtrl    equ 0x30 ; // IobCtrl   +0 ; // W: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU IrqAEn
                          ; //              ; // R: 2xRFU 2xEn(11=CLK 10=1M 01=1K 00=OFF) RFU RstA RFU CmpA
IowTimerACmpA    equ 0x31 ; // IowCmpA   +1
IobTimerAIrqR    equ 0x33 ; // IobIrqR   +3 ; // Write bit 0 to reset IrqA
IowTimerAThis    equ 0x33 ; // IowThis   +3 ; // RD: FCounter

;// TimerB 16-bit (Used by Term)
IobTTermCtrl    equ 0x34
IowTTermCmpA    equ 0x35
IowTTermPres    equ 0x36
IobTTermIrqR    equ 0x37

;// Term
IobTermCtrl     equ 0x50 ; // WR: TxEn RxEn 2xTimerSrc 2Stop TimerAutoRstEn 2xIrqEn
                         ; // RD: TxEn RxEn RFU TimerNZ SendBusy BaudUpdate CanWrite CanRead
IowTermBaud     equ 0x51 ; // WR/RD: Baud rate
IobTermData     equ 0x52 ; // WR/RD: Data
IowTermTOut     equ 0x53 ; // WR: TOut (Starts from this value and decrementing until zero. Starts at reception, transmission and writing to this port)
                         ; // RD: TimerThis

;// Scpi
IobScpiCtrl     equ 0x54 ; // WR: TxEn RxEn 2xTimerSrc 2Stop TimerAutoRstEn 2xIrqEn
                         ; // RD: TxEn RxEn 2xRFU SendEmpty RecvEmpty CanWrite CanRead
IowScpiBaud     equ 0x55 ; // WR/RD: Baud rate
IobScpiData     equ 0x56 ; // WR/RD: Data
IowScpiTOut     equ 0x57 ; // WR/RD: TimeOut
IobScpiGpio     equ 0x57 ; // WR: 4xGpioE 4xGpioO
                         ; // RD: 4xRFU   4xGpioI

;// Sent
IowSentCtrl     equ 0x98 ; // W: NLen[3:0] RFU NibCnt[2:0] _ TxEn RxEn AddCrc RFU BIrqEn[3:0]
                         ; // R: NLen[3:0] RFU NibCnt[2:0] _ TxEn RxEn CrcErr TimerNZ SSize=0 RSize=0 CanSend CanRecv
IodSentBaud     equ 0x99 ; // W: RecvMaxTickLen[31:20] : SendTickLen[15:4]
                         ; // R: RecvMaxTickLen[31:20] : RecvTickLen[15:4]
IodSentData     equ 0x9A
IowSentPLen     equ 0x9B
IowSentTOut     equ 0x9A ; // Timer starts with either BBSendStateNZ or BBRecvStateNZ and counts down to zero
IowSentPins     equ 0x9B

;// UFL
IobUfmCtrl      equ 0x00E0  ;// IobCtrl = +0; // WR: SpiCS MSB/LSB 2xTimerSrc Mode[1:0] 2xRFU
                            ;//               // RD: SpiCS MSB/LSB Busy TOutNZ Mode[1:0] 2xRFU
IobUfmBaud      equ 0x00E1  ;// IobBaud = +1;
IobUfmData      equ 0x00E2  ;// IobData = +2;
IobUfmGpio      equ 0x00E3  ;// IobGpio = +3; // W GpioE[3:0] GpioO[3:0]
                            ;//               // R 3xRFU GpioI GpioO[3:0]
IowUfmTOut      equ 0x00E3  ;// IowTOut = +3;

;// PinDbg
IobPinDbg       equ 0xF1

;// Board
IowHexData      equ 0xF4
IobLedY         equ 0xF4
IobLedRgb       equ 0xF5
IobLedPwr       equ 0xF6

; // Scope
IobScopeCtrl    equ 0xF8 ; // WR: IrqEn 7xRFU | RD: 4xRFU Stat[3:0] (Stat[4:0]: {Stop Send Meas Sync Wait})
IowScopeDimI    equ 0xF8 ; // WR: VectCnt[2:0] VectLen[3:0] PlotIdx[7:0]
IowScopeBaud    equ 0xF9 ; // WR/RD: Nr of AClkS per sample
IobScopeSMux    equ 0xF9 ; // WR: 16x 64-in mux for each 8x-bit vector
IobScopeTrig    equ 0xFA ; // WR: 0=start aquisition; 1=confirm ready
IowScopePres    equ 0xFB ; // WR/RD: StartTimerPrescaler[15:0]
IodScopeSDel    equ 0xFB ; // WR/RD: StartTimerDelay[31:0]. Set to zero to start immediately

 ; // 3F..20  BRegFile,
 ; //
 ; // 1F 8'h0,
 ; // 1E LCvznReg, LCxReg,
 ; // 1D LSReg[15:8],
 ; // 1C LSReg[ 7:0],
 ; // 1B LXReg[15:8],
 ; // 1A LXReg[ 7:0],
 ; // 19 LYReg[15:8],
 ; // 18 LYReg[ 7:0],
 ; // 17 LPcReg[15:8],
 ; // 16 LPcReg[ 7:0],
 ; // 15 LAReg[15:8],
 ; // 14 LAReg[ 7:0],
 ; // 13 BDbgPins[15:8],
 ; // 12 BDbgPins[ 7:0],
 ; // 11 8'h0,                                     // hporb  lporb     hvdig_ok
 ; // 10 AClkIda, AClkMlx, BClk24M_Dut, BClk1M_Dut, FCtrl[5], FCtrl[4], FCtrl[2], 1'h0
 ; //
 ; // 0F   8'h0,
 ; // 0E   8'h0,
 ; // 0D   8'h0,
 ; // 0C   8'h0,
 ; // 0B   8'h0,
 ; // 0A   8'h0,
 ; // 09   8'h0,
 ; // 08   8'h0,
 ; // 07   8'h0,
 ; // 06   8'h0,
 ; // 05   BLinBTest,
 ; // 04   BLinATest,
 ; // 03   BTestUTest,
 ; // 02   BScopeTest,
 ; // 01   AClkPllA, AClkPllB, AResetPllAN, AClkIda, AResetIdaN, AClkMlx, AResetMlxN, BClkOsc,
 ; // 00   8'h0

;// Debug Ctrl (Iris terminal)
IobDbgTestCtrl       equ 0xFC ; // RD: 4'h0 2'h0 TermSpace 1'b0
IobDbgTermData       equ 0xFE

; // Offsets only

; // Lin_X
IowLinXCtrl       equ 0x00 ; // WR: 2xRFU 2xTimerSrc 2xRFU SenseColl Lin/Uart | TxEn RxEn 6xIrqEn
                           ; // RD: BusStatPend RFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr | StatePend TimerNZ SyncReady Collision SendEmpty RecvEmpty CanWrite CanRead
IowLinXBaud       equ 0x01 ; // Word access is baud rate
IowLinXData       equ 0x02 ;
IodLinXTOut       equ 0x03 ; // Timer counts from this value down to Zero. Resets when next byte is received
IobLinXFlagsBR    equ 0x00 ; // WR: Flags2_Reset: 2xRFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr
IobLinXCfg        equ 0x01 ; // WR: ColWnd:AvgWnd TolBit BrkMin BrkMax BpdMin Master TxTail
IobLinXPidCalc    equ 0x02 ; // WR/RD: PID calculator
IobLinXIrqR       equ 0x03 ; // WR: IrqR IRQ reset
IobLinXSendPid    equ 0x03 ; // RD: Resulting PID (including parity bits)
IodLinXBusStatL   equ 0x02 ; // WR: Set LIN bus LOW for specified duration (in us)
IoqLinXState      equ 0x03 ; // RD: FsmState (for debug only)


; // PpmX
IowPpmXCtrl     equ 0x00 ; // WR: 8xRFU | TxEn RxEn 6xRFU || RD: 8xRFU | TxEn RxEn RxActive TOut SSize=0 RSize=Full CanSend CanRecv
IowPpmXBaud     equ 0x01 ;
IobPpmXData     equ 0x02 ; // WR/RD: 1-bit data FIFO
IodPpmXTOut     equ 0x03 ;



CBaud_921k6     equ  216
CBaud_500k0     equ  399
CBaud_230k4     equ  867
CBaud_115k2     equ 1736
CBaud_57k6      equ 3472

LRomAddr        equ 0x10000000
LRamAddr        equ 0x20000000
LEepromAddr     equ 0x30000000
LEepromCsAddr   equ 0x30000200
LFlashAddr      equ 0x40000000
LLinRomAddr     equ 0x50000000

CMelibusBaud    equ 0x003F
CScopePres1us   equ 24

