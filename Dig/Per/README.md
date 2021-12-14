# Peripheral units

## Peripheral address space

The addressing of peripheral units is not al all the same as addressing memory. 
The byte, word, dword access to the same address may have different meaning and correspond to different functionality. 
Misaligned word and dword access will not cause align error like in the case of memory.
This is made mainly to save the address space occupied by the peripheral blocks.

Usually each block has a so called "base address". The addressing of instantiated blocks is presented as an offset from this base address.

## LIN

+-+-+-+
|Register name|Offset|Description|
|and size     |      |           |
+-+-+-+
||||
+-+-+-+

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