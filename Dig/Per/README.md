# Peripheral units

## Peripheral address space

The addressing of peripheral units is not al all the same as addressing memory. 
The byte, word, dword access to the same address may have different meaning and correspond to different functionality. 
Misaligned word and dword access will not cause align error like in the case of memory.
This is made mainly to save the address space occupied by the peripheral blocks.

Usually each block has a so called "base address". The addressing of instantiated blocks is presented as an offset from this base address.

## LIN

| LIN block IO ports |||
| Name <br/> and size| Offset | Description |
| :--- | :--- | :--- |
| IowUartCtrl <br/> 16 | +0 | WR: 2xRFU 2xTimerSrc 2xRFU SenseColl Lin/Uart TxEn RxEn 6xIrqEn <br/> RD: BusStatPend RFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr StatePend TimerNZ SyncReady Collision SendEmpty RecvEmpty CanWrite CanRead |
| IowUartBaud <br/> 16 | +1 | Baud rate <br/> this is the length of 8 bits, not of 1 bit. <br/> In Master mode this is a transmission baud rate, in Slave mode this is a detected baud rate (i.e. it can be read) |
| IowUartData <br/> 16 | +2 | WR: Data to be sent <br/> RD: Data received <br/> Though the access is 16-bit, only 9 bit are used. Bits 7:0 are data bits, bit 8 has a special meaning (see following table)  <br/> There is a 4-words FIFO for transmission and a 4-words FIFO for reception |
| IodTOut <br/> 32 | +3 | Time out. Timer counts from this value down to Zero. Resets when next byte is received |
| IobFlagsBR <br/> 8 | +0 | Writing "1" resets the corresponding flag from IowUartCtrl register <br/> WR: Flags2_Reset: 2xRFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr |
| IobUartCfg <br/> 8 | +1 | Lin configuration. There are several configuration regisetrs which need to be written one after another. <br/> There is an internal index which increments each time register is written. Access to any other register (for example IowLinCtrl) resets this index <br/> WR: ColWnd:AvgWnd TolBit BrkMin BrkMax BpdMin Master TxTail <br/> Detailed description is given in a separate table |
| IobPidCalc <br/> 8 | +2 | WR/RD: PID calculator |
| IobIrqR <br/> 8 | +3 | WR: IrqR IRQ reset |
| IobSendPid <br/> 8 | +3 | RD: Resulting PID after transmission (including parity bits) |
| IodBusStatL <br/> 32 | +2 | WR: Set LIN bus LOW for specified duration (in us) |

| IowUartCtrl (Write access) |||
| :--- | ---: | :--- |
| Name | Bits | Function |
| RFU | 15:14 | Reserved, do not use |
| TimerSrc | 13:12 | Timer source <br/> 0 = Timer disabled <br/> 1 = 1kHz <br/> 2 = 1MHz <br/> 3 = System clk |
| RFU | 11:10 | Reserved, do not use |
| SenseColl | 9 | Detect collision when in transmission mode. The block will stop if a collision is detected. Transmission FIFO will be cleaned, and any further writes to the transmission FIFO will be ignored. The function will be back to normal when the flag is cleaned through IobFlagsBR |
| Lin/Uart | 8 | The block can work as a generic UART. <br/> 1 = LIN <br/> 0 = UART |
| TxEn | 7 | Enable Transmission (otherwise the data will stay in the FIFO) |
| RxEn | 6 | Enable Reception (otherwise the RX pin toggle will be ignored) |
| IrqEn | 5:0 | Enable the corresponding IRQ: <br/> SyncReady - in reception mode, interrupt when Sync is received <br/> Collision - if collision is detected <br/> SendEmpty - transmission FIFO is empty <br/> RecvEmpty - reception FIFO is empty <br/> CanWrite - there is a space for at least 1 word in the TX FIFO <br/> CanRead - there is at least 1 word in RX FIFO |

RD: BusStatPend RFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr StatePend TimerNZ SyncReady Collision SendEmpty RecvEmpty CanWrite CanRead |

| IowUartCtrl (Read access) |||
| :--- | ---: | :--- |
| Name | Bits | Function |
| RFU | 15:14 | Reserved, do not use |
| TimerSrc | 13:12 | Timer source <br/> 0 = Timer disabled <br/> 1 = 1kHz <br/> 2 = 1MHz <br/> 3 = System clk |
| RFU | 11:10 | Reserved, do not use |
| SenseColl | 9 | Detect collision when in transmission mode. The block will stop if a collision is detected. Transmission FIFO will be cleaned, and any further writes to the transmission FIFO will be ignored. The function will be back to normal when the flag is cleaned through IobFlagsBR |
| Lin/Uart | 8 | The block can work as a generic UART. <br/> 1 = LIN <br/> 0 = UART |
| TxEn | 7 | Enable Transmission (otherwise the data will stay in the FIFO) |
| RxEn | 6 | Enable Reception (otherwise the RX pin toggle will be ignored) |
| IrqEn | 5:0 | Enable the corresponding IRQ: <br/> SyncReady - in reception mode, interrupt when Sync is received <br/> Collision - if collision is detected <br/> SendEmpty - transmission FIFO is empty <br/> RecvEmpty - reception FIFO is empty <br/> CanWrite - there is a space for at least 1 word in the TX FIFO <br/> CanRead - there is at least 1 word in RX FIFO |
