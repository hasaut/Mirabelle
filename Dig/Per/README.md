# Peripheral units

## Peripheral address space

The addressing of peripheral units is not al all the same as addressing memory. 
The byte, word, dword access to the same address may have different meaning and correspond to different functionality. 
Misaligned word and dword access will not cause align error like in the case of memory.
This is made mainly to save the address space occupied by the peripheral blocks.

Usually each block has a so called "base address". The addressing of instantiated blocks is presented as an offset from this base address.

## LIN

The following table lists briefly the IO ports FW may use to configure and control the LIN block and to obtain the LIN block state.

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

Write access to IowUartCtrl port controls the block.

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

By reading IowUartCtrl the FW can obtain the current state of the block. For example, to see if it can write to TX FIFO, read from the RX FIFO, obtain the state of the block etc.

Note that some flags reflect the current state. For example, "CanWrite" flag indicates if at least one word can be written to the FIFO. It can became "0" when transmission FIFO is full. This flag goes back to "1" when the space in TX FIFO becomes available.

Some other flags need to be cleaned by the FW, by writing to IobFlagsWR. For example, when a collision is detected during the transmission, the "Collision" flag will be set. The transmission will be cancelled and TX FIFO will be cleaned. Any further write to the TX FIFO will have no effect until the "Collision" flag is cleaned by writing the corresponding bit to "IobFlagsWR" IO port.

| IowUartCtrl (Read access) |||
| :--- | ---: | :--- |
| Name | Bits | Function |
| BusStatPend | 15 | Sets to "1" after writing to IowBusStatL. Stays at "1" for the duration specified by IowBusStatL |
| RFU | 14 | No meaning, reads as "0" |
| RecvOvf | 13 | Reception FIFO overflow: the FW was not fast enough to read data. Clean this flag by writing to IobFlagsBR |
| AvgDErr | 12 | Average error in Data |
| AvgSErr | 11 | Average error in Start bit |
| AvgPErr | 10 | Average error in Stop bit |
| SBitErr |  9 | Start bit error |
| PBitErr |  8 | Stop bit error |
| StatePend |  7 | Indicated that SteteMachine is not idle (can be used for Debug purpose) |
| TimerNZ |  6 | Time out is not zero |
| SyncReady | 5 | Sync is received (and baud rate is determined) |
| Collision | 4 | Collision was detected |
| SendEmpty | 3 | Send FIFO is empty |
| RecvEmpty | 2 | Recv FIFO is empty |
| CanWrite | 1 | At least 1 word can be written to the FIFO |
| CanRead | 0 | At least 1 word can be read from the FIFO |

Group of flags "AvgDErr", "AvgSErr", "AvgPErr" indicate the averaging error in Data, Start and Stop bits. This means that those bits are not fully "0" neither "1" during the reception but has glitches inside.

