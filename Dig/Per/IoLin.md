# LIN

LIN module supports Master and Slave mode and may additionally be used as a generic UART. This block is highly automized and configurable:
  - SYNC transmission and reception.
  - Baud rate recognition.
  - Collision and anomaly detection.
 
The block must be properly configured before the use. The configuration mainly includes the geometry of the frame.

After being configured, the communication is quite straightforward. 
In transmission mode the FW needs to fill TX FIFO with data. 
In reception mode the FW needs to pick received data from the RX FIFO. 

The FIFO is organized as 9-bit-wide words (and not 8-bit). 
This is made to transmit/receive LIN-related parts.
For example, if FW writes 0x03C into the TX FIFO then a 0x3C symbol will be sent, like in Generic UART.
But if FW writes 0x13C into the TX FIFO, then the block will send Break + Sync + PID. Where PID = 0x3C and parity bits will be auto-generated.
RX FIFO works by the similar way. 

The following chapters explain the communication and particularities in details.

## List of the IO ports

The following table gives the list of the IO ports. Detailed description is given in the following chapters.

| Name <br/> and size| Offset | Description |
| :--- | :--- | :--- |
| IowUartCtrl <br/> 16 | +0 | WR: 2xRFU 2xTimerSrc 2xRFU SenseColl Lin/Uart TxEn RxEn 6xIrqEn <br/> RD: BusStatPend RFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr StatePend TimerNZ SyncReady Collision SendEmpty RecvEmpty CanWrite CanRead |
| IowUartBaud <br/> 16 | +1 | Baud rate <br/> this is the length of 8 bits, not of 1 bit. <br/> In Master mode this is a transmission baud rate, in Slave mode this is a detected baud rate (i.e. it can be read) |
| IowUartData <br/> 16 | +2 | WR: Data to be sent <br/> RD: Data received <br/> Though the access is 16-bit, only 9 bit are used. Bits 7:0 are data bits, bit 8 has a special meaning (description is given below)  <br/> There is a 4-words FIFO for transmission and a 4-words FIFO for reception |
| IodTOut <br/> 32 | +3 | Time out. Timer counts from this value down to Zero. Resets when next byte is received |
| IobFlagsBR <br/> 8 | +0 | Writing "1" resets the corresponding flag in IowUartCtrl register <br/> WR: 2xRFU RecvOvf AvgDErr AvgSErr AvgPErr SBitErr PBitErr |
| IobUartCfg <br/> 8 | +1 | Lin configuration. These parameters configure the geometry of the frame. <br/> WR: ColWnd:AvgWnd TolBit BrkMin BrkMax BpdMin Master TxTail |
| IobPidCalc <br/> 8 | +2 | WR/RD: PID calculator |
| IobIrqR <br/> 8 | +3 | WR: IrqR IRQ reset |
| IobSendPid <br/> 8 | +3 | RD: Resulting PID after transmission (including parity bits) |
| IodBusStatL <br/> 32 | +2 | WR: Set LIN bus LOW for specified duration (in us) |

## Detailed description of the IO ports

By writing to IowUartCtrl port FW configures and controls the block.

By reading from IowUartCtrl port FW gets access to the status flags.

**IowUartCtrl (Write access)**

| Name | Bits | Function |
| :--- | ---: | :--- |
| RFU | 15:14 | Reserved, do not use |
| TimerSrc | 13:12 | Timer source <br/> 0 = Timer disabled <br/> 1 = 1kHz <br/> 2 = 1MHz <br/> 3 = System clk |
| RFU | 11:10 | Reserved, do not use |
| SenseColl | 9 | Detect collision when in transmission mode. The block will stop if a collision is detected. Transmission FIFO will be cleaned, and any further writes to the transmission FIFO will be ignored. The function will be back to normal when the flag is cleaned through IobFlagsBR |
| Lin/Uart | 8 | The block can work as a generic UART. <br/> 1 = LIN <br/> 0 = UART |
| TxEn | 7 | Enable Transmission (otherwise the data will stay in the FIFO) |
| RxEn | 6 | Enable Reception (otherwise the RX pin toggle will be ignored) |
| IrqEn | 5:0 | Enable the corresponding IRQ: <br/> SyncReady - in reception mode, interrupt when Sync is received <br/> Collision - if collision is detected <br/> SendEmpty - transmission FIFO is empty <br/> RecvEmpty - reception FIFO is empty <br/> CanWrite - there is a space for at least 1 word in the TX FIFO <br/> CanRead - there is at least 1 word in RX FIFO |

By reading IowUartCtrl the FW can obtain the current state of the block: if it can write to TX FIFO, read from the RX FIFO, obtain the state of the block etc.

Some flags reflect the current state and do not need to be explicitely cleaned by the FW. 
For example, "CanWrite" flag indicates if at least one word can be written to the FIFO. It can became "0" when transmission FIFO is full. This flag goes back to "1" when the space in TX FIFO becomes available.

Some other flags need to be cleaned by the FW, by writing to IobFlagsWR. For example, when a collision is detected during the transmission, the "Collision" flag will be set. The transmission will be cancelled and TX FIFO will be cleaned. Any further write to the TX FIFO will have no effect until the "Collision" flag is cleaned by writing the corresponding bit to "IobFlagsWR" IO port.

**IowUartCtrl (Read access)**

| Name | Bits | Function |
| :--- | ---: | :--- |
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


**IowUartBaud** FW can write and read. On write access it specifies the transmission baud rate. 

IowUartBaud = SystemClk * 8 / DesiredBaudRate

For example, if SystemClk = 50 MHz, and desired baud rate is 9600, the FW needs to write 50000000 * 8 / 9600 = 0xA2C3 to IowUartBaud.

During the reception FW may read this register to obtain the baud rate.


**IowUartData** is a 9-bit command and data port. This port gives an access to TX and RX FIFO. 

This port is used not only for data but also to write commands and read the error codes. These 9 bits have the following meaning.

MOSI (when FW writes data to the port): <br/>
  - **1_00PPPPPP** = MasterSendSof (Break+Sync+Pid). Here "PPPPPP" is a PID.  Parity bits will be computed by the HW. Master starts the communication by writing this command.
  - **1_10xxxxxx** = SlaveListen. Bits "xxxxxx" have no meaning and are recommended to be all Zeroes. FW writes this command to start listening for Master frames in Slave mode. Once the Break+Sync is received and recognized, the "SyncReady" flag will be set in IowUartCtrl port.
  - **1_01000000** = StopListen. Exit listening mode. Cancel reception if it was already started. 
  - **1_110VCCCC** = Send CCCC times value V. Useful to set LinBus low for a certain time. 
  - **0_DDDDDDDD** = Data. Regular data to be sent.
 
MISO (when FW reads data from the port): <br/>
  - **1_01000000** = (Brake+Sync) received and baud rate is set.
  - **1_01EEEEEE** = Error code. "EEEEEE" can be one of the following:
    - 05 ListenCancelled. When Listen mode is cancelled by StopListen command.
    - 06 InvalidCmd. Happens when the command is not applicable. For example StopListen command is send, but the device is not in the Listen mode.
    - 07 MispListen. Misplaced Listen command. Happend when FW sends SlaveListen command but the device is already in Listen mode, or when the communication is active in Master mode.
  - **0_DDDDDDDD** = Data. Regular data received.


**IodTOut** TimeOut for the reception. Timer starts from this value when a byte is received and counts down to zero. When a next byte is received, timer sets again to this value and starts counting down and so on. This is useful to detect the Time-Out during reception. Use bit TimerNZ in IowUartCtrl register to check for the time-out.

**IobFlagsBR** is used to reset some status flags in IowUartCtrl register. Some flags in IowUartCtrl register once been set keep their value until FW explicitely cleans them.
  - RecvOvf = resets reception FIFO overflow.
  - AvgDErr = resets Data averaging error
  - AvgSErr = resets Start bit averaging error
  - AvgPErr = resets Stop bit averaging error
  - SBitErr = resets Start bit error
  - PBitErr = resets Stop bit  error

**IobUartCfg** is used to specify the geometry of the frame.

There are 7 parameters which need to be configured through the same port. 
FW needs to write all 7 parameters one after another to this port. 
There is an internal index which will be incremented after each writing to the port. This index allows accessing these configuration parameters one by one through the same port.
FW must not access any other LIN IO port, otherwise the index of the parameter will be reset. FW may access any other LIN port in order to explicitely reset the parameter index.


The measurement unit to configure the geometry is the bit length. For example the range for "CfgBrkMin" parameter (see this parameter below) is indicated as (31.875..0.125). 
This means that the minimal value 0x00 will be equal to 0.125 of the bit length (12.5%) and the maximal value 0xFF will be equal to 31.875 of the bit duration (3187.5%). 
If we need to configure MinBrkLen to be 9 bits, we need to set MinBrkLen to be 71. 

In general, the duration = Desired_duration_in_bits / MinRange - 1. For the example above 9 / 0.125 - 1 = 71.

  - CfgSenLen = 7:4 collision SenWnd, 3:0 average SenWnd | Range = (0.4375..0.0625) <br/> window size to sense collisions and for averaging
  - CfgBitTol = 7:4 RFU, 3:0 Sync (0x55) jitter tolerance | Range = (0.4375..0.0625) <br/> jitter tolerance during Sync reception                                                             
  - CfgBrkMin = 7:0 MinBrkLen | Range = (31.875..0.125) <br/> Minimal duration of Break
  - CfgBrkMax = 7:0 MaxBrkLen | Range = (31.875..0.125) <br/> Maximal duration of Break
  - CfgBpdMax = 7:0 Max Brk+Dlm Len | Range = (31.875..0.125) <br/> Maximal duration of Break+Delimiter
  - CfgMaster = 6xRFU DblStopBrk DblStopData <br/> Double stop-bit duration during break transmission, Double stop-bit duration during data transmission
  - CfgTxTail = TxTail Duration of tail H level (in System CLK pulses) <br/> This parameter controls a push-pull driver of Lin TX pin. It forces high level for a short period of time to "help" pull-up resistor. In Legacy LIN application it must be set to zero. Non-zero values help increase the LIN physical baud rate.

The following parameters work fine:
CfgColAvg = 0xCC;   // 7:4 collision SenWnd, 3:0 average SenWnd | Range = (0.4375..0.0625) <br/>
CfgTolBit = 0x03;   // 7:4 RFU, 3:0 Sync (0x55) jitter tolerance | Range = (0.4375..0.0625) <br/>
CfgBrkMin =  9 * 8; // 7:0 MinBrkLen | Range = (31.875..0.125) <br/>
CfgBrkMax = 11 * 8; // 7:0 MaxBrkLen | Range = (31.875..0.125) <br/>
CfgBpdMin = 14 * 8; // 7:0 Max Brk+Dlm Len | Range = (31.875..0.125) <br/>
CfgMaster = 0x02;   // 7:2 RFU, 1 2x StopBrk, 0 2x StopData <br/>
CfgTail   = 0x01;

**IobPidCalc** helps FW to calculate parity bits of PID if necessary. 

Write 6-bit PID to this port. Read resulting parity_bits + PID.

**IobIrqR** Reset corresponding IRQ.

IRQ is enabled in IowUartCtrl register. By writing "1" to the corresponding bit of IobIrqR FW resets the IRQ.

**IobSendPid** is used to obtain the resulting PID value after "MasterSendSof" command.


