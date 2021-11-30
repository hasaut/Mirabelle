# Mirabelle

Multi-core/multi-ISA CPU and tools. 

Features:
- Risc-V "emac" complaint (this is not the only ISA it supports).
- Single-precision floating point on embedded class "ZFinx". 
- Hardware context switch to implement non-preemptive and preemptive multitasking. 
- Thread-based interrupts.

Project includes:
- The CPU itself, pure Verilog code.
- Tools: Instruction Set Simulator (ISS), debugger, compiler, linker. All tools are GUI and CLI, for Windows, Linux and Raspberry-Pi.
- Some generic peripheral modules like SPI, UART, etc. Also pure Verilog.
- FW and FPGA examles.

Examples are FPGA-synthesisable. The debugger can run FW on Simulator, on FPGA and on both Simulator and FPGA at the same time. 
