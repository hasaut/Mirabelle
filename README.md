# Mirabelle

Multi-core/multi-ISA CPU and tools. 

Features:
- Multi-ISA:
  - Generic Mirabelle instruction set
  - Risc-V "emac" complaint instruction set
  - WASM (under development)
- Single-precision floating point, corresponds to Risc-V "ZFinx". 
- Hardware context switch to implement non-preemptive and preemptive multitasking. 
- Thread-based interrupts.

Project includes:
- The CPU itself, as Verilog code.
- Tools: Instruction Set Simulator (ISS), debugger, compiler, linker. All tools are GUI and CLI, for Windows, Linux and Raspberry-Pi.
- Some generic peripheral modules like SPI, UART, etc. in Verilog.
- FW and FPGA examles.

Examples are FPGA-synthesisable. The debugger can run FW on Simulator, on FPGA and on both 
Simulator and FPGA at the same time. 

## Multi-ISA

Processors can understand multiple ISAs like people speak multiple languages.

The main interest of multi-isa is to reduce the code size and to increase the performance. 
One algorithm can be more optimal when using register architecture CPU, another algorithm 
can be more optimal using stack architecture. 
The instruction decoder itself is small in the digital design. Mirabelle project targets embedded 
applications where the cost of program memory like flash can be important.
It is expected that the increased size of digital part for the decoder will be payed out 
by decreased size of flash memory.

It is not necessary to keep all decoders: one can include only Risc-V for example. 

## Multi-core

By default Mirabelle project supposes multi-core CPU. The number of cores can be set as a 
parameter of a verilog file.

The minimal number of cores is 1.

Dual-core version works fine with 64-bit wide memory even with no cache. More cores will need 
bigger memory width and cache.

## Multi-threading

The context switch is done by the HW. Maximal possible memory width will be used to store/load CPU 
registers to/from memory. This means that several registers will be stored/loaded in a single 
memory cycle.

This context switch concept is used for both threads and interrupts. The regular threads are 
normally non-preemptive and interrupts are preemptive. 

Interrupts can be seen as threads driven by the events. 

[CPU startup](Doc/CpuStartup.pdf) document describes the thread and interrupt mechanism.

# Quick start with FW

## Basic FW example

[Basic SW example](Examples/MsaBasic) is a project which creates 4 threads and 1 interrupt and 
shows how it works. See the commants in the source files.  


