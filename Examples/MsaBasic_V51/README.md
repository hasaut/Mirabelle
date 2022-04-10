# Basic SW example

In this example a simple project which contains 4 threads and 1 interrupt is created.

To easy start with the example, open file [MsaBasic.prj](FW/MsaBasic.prj) with "md_gui" tool.

There will be 4 threads:
- MainA written in Pascal and compiled by an internal compiler.
- MainB written in C and compiled by an internal compiler.
- MainC written in C and compiled by external GCC compiler.
- MainD stays in Risc-V assembly.

The interrupt is a Timer interrupt which ticks once per second.
