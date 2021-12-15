# Peripheral units

## Peripheral address space

The addressing of peripheral units is not al all the same as addressing memory. 
The byte, word, dword access to the same address may have different meaning and correspond to different functionality. 
Misaligned word and dword access will not cause align error like in the case of memory.
This is made mainly to save the address space occupied by the peripheral blocks.

Usually each block has a so called "base address". The addressing of instantiated blocks is presented as an offset from this base address.

## LIN

[IoLin description](IoLin.md)

