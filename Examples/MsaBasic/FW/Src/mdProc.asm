Public FContextA
Public FContextB
Public FContextC
Public FContIrqA

Public ProcessA
Public ProcessB
Public ProcessC
Public ProcIrqA

Extern DbgSendByteA

#include "IoIda.h"

.seg data

        Align 8
    FContextA:  dd 0 dup(16)
    FContextB:  dd 0 dup(16)
    FContextC:  dd 0 dup(16)
    FContIrqA:  dd 0 dup(16)


        #Stack ProcessB, 0
    FStackB:

        #Stack ProcIrqA, 0
    FStackIrqA:

    FDataA:     dd 0

.seg code

ProcessA:
        mov     gr,1
        mov     al,0x90
        setfl
     paWait:
        inc     er
        inc     er
        inc     er

        nop
        ;trap
        nop

        mov     aq,[IowHexData]

        inc     dr
        swt     paWait

ProcessB:
        mov     gr,2
        mov     esp,FStackB

        nop
        trap
        nop

        call    ProbaA

        mov     al,9
        mov     bh,8

        nop
        trap
        nop

    pbStop:
        swt     pbStop

ProcessC:
        mov     gr,3
        inc     ar
        swt     ProcessC

ProcIrqA:
        mov     esp,FStackIrqA
        mov     gr,0x0A
   piaWait:
        push    awx
        inc     awx
        pop     awx
        swt     piaWait

ProbaA:
        enter   awx,0
        nop
        leave   awx,0,0
