Public FContextA
Public FContextB
Public FContextC

Public ProcessA
Public ProcessB
Public ProcessC

Extern FFlashCheckLen
Extern FFlashCheckIdx

#include "IoIda.h"

.seg data

        Align 8
    FContextA:  dd 0 dup(16)
    FContextB:  dd 0 dup(16)
    FContextC:  dd 0 dup(16)


        #Stack ProcessB, 0
    FStackB:

.seg code

ProcessA:
        inc     er
        inc     er
        inc     er

        nop
        nop     ; trap
        nop

        inc     dr
        swt     ProcessA

ProcessB:
        mov     esp,FStackB
    pbStop:
        mov     awx,[FFlashCheckLen]
        cmp     awx,zwx
        be      pbSkipProgr
        mov     ar,[FFlashCheckIdx]
        sub     ar,awx,ar
        shr     awx,3
        udiv    bwx,ar,awx
        mov     al,zl
        cmp     bl,zl
        be      pbSetLed
    pbShiftNext:
        shl     al,1
        or      al,0x01
        dec     bl
        bnz     pbShiftNext
    pbSetLed:
        mov     [IobLedY],al
    pbSkipProgr:
        swt     pbStop

ProcessC:
        inc     gr
        swt     ProcessC


