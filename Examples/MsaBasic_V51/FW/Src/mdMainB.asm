Public FRegsMainB
Public ProcMainB

Extern MainB

#include "IoIda.h"

.seg data

        Align 8
    FRegsMainB:    dd 0 dup(24)


        #Stack ProcMainB, 0
    FStack:

.seg code

ProcMainB:
        mov     esp,FStack
        mov     al,0x90
        setfl
        call    MainB
     pmbWait:
        inc     er
        swt     pmbWait



