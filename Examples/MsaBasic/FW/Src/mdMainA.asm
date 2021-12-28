Public FRegsMainA
Public FRegsIrqTimer

Public ProcMainA
Public ProcIrqTimer

Public TimerAWait
Extern IrqTimerA

Extern MainA

#include "IoIda.h"

.seg data

        Align 8
    FRegsMainA:    dd 0 dup(16)
    FRegsIrqTimer: dd 0 dup(16)


        #Stack ProcMainA, 0
    FStack:

        #Stack ProcIrqTimer, 0
    FStackIrqTimer:

    Align 4
    FTimer1ms:  dd 0

.seg code

ProcMainA:
        mov     esp,FStack

        mov     [FTimer1ms],zwx

        mov     al,0x00
        mov     [IobTimerACtrl],al
        mov     ax,999
        mov     [IowTimerACmpA],ax
        mov     al,0x15
        mov     [IobTimerACtrl],al
        mov     ax,0x0001
        ;mov     [IowIrqEn],ax

        mov     al,0x90
        setfl

     pmaWait:
        call    MainA
        inc     dr
        swt     pmaWait

TimerAWait:
        enter   awx,0
        swt     tawNext
   tawNext:
        mov     al,0x01
        mov     [IobTimerAIrqR],al
        leave   awx,0,0

ProcIrqTimer:
        mov     esp,FStackIrqTimer
        call    IrqTimerA
   pitLoop:
        bra     pitLoop


