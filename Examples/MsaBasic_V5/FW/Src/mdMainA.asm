Public FRegsMainA
Public FRegsIrqTimer

Public ProcMainA
Public ProcIrqTimer

Public TimerAWait
Extern IrqTimerA

Public UartGetData

Extern MainA

Extern UartRecvA
Extern UartRecvB
Extern FUartActive

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
        mov     ax,499
        mov     [IowTimerACmpA],ax
        mov     al,0x15
        mov     [IobTimerACtrl],al
        mov     ax,0x0001
        mov     [IowIrqEn],ax

        mov     al,0x90
        setfl

        mov     al,0x01
        mov     [FUartActive],al
        ;call    UartRecvA
        ;call    UartRecvB

     pmaWait:
        call    MainA
        inc     dr
        swt     pmaWait

TimerAWait:
        enter   awx,0
        ; Wait for IRQ
   tawWait:
        mov     al,[IobTimerACtrl]
        test    al,0x01
        swt_z   tawWait
        ; Reset TimerIrq flag
        mov     al,0x01
        mov     [IobTimerAIrqR],al
        leave   awx,0,0

ProcIrqTimer:
        mov     esp,FStackIrqTimer
        call    IrqTimerA
   pitLoop:
        bra     pitLoop

UartGetData:
        enter   #,0
        mov     ax,0x0055
        leave   #,0,0

