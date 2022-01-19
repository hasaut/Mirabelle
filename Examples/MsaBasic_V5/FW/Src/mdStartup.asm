Extern AppendThreadSd
Extern AppendThreadRv
Extern AppendIrqSd

Extern FRegsMainA
Extern FRegsMainB
Extern FRegsMainC
Extern FRegsMainD
Extern FRegsIrqTimer

Extern ProcMainA
Extern ProcMainB
Extern ProcMainC
Extern ProcMainD
Extern ProcIrqTimer


#include "IoIda.h"

.seg data
    FThList:
        dd 0
        dd 0 dup(32)

    FIrqList:
        dd 0 dup(16)

        #Stack sCoreA, 0
        ;dd 0 dup(64)
    FStackA:

   FStartFlags:      db 0

.seg code

Start_@ep:
StartTable:
        dd sCoreA or $10000000
        dd sCoreB or $10000000

; If label is declared as external and locally, there no error is reported

sCoreA:
        mov     [FStartFlags],zl
        mov     [IowIrqEn],zx
        mov     esp,FStackA

        mov     [IobLedPwr],zl
        mov     [IobLedRgb],zl

        mov     ax,0x0101
        mov     [IowClkDivIda],ax

        ; Set some leds
        mov     [IobLedPwr],zl
        mov     [IobLedRgb],zl
        mov     al,0x01
        mov     [IobLedPwr],al
        mov     al,0x04                 ; Blue
        mov     [IobLedRgb],al

        ; Build thread list
        mov     bwx,FThList
        mov     awx,0x000F0400 ; Queue length = 16 (0x0F); 4 active threads
        mov     [bwx++],awx

        mov     awx,ProcMainA
        mov     dwx,FRegsMainA
        call    AppendThreadSd

        mov     awx,ProcMainB
        mov     dwx,FRegsMainB
        call    AppendThreadSd

        mov     awx,ProcMainC
        mov     dwx,FRegsMainC
        call    AppendThreadRv

        mov     awx,ProcMainD
        mov     dwx,FRegsMainD
        bra     AppendThreadRv

        mov     bwx,FIrqList
        ;add     bwx,0
        mov     dwx,FRegsIrqTimer
        mov     awx,ProcIrqTimer
        bra     AppendIrqSd

        mov     awx,FThList
        mov     ar,FIrqList
        siconf

        mov     al,0x02                 ; Green
        mov     [IobLedRgb],al
        mov     al,0x55
        mov     [IobLedY],al

        ; Set confirmation for other cores
        mov     al,0x01
        mov     [FStartFlags],al
   saWaitA:
        mov     al,[FStartFlags]
        cmp     al,0x03
        bne     saWaitA
        ; When confirmation is received, end this [startup] thread
        siend

sCoreB:
        mov     awx,10
   sbWait:
        dec     awx
        bnz     sbWait
   sbWaitA:
        mov     al,[FStartFlags]
        cmp     al,0x01
        bne     sbWaitA
        mov     al,0x03
        mov     [FStartFlags],al
        siend


