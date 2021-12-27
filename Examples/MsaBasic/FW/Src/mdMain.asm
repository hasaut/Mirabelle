Public StartupDelay
Public ProbaA

Extern AppendThreadSd
Extern AppendThreadRv
Extern AppendIrqSd

Extern FRegsA
Extern FRegsB
Extern FRegsC
Extern FRegsRV
Extern FRegsIrqTimer

Extern ProcA
Extern ProcB
Extern ProcC
Extern ProcRV
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
   FDataA:           db 0

.seg code

Start_@ep:
StartTable:
        dd sCoreA or $10000000
        dd sCoreB or $10000000

; If label is declared as external and locally, there no error is reported

StartupDelay:
        enter   awx,0
   sdWait:
        mov     al,[FStartFlags]
        test    al,0x01
        bnz     sdRet
        swt     sdWait
   sdRet:
        leave   awx,0,0


sCoreA:
        mov     [FStartFlags],zl
        mov     esp,FStackA

        mov     [IobLedPwr],zl
        mov     [IobLedRgb],zl

        mov     ax,0x0101
        mov     [IowClkDivIda],ax

        nop
        trap
        nop

        ; Set some leds
        mov     [IobLedPwr],zl
        mov     [IobLedRgb],zl
        mov     al,0x01
        mov     [IobLedPwr],al
        mov     al,0x04
        mov     [IobLedRgb],al

        ; Build thread list
        mov     bwx,FThList
        mov     awx,0x000F0400
        mov     [bwx++],awx

        mov     dwx,FContextA
        mov     awx,ProcessA
        call    AppendThread

        mov     dwx,FContextB
        mov     awx,ProcessB
        call    AppendThread

        mov     dwx,FContextC
        mov     awx,ProcessC
        call    AppendThread

        mov     dwx,FContApplMainA
        mov     awx,ProcApplMainA
        bra     AppendThread

        mov     bwx,FIrqList
        add     bwx,4
        mov     dwx,FContIrqA
        mov     awx,ProcIrqA
        bra     AppendIrq

        mov     awx,FThList
        mov     ar,FIrqList
        siconf

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

StartB:
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

AppendThread:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   atFillZNext:
        mov     [ewx++],zwx
        dec     cl,1
        bnz     atFillZNext
        mov     awx,[esp+8]
        or      awx,0x10000000
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0

AppendIrq:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   aiFillZNext:
        mov     [ewx++],zwx
        dec     cl,1
        bnz     aiFillZNext
        mov     awx,[esp+8]
        or      awx,0x50000000
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0


ProbaA:
        enter   awx,0
        mov     awx,0
        inc     fl
        swt     paNext
    paNext:
        nop
        ;trap
        nop
        leave   awx,0,0

RdRegs:
        enter   awx|cwx,0
        mov     cl,16
        mov     al,[IobMlxDbgCtrl] ; Reset Idx
   rrNext:
        mov     ax,[IowMlxDbgRegs] ; A
        dec     cl
        bnz     rrNext
        leave   awx|cwx,0,0

