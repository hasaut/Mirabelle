Public StartupDelay

Extern FContextA
Extern FContextB
Extern FContextC
Extern ProcessA
Extern ProcessB
Extern ProcessC

Extern FContApplMainA
Extern ProcApplMainA

Extern StartC
Extern StartD

#include "IoIda.h"

.seg data
    FThList:
        dd 0
        dd 0 dup(32)


        #Stack StartA, 0
    FStackA:

    FDataA:
        dd 0
    FDataB:
        dd 0

    FStartFlags:
        db 0
    FDummyData:
        db 0

.seg code

Start_@ep:
StartTable:
        dd StartA or $10000000
        dd StartB or $10000000

; If label is declared as external and locally, there no error is reported

StartupDelay:
        enter   awx,0
   sdWait:
        mov     al,[FStartFlags]
        test    al,0x01
        swt_z   sdWait
        leave   awx,0,0


StartA:
        mov     al,0
        mov     [FStartFlags],al
        mov     esp,FStackA

        mov     ax,0x0101
        mov     [IowClkDivIda],ax

        mov     al,0x01
        mov     [IobLedPwr],al

        mov     al,0x04
        mov     [IobLedRgb],al

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
        call    AppendThread

        mov     awx,FThList
        mov     ar,0
        siconf

        mov     ax,0x0000
        mov     [IowHexData],ax
        mov     ax,0x55AA
        mov     [IowHexData],ax
        mov     cx,[IowHexData]

        mov     al,0x55
        mov     [IobLedY],al

        mov     al,0x01
        mov     [FStartFlags],al
   saWaitA:
        mov     al,[FStartFlags]
        cmp     al,0x03
        bne     saWaitA
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
        mov     awx,0
   atFillZNext:
        mov     [ewx++],awx
        dec     cl
        bnz     atFillZNext
        mov     awx,[esp+8]
        or      aw,0x1000
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0



