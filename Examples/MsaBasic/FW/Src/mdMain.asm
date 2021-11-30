Public StartupDelay
Public ProbaA

Extern FContextA
Extern FContextB
Extern FContextC
Extern FContIrqA

Extern ProcessA
Extern ProcessB
Extern ProcessC
Extern ProcIrqA
Extern FContApplMainA
Extern ProcApplMainA

;Extern MsUnitTest

#include "IoIda.h"

.seg data
    FThList:
        dd 0
        dd 0 dup(32)

    FIrqList:
        dd 0 dup(16)

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
        dd StartA or $10000000

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


StartA:
        mov     awx,esp
        cmp     al,zl
        bne     StartB
        mov     [FStartFlags],zl
        mov     esp,FStackA

        mov     [IowIrqEn],zx
        mov     [IobTimerACtrl],zl

        mov     ax,0x0303
        mov     [IowClkDivIda],ax

        nop
        trap
        nop

        mov     awx,77
        urem    awx,10

        mov     awx,0.0
        mov     bwx,1.0
        fadd    awx,bwx
        mov     ar,1.0

        mov     awx,1.0
        fmul    bwx,awx,awx
        mov     br,1.0

        mov     awx,1.0
        fdiv    cwx,awx,awx
        mov     cr,1.0

        mov     awx,2.0
        fadd    dwx,awx,awx
        mov     dr,4.0

        mov     ewx,1
        itf     ewx
        mov     er,1.0

        mov     awx,10.0
        mov     fwx,0.1
        fmul    fwx,fwx,awx
        mov     fr,1.0

        mov     ar,1E-38
        fmul    br,ar,ar


        mov     bwx,0x54
        mov     al,[bwx]

        nop
        mov     al,0x55
        mov     [FDataB],al
        nop
        btr     [FDataB],0
        mov     al,zl
        mov     al,[FDataB]

        mov     awx,2.5
        mov     cwx,awx
        mov     bwx,-100.0
        fadd    awx,bwx
        cmp     awx,-97.5

        mov     awx,1.0
        mov     bwx,2.0
        fadd    awx,bwx
        mov     ar,3.0

        mov     ax,0x0000
        mov     [IowDutCtrl],ax
        mov     al,0x02
        mov     [IobMlxDbgCtrl],al
        mov     ax,0x0103
        mov     [IowDutCtrl],ax
        mov     ax,0x0107
        mov     [IowDutCtrl],ax
        mov     ax,0x0137
        mov     [IowDutCtrl],ax
        mov     ax,zx
        mov     ax,[IowDutCtrl]
        mov     al,0x01
        mov     [IobMlxDbgMaskClr],al

        call    RdRegs

        mov     al,0x01
        mov     [IobMlxDbgMaskClr],al

        call    RdRegs

        mov     al,0x01
        mov     [IobMlxDbgMaskClr],al

        call    RdRegs

        mov     awx,0x10000000
        mov     [IodDutMemAddr],awx
        mov     awx,0x01234567
        mov     ar,0x89ABCDEF
        mov     [IoqDutMemData],aq
        mov     awx,0x10000000
        mov     [IodDutMemAddr],awx
        mov     awx,zwx
        mov     ar,zwx
        mov     aq,[IoqDutMemData]

        mov     bwx,zwx
        mov     fwx,zwx
        mov     al,0x80
        subzx   fwx,bwx,al

        mov     bwx,zwx
        mov     fwx,zwx
        subzx   fwx,bwx,0x80



        ;push    zwx
        ;bra     msUnitTest

   ;saWait:
   ;     bra     saWait

        mov     al,0x00
        mov     [IobLedPwr],al

        mov     al,0x00
        mov     [IobLedRgb],al

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
        bra     AppendThread

        mov     bwx,FIrqList
        add     bwx,4
        mov     dwx,FContIrqA
        mov     awx,ProcIrqA
        bra     AppendIrq

        mov     awx,FThList
        mov     ar,FIrqList
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

