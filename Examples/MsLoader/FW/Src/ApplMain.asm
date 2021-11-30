Public FContApplMainA
Public ProcApplMainA

Public ProcApplTimerWait
Public ProcApplFlashCheck

Public ProcApplReconfig
Public StartupSet
Public StartupGet

Public FFlashCheckLen
Public FFlashCheckIdx


Extern StartupDelay
Extern ApplMainProc

#include "IoIda.h"

.seg data

        Align 8
    FContApplMainA:  dd 0 dup(16)


        #Stack ProcApplMainA, 0
    FStackApplMainA:

    FFlashCheckLen: dd 0
    FFlashCheckIdx: dd 0

.seg code

ProcApplMainA:
        mov     esp,FStackApplMainA
        mov     [FFlashCheckLen],zwx
        ;call    StartupDelay

        mov     al,0x14
        mov     [IobTimerACtrl],al
        mov     ax,99
        mov     [IowTimerACmpA],ax
        mov     al,0x01
        mov     [IobTimerAIrqR],al
        mov     awx,0x11223344
        call    ApplMainProc
    pamaStop:
        nop
        trap
        nop
        swt    pamaStop

ProcApplTimerWait:
        enter   awx,0
   patwWait:
        mov     al,[IobTimerACtrl]
        test    al,0x01
        swt_z   patwWait
        mov     al,0x01
        mov     [IobTimerAIrqR],al
        leave   awx,0,0

UfmCsLo:
        enter   awx,0
        mov     al,0xC0
        mov     [IobUfmCtrl],al
        leave   awx,0,0

UfmCsHi:
        enter   awx,0
        mov     al,0x00
        mov     [IobUfmCtrl],al
        leave   awx,0,0

UfmSendRecvB:
        enter   #,0
        mov     [IobUfmData],al
   usrbWait:
        mov     al,[IobUfmCtrl]
        test    al,0x20
        bnz     usrbWait
        mov     al,[IobUfmData]
        leave   #,0,0

UfmSendRecvD:
        enter   #,0
        call    UfmSendRecvB
        rol     awx,24
        call    UfmSendRecvB
        rol     awx,24
        call    UfmSendRecvB
        rol     awx,24
        call    UfmSendRecvB
        rol     awx,24
        leave   #,0,0

UfmSendAddr:
        enter   awx,0
        rol     awx,16
        call    UfmSendRecvB
        rol     awx,8
        call    UfmSendRecvB
        rol     awx,8
        call    UfmSendRecvB
        leave   awx,0,0

ProcApplFlashCheck:
        enter   awx|cwx|dwx,0

        call    UfmCsHi

        mov     al,0x01
        mov     [IobUfmBaud],al

        call    UfmCsLo

        mov     al,0x03
        call    UfmSendRecvB

        mov     awx,[esp+16]
        mov     cwx,[esp+20]
        add     awx,cwx
        sub     awx,8
        call    UfmSendAddr

        mov     awx,0xFFFFFFFF
        call    UfmSendRecvD
        mov     cwx,awx

        mov     awx,0xFFFFFFFF
        call    UfmSendRecvD
        mov     dwx,awx

        call    UfmCsHi

        mov     awx,[esp+20]
        cmp     cwx,awx
        ba      pafcError

        cmp     cwx,zwx
        be      pafcError

        call    UfmCsLo

        mov     al,0x03
        call    UfmSendRecvB

        mov     awx,[esp+16]
        call    UfmSendAddr
        mov     awx,zwx

        mov     [FFlashCheckLen],cwx

   pafcReadNext:
        mov     al,0xFF
        mov     [IobUfmData],al
   pafcReadWait:
        mov     al,[IobUfmCtrl]
        test    al,0x20
        bnz     pafcReadWait
        mov     al,[IobUfmData]
        add     aw,ax
        dec     cwx
        mov     [FFlashCheckIdx],cwx
        bnz     pafcReadNext

        mov     [FFlashCheckLen],zwx

        call    UfmCsHi

        cmp     dw,aw
        bne     pafcError

        mov     al,0x01
        bra     pafcEnd
   pafcError:
        mov     al,0x00
   pafcEnd:
        mov     [esp+8],al

        leave   awx|cwx|dwx,0,8

ProcApplReconfig:
        enter   awx,0
        ;// Use internal osc
        mov     al,0x06
        mov     [IobRsuAddr],al
        mov     awx,0x00000001
        mov     [IodRsuData],awx
        ;// Reset addr
        mov     al,0x04
        mov     [IobRsuAddr],al
        mov     awx,[esp+8]
        mov     [IodRsuData],awx
        ;// TimeOut
        mov     al,0x02
        mov     [IobRsuAddr],al
        mov     awx,0x0000001F
        mov     [IodRsuData],awx
        ;// Reconfig
        mov     al,0x01
        mov     [IobRsuCtrl],al
        leave   awx,0,4

StartupGet:
        enter   #,0
        mov     al,[IobDbgStartup]
        and     al,0x0F
        leave   #,0,0

StartupSet:
        enter   awx,0
        mov     al,[esp+8]
        mov     [IobDbgStartup],al
        leave   awx,0,4

