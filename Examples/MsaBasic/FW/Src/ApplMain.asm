Public FContApplMainA
Public ProcApplMainA

Extern StartupDelay
Extern ApplMainProc

.seg data

        Align 8
    FContApplMainA:  dd 0 dup(16)


        #Stack ProcApplMainA, 0
    FStackApplMainA:

.seg code

ProcApplMainA:
        mov     gr,4
        mov     esp,FStackApplMainA
        ;call    StartupDelay
        mov     awx,0x11223344
        bra     ApplMainProc
    pamaStop:
        nop
        trap
        nop
        swt    pamaStop



