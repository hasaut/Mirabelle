Public SinCos

.seg code

SinCos:
        enter   awx|ar|bwx|br|cwx|cr|dwx|dr|ewx|er,0

        mov     dwx,[esp+44]
        mov     awx,3.141592654
        mov     ar,2.0
        fmul    ar,awx
        fadd    dwx,awx
        btr     dwx,31
        fdiv    dr,dwx,ar
        trunc   dr
        fmul    dr,ar
        fsub    dwx,dr
        fsub    dwx,awx

        mov     bwx,1           ; index of factorial
        mov     br,2            ; accumulated n!
        mov     dr,dwx          ; accumulated x^n
        mov     cl,3            ; number of steps
        mov     ewx,1.0         ; Starting value for Cos
        mov     er,dwx          ; Starting value for Sin

    scNext:
        call    SinCosStep
        fsub    ewx,awx
        call    SinCosStep
        fsub    er,awx
        call    SinCosStep
        fadd    ewx,awx
        call    SinCosStep
        fadd    er,awx
        dec     cl
        bnz     scNext

        mov     bwx,[esp+48]
        mov     [bwx],er
        mov     bwx,[esp+52]
        mov     [bwx],ewx

        leave   awx|ar|bwx|br|cwx|cr|dwx|dr|ewx|er,0,12

SinCosStep:
        enter   #,0

        fmul    dr,dr,dwx       ; x^n
        mulzx   br,br,bwx       ; n!
        inc     bwx
        mov     cr,br
        itf     cr
        fdiv    awx,dr,cr

        leave   #,0,0

