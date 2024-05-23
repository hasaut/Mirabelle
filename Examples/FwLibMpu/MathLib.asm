Public MsSinCos
Public MsAbs
Public MsRound
Public MsSqrt
Public MsAtan2

.seg code

CHalfPi equ 1.57079632679489661923
CDualPi equ 6.28318530717958647693
CMathPi equ 3.14159265358979323846

MsAbs:
        enter   #,0
        mov     awx,[esp+4]
        and     awx,0x7FFFFFFF
        leave   #,0,4

MsRound:
        enter   #,0
        mov     awx,[esp+4]
        round   awx
        leave   #,0,4

MsSinCos:
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

MsSqrt:
        enter   bwx|cwx|dwx|ewx,0

        mov     awx,[esp+20]
        mov     ewx,awx
        and     awx,0x7F800000
        sub     awx,0x3F800000
        shr     awx
        add     awx,0x3F800000
        and     awx,0x7F800000
        mov     dwx,awx
        mov     cwx,0x00400000

    msNextB:
        mov     bwx,dwx
    msNextA:
        or      bwx,cwx
        mov     awx,bwx
        fmul    awx,bwx
        cmp     awx,ewx
        ba      msSkipAdd
        mov     dwx,bwx
        shr     cwx
        bnz     msNextA
        bra     msRet
    msSkipAdd:
        shr     cwx
        bnz     msNextB

    msRet:
        mov     awx,dwx

        leave   bwx|cwx|dwx|ewx,0,4

; arctan(x) = x − x^3/3 + x^5/5 - x^7/7...
MsAtanSmallA:
        enter   bwx|cwx|dwx|br,0

        mov     dwx,0.0
        mov     bwx,awx
        fmul    br,bwx,bwx
        mov     cl,1

   masNext:
        movzx   awx,cl
        itf     awx
        fdiv    awx,bwx,awx
        fadd    dwx,awx
        inc     cl,2
        fmul    bwx,br
        movzx   awx,cl
        itf     awx
        fdiv    awx,bwx,awx
        fsub    dwx,awx
        cmp     cl,11
        ba      masRet
        inc     cl,2
        fmul    bwx,br
        bra     masNext
   masRet:
        mov     awx,dwx

        leave   bwx|cwx|dwx|br,0,0

; arctan(x) = 2arctan(x/(1+√(1+x^2)))
MsAtanBigA:
        enter   bwx,0

        mov     bwx,awx
        fmul    awx,bwx,bwx
        fadd    awx,1.0
        push    awx
        call    MsSqrt
        fadd    awx,1.0
        fdiv    awx,bwx,awx
        call    MsAtanSmallA
        fmul    awx,2.0

        leave   bwx,0,0

MsAtan2:
        enter   bwx|ar|br|fwx,0

        mov     br,[esp+20]  ; Y
        mov     ar,[esp+24]  ; X
        mov     awx,0x80000000
        and     fwx,br,awx
        and     awx,ar,awx
        shr     awx
        or      fwx,awx
        shr     fwx,24
        and     ar,0x7FFFFFFF
        and     br,0x7FFFFFFF
        cmp     br,ar
        bb      ma2NoInv
        mov     awx,ar
        mov     ar,br
        mov     br,awx
        or      fl,0x01
    ma2NoInv:
        fdiv    awx,br,ar
        cmp     awx,0.5
        bb      ma2Small
    ma2Big:
        call    MsAtanBigA      ; pi/4 to pi/2
        bra     ma2Sign
    ma2Small:
        call    MsAtanSmallA
    ma2Sign:
        test    fl,0x01
        bz      ma2NoInvA
        mov     bwx,CHalfPi
        fsub    awx,bwx,awx
    ma2NoInvA:
        and     fl,0xC0
        cmp     fl,0x40
        be      ma2QuarterB
        cmp     fl,0xC0
        be      ma2QuarterC
        cmp     fl,0x80
        be      ma2QuarterD
        ; ma2QuarterA
        bra     ma2Ret
    ma2QuarterB:
        mov     bwx,CMathPi
        fsub    awx,bwx,awx
        bra     ma2Ret
    ma2QuarterC:
        fadd    awx,CMathPi
        bra     ma2Ret
    ma2QuarterD:
        mov     bwx,CDualPi
        fsub    awx,bwx,awx
    ma2Ret:
        leave   bwx|ar|br|fwx,0,8


