Public ZVect3s
Public EVect3s
Public CAxisX
Public CAxisY
Public CAxisZ

Public Sqrt
Public Abs
Public Round

.seg code
 Align 4
ZVect3s: dd 0.0, 0.0, 0.0
EVect3s: dd 1.0, 1.0, 1.0
CAxisX:  dd 1.0, 0.0, 0.0
CAxisY:  dd 0.0, 1.0, 0.0
CAxisZ:  dd 0.0, 0.0, 1.0

Sqrt:
        enter   bwx|cwx|dwx|ewx,0
        mov     awx,[esp+20]
        mov     ewx,awx
        ;// figure out the starting value
        and     awx,0x7F800000
        bz      msSkipShift
        sub     awx,0x3F800000
        shr     awx
        add     awx,0x3F800000
        and     awx,0x7F800000
    msSkipShift:
        mov     dwx,awx
        mov     cwx,0x00400000

    msNextB:
        mov     bwx,dwx
    msNextA:
        or      bwx,cwx
        fmul    awx,bwx,bwx
        cmp     awx,ewx
        bae     msSkipAdd
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

Abs:
        enter   #,0
        mov     awx,[esp+4]
        and     awx,0x7FFFFFFF
        leave   #,0,4

Round:
        enter   #,0
        mov     awx,[esp+4]
        round   awx
        leave   #,0,4


