Public AppendThreadSd
Public AppendThreadRv
Public AppendIrqSd


.seg code

AppendThreadSd:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   atsFillZNext:
        mov     [ewx++],zwx
        dec     cl,1
        bnz     atsFillZNext
        mov     awx,[esp+8]
        or      awx,0x10000000
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0

AppendThreadRv:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   atrFillZNext:
        mov     [ewx++],zwx
        dec     cl,1
        bnz     atrFillZNext
        mov     awx,[esp+8]
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0

AppendIrqSd:
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


