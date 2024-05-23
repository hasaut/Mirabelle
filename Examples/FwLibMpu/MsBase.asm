Public AppendThreadSd
Public AppendThreadRv
Public AppendIrqSd

Public AppendThreadRv_2seg
Public AppendThreadRv_2seg_IO

.seg code

AppendThreadSd:
        enter   awx|cwx|ewx,0
        mov     cl,24
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
        mov     cl,24
        mov     ewx,dwx
   atrFillZNext:
        mov     [ewx++],zwx
        dec     cl
        bnz     atrFillZNext
        mov     awx,[esp+8]
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0

AppendIrqSd:
        enter   awx|cwx|ewx,0
        mov     cl,24
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

AppendThreadRv_2seg:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   atr2sFillZNext:
        mov     [ewx++],zwx
        dec     cl
        bnz     atr2sFillZNext
        mov     [ewx++],fwx
        mov     [ewx++],fr
        mov     [ewx++],gwx
        mov     [ewx++],gr
        mov     [ewx++],zwx
        mov     [ewx++],zwx
        mov     [ewx++],zwx
        mov     [ewx++],zwx
        mov     awx,[esp+8]
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0

AppendThreadRv_2seg_IO:
        enter   awx|cwx|ewx,0
        mov     cl,16
        mov     ewx,dwx
   atr2sioFillZNext:
        mov     [ewx++],zwx
        dec     cl
        bnz     atr2sioFillZNext
        mov     [ewx++],fwx
        mov     [ewx++],fr
        mov     [ewx++],gwx
        mov     [ewx++],gr
        mov     [ewx++],zwx
        mov     [ewx++],zwx
        mov     awx,0x01000000
        mov     [ewx++],awx
        mov     awx,0x60000000
        mov     [ewx++],awx
        mov     awx,[esp+8]
        mov     [dwx],awx
        mov     [bwx++],dwx
        leave   awx|cwx|ewx,0,0


