Public LedYSet
Public LedRgbSet
Public WDReset

Public Sys@_MemCopy
Public DbgSendByteA

Public Writeln
Public IntToStr
Public FloatToStr
Public DbgMark
Public DbgSendByte
Public DbgSendHexT
Public SysStop
Public PinDbgSet
Public Bit4ToHexA

Public mtrunc

Public TimerBStart
Public TimerBStop
Public TimerBCheck

#include "IoIda.h"

.seg code

Sys@_MemCopy:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+28]
        mov     ewx,[esp+24]
        mov     cwx,[esp+20]

        cmp     cwx,zwx
        be      smcRet
    smcNext:
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     cwx
        bnz     smcNext

    smcRet:
        leave   ewx|cwx|bwx|awx,0,12

DbgSendByteA:
        enter   awx,0
    dsbaWait:
        mov     al,[IobDbgTestCtrl]
        test    al,0x02
        bnz     dsbaSend
        swt     dsbaWait
    dsbaSend:
        mov     al,[esp+0]
        mov     [IobDbgTermData],al
        leave   awx,0,0

LedYSet:
        enter   awx,0
        mov     al,[esp+8]
        mov     [IobLedY],al
        leave   awx,0,4

LedRgbSet:
        enter   awx,0
        mov     al,[esp+8]
        mov     [IobLedRgb],al
        leave   awx,0,4

PinDbgSet:
        enter   awx,0
        mov     al,[esp+8]
        mov     [IobPinDbg],al
        leave   awx,0,4

WDReset:
        enter   awx,0
        mov     awx,2
    wdrWait:
        dec     awx
        swt_nz  wdrWait
        leave   awx,0,0

;ExtIoCfg:
;        push    awx
;        mov     ax,[esp+8]
;        out     IowExtCtrl,ax
;        mov     al,[esp+12]
;        out     IobExtOutEn,al
;        mov     al,0xFF
;        out     IobExtIrqR,al
;        pop     awx
;        ret     8
;
;ExtIoDataWr:
;        push    awx
;        mov     al,[esp+8]
;        out     IobExtData,al
;        pop     awx
;        ret     4
;
;ExtIoDataRd:
;        in      al,IobExtData
;        ret

TtySendByte:
        enter   awx,0
    tsbWait:
        mov     al,[IobDbgTestCtrl]
        test    al,0x02
        bnz     tsbSend
        swt     tsbWait
    tsbSend:
        mov     al,[esp+0]
        mov     [IobDbgTermData],al
        leave   awx,0,0

Writeln:
        enter   awx|bwx,0
        mov     bwx,[esp+12]
        mov     ah,[bwx++]
        cmp     ah,0
        be      wStop
    wNext:
        mov     al,[bwx++]
        call    DbgSendByteA
        dec     ah
        bnz     wNext
    wStop:
        mov     al,13
        call    DbgSendByteA
        leave   bwx|awx,0,4

IntToStr:
        enter   awx|bwx|cwx|dwx|ewx|fwx,0

        mov     cl,0
        mov     ewx,[esp+28]
        mov     awx,[esp+32]
        inc     ewx
        mov     fh,0
        mov     dwx,awx
        mov     bwx,1000000000
        bt      awx,31
        bnc     itsPositive
        mov     al,'-'
        mov     [ewx++],al
        inc     cl
        xor     dwx,0xFFFFFFFF
        inc     dwx
        mov     awx,dwx
    itsPositive:
    itsNext:
        mov     dwx,awx
        udiv    awx,bwx
        urem    dwx,bwx
        cmp     awx,zwx
        bne     itsOutData
        cmp     fh,0
        be      itsSkipOut
    itsOutData:
        add     al,'0'
        mov     [ewx++],al
        inc     cl
        or      fh,1
    itsSkipOut:
        mov     awx,10
        udiv    bwx,awx
        mov     awx,dwx
        cmp     bwx,1
        ba      itsNext

        add     dl,'0'
        mov     [ewx++],dl
        inc     cl

        mov     ewx,[esp+28]
        mov     [ewx],cl

        leave   fwx|ewx|dwx|cwx|bwx|awx,0,8

mtrunc:
        enter   #,0
        mov     awx,[esp+4]
        trunc   awx
        leave   #,0,4

FloatToStr:
        enter   awx|bwx|cwx|dwx|ewx|fwx,0

        mov     cl,0
        mov     ewx,[esp+28]
        ; Int part
        mov     awx,[esp+32]
        inc     ewx
        mov     fh,0
        mov     dwx,awx
        test    dwx,0x80000000
        bz      itfPositive
        and     dwx,0x7FFFFFFF
        mov     al,'-'
        mov     [ewx++],al
        inc     cl
     itfPositive:
        mov     bwx,1000000000
        trunc   dwx
        mov     awx,dwx
    itfNext:
        mov     dwx,awx
        udiv    awx,bwx
        urem    dwx,bwx
        cmp     awx,zwx
        bne     itfOutData
        cmp     fh,0
        be      itfSkipOut
    itfOutData:
        add     al,'0'
        mov     [ewx++],al
        inc     cl
        or      fh,1
    itfSkipOut:
        mov     awx,10
        udiv    bwx,awx
        mov     awx,dwx
        cmp     bwx,1
        ba      itfNext

        add     dl,'0'
        mov     [ewx++],dl
        mov     al,'.'
        mov     [ewx++],al
        inc     cl,2

        ; Frac part
        mov     awx,[esp+32]
        and     aw,0x7FFF
        mov     dwx,awx
        trunc   awx
        itf     awx
        fsub    dwx,dwx,awx
        mov     awx,1000.0
        fmul    dwx,dwx,awx
        trunc   dwx
        mov     bwx,100
        mov     awx,dwx
    itfNextA:
        mov     dwx,awx
        udiv    awx,bwx
        urem    dwx,bwx
        add     al,'0'
        mov     [ewx++],al
        inc     cl
        mov     awx,10
        udiv    bwx,awx
        mov     awx,dwx
        cmp     bwx,1
        ba      itfNextA

        add     dl,'0'
        mov     [ewx++],dl
        inc     cl
        mov     ewx,[esp+28]
        mov     [ewx],cl

        leave   fwx|ewx|dwx|cwx|bwx|awx,0,8

DbgMark:
        enter   #,0
        nop
        trap
        nop
        leave   #,0,0

DbgSendByte:
        enter   awx,0
        mov     al,[esp+8]
        bra     DbgSendByteA
        leave   awx,0,4

Bit4ToHexA:
        enter   #,0
        and     al,0x0F
        add     al,'0'
        cmp     al,'9'
        bbe     bthaRet
        add     al,7
    bthaRet:
        leave   #,0,0

DbgSendHexT:
        enter   awx,0
        mov     al,'<'
        call    DbgSendByteA
        mov     ah,[esp+8]
        shr     al,ah,4
        call    Bit4ToHexA
        call    DbgSendByteA
        mov     al,ah
        call    Bit4ToHexA
        call    DbgSendByteA
        mov     al,'>'
        call    DbgSendByteA
        leave   awx,0,4

TimerBStart:
        enter   awx,0
        mov     [IobTimerBCtrl],zl
        mov     ax,[esp+8]
        mov     [IowTimerBCmpA],ax
        mov     al,0x14
        mov     [IobTimerBCtrl],al
        mov     al,0x01
        mov     [IobTimerBIrqR],al
        leave   awx,0,4

TimerBStop:
        enter   awx,0
        mov     [IobTimerBCtrl],zl
        mov     al,0x01
        mov     [IobTimerBIrqR],al
        leave   awx,0,0

TimerBCheck:
        enter   #,0
        mov     al,[IobTimerBCtrl]
        and     al,0x01
        leave   #,0,0

SysStop:
        enter   #,0
        nop
        trap
        nop
        leave   #,0,0

