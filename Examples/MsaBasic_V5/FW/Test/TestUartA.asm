Public UartRecvA
Public UartRecvB

Public FUartActive

Extern UartGetData

.seg data
    FUartActive: db 0

    FRecvLen:   db 0
    FRecvBuf:   db 0 dup(255)

    FState:     db 0
    FWorkLen:   db 0
    FWorkIdx:   db 0
    FWorkSave:  db 0
    FWorkSum:   db 0

.seg code

UartRecvA:
     uraWait:
        ; Header
        call    UartGetData
        bc      uraError
        cmp     al,0x55
        bne     uraWait                 ; 21
        ; Length
        call    UartGetData
        bc      uraError
        mov     cl,al
        mov     ch,al
        mov     dl,al           ; checksum
        mov     bwx,FRecvBuf
        mov     dh,[FRecvLen]
        cmp     cl,zl
        be      uraSkipRecv              ; 27
        ; Data
     uraRecvNext:
        call    UartGetData
        bc      uraError
        add     dl,al
        cmp     dh,zl
        bne     uraSkipSaveA
        mov     [bwx++],al
     uraSkipSaveA:
        dec     cl
        bnz     uraRecvNext               ; 25
     uraSkipRecv:
        ; Checksum
        call    UartGetData
        bc      uraError
        cmp     al,dl
        bne     uraError
        cmp     dh,zl
        bne     uraSkipSaveB
        mov     [FRecvLen],ch
     uraSkipSaveB:
        bra     uraWait                   ; 25
     uraError:
        bra     uraWait

UartRecvB:
        enter   awx|bwx,0               ; 4

        call    UartGetData             ; 2
        bc      urbError                ; 1
        mov     ah,[FState]             ; 2
        cmp     ah,zl                   ; 1
        be      urbStHdr                ; 1
        cmp     ah,0x01                 ; 1
        be      urbStLen                ; 1
        cmp     ah,0x02                 ; 1
        be      urbStData               ; 1
        cmp     ah,0x03                 ; 1
        be      urbStSum                ; 1
        mov     [FState],zl             ; 1
        bra     urbError                ; 1

    urbStHdr:
        cmp     al,0x55                 ; 1
        bne     urbError                ; 1
        mov     al,0x01
        mov     [FState],al
        bra     urbEnd

    urbStLen:
        mov     [FWorkLen],al
        mov     [FWorkSum],al
        mov     ah,[FRecvLen]
        mov     [FWorkSave],ah
        cmp     al,zl
        be      urbSkipRecv
        mov     ah,0x02
        bra     urbStLenSetState
    urbSkipRecv:
        mov     ah,0x03
    urbStLenSetState:
        mov     [FState],ah
        mov     [FWorkIdx],zl
        bra     urbEnd

    urbStData:
        mov     ah,[FWorkSum]
        add     ah,al
        mov     [FWorkSum],ah
        mov     bl,[FWorkIdx]
        movzx   bwx,bl
        mov     ah,[FWorkSave]
        cmp     ah,zl
        bne     urbStDataSkipSave
        mov     [FRecvBuf+bwx],al
    urbStDataSkipSave:
        inc     bl
        mov     [FWorkIdx],bl
        mov     al,[FWorkLen]
        cmp     bl,al
        bb      urbEnd
        mov     ah,0x03
        mov     [FState],ah
        bra     urbEnd

    urbStSum:
        mov     ah,[FWorkSum]
        cmp     ah,al
        bne     urbError
        mov     ah,[FWorkSave]
        cmp     ah,zl
        bne     urbStSumSkipSave
        mov     al,[FWorkLen]
        mov     [FRecvLen],al
    urbStSumSkipSave:
        mov     [FState],zl
        bra     urbEnd

    urbError:
        mov     [FState],zl

    urbEnd:
        leave   awx|bwx,0,0

        ; Hdr = 20
        ; Len = 29
        ; Data = 34
        ; Sum = 34

