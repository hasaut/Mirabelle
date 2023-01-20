Public StrCmpCP
Public StrCmpDP
Public StrCopyP
Public StrPosCP ; Code, Pas
Public StrWPosCP ; Same as before, but origin str is >256 bytes
Public StrWPosCP_Len ; same as StrWPosCP, but requires CX bytes in the buffer. Used for Wifi recv
Public StrDel
Public StrDelFirstSpace
Public StrRdParam       ; AL - separator
Public StrRdParamCheckLen ; AL - separator, AH - Max length
Public StrToWord
Public StrToWordEwx       ; EWX -> Str
Public HexToWord
Public StrConvertCP
Public WordToStr
Public StrHexToBinA ; string is in EWX

Public Sys@_StrMovS
Public Sys@_StrAddS
Public Sys@_StrCmpS
Public Sys@_StrMovC
Public Sys@_StrAddC
Public Sys@_StrCmpC

Public strcpy
Public strcmp

Public Length
Public SetLength
Public ReadParamStr
Public ReadTillC
Public LowerCase
Public TryHexToInt
Public TryStrToInt
Public DelFirstSpace
Public Delete
Public HexToBin
Public BinToHex
Public Copy
Public IntToHex

Extern HexToBit4
Extern Bit4ToHex

.seg code
        Align   4
StrCmpCP:
        enter   awx|bwx|cwx|ewx,0

        mov     al,[ewx++]
        mov     ah,[bwx++]
        cmp     al,ah
        bne     cspRet
        mov     cl,al
        cmp     al,0
        be      cspRet
    cspNext:
        mov     al,[ewx++]
        mov     ah,[bwx++]
        cmp     al,ah
        bne     cspRet
        dec     cl
        bnz     cspNext

    cspRet:
        leave   awx|bwx|cwx|ewx,0,0

StrCmpDP:
        enter   awx|bwx|cwx|ewx,0

        mov     al,[ewx++]
        mov     ah,[bwx++]
        cmp     al,ah
        bne     scdpRet
        mov     cl,al
        cmp     al,0
        be      scdpRet
    scdpNext:
        mov     al,[ewx++]
        mov     ah,[bwx++]
        cmp     al,ah
        bne     scdpRet
        dec     cl
        bnz     scdpNext

    scdpRet:
        leave   awx|bwx|cwx|ewx,0,0

StrCopyP:
        enter   awx|bwx|ewx,0

        mov     ah,[bwx++]
        mov     [ewx++],ah
        cmp     ah,0
        be      scRet
    scNext:
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bnz     scNext

    scRet:
        leave   awx|bwx|ewx,0,0

; AH - start index (beginning with 1, like in Pascal)
; AL - Bytes to delete
StrDel:
        enter   awx|bwx|cwx|ewx,0

        cmp     ah,0
        be      sdRet
        cmp     al,0
        be      sdRet
        mov     ch,[bwx]        ; Len of origin
        cmp     ch,ah           ; Len < index
        bb      sdRet
        mov     cl,al
        add     cl,ah
        movzx   ewx,cl
        add     ewx,bwx
        cmp     ch,cl
        bae     sdDoDel         ; There is a tail to move
        dec     ah              ; No tail, crop the string
        mov     [bwx],ah
        bra     sdRet
    sdDoDel:
        sub     ch,al           ; Resulting String length
        mov     [bwx],ch
        sub     ch,ah
        inc     ch              ; Bytes to copy
        movzx   awx,ah
        add     bwx,awx
    sdDelNext:
        mov     al,[ewx++]
        mov     [bwx++],al
        dec     ch
        bnz     sdDelNext

    sdRet:
        leave   awx|bwx|cwx|ewx,0,0

StrDelFirstSpace:
        enter   awx,0
    sdfsNext:
        mov     ah,[bwx+0]
        mov     al,[bwx+1]
        cmp     ah,0
        be      sdfsRet
        cmp     al,' '
        bne     sdfsRet
        mov     ax,0x0101
        bra     StrDel
        bra     sdfsNext
    sdfsRet:
        leave   awx,0,0

StrRdParam:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     dl,al

        mov     cl,[bwx++]
        mov     ch,0
        mov     [ewx++],ch
        cmp     cl,0
        be      srpRet

    srpRdNext:
        mov     al,[bwx++]
        cmp     al,dl
        be      srpRdEnd
        mov     [ewx++],al
        inc     ch
        dec     cl
        bnz     srpRdNext
    srpRdEnd:
        mov     ewx,[esp+0]
        mov     [ewx],ch
        mov     ah,1
        mov     al,ch
        inc     al
        mov     bwx,[esp+12]
        bra     StrDel

    srpRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

StrRdParamCheckLen:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     dl,al

        mov     cl,[bwx++]
        mov     ch,0
        mov     [ewx++],ch
        cmp     cl,0
        be      srpclRet

    srpclRdNext:
        mov     al,[bwx++]
        cmp     al,dl
        be      srpclRdEnd
        cmp     ch,ah
        bae     srpclSkipStore
        mov     [ewx++],al
    srpclSkipStore:
        inc     ch
        dec     cl
        bnz     srpclRdNext
    srpclRdEnd:
        mov     ewx,[esp+0]
        mov     [ewx],ch
        cmp     ch,ah
        bae     srpclLenOk
        mov     [ewx],ah
    srpclLenOk:
        mov     ah,1
        mov     al,ch
        inc     al
        mov     bwx,[esp+12]
        bra     StrDel

    srpclRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

StrToWordEwx:
        enter   bwx,0
        mov     bwx,ewx
        bra     StrToWord
        leave   bwx,0,0

StrToWord:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     ax,0
        mov     ex,ax
        mov     dx,10
        mov     cl,[bwx++]
        cmp     cl,0
        be      stbEnd
    stbNext:
        mov     el,[bwx++]
        mul     ax,dx
        sub     el,'0'
        bb      stbError
        cmp     el,9
        ba      stbError
        add     ax,ex
        dec     cl
        bnz     stbNext

    stbEnd:
        mov     [esp+16],ax
        and     fl,0xFE

    stbRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

    stbError:
        or      fl,0x01
        bra     stbRet

WordToStr:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     cl,5
        mov     [ewx],cl
        add     ewx,5
        mov     bx,10
    wtsDivNext:
        urem    dx,ax,bx
        add     dl,'0'
        mov     [ewx],dl
        dec     ewx
        dec     cl
        udiv    ax,bx
        cmp     ax,0
        bne     wtsDivNext

        cmp     cl,0
        be      wtsSkipDel
        mov     bwx,[esp+0]
        mov     al,cl
        mov     ah,1
        bra     StrDel
    wtsSkipDel:

        leave   awx|bwx|cwx|dwx|ewx,0,0

HexToWord:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     ax,0
        mov     ex,ax
        mov     dx,16
        mov     cl,[bwx++]
        cmp     cl,0
        be      htwEnd
    htwNext:
        mov     el,[bwx++]
        mul     ax,dx
        sub     el,'0'
        bb      htwError
        cmp     el,9
        bbe     htwProcess
        sub     el,17
        bb      htwError
        cmp     el,6
        bbe     htwProcessA
        sub     el,32
        bb      htwError
        cmp     el,6
        bae     htwError
    htwProcessA:
        add     el,10
    htwProcess:
        add     ax,ex
        dec     cl
        bnz     htwNext

    htwEnd:
        mov     [esp+16],ax
        and     fl,0xFE

    htwRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

    htwError:
        or      fl,0x01
        bra     htwRet

StrConvertCP:
        enter   awx|bwx|cwx|ewx,0

        mov     cl,0
        inc     ewx
        mov     al,[bwx++]
    sccpNext:
        cmp     al,0
        be      sccpLastByte
        mov     [ewx++],al
        inc     cl
        mov     al,[bwx++]
        bra     sccpNext
    sccpLastByte:
        mov     ewx,[esp+0]
        mov     [ewx],cl

        leave   awx|bwx|cwx|ewx,0,0

StrPosCP:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     dwx,0
        mov     al,[ewx]
        cmp     al,0
        be      spcpError

    spcpNext:
        mov     bwx,[esp+12]
        mov     ewx,[esp+0]
        mov     ah,[bwx++]
        mov     al,[ewx++]
        mov     cx,ax
        add     al,dl
        cmp     ah,al
        bb      spcpRet
        add     bwx,dwx
    spcpNextByte:
        mov     ah,[bwx++]
        mov     al,[ewx++]
        cmp     al,ah
        bne     spcpMovePtr
        dec     cl
        bnz     spcpNextByte

        ; Found
        mov     [esp+16],dl
        and     fl,0xFE
        bra     spcpRet

    spcpMovePtr:
        inc     dl
        bra     spcpNext

    spcpError:
        or      fl,0x01
        ;bra     spcpRet

    spcpRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

StrWPosCP:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     dwx,0
    swpcpX:
        mov     al,[ewx]
        cmp     al,0
        be      swpcpError

    swpcpNext:
        mov     ewx,[esp+0]
        mov     bwx,[esp+12]
        mov     al,[ewx++]
        mov     cw,[bwx++]
        movzx   ax,al           ; CX = substr len
        mov     cx,ax
        add     ax,dx
        cmp     cw,ax
        bb      swpcpRet
        add     bwx,dwx
    swpcpNextByte:
        mov     ah,[bwx++]
        mov     al,[ewx++]
        cmp     al,ah
        bne     swpcpMovePtr
        dec     cx
        bnz     swpcpNextByte

        ; Found
        mov     [esp+16],dx
        and     fl,0xFE
        bra     swpcpRet

    swpcpMovePtr:
        inc     dx
        bra     swpcpNext

    swpcpError:
        or      fl,0x01
        ;bra     spcpRet

    swpcpRet:
        leave   awx|bwx|cwx|dwx|ewx,0,0

StrWPosCP_Len:
        enter   awx|bwx|cwx|dwx|ewx,0

        movzx   dwx,cx
        bra     swpcpX

StrHexToBinA:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,ewx
        mov     cl,[bwx++]
        shr     cl
        mov     [ewx++],cl
        cmp     cl,0
        be      shtbaRet
    shtbaNext:
        mov     al,[bwx++]
        bra     HexToBit4
        mov     ah,al
        mov     al,[bwx++]
        bra     HexToBit4
        shl     ah
        shl     ah
        shl     ah
        shl     ah
        add     al,ah
        mov     [ewx++],al
        dec     cl
        bnz     shtbaNext
    shtbaRet:

        leave   awx|bwx|cwx|ewx,0,0

Sys@_StrCmpS:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+20]    ; Left part
        mov     ewx,[esp+24]    ; Right part

        mov     al,[bwx++]
        mov     ah,[ewx++]
        cmp     al,ah
        bne     sscsRet
        mov     cl,al
        cmp     al,0
        be      sscsRet
    sscsNext:
        mov     al,[bwx++]
        mov     ah,[ewx++]
        dec     cl
        bz      sscsLastCmp
        cmp     al,ah
        bne     sscsRet
        bra     sscsNext

    sscsLastCmp:
        cmp     al,ah
    sscsRet:
        leave   awx|bwx|cwx|ewx,0,8

Sys@_StrMovS:
        enter   awx|bwx|ewx,0

        ;mov     ax,0x0001
        ;out     IoHexData,ax

        mov     bwx,[esp+24] ; SRC
        mov     ewx,[esp+20] ; DST
        mov     al,[esp+16]  ; Max DST len

        mov     ah,[bwx++]
        mov     [ewx++],ah
        cmp     ah,0
        be      ssmsRet
        cmp     ah,al
        bbe     ssmsStartCopy
        mov     ah,al
    ssmsStartCopy:
        bra     ssmsNext
        Align 8
    ssmsNext:
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bz      ssmsCopyEnd
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bz      ssmsCopyEnd
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bz      ssmsCopyEnd
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bz      ssmsCopyEnd
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     ah
        bnz     ssmsNext
    ssmsCopyEnd:

    ssmsRet:
        leave   awx|bwx|ewx,0,12

Sys@_StrAddS:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+28] ; SRC
        mov     ewx,[esp+24] ; DST
        mov     ah,[esp+20]  ; Max DST len

        mov     al,[ewx]     ; Len of DST
        mov     ch,al
        cmp     al,ah
        bae     ssasRet
        mov     cl,[bwx++]   ; Len of SRC (How many bytes to copy)
        add     al,cl
        cmp     al,ah
        bbe     ssasStartCopy
        mov     cl,ah
        sub     cl,al
        mov     al,ah

    ssasStartCopy:
        mov     [ewx++],al      ; New len of DST
        cmp     cl,0
        be      ssasRet
        movzx   awx,ch
        add     ewx,awx
    ssasNext:
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     cl
        bz      ssasCopyEnd
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     cl
        bnz     ssasNext
    ssasCopyEnd:

    ssasRet:
        leave   awx|bwx|cwx|ewx,0,12

Sys@_StrMovC:
        enter   awx|bwx|ewx,0

        mov     bl,[esp+24] ; SRC
        mov     ewx,[esp+20] ; DST
        mov     al,[esp+16]  ; Max DST len

        cmp     al,1
        bb      ssmcRet
        mov     al,1
        mov     [ewx++],al
        mov     [ewx++],bl

    ssmcRet:
        leave   awx|bwx|ewx,0,12

Sys@_StrAddC:
        enter   awx|bwx|cwx|ewx,0

        mov     bl,[esp+28] ; SRC
        mov     ewx,[esp+24] ; DST
        mov     ah,[esp+20]  ; Max DST len

        mov     al,[ewx]     ; Len of DST
        movzx   cwx,al
        cmp     al,ah
        bae     ssacRet
        inc     al
        cmp     al,ah
        bae     ssacRet

        mov     [ewx++],al      ; New len of DST
        add     ewx,cwx
        mov     [ewx],bl

    ssacRet:
        leave   awx|bwx|cwx|ewx,0,12

Sys@_StrCmpC:
        enter   awx|bwx,0

        mov     bwx,[esp+12]    ; Left part
        mov     ah,[esp+16]     ; Right part

        mov     al,[bwx++]
        cmp     al,1
        bne     ssccRet
        mov     al,[bwx++]
        cmp     al,ah
        ;bne     ssccRet

    ssccRet:
        leave   awx|bwx,0,8

Length:
        enter   bwx,0
        mov     bwx,[esp+8]
        mov     al,[bwx]
        leave   bwx,0,4

SetLength:
        enter   awx|bwx,0

        mov     bwx,[esp+12]
        mov     al,[esp+16]
        mov     [bwx+0],al

        leave   awx|bwx,0,8

strcpy:
        enter   awx|bwx|ewx,0

        mov     bwx,[esp+20]
        mov     ewx,[esp+16]
    scpNext:
        mov     al,[bwx++]
        cmp     al,0
        mov     [ewx++],al
        bnz     scpNext

        leave   awx|bwx|ewx,0,8

strcmp:
        enter   awx|bwx|ewx,0

        mov     bwx,[esp+20]
        mov     ewx,[esp+16]
    scmNext:
        mov     al,[bwx++]
        mov     ah,[ewx++]
        cmp     al,ah
        bne     scmDiffer
        cmp     al,0
        bne     scmNext
        mov     al,1
        bra     scmOpti
    scmDiffer:
        mov     al,0
    scmOpti:
        mov     [esp+8],al

        leave   awx|bwx|ewx,0,8

ReadParamStr:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     bwx,[esp+28]
        mov     ewx,[esp+24]

        mov     cl,[bwx++]
        mov     ch,0
        mov     [ewx++],ch
        cmp     cl,0
        be      rpsRet

        mov     dh,0
    rpsDetStartNext:
        mov     al,[bwx]
        cmp     al,32
        bne     rpsStartFound
        inc     dh
        inc     bwx
        dec     cl
        bnz     rpsDetStartNext
        bra     rpsRdEnd

    rpsStartFound:
    rpsRdNext:
        mov     al,[bwx++]
        cmp     al,32
        be      rpsRdEnd
        mov     [ewx++],al
        inc     ch
        inc     dh
        dec     cl
        bnz     rpsRdNext
    rpsRdEnd:
        mov     ewx,[esp+24]
        mov     [ewx],ch
        mov     ah,1
        mov     al,dh
        inc     al
        mov     bwx,[esp+28]
        bra     StrDel

    rpsRet:
        leave   awx|bwx|cwx|dwx|ewx,0,8

ReadTillC:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     bwx,[esp+28]
        mov     ewx,[esp+24]
        mov     ah,[esp+32]

        mov     cl,[bwx++]
        mov     ch,0
        mov     [ewx++],ch
        cmp     cl,0
        be      rpscRet

        mov     dh,0
    rpscDetStartNext:
        mov     al,[bwx]
        cmp     al,32
        bne     rpscStartFound
        inc     dh
        inc     bwx
        dec     cl
        bnz     rpscDetStartNext
        bra     rpscRdEnd

    rpscStartFound:
    rpscRdNext:
        mov     al,[bwx++]
        cmp     al,ah
        be      rpscRdEnd
        mov     [ewx++],al
        inc     ch
        inc     dh
        dec     cl
        bnz     rpscRdNext
    rpscRdEnd:
        mov     ewx,[esp+24]
        mov     [ewx],ch
        mov     ah,1
        mov     al,dh
        inc     al
        mov     bwx,[esp+28]
        bra     StrDel

    rpscRet:
        leave   awx|bwx|cwx|dwx|ewx,0,12

LowerCase:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+24]
        mov     ewx,[esp+20]

        mov     cl,[bwx++]
        mov     [ewx++],cl
        cmp     cl,0
        be      lcRet

    lcRdNext:
        mov     al,[bwx++]
        cmp     al,'A'
        bb      lcSkipChange
        cmp     al,'Z'
        ba      lcSkipChange
        add     al,32
    lcSkipChange:
        mov     [ewx++],al
        dec     cl
        bnz     lcRdNext

    lcRet:

        leave   awx|bwx|cwx|ewx,0,8

TryHexToInt:
        enter   awx|bwx|cwx|dwx,0

        mov     al,0x00
        mov     [esp+12],al

        mov     dwx,0
        mov     bwx,[esp+20]
        mov     cl,[bwx++]
        cmp     cl,0
        be      thtiError
    thtiNext:
        mov     al,[bwx++]
        cmp     al,'0'
        bb      thtiError
        cmp     al,'9'
        ba      thtiCmpUA
        sub     al,'0'
        bra     thtiCompute
    thtiCmpUA:
        cmp     al,'A'
        bb      thtiError
        cmp     al,'F'
        ba      thtiCmpLA
        sub     al,'A'
        add     al,10
        bra     thtiCompute
    thtiCmpLA:
        cmp     al,'a'
        bb      thtiError
        cmp     al,'f'
        ba      thtiError
        sub     al,'a'
        add     al,10
    thtiCompute:
        shl     dwx,dwx,4
        addzx   dwx,al
        dec     cl
        bnz     thtiNotLast
        mov     bwx,[esp+24]
        mov     [bwx],dwx
        mov     al,0x01
        mov     [esp+12],al
        bra     thtiRet
    thtiNotLast:
        test    dwx,0xF0000000
        bz      thtiNext
    thtiError:

    thtiRet:
        leave   awx|bwx|cwx|dwx,0,8

TryStrToInt:
        enter   awx|bwx|cwx|dwx,0

        mov     al,0x00
        mov     [esp+12],al

        mov     dwx,0
        mov     bwx,[esp+20]
        mov     cl,[bwx++]
        cmp     cl,0
        be      tstiError
        mov     ch,0
        mov     al,[bwx++]
        cmp     al,'-'
        bne     tstiNotNeg
        mov     ch,1
        dec     cl
        bz      tstiError
    tstiNext:
        mov     al,[bwx++]
    tstiNotNeg:
        cmp     al,'0'
        bb      tstiError
        cmp     al,'9'
        ba      tstiError
        sub     al,'0'
        mul     dwx,10
        movzx   awx,al
        add     dwx,awx
        dec     cl
        bnz     tstiNotLast
        mov     bwx,[esp+24]
        cmp     ch,0
        be      tstiPositive
        mov     cwx,0
        sub     dwx,cwx,dwx
    tstiPositive:
        mov     [bwx],dwx
        mov     al,0x01
        mov     [esp+12],al
        bra     tstiRet
    tstiNotLast:
        cmp     dwx,429496729
        bb      tstiNext
    tstiError:

    tstiRet:
        leave   awx|bwx|cwx|dwx,0,8

DelFirstSpace:
        enter   awx|bwx|cwx,0

        mov     bwx,[esp+16]
        mov     ch,0
        mov     cl,[bwx++]
        cmp     cl,0
        be      dfsRet
    dfsSearchNext:
        mov     al,[bwx++]
        cmp     al,32
        bne     dfsDel
        inc     ch
        dec     cl
        bnz     dfsSearchNext
        ; Empty string
        mov     bwx,[esp+16]
        mov     al,0
        mov     [bwx],al
        bra     dfsRet
    dfsDel:
        cmp     ch,0
        be      dfsRet
        mov     ah,1
        mov     al,ch
        bra     StrDel
    dfsRet:
        leave   awx|bwx|cwx,0,4

Delete:
        enter   awx|bwx,0

        mov     bwx,[esp+12]
        mov     ah,[esp+16]
        mov     al,[esp+20]
        bra     StrDel

        leave   awx|bwx,0,12

HexToBin:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+20]
        mov     ewx,bwx
        mov     cl,[bwx++]
        shr     cl
        mov     [ewx++],cl
        cmp     cl,0
        be      htbRet
    htbNext:
        mov     al,[bwx++]
        bra     HexToBit4
        mov     ah,al
        mov     al,[bwx++]
        bra     HexToBit4
        shl     ah,ah,4
        add     al,ah
        mov     [ewx++],al
        dec     cl
        bnz     htbNext
    htbRet:

        leave   awx|bwx|cwx|ewx,0,4

BinToHex:
        enter   awx|bwx|cwx|ewx,0

        mov     bwx,[esp+20]
        mov     ewx,bwx
        mov     cl,[bwx++]
        cmp     cl,127
        bb      bthSizeOk
        mov     cl,127
    bthSizeOk:
        mov     al,cl
        shl     al
        mov     [ewx++],al
        cmp     cl,0
        be      bthRet
        movzx   cwx,cl
        add     bwx,cwx
        movzx   awx,al
        add     ewx,awx
    bthNext:
        mov     al,[--bwx]
        mov     ah,al
        shr     ah,ah,4
        and     al,0x0F
        add     ax,0x3030
        cmp     ah,'9'
        bbe     bthHOK
        add     ah,7
    bthHOK:
        cmp     al,'9'
        bbe     bthLOK
        add     al,7
    bthLOK:
        mov     [--ewx],al
        mov     [--ewx],ah
        dec     cl
        bnz     bthNext
    bthRet:

        leave   awx|bwx|cwx|ewx,0,4

Copy:
        enter   awx|bwx|cwx|dwx|ewx,0

        mov     ewx,[esp+24]
        mov     bwx,[esp+28]
        mov     al,[esp+32]
        mov     cl,[esp+36]
        add     dl,al,cl
        dec     dl
        mov     dh,[bwx]
        sub     dl,dh
        bbe     cSizeOk
        sub     cl,dl
    cSizeOk:
        mov     [ewx++],cl
        cmp     cl,0
        be      cRet
        movzx   awx,al
        add     bwx,awx
    cNext:
        mov     al,[bwx++]
        mov     [ewx++],al
        dec     cl
        bnz     cNext

    cRet:
        leave   awx|bwx|cwx|dwx|ewx,0,16

IntToHex:
        enter   awx|bwx|cwx|dwx,0

        mov     cl,[esp+28]
        mov     dwx,[esp+24]
        mov     bwx,[esp+20]
        mov     [bwx++],cl
        cmp     cl,0
        be      ithRet
        mov     ch,9
        sub     ch,cl
        shl     ch,2
        rolzx   dwx,dwx,ch
    ithNext:
        mov     al,dl
        call    Bit4ToHex
        mov     [bwx++],al
        rol     dwx,dwx,4
        dec     cl
        bnz     ithNext

    ithRet:
        leave   awx|bwx|cwx|dwx,0,12

