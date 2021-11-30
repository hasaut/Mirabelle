.seg data
;.org 0x10000

FDataA:   dd 0
FDataB:   dw 0

.seg code

Start_@ep:
StartTable:
        dd StartA, StartB, StartC, StartD

StartA:
        inc     al
        inc     bl
        inc     cl
        sjmp    StartA

StartB:
        mov     el,2
        add     el,4
        inc     fl
        inc     gl
        sjmp    StartB

StartC:
        mov     bl,1
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        inc     bl
        sjmp    StartC

StartD:
        mov     cl,8
        shl     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        rol     cl
        sjmp    StartD

