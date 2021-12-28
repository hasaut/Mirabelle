;@M IrqTimerA
;@S /home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci

Public IrqTimerA

Extern TimerAWait
Extern LedYSet

.seg data
        Align 1
   FActive:              db 0

.seg code

;@P [Proc:~xh_{}:IrqTimerA#][ChTargList:~dbb:BLedY# ~deb:BTmp0# ~deb:BTmp2# ][VarStackMap:~dbb:BLedY# 1 0 0 00000 ~deb:BTmp0# 1 0 0 00000 ~deb:BTmp2# 1 0 0 00000 ]
IrqTimerA:
        enter    awx|bwx,0        ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:6,1][Esp:-8][SrcOrig:]
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:6,1][Esp:-8][SrcOrig:]
        mov      al,1             ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:10,11][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:.d.][Esp:0][TargRegMap:  .  al   . ][SrcOrig:]FActive=1;
        mov      [FActive],al     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:10,11][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:.s.][Esp:0][TargRegMap:  .  al   . ][SrcOrig:]FActive=1;
        mov      bl,0x00          ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:11,12][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:2][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:d..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]BLedY=0x00;
        pushzx   bl               ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:12,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:3][JmpsPos:4][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:s..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:12,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:4][JmpsPos:5][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:|..][Esp:4][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
     IrqTimerA_WhileCmpA0_U1:     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:14,7][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:6][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]while (FActive)
        mov      al,[FActive]     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:14,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:7][JmpsPos:8][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:|.d][Esp:0][TargRegMap: bl   .  al ][SrcOrig:]while (FActive)
        cmp      al,zl            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:14,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:8][JmpsPos:9][VarsPos:8][SRecPos:8][OrdrPos:8][MatrPos:8][UseMatr:|.s][Esp:0][TargRegMap: bl   .  al ][SrcOrig:]while (FActive)
        be       IrqTimerA_X4     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:14,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:9][JmpsPos:10][VarsPos:9][SRecPos:9][OrdrPos:9][MatrPos:9][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]while (FActive)
        call     TimerAWait       ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:16,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:12][JmpsPos:11][VarsPos:10][SRecPos:10][OrdrPos:10][MatrPos:10][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]TimerAWait();
        pushzx   bl               ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:17,18][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:14][JmpsPos:13][VarsPos:11][SRecPos:11][OrdrPos:11][MatrPos:11][UseMatr:s..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:17,18][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:15][JmpsPos:14][VarsPos:12][SRecPos:12][OrdrPos:12][MatrPos:12][UseMatr:|..][Esp:4][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
        inc      bl               ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:18,11][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:17][JmpsPos:16][VarsPos:13][SRecPos:13][OrdrPos:13][MatrPos:13][UseMatr:x..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]BLedY++;
        bra      IrqTimerA_WhileCmpA0_U1 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:19,4][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:19][JmpsPos:18][VarsPos:14][SRecPos:14][OrdrPos:14][MatrPos:14][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]}
     IrqTimerA_X4:                ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:19,4][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:20][JmpsPos:19][VarsPos:15][SRecPos:15][OrdrPos:15][MatrPos:15][UseMatr:...][Esp:0][TargRegMap:  .   .   . ][SrcOrig:]}
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:19,1][Esp:0][SrcOrig:]
        leave    awx|bwx,0,0      ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/IrqTimerA.ci][SrcPos:19,1][Esp:0][SrcOrig:]

