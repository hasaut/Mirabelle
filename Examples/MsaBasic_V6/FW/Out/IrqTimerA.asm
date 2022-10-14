;@M IrqTimerA
;@S C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci

Public IrqTimerA

Extern TimerAWait
Extern LedYSet

.seg data
        Align 1
   FActive:              db 0

.seg code

;@P [Proc:~xh_{}:IrqTimerA#][ChTargList:~dbb:BLedY# ~dbb:BLedY# ~dbb:BLedY# ~deb:BTmp0# ~deb:BTmp3# ][VarStackMap:~dbb:BLedY# 1 0 0 00000 ~deb:BTmp0# 1 0 0 00000 ~deb:BTmp3# 1 0 0 00000 ]
IrqTimerA:
        enter    awx|bwx,0        ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:6,1][Esp:-8][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:6,1][Esp:-8][SrcOrig:]
        mov      al,1             ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:10,11][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:...d.][Esp:0][TargRegMap:  .   .   .  al   . ][SrcOrig:]FActive=1;
        mov      [FActive],al     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:10,11][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:...s.][Esp:0][TargRegMap:  .   .   .  al   . ][SrcOrig:]FActive=1;
        mov      al,0x00          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:11,12][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:2][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:d....][Esp:0][TargRegMap: al   .   .   .   . ][SrcOrig:]BLedY=0x00;
        pushzx   al               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:12,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:3][JmpsPos:4][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:s....][Esp:0][TargRegMap: al   .   .   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:12,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:4][JmpsPos:5][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:.....][Esp:4][TargRegMap:  .   .   .   .   . ][SrcOrig:]LedYSet(BLedY);
        call     TimerAWait       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:13,14][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:6][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:.....][Esp:0][TargRegMap:  .   .   .   .   . ][SrcOrig:]TimerAWait();
     IrqTimerA_WhileCmpA0_U1:     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:15,7][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:8][JmpsPos:9][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:.....][Esp:0][TargRegMap:  .   .   .   .   . ][SrcOrig:]while (FActive)
        mov      al,[FActive]     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:15,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:9][JmpsPos:10][VarsPos:8][SRecPos:8][OrdrPos:8][MatrPos:8][UseMatr:....d][Esp:0][TargRegMap:  .   .   .   .  al ][SrcOrig:]while (FActive)
        cmp      al,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:15,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:10][JmpsPos:11][VarsPos:9][SRecPos:9][OrdrPos:9][MatrPos:9][UseMatr:....s][Esp:0][TargRegMap:  .   .   .   .  al ][SrcOrig:]while (FActive)
        be       IrqTimerA_X10    ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:15,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:11][JmpsPos:12][VarsPos:10][SRecPos:10][OrdrPos:10][MatrPos:10][UseMatr:.....][Esp:0][TargRegMap:  .   .   .   .   . ][SrcOrig:]while (FActive)
        mov      bl,0x01          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:18,14][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:14][JmpsPos:13][VarsPos:11][SRecPos:11][OrdrPos:11][MatrPos:11][UseMatr:.d...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]BLedY=0x01;
     IrqTimerA_Loop11:            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:24,6][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:28][JmpsPos:14][VarsPos:12][SRecPos:12][OrdrPos:12][MatrPos:12][UseMatr:.|...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]}
        pushzx   bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:21,20][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:20][JmpsPos:15][VarsPos:13][SRecPos:13][OrdrPos:13][MatrPos:13][UseMatr:.s...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:21,20][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:21][JmpsPos:16][VarsPos:14][SRecPos:14][OrdrPos:14][MatrPos:14][UseMatr:.|...][Esp:4][TargRegMap:  .  bl   .   .   . ][SrcOrig:]LedYSet(BLedY);
        call     TimerAWait       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:22,18][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:23][JmpsPos:18][VarsPos:15][SRecPos:15][OrdrPos:15][MatrPos:15][UseMatr:.|...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]TimerAWait();
        shl      bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:23,15][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:26][JmpsPos:21][VarsPos:16][SRecPos:16][OrdrPos:16][MatrPos:16][UseMatr:.x...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]BLedY<<=1;
        cmp      bl,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:19,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:16][JmpsPos:23][VarsPos:17][SRecPos:17][OrdrPos:17][MatrPos:17][UseMatr:.s...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]while (BLedY)
        bne      IrqTimerA_Loop11 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:19,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:17][JmpsPos:24][VarsPos:18][SRecPos:18][OrdrPos:18][MatrPos:18][UseMatr:.|...][Esp:0][TargRegMap:  .  bl   .   .   . ][SrcOrig:]while (BLedY)
        mov      bl,0x80          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:26,14][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:30][JmpsPos:25][VarsPos:19][SRecPos:19][OrdrPos:19][MatrPos:19][UseMatr:..d..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]BLedY=0x80;
     IrqTimerA_Loop12:            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:32,6][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:44][JmpsPos:26][VarsPos:20][SRecPos:20][OrdrPos:20][MatrPos:20][UseMatr:..|..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]}
        pushzx   bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:29,20][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:36][JmpsPos:27][VarsPos:21][SRecPos:21][OrdrPos:21][MatrPos:21][UseMatr:..s..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:29,20][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:37][JmpsPos:28][VarsPos:22][SRecPos:22][OrdrPos:22][MatrPos:22][UseMatr:..|..][Esp:4][TargRegMap:  .   .  bl   .   . ][SrcOrig:]LedYSet(BLedY);
        call     TimerAWait       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:30,18][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:39][JmpsPos:30][VarsPos:23][SRecPos:23][OrdrPos:23][MatrPos:23][UseMatr:..|..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]TimerAWait();
        shr      bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:31,15][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:42][JmpsPos:33][VarsPos:24][SRecPos:24][OrdrPos:24][MatrPos:24][UseMatr:..x..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]BLedY>>=1;
        cmp      bl,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:27,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:32][JmpsPos:35][VarsPos:25][SRecPos:25][OrdrPos:25][MatrPos:25][UseMatr:..s..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]while (BLedY)
        bne      IrqTimerA_Loop12 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:27,16][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:33][JmpsPos:36][VarsPos:26][SRecPos:26][OrdrPos:26][MatrPos:26][UseMatr:..|..][Esp:0][TargRegMap:  .   .  bl   .   . ][SrcOrig:]while (BLedY)
        bra      IrqTimerA_WhileCmpA0_U1 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:33,4][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:46][JmpsPos:37][VarsPos:27][SRecPos:27][OrdrPos:27][MatrPos:27][UseMatr:.....][Esp:0][TargRegMap:  .   .   .   .   . ][SrcOrig:]}
     IrqTimerA_X10:               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:33,4][ProcNameL:~xh_{}:IrqTimerA#][AtomPos:47][JmpsPos:38][VarsPos:28][SRecPos:28][OrdrPos:28][MatrPos:28][UseMatr:.....][Esp:0][TargRegMap:  .   .   .   .   . ][SrcOrig:]}
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:33,1][Esp:0][SrcOrig:]
        leave    awx|bwx,0,0      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\IrqTimerA.ci][SrcPos:33,1][Esp:0][SrcOrig:]

