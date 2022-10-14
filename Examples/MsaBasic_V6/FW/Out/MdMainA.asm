;@M MdMainA
;@S C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas

Public MainA

Extern WDReset
Extern LedRgbSet
Extern LedYSet
Extern SysStop

.seg data
        Align 1
   FActive:              db 0

.seg code

;@P [Proc:~xh_{}:MainA#][ChTargList:~del:BTmp0# ~del:BTmp2# ][VarStackMap:~dr_:Result# 0 0 0 10010 ~del:BTmp0# 1 0 0 00000 ~del:BTmp2# 1 0 0 00000 ]
MainA:
        enter    awx,0            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:16,6][Esp:-4][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:16,6][Esp:-4][SrcOrig:]
        mov      al,1             ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:17,15][ProcNameL:~xh_{}:MainA#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:d.][Esp:0][TargRegMap: al   . ][SrcOrig:]FActive:=TRUE;
        mov      [FActive],al     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:17,15][ProcNameL:~xh_{}:MainA#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:s.][Esp:0][TargRegMap: al   . ][SrcOrig:]FActive:=TRUE;
     MainA_RepeatStartA0_U0:      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:18,8][ProcNameL:~xh_{}:MainA#][AtomPos:2][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]repeat
        call     WDReset          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:19,9][ProcNameL:~xh_{}:MainA#][AtomPos:3][JmpsPos:4][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]WDReset;
        mov      al,[FActive]     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:20,21][ProcNameL:~xh_{}:MainA#][AtomPos:5][JmpsPos:6][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:.d][Esp:0][TargRegMap:  .  al ][SrcOrig:]until FActive=FALSE;
        cmp      al,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:20,21][ProcNameL:~xh_{}:MainA#][AtomPos:6][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:.s][Esp:0][TargRegMap:  .  al ][SrcOrig:]until FActive=FALSE;
        bne      MainA_RepeatStartA0_U0 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:20,21][ProcNameL:~xh_{}:MainA#][AtomPos:7][JmpsPos:8][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]until FActive=FALSE;
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:21,4][Esp:0][SrcOrig:]
        leave    awx,0,0          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:21,4][Esp:0][SrcOrig:]

;@P [Proc:~xf_{}:MainA_old#][ChTargList:~dbb:BLedY# ~del:BTmp1# ~del:BTmp4# ][VarStackMap:~dr_:Result# 0 0 4 10010 ~dbb:BLedY# 1 0 0 00000 ~del:BTmp1# 1 0 0 00000 ~del:BTmp4# 1 0 0 00000 ]
MainA_old:
        enter    awx|bwx,0        ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:26,6][Esp:-8][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:26,6][Esp:-8][SrcOrig:]
        push     0x02             ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:27,16][ProcNameL:~xf_{}:MainA_old#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:...][Esp:0][TargRegMap:  .   .   . ][SrcOrig:]LedRgbSet($02);
        call     LedRgbSet        ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:27,16][ProcNameL:~xf_{}:MainA_old#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:...][Esp:4][TargRegMap:  .   .   . ][SrcOrig:]LedRgbSet($02);
        mov      bl,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:29,10][ProcNameL:~xf_{}:MainA_old#][AtomPos:3][JmpsPos:4][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:d..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]BLedY:=0;
        mov      al,1             ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:30,15][ProcNameL:~xf_{}:MainA_old#][AtomPos:4][JmpsPos:5][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:|d.][Esp:0][TargRegMap: bl  al   . ][SrcOrig:]FActive:=TRUE;
        mov      [FActive],al     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:30,15][ProcNameL:~xf_{}:MainA_old#][AtomPos:5][JmpsPos:6][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:|s.][Esp:0][TargRegMap: bl  al   . ][SrcOrig:]FActive:=TRUE;
     MainA_old_RepeatStartA0_U2:  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:31,8][ProcNameL:~xf_{}:MainA_old#][AtomPos:6][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]repeat
        pushzx   bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:32,16][ProcNameL:~xf_{}:MainA_old#][AtomPos:7][JmpsPos:8][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:s..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:32,16][ProcNameL:~xf_{}:MainA_old#][AtomPos:8][JmpsPos:9][VarsPos:8][SRecPos:8][OrdrPos:8][MatrPos:8][UseMatr:|..][Esp:4][TargRegMap: bl   .   . ][SrcOrig:]LedYSet(BLedY);
        inc      bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:35,12][ProcNameL:~xf_{}:MainA_old#][AtomPos:10][JmpsPos:11][VarsPos:9][SRecPos:9][OrdrPos:9][MatrPos:9][UseMatr:x..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]inc(BLedY);
        call     WDReset          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:36,11][ProcNameL:~xf_{}:MainA_old#][AtomPos:12][JmpsPos:13][VarsPos:10][SRecPos:10][OrdrPos:10][MatrPos:10][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]WDReset();
        mov      al,[FActive]     ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:38,21][ProcNameL:~xf_{}:MainA_old#][AtomPos:14][JmpsPos:15][VarsPos:11][SRecPos:11][OrdrPos:11][MatrPos:11][UseMatr:|.d][Esp:0][TargRegMap: bl   .  al ][SrcOrig:]until FActive=FALSE;
        cmp      al,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:38,21][ProcNameL:~xf_{}:MainA_old#][AtomPos:15][JmpsPos:16][VarsPos:12][SRecPos:12][OrdrPos:12][MatrPos:12][UseMatr:|.s][Esp:0][TargRegMap: bl   .  al ][SrcOrig:]until FActive=FALSE;
        bne      MainA_old_RepeatStartA0_U2 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:38,21][ProcNameL:~xf_{}:MainA_old#][AtomPos:16][JmpsPos:17][VarsPos:13][SRecPos:13][OrdrPos:13][MatrPos:13][UseMatr:|..][Esp:0][TargRegMap: bl   .   . ][SrcOrig:]until FActive=FALSE;
        call     SysStop          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:40,9][ProcNameL:~xf_{}:MainA_old#][AtomPos:19][JmpsPos:18][VarsPos:14][SRecPos:14][OrdrPos:14][MatrPos:14][UseMatr:...][Esp:0][TargRegMap:  .   .   . ][SrcOrig:]SysStop;
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:41,4][Esp:0][SrcOrig:]
        leave    awx|bwx,0,0      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic_V51\FW\Src\MdMainA.pas][SrcPos:41,4][Esp:0][SrcOrig:]

