;@M ApplMain
;@S C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas

Public ApplMainProc

Extern ProbaA
Extern LedRgbSet
Extern LedYSet
Extern WDReset
Extern SysStop

.seg data
        Align 1
   FExecEnd:             db 0

.seg code

;@P [Proc:~xh_{}:ApplMainProc#][ChTargList:~dbb:BLedY# ~dbd:BData# ~del:BTmp2# ~del:BTmp5# ][VarStackMap:~dr_:Result# 0 0 8 10010 ~dbb:BLedY# 1 0 0 00000 ~dbd:BData# 4 0 0 00001 ~del:BTmp2# 1 0 0 00000 ~del:BTmp5# 1 0 0 00000 ]
ApplMainProc:
        enter    awx|bwx,4        ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:31,6][Esp:-12][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:31,6][Esp:-12][SrcOrig:]
        push     0x02             ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:32,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:....][Esp:0][TargRegMap:  .   .   .   . ][SrcOrig:]LedRgbSet($02);
        call     LedRgbSet        ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:32,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:....][Esp:4][TargRegMap:  .   .   .   . ][SrcOrig:]LedRgbSet($02);
        push     zwx              ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:34,18][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:3][JmpsPos:4][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:....][Esp:0][TargRegMap:  .   .   .   . ][SrcOrig:]BData:=ProbaB(0);
        call     ProbaB           ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:34,18][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:4][JmpsPos:5][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:.d..][Esp:4][TargRegMap:  . awx   .   . ][SrcOrig:]BData:=ProbaB(0);
        mov      bl,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:36,10][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:6][JmpsPos:7][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:6][UseMatr:d...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]BLedY:=0;
        mov      al,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:37,17][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:7][JmpsPos:8][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:7][UseMatr:|.d.][Esp:0][TargRegMap: bl   .  al   . ][SrcOrig:]FExecEnd:=FALSE;
        mov      [FExecEnd],al    ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:37,17][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:8][JmpsPos:9][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:8][UseMatr:|.s.][Esp:0][TargRegMap: bl   .  al   . ][SrcOrig:]FExecEnd:=FALSE;
     ApplMainProc_RepeatStartA0_U0: ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:38,8][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:9][JmpsPos:10][VarsPos:8][SRecPos:8][OrdrPos:8][MatrPos:9][UseMatr:|...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]repeat
        pushzx   bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:39,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:10][JmpsPos:11][VarsPos:9][SRecPos:9][OrdrPos:9][MatrPos:10][UseMatr:s...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]LedYSet(BLedY);
        call     LedYSet          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:39,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:11][JmpsPos:12][VarsPos:10][SRecPos:10][OrdrPos:10][MatrPos:11][UseMatr:|...][Esp:4][TargRegMap: bl   .   .   . ][SrcOrig:]LedYSet(BLedY);
        inc      bl               ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:42,12][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:13][JmpsPos:14][VarsPos:11][SRecPos:11][OrdrPos:11][MatrPos:12][UseMatr:x...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]inc(BLedY);
        call     WDReset          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:43,11][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:15][JmpsPos:16][VarsPos:12][SRecPos:12][OrdrPos:12][MatrPos:13][UseMatr:|...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]WDReset();
        mov      al,[FExecEnd]    ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:45,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:17][JmpsPos:18][VarsPos:13][SRecPos:13][OrdrPos:13][MatrPos:14][UseMatr:|..d][Esp:0][TargRegMap: bl   .   .  al ][SrcOrig:]until FExecEnd;
        cmp      al,zl            ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:45,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:18][JmpsPos:19][VarsPos:14][SRecPos:14][OrdrPos:14][MatrPos:15][UseMatr:|..s][Esp:0][TargRegMap: bl   .   .  al ][SrcOrig:]until FExecEnd;
        be       ApplMainProc_RepeatStartA0_U0 ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:45,16][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:19][JmpsPos:20][VarsPos:15][SRecPos:15][OrdrPos:15][MatrPos:16][UseMatr:|...][Esp:0][TargRegMap: bl   .   .   . ][SrcOrig:]until FExecEnd;
        call     SysStop          ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:47,9][ProcNameL:~xh_{}:ApplMainProc#][AtomPos:22][JmpsPos:21][VarsPos:16][SRecPos:16][OrdrPos:16][MatrPos:17][UseMatr:....][Esp:0][TargRegMap:  .   .   .   . ][SrcOrig:]SysStop;
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:48,4][Esp:0][SrcOrig:]
        leave    awx|bwx,4,0      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:48,4][Esp:0][SrcOrig:]

;@P [Proc:~xfd{dad}:ProbaB#][ChTargList:~dad:AData# ~drd:Result# ][VarStackMap:~dad:AData# 4 12 12 10000 ~drd:Result# 4 0 4 10010 ]
ProbaB:
        enter    awx|ar,0         ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:18,6][Esp:-8][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:18,6][Esp:-8][SrcOrig:]
        mov      ar,[esp+12]      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:19,15][ProcNameL:~xfd{dad}:ProbaB#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:d.][Esp:0][TargRegMap: ar   . ][SrcOrig:]Result:=AData; [A:TProcUseMatr.WarnUsage]
        mov      [esp+4],ar       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:19,15][ProcNameL:~xfd{dad}:ProbaB#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:3][UseMatr:.s][Esp:0][TargRegMap:  .  ar ][SrcOrig:]Result:=AData; [A:TProcUseMatr.WarnUsage]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:20,4][Esp:0][SrcOrig:]
        leave    awx|ar,0,4       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:20,4][Esp:0][SrcOrig:]

;@P [Proc:~xfd{dad}:ProbaC#][ChTargList:~dad:AData# ~drd:Result# ][VarStackMap:~dad:AData# 4 12 12 10000 ~drd:Result# 4 0 4 10010 ]
ProbaC:
        enter    awx|ar,0         ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:23,6][Esp:-8][SrcOrig:]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:23,6][Esp:-8][SrcOrig:]
        mov      ar,[esp+12]      ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:24,15][ProcNameL:~xfd{dad}:ProbaC#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:d.][Esp:0][TargRegMap: ar   . ][SrcOrig:]Result:=AData; [A:TProcUseMatr.WarnUsage]
        mov      [esp+4],ar       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:24,15][ProcNameL:~xfd{dad}:ProbaC#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:3][UseMatr:.s][Esp:0][TargRegMap:  .  ar ][SrcOrig:]Result:=AData; [A:TProcUseMatr.WarnUsage]
                                  ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:25,4][Esp:0][SrcOrig:]
        leave    awx|ar,0,4       ;[SrcFile:C:\ZukH\Projects\Mirabelle\Examples\MsaBasic\FW\Src\ApplMain.pas][SrcPos:25,4][Esp:0][SrcOrig:]

