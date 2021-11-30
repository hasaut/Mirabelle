;@M SysLib
;@S /home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas

Public DbgSendStr
Public GetClock

Extern WDReset
Extern LedYSet
Extern LedRgbSet
Extern PinDbgSet
Extern Writeln
Extern IntToStr
Extern FloatToStr
Extern DbgMark
Extern DbgSendByte
Extern DbgSendHexT
Extern SysStop

.seg code

;@P [Proc:~xh_{dcsp255e}:DbgSendStr#][ChTargList:~dbi:BIndex# ~dbi:BLen# ~dbb:BData# ~deq:BTmp0# ~dec:BTmp1# ~dei:BTmp4# ~dec:BTmp5# ~deqc:BTmp9# ][VarStackMap:~dcsp255e:AStr# 4 28 0 10100 ~dr_:Result# 0 0 20 10010 ~dbi:BIndex# 4 0 0 00000 ~dbi:BLen# 4 0 0 00000 ~dbb:BData# 1 0 0 00000 ~deq:BTmp0# 4 0 0 00000 ~dec:BTmp1# 1 0 0 00000 ~dei:BTmp4# 4 0 0 00000 ~dec:BTmp5# 1 0 0 00000 ~deqc:BTmp9# 4 0 0 00000 ]
DbgSendStr:
        enter    awx|ar|br|cr|dr|er,0 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:41,6][Esp:-24][SrcOrig:]
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:41,6][Esp:-24][SrcOrig:]
        mov      br,[esp+28]      ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:41,6][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:...d....][Esp:0][TargRegMap:  .   .   .  br   .   .   .   . ][SrcOrig:]
        mov      al,[br+0]        ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:42,15][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:...sd...][Esp:0][TargRegMap:  .   .   .  br  al   .   .   . ][SrcOrig:]BLen:=AStr[0];
        movzx    er,al            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:42,15][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:2][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:.d.|s...][Esp:0][TargRegMap:  .  er   .  br  al   .   .   . ][SrcOrig:]BLen:=AStr[0];
        mov      dr,1             ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:43,15][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:4][JmpsPos:5][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:d|.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]for BIndex:=1 to BLen do
        cmp      dr,er            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:43,23][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:6][JmpsPos:6][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:ss.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]for BIndex:=1 to BLen do
        bg       DbgSendStr_ForEndA0_U2 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:43,23][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:7][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:||.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]for BIndex:=1 to BLen do
     DbgSendStr_Loop3:            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:47,6][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:23][JmpsPos:8][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:||.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]end;
        mov      cr,dr            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:45,22][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:10][JmpsPos:9][VarsPos:8][SRecPos:8][OrdrPos:8][MatrPos:8][UseMatr:s|.|.d..][Esp:0][TargRegMap: dr  er   .  br   .  cr   .   . ][SrcOrig:]BData:=AStr[BIndex];
        add      ar,br,cr         ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:45,22][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:14][JmpsPos:13][VarsPos:9][SRecPos:9][OrdrPos:9][MatrPos:9][UseMatr:||.s.|.d][Esp:0][TargRegMap: dr  er   .  br   .  cr   .  ar ][SrcOrig:]BData:=AStr[BIndex];
        mov      al,[ar+0]        ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:45,22][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:14][JmpsPos:13][VarsPos:11][SRecPos:11][OrdrPos:11][MatrPos:11][UseMatr:||.|..ds][Esp:0][TargRegMap: dr  er   .  br   .   .  al  ar ][SrcOrig:]BData:=AStr[BIndex];
        pushzx   al               ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:46,21][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:17][JmpsPos:16][VarsPos:13][SRecPos:13][OrdrPos:13][MatrPos:13][UseMatr:||s|....][Esp:0][TargRegMap: dr  er  al  br   .   .   .   . ][SrcOrig:]DbgSendByte(BData);
        call     DbgSendByte      ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:46,21][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:18][JmpsPos:17][VarsPos:14][SRecPos:14][OrdrPos:14][MatrPos:14][UseMatr:||.|....][Esp:4][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]DbgSendByte(BData);
        inc      dr               ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:47,6][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:21][JmpsPos:20][VarsPos:15][SRecPos:15][OrdrPos:15][MatrPos:15][UseMatr:x|.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]end;
        cmp      dr,er            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:43,23][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:6][JmpsPos:22][VarsPos:16][SRecPos:16][OrdrPos:16][MatrPos:16][UseMatr:ss.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]for BIndex:=1 to BLen do
        bse      DbgSendStr_Loop3 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:43,23][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:7][JmpsPos:23][VarsPos:17][SRecPos:17][OrdrPos:17][MatrPos:17][UseMatr:||.|....][Esp:0][TargRegMap: dr  er   .  br   .   .   .   . ][SrcOrig:]for BIndex:=1 to BLen do
     DbgSendStr_ForEndA0_U2:      ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:47,6][ProcNameL:~xh_{dcsp255e}:DbgSendStr#][AtomPos:24][JmpsPos:24][VarsPos:18][SRecPos:18][OrdrPos:18][MatrPos:18][UseMatr:........][Esp:0][TargRegMap:  .   .   .   .   .   .   .   . ][SrcOrig:]end;
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:48,4][Esp:0][SrcOrig:]
        leave    awx|ar|br|cr|dr|er,0,4 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:48,4][Esp:0][SrcOrig:]

;@P [Proc:~xhd{}:GetClock#][ChTargList:~drd:Result# ][VarStackMap:~drd:Result# 4 0 4 10010 ]
GetClock:
        enter    awx|ar,0         ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:51,6][Esp:-8][SrcOrig:]
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:51,6][Esp:-8][SrcOrig:]
        mov      ar,zwx           ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:52,11][ProcNameL:~xhd{}:GetClock#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:d][Esp:0][TargRegMap: ar ][SrcOrig:]Result:=0;
        mov      [esp+4],ar       ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:52,11][ProcNameL:~xhd{}:GetClock#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:2][UseMatr:s][Esp:0][TargRegMap: ar ][SrcOrig:]Result:=0; [A:TProcUseMatr.WarnUsage]
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:53,4][Esp:0][SrcOrig:]
        leave    awx|ar,0,0       ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/FwLib/SysLib.pas][SrcPos:53,4][Esp:0][SrcOrig:]

