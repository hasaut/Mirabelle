;@M MdMainB
;@S /home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci

Public MainB

Extern WDReset

.seg data
        Align 1
   FActive:              db 0

.seg code

;@P [Proc:~xh_{}:MainB#][ChTargList:~deb:BTmp0# ~deb:BTmp2# ][VarStackMap:~deb:BTmp0# 1 0 0 00000 ~deb:BTmp2# 1 0 0 00000 ]
MainB:
        enter    awx,0            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:5,1][Esp:-4][SrcOrig:]
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:5,1][Esp:-4][SrcOrig:]
        mov      al,1             ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:7,11][ProcNameL:~xh_{}:MainB#][AtomPos:0][JmpsPos:1][VarsPos:1][SRecPos:1][OrdrPos:1][MatrPos:1][UseMatr:d.][Esp:0][TargRegMap: al   . ][SrcOrig:]FActive=1;
        mov      [FActive],al     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:7,11][ProcNameL:~xh_{}:MainB#][AtomPos:1][JmpsPos:2][VarsPos:2][SRecPos:2][OrdrPos:2][MatrPos:2][UseMatr:s.][Esp:0][TargRegMap: al   . ][SrcOrig:]FActive=1;
     MainB_RepeatStartA0_U1:      ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:8,4][ProcNameL:~xh_{}:MainB#][AtomPos:2][JmpsPos:3][VarsPos:3][SRecPos:3][OrdrPos:3][MatrPos:3][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]do
        call     WDReset          ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:10,13][ProcNameL:~xh_{}:MainB#][AtomPos:3][JmpsPos:4][VarsPos:4][SRecPos:4][OrdrPos:4][MatrPos:4][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]WDReset();
        mov      al,[FActive]     ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:11,18][ProcNameL:~xh_{}:MainB#][AtomPos:5][JmpsPos:6][VarsPos:5][SRecPos:5][OrdrPos:5][MatrPos:5][UseMatr:.d][Esp:0][TargRegMap:  .  al ][SrcOrig:]} while(FActive);
        cmp      al,zl            ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:11,18][ProcNameL:~xh_{}:MainB#][AtomPos:6][JmpsPos:7][VarsPos:6][SRecPos:6][OrdrPos:6][MatrPos:6][UseMatr:.s][Esp:0][TargRegMap:  .  al ][SrcOrig:]} while(FActive);
        bne      MainB_RepeatStartA0_U1 ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:11,18][ProcNameL:~xh_{}:MainB#][AtomPos:7][JmpsPos:8][VarsPos:7][SRecPos:7][OrdrPos:7][MatrPos:7][UseMatr:..][Esp:0][TargRegMap:  .   . ][SrcOrig:]} while(FActive);
                                  ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:11,1][Esp:0][SrcOrig:]
        leave    awx,0,0          ;[SrcFile:/home/ozh/smb/ZukH/Projects/Mirabelle/Examples/MsaBasic/FW/Src/MdMainB.ci][SrcPos:11,1][Esp:0][SrcOrig:]

