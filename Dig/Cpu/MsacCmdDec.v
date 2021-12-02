// AAS    |00      ||  cccwrr|        | add sub mov cmp and or test xor
// AAI    |01000   ||     wcc|        | add ___ mov cmp and or test xor
// MAID   |010     ||   ccwaa|        | rd[reg] wr[reg] rd[reg++] wr[reg++]
// MAAC   |011     ||    cw  |#1      | rd[#] wr[#]

// ARUC   |00      ||  ccc4ww|uuuurrrr|#2/4             | add sub and or xor mul udiv sdiv | (r := u cmd Const)
// BM     |00   10 ||  iii  i|iiiirrrr|                 | mov (bytes) (Unused codes of ARUC (because 4=1(32 bit) cannot be used with byte/word constant)
// ARRS   |01   0  ||  ccc ww|ssssrrrr|                 | add sub mov cmp and or test xor | (r := r cmd s) of same size
// BC     |01   10 ||  iii  i|iiiirrrr|                 | cmp (bytes)
// BA     |01   11 ||  iii  i|iiiirrrr|                 | add (bytes)
// MAID   |100     ||   cccww|aaaarrrr|                 | rd[reg] RFU wr[reg] RFU rd[reg++] rd[--reg] wr[reg++] wr[--reg] | ccc = {use_inc_dec wr/rd -/+}
// MARC   |1010    ||    c4ww|aaaarrrr|#2/4             | Mem r/w
// ONL    |10110   ||     4cc|uuuuffff|#2/4             | bra 3xRFU | if (uuuu=IP) then relative JMP
// IDE    |10111   ||     cww|0iiirrrr|                 | inc dec
// PEX    |10111   ||     cww|1000rrrr|                 | pushzx pushsx
// AF     |10111   ||     ccc|1001rrrr|                 | itf trunc round 5xRFU
// REM    |10111   ||     cww|1010rrrr|                 | rem RFU
// PRCS   |10111   ||     4cc|10110000|#2/4             | swt push_# 2xRFU
// Sys    |10111   ||     ccc|10110001|                 | info siend silock siunlock siconf # # #
// NTRE   |10111   ||     ccc|10110010|                 | nop trap # # # # # #
// PPL    |10111   ||     4cc|10110011|#2/4             | popl pushl 2xRFU leave enter 2xRFU (4 is set by natural way)
// RFU    |10111   ||        |        |
// ONS    |110     ||   djjjj|jjjjffff|                 | bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv
// FRUC   |11100   ||     ccc|uuuurrrr|#4               | fadd fsub fmul fdiv 4xRFU | (r = u cmd Const)
// FRRS   |11101   ||     ccc|ssssrrrr|                 | fadd fsub fmul fdiv 4xRFU | (r = r cmd s)
// BTR    |11110   ||     icc|iiiirrrr|                 | bt btr bts btx (test is always 32-bit: if reg=AH[bit_1], then iiiiiii=9)
// BTM    |111110  ||      cc|aaaa4iii|#2/4             | bt btr bts btx
// ARUI   |11111100||        |uuuurrrr|ccccwwww|iiiiiiii| addex subex andex orex xorex mulex udivex sdivex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd i)
// ARUS   |11111101||        |uuuurrrr|ccccwwww|00wwssss| addex subex andex orex xorex mulex udivex sdivex shl shr rol asr fadd fsub fmul fdiv | (r = u cmd s)

module MssdCmdDec
 (
  input  [47:0] AQueTop, input [23:1] AIpThis, input AUseThisCpu,
  // CmdLen
  output  [1:0] ACmdLen,
  // VLIW
  output [3:0] ACond, output ALoadEipImm, // load IP by a special BusP (usually constant directly)
  output [1:0] ATrap,
  output [4:0] ASysReq, // conf unlock lock end swt
  output [5:0] ARegIdxS, output [5:0] ARegIdxU,
  output [31:0] AConst, output [3:0] AMlsc, output [1:0] ALoopD,
  output [7:0] AMuxSrc, output [2:0] ASelIp,
  output [5:0] ARegIdxR, output ADstFlagWr,
  output [3:0] AAluSelA, output [7:0] AAluSelU, output [3:0] AAluSelS, output [3:0] AAluSelT, output [6:0] AAluSelF,
  output [1:0] AMioWrRdEn, output [1:0] AMioSize, output [2:0] AMioSignExt,
  input ACmdLenValid, input [9:0] AStepThis, output [9:0] AStepNext,
  input [15:0] APplListThis, output [15:0] APplListNext,
  output AUnityReq, input AUnityAck,
  input AMemPend
 );

 localparam IStBtMemLock =  0;
 localparam IStBtMemWait =  1;
 localparam IStBtMemRd   =  2;
 localparam IStBtAlu     =  3;
 localparam IStBtMemWr   =  4;

 localparam IStPplRd     =  5;
 localparam IStPplWr     =  6;
 localparam IStLeaveA    =  7;
 localparam IStLeaveB    =  8;
 localparam IStEnterA    =  9;

 localparam CCmdIsLen  = 24;
 localparam CCmdIsNil  = 24'h0;
 localparam IIsARUC    = 23;
 localparam IIsBM      = 22;
 localparam IIsARRS    = 21;
 localparam IIsBC      = 20;
 localparam IIsBA      = 19;
 localparam IIsMAID    = 18;
 localparam IIsMARC    = 17;
 localparam IIsONL     = 16;
 localparam IIsIDE     = 15;
 localparam IIsPEX     = 14;
 localparam IIsAF      = 13;
 localparam IIsREM     = 12;
 localparam IIsPRCS    = 11;
 localparam IIsSys     = 10;
 localparam IIsNTRE    =  9;
 localparam IIsPPL     =  8;
 localparam IIsRFU     =  7;
 localparam IIsONS     =  6;
 localparam IIsFRUC    =  5;
 localparam IIsFRRS    =  4;
 localparam IIsBTR     =  3;
 localparam IIsBTM     =  2;
 localparam IIsARUI    =  1;
 localparam IIsARUS    =  0;

 localparam CAluACmdOr  = 4'hC;
 localparam CAluACmdSub = 4'h2;
 localparam CAluACmdAdd = 4'h1;

 localparam CRegSp = 6'h28;
 localparam CRegIp = 6'h20;

 wire [3:0] LRegAU = AQueTop[ 7: 4];
 wire [3:0] LRegRR = AQueTop[ 3: 0];
 wire [3:0] LRegSS = AQueTop[19:16];

 wire [1:0] LWw  = AQueTop[ 9: 8];  wire [3:0] LWwDec; MsDec2x4a UWwDec ( .ADataI(LWw), .ADataO(LWwDec) );
 wire [1:0] LWwU = AQueTop[27:26];
 wire [1:0] LWwR = AQueTop[25:24];
 wire [1:0] LWwS = AQueTop[21:20];
 wire LConstLenX = AQueTop[10];
 wire LConstLenY = AQueTop[ 3];

 wire [2:0] LCmd3C = AQueTop[10: 8]; wire [ 7:0] LCmd3CDec; MsDec3x8a  UCmd3CDec ( .ADataI(LCmd3C), .ADataO(LCmd3CDec) );
 wire [2:0] LCmd3B = AQueTop[12:10]; wire [ 7:0] LCmd3BDec; MsDec3x8a  UCmd3BDec ( .ADataI(LCmd3B), .ADataO(LCmd3BDec) );
 wire [2:0] LCmd3A = AQueTop[13:11]; wire [ 7:0] LCmd3ADec; MsDec3x8a  UCmd3ADec ( .ADataI(LCmd3A), .ADataO(LCmd3ADec) );

 wire [1:0] LCmd2A = AQueTop[ 9: 8]; wire [ 3:0] LCmd2ADec; MsDec2x4a  UCmd2ADec ( .ADataI(LCmd2A), .ADataO(LCmd2ADec) );

 wire [3:0] LCmd4A = AQueTop[31:28]; wire [15:0] LCmd4ADec; MsDec4x16a UCmd4ADec ( .ADataI(LCmd4A), .ADataO(LCmd4ADec) );

 wire [CCmdIsLen-1:0] MCmdIs, BCmdIs; assign BCmdIs = AUseThisCpu ? MCmdIs : CCmdIsNil;

 assign MCmdIs[IIsARUC] = (AQueTop[15:14]==2'b00) & (AQueTop[10: 9]!=2'b10);
 assign MCmdIs[IIsBM  ] = (AQueTop[15:14]==2'b00) & (AQueTop[10: 9]==2'b10);
 assign MCmdIs[IIsARRS] = (AQueTop[15:14]==2'b01) & (AQueTop[10]==1'b0);
 assign MCmdIs[IIsBC  ] = (AQueTop[15:14]==2'b01) & (AQueTop[10: 9]==2'b10);
 assign MCmdIs[IIsBA  ] = (AQueTop[15:14]==2'b01) & (AQueTop[10: 9]==2'b11);
 assign MCmdIs[IIsMAID] = (AQueTop[15:13]==3'b100);
 assign MCmdIs[IIsMARC] = (AQueTop[15:12]==4'b1010);
 assign MCmdIs[IIsONL ] = (AQueTop[15:11]==5'b10110);
 assign MCmdIs[IIsIDE ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7]==1'b0);
 assign MCmdIs[IIsPEX ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:4]==4'b1000);
 assign MCmdIs[IIsAF  ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:4]==4'b1001);
 assign MCmdIs[IIsREM ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:4]==4'b1010);
 assign MCmdIs[IIsPRCS] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:0]==8'b10110000);
 assign MCmdIs[IIsSys ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:0]==8'b10110001);
 assign MCmdIs[IIsNTRE] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:0]==8'b10110010);
 assign MCmdIs[IIsPPL ] = (AQueTop[15:11]==5'b10111) & (AQueTop[7:0]==8'b10110011);
 assign MCmdIs[IIsRFU ] = 1'b0;
 assign MCmdIs[IIsONS ] = (AQueTop[15:13]==3'b110);
 assign MCmdIs[IIsFRUC] = (AQueTop[15:11]==5'b11100);
 assign MCmdIs[IIsFRRS] = (AQueTop[15:11]==5'b11101);
 assign MCmdIs[IIsBTR ] = (AQueTop[15:11]==5'b11110);
 assign MCmdIs[IIsBTM ] = (AQueTop[15:10]==6'b111110);
 assign MCmdIs[IIsARUI] = (AQueTop[15: 8]==8'b11111100);
 assign MCmdIs[IIsARUS] = (AQueTop[15: 8]==8'b11111101);

 assign ACmdLen =
   ((|(BCmdIs & 24'h7CF6D8)) ? 2'h1 : 2'h0) |
   ((|(BCmdIs & 24'h830900)) ? {1'b1, LConstLenX} : 2'h0) |
   ((|(BCmdIs & 24'h000004)) ? {1'b1, LConstLenY} : 2'h0) |
   ((|(BCmdIs & 24'h000003)) ? 2'h2 : 2'h0) |
   ((|(BCmdIs & 24'h000020)) ? 2'h3 : 2'h0);

 // Intermediate
 /*wire BCcJCS_IsCall = (LCmdJCS==2'h1);

 wire BCmdIsRetX   = |{BCmdIs[IIsPRCS] & LCmd1a, BCmdIs[IIsNTRE] & LCmd3ADec[2]};
 wire BCmdIsSwtJmp = BCmdIs[IIsJCS] & ~BCcJCS_IsCall;
 wire BCmdIsCallC  = BCmdIs[IIsJCS] &  BCcJCS_IsCall & ~BUuuuNZ; // Using register with CALL commands is not possible
 wire BCmdIsPushC  = BCmdIs[IIsPRCS] & ~LCmd1a;
 wire BCmdIsRetZ   = BCmdIs[IIsNTR] & LCmd3ADec[2];
 wire BCmdIsStream = BCmdIs[IIsMem] &  LCmdMem[1];
 wire BCmdIsMioGen = BCmdIs[IIsMem] & ~LCmdMem[1]; */

 wire BCmdIsStream = BCmdIs[IIsMAID] & LCmd3B[2];
 wire BCmdIsPushC  = BCmdIs[IIsPRCS] & LCmd2ADec[1];

 wire [3:0] BMioIncDec =
   ((LWw==2'h0) ? {{3{LCmd3B[0]}}, 1'h1} : 4'h0) |
   ((LWw==2'h1) ? {{2{LCmd3B[0]}}, 2'h2} : 4'h0) |
   ((LWw==2'h2) ? {{1{LCmd3B[0]}}, 3'h4} : 4'h0);

 //wire [7:0] BUuuuRowB; MsCodeToRow UUuuuRow ( .ADataI(LRegAU), .ADataO(BUuuuRowB) );
 //wire [7:0] BRrrrRowB; MsCodeToRow URrrrRow ( .ADataI(LRegRR), .ADataO(BRrrrRowB) );
 //wire [7:0] BSsssRowB; MsCodeToRow USsssRow ( .ADataI(LRegSS), .ADataO(BSsssRowB) );


 // A = acc (general), B = bytes, D = 32-bit
 //wire [11:0] BRrrrColRowA = {WwToCol(LRegRR[3], LWw), BRrrrRowB};
 //wire [11:0] BRrrrColRowD = {WwToCol(LRegRR[3],2'h2), BRrrrRowB};
 //wire [11:0] BRrrrColRowB = {WwToCol(LRegRR[3],2'h0), BRrrrRowB};
 //wire [11:0] BUuuuColRowA = {WwToCol(LRegAU[3], LWw), BUuuuRowB};
 //wire [11:0] BUuuuColRowD = {WwToCol(LRegAU[3],2'h2), BUuuuRowB};
 //wire [11:0] BRrrrColRowE = {WwToCol(LRegRR[3],LWwR), BRrrrRowB};
 //wire [11:0] BUuuuColRowE = {WwToCol(LRegAU[3],LWwU), BUuuuRowB};
 //wire [11:0] BSsssColRowE = {WwToCol(LRegSS[3],LWwS), BSsssRowB};

 // *** PPL ***
 wire [15:0] BPplIdx;
 wire [3:0] BPplRegIdxA; MsEnc16x4a UPplRegIdx ( .ADataI(BPplIdx), .ADataO(BPplRegIdxA) );
 wire [3:0] BPplRegIdx = {BPplRegIdxA[0], BPplRegIdxA[3:1]};
 wire [1:0] BPplOp, BPplWrRdEn;
 wire BPplListMoreThanE;

 // *** Step ***
 wire [13:0] BStepThis = AUseThisCpu ? AStepThis : 14'h0;
 wire BStepThisNZ = |BStepThis;
 wire [1:0] BBTxWrRdEn;

 // *** Other ***
 wire BCmd4AFast = |{LCmd4ADec[4:0], LCmd4ADec[11:8]};

 // *** VLIW ***
 assign ACond       = (|{BCmdIs[IIsONS], BCmdIs[IIsONL]}) ? AQueTop[3:0] : 4'h0;

 assign ALoadEipImm = (|{BCmdIs[IIsONS], BCmdIs[IIsONL]});

 assign ATrap       = {
                       BCmdIs[IIsNTRE] & LCmd3CDec[2],
                       BCmdIs[IIsNTRE] & (LCmd3CDec[1] | LCmd3CDec[2])
                      };

 assign ASysReq     = {
                       BCmdIs[IIsSys] ? LCmd3CDec[4:1] : 4'h0,
                       BCmdIs[IIsPRCS] & LCmd2ADec[0]   // SWT (and not INFO)
                      };

 assign ARegIdxS    = (BCmdIs[IIsARUC] ? {LWw, 4'h0} : 6'h0) |
                      (BCmdIs[IIsFRUC] ? {2'h2, 4'h0} : 6'h0) |
                      (BCmdIs[IIsARRS] ? {LWw, LRegAU} : 6'h0) |
                      ((|{BCmdIs[IIsMAID], BCmdIs[IIsMARC], BCmdIs[IIsPEX]}) ? {LWw, LRegRR} : 6'h0) |
                      (BCmdIs[IIsFRRS] ? {2'h2, LRegAU} : 6'h0) |
                      (BCmdIs[IIsARUI] ? {2'h0, 4'h0} : 6'h0) |
                      (BCmdIs[IIsARUS] ? {LWwS, LRegSS} : 6'h0) |
                      ((|BPplWrRdEn) ? {2'h2, BPplRegIdx} : 6'h0);
                      //((|BBTxWrRdEn) ? 12'h100 : 12'h0);

 assign ARegIdxU    = (BCmdIs[IIsARUC] ? {LWw, LRegAU} : 6'h0) |
                      ((|{BCmdIs[IIsBC], BCmdIs[IIsBA]}) ? {2'h0, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsMAID], BCmdIs[IIsMARC], BCmdIs[IIsFRUC], |BBTxWrRdEn}) ? {2'h2, LRegAU} : 6'h0) |
                      ((|{BCmdIs[IIsIDE], BCmdIs[IIsARRS] & ~LCmd3ADec[2]}) ? {LWw, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsAF], BCmdIs[IIsFRRS], BCmdIs[IIsBTR]}) ? {2'h2, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsPEX], BPplWrRdEn, AStepNext[IStLeaveA]}) ? CRegSp : 6'h0) |
                      ((|{BCmdIs[IIsONL]}) ? {2'h2, LRegAU} : 6'h0) |
                      ((|{BCmdIs[IIsONS]}) ? CRegIp : 6'h0) |
                      ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? {LWwU, LRegAU} : 6'h0);

 assign AConst      = ((|{BCmdIs[IIsARUC], BCmdIs[IIsPRCS]}) ? {LConstLenX ? AQueTop[47:32] : {16{AQueTop[31]}}, AQueTop[31:16]} : 32'h0) |
                      ((|{BCmdIs[IIsMARC]}) ? {LConstLenX ? AQueTop[47:32] : 16'h0, AQueTop[31:16]} : 32'h0) |
                      ((|{BCmdIs[IIsBM], BCmdIs[IIsBC], BCmdIs[IIsBA]}) ? {24'h0, AQueTop[13:11], AQueTop[8:4]} : 32'h0) |
                      (BCmdIs[IIsIDE] ? {28'h0, {1'b0, AQueTop[6:4]} + 4'h1} : 32'h0) |
                      (BCmdIs[IIsFRUC] ? AQueTop[47:16] : 32'h0) |
                      (BCmdIs[IIsONL] ? {LConstLenX ? AQueTop[46:32] : 15'h0, AQueTop[31:16], 1'b0} : 32'h0) |
                      (BCmdIs[IIsONS] ? {8'h0, {15{AQueTop[12]}}, AQueTop[11:4], 1'b0} : 32'h0) |
                      ((|{BCmdIs[IIsBTM]}) ? {LConstLenY ? AQueTop[47:32] : 16'h0, AQueTop[31:16]} : 32'h0) |
                      ((|{BCmdIs[IIsARUI]}) ? {26'h0, AQueTop[21:16]} : 32'h0) |
                      (AStepNext[IStBtAlu] ? {29'h0, AQueTop[ 2: 0]} : 32'h0) |
                      ((|BBTxWrRdEn) ? {LConstLenY ? AQueTop[47:32] : 16'h0, AQueTop[31:16]} : 32'h0) |
                      (AStepNext[IStLeaveA] ? {22'h0, AQueTop[39:32], 2'h0} : 32'h0) |
                      (BPplWrRdEn[1] ? ((~BPplListMoreThanE & LConstLenX) ? {22'h0, AQueTop[39:32], 2'h0}+32'h4 : 32'h4) : 32'h0) |
                      (BPplWrRdEn[0] ? ((~BPplListMoreThanE & LConstLenX) ? {22'h0, AQueTop[47:40], 2'h0}+32'h4 : 32'h4) : 32'h0);

 assign AMlsc       = (BCmdIsStream ? BMioIncDec : 4'h0) |
                      ((|{BCmdIs[IIsPEX], BCmdIsPushC, BPplWrRdEn[1]}) ? 4'hC : 4'h0) |
                      ((|{BPplWrRdEn[0]}) ? 4'h4 : 4'h0);

 assign ALoopD      = {
                       BBTxWrRdEn[1],
                       AStepNext[IStBtAlu]
                      };

 // assign BAluXMuxS = (LMuxSrc[0] ? BRegMuxS[31:0] : 32'h0) |
 //                    (LMuxSrc[1] ? LConst : 32'h0) |
 //                    (LMuxSrc[2] ? BMlsc : 32'h0);
 // // [3] is used in URegMuxS
 // assign BAluXMuxU = (LMuxSrc[4] ? BRegMuxU[31:0] : 32'h0) |
 //                    (LLoopD[0] ? {24'h0, BBusBData[7:0]} : 32'h0);
 // wire [63:0] BMioWrData = LMuxSrc[5] ? {32'h0, LConst} : (BRegMuxS | {56'h0, LLoopD[1] ? BBusAData[7:0] : 8'h0});
 // wire [31:0] BMioAddrS = ({32{LMuxSrc[7]}} & BMlsc) | ({32{LMuxSrc[6]}} & LConst);
 // assign      BMioAddr = BMioAddrS + BRegMuxM;

 assign AMuxSrc     = {
 /* MioA = Mlsc    */  |{BCmdIsStream & LCmd3B[0], BCmdIsPushC, BCmdIs[IIsPEX], BPplWrRdEn[1]},
 /* MioA = Const   */  |{BCmdIs[IIsMARC], BBTxWrRdEn, BCmdIs[IIsONS], BCmdIs[IIsONL]},
 /* MioD = C : R   */  |{BCmdIsPushC},
 /* MuxU = Reg     */  |{BCmdIs[IIsARUC], BCmdIs[IIsARRS], BCmdIs[IIsFRRS], BCmdIs[IIsARUS], BCmdIs[IIsARUI], BCmdIs[IIsAF], BCmdIsStream, BCmdIs[IIsBC], BCmdIs[IIsBA], BCmdIs[IIsIDE], BCmdIs[IIsFRUC], BCmdIs[IIsPEX], BCmdIsPushC, BPplWrRdEn, AStepNext[IStBtAlu], AStepNext[IStLeaveA]},
 /* MuxS = IpCache */  |{AStepNext[IStEnterA], AStepNext[IStPplWr]},
 /* MuxS = Mlsc    */  |{BCmdIsStream, BCmdIsPushC, BCmdIs[IIsPEX]},
 /* MuxS = Const   */  |{BCmdIs[IIsARUC], BCmdIs[IIsBM], BCmdIs[IIsBC], BCmdIs[IIsBA], BCmdIs[IIsIDE], BCmdIs[IIsFRUC], BCmdIs[IIsARUI], AStepNext[IStBtAlu], BPplWrRdEn, AStepNext[IStLeaveA]},
 /* MuxS = Reg     */  |{BCmdIs[IIsARRS], BCmdIs[IIsFRRS], BCmdIs[IIsARUS]}
                      };

 assign ASelIp      = {
                       |{BPplOp},
                       |{BCmdIs[IIsONL], BCmdIs[IIsONS]},
                       1'b0
                      };

 assign AAluSelA    = (BCmdIs[IIsARUC] ? {LCmd3ADec[4] | LCmd3ADec[3], LCmd3ADec[2] | LCmd3ADec[3], LCmd3ADec[1], LCmd3ADec[0]} : 4'h0) |
                      (BCmdIs[IIsBM] ? CAluACmdOr : 4'h0) |
                      (BCmdIs[IIsARRS] ? {LCmd3ADec[2] | LCmd3ADec[5] | LCmd3ADec[7], LCmd3ADec[2] | LCmd3ADec[4] | LCmd3ADec[5] | LCmd3ADec[6], LCmd3ADec[3] | LCmd3ADec[1], LCmd3ADec[0]} : 4'h0) |
                      (BCmdIs[IIsBC] ? CAluACmdSub : 4'h0) |
                      (BCmdIs[IIsBA] ? CAluACmdAdd : 4'h0) |
                      (BCmdIsStream ? CAluACmdAdd : 4'h0) |
                      (BCmdIs[IIsIDE] ? (AQueTop[10] ? CAluACmdSub : CAluACmdAdd) : 4'h0) |
                      ((|{BCmdIs[IIsPEX], BCmdIsPushC, BPplWrRdEn}) ? CAluACmdAdd : 4'h0) |
                      ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? {LCmd4ADec[4] | LCmd4ADec[3], LCmd4ADec[2] | LCmd4ADec[3], LCmd4ADec[1], LCmd4ADec[0]} : 4'h0) |
                      (BPplWrRdEn[1] ? CAluACmdSub : 4'h0) |
                      ((AStepNext[IStLeaveA] | BPplWrRdEn[0]) ? CAluACmdAdd : 4'h0);

                      // urem udiv srem sdiv mulhu mulhsu mulh mul 6xRFU rem 0
                      wire [7:0] LCmd3A_AsAluU =
                        {
                         1'b0,
                         LCmd3ADec[6],
                         1'b0,
                         LCmd3ADec[7],
                         1'b0,
                         1'b0,
                         1'b0,
                         LCmd3ADec[5]
                        };
                      /*wire [3:0] LCmd3A_AsAluU =
                        (LCmd3ADec[7] ? 4'hC : 4'h0) |
                        (LCmd3ADec[6] ? 4'hE : 4'h0) |
                        (LCmd3ADec[5] ? 4'h8 : 4'h0);*/
                      wire [7:0] LCmd4A_AsAluU =
                        {
                         1'b0,
                         LCmd4ADec[6],
                         1'b0,
                         LCmd4ADec[7],
                         1'b0,
                         1'b0,
                         1'b0,
                         LCmd4ADec[5]
                        };
                      /*wire [3:0] LCmd4A_AsAluU =
                        (LCmd4ADec[7] ? 4'hC : 4'h0) |
                        (LCmd4ADec[6] ? 4'hE : 4'h0) |
                        (LCmd4ADec[5] ? 4'h8 : 4'h0);*/

 assign AAluSelU    = (BCmdIs[IIsARUC] ? LCmd3A_AsAluU : 8'h0) |
                      ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? LCmd4A_AsAluU : 8'h0);

 assign AAluSelS    = ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? LCmd4ADec[11:8] : 4'h0);

 assign AAluSelT    = ((|{BCmdIs[IIsBTR], AStepNext[IStBtAlu]}) ? LCmd2ADec : 3'h0);

                      // round trunc itf div mul sub add
 assign AAluSelF    = (BCmdIs[IIsAF] ? {LCmd3CDec[2:0], 4'h0} : 7'h0) |
                      ((|{BCmdIs[IIsFRUC], BCmdIs[IIsFRRS]}) ? {3'h0, LCmd3CDec[ 3:0]} : 7'h0) |
                      ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? {3'h0, LCmd4ADec[15:12]} : 7'h0);

 assign ARegIdxR    = ((|{BCmdIs[IIsARUC], BCmdIs[IIsARRS] & ~LCmd3ADec[3] & ~LCmd3ADec[6], BCmdIs[IIsIDE], BCmdIs[IIsREM]}) ? {LWw, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsARRS] & (LCmd3ADec[3] | LCmd3ADec[6])}) ? {LWw, 4'h0} : 6'h0) |
                      ((|{BCmdIs[IIsBM], BCmdIs[IIsBA]}) ? {2'h0, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsBC]}) ? {2'h0, 2'h0} : 6'h0) |
                      ((|{BCmdIs[IIsAF], BCmdIs[IIsREM], BCmdIs[IIsBTR], BCmdIs[IIsFRUC], BCmdIs[IIsFRRS]}) ? {2'h2, LRegRR} : 6'h0) |
                      ((|{BCmdIs[IIsARUI], BCmdIs[IIsARUS]}) ? {LWwR, LRegRR} : 6'h0) |
                      (BCmdIsStream ? {2'h2, LRegAU} : 6'h0) |
                      ((|{BCmdIs[IIsPEX], BPplWrRdEn, AStepNext[IStLeaveA]}) ? CRegSp : 6'h0);
                      //(AStepNext[IStBtAlu] ? 12'h100 : 12'h0);

 assign ADstFlagWr  = (BCmdIs[IIsARUC] & (|LCmd3ADec[4:0])) |
                      BCmdIs[IIsBA] | BCmdIs[IIsBC] |
                      BCmdIs[IIsARRS] |
                      ((BCmdIs[IIsARUS] | BCmdIs[IIsARUI]) & BCmd4AFast) |
                      BCmdIs[IIsIDE] |
                      BCmdIs[IIsBTR] |
                      AStepNext[IStBtAlu];

 assign AMioWrRdEn  = ((|{BCmdIs[IIsMAID], BCmdIs[IIsMARC]}) ? (AQueTop[11] ? 2'b10 : 2'b01) : 2'h0) |
                      ((|{BCmdIsPushC, BCmdIs[IIsPEX]}) ? 2'b10 : 2'h0) |
                      BPplWrRdEn |
                      BBTxWrRdEn;

 assign AMioSize    = ((|{BCmdIs[IIsMAID], BCmdIs[IIsMARC]}) ? LWw : 2'h0) |
                      ((|{BCmdIsPushC, BCmdIs[IIsPEX]}) ? 2'h2 : 2'h0) |
                      (BPplWrRdEn ? 2'h2 : 2'h0) |
                      (BBTxWrRdEn ? 2'h0 : 2'h0);

 assign AMioSignExt = {
                       |{BCmdIs[IIsPEX] & AQueTop[10]},
                       ((|{BCmdIs[IIsMAID], BCmdIs[IIsMARC], BCmdIs[IIsPEX]}) ? LWw : 2'h0) |
                       ((|{BCmdIsPushC, BPplWrRdEn}) ? 2'h2 : 2'h0)
                      };

 // Multi-step
 wire BCmdBTxBtWr = |AQueTop[9:8]; // Shows that data needs to be written back (i.e. btr bts btx)

 assign BBTxWrRdEn =
  {
   BStepThis[IStBtAlu] & BCmdBTxBtWr,
   (BStepThis[IStBtMemLock] &  AUnityAck & ~AMemPend) |
   (BStepThis[IStBtMemWait] &              ~AMemPend)
  };

 // PPL group
 wire BStartPpl = BCmdIs[IIsPPL] & ~BStepThisNZ & ACmdLenValid;
 wire [15:0] BPplList = BStartPpl ? AQueTop[31:16] : APplListThis; wire BPplListNZ = |BPplList;
 wire [15:0] BPplListOr; MsOrVectL #(.CVectLen(16)) UPplListOr ( .ADataI(BPplList), .ADataO(BPplListOr) );
 assign BPplListMoreThanE = |(BPplList[15:1] & BPplListOr[14:0]);
 wire BPplKeep = BPplListNZ | AMemPend;

 wire BGoPplRd  = BStartPpl & ~LConstLenX & LCmd2ADec[0];
 wire BGoPplWr  = BStartPpl & ~LConstLenX & LCmd2ADec[1];
 wire BGoLeaveA = BStartPpl &  LConstLenX & LCmd2ADec[0];
 wire BGoEnterA = BStartPpl &  LConstLenX & LCmd2ADec[1];

 wire [15:0] BPplIdxWr; MsPriorityMaskL #(.CVectLen(16)) UPplIdxWr ( .ADataI(BPplList), .ADataO(BPplIdxWr) );
 wire [15:0] BPplIdxRd; MsPriorityMaskH #(.CVectLen(16)) UPplIdxRd ( .ADataI(BPplList), .ADataO(BPplIdxRd) );
 assign BPplOp = {2{BPplListNZ}} &
   {
    |{BGoPplWr, BStepThis[IStPplWr], BGoEnterA,            BStepThis[IStEnterA]},
    |{BGoPplRd, BStepThis[IStPplRd], AStepNext[IStLeaveB], BStepThis[IStLeaveB]}
   };
 assign      BPplIdx = (BPplOp[1] ? BPplIdxWr : 16'h0) |
                       (BPplOp[0] ? BPplIdxRd : 16'h0);
 wire [15:0] BPplClr = AMemPend ? 16'h0 : BPplIdx;
 assign BPplWrRdEn =
  {
   BPplOp[1] & ~AMemPend,
   BPplOp[0] & ~AMemPend
  };

 assign AStepNext[IStBtMemLock] = (BCmdIs[IIsBTM] & ~BStepThisNZ & ACmdLenValid) |
                                  (BStepThis[IStBtMemLock] & ~AUnityAck);
 assign AStepNext[IStBtMemWait] = (BStepThis[IStBtMemLock] &  AUnityAck & AMemPend) |
                                  (BStepThis[IStBtMemWait] &              AMemPend);
 assign AStepNext[IStBtMemRd]   =  BBTxWrRdEn[0] |
                                  (BStepThis[IStBtMemRd] &  AMemPend);
 assign AStepNext[IStBtAlu]     = (BStepThis[IStBtMemRd] & ~AMemPend);
 assign AStepNext[IStBtMemWr]   =  BBTxWrRdEn[1] |
                                  (BStepThis[IStBtMemWr] &  AMemPend);

 assign AStepNext[IStPplWr]     = (BGoPplWr | BStepThis[IStPplWr]) & BPplKeep;
 assign AStepNext[IStPplRd]     = (BGoPplRd | BStepThis[IStPplRd]) & BPplKeep;
 assign AStepNext[IStEnterA]    = (BGoEnterA | BStepThis[IStEnterA]) &  BPplKeep;
 assign AStepNext[IStLeaveA]    =  BGoLeaveA;
 assign AStepNext[IStLeaveB]    = (BStepThis[IStLeaveA] | BStepThis[IStLeaveB]) &  BPplKeep;

 assign AUnityReq = |BStepThis[IStBtMemWr:IStBtMemLock];
 assign APplListNext = BPplList & ~BPplClr;
endmodule

module MsCodeToRow ( input [3:0] ADataI, output [7:0] ADataO ); // Excludes Z register
 wire [7:0] BRowA; MsDec3x8a URowA ( .ADataI(ADataI[2:0]), .ADataO(BRowA) );
 assign ADataO = {BRowA[7:1], BRowA[0] & ADataI[3]};
endmodule
