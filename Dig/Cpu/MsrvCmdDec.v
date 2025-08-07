module MsrvCmdDec
 (
  input wire [31:0] AQueTop, input wire [23:1] AIpThis, input wire AUseThisCpu,
  // CmdLen
  output wire [1:0] ACmdLen,
  // VLIW
  output wire [3:0] ACond, output wire ALoadEipImm,
  output wire [1:0] ATrap,
  output wire [4:0] ASysReq, // conf unlock lock end swt
  output wire [5:0] ARegIdxS, ARegIdxU,
  output wire [1:0] AWwConst, output wire [31:0] AConst, output wire [4:0] AMlsc, output wire [2:0] ALoopD,
  output wire [7:0] AMuxSrc, output wire [2:1] ASelIp,
  output wire [5:0] ARegIdxR, output wire ADstFlagWr,
  output wire AAluSignExt, output wire [3:0] AAluSelA, output wire [7:0] AAluSelU, output wire [3:0] AAluSelS, output wire [3:0] AAluSelT, output wire [12:0] AAluSelF,
  output wire [1:0] AMioWrRdEn, output wire [1:0] AMioSize, output wire [2:0] AMioSignExt,
  input wire ACmdLenValid, input wire [9:0] AStepThis, output wire [9:0] AStepNext,
  output wire AUnityReq, input wire AUnityAck,
  input wire AMemPend
 );

 localparam IStBx     = 0;

 wire [ 3:0] LOpDecA; MsDec2x4a UOpDecA ( .ADataI(AQueTop[1:0]), .ADataO(LOpDecA) );
 wire LGrIA = LOpDecA[3] & ~AQueTop[2];
 wire LGrIB = LOpDecA[3] &  AQueTop[2];
 wire [15:0] LOpDecB; MsDec4x16a UOpDecB ( .ADataI(AQueTop[6:3]), .ADataO(LOpDecB) );
 wire [ 7:0] LOpDecC; MsDec3x8a  UOpDecC ( .ADataI(AQueTop[15:13]), .ADataO(LOpDecC) );
 wire [2:0] LFn3x = AQueTop[14:12];
 wire [6:0] LFn7x = AQueTop[31:25];
 wire [7:0] LFn3Dec; MsDec3x8a UFn3Dec ( .ADataI(LFn3x), .ADataO(LFn3Dec) );
 wire [1:0] BFn3SizeMio = LFn3x[1:0];

 wire LFn7_00x = (LFn7x==7'h00);
 wire LFn7_20x = (LFn7x==7'h20);
 wire LFn7_01x = (LFn7x==7'h01);

 wire [3:0] LRegR = AQueTop[10: 7]; wire BRegRNZ = |LRegR; wire BIsRegRSp = (LRegR==4'h2);
 wire [3:0] LRegU = AQueTop[18:15]; wire BRegUNZ = |LRegU;
 wire [3:0] LRegS = AQueTop[23:20];

 wire [4:0] LReg62C = AQueTop[6:2]; wire BReg62C_NZ = |LReg62C;
 wire [2:0] LReg42C = AQueTop[4:2]; //wire [7:0] BRow42C; MsDec3x8a URow42C ( .ADataI(LReg42C), .ADataO(BRow42C) );
 wire [2:0] LReg97C = AQueTop[9:7]; //wire [7:0] BRow97C; MsDec3x8a URow97C ( .ADataI(LReg97C), .ADataO(BRow97C) );

 localparam CRegSP   = 6'h22;
 localparam CRegRA   = 6'h21;
 localparam CRegPC   = 6'h20;
 localparam CRegZero = 6'h20;

 localparam CAluASelAdd = 4'h1;
 localparam CAluASelSub = 4'h2;
 localparam CAluASelAnd = 4'h4;
 localparam CAluASelXor = 4'h8;
 localparam CAluASelOr  = 4'hC;

 //wire [11:0] BGenColRowR = {LRegR[3], {3{~LRegR[3]}}, BRowR[7:1], BRowR[0] & LRegR[3]}; // Always 32bit
 //wire [11:0] BGenColRowU = {LRegU[3], {3{~LRegU[3]}}, BRowU[7:1], BRowU[0] & LRegU[3]};
 //wire [11:0] BGenColRowS = {LRegS[3], {3{~LRegS[3]}}, BRowS[7:1], BRowS[0] & LRegS[3]};

 //wire [11:0] BMioColRowR = {LRegR[3], {3{~LRegR[3]}}, BRowR[7:1], BRowR[0] & LRegR[3]}; // Fn3 dependent
 //wire [11:0] BMioColRowS = {LRegS[3], {3{~LRegS[3]}}, BRowS[7:1], BRowS[0] & LRegS[3]}; // Fn3 dependent

 //wire [11:0] BColRow42C = {4'h8, BRow42C[7:0]}; // Compressed
 //wire [11:0] BColRow97C = {4'h8, BRow97C[7:0]}; // Compressed
 //wire [11:0] BColRow62C = {LReg62C[3], {3{~LReg62C[3]}}, BRow42C[7:1], BRow42C[0] & LReg62C[3]}; // Compressed

 wire [5:0] BRegIdx42C = {2'h2, 1'b1, LReg42C};
 wire [5:0] BRegIdx97C = {2'h2, 1'b1, LReg97C};
 wire [5:0] BRegIdx62C = {2'h2, LReg62C[3:0]};

 wire [31:0] LImmI = {{20{AQueTop[31]}}, AQueTop[31:20]};

 // imm[11: 5]    rs2 rs1 funct3 imm[ 4:0]    opcode S-type
 //    [31:25]                      [11:7]
 wire [31:0] LImmS = {{20{AQueTop[31]}}, AQueTop[31:25], AQueTop[11:7]};

 // imm[12|10: 5] rs2 rs1 funct3 imm[ 4:1|11] opcode B-type
 //    [31|30:25]                   [11:8| 7]
 wire [31:0] LImmB = {{19{AQueTop[31]}}, AQueTop[31], AQueTop[7], AQueTop[30:25], AQueTop[11:8], 1'b0};

 wire [31:0] LImmU = {AQueTop[31:12], 12'h0};

 // imm[20|10: 1|11|19:12]       rd          opcode J-type
 //    [31|30:21|20|19:12]
 wire [31:0] LImmJ = {{11{AQueTop[31]}}, AQueTop[31], AQueTop[19:12], AQueTop[20], AQueTop[30:21], 1'b0};
 wire [31:0] LImmCB = {{24{AQueTop[12]}}, AQueTop[6:5], AQueTop[2], AQueTop[11:10], AQueTop[4:3], 1'b0};

 wire [31:0] LImmCA = {{27{AQueTop[12]}}, AQueTop[6:2]}; wire LImmCA_NZ = |LImmCA;

 //                0   1  2   3  4   5  6    7  8   9  A   B  C  D   E   F
 // CBraListA = 'bra bbe bc bnc bz bnz ba bany bg bge bs bse bn bv bnn bnv';
 // CBraListB = 'bra bbe bb bae be bne ba bany bg bge bs bse bn bv bnn bnv';
 wire [3:0] LCondB =
  (LFn3Dec[7] ? 4'h3 : 4'h0) | // jae
  (LFn3Dec[6] ? 4'h2 : 4'h0) | // jb
  (LFn3Dec[5] ? 4'h9 : 4'h0) | // jge
  (LFn3Dec[4] ? 4'hA : 4'h0) | // js
  (LFn3Dec[1] ? 4'h5 : 4'h0) | // jne
  (LFn3Dec[0] ? 4'h4 : 4'h0);  // je

 wire [3:0] BCmdIIAluADec = {LFn3Dec[4] | LFn3Dec[6], LFn3Dec[7] | LFn3Dec[6], 1'b0, LFn3Dec[0]};
 wire [3:0] BCmdIRAluADec = {LFn7_00x & (LFn3Dec[4] | LFn3Dec[6]), LFn7_00x & (LFn3Dec[7] | LFn3Dec[6]), LFn7_20x & LFn3Dec[0], LFn7_00x & LFn3Dec[0]};
 wire [3:0] BCmdIIAluBDec = (LFn3Dec[3] ? 4'b0110 : 4'h0) | (LFn3Dec[2] ? 4'b1110 : 4'h0);
 wire [3:0] BCmdIRAluBDec = {4{LFn7_00x}} & BCmdIIAluBDec;
                            // asr rol shr shl
 wire [3:0] BCmdIXAluSDec = {LFn3Dec[5] & LFn7_20x, 1'b0, LFn3Dec[5] & LFn7_00x, LFn3Dec[1] & LFn7_00x};
 //wire [2:0] BCmdIRAluMDec = {3{LFn7_01x}} & LFn3x;
 wire [7:0] BCmdIRAluMDec = {8{LFn7_01x}} & LFn3Dec;

 wire [12:0] BCmdIRAluFDec =
  {
   {3{LFn7x==7'h50}} & LFn3Dec[2:0],
   {3{LFn7x==7'h10}} & LFn3Dec[2:0],
   (LFn7x==7'h60) & (AQueTop[24:20]==5'h0) & LFn3Dec[0], (LFn7x==7'h60) & (AQueTop[24:20]==5'h1) & LFn3Dec[1], (LFn7x==7'h68) & (AQueTop[24:20]==5'h0), // round trunc itf
   (LFn7x==7'h0C), (LFn7x==7'h08), (LFn7x==7'h04), (LFn7x==7'h00)
  };

 wire BCmdIIAluADecNZ = |BCmdIIAluADec;
 wire BCmdIIAluBDecNZ = |BCmdIIAluBDec;
 //wire BCmdIRAluADecNZ = |BCmdIRAluADec;
 wire BCmdIXAluSDecNZ = |BCmdIXAluSDec;

 localparam CCmdIIsLen = 15;
 localparam CCmdIIsNil = 15'h0;
 localparam IIsLui     = 14;
 localparam IIsAuipc   = 13;
 localparam IIsJal     = 12;
 localparam IIsJalr    = 11;
 localparam IIsBx      = 10;
 localparam IIsLx      =  9;
 localparam IIsSx      =  8;
 localparam IIsAI      =  7;
 localparam IIsAR      =  6;
 localparam IIsMR      =  5;
 localparam IIsFR      =  4;
 localparam IIsAmo     =  3;
 localparam IIsFence   =  2;
 localparam IIsECall   =  1;
 localparam IIsEBreak  =  0;

 localparam CCmdCIsLen   = 26;
 localparam CCmdCIsNil   = 26'h0;
 localparam IIsCAdd4Spn  = 25;
 localparam IIsCLW       = 24;
 localparam IIsCSW       = 23;
 localparam IIsCAddi     = 22;
 localparam IIsCJal      = 21;
 localparam IIsCLI       = 20;
 localparam IIsCAddi16Sp = 19;
 localparam IIsCLui      = 18;
 localparam IIsCSrli     = 17;
 localparam IIsCSrai     = 16;
 localparam IIsCAndi     = 15;
 localparam IIsCSub      = 14;
 localparam IIsCXor      = 13;
 localparam IIsCOr       = 12;
 localparam IIsCAnd      = 11;
 localparam IIsCJ        = 10;
 localparam IIsCBeqz     =  9;
 localparam IIsCBnez     =  8;
 localparam IIsCSlli     =  7;
 localparam IIsCLwsp     =  6;
 localparam IIsCJR       =  5;
 localparam IIsCMV       =  4;
 localparam IIsCEBreak   =  3;
 localparam IIsCJalr     =  2;
 localparam IIsCAdd      =  1;
 localparam IIsCSwsp     =  0;

 wire [CCmdIIsLen-1:0] MCmdIIs, BCmdIIs; assign BCmdIIs = AUseThisCpu ? MCmdIIs : CCmdIIsNil;
 wire [CCmdCIsLen-1:0] MCmdCIs, BCmdCIs; assign BCmdCIs = AUseThisCpu ? MCmdCIs : CCmdCIsNil;

 assign MCmdIIs[IIsLui]    = LGrIB & LOpDecB[ 6];
 assign MCmdIIs[IIsAuipc]  = LGrIB & LOpDecB[ 2];
 assign MCmdIIs[IIsJal]    = LGrIB & LOpDecB[13];
 assign MCmdIIs[IIsJalr]   = LGrIB & LOpDecB[12];
 assign MCmdIIs[IIsBx]     = LGrIA & LOpDecB[12] & (|LCondB);
 assign MCmdIIs[IIsLx]     = LGrIA & LOpDecB[ 0] & (|{LFn3Dec[5], LFn3Dec[4], LFn3Dec[2], LFn3Dec[1], LFn3Dec[0]});
 assign MCmdIIs[IIsSx]     = LGrIA & LOpDecB[ 4] & (|{                        LFn3Dec[2], LFn3Dec[1], LFn3Dec[0]});
 assign MCmdIIs[IIsAI]     = LGrIA & LOpDecB[ 2] & (|{BCmdIIAluADec, BCmdIIAluBDec, BCmdIXAluSDec});
 assign MCmdIIs[IIsAR]     = LGrIA & LOpDecB[ 6] & (|{BCmdIRAluADec, BCmdIRAluBDec, BCmdIXAluSDec});
 assign MCmdIIs[IIsMR]     = LGrIA & LOpDecB[ 6] & (| LFn7_01x);
 assign MCmdIIs[IIsFR]     = LGrIA & LOpDecB[10] & (| BCmdIRAluFDec);
 assign MCmdIIs[IIsAmo]    = 1'b0;
 assign MCmdIIs[IIsFence]  = LGrIB & LOpDecB[1] & ~BRegRNZ & ~BRegUNZ & ((LFn3Dec[0] & (AQueTop[31:27]==5'h0)) | (LFn3Dec[1] & (AQueTop[31:20]==12'h0)));
 assign MCmdIIs[IIsECall]  = LGrIA & LOpDecB[14] & (AQueTop[31:20]==12'h000);
 assign MCmdIIs[IIsEBreak] = LGrIA & LOpDecB[14] & (AQueTop[31:20]==12'h001);

 assign MCmdCIs[IIsCAdd4Spn]  = LOpDecA[0] & LOpDecC[0];
 assign MCmdCIs[IIsCLW]       = LOpDecA[0] & LOpDecC[2];
 assign MCmdCIs[IIsCSW]       = LOpDecA[0] & LOpDecC[6];
 assign MCmdCIs[IIsCAddi]     = LOpDecA[1] & LOpDecC[0] & LImmCA_NZ & BRegRNZ;
 assign MCmdCIs[IIsCJal]      = LOpDecA[1] & LOpDecC[1];
 assign MCmdCIs[IIsCLI]       = LOpDecA[1] & LOpDecC[2] & BRegRNZ;
 assign MCmdCIs[IIsCAddi16Sp] = LOpDecA[1] & LOpDecC[3] &  BIsRegRSp;
 assign MCmdCIs[IIsCLui]      = LOpDecA[1] & LOpDecC[3] & ~BIsRegRSp & BRegRNZ;
 assign MCmdCIs[IIsCSrli]     = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h0);
 assign MCmdCIs[IIsCSrai]     = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h1);
 assign MCmdCIs[IIsCAndi]     = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h2);
 assign MCmdCIs[IIsCSub]      = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h3) & ~AQueTop[12] & (AQueTop[6:5]==2'h0);
 assign MCmdCIs[IIsCXor]      = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h3) & ~AQueTop[12] & (AQueTop[6:5]==2'h1);
 assign MCmdCIs[IIsCOr]       = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h3) & ~AQueTop[12] & (AQueTop[6:5]==2'h2);
 assign MCmdCIs[IIsCAnd]      = LOpDecA[1] & LOpDecC[4] & (AQueTop[11:10]==2'h3) & ~AQueTop[12] & (AQueTop[6:5]==2'h3);
 assign MCmdCIs[IIsCJ]        = LOpDecA[1] & LOpDecC[5];
 assign MCmdCIs[IIsCBeqz]     = LOpDecA[1] & LOpDecC[6];
 assign MCmdCIs[IIsCBnez]     = LOpDecA[1] & LOpDecC[7];
 assign MCmdCIs[IIsCSlli]     = LOpDecA[2] & LOpDecC[0] & BRegRNZ;
 assign MCmdCIs[IIsCLwsp]     = LOpDecA[2] & LOpDecC[2] & BRegRNZ;
 assign MCmdCIs[IIsCJR]       = LOpDecA[2] & LOpDecC[4] & ~AQueTop[12] &  BRegRNZ & ~BReg62C_NZ;
 assign MCmdCIs[IIsCMV]       = LOpDecA[2] & LOpDecC[4] & ~AQueTop[12] &  BRegRNZ &  BReg62C_NZ;
 assign MCmdCIs[IIsCEBreak]   = LOpDecA[2] & LOpDecC[4] &  AQueTop[12] & ~BRegRNZ & ~BReg62C_NZ;
 assign MCmdCIs[IIsCJalr]     = LOpDecA[2] & LOpDecC[4] &  AQueTop[12] &  BRegRNZ & ~BReg62C_NZ;
 assign MCmdCIs[IIsCAdd]      = LOpDecA[2] & LOpDecC[4] &  AQueTop[12] &  BRegRNZ &  BReg62C_NZ;
 assign MCmdCIs[IIsCSwsp]     = LOpDecA[2] & LOpDecC[6];

 //wire BCmdIIsNZ = |BCmdIIs;
 //wire BCmdCIsNZ = |BCmdCIs;

 assign ACmdLen = AUseThisCpu ? (LOpDecA[3] ? 2'h2 : 2'h1) : 2'h0;

 // *** Intermediate ***

 wire BIsJalr = BCmdIIs[IIsJalr] & LFn3Dec[0];
 wire BIsSwt  = BCmdIIs[IIsJalr] & LFn3Dec[1];
 wire BIsFR_A = BCmdIIs[IIsFR] & (|{BCmdIRAluFDec[12:7], BCmdIRAluFDec[3:0]});

 // *** Step ***
 wire [9:0] BStepThis = AUseThisCpu ? AStepThis : 10'h0;
 wire BStepThisNZ = |BStepThis;

 // *** VLIW ***
 assign ACond          = ((BCmdIIs[IIsBx] & BStepThis[IStBx]) ? LCondB : 4'h0) |
                         ((BCmdCIs[IIsCBeqz] & BStepThis[IStBx]) ? 4'h4 : 4'h0) |
                         ((BCmdCIs[IIsCBnez] & BStepThis[IStBx]) ? 4'h5 : 4'h0) |
                         ((|{BCmdIIs[IIsJal], BCmdIIs[IIsJalr], BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr], BCmdCIs[IIsCJ], BCmdCIs[IIsCJR]}) ? 4'h0 : 4'h0);

 assign ALoadEipImm    = |{BCmdIIs[IIsJal], BIsJalr, BIsSwt, BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr], BCmdCIs[IIsCJ], BCmdCIs[IIsCJR], BStepThis[IStBx]};

 /*assign AMuxEip        = |{BCmdIIs[IIsJal], BCmdCIs[IIsCJal], BCmdIIs[IIsJalr], BCmdCIs[IIsCJalr]};

 assign ALoadEip       = {
                          |{BCmdIIs[IIsBx], BCmdIIs[IIsJal], BCmdCIs[IIsCJal], BCmdCIs[IIsCJ], BCmdCIs[IIsCJR], BCmdCIs[IIsCBeqz], BCmdCIs[IIsCBnez]}, // FConst
                          |{BCmdIIs[IIsJalr], BCmdCIs[IIsCJalr], BCmdCIs[IIsCJR]} // AluA
                         };*/

 assign ATrap          = {
                          1'b0,
                          |{BCmdIIs[IIsEBreak], BCmdIIs[IIsECall], BCmdCIs[IIsCEBreak]}
                         };

 assign ASysReq        = {
                          4'h0,
                          BIsSwt   // SWT (and not INFO)
                         };

 assign ARegIdxS       = ((|{BCmdIIs[IIsBx] & AStepNext[IStBx], BCmdIIs[IIsAR], BCmdIIs[IIsMR], BIsFR_A}) ? {2'h2, LRegS} : 6'h0) |
                         ((|{BCmdIIs[IIsSx]}) ? {2'h2, LRegS} : 6'h0) |
                         ((|{BCmdIIs[IIsLx]}) ? {2'h2, LRegR} : 6'h0) | // For Load, the register is defined by S and not by R
                         ((|{BIsJalr}) ? {2'h2, LRegU} : 6'h0) |
                         ((|{BCmdCIs[IIsCJalr], BCmdCIs[IIsCJR]}) ? {2'h2, LRegR} : 6'h0) |
                         ((|{BCmdCIs[IIsCLW], BCmdCIs[IIsCSW]}) ? BRegIdx42C : 6'h0) |
                         ((|{BCmdCIs[IIsCLwsp]}) ? {2'h2, LRegR} : 6'h0) |
                         ((|{BCmdCIs[IIsCBeqz] & AStepNext[IStBx], BCmdCIs[IIsCBnez] & AStepNext[IStBx]}) ? CRegZero : 6'h0) |
                         ((|{BCmdCIs[IIsCSub], BCmdCIs[IIsCAnd], BCmdCIs[IIsCOr], BCmdCIs[IIsCXor]}) ? BRegIdx42C : 6'h0) |
                         ((|{BCmdCIs[IIsCAdd], BCmdCIs[IIsCSwsp]}) ? BRegIdx62C : 6'h0);


 assign ARegIdxU       = ((|{BCmdIIs[IIsBx] & AStepNext[IStBx], BCmdIIs[IIsLx], BCmdIIs[IIsSx], BCmdIIs[IIsAI], BCmdIIs[IIsAR], BCmdIIs[IIsMR], BCmdIIs[IIsFR]}) ? {2'h2, LRegU} : 6'h0) |
                         ((|{BCmdIIs[IIsJal], BIsJalr, BIsSwt, BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr]}) ? CRegPC : 6'h0) |
                         ((|{BCmdCIs[IIsCAdd4Spn], BCmdCIs[IIsCAddi16Sp], BCmdCIs[IIsCLwsp], BCmdCIs[IIsCSwsp]}) ? CRegSP : 6'h0) |
                         ((|{BCmdCIs[IIsCLW], BCmdCIs[IIsCSW], BCmdCIs[IIsCBeqz] & AStepNext[IStBx], BCmdCIs[IIsCBnez] & AStepNext[IStBx]}) ? BRegIdx97C : 6'h0) |
                         ((|{BCmdCIs[IIsCAddi], BCmdCIs[IIsCAdd], BCmdCIs[IIsCSlli]}) ? {2'h2, LRegR} : 6'h0) |
                         ((|{BCmdCIs[IIsCAndi], BCmdCIs[IIsCSrli], BCmdCIs[IIsCSrai], BCmdCIs[IIsCSub], BCmdCIs[IIsCAnd], BCmdCIs[IIsCOr], BCmdCIs[IIsCXor]}) ? BRegIdx97C : 6'h0) |
                         ((|{BCmdCIs[IIsCMV]}) ? BRegIdx62C : 6'h0);

                         wire [31:0] BOptiIpDelta =
                           (BCmdIIs[IIsAuipc] ? LImmU : 32'h0) |
                           ((BCmdIIs[IIsBx] & BStepThis[IStBx]) ? LImmB : 32'h0) |
                           (BCmdIIs[IIsJal] ? LImmJ : 32'h0) |
                           (BIsSwt ? {LImmI[30:0], 1'b0} : 32'h0) |
                           // 12 11   9  8 7 6    3 2
                           // 11  4 9:8 10 6 7  3:1 5
                           ((|{BCmdCIs[IIsCJal], BCmdCIs[IIsCJ]}) ? {{20{AQueTop[12]}}, AQueTop[12], AQueTop[8], AQueTop[10:9], AQueTop[6], AQueTop[7], AQueTop[2], AQueTop[11], AQueTop[5:3], 1'b0} : 32'h0) |
                           ((|{BCmdCIs[IIsCBeqz] & BStepThis[IStBx], BCmdCIs[IIsCBnez] & BStepThis[IStBx]}) ? LImmCB : 32'h0);
                         wire [31:0] BOptiIpNew = {8'h0, AIpThis, 1'b0} + BOptiIpDelta;

 assign AWwConst       = ((|{BCmdIIs[IIsAuipc], BCmdIIs[IIsLui], BCmdCIs[IIsCLui], BCmdCIs[IIsCLI], BCmdIIs[IIsAI], BCmdCIs[IIsCAdd4Spn], BCmdCIs[IIsCAddi16Sp], BCmdCIs[IIsCAddi], BCmdCIs[IIsCAndi]}) ? 2'h2 : 2'h0);

 assign AConst         = ((|{BCmdIIs[IIsLui]}) ? LImmU : 32'h0) |
                         ((|{BIsJalr}) ? LImmI : 32'h0) |
                         ( BCmdIIs[IIsLx] ? LImmI : 32'h0) |
                         ( BCmdIIs[IIsSx] ? LImmS : 32'h0) |
                         ((BCmdIIs[IIsAI] & (BCmdIIAluADecNZ | BCmdIIAluBDecNZ)) ? LImmI : 32'h0) |
                         ((BCmdIIs[IIsAI] & BCmdIXAluSDecNZ) ? {27'h0, LImmI[4:0]} : 32'h0) |
                         (BCmdCIs[IIsCAdd4Spn] ? {22'h0, AQueTop[10:7], AQueTop[12:11], AQueTop[5], AQueTop[6], 2'h0} : 32'h0) |
                         (BCmdCIs[IIsCAddi16Sp] ? {{23{AQueTop[12]}}, AQueTop[4:3], AQueTop[5], AQueTop[2], AQueTop[6], 4'h0} : 32'h0) |
                         ((|{BCmdCIs[IIsCLW], BCmdCIs[IIsCSW]}) ? {25'h0, AQueTop[5], AQueTop[12:10], AQueTop[6], 2'h0} : 32'h0) |
                         ((|{BCmdCIs[IIsCLwsp]}) ? {24'h0, AQueTop[3:2], AQueTop[12], AQueTop[6:4], 2'h0} : 32'h0) |
                         ((|{BCmdCIs[IIsCSwsp]}) ? {24'h0, AQueTop[8:7], AQueTop[12:9], 2'h0} : 32'h0) |
                         ((|{BCmdCIs[IIsCAddi], BCmdCIs[IIsCLI], BCmdCIs[IIsCSrli], BCmdCIs[IIsCSrai], BCmdCIs[IIsCAndi], BCmdCIs[IIsCSlli]}) ? LImmCA : 32'h0) |
                         ((|{BCmdCIs[IIsCLui]}) ? {LImmCA[19:0], 12'h0} : 32'h0) |
                         ((|{BCmdIIs[IIsAuipc], BCmdIIs[IIsBx] & BStepThis[IStBx], BCmdIIs[IIsJal], BIsSwt, BCmdCIs[IIsCJal], BCmdCIs[IIsCJ], BCmdCIs[IIsCBeqz] & BStepThis[IStBx], BCmdCIs[IIsCBnez] & BStepThis[IStBx]}) ? BOptiIpNew : 32'h0);

 assign AMlsc          = 5'h0;

 assign ALoopD         = 3'h0;

 assign AMuxSrc        = {
 /* MioA = Mlsc    */     1'b0,
 /* MioA = Const   */     |{BCmdIIs[IIsLx], BCmdIIs[IIsSx], BCmdCIs[IIsCLW], BCmdCIs[IIsCSW], BCmdCIs[IIsCLwsp], BCmdCIs[IIsCSwsp], BStepThis[IStBx]}, // 6
 /* MioD = C : R   */     1'b0,
 /* MuxU = Reg     */     |{BCmdIIs[IIsAI], BCmdIIs[IIsAR], BCmdIIs[IIsMR], BCmdIIs[IIsFR], BCmdCIs[IIsCJ], AStepNext[IStBx], BCmdCIs[IIsCAdd], BCmdCIs[IIsCSub], BCmdCIs[IIsCAnd], BCmdCIs[IIsCOr], BCmdCIs[IIsCXor], BCmdCIs[IIsCAddi16Sp], BCmdCIs[IIsCAdd4Spn], BCmdCIs[IIsCAddi], BCmdCIs[IIsCAndi], BCmdCIs[IIsCSlli], BCmdCIs[IIsCSrli], BCmdCIs[IIsCSrai], BCmdCIs[IIsCMV], BCmdIIs[IIsJal], BIsJalr, BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr]},
 /* MuxS = IpCache */     1'b0,
 /* MuxS = Mlsc    */     1'b0,
 /* MuxS = Const   */     |{BCmdIIs[IIsAuipc], BCmdIIs[IIsLui], BCmdCIs[IIsCLui], BCmdIIs[IIsAI], BCmdCIs[IIsCAdd4Spn], BCmdCIs[IIsCAddi16Sp], BCmdCIs[IIsCAddi], BCmdCIs[IIsCLI], BCmdCIs[IIsCLui], BCmdCIs[IIsCMV], BCmdCIs[IIsCAndi], BCmdCIs[IIsCSrai], BCmdCIs[IIsCSrli], BCmdCIs[IIsCSlli]},
 /* MuxS = Reg     */     |{BCmdIIs[IIsAR], BCmdIIs[IIsMR], BIsFR_A, AStepNext[IStBx], BCmdCIs[IIsCAdd], BCmdCIs[IIsCSub], BCmdCIs[IIsCAnd], BCmdCIs[IIsCOr], BCmdCIs[IIsCXor]}
                         };

 assign ASelIp         = { // Select IP instead of Z
 /* MuxS = Eip    */      1'b0,
 /* MuxU = Eip    */      |{BCmdIIs[IIsJal], BCmdCIs[IIsCJ], BIsJalr, BIsSwt, BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr]}
                         };

 assign AAluSignExt    = 1'b0;

 assign AAluSelA       = ((|{BCmdIIs[IIsAuipc], BCmdIIs[IIsLui], BCmdCIs[IIsCLI], BCmdCIs[IIsCLui], BCmdCIs[IIsCMV]}) ? CAluASelOr : 4'h0) |
                         ((|{AStepNext[IStBx]}) ? CAluASelSub : 4'h0) |
                         (BCmdIIs[IIsAI] ? BCmdIIAluADec | BCmdIIAluBDec : 4'h0) |
                         (BCmdIIs[IIsAR] ? BCmdIRAluADec | BCmdIRAluBDec : 4'h0) |
                         ((|{BCmdCIs[IIsCAdd4Spn], BCmdCIs[IIsCAddi16Sp], BCmdCIs[IIsCAddi], BCmdCIs[IIsCAdd]}) ? CAluASelAdd : 4'h0) |
                         ((|{BCmdCIs[IIsCSub]}) ? CAluASelSub : 4'h0) |
                         ((|{BCmdCIs[IIsCAndi], BCmdCIs[IIsCAnd]}) ? CAluASelAnd : 4'h0) |
                         ((|{BCmdCIs[IIsCOr], BCmdIIs[IIsJal], BIsJalr, BIsSwt, BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr]}) ? CAluASelOr : 4'h0) |
                         ((|{BCmdCIs[IIsCXor]}) ? CAluASelXor : 4'h0);

 assign AAluSelU       = (BCmdIIs[IIsMR] ? BCmdIRAluMDec : 8'h0);

 assign AAluSelS       = (BCmdIIs[IIsAI] ? BCmdIXAluSDec : 4'h0) |
                         (BCmdIIs[IIsAR] ? BCmdIXAluSDec : 4'h0) |
                         {BCmdCIs[IIsCSrai], 1'b0, BCmdCIs[IIsCSrli], BCmdCIs[IIsCSlli]};

 assign AAluSelT       = 4'h0;

 assign AAluSelF       = (BCmdIIs[IIsFR] ? BCmdIRAluFDec : 13'h0);

 assign ARegIdxR       = ((|{BCmdIIs[IIsLui], BCmdIIs[IIsAuipc], BCmdIIs[IIsJal], BIsJalr, BIsSwt, BCmdIIs[IIsAI], BCmdIIs[IIsAR], BCmdIIs[IIsMR], BCmdIIs[IIsFR], BCmdCIs[IIsCAddi], BCmdCIs[IIsCAdd], BCmdCIs[IIsCLI], BCmdCIs[IIsCLui], BCmdCIs[IIsCMV], BCmdCIs[IIsCSlli]}) ? {2'h2, LRegR} : 6'h0) |
                         ((|{AStepNext[IStBx]}) ? {2'h2, 4'h0} : 6'h0) |
                         ((|{BCmdCIs[IIsCAdd4Spn]}) ? BRegIdx42C : 6'h0) |
                         ((|{BCmdCIs[IIsCAddi16Sp]}) ? CRegSP : 6'h0) |
                         ((|{BCmdCIs[IIsCJal], BCmdCIs[IIsCJalr]}) ? CRegRA : 6'h0) |
                         ((|{BCmdCIs[IIsCAndi], BCmdCIs[IIsCSrli], BCmdCIs[IIsCSrai], BCmdCIs[IIsCSub], BCmdCIs[IIsCAnd], BCmdCIs[IIsCOr], BCmdCIs[IIsCXor]}) ? BRegIdx97C : 6'h0);

 assign ADstFlagWr     = AStepNext[IStBx];

 assign AMioWrRdEn     = {
                          |{BCmdIIs[IIsSx], BCmdCIs[IIsCSW], BCmdCIs[IIsCSwsp]},
                          |{BCmdIIs[IIsLx], BCmdCIs[IIsCLW], BCmdCIs[IIsCLwsp]}
                         };

 assign AMioSize       = ((|{BCmdIIs[IIsSx], BCmdIIs[IIsLx]}) ? BFn3SizeMio : 2'h0) |
                         ((|{BCmdCIs[IIsCSW], BCmdCIs[IIsCLW], BCmdCIs[IIsCLwsp], BCmdCIs[IIsCSwsp]}) ? 2'h2 : 2'h0);

 assign AMioSignExt    = {
                          |{BCmdIIs[IIsLx] & (LFn3Dec[1] | LFn3Dec[0]), BCmdCIs[IIsCLW], BCmdCIs[IIsCLwsp]},
                          ((BCmdIIs[IIsLx] & (|{LFn3Dec[5], LFn3Dec[4], LFn3Dec[2], LFn3Dec[1], LFn3Dec[0]})) ? BFn3SizeMio : 2'h0) |
                          ((BCmdIIs[IIsSx] & (|{LFn3Dec[2], LFn3Dec[1], LFn3Dec[0]})) ? BFn3SizeMio : 2'h0) |
                          ((|{BCmdCIs[IIsCSW], BCmdCIs[IIsCLW], BCmdCIs[IIsCLwsp], BCmdCIs[IIsCSwsp]}) ? 2'h2 : 2'h0)
                         };

 // Multi-step

 assign AStepNext[9:1] = 9'h0;
 assign AStepNext[IStBx]     = (|{BCmdIIs[IIsBx], BCmdCIs[IIsCBeqz], BCmdCIs[IIsCBnez]}) & ~BStepThisNZ & ACmdLenValid;

 assign AUnityReq = 1'b0;
endmodule

/*
funct7       rs2 rs1 funct3 rd          opcode R-type
imm[11:0]        rs1 funct3 rd          opcode I-type
imm[11:5]    rs2 rs1 funct3 imm[4:0]    opcode S-type
imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] opcode B-type
imm[31:12]                  rd          opcode U-type
imm[20|10:1|11|19:12]       rd          opcode J-type

              imm[31:12] rd          0110 111 LUI
              imm[31:12] rd          0010 111 AUIPC
   imm[20|10:1|11|19:12] rd          1101 111 JAL
   imm[11:0]     rs1 000 rd          1100 111 JALR

imm[12|10:5] rs2 rs1 000 imm[4:1|11] 1100 011 BEQ
imm[12|10:5] rs2 rs1 001 imm[4:1|11] 1100 011 BNE
imm[12|10:5] rs2 rs1 100 imm[4:1|11] 1100 011 BLT
imm[12|10:5] rs2 rs1 101 imm[4:1|11] 1100 011 BGE
imm[12|10:5] rs2 rs1 110 imm[4:1|11] 1100 011 BLTU
imm[12|10:5] rs2 rs1 111 imm[4:1|11] 1100 011 BGEU

       imm[11:0] rs1 000 rd          0000 011 LB
       imm[11:0] rs1 001 rd          0000 011 LH
       imm[11:0] rs1 010 rd          0000 011 LW
       imm[11:0] rs1 100 rd          0000 011 LBU
       imm[11:0] rs1 101 rd          0000 011 LHU

   imm[11:5] rs2 rs1 000 imm[4:0]    0100 011 SB
   imm[11:5] rs2 rs1 001 imm[4:0]    0100 011 SH
   imm[11:5] rs2 rs1 010 imm[4:0]    0100 011 SW

       imm[11:0] rs1 000 rd          0010 011 ADDI
       imm[11:0] rs1 010 rd          0010 011 SLTI
       imm[11:0] rs1 011 rd          0010 011 SLTIU
       imm[11:0] rs1 100 rd          0010 011 XORI
       imm[11:0] rs1 110 rd          0010 011 ORI
       imm[11:0] rs1 111 rd          0010 011 ANDI
   0000000 shamt rs1 001 rd          0010 011 SLLI
   0000000 shamt rs1 101 rd          0010 011 SRLI
   0100000 shamt rs1 101 rd          0010 011 SRAI

     0000000 rs2 rs1 000 rd          0110 011 ADD
     0100000 rs2 rs1 000 rd          0110 011 SUB
     0000000 rs2 rs1 010 rd          0110 011 SLT
     0000000 rs2 rs1 011 rd          0110 011 SLTU
     0000000 rs2 rs1 100 rd          0110 011 XOR
     0000000 rs2 rs1 110 rd          0110 011 OR
     0000000 rs2 rs1 111 rd          0110 011 AND
     0000000 rs2 rs1 001 rd          0110 011 SLL
     0000000 rs2 rs1 101 rd          0110 011 SRL
     0100000 rs2 rs1 101 rd          0110 011 SRA

     0000001 rs2 rs1 000 rd          0110 011 MUL
     0000001 rs2 rs1 001 rd          0110 011 MULH
     0000001 rs2 rs1 010 rd          0110 011 MULHSU
     0000001 rs2 rs1 011 rd          0110 011 MULHU
     0000001 rs2 rs1 100 rd          0110 011 DIV
     0000001 rs2 rs1 101 rd          0110 011 DIVU
     0000001 rs2 rs1 110 rd          0110 011 REM
     0000001 rs2 rs1 111 rd          0110 011 REMU

     0000000 rs2   rs1 rm rd         1010 011 FADD.S
     0000100 rs2   rs1 rm rd         1010 011 FSUB.S
     0001000 rs2   rs1 rm rd         1010 011 FMUL.S
     0001100 rs2   rs1 rm rd         1010 011 FDIV.S
     1100000 00000 rs1 rm rd         1010 011 FCVT.W.S
     1100000 00001 rs1 rm rd         1010 011 FCVT.WU.S
     1101000 00000 rs1 rm rd         1010 011 FCVT.S.W
     1101000 00001 rs1 rm rd         1010 011 FCVT.S.WU

  000000000001 00000 000 00000       1110 011 EBREAK

    000 nzuimm[5:4|9:6|2|3]      rd′  00 C.ADDI4SPN (RES, nzuimm=0)
    010 uimm[5:3] rs1′ uimm[2|6] rd′  00 C.LW
    110 uimm[5:3] rs1′ uimm[2|6] rs2′ 00 C.SW

000 nzimm[5] 0 nzimm[4:0] 01 C.NOP (HINT, nzimm̸=0)
000 nzimm[5] rs1/rd̸=0 nzimm[4:0] 01 C.ADDI (HINT, nzimm=0)
001 imm[11|4|9:8|10|6|7|3:1|5] 01 C.JAL (RV32)
001 imm[5] rs1/rd̸=0 imm[4:0] 01 C.ADDIW (RV64/128; RES, rd=0)
010 imm[5] rd̸=0 imm[4:0] 01 C.LI (HINT, rd=0)
011 nzimm[9] 2 nzimm[4|6|8:7|5] 01 C.ADDI16SP (RES, nzimm=0)

011 nzimm[17] rd̸={0, 2} nzimm[16:12] 01 C.LUI (RES, nzimm=0; HINT, rd=0)

100 nzuimm[5] 00 rs1 ′/rd ′ nzuimm[4:0] 01 C.SRLI (RV32 NSE, nzuimm[5]=1)
100 0 00 rs1 ′/rd ′ 0 01 C.SRLI64 (RV128; RV32/64 HINT)
100 nzuimm[5] 01 rs1 ′/rd ′ nzuimm[4:0] 01 C.SRAI (RV32 NSE, nzuimm[5]=1)
100 0 01 rs1 ′/rd ′ 0 01 C.SRAI64 (RV128; RV32/64 HINT)
100 imm[5] 10 rs1 ′/rd ′ imm[4:0] 01 C.ANDI
100 0 11 rs1′/rd′ 00 rs2′ 01 C.SUB
100 0 11 rs1′/rd′ 01 rs2′ 01 C.XOR
100 0 11 rs1′/rd′ 10 rs2′ 01 C.OR
100 0 11 rs1′/rd′ 11 rs2′ 01 C.AND
100 1 11 rs1′/rd′ 00 rs2′ 01 C.SUBW (RV64/128; RV32 RES)
100 1 11 rs1′/rd′ 01 rs2′ 01 C.ADDW (RV64/128; RV32 RES)
100 1 11 — 10 — 01 Reserved
100 1 11 — 11 — 01 Reserved
101 imm[11|4|9:8|10|6|7|3:1|5] 01 C.J
110 imm[8|4:3] rs1 ′ imm[7:6|2:1|5] 01 C.BEQZ
111 imm[8|4:3] rs1 ′ imm[7:6|2:1|5] 01 C.BNEZ
*/

