/*module MsFpuAlignL ( input [31:0] ADataI, output [23:0] ADataO, output [4:0] AIdx );
 wire [31:0] BDataF = ADataI;
 assign AIdx[4] = |BDataF[31:16]; wire [31:0] BDataE = AIdx[4] ? BDataF[31: 0] : {BDataF[15: 0], 16'h0};
 assign AIdx[3] = |BDataE[31:24]; wire [31:0] BDataD = AIdx[3] ? BDataE[31: 0] : {BDataE[23: 0],  8'h0};
 assign AIdx[2] = |BDataD[31:28]; wire [31:0] BDataC = AIdx[2] ? BDataD[31: 0] : {BDataD[27: 0],  4'h0};
 assign AIdx[1] = |BDataC[31:30]; wire [31:0] BDataB = AIdx[1] ? BDataC[31: 0] : {BDataC[29: 0],  2'h0};
 assign AIdx[0] =  BDataB[31];    wire [31:0] BDataA = AIdx[0] ? BDataB[31: 0] : {BDataB[30: 0],  1'h0};
 assign ADataO = BDataA[31:8];
endmodule*/

module MsFpuAlignL ( input [31:0] ADataI, output [31:0] ADataO, output [4:0] AIdx );
 wire [31:0] BDataF = ADataI;
 assign AIdx[4] = |BDataF[31:16]; wire [31:0] BDataE = AIdx[4] ? BDataF[31: 0] : {BDataF[15: 0], 16'h0};
 assign AIdx[3] = |BDataE[31:24]; wire [31:0] BDataD = AIdx[3] ? BDataE[31: 0] : {BDataE[23: 0],  8'h0};
 assign AIdx[2] = |BDataD[31:28]; wire [31:0] BDataC = AIdx[2] ? BDataD[31: 0] : {BDataD[27: 0],  4'h0};
 assign AIdx[1] = |BDataC[31:30]; wire [31:0] BDataB = AIdx[1] ? BDataC[31: 0] : {BDataC[29: 0],  2'h0};
 assign AIdx[0] =  BDataB[31];    wire [31:0] BDataA = AIdx[0] ? BDataB[31: 0] : {BDataB[30: 0],  1'h0};
 assign ADataO = BDataA[31:0];
endmodule

module MsFpuSA
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [31:0] AMuxS, input [31:0] AMuxU,
  input [6:0] AAluSel, // # round trunc itf div mul sub add
  // Common MulDiv
  output [31:0] AMulDivDataS, output [31:0] AMulDivDataD, output [1:0] AMulDivStart,
  input [31:0] AMulDivDataH, AMulDivDataR, input AMulDivWrEn, // DataH = Resid
  // Output
  output [31:0] AFpuRes, output AFpuAck
 );

 localparam IOperRnd = 6;
 localparam IOperTru = 5;
 localparam IOperItf = 4;
 localparam IOperDiv = 3;
 localparam IOperMul = 2;
 localparam IOperSub = 1;
 localparam IOperAdd = 0;

 // Implementation
 wire [6:0] FOper, BOper;
 wire [31:0] FDataS, BDataS;
 wire [31:0] FDataD, BDataD;
 wire [1:0] FMulDivOper, BMulDivOper;
 wire FMulDivReady, BMulDivReady;
 wire FZeroDiv, BZeroDiv;

 MsDffList #(.CRegLen(7+32+32+2+1+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BOper, BDataS, BDataD, BMulDivOper, BMulDivReady, BZeroDiv}),
   .ADataO({FOper, FDataS, FDataD, FMulDivOper, FMulDivReady, FZeroDiv})
  );

 wire BBusBWrEn;
 wire BAluSelNZ = |AAluSel;

 wire [23:0] BDataSM_Init = BAluSelNZ ? {|AMuxS[30:0], AMuxS[22:0]} : 24'h0;
 wire [23:0] BDataDM_Init = BAluSelNZ ? {|AMuxU[30:0], AMuxU[22:0]} : 24'h0;

 assign {BOper, BDataS[31:23], BDataD[31:23], BZeroDiv} =
  (BAluSelNZ ? {AAluSel[6:0], AMuxS[31:23], AMuxU[31:23], ~(|AMuxS[30:0]) & AAluSel[3]} : 26'h0) |
  ((~BBusBWrEn) ? {FOper, FDataS[31:23], FDataD[31:23], FZeroDiv} : 26'h0);

 // Common
 wire BOperIsAddSub = |FOper[IOperSub:IOperAdd];
 //wire BOperIsMulDiv = |FOper[IOperDiv:IOperMul];
 wire BOperIsTruRnd = |FOper[IOperRnd:IOperTru];
 wire BOperIsItf    =  FOper[IOperItf];

 // Common Shr
 wire [31:0] BShrDataI;
 wire [5:0] BShrIdx;
 wire [63:0] BShrDataOX; wire [31:0] BShrDataO = BShrDataOX[63:32];
 MsShrFpu UShr ( .ADataI(BShrDataI), .AIdx(BShrIdx), .ADataO(BShrDataOX) );

 // Which one is bigger by module?
 wire BCmpModA = FDataS[30:0] >  FDataD[30:0];
 wire BCmpModE = FDataS[30:0] == FDataD[30:0];
 //wire BCmpMod = BCmpModA | BCmpModE;
 // BDataB is bigger than BDataA
 wire [31:0] BDataB, BDataA; assign {BDataB, BDataA} = (BCmpModA & BOperIsAddSub) ? {FDataS, FDataD} : {FDataD, FDataS};
 // Mantissa for B and A
 wire BDataBNZ = |BDataB[30:0]; wire BDataANZ = |BDataA[30:0];
 wire [23:0] BMantB = {BDataBNZ, BDataB[22:0]}; wire [23:0] BMantA = {BDataANZ, BDataA[22:0]};
 // Is physical operation is add or sub
 wire BIsSub = FOper[IOperSub] ^ FDataS[31] ^ FDataD[31];

 // exp delta shows smaller operand shr steps
 wire [7:0] BExpDelta = BDataB[30:23]-BDataA[30:23];

 // AddSub section
 wire [5:0] BShrIdxAddSub = {|BExpDelta[7:5], BExpDelta[4:0]};
 wire [32:0] BDataRAddSubA = {1'b0, BMantB, 7'h0, BIsSub} + ({33{BIsSub}}^{1'b0, BShrDataO[31:0]});

 // Aliases
 //wire [23:0] LDataSM = {|FDataS[30:0], FDataS[22:0]};
 wire [23:0] LDataDM = {|FDataD[30:0], FDataD[22:0]};

 // MulDiv section
 assign AMulDivDataS = {8'h0, BDataSM_Init};
 assign AMulDivDataD = {8'h0, BDataDM_Init};
 assign AMulDivStart = AAluSel[IOperDiv:IOperMul];

 assign BMulDivOper = AAluSel[IOperDiv:IOperMul] | (FMulDivOper & {2{~AMulDivWrEn}});
 assign BMulDivReady = (|FMulDivOper) & AMulDivWrEn;
 //wire BMulReady = FMulDivOper[0] & AMulDivWrEn;
 //wire BDivReady = FMulDivOper[1] & AMulDivWrEn;

 assign BDataD[22:0] =
  ((|{AAluSel[IOperSub:IOperAdd], AAluSel[IOperRnd:IOperTru], AAluSel[IOperItf]}) ? AMuxU[22:0] : 23'h0) |
  (BMulDivReady ? AMulDivDataR[22:0] : 23'h0);

 assign BDataS[22:0] =
  ((|{AAluSel[IOperSub:IOperAdd]}) ? AMuxS[22:0] : 23'h0) |
  (BMulDivReady ? {FMulDivOper[1] & (|AMulDivDataH), 16'h0, AMulDivDataR[28:23]} : 23'h0);

 wire [29:0] LDataRMulDivA = {FDataS[5:0], FDataD[22:0], FDataS[22]};

 // Tru/Rnd section
 wire [7:0] BShrIdxTruRndA = 8'h9D - FDataD[30:23]; wire [5:0] BShrIdxTruRnd = BShrIdxTruRndA[5:0];
 //wire [39:0] BShrDataOA = (FDataD[30:23]>8'h9D) ? 40'hFFFFFFFE00 : ((FDataD[30:23]<8'h1E) ? 40'h0 : {8'h0, BShrDataO});
 wire [30:0] BDataRI_Mant = ({31{FDataD[31]}} ^ BShrDataOX[63:33]) + {30'h0, (FOper[IOperRnd] & BShrDataOX[32] & (|{BShrDataOX[31:8], BShrDataOX[33]})) ^ FDataD[31]}; // 31-bit "integer_mantissa" (without sign); Rounding bit should take in account bit [0] of main part: this seem to be a standard of IEEE
 wire BDataRI_Mant_NZ = |BDataRI_Mant;
 wire [31:0] BDataRI = (FDataD[30:23]>8'h9D) ? {32{FDataD[31]}} ^ 32'h7FFFFFFF :
                       (FDataD[30:23]<8'h1E) ? 32'h0 : {BDataRI_Mant_NZ & FDataD[31], BDataRI_Mant}; // If zero, do not set bit 31

 // Align (both Itf and others)
 wire [31:0] BMantXA =
  (BOperIsAddSub ? BDataRAddSubA[32:1] : 32'h0) |
  (FMulDivReady  ? {LDataRMulDivA, 2'h0} : 32'h0);
 wire [31:0] BAlignDataI =
    BMantXA |
   (BOperIsItf ? {1'b0, (FDataD[31] ? ~FDataD[30:0]+31'h1 : FDataD[30:0])} : 32'h0);
 wire [31:0] BMantXB;
 wire [4:0] BShlIdxC;
 MsFpuAlignL UMantX ( .ADataI(BAlignDataI), .ADataO(BMantXB), .AIdx(BShlIdxC) );
 wire [24:0] BMantXC = FZeroDiv ? 25'h1FFFFFF : {1'b0, BMantXB[31:8]} + {24'h0, BMantXB[7] & (|BMantXB[6:0])}; // LSB correction, but this can overflow
 wire [4:0] BShlIdx = BShlIdxC + {4'h0, BMantXC[24]};
 wire [23:0] BMantX = BMantXC[24] ? BMantXC[24:1] : BMantXC[23:0];

 // Exp
 /*
 wire [9:0] BExpMul = // [9:8]: 0=underflow; 1=OK; 2=overflow (perhaps 10'h100 = "underflow")
  {2'h0, FDataD[30:23]}+
  {2'h0, FDataS[30:23]}+
  10'h81; // 0x81-31+BShlIdx = 0x62+BShlIdx

 wire [9:0] BExpDiv = // [9:8]: 0=underflow; 1=OK*; 2=overflow  ( * 10'h100 = "undeflow")
  {2'h0, FDataD[30:23]}+
  {2'h3, ~FDataS[30:23]}+
  10'h17F; // 0x17F-31+BShlIdx = 0x160+BShlIdx

 wire [9:0] BExpAddSub =
  {2'h0, BDataB[30:23]}+
  10'h100; // 0x100-31+BShlIdx = 0xE1+BShlIdx

 wire [9:0] BExpItf =
  BShlIdx+10'h17F;
 */

 wire [9:0] BExpA =
  (FMulDivReady  ? {2'h0, FDataD[30:23]} : 10'h0) |
  (BOperIsAddSub ? {2'h0, BDataB[30:23]} : 10'h0);
 wire [9:0] BExpB =
  (FMulDivReady ? {2'h0, FDataS[30:23]} ^ {10{FOper[IOperDiv]}} : 10'h0);
 wire [9:0] BExpC =
  ((FMulDivReady & FOper[IOperMul]) ? 10'h063 : 10'h0) |
  ((FMulDivReady & FOper[IOperDiv]) ? 10'h162 : 10'h0) |
  (BOperIsAddSub ? 10'h0E2 : 10'h0) |
  (FOper[IOperItf] ? 10'h17F : 10'h0);

 wire [9:0] BExp = BExpA + BExpB + BExpC + {5'h0, BShlIdx};
 wire BExpOvf = BExp[9] | FZeroDiv;
 wire BExpUdf = (BExp[9:8]==2'h0) | (BExp==10'h100);

 /*
 wire [8:0] BExpA = {1'b0, FDataS[30:23]} - 9'h80;
 wire [8:0] BExpB = {4'h0, BShlIdx} + 9'h1E2;  // i.e. ShlIdx-30
 wire [8:0] BExpC =
  (
   (BOperIsAddSub ? {1'b0, BDataB[30:23]} : 9'h0) |
   (FMulDivReady  ? {1'b0, FDataD[30:23]} : 9'h0) |
   (FOper[IOperItf] ? {1'b0, 8'h9D} : 9'h0)          // To compensate -30
  ) +
  (
   ((FMulDivReady & FOper[IOperMul]) ?  BExpA+9'h1 : 9'h0) |
   ((FMulDivReady & FOper[IOperDiv]) ? ~BExpA      : 9'h0)
  );
 wire [8:0] BExp = FZeroDiv ? 9'h1FF : BExpB + BExpC;*/

 wire BOrigSignE = FDataD[31]==FDataS[31];
 wire BSign =
  (FOper[IOperAdd] ? ((BCmpModE & ~BOrigSignE) ? 1'b0 : BDataB[31]) : 1'b0) |
  (FOper[IOperSub] ? ((BCmpModE &  BOrigSignE) ? 1'b0 : BCmpModA^BDataB[31]) : 1'b0) |
  (FMulDivReady    ? ~BOrigSignE : 1'b0) |
  (BOperIsItf      ? FDataD[31] : 1'b0);

 wire BMantXNZ = |BMantX;
 //wire [31:0] BDataRF = {BSign, {31{BMantXNZ}} & (BExp[8] ? {8'hFF, 23'h0} : {BExp[7:0], BMantX[22:0]})};
 wire [31:0] BDataRF = {BSign, {31{BMantXNZ & ~BExpUdf}} & (BExpOvf ? {8'hFF, 23'h0} : {BExp[7:0], BMantX[22:0]})};
 //wire [31:0] BDataRI = {FDataD[31], FDataD[31] ? ~BDataRTruRndA+31'h1 : BDataRTruRndA};

 // Shr (continue)
 assign BShrDataI =
  (BOperIsAddSub ? {BMantA, 8'h0} : 32'h0) |
  (BOperIsTruRnd ? {LDataDM, 8'h0} : 32'h0);

 assign BShrIdx =
  (BOperIsAddSub ? BShrIdxAddSub : 6'h0) |
  (BOperIsTruRnd ? BShrIdxTruRnd : 6'h0);

 // MCU Ctrl
 wire BOperIsI = BOperIsTruRnd;
 wire BOperIsF = |{FOper[IOperItf], FMulDivReady, BOperIsAddSub};
 assign BBusBWrEn = BOperIsI | BOperIsF;

 wire [31:0] BBusBDataI =
  (BOperIsI ? BDataRI : 32'h0) |
  (BOperIsF ? BDataRF : 32'h0);

 assign AFpuRes = BBusBDataI;
 assign AFpuAck = BBusBWrEn;
endmodule

module MsShrFpu ( input [31:0] ADataI, input [5:0] AIdx, output [63:0] ADataO );
 wire [63:0] BDataF = AIdx[5] ? {32'h0, ADataI} : {ADataI, 32'h0};
 wire [63:0] BDataE = AIdx[4] ? {16'h0, BDataF[63:16]} : BDataF;
 wire [63:0] BDataD = AIdx[3] ? { 8'h0, BDataE[63: 8]} : BDataE;
 wire [63:0] BDataC = AIdx[2] ? { 4'h0, BDataD[63: 4]} : BDataD;
 wire [63:0] BDataB = AIdx[1] ? { 2'h0, BDataC[63: 2]} : BDataC;
 wire [63:0] BDataA = AIdx[0] ? { 1'h0, BDataB[63: 1]} : BDataB;
 assign ADataO = BDataA;
endmodule


