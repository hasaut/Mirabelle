module FpuMantShr #(parameter CExpLen=0, CMantLen=0)
 (
  input wire [CMantLen-1:0] ADataI, input wire [CExpLen-1:0] AShr, output wire [CMantLen-1:0] ADataO
 );

 wire [CMantLen+32-1:0] BDataG = {{32{1'b0}}, ADataI};
 wire [CMantLen+16-1:0] BDataF = AShr[5] ? BDataG[CMantLen+32-1:32] : BDataG[CMantLen+16-1:0];
 wire [CMantLen+ 8-1:0] BDataE = AShr[4] ? BDataF[CMantLen+16-1:16] : BDataF[CMantLen+ 8-1:0];
 wire [CMantLen+ 4-1:0] BDataD = AShr[3] ? BDataE[CMantLen+ 8-1: 8] : BDataE[CMantLen+ 4-1:0];
 wire [CMantLen+ 2-1:0] BDataC = AShr[2] ? BDataD[CMantLen+ 4-1: 4] : BDataD[CMantLen+ 2-1:0];
 wire [CMantLen+ 1-1:0] BDataB = AShr[1] ? BDataC[CMantLen+ 2-1: 2] : BDataC[CMantLen+ 1-1:0];
 wire [CMantLen+ 0-1:0] BDataA = AShr[0] ? BDataB[CMantLen+ 1-1: 1] : BDataB[CMantLen+ 0-1:0];

 assign ADataO = BDataA;
endmodule

module FpuAlignL #(parameter CExpLen=0, CMantLen=0)
 (
  input wire [CMantLen+2-1:0] ADataI, output wire [CMantLen-1:0] ADataO, output wire [CExpLen-1:0] AIdx
 );

 localparam CDataILen = CMantLen+2;

 wire [CDataILen+32-1:0] BDataIA = {ADataI, 32'h0};
 wire [31:0] BDataI = BDataIA[CDataILen+32-1:CDataILen];

 wire [4:0] BIdx;
 assign BIdx[4] = ~|BDataI[31:16]; wire [31:0] BDataE = BIdx[4] ? {BDataI[15:0], 16'h0} : BDataI;
 assign BIdx[3] = ~|BDataE[31:24]; wire [31:0] BDataD = BIdx[3] ? {BDataE[23:0],  8'h0} : BDataE;
 assign BIdx[2] = ~|BDataD[31:28]; wire [31:0] BDataC = BIdx[2] ? {BDataD[27:0],  4'h0} : BDataD;
 assign BIdx[1] = ~|BDataC[31:30]; wire [31:0] BDataB = BIdx[1] ? {BDataC[29:0],  2'h0} : BDataC;
 assign BIdx[0] = ~|BDataB[31:31]; wire [31:0] BDataA = BIdx[0] ? {BDataB[30:0],  1'h0} : BDataB;

 wire BIdxF = &BIdx;
 assign AIdx = BIdxF ? {CExpLen{1'b1}} : {{(CExpLen-5){1'b0}}, BIdx};
 assign ADataO = BDataA[31:32-CMantLen];
endmodule

module FpuTaylorSerCoef
 (
  input wire [5:0] AAddr, output wire [35:0] ASerCoef // {Exp[7:0], Mant[27:0]}
 );

 reg [35:0] GSerCoef;

 always @(AAddr)
  begin
  GSerCoef = 36'h0;
  case (AAddr)
    6'h00: GSerCoef = 36'h00000000;
    6'h01: GSerCoef = {8'h7F, 28'h8000000}; // 1.0
    6'h02: GSerCoef = {8'h7E, 28'h8000000}; // 1/2!
    6'h03: GSerCoef = {8'h7C, 28'hAAAAAAA}; // 1/3!
    6'h04: GSerCoef = {8'h7A, 28'hAAAAAAA}; // 1/4!
    6'h05: GSerCoef = {8'h78, 28'h8888888}; // 1/5!
    6'h06: GSerCoef = {8'h75, 28'hB60B60B}; // 1/6!
    6'h07: GSerCoef = {8'h72, 28'hD00D00D}; // 1/7!
    6'h08: GSerCoef = {8'h6F, 28'hD00D00D}; // 1/8!
    6'h09: GSerCoef = {8'h6C, 28'hB8EF1D2}; // 1/9!
    6'h0A: GSerCoef = {8'h69, 28'h93F27DB}; // 1/10!
    6'h0B: GSerCoef = {8'h65, 28'hD7322B3}; // 1/11!
    6'h0C: GSerCoef = {8'h62, 28'h8F76C77}; // 1/12!
    6'h0D: GSerCoef = {8'h5E, 28'hB092309}; // 1/13!
    6'h0E: GSerCoef = {8'h5A, 28'hC9CBA54}; // 1/14!
    6'h0F: GSerCoef = {8'h56, 28'hD73F9F2}; // 1/15!
    6'h10: GSerCoef = {8'h52, 28'hD73F9E3}; // 1/16!
    6'h11: GSerCoef = 36'h00000000;
    6'h12: GSerCoef = 36'h00000000;
    6'h13: GSerCoef = 36'h00000000;
    6'h14: GSerCoef = 36'h00000000;
    6'h15: GSerCoef = 36'h00000000;
    6'h16: GSerCoef = 36'h00000000;
    6'h17: GSerCoef = 36'h00000000;
    6'h18: GSerCoef = 36'h00000000;
    6'h19: GSerCoef = 36'h00000000;
    6'h1A: GSerCoef = 36'h00000000;
    6'h1B: GSerCoef = 36'h00000000;
    6'h1C: GSerCoef = 36'h00000000;
    6'h1D: GSerCoef = 36'h00000000;
    6'h1E: GSerCoef = 36'h00000000;
    6'h1F: GSerCoef = 36'h00000000;
  default: GSerCoef = 36'h0;
  endcase
 end

/*
 3FF0000000000000 3F800000 | {8'h7F, 28'h8000000}
 3FE0000000000000 3F000000 | {8'h7E, 28'h8000000}
 3FC5555555555555 3E2AAAAB | {8'h7C, 28'hD555555}
 3FA5555555555555 3D2AAAAB | {8'h7A, 28'hD555555}
 3F81111111111111 3C088889 | {8'h78, 28'h9111111}
 3F56C16C16C16C17 3AB60B61 | {8'h75, 28'h96C16C1}
 3F2A01A01A01A01A 39500D01 | {8'h72, 28'h9A01A01}
 3EFA01A01A01A01A 37D00D01 | {8'h6F, 28'h9A01A01}
 3EC71DE3A556C736 3638EF1D | {8'h6C, 28'hA556C73}
 3E927E4FB7789F71 3493F27E | {8'h69, 28'hB7789F7}
 3E5AE64567F542DC 32D7322B | {8'h65, 28'hE7F542D}
 3E21EED8EFF8F6B9 310F76C7 | {8'h62, 28'hEFF8F6B}
 3DE6124613A97A9B 2F309231 | {8'h5E, 28'h93A97A9}
 3DA93974A8D5EDCD 2D49CBA5 | {8'h5A, 28'hA8D5EDC}
 3D6AE7F3E552DF2B 2B573F9F | {8'h56, 28'hE552DF2}
 3D2AE7F3C7CF15DB 29573F9E | {8'h52, 28'hC7CF15D}

*/

 assign ASerCoef = GSerCoef;
endmodule


