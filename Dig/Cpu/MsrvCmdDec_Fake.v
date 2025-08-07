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

  assign ACmdLen = 2'h0;
  // VLIW
  assign ACond = 4'h0; assign ALoadEipImm = 1'b0;
  assign ATrap = 2'h0;
  assign ASysReq = 5'h0; // conf unlock lock end swt
  assign ARegIdxS = 6'h0; assign ARegIdxU = 6'h0;
  assign AWwConst = 2'h0; assign AConst = 32'h0; assign AMlsc = 5'h0; assign ALoopD = 3'h0;
  assign AMuxSrc = 8'h0; assign ASelIp = 2'h0;
  assign ARegIdxR = 6'h0; assign ADstFlagWr = 1'b0;
  assign AAluSignExt = 1'b0; assign AAluSelA = 4'h0; assign AAluSelU = 8'h0; assign AAluSelS = 4'h0; assign AAluSelT = 4'h0; assign AAluSelF = 13'h0;
  assign AMioWrRdEn = 2'h0; assign AMioSize = 2'h0; assign AMioSignExt = 3'h0;
  assign AStepNext = 10'h0;
  assign AUnityReq = 1'h0;

endmodule


