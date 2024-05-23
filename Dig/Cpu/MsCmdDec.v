module MsCmdDec
 (
  input [47:0] AQueTop, input [23:1] AIpThis, input [1:0] ACpuType,
  // CmdLen
  output  [1:0] ACmdLen,
  // VLIW
  output [3:0] ACond, output ALoadEipImm, // load IP by a special BusP (usually constant directly)
  output [1:0] ATrap,
  output [4:0] ASysReq, // conf unlock lock end swt
  output [5:0] ARegIdxS, ARegIdxU,
  output [1:0] AWwConst, output [31:0] AConst, output [4:0] AMlsc, output [2:0] ALoopD,
  output [7:0] AMuxSrc, output [2:1] ASelIp,
  output [5:0] ARegIdxR, output ADstFlagWr,
  output AAluSignExt, output [3:0] AAluSelA, output [7:0] AAluSelU, output [3:0] AAluSelS, output [3:0] AAluSelT, output [12:0] AAluSelF,
  output [1:0] AMioWrRdEn, output [1:0] AMioSize, output [2:0] AMioSignExt,
  input ACmdLenValid, input [9:0] AStepThis, output [9:0] AStepNext,
  input [15:0] APplListThis, output [15:0] APplListNext,
  output AUnityReq, input AUnityAck,
  input AMemPend
 );

 
 wire  [1:0] MSdCmdLen,        MRvCmdLen;

 // VLIW
 wire  [3:0] MSdCond,          MRvCond;
 wire        MSdLoadEipImm,    MRvLoadEipImm;
 wire  [1:0] MSdTrap,          MRvTrap;
 wire  [4:0] MSdSysReq,        MRvSysReq;
 wire  [5:0] MSdRegIdxS,       MRvRegIdxS;
 wire  [5:0] MSdRegIdxU,       MRvRegIdxU;
 wire  [1:0] MSdWwConst,       MRvWwConst;
 wire [31:0] MSdConst,         MRvConst;
 wire  [4:0] MSdMlsc,          MRvMlsc;
 wire  [2:0] MSdLoopD,         MRvLoopD;              // BusB BusA
 wire  [7:0] MSdMuxSrc,        MRvMuxSrc;
 wire  [2:1] MSdSelIp,         MRvSelIp;
 wire  [5:0] MSdRegIdxR,       MRvRegIdxR;
 wire        MSdDstFlagWr,     MRvDstFlagWr;
 wire        MSdAluSignExt,    MRvAluSignExt;
 wire  [3:0] MSdAluSelA,       MRvAluSelA;
 wire  [7:0] MSdAluSelU,       MRvAluSelU;
 wire  [3:0] MSdAluSelS,       MRvAluSelS;
 wire  [3:0] MSdAluSelT,       MRvAluSelT;
 wire [12:0] MSdAluSelF,       MRvAluSelF;
 wire  [1:0] MSdMioWrRdEn,     MRvMioWrRdEn;
 wire  [1:0] MSdMioSize,       MRvMioSize;
 wire  [2:0] MSdMioSignExt,    MRvMioSignExt;
 wire        MSdUnityReq,      MRvUnityReq;

 wire  [9:0] MSdStepNext,      MRvStepNext;

 MssdCmdDec USdCmdDec
  (
   .AQueTop(AQueTop), .AIpThis(AIpThis), .AUseThisCpu(ACpuType==2'h1),
   // CmdLen
   .ACmdLen(MSdCmdLen),
   // Vliw
   .ACond(MSdCond), .ALoadEipImm(MSdLoadEipImm),
   .ATrap(MSdTrap), .ASysReq(MSdSysReq),
   .ARegIdxS(MSdRegIdxS), .ARegIdxU(MSdRegIdxU),
   .AWwConst(MSdWwConst), .AConst(MSdConst), .AMlsc(MSdMlsc), .ALoopD(MSdLoopD),
   .AMuxSrc(MSdMuxSrc), .ASelIp(MSdSelIp),
   .ARegIdxR(MSdRegIdxR), .ADstFlagWr(MSdDstFlagWr),
   .AAluSignExt(MSdAluSignExt), .AAluSelA(MSdAluSelA), .AAluSelU(MSdAluSelU), .AAluSelS(MSdAluSelS), .AAluSelT(MSdAluSelT), .AAluSelF(MSdAluSelF),
   .AMioWrRdEn(MSdMioWrRdEn), .AMioSize(MSdMioSize), .AMioSignExt(MSdMioSignExt),
   // Multi-Step
   .ACmdLenValid(ACmdLenValid), .AStepThis(AStepThis), .AStepNext(MSdStepNext),
   .APplListThis(APplListThis), .APplListNext(APplListNext),
   .AUnityReq(MSdUnityReq), .AUnityAck(AUnityAck),
   .AMemPend(AMemPend)
  );


 MsrvCmdDec URvCmdDec
  (
   .AQueTop(AQueTop[31:0]), .AIpThis(AIpThis), .AUseThisCpu(ACpuType==2'h0),
   // CmdLen
   .ACmdLen(MRvCmdLen),
   // Vliw
   .ACond(MRvCond), .ALoadEipImm(MRvLoadEipImm),
   .ATrap(MRvTrap), .ASysReq(MRvSysReq),
   .ARegIdxS(MRvRegIdxS), .ARegIdxU(MRvRegIdxU),
   .AWwConst(MRvWwConst), .AConst(MRvConst), .AMlsc(MRvMlsc), .ALoopD(MRvLoopD),
   .AMuxSrc(MRvMuxSrc), .ASelIp(MRvSelIp),
   .ARegIdxR(MRvRegIdxR), .ADstFlagWr(MRvDstFlagWr),
   .AAluSignExt(MRvAluSignExt), .AAluSelA(MRvAluSelA), .AAluSelU(MRvAluSelU), .AAluSelS(MRvAluSelS), .AAluSelT(MRvAluSelT), .AAluSelF(MRvAluSelF),
   .AMioWrRdEn(MRvMioWrRdEn), .AMioSize(MRvMioSize), .AMioSignExt(MRvMioSignExt),
   // Multi-Step
   .ACmdLenValid(ACmdLenValid), .AStepThis(AStepThis), .AStepNext(MRvStepNext),
   .AUnityReq(MRvUnityReq), .AUnityAck(AUnityAck),
   .AMemPend(AMemPend)
  );

 assign ACmdLen     = MSdCmdLen     | MRvCmdLen;
 assign ACond       = MSdCond       | MRvCond;
 assign ALoadEipImm = MSdLoadEipImm | MRvLoadEipImm;
 assign ATrap       = MSdTrap       | MRvTrap;
 assign ASysReq     = MSdSysReq     | MRvSysReq;
 assign ARegIdxS    = MSdRegIdxS    | MRvRegIdxS;
 assign ARegIdxU    = MSdRegIdxU    | MRvRegIdxU;
 assign AWwConst    = MSdWwConst    | MRvWwConst;
 assign AConst      = MSdConst      | MRvConst;
 assign AMlsc       = MSdMlsc       | MRvMlsc;
 assign ALoopD      = MSdLoopD      | MRvLoopD;
 assign AMuxSrc     = MSdMuxSrc     | MRvMuxSrc;
 assign ASelIp      = MSdSelIp      | MRvSelIp;
 assign ARegIdxR    = MSdRegIdxR    | MRvRegIdxR;
 assign ADstFlagWr  = MSdDstFlagWr  | MRvDstFlagWr;
 assign AAluSignExt = MSdAluSignExt | MRvAluSignExt;
 assign AAluSelA    = MSdAluSelA    | MRvAluSelA;
 assign AAluSelU    = MSdAluSelU    | MRvAluSelU;
 assign AAluSelS    = MSdAluSelS    | MRvAluSelS;
 assign AAluSelT    = MSdAluSelT    | MRvAluSelT;
 assign AAluSelF    = MSdAluSelF    | MRvAluSelF;
 assign AMioWrRdEn  = MSdMioWrRdEn  | MRvMioWrRdEn;
 assign AMioSize    = MSdMioSize    | MRvMioSize;
 assign AMioSignExt = MSdMioSignExt | MRvMioSignExt;
 assign AUnityReq   = MSdUnityReq   | MRvUnityReq;

 assign AStepNext   = MSdStepNext   | MRvStepNext;
endmodule
