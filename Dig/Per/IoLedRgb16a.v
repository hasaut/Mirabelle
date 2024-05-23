module IoLedRgb16b #(parameter CAddrBase=16'h0000)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [15:0] AIoAddr, output [63:0] AIoMiso, input [63:0] AIoMosi, input [3:0] AIoWrSize, input [3:0] AIoRdSize, output AIoAddrAck, output AIoAddrErr,
  output [15:0] ALedIdx, output [2:0] AColor
 );

 wire [3:0] BWrEnQ, BRdEnQ;
 wire [3:0] BWrEnD, BRdEnD;
 wire [3:0] BWrEnW, BRdEnW;
 wire [3:0] BWrEnB, BRdEnB;

 IoIntf2a #(.CAddrBase(CAddrBase), .CAddrUsed(32'h00100001)) UIntf
  (
   .AAddr(AIoAddr), .AWrSize(AIoWrSize), .ARdSize(AIoRdSize),
   .AWrEnQ(BWrEnQ), .ARdEnQ(BRdEnQ),
   .AWrEnD(BWrEnD), .ARdEnD(BRdEnD),
   .AWrEnW(BWrEnW), .ARdEnW(BRdEnW),
   .AWrEnB(BWrEnB), .ARdEnB(BRdEnB),
   .AAddrAck(AIoAddrAck), .AAddrErr(AIoAddrErr)
  );

 wire [3:0] FLedIdx, BLedIdx;
 wire [7:0] FPwmCnt, BPwmCnt;
 wire [15:0] FLitIdx, BLitIdx;
 wire [15:0] FLitIdxOut, BLitIdxOut;
 wire [2:0] FColorOut, BColorOut;

 MsDffList #(.CRegLen(4+8+16+16+3)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BLedIdx, BPwmCnt, BLitIdx, BLitIdxOut, BColorOut}),
   .ADataO({FLedIdx, FPwmCnt, FLitIdx, FLitIdxOut, FColorOut})
  );

 wire BPwmCntE = &FPwmCnt;
 assign BLedIdx = BRdEnB[0] ? 4'h0 : FLedIdx + {3'h0, BWrEnD[0]};
 assign BPwmCnt = FPwmCnt+8'h1;
 assign BLitIdx = BPwmCntE ? {FLitIdx[14:0], ~(|FLitIdx[14:0])} : FLitIdx;

 wire [15:0] BLedIdxDec; MsDec4x16a ULedIdxDec ( .ADataI(FLedIdx), .ADataO(BLedIdxDec) );

 wire [47:0] BPwmAll;
 LedRgbCtrlB ULedPwm[15:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AColor(AIoMosi[23:0]), .AWrEn(BWrEnD[0] ? BLedIdxDec : 16'h0),
   .APwmCnt(FPwmCnt), .APwm(BPwmAll)
  );

 assign BLitIdxOut = FLitIdx;
 MsSelectRow #(.CRowCnt(16), .CColCnt(3)) UColorOut ( .ADataI(BPwmAll), .AMask(FLitIdx), .ADataO(BColorOut) );

 assign {ALedIdx, AColor} = {FLitIdxOut, FColorOut};

 assign AIoMiso = 64'h0;
endmodule

module LedRgbCtrlB
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [23:0] AColor, input AWrEn,
  input [7:0] APwmCnt, output [2:0] APwm
 );

 wire [23:0] FColor, BColor;

 MsDffList #(.CRegLen(24)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BColor}),
   .ADataO({FColor})
  );

 assign BColor = AWrEn ? AColor : FColor;
 assign APwm =
  {
   APwmCnt<FColor[23:16],
   APwmCnt<FColor[15: 8],
   APwmCnt<FColor[ 7: 0]
  };

endmodule


