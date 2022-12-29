module MsDffList #(parameter CRegLen=16) ( input AClkH, input AResetHN, input AClkHEn, input [CRegLen-1:0] ADataI, output [CRegLen-1:0] ADataO );
 reg [CRegLen-1:0] FData; wire [CRegLen-1:0] BData;
 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  FData<={CRegLen{1'b0}};
  end
 else if (AClkHEn)
  begin
  FData<=BData;
  end
 assign BData = ADataI;
 assign ADataO = FData;
endmodule

module MsDRS ( input AClkH, AResetHN, ASet, input ADataI, output ADataO );
 reg FData; wire BData;
 always @(posedge AClkH or negedge AResetHN or posedge ASet)
 if (AResetHN==1'b0)
  begin
  FData<=1'b0;
  end
 else if (ASet)
  begin
  FData<=1'b1;
  end
 else
  begin
  FData<=BData;
  end
 assign BData = ADataI;
 assign ADataO = FData;
endmodule

module MsMux1b ( input ADataA, input ADataB, output ADataO, input AAddr );
 assign ADataO = AAddr ? ADataB : ADataA;
endmodule

module MsVectMux1a #( parameter CVectLen=32 ) ( input [CVectLen-1:0] ADataA, input [CVectLen-1:0] ADataB, output [CVectLen-1:0] ADataO, input AAddr );
 assign ADataO = AAddr ? ADataB : ADataA;
endmodule

module MsDec2x4a ( input [1:0] ADataI, output [3:0] ADataO );
 assign ADataO = {ADataI==2'h3, ADataI==2'h2, ADataI==2'h1, ADataI==2'h0};
endmodule

module MsDec3x8a ( input [2:0] ADataI, output [7:0] ADataO );
 //assign ADataO = {ADataI==3'h7, ADataI==3'h6, ADataI==3'h5, ADataI==3'h4, ADataI==3'h3, ADataI==3'h2, ADataI==3'h1, ADataI==3'h0};
 wire [ 3:0] BMaskA = {{ 2{ADataI[1]}}, { 2{~ADataI[1]}}} & {2{ADataI[0], ~ADataI[0]}};
 wire [ 7:0] BMaskB = {{ 4{ADataI[2]}}, { 4{~ADataI[2]}}} & {2{BMaskA}};
 assign ADataO = BMaskB;
endmodule

module MsDec3x8e ( input [2:0] ADataI, input AEn, output [7:0] ADataO );
 wire [7:0] BDataO; MsDec3x8a UDataO ( .ADataI(ADataI), .ADataO(BDataO) );
 assign ADataO = {8{AEn}} & BDataO;
endmodule

module MsDec4x16a ( input [3:0] ADataI, output [15:0] ADataO );
 wire [ 3:0] BMaskA = {{ 2{ADataI[1]}}, { 2{~ADataI[1]}}} & {2{ADataI[0], ~ADataI[0]}};
 wire [ 7:0] BMaskB = {{ 4{ADataI[2]}}, { 4{~ADataI[2]}}} & {2{BMaskA}};
 wire [15:0] BMaskC = {{ 8{ADataI[3]}}, { 8{~ADataI[3]}}} & {2{BMaskB}};
 assign ADataO = BMaskC;
endmodule

module MsDec4x16e ( input [3:0] ADataI, input AEn, output [15:0] ADataO );
 wire [15:0] BDataO; MsDec4x16a UDataO ( .ADataI(ADataI), .ADataO(BDataO) );
 assign ADataO = {16{AEn}} & BDataO;
endmodule

module MsDec5x32a ( input [4:0] ADataI, output [31:0] ADataO );
 wire [ 3:0] BMaskA = {{ 2{ADataI[1]}}, { 2{~ADataI[1]}}} & {2{ADataI[0], ~ADataI[0]}};
 wire [ 7:0] BMaskB = {{ 4{ADataI[2]}}, { 4{~ADataI[2]}}} & {2{BMaskA}};
 wire [15:0] BMaskC = {{ 8{ADataI[3]}}, { 8{~ADataI[3]}}} & {2{BMaskB}};
 wire [31:0] BMaskD = {{16{ADataI[4]}}, {16{~ADataI[4]}}} & {2{BMaskC}};
 assign ADataO = BMaskD;
endmodule

module MsEnc4x2a ( input [3:0] ADataI, output [1:0] ADataO );
 assign ADataO =
  {
   |ADataI[3:2],
   |{ADataI[3], ADataI[1]}
  };
endmodule

module MsEnc8x3a ( input [7:0] ADataI, output [2:0] ADataO );
 assign ADataO =
  {
   |ADataI[7:4],
   |{ADataI[7:6], ADataI[3:2]},
   |{ADataI[7], ADataI[5], ADataI[3], ADataI[1]}
  };
endmodule

module MsEnc16x4a ( input [15:0] ADataI, output [3:0] ADataO );
 assign ADataO =
  {
   |{ADataI[15:8]},
   |{ADataI[15:12], ADataI[7:4]},
   |{ADataI[15:14], ADataI[11:10], ADataI[7:6], ADataI[3:2]},
   |{ADataI[15], ADataI[13], ADataI[11], ADataI[9], ADataI[7], ADataI[5], ADataI[3], ADataI[1]}
  };
endmodule

module MsShr32a ( input [31:0] ADataI, input [5:0] AIdx, output [31:0] ADataO );
 wire [31:0] BDataF = AIdx[5] ?  32'h0                 : ADataI;
 wire [31:0] BDataE = AIdx[4] ? {16'h0, BDataF[31:16]} : BDataF;
 wire [31:0] BDataD = AIdx[3] ? { 8'h0, BDataE[31: 8]} : BDataE;
 wire [31:0] BDataC = AIdx[2] ? { 4'h0, BDataD[31: 4]} : BDataD;
 wire [31:0] BDataB = AIdx[1] ? { 2'h0, BDataC[31: 2]} : BDataC;
 wire [31:0] BDataA = AIdx[0] ? { 1'h0, BDataB[31: 1]} : BDataB;
 assign ADataO = BDataA;
endmodule

module MsBuildRorMatr #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, output [CVectLen*CVectLen-1:0] ADataO );
 integer BRowIdx, BColIdx;
 wire [2*CVectLen-1:0] BRotVect = {ADataI[CVectLen-1:0], ADataI[CVectLen-1:0]};
 reg [CVectLen*CVectLen-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CVectLen; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CVectLen; BColIdx=BColIdx+1)
    begin
    BDataO[CVectLen*BRowIdx+BColIdx] = BRotVect[BRowIdx+BColIdx];
    end
   end
  end

 assign ADataO = BDataO;

endmodule

module MsBuildRolMatr #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, output [CVectLen*CVectLen-1:0] ADataO );
 integer BRowIdx, BColIdx;
 wire [2*CVectLen-1:0] BRotVect = {ADataI[CVectLen-1:0], ADataI[CVectLen-1:0]};
 reg [CVectLen*CVectLen-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CVectLen; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CVectLen; BColIdx=BColIdx+1)
    begin
    BDataO[CVectLen*BRowIdx+BColIdx] = BRotVect[CVectLen-BRowIdx+BColIdx];
    end
   end
  end

 assign ADataO = BDataO;

endmodule

module MsSpreadVect #(parameter CVectLen=3, CSpreadLen=0) ( input [CVectLen-1:0] ADataI, output [CVectLen*CSpreadLen-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CVectLen*CSpreadLen-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CVectLen; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CSpreadLen; BColIdx=BColIdx+1)
    begin
    BDataO[CSpreadLen*BRowIdx+BColIdx] = ADataI[BRowIdx];
    end
   end
  end

 assign ADataO = BDataO;
endmodule

module MsDeliverVect #(parameter CRowCnt=2, CColCnt=3) ( input [CColCnt-1:0] ADataI, input [CRowCnt-1:0] AMask, output [CRowCnt*CColCnt-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CRowCnt*CColCnt-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CRowCnt; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CColCnt; BColIdx=BColIdx+1)
    begin
    BDataO[CColCnt*BRowIdx+BColIdx] = ADataI[BColIdx] & AMask[BRowIdx];
    end
   end
  end

 assign ADataO = BDataO;

endmodule

module MsPriorityMaskL #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, output [CVectLen-1:0] ADataO );
 integer BIndex;
 reg [CVectLen-1:0] BDataO;

 always @*
  begin
  BDataO[0]=ADataI[0];
  for (BIndex=1; BIndex<CVectLen; BIndex=BIndex+1)
   begin
   BDataO[BIndex]=ADataI[BIndex]|BDataO[BIndex-1];
   end
  end

 wire [CVectLen:0] BDataAnd = {~BDataO[CVectLen-1:0], 1'b1};
 assign ADataO = ADataI & BDataAnd[CVectLen-1:0];
endmodule

module MsPriorityMaskH #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, output [CVectLen-1:0] ADataO );
 integer BIndex;
 reg [CVectLen-1:0] BDataO;

 always @*
  begin
  BDataO[CVectLen-1]=ADataI[CVectLen-1];
  for (BIndex=(CVectLen-1); BIndex>0; BIndex=BIndex-1)
   begin
   BDataO[BIndex-1]=ADataI[BIndex-1]|BDataO[BIndex];
   end
  end

 wire [CVectLen:0] BDataAnd = {1'b1, ~BDataO[CVectLen-1:0]};
 assign ADataO = ADataI & BDataAnd[CVectLen:1];
endmodule

module MsSelectRow #(parameter CRowCnt=17, CColCnt=8) ( input [CRowCnt*CColCnt-1:0] ADataI, input [CRowCnt-1:0] AMask, output [CColCnt-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CColCnt-1:0] BDataO;

 always @*
  begin
  BDataO = ADataI[CColCnt-1:0] & {CColCnt{AMask[0]}};
  for (BRowIdx=1; BRowIdx<CRowCnt; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CColCnt; BColIdx=BColIdx+1)
    begin
    BDataO[BColIdx] = BDataO[BColIdx] | (ADataI[BRowIdx*CColCnt+BColIdx] & AMask[BRowIdx]);
    end
   end
  end

 assign ADataO = BDataO;
endmodule

module MsRor #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, input [CVectLen-1:0] ASel, output [CVectLen-1:0] ADataO );
 wire [CVectLen*CVectLen-1:0] BRotMatr; MsBuildRorMatr #(.CVectLen(CVectLen)) URotMatr ( .ADataI(ADataI), .ADataO(BRotMatr) );
 MsSelectRow #(.CRowCnt(CVectLen), .CColCnt(CVectLen)) UDataO ( .ADataI(BRotMatr), .AMask(ASel), .ADataO(ADataO) );
endmodule

module MsRol #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, input [CVectLen-1:0] ASel, output [CVectLen-1:0] ADataO );
 wire [CVectLen*CVectLen-1:0] BRotMatr; MsBuildRolMatr #(.CVectLen(CVectLen)) URotMatr ( .ADataI(ADataI), .ADataO(BRotMatr) );
 MsSelectRow #(.CRowCnt(CVectLen), .CColCnt(CVectLen)) UDataO ( .ADataI(BRotMatr), .AMask(ASel), .ADataO(ADataO) );
endmodule

// When DataO is granted, then device
module MsPrioritize #(parameter CLineCnt=0)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CLineCnt-1:0] ADataI, output [CLineCnt-1:0] ADataO
 );

 wire [CLineCnt-1:0] FPrioIdx, BPrioIdx;
 wire [CLineCnt-1:0] FDataO, BDataO;
 MsDffList #(.CRegLen(CLineCnt*2)) UDffList
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BPrioIdx, BDataO}),
   .ADataO({FPrioIdx, FDataO})
  );

 wire BPrioIdxNZ = |FPrioIdx;
 wire BAccessNZ  = |ADataI;

 wire BNoRol = |(ADataI & FDataO); // Stop rolling counter until this request is removed
 wire [CLineCnt:0] BPrioIdxRol = {FPrioIdx, FPrioIdx[CLineCnt-1]}; // Avoiding synthesis issue when CLineCnt=1. We cannot do {FPrioIdx[CLineCnt-2:0], FPrioIdx[CLineCnt-1]} due to "CLineCnt-2"
 wire [CLineCnt:0] BPrioIdxE   = {{CLineCnt{1'b0}}, 1'b1};
 assign BPrioIdx = BPrioIdxNZ ? ((BAccessNZ & ~BNoRol) ? BPrioIdxRol[CLineCnt-1:0] : FPrioIdx) : BPrioIdxE[CLineCnt-1:0];

 wire [CLineCnt-1:0] BAccessRor; MsRor #(.CVectLen(CLineCnt)) UAccessRor ( .ADataI(ADataI), .ASel(FPrioIdx), .ADataO(BAccessRor) );
 wire [CLineCnt-1:0] BAccessMasked; MsPriorityMaskL #(.CVectLen(CLineCnt)) UAccessMasked ( .ADataI(BAccessRor), .ADataO(BAccessMasked) );
 MsRol #(.CVectLen(CLineCnt)) UDataO ( .ADataI(BAccessMasked), .ASel(FPrioIdx), .ADataO(BDataO) );

 assign ADataO = BDataO;
endmodule

module MsItemCmp #(parameter CItemLen=4) ( input [CItemLen-1:0] AItem, input [CItemLen-1:0] ACmp, output AResult );
  assign AResult = (AItem==ACmp);
endmodule

// OR by rows
module MsMatrOrRow #(parameter CRowCnt=2, CColCnt=4) ( input [CRowCnt*CColCnt-1:0] ADataI, output [CRowCnt-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CRowCnt-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CRowCnt; BRowIdx=BRowIdx+1)
   begin
   BDataO[BRowIdx] = ADataI[BRowIdx*CColCnt+0];
   for (BColIdx=1; BColIdx<CColCnt; BColIdx=BColIdx+1)
    begin
    BDataO[BRowIdx] = BDataO[BRowIdx] | ADataI[BRowIdx*CColCnt+BColIdx];
    end
   end
  end

 assign ADataO = BDataO;

endmodule

// OR by columns
module MsMatrOrCol #(parameter CRowCnt=4, CColCnt=4) ( input [CRowCnt*CColCnt-1:0] ADataI, output [CColCnt-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CColCnt-1:0] BDataO;

 always @*
  begin
  BDataO = ADataI[CColCnt-1:0];
  for (BRowIdx=1; BRowIdx<CRowCnt; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CColCnt; BColIdx=BColIdx+1)
    begin
    BDataO[BColIdx] = BDataO[BColIdx] | ADataI[BRowIdx*CColCnt+BColIdx];
    end
   end
  end

 assign ADataO = BDataO;
endmodule

// Transpose Matr
module MsMatrTrans #(parameter CRowCnt=4, CColCnt=4) ( input [(CRowCnt*CColCnt)-1:0] ADataI, output [(CColCnt*CRowCnt)-1:0] ADataO );
 integer BRowIdx, BColIdx;
 reg [CColCnt*CRowCnt-1:0] BDataO;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CRowCnt; BRowIdx=BRowIdx+1)
   begin
   for (BColIdx=0; BColIdx<CColCnt; BColIdx=BColIdx+1)
    begin
    BDataO[BColIdx*CRowCnt+BRowIdx] = ADataI[BRowIdx*CColCnt+BColIdx];
    end
   end
  end

 assign ADataO = BDataO;
endmodule

module MsTmpCompA #(parameter CLenI=10, CLenO=20)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CLenI-1:0] ADataI, output [CLenO-1:0] ADataO
 );

 wire [CLenO-1:0] FDataS, BDataS;
 MsDffList #(.CRegLen(CLenO)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BDataS),
   .ADataO(FDataS)
  );

 assign BDataS = {FDataS[CLenO-2:0], 1'b0} ^ {{(CLenO-CLenI){1'b0}}, ADataI};
 assign ADataO = FDataS;
endmodule

module MsTmpCompB #(parameter CLenI=25, CLenO=10)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CLenI-1:0] ADataI, output [CLenO-1:0] ADataO
 );

 wire [CLenI-1:0] FDataS, BDataS;
 MsDffList #(.CRegLen(CLenI)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI(BDataS),
   .ADataO(FDataS)
  );

 assign BDataS = {FDataS[CLenO-2:0], 1'b0} ^ ADataI;
 assign ADataO = FDataS[CLenI-1:CLenI-CLenO];
endmodule

module MsVectCmpAE #(parameter CItemCnt=8, CItemLen=4) ( input [CItemCnt*CItemLen-1:0] AItemList, input [CItemLen-1:0] ACmp, output [CItemCnt-1:0] AResult );
  //OzhItemCmp #(.CItemLen(CItemLen)) UResult[CItemCnt-1:0] ( .AItem(AItemList), .ACmp({CItemCnt{ACmp}}), .AResult(AResult) );
 integer BRowIdx, BColIdx;
 reg [CItemCnt-1:0] BResult;

 always @*
  begin
  for (BRowIdx=0; BRowIdx<CItemCnt; BRowIdx=BRowIdx+1)
   begin
   BResult[BRowIdx] = (AItemList[BRowIdx*CItemLen+0]==ACmp[0]);
   for (BColIdx=1; BColIdx<CItemLen; BColIdx=BColIdx+1)
    begin
    BResult[BRowIdx] = BResult[BRowIdx] & (AItemList[BRowIdx*CItemLen+BColIdx]<ACmp[BColIdx]);
    end
   end
  end

 assign AResult = BResult;
endmodule

module MsCrossSync
 (
  input AClkA, AResetAN, AClkAEn,
  input AReqA,
  input AClkB, AResetBN, AClkBEn,
  output AAckB
 );

 reg FSyncA; wire BSyncA;
 reg FSyncB;
 reg FSyncC;

 wire BResetAN = AResetAN & ~FSyncC;
 always @(posedge AClkA or negedge BResetAN)
 if (BResetAN==1'b0)
  begin
  FSyncA<=1'b0;
  end
 else if (AClkAEn)
  begin
  FSyncA<=BSyncA;
  end

 assign BSyncA = AReqA | FSyncA;

 wire BResetBN = AResetBN & ~FSyncC;
 always @(posedge AClkB or negedge BResetBN)
 if (BResetBN==1'b0)
  begin
  FSyncB<=1'b0;
  end
 else if (AClkBEn)
  begin
  FSyncB<=FSyncA;
  end

 always @(posedge AClkB or negedge AResetBN)
 if (AResetBN==1'b0)
  begin
  FSyncC<=1'b0;
  end
 else if (AClkBEn)
  begin
  FSyncC<=FSyncB;
  end

 assign AAckB = FSyncC;

endmodule

module MsMuxAny #(parameter CLenFinal = 8, CAddrLen = 3)
 (
  input [CLenFinal*(1<<CAddrLen)-1:0] ADataI,
  input [CAddrLen-1:0] AAddr,
  output [CLenFinal-1:0] ADataO
 );

 localparam CLenHalf = CLenFinal * (1<<(CAddrLen-1));
 wire [CLenHalf-1:0] BData = AAddr[CAddrLen-1] ? ADataI[CLenHalf*2-1:CLenHalf] : ADataI[CLenHalf-1:0];

 generate
 if (CAddrLen==1)
  begin
  assign ADataO = BData;
  end
 else
  begin
  MsMuxAny #(.CLenFinal(CLenFinal), .CAddrLen(CAddrLen-1)) UMuxA ( .ADataI(BData), .AAddr(AAddr[CAddrLen-2:0]), .ADataO(ADataO) );
  end
 endgenerate

endmodule

module MsDecAny #(parameter CAddrLen = 3)
 (
  input [CAddrLen-1:0] AAddr,
  output [(1<<CAddrLen)-1:0] ADataO
 );

 localparam CLenHalf = 1<<(CAddrLen-1);
 wire [CLenHalf-1:0] BData;

 generate
 if (CAddrLen==1)
  begin
  assign BData = 1'b1;
  end
 else
  begin
  MsDecAny #(.CAddrLen(CAddrLen-1)) UDecA ( .AAddr(AAddr[CAddrLen-2:0]), .ADataO(BData) );
  end
 endgenerate

 assign ADataO = AAddr[CAddrLen-1] ? {BData, {CLenHalf{1'b0}}} : {{CLenHalf{1'b0}}, BData};

endmodule

module MsOrVectL #(parameter CVectLen=3) ( input [CVectLen-1:0] ADataI, output [CVectLen-1:0] ADataO );
 integer BIndex;
 reg [CVectLen-1:0] BDataO;

 always @*
  begin
  BDataO[0]=ADataI[0];
  for (BIndex=1; BIndex<CVectLen; BIndex=BIndex+1)
   begin
   BDataO[BIndex]=ADataI[BIndex]|BDataO[BIndex-1];
   end
  end

 assign ADataO = BDataO;
endmodule


module MsBufReg #(parameter CDataWidth=32)
 (
  input AClkH, input AResetHN, input AClkHEn, 
  input [(CDataWidth-1):0] AMosi, input AWrEn, output [(CDataWidth-1):0] ARegData
 );

 wire [(CDataWidth-1):0] FRegData, BRegData;
 MsDffList #(.CRegLen(CDataWidth)) URegData
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(BRegData),
   .ADataO(FRegData)
  );

 assign BRegData = AWrEn ? AMosi : FRegData;
 assign ARegData = FRegData;
endmodule


module MsCrossData #(parameter CRegLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn, 
  input [CRegLen-1:0] ADataI, output [CRegLen-1:0] ADataO
 );


 MsDffList #(.CRegLen(1)) ULocalVars [CRegLen-1:0]
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(ADataI), .ADataO(ADataO)
  );

endmodule

module MdCrossSync
 (
  input AClkH, input AResetHN, input AClkHEn, 
  input AClkS, input AResetSN,
  input AReqS, output AAckH
 );

 wire FReqS, BReqS;
 wire [1:0] FAckH, BAckH;

 MsDffList #(.CRegLen(1)) ULocalVarsS
  (
   .AClkH(AClkS), .AResetHN(AResetSN & ~FAckH[1]), .AClkHEn(1'b1), 
   .ADataI(BReqS),
   .ADataO(FReqS)
  );

 MsDffList #(.CRegLen(2)) ULocalVarsH
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(BAckH),
   .ADataO(FAckH)
  );

 wire BAckHNZ = |FAckH;
 assign BReqS = AReqS | FReqS;
 assign BAckH =
  {
   FAckH[0],
   FReqS & ~BAckHNZ
  };

 assign AAckH = FAckH[1];
endmodule

module MsCrossDS #(parameter CRegLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn, 
  input AClkS, input AResetSN,
  input [CRegLen-1:0] ADataI, output [CRegLen-1:0] ADataO,
  input AReqS, output AAckH
 );

 wire [CRegLen-1:0] FDataS, BDataS;

 MsDffList #(.CRegLen(CRegLen)) ULocalVarsS
  (
   .AClkH(AClkS), .AResetHN(AResetSN), .AClkHEn(1'b1), 
   .ADataI(BDataS),
   .ADataO(FDataS)
  );

 assign BDataS = AReqS ? ADataI : FDataS;

 MsCrossData #(.CRegLen(CRegLen)) UData
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(FDataS), .ADataO(ADataO)
  );

 MsCrossSync USync
  (
   .AClkA(AClkS), .AResetAN(AResetSN), .AClkAEn(1'b1),
   .AReqA(AReqS),
   .AClkB(AClkH), .AResetBN(AResetHN), .AClkBEn(AClkHEn),
   .AAckB(AAckH)
  );

endmodule

module DpCrossData
 (
  AClkB, AResetBN, AClkBEn,
  ADataA, ADataB
 );

 input AClkB, AResetBN, AClkBEn;
 input ADataA;
 output ADataB;

 reg FData;

 always @(posedge AClkB or negedge AResetBN)
 if (AResetBN==1'b0)
  begin
  FData<=1'b0;
  end
 else if (AClkBEn)
  begin
  FData<=ADataA;
  end

 assign ADataB = FData;

endmodule

module DpCrossDataX
 (
  AClkB, AResetBN, AClkBEn,
  ADataA, ADataB
 );

 parameter CDataWidth = 32;

 input AClkB, AResetBN, AClkBEn;
 input [CDataWidth-1:0] ADataA;
 output [CDataWidth-1:0] ADataB;

 DpCrossData UCross [CDataWidth-1:0] (.AClkB({CDataWidth{AClkB}}), .AResetBN({CDataWidth{AResetBN}}), .AClkBEn({CDataWidth{AClkBEn}}), .ADataA(ADataA), .ADataB(ADataB) );

endmodule

module MdClkEn
 (
  input AClkH, input AResetHN, input AClkHEn, 
  input AClkEn, output AClkO
 );

 wire FClkA, BClkA;
 wire FClkB, BClkB;
 wire FClkEn, BClkEn;

 MsDffList #(.CRegLen(1)) UClkA
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI(BClkA), .ADataO(FClkA)
  );

 MsDffList #(.CRegLen(1+1)) UClkB
  (
   .AClkH(~AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn), 
   .ADataI({BClkB, BClkEn}), .ADataO({FClkB, FClkEn})
  );

 assign BClkA = FClkA ^ FClkEn;
 assign BClkB = FClkA;
 assign BClkEn = AClkEn;

 assign AClkO = FClkA ^ FClkB;
endmodule

module DpCrossSync
 (
  AClkA, AResetAN, AClkAEn,
  AReqA,
  AClkB, AResetBN, AClkBEn,
  AAckB
 );

 input AClkA, AResetAN, AClkAEn;
 input AReqA;
 input AClkB, AResetBN, AClkBEn;
 output AAckB;

 reg FSyncA; wire BSyncA;
 reg FSyncB;
 reg FSyncC;

 wire BResetAN = AResetAN & ~FSyncC;
 always @(posedge AClkA or negedge BResetAN)
 if (BResetAN==1'b0)
  begin
  FSyncA<=1'b0;
  end
 else if (AClkAEn)
  begin
  FSyncA<=BSyncA;
  end

 assign BSyncA = AReqA | FSyncA;

 wire BResetBN = AResetBN & ~FSyncC;
 always @(posedge AClkB or negedge BResetBN)
 if (BResetBN==1'b0)
  begin
  FSyncB<=1'b0;
  end
 else if (AClkBEn)
  begin
  FSyncB<=FSyncA;
  end

 always @(posedge AClkB or negedge AResetBN)
 if (AResetBN==1'b0)
  begin
  FSyncC<=1'b0;
  end
 else if (AClkBEn)
  begin
  FSyncC<=FSyncB;
  end

 assign AAckB = FSyncC;

endmodule

module DpCrossDataAndSync
 (
  AClkA, AResetAN, AClkAEn,
  AReqA, ADataA,
  AClkB, AResetBN, AClkBEn,
  AAckB, ADataB
 );

 parameter CDataWidth = 8;

 input AClkA, AResetAN, AClkAEn;
 input AReqA; input [(CDataWidth-1):0] ADataA;
 input AClkB, AResetBN, AClkBEn;
 output AAckB; output [(CDataWidth-1):0] ADataB;

 reg [(CDataWidth-1):0] FDataA; wire [(CDataWidth-1):0] BDataA;

 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  FDataA<={CDataWidth{1'b0}};
  end
 else if (AClkAEn)
  begin
  FDataA<=BDataA;
  end

 assign BDataA = AReqA ? ADataA : FDataA;

 DpCrossData UCrossData[(CDataWidth-1):0] (.AClkB({CDataWidth{AClkB}}), .AResetBN({CDataWidth{AResetBN}}), .AClkBEn({CDataWidth{AClkBEn}}), .ADataA(FDataA), .ADataB(ADataB));
 DpCrossSync UCrossSync (.AClkA(AClkA), .AResetAN(AResetAN), .AClkAEn(AClkAEn), .AReqA(AReqA), .AClkB(AClkB), .AResetBN(AResetBN), .AClkBEn(AClkBEn), .AAckB(AAckB));

endmodule

module MsIdxOf32a ( input [31:0] ADataI, output [4:0] ADataO );
 assign ADataO = (ADataI[31] ? 5'h1F : 5'h0) | (ADataI[30] ? 5'h1E : 5'h0) |
                 (ADataI[29] ? 5'h1D : 5'h0) | (ADataI[28] ? 5'h1C : 5'h0) | (ADataI[27] ? 5'h1B : 5'h0) | (ADataI[26] ? 5'h1A : 5'h0) | (ADataI[25] ? 5'h19 : 5'h0) |
                 (ADataI[24] ? 5'h18 : 5'h0) | (ADataI[23] ? 5'h17 : 5'h0) | (ADataI[22] ? 5'h16 : 5'h0) | (ADataI[21] ? 5'h15 : 5'h0) | (ADataI[20] ? 5'h14 : 5'h0) |
                 (ADataI[19] ? 5'h13 : 5'h0) | (ADataI[18] ? 5'h12 : 5'h0) | (ADataI[17] ? 5'h11 : 5'h0) | (ADataI[16] ? 5'h10 : 5'h0) | (ADataI[15] ? 5'h0F : 5'h0) |
                 (ADataI[14] ? 5'h0E : 5'h0) | (ADataI[13] ? 5'h0D : 5'h0) | (ADataI[12] ? 5'h0C : 5'h0) | (ADataI[11] ? 5'h0B : 5'h0) | (ADataI[10] ? 5'h0A : 5'h0) |
                 (ADataI[ 9] ? 5'h09 : 5'h0) | (ADataI[ 8] ? 5'h09 : 5'h0) | (ADataI[ 7] ? 5'h07 : 5'h0) | (ADataI[ 6] ? 5'h06 : 5'h0) | (ADataI[ 5] ? 5'h05 : 5'h0) |
                 (ADataI[ 4] ? 5'h04 : 5'h0) | (ADataI[ 3] ? 5'h03 : 5'h0) | (ADataI[ 2] ? 5'h02 : 5'h0) | (ADataI[ 1] ? 5'h01 : 5'h0) | (ADataI[ 0] ? 5'h00 : 5'h0);
endmodule

// Cross data and sync. Executed asynchronously by rising edge
module MsCrossDS_Async #(parameter CRegLen=32)
 (
  input AClkH, AResetHN, AClkHEn,
  input AEventRE, // By RisingEdge
  input AEventEn,
  output AEventAck,
  input [CRegLen-1:0] ADataI, output [CRegLen-1:0] ADataO
 );

 wire [2:0] FEventB, BEventB;
 wire [CRegLen-1:0] FData, BData;
 wire [CRegLen-1:0] FDataExt, BDataExt;
 MsDffList #(.CRegLen(3+CRegLen+CRegLen)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BEventB, BData, BDataExt}), .ADataO({FEventB, FData, FDataExt})
  );

 assign BDataExt = ADataI;

 wire FEventA, BEventA;
 MsDffList #(.CRegLen(1)) UEventA
  (
   .AClkH(AEventRE), .AResetHN(AResetHN & ~FEventB[1]), .AClkHEn(AClkHEn),
   .ADataI(BEventA), .ADataO(FEventA)
  );

 assign BEventA = 1'b1;

 wire BEventBNZ = |FEventB;
 assign BEventB = {FEventB[1:0], FEventA & (~BEventBNZ)};

 assign BData = FEventB[1] ? FDataExt : FData;

 assign AEventAck = FEventB[2];
 assign ADataO = FData;
endmodule

module MsAntiGlitch #(parameter CRegLen=32)
 (
  input AClkH, AResetHN, AClkHEn,
  input ADataI, output ADataO
 );

 wire [CRegLen-1:0] FDataS, BDataS;
 wire FDataO, BDataO;
 MsDffList #(.CRegLen(CRegLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BDataS, BDataO}), .ADataO({FDataS, FDataO})
  );

 assign BDataS = {FDataS[CRegLen-2:0], ADataI};

 wire BDataS_NZ = |FDataS;
 wire BDataS_E  = &FDataS;
 assign BDataO = BDataS_E | (BDataS_NZ ? FDataO : 1'b0);

 assign ADataO = FDataO;
endmodule




