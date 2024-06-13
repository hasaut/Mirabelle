module RamDX #(parameter CAddrLen=11, CDataLen=8)
 (
  input wire AClkA, input wire AResetAN, input wire AClkAEn,
  input wire [CAddrLen-1:0] AAddrA, input wire [CDataLen-1:0] AMosiA, output wire [CDataLen-1:0] AMisoA, input wire AWrEnA, input wire ARdEnA,
  input wire AClkB, input wire AResetBN, input wire AClkBEn,
  input wire [CAddrLen-1:0] AAddrB, input wire [CDataLen-1:0] AMosiB, output wire [CDataLen-1:0] AMisoB, input wire AWrEnB, input wire ARdEnB
 );

 localparam CDataZ = {CDataLen{1'b0}};
 localparam CAddrZ = {CAddrLen{1'b0}};
 localparam CWordCnt = (1<<CAddrLen);

 // Local variables
 reg [CAddrLen-1:0] FAddrA; reg [CDataLen-1:0] FMosiA; reg FWrEnA, FRdEnA;
 reg [CAddrLen-1:0] FAddrB; reg [CDataLen-1:0] FMosiB; reg FWrEnB, FRdEnB;

 reg [CWordCnt-1:0] FMem [CDataLen-1:0];
 wire [CDataLen-1:0] BMemRdA;
 wire [CDataLen-1:0] BMemRdB;

 // Implementation

 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  FAddrA<=CAddrZ; FMosiA<=CDataZ; FWrEnA<=1'h0; FRdEnA<=1'b0;
  end
 else if (AClkAEn)
  begin
  FAddrA<=AAddrA; FMosiA<=AMosiA; FWrEnA<=AWrEnA; FRdEnA<=ARdEnA;
  end

 always @(posedge AClkB or negedge AResetBN)
 if (AResetBN==1'b0)
  begin
  FAddrB<=CAddrZ; FMosiB<=CDataZ; FWrEnB<=1'h0; FRdEnB<=1'b0;
  end
 else if (AClkBEn)
  begin
  FAddrB<=AAddrB; FMosiB<=AMosiB; FWrEnB<=AWrEnB; FRdEnB<=ARdEnB;
  end

 assign BMemRdA = FMem[FAddrA];
 assign BMemRdB = FMem[FAddrB];
 
 always @(AClkA or AClkB or FAddrA or FAddrB or FWrEnA or FWrEnB or FMosiA or FMosiB)
  begin
  if ((AClkA==1'b0) & FWrEnA) FMem[FAddrA]=FMosiA;
  if ((AClkB==1'b0) & FWrEnB) FMem[FAddrB]=FMosiB;
  end

 /*
 genvar BIndex;
 for (BIndex=0; BIndex<CWordCnt; BIndex=BIndex+1)
  begin
  always @(FMem[BIndex] or AResetAN or AResetBN or AClkA or AClkB or FWrEnA or FWrEnB or FAddrA or FAddrB or FMosiA or FMosiB)
   begin
   if ((AResetAN==1'b0) || (AResetBN==1'b0))
    begin
    FMem[BIndex]=CDataZ;
    end
   else
    begin
    if ((AClkA==1'b0) & FWrEnA & (BIndex==FAddrA)) FMem[BIndex]=FMosiA;
    if ((AClkB==1'b0) & FWrEnB & (BIndex==FAddrB)) FMem[BIndex]=FMosiB;
    end
   end
  end
 */
 
 assign AMisoA=FRdEnA ? BMemRdA : CDataZ;
 assign AMisoB=FRdEnB ? BMemRdB : CDataZ;

endmodule

