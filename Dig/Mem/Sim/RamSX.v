module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddr, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn, ARdEn
 );

 localparam CNumWords = (1<<CAddrLen);

 // Local variables
 reg [CAddrLen-1:0] FAddr; wire [CAddrLen-1:0] BAddr;
 reg [CDataLen-1:0] FMosi;
 reg FWrEn;
 reg FRdEn;

 reg [CDataLen-1:0] FMem [CNumWords-1:0];
 wire [CDataLen-1:0] BMemA;
 wire [CDataLen-1:0] BMemB;

 // Implementation

 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  FAddr<={CAddrLen{1'b0}};
  FMosi<={CDataLen{1'b0}};
  FWrEn<=1'h0;
  FRdEn<=1'b0;
  end
 else if (AClkHEn)
  begin
  FAddr<=BAddr;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  FRdEn<=ARdEn;
  end

 wire BAccessAny = |{AWrEn, ARdEn};
 assign BAddr = BAccessAny ? AAddr : FAddr;

 assign BMemA = FMem[FAddr];

 assign BMemB = FWrEn ? FMosi : BMemA;

 integer BIndex;
 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==0)
  begin
  for (BIndex=0; BIndex<CNumWords; BIndex=BIndex+1) FMem[BIndex]<={CDataLen{1'b0}};
  end
 else if (AClkHEn)
  begin
  FMem[FAddr]<=BMemB;
  end

 assign AMiso = FRdEn ? BMemA : {CDataLen{1'b0}};
endmodule

