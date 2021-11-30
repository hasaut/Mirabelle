module Ram8a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

// Interface
input AClkH, AResetB, AClkHEn;
input [7:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

// Local variables
reg [7:0] FAddrWr, FAddrRd;
reg [7:0] FMosi;
reg FWrEn;

reg [7:0] FMem [255:0];
wire [7:0] BMemA;
reg [7:0] BMemB;

// body

always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 FAddrWr<=8'h00;
 FAddrRd<=8'h00;
 FMosi<=8'h0;
 FWrEn<=1'h0;
 end
else if (AClkHEn)
 begin
 FAddrWr<=AAddrWr;
 FAddrRd<=AAddrRd;
 FMosi<=AMosi;
 FWrEn<=AWrEn;
 end

assign BMemA=FMem[FAddrWr];

always @(BMemA or FWrEn or FMosi)
 begin
 BMemB=BMemA;
 if (FWrEn) BMemB[ 7: 0]=FMosi[ 7: 0];
 end


integer BIndex;
always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
 end
else if (AClkHEn)
 begin
 FMem[FAddrWr]<=BMemB;
 end

assign AMiso=FMem[FAddrRd];

endmodule

module Ram8a9d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

 // Interface
 input AClkH, AResetB, AClkHEn;
 input [7:0] AAddrWr, AAddrRd;
 input [8:0] AMosi; output [8:0] AMiso;
 input AWrEn;

 // Local variables
 reg [7:0] FAddrWr, FAddrRd;
 reg [8:0] FMosi;
 reg FWrEn;

 reg [8:0] FMem [255:0];
 wire [8:0] BMemA;
 reg [8:0] BMemB;

 // Implementation

 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  FAddrWr<=8'h00;
  FAddrRd<=8'h00;
  FMosi<=9'h0;
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB[ 8: 0]=FMosi[ 8: 0];
  end


 integer BIndex;
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<=9'h0;
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];

endmodule

module Ram10a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

// Interface
input AClkH, AResetB, AClkHEn;
input [9:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

// Local variables
reg [9:0] FAddrWr, FAddrRd;
reg [7:0] FMosi;
reg FWrEn;

reg [7:0] FMem [1023:0];
wire [7:0] BMemA;
reg [7:0] BMemB;

// body

always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 FAddrWr<=10'h000;
 FAddrRd<=10'h000;
 FMosi<=8'h0;
 FWrEn<=1'h0;
 end
else if (AClkHEn)
 begin
 FAddrWr<=AAddrWr;
 FAddrRd<=AAddrRd;
 FMosi<=AMosi;
 FWrEn<=AWrEn;
 end

assign BMemA=FMem[FAddrWr];

always @(BMemA or FWrEn or FMosi)
 begin
 BMemB=BMemA;
 if (FWrEn) BMemB[ 7: 0]=FMosi[ 7: 0];
 end


integer BIndex;
always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 for (BIndex=0; BIndex<1024; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
 end
else if (AClkHEn)
 begin
 FMem[FAddrWr]<=BMemB;
 end

assign AMiso=FMem[FAddrRd];

endmodule


module Ram12a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

// Interface
input AClkH, AResetB, AClkHEn;
input [11:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

// Local variables
reg [11:0] FAddrWr, FAddrRd;
reg [7:0] FMosi;
reg FWrEn;

reg [7:0] FMem [4095:0];
wire [7:0] BMemA;
reg [7:0] BMemB;

// body

always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 FAddrWr<=12'h000;
 FAddrRd<=12'h000;
 FMosi<=8'h0;
 FWrEn<=1'h0;
 end
else if (AClkHEn)
 begin
 FAddrWr<=AAddrWr;
 FAddrRd<=AAddrRd;
 FMosi<=AMosi;
 FWrEn<=AWrEn;
 end

assign BMemA=FMem[FAddrWr];

always @(BMemA or FWrEn or FMosi)
 begin
 BMemB=BMemA;
 if (FWrEn) BMemB[ 7: 0]=FMosi[ 7: 0];
 end


integer BIndex;
always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 for (BIndex=0; BIndex<4096; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
 end
else if (AClkHEn)
 begin
 FMem[FAddrWr]<=BMemB;
 end

assign AMiso=FMem[FAddrRd];

endmodule


module Ram10a9d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

 // Interface
 input AClkH, AResetB, AClkHEn;
 input [9:0] AAddrWr, AAddrRd;
 input [8:0] AMosi; output [8:0] AMiso;
 input AWrEn;

 // Local variables
 reg [9:0] FAddrWr, FAddrRd;
 reg [8:0] FMosi;
 reg FWrEn;

 reg [8:0] FMem [1023:0];
 wire [8:0] BMemA;
 reg [8:0] BMemB;

 // Implementation

 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  FAddrWr<=10'h00;
  FAddrRd<=10'h00;
  FMosi<=9'h0;
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB[ 8: 0]=FMosi[ 8: 0];
  end


 integer BIndex;
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  for (BIndex=0; BIndex<1024; BIndex=BIndex+1) FMem[BIndex]<=9'h0;
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];

endmodule


module Ram8a25d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

 // Interface
 input AClkH, AResetB, AClkHEn;
 input [7:0] AAddrWr, AAddrRd;
 input [24:0] AMosi; output [24:0] AMiso;
 input AWrEn;

 // Local variables
 reg [7:0] FAddrWr, FAddrRd;
 reg [24:0] FMosi;
 reg FWrEn;

 reg [24:0] FMem [255:0];
 wire [24:0] BMemA;
 reg [24:0] BMemB;

 // body

 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  FAddrWr<=8'h00;
  FAddrRd<=8'h00;
  FMosi<=25'h0;
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB[24:0]=FMosi[24:0];
  end


 integer BIndex;
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==0)
  begin
  for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<=25'h0;
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];

endmodule

module Ram8a64d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

// Interface
input AClkH, AResetB, AClkHEn;
input [7:0] AAddrWr, AAddrRd;
input [63:0] AMosi; output [63:0] AMiso;
input AWrEn;

// Local variables
reg [7:0] FAddrWr, FAddrRd;
reg [63:0] FMosi;
reg FWrEn;

reg [63:0] FMem [255:0];
wire [63:0] BMemA;
reg [63:0] BMemB;

// body

always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 FAddrWr<=8'h00;
 FAddrRd<=8'h00;
 FMosi<=64'h0;
 FWrEn<=1'h0;
 end
else if (AClkHEn)
 begin
 FAddrWr<=AAddrWr;
 FAddrRd<=AAddrRd;
 FMosi<=AMosi;
 FWrEn<=AWrEn;
 end

assign BMemA=FMem[FAddrWr];

always @(BMemA or FWrEn or FMosi)
 begin
 BMemB=BMemA;
 if (FWrEn) BMemB[63:0]=FMosi[63:0];
 end


integer BIndex;
always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
 end
else if (AClkHEn)
 begin
 FMem[FAddrWr]<=BMemB;
 end

assign AMiso=FMem[FAddrRd];

endmodule

module Ram8a105d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

// Interface
input AClkH, AResetB, AClkHEn;
input [7:0] AAddrWr, AAddrRd;
input [104:0] AMosi; output [104:0] AMiso;
input AWrEn;

// Local variables
reg [7:0] FAddrWr, FAddrRd;
reg [104:0] FMosi;
reg FWrEn;

reg [104:0] FMem [255:0];
wire [104:0] BMemA;
reg [104:0] BMemB;

// body

always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 FAddrWr<=8'h00;
 FAddrRd<=8'h00;
 FMosi<=105'h0;
 FWrEn<=1'h0;
 end
else if (AClkHEn)
 begin
 FAddrWr<=AAddrWr;
 FAddrRd<=AAddrRd;
 FMosi<=AMosi;
 FWrEn<=AWrEn;
 end

assign BMemA=FMem[FAddrWr];

always @(BMemA or FWrEn or FMosi)
 begin
 BMemB=BMemA;
 if (FWrEn) BMemB[104:0]=FMosi[104:0];
 end


integer BIndex;
always @(posedge AClkH or negedge AResetB)
if (AResetB==0)
 begin
 for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
 end
else if (AClkHEn)
 begin
 FMem[FAddrWr]<=BMemB;
 end

assign AMiso=FMem[FAddrRd];

endmodule

module Ram16a8d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [15:0] AAddrWr, input [15:0] AAddrRd,
  input [7:0] AMosi, output [7:0] AMiso,
  input AWrEn
 );

 reg [7:0] FAddrWr, FAddrRd;
 reg [7:0] FMosi;
 reg FWrEn;

 reg [7:0] FMem [65535:0];
 wire [7:0] BMemA;
 reg [7:0] BMemB;

 always @(posedge AClkH or negedge AResetB)
 if (AResetB==1'b0)
  begin
  FAddrWr<=16'h00;
  FAddrRd<=16'h00;
  FMosi<=8'h0;
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB[ 7: 0]=FMosi[ 7: 0];
  end

 integer BIndex;
 always @(posedge AClkH or negedge AResetB)
 if (AResetB==1'b0)
  begin
  for (BIndex=0; BIndex<65536; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];
endmodule

module RamScope12a128d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [11:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [11:0] AAddrBRd, output [127:0] AMisoB
 );

 wire [11:0] FAddrBRd;
 wire [127:0] FMisoB, BMisoB;
 OzhDffList #(.CRegLen(12+128)) ULocalVars
  (
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn), .AScanI(1'b0), .AScanO(), .AScanE(1'b0),
   .ADataI({AAddrBRd, BMisoB}),
   .ADataO({FAddrBRd, FMisoB})
  );

 reg [127:0] FMem [4096-1:0];

 wire [127:0] BMemWrData = AWrEn ? AMosiA : FMem[AAddrAWr];

 integer BIndex;
 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  for (BIndex=0; BIndex<4096; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
  end
 else if (AClkAEn)
  begin
  FMem[AAddrAWr]<=BMemWrData;
  end

 assign AMisoB = FMem[FAddrBRd];
endmodule

module RamScope13a128d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [12:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [12:0] AAddrBRd, output [127:0] AMisoB
 );

 wire [12:0] FAddrBRd;
 wire [127:0] FMisoB, BMisoB;
 OzhDffList #(.CRegLen(13+128)) ULocalVars
  (
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn), .AScanI(1'b0), .AScanO(), .AScanE(1'b0),
   .ADataI({AAddrBRd, BMisoB}),
   .ADataO({FAddrBRd, FMisoB})
  );

 reg [127:0] FMem [8192-1:0];

 wire [127:0] BMemWrData = AWrEn ? AMosiA : FMem[AAddrAWr];

 integer BIndex;
 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  for (BIndex=0; BIndex<8192; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
  end
 else if (AClkAEn)
  begin
  FMem[AAddrAWr]<=BMemWrData;
  end

 assign AMisoB = FMem[FAddrBRd];
endmodule

module RamScope128d #(parameter CAddrLen=12)
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [CAddrLen-1:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [CAddrLen-1:0] AAddrBRd, output [127:0] AMisoB
 );

 localparam CNumWords = (1<<CAddrLen);

 wire [CAddrLen-1:0] FAddrBRd;
 wire [127:0] FMisoB, BMisoB;
 OzhDffList #(.CRegLen(CAddrLen+128)) ULocalVars
  (
   .AClkH(AClkB), .AResetHN(AResetBN), .AClkHEn(AClkBEn), .AScanI(1'b0), .AScanO(), .AScanE(1'b0),
   .ADataI({AAddrBRd, BMisoB}),
   .ADataO({FAddrBRd, FMisoB})
  );

 reg [127:0] FMem [CNumWords-1:0];

 wire [127:0] BMemWrData = AWrEn ? AMosiA : FMem[AAddrAWr];

 integer BIndex;
 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  for (BIndex=0; BIndex<CNumWords; BIndex=BIndex+1) FMem[BIndex]<=8'h0;
  end
 else if (AClkAEn)
  begin
  FMem[AAddrAWr]<=BMemWrData;
  end

 assign AMisoB = FMem[FAddrBRd];
endmodule

module Ram8aXd2 #(parameter CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso,
  input AWrEn
 );

 // Local variables
 reg [7:0] FAddrWr, FAddrRd;
 reg [CDataLen-1:0] FMosi;
 reg FWrEn;

 reg [CDataLen-1:0] FMem [255:0];
 wire [CDataLen-1:0] BMemA;
 reg [CDataLen-1:0] BMemB;

 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  FAddrWr<=8'h00;
  FAddrRd<=8'h00;
  FMosi<={CDataLen{1'h0}};
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB=FMosi;
  end


 integer BIndex;
 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  for (BIndex=0; BIndex<256; BIndex=BIndex+1) FMem[BIndex]<={CDataLen{1'b0}};
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];

endmodule


module RamXaXd2 #(parameter CAddrLen=8, CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddrWr, input [CAddrLen-1:0] AAddrRd,
  input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso,
  input AWrEn
 );

 localparam CRamSize = (1<<CAddrLen);
 localparam CAddrNil = {CAddrLen{1'b0}};

 // Local variables
 reg [CAddrLen-1:0] FAddrWr, FAddrRd;
 reg [CDataLen-1:0] FMosi;
 reg FWrEn;

 reg [CDataLen-1:0] FMem [CRamSize-1:0];
 wire [CDataLen-1:0] BMemA;
 reg [CDataLen-1:0] BMemB;

 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  FAddrWr<=CAddrNil;
  FAddrRd<=CAddrNil;
  FMosi<={CDataLen{1'h0}};
  FWrEn<=1'h0;
  end
 else if (AClkHEn)
  begin
  FAddrWr<=AAddrWr;
  FAddrRd<=AAddrRd;
  FMosi<=AMosi;
  FWrEn<=AWrEn;
  end

 assign BMemA=FMem[FAddrWr];

 always @(BMemA or FWrEn or FMosi)
  begin
  BMemB=BMemA;
  if (FWrEn) BMemB=FMosi;
  end


 integer BIndex;
 always @(posedge AClkH or negedge AResetHN)
 if (AResetHN==1'b0)
  begin
  for (BIndex=0; BIndex<CRamSize; BIndex=BIndex+1) FMem[BIndex]<={CDataLen{1'b0}};
  end
 else if (AClkHEn)
  begin
  FMem[FAddrWr]<=BMemB;
  end

 assign AMiso=FMem[FAddrRd];

endmodule


