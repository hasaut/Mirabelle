module TryComp
 (
  input AClkH, input AResetHN, input AClkHEn,
  input ADataI, output ADataO
 );

 localparam CDualCnt = 2;

 // Mosi
 wire BExecEn;
 wire [63:0] BRomMiso;
 wire [63:0] BRamMiso;
 wire [31:0] BIoMiso;
 // Miso
 wire [31:3] BRomAddr;  wire BRomRdEn;
 wire [31:3] BRamAddr; wire [63:0] BRamMosi; wire [7:0] BRamWrEn, BRamRdEn;
 wire [15:0] BIoAddr; wire [31:0] BIoMosi; wire [2:0] BIoWrSize, BIoRdSize;
 wire [CDualCnt*2*23-1:0] BEipThis;

 MiTmpCompB #(.CLenI(29+1+29+64+8+8+16+32+3+3+CDualCnt*2*23+1), .CLenO(1+1+64+64+32)) UTmpComp
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BRomAddr, BRomRdEn, BRamAddr, BRamMosi, BRamWrEn, BRamRdEn, BIoAddr, BIoMosi, BIoWrSize, BIoRdSize, BEipThis, ADataI}),
   .ADataO({ADataO, BExecEn, BRomMiso, BRamMiso, BIoMiso})
  );

 MiProcTop #(.CDualCnt(CDualCnt), .CStartAddr(32'h0000)) UProc
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .AExecEn(BExecEn),
   .ARomAddr(BRomAddr), .ARomMiso(BRomMiso), .ARomRdEn(BRomRdEn),
   .ARamAddr(BRamAddr), .ARamMosi(BRamMosi), .ARamMiso(BRamMiso), .ARamWrEn(BRamWrEn), .ARamRdEn(BRamRdEn),
   .AIoAddr(BIoAddr), .AIoMosi(BIoMosi), .AIoMiso(BIoMiso), .AIoWrSize(BIoWrSize), .AIoRdSize(BIoRdSize),
   .AEipThis(BEipThis)
  );

endmodule


