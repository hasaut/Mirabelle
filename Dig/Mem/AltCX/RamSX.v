module RamSX #(parameter CAddrLen=13, CDataLen=128)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddr, input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso, input AWrEn, input ARdEn
 );

 wire [CDataLen-1:0] BMiso;
 wire [CAddrLen-1:0] FAddr, BAddr;
 wire FRdEn, BRdEn;

 MsDffList #(.CRegLen(CAddrLen+1)) ULocalVars
  (
   .AClkH(AClkH), .AResetHN(AResetHN), .AClkHEn(AClkHEn),
   .ADataI({BAddr, BRdEn}),
   .ADataO({FAddr, FRdEn})
  );

 assign BRdEn = ARdEn;
 wire BAccessAny = |{AWrEn, ARdEn};
 assign BAddr = BAccessAny ? AAddr : FAddr;

 altsyncram altsyncram_component
  (
   .wren_a (AWrEn),
   .clock0 (AClkH),
   .byteena_a (AWrEn),
   .address_a (BAddr),
   .data_a (AMosi),
   .q_a (BMiso),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .address_b (1'b1),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_b (1'b1),
   .clock1 (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b (1'b1),
   .eccstatus (),
   .q_b (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

 defparam
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  //altsyncram_component.intended_device_family = "MAX 10",
  altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = (1<<CAddrLen),
  altsyncram_component.operation_mode = "SINGLE_PORT",
  altsyncram_component.outdata_aclr_a = "NONE",
  altsyncram_component.outdata_reg_a = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.widthad_a = CAddrLen,
  altsyncram_component.width_a = CDataLen,
  altsyncram_component.width_byteena_a = 1;

 assign AMiso = {CDataLen{FRdEn}} & BMiso;

endmodule

