module Ram8a16d_1g
 (
  AClkH, AResetN, AClkHEn,
  AAddr, AMosi, AMiso, AWrEn, ARdEn
 );

 // Interface
 input AClkH, AResetN, AClkHEn;
 input [7:0] AAddr; input [15:0] AMosi; output [15:0] AMiso; input [15:0] AWrEn, ARdEn;

 // Implementation
 wire [15:0] BMiso;

 reg [15:0] FRdEn;
 always @(posedge AClkH or negedge AResetN)
 if (AResetN==1'b0)
  begin
  FRdEn<=16'h0;
  end
 else if (AClkHEn)
  begin
  FRdEn<=ARdEn;
  end

 assign AMiso = FRdEn & BMiso;

 altsyncram altsyncram_component
  (
   .address_a (AAddr),
   .byteena_a (AWrEn),
   .clock0 (AClkH),
   .clocken0 (AClkHEn),
   .data_a (AMosi),
   .wren_a (|AWrEn),
   .q_a (BMiso),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .address_b (1'b1),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_b (1'b1),
   .clock1 (1'b1),
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
  altsyncram_component.byte_size = 1,
  altsyncram_component.clock_enable_input_a = "NORMAL",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  //altsyncram_component.intended_device_family = "MAX 10",
  altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.operation_mode = "SINGLE_PORT",
  altsyncram_component.outdata_aclr_a = "NONE",
  altsyncram_component.outdata_reg_a = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_port_a = "NEW_DATA_WITH_NBE_READ",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.width_a = 16,
  altsyncram_component.width_byteena_a = 16;

endmodule

module Ram8a1d
 (
  AClkH, AResetN, AClkHEn,
  AAddr, AMosi, AMiso, AWrEn, ARdEn
 );

 // Interface
 input AClkH, AResetN, AClkHEn;
 input [7:0] AAddr; input AMosi; output AMiso; input AWrEn, ARdEn;

 // Implementation
 wire BMiso;

 reg FRdEn;
 always @(posedge AClkH or negedge AResetN)
 if (AResetN==1'b0)
  begin
  FRdEn<=1'b0;
  end
 else if (AClkHEn)
  begin
  FRdEn<=ARdEn;
  end

 assign AMiso = FRdEn & BMiso;

 altsyncram altsyncram_component
  (
   .address_a (AAddr),
   .byteena_a (AWrEn),
   .clock0 (AClkH),
   .clocken0 (AClkHEn),
   .data_a (AMosi),
   .wren_a (|AWrEn),
   .q_a (BMiso),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .address_b (1'b1),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_b (1'b1),
   .clock1 (1'b1),
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
  altsyncram_component.clock_enable_input_a = "NORMAL",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  //altsyncram_component.intended_device_family = "MAX 10",
  altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.operation_mode = "SINGLE_PORT",
  altsyncram_component.outdata_aclr_a = "NONE",
  altsyncram_component.outdata_reg_a = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  //altsyncram_component.read_during_write_mode_port_a = "NEW_DATA_WITH_NBE_READ",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.width_a = 1,
  altsyncram_component.width_byteena_a = 1;

endmodule


module RamDX #(parameter CAddrLen=11, CDataLen=8)
 (
  input AClkA, input AResetAN, input AClkAEn,
  input [CAddrLen-1:0] AAddrA, input [CDataLen-1:0] AMosiA, output [CDataLen-1:0] AMisoA, input AWrEnA, input ARdEnA,
  input AClkB, input AResetBN, input AClkBEn,
  input [CAddrLen-1:0] AAddrB, input [CDataLen-1:0] AMosiB, output [CDataLen-1:0] AMisoB, input AWrEnB, input ARdEnB
 );

 localparam CDataZ = {CDataLen{1'b0}};

 wire [CDataLen-1:0] BMisoA, BMisoB;

 reg FRdEnA;
 always @(posedge AClkA or negedge AResetAN)
 if (AResetAN==1'b0)
  begin
  FRdEnA<=1'b0;
  end
 else if (AClkAEn)
  begin
  FRdEnA<=ARdEnA;
  end

 reg FRdEnB;
 always @(posedge AClkB or negedge AResetBN)
 if (AResetBN==1'b0)
  begin
  FRdEnB<=1'b0;
  end
 else if (AClkBEn)
  begin
  FRdEnB<=ARdEnB;
  end

 assign AMisoA = FRdEnA ? BMisoA : CDataZ;
 assign AMisoB = FRdEnB ? BMisoB : CDataZ;

 altsyncram altsyncram_component
  (
   .address_a(AAddrA),
   .address_b(AAddrB),
   .clock0(AClkA),
   .clock1(AClkB),
   .clocken0(AClkAEn),
   .clocken1(AClkBEn),
   .data_a(AMosiA),
   .data_b(AMosiB),
   .wren_a(AWrEnA),
   .wren_b(AWrEnB),
   .q_a(BMisoA),
   .q_b(BMisoB),
   .aclr0(1'b0),
   .aclr1(1'b0),
   .addressstall_a(1'b0),
   .addressstall_b(1'b0),
   .byteena_a(1'b1),
   .byteena_b(1'b1),
   .clocken2(1'b1),
   .clocken3(1'b1),
   .eccstatus(),
   .rden_a(1'b1),
   .rden_b(1'b1)
  );

 defparam
  altsyncram_component.address_reg_b = "CLOCK1",
  altsyncram_component.clock_enable_input_a = "NORMAL",
  altsyncram_component.clock_enable_input_b = "NORMAL",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  altsyncram_component.indata_reg_b = "CLOCK1",
  //altsyncram_component.intended_device_family = "MAX 10",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = (1<<CAddrLen),
  altsyncram_component.numwords_b = (1<<CAddrLen),
  altsyncram_component.operation_mode = "BIDIR_DUAL_PORT",
  altsyncram_component.outdata_aclr_a = "NONE",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_a = "UNREGISTERED",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  //altsyncram_component.read_during_write_mode_port_a = "NEW_DATA_WITH_NBE_READ",
  //altsyncram_component.read_during_write_mode_port_b = "NEW_DATA_WITH_NBE_READ",
  altsyncram_component.widthad_a = CAddrLen,
  altsyncram_component.widthad_b = CAddrLen,
  altsyncram_component.width_a = CDataLen,
  altsyncram_component.width_b = CDataLen,
  altsyncram_component.width_byteena_a = 1,
  altsyncram_component.width_byteena_b = 1,
  altsyncram_component.wrcontrol_wraddress_reg_b = "CLOCK1";

endmodule

