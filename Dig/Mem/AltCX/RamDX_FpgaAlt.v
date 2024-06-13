module RamDX_Fpga #(parameter CAddrLen=11, CDataLen=8)
 (
  input wire AClkA, input wire AResetAN, input wire AClkAEn,
  input wire [CAddrLen-1:0] AAddrA, input wire [CDataLen-1:0] AMosiA, output wire [CDataLen-1:0] AMisoA, input wire AWrEnA, input wire ARdEnA,
  input wire AClkB, input wire AResetBN, input wire AClkBEn,
  input wire [CAddrLen-1:0] AAddrB, input wire [CDataLen-1:0] AMosiB, output wire [CDataLen-1:0] AMisoB, input wire AWrEnB, input wire ARdEnB
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

