module Ram8a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

input AClkH, AResetB, AClkHEn;
input [7:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({8{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 256,
 altsyncram_component.numwords_b = 256,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = 8,
 altsyncram_component.widthad_b = 8,
 altsyncram_component.width_a = 8,
 altsyncram_component.width_b = 8,
 altsyncram_component.width_byteena_a = 1;

endmodule

module Ram10a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

input AClkH, AResetB, AClkHEn;
input [9:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({8{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 1024,
 altsyncram_component.numwords_b = 1024,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = 10,
 altsyncram_component.widthad_b = 10,
 altsyncram_component.width_a = 8,
 altsyncram_component.width_b = 8,
 altsyncram_component.width_byteena_a = 1;

endmodule

module Ram12a8d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

input AClkH, AResetB, AClkHEn;
input [11:0] AAddrWr, AAddrRd;
input [7:0] AMosi; output [7:0] AMiso;
input AWrEn;

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({8{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 4096,
 altsyncram_component.numwords_b = 4096,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = 12,
 altsyncram_component.widthad_b = 12,
 altsyncram_component.width_a = 8,
 altsyncram_component.width_b = 8,
 altsyncram_component.width_byteena_a = 1;

endmodule

module Ram8a9d2
 (
  AClkH, AResetB, AClkHEn,
  AAddrWr, AAddrRd, AMosi, AMiso, AWrEn
 );

input AClkH, AResetB, AClkHEn;
input [7:0] AAddrWr, AAddrRd;
input [8:0] AMosi; output [8:0] AMiso;
input AWrEn;

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({9{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 256,
 altsyncram_component.numwords_b = 256,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = 8,
 altsyncram_component.widthad_b = 8,
 altsyncram_component.width_a = 9,
 altsyncram_component.width_b = 9,
 altsyncram_component.width_byteena_a = 1;

endmodule

module Ram10a9d2
 (
  input AClkH, AResetB, AClkHEn,
  input [9:0] AAddrWr, input [9:0] AAddrRd,
  input [8:0] AMosi, output [8:0] AMiso,
  input AWrEn
 );


 altsyncram altsyncram_component
  (
   .wren_a (AWrEn),
   .clock0 (AClkH),
   .address_a (AAddrWr),
   .address_b (AAddrRd),
   .data_a (AMosi),
   .q_b (AMiso),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_a (AWrEn),
   .byteena_b (1'b1),
   .clock1 (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b ({9{1'b1}}),
   .eccstatus (),
   .q_a (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 1024,
  altsyncram_component.numwords_b = 1024,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 10,
  altsyncram_component.widthad_b = 10,
  altsyncram_component.width_a = 9,
  altsyncram_component.width_b = 9,
  altsyncram_component.width_byteena_a = 1;

endmodule

module Ram8a32d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [31:0] AMosi, output [31:0] AMiso,
  input AWrEn
 );


altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({32{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.numwords_b = 256,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.widthad_b = 8,
  altsyncram_component.width_a = 32,
  altsyncram_component.width_b = 32,
  altsyncram_component.width_byteena_a = 1;

endmodule

module Ram8a25d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [24:0] AMosi, output [24:0] AMiso,
  input AWrEn
 );


altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({25{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.numwords_b = 256,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.widthad_b = 8,
  altsyncram_component.width_a = 25,
  altsyncram_component.width_b = 25,
  altsyncram_component.width_byteena_a = 1;

endmodule

module Ram8a64d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [63:0] AMosi, output [63:0] AMiso,
  input AWrEn
 );


altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({64{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.numwords_b = 256,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.widthad_b = 8,
  altsyncram_component.width_a = 64,
  altsyncram_component.width_b = 64,
  altsyncram_component.width_byteena_a = 1;

endmodule

module Ram8a105d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [104:0] AMosi, output [104:0] AMiso,
  input AWrEn
 );


altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({105{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 256,
  altsyncram_component.numwords_b = 256,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 8,
  altsyncram_component.widthad_b = 8,
  altsyncram_component.width_a = 105,
  altsyncram_component.width_b = 105,
  altsyncram_component.width_byteena_a = 1;

endmodule

module Ram16a8d2
 (
  input AClkH, input AResetB, input AClkHEn,
  input [15:0] AAddrWr, input [15:0] AAddrRd,
  input [7:0] AMosi, output [7:0] AMiso,
  input AWrEn
 );

 altsyncram altsyncram_component
  (
   .wren_a (AWrEn),
   .clock0 (AClkH),
   .address_a (AAddrWr),
   .address_b (AAddrRd),
   .data_a (AMosi),
   .q_b (AMiso),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_a (AWrEn),
   .byteena_b (1'b1),
   .clock1 (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b ({8{1'b1}}),
   .eccstatus (),
   .q_a (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

 defparam
  altsyncram_component.address_reg_b = "CLOCK0",
  altsyncram_component.clock_enable_input_a = "BYPASS",
  altsyncram_component.clock_enable_input_b = "BYPASS",
  altsyncram_component.clock_enable_output_a = "BYPASS",
  altsyncram_component.clock_enable_output_b = "BYPASS",
  //altsyncram_component.intended_device_family = "Cyclone IV E",
  altsyncram_component.lpm_type = "altsyncram",
  altsyncram_component.numwords_a = 65536,
  altsyncram_component.numwords_b = 65536,
  altsyncram_component.operation_mode = "DUAL_PORT",
  altsyncram_component.outdata_aclr_b = "NONE",
  altsyncram_component.outdata_reg_b = "UNREGISTERED",
  altsyncram_component.power_up_uninitialized = "FALSE",
  altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
  altsyncram_component.widthad_a = 16,
  altsyncram_component.widthad_b = 16,
  altsyncram_component.width_a = 8,
  altsyncram_component.width_b = 8,
  altsyncram_component.width_byteena_a = 1;

endmodule

module RamScope12a128d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [11:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [11:0] AAddrBRd, output [127:0] AMisoB
 );


 altsyncram altsyncram_component
  (
   .address_a (AAddrAWr),
   .address_b (AAddrBRd),
   .clock0 (AClkA),
   .clock1 (AClkB),
   .data_a (AMosiA),
   .wren_a (AWrEn),
   .q_b (AMisoB),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_a (1'b1),
   .byteena_b (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b ({128{1'b1}}),
   .eccstatus (),
   .q_a (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

  defparam
   altsyncram_component.address_aclr_b = "NONE",
   altsyncram_component.address_reg_b = "CLOCK1",
   altsyncram_component.clock_enable_input_a = "BYPASS",
   altsyncram_component.clock_enable_input_b = "BYPASS",
   altsyncram_component.clock_enable_output_b = "BYPASS",
   //altsyncram_component.intended_device_family = "Cyclone 10 LP",
   altsyncram_component.lpm_type = "altsyncram",
   altsyncram_component.numwords_a = 4096,
   altsyncram_component.numwords_b = 4096,
   altsyncram_component.operation_mode = "DUAL_PORT",
   altsyncram_component.outdata_aclr_b = "NONE",
   altsyncram_component.outdata_reg_b = "UNREGISTERED",
   altsyncram_component.power_up_uninitialized = "FALSE",
   altsyncram_component.widthad_a = 12,
   altsyncram_component.widthad_b = 12,
   altsyncram_component.width_a = 128,
   altsyncram_component.width_b = 128,
   altsyncram_component.width_byteena_a = 1;


endmodule

module RamScope13a128d
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [12:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [12:0] AAddrBRd, output [127:0] AMisoB
 );


 altsyncram altsyncram_component
  (
   .address_a (AAddrAWr),
   .address_b (AAddrBRd),
   .clock0 (AClkA),
   .clock1 (AClkB),
   .data_a (AMosiA),
   .wren_a (AWrEn),
   .q_b (AMisoB),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_a (1'b1),
   .byteena_b (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b ({128{1'b1}}),
   .eccstatus (),
   .q_a (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

  defparam
   altsyncram_component.address_aclr_b = "NONE",
   altsyncram_component.address_reg_b = "CLOCK1",
   altsyncram_component.clock_enable_input_a = "BYPASS",
   altsyncram_component.clock_enable_input_b = "BYPASS",
   altsyncram_component.clock_enable_output_b = "BYPASS",
   //altsyncram_component.intended_device_family = "Cyclone 10 LP",
   altsyncram_component.lpm_type = "altsyncram",
   altsyncram_component.numwords_a = 8192,
   altsyncram_component.numwords_b = 8192,
   altsyncram_component.operation_mode = "DUAL_PORT",
   altsyncram_component.outdata_aclr_b = "NONE",
   altsyncram_component.outdata_reg_b = "UNREGISTERED",
   altsyncram_component.power_up_uninitialized = "FALSE",
   altsyncram_component.widthad_a = 13,
   altsyncram_component.widthad_b = 13,
   altsyncram_component.width_a = 128,
   altsyncram_component.width_b = 128,
   altsyncram_component.width_byteena_a = 1;


endmodule

module RamScope128d #(parameter CAddrLen=12)
 (
  input AClkA, input AResetAN, input AClkAEn,
  input AClkB, input AResetBN, input AClkBEn,
  input [CAddrLen-1:0] AAddrAWr, input [127:0] AMosiA, input AWrEn,
  input [CAddrLen-1:0] AAddrBRd, output [127:0] AMisoB
 );


 altsyncram altsyncram_component
  (
   .address_a (AAddrAWr),
   .address_b (AAddrBRd),
   .clock0 (AClkA),
   .clock1 (AClkB),
   .data_a (AMosiA),
   .wren_a (AWrEn),
   .q_b (AMisoB),
   .aclr0 (1'b0),
   .aclr1 (1'b0),
   .addressstall_a (1'b0),
   .addressstall_b (1'b0),
   .byteena_a (1'b1),
   .byteena_b (1'b1),
   .clocken0 (1'b1),
   .clocken1 (1'b1),
   .clocken2 (1'b1),
   .clocken3 (1'b1),
   .data_b ({128{1'b1}}),
   .eccstatus (),
   .q_a (),
   .rden_a (1'b1),
   .rden_b (1'b1),
   .wren_b (1'b0)
  );

  defparam
   altsyncram_component.address_aclr_b = "NONE",
   altsyncram_component.address_reg_b = "CLOCK1",
   altsyncram_component.clock_enable_input_a = "BYPASS",
   altsyncram_component.clock_enable_input_b = "BYPASS",
   altsyncram_component.clock_enable_output_b = "BYPASS",
   //altsyncram_component.intended_device_family = "Cyclone 10 LP",
   altsyncram_component.lpm_type = "altsyncram",
   altsyncram_component.numwords_a = (1<<CAddrLen),
   altsyncram_component.numwords_b = (1<<CAddrLen),
   altsyncram_component.operation_mode = "DUAL_PORT",
   altsyncram_component.outdata_aclr_b = "NONE",
   altsyncram_component.outdata_reg_b = "UNREGISTERED",
   altsyncram_component.power_up_uninitialized = "FALSE",
   altsyncram_component.widthad_a = CAddrLen,
   altsyncram_component.widthad_b = CAddrLen,
   altsyncram_component.width_a = 128,
   altsyncram_component.width_b = 128,
   altsyncram_component.width_byteena_a = 1;


endmodule


module Ram8aXd2 #(parameter CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [7:0] AAddrWr, input [7:0] AAddrRd,
  input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso,
  input AWrEn
 );

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({CDataLen{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 256,
 altsyncram_component.numwords_b = 256,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = 8,
 altsyncram_component.widthad_b = 8,
 altsyncram_component.width_a = CDataLen,
 altsyncram_component.width_b = CDataLen,
 altsyncram_component.width_byteena_a = 1;

endmodule


module RamXaXd2 #(parameter CAddrLen=8, CDataLen=16)
 (
  input AClkH, input AResetHN, input AClkHEn,
  input [CAddrLen-1:0] AAddrWr, input [CAddrLen-1:0] AAddrRd,
  input [CDataLen-1:0] AMosi, output [CDataLen-1:0] AMiso,
  input AWrEn
 );

altsyncram altsyncram_component
 (
  .wren_a (AWrEn),
  .clock0 (AClkH),
  .address_a (AAddrWr),
  .address_b (AAddrRd),
  .data_a (AMosi),
  .q_b (AMiso),
  .aclr0 (1'b0),
  .aclr1 (1'b0),
  .addressstall_a (1'b0),
  .addressstall_b (1'b0),
  .byteena_a (AWrEn),
  .byteena_b (1'b1),
  .clock1 (1'b1),
  .clocken0 (1'b1),
  .clocken1 (1'b1),
  .clocken2 (1'b1),
  .clocken3 (1'b1),
  .data_b ({CDataLen{1'b1}}),
  .eccstatus (),
  .q_a (),
  .rden_a (1'b1),
  .rden_b (1'b1),
  .wren_b (1'b0)
 );

defparam
 altsyncram_component.address_reg_b = "CLOCK0",
 altsyncram_component.clock_enable_input_a = "BYPASS",
 altsyncram_component.clock_enable_input_b = "BYPASS",
 altsyncram_component.clock_enable_output_a = "BYPASS",
 altsyncram_component.clock_enable_output_b = "BYPASS",
 //altsyncram_component.intended_device_family = "Cyclone IV E",
 altsyncram_component.lpm_type = "altsyncram",
 altsyncram_component.numwords_a = 1<<CAddrLen,
 altsyncram_component.numwords_b = 1<<CAddrLen,
 altsyncram_component.operation_mode = "DUAL_PORT",
 altsyncram_component.outdata_aclr_b = "NONE",
 altsyncram_component.outdata_reg_b = "UNREGISTERED",
 altsyncram_component.power_up_uninitialized = "FALSE",
 altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
 altsyncram_component.widthad_a = CAddrLen,
 altsyncram_component.widthad_b = CAddrLen,
 altsyncram_component.width_a = CDataLen,
 altsyncram_component.width_b = CDataLen,
 altsyncram_component.width_byteena_a = 1;

endmodule

