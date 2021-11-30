// IpSysUpdate_S25FL128.v

// Generated using ACDS version 18.1 625

`timescale 1 ps / 1 ps
module IpSysUpdate_S25FL128 (
		output wire        busy,        //        busy.busy
		input  wire        clock,       //       clock.clk
		input  wire [23:0] data_in,     //     data_in.data_in
		output wire [28:0] data_out,    //    data_out.data_out
		input  wire [2:0]  param,       //       param.param
		input  wire        read_param,  //  read_param.read_param
		input  wire [1:0]  read_source, // read_source.read_source
		input  wire        reconfig,    //    reconfig.reconfig
		input  wire        reset,       //       reset.reset
		input  wire        reset_timer, // reset_timer.reset_timer
		input  wire        write_param  // write_param.write_param
	);

	IpSysUpdate_S25FL128_remote_update_0 remote_update_0 (
		.busy        (busy),        //        busy.busy
		.data_out    (data_out),    //    data_out.data_out
		.param       (param),       //       param.param
		.read_param  (read_param),  //  read_param.read_param
		.reconfig    (reconfig),    //    reconfig.reconfig
		.reset_timer (reset_timer), // reset_timer.reset_timer
		.write_param (write_param), // write_param.write_param
		.data_in     (data_in),     //     data_in.data_in
		.read_source (read_source), // read_source.read_source
		.clock       (clock),       //       clock.clk
		.reset       (reset)        //       reset.reset
	);

endmodule
