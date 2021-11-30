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

assign busy = 1'b0;
assign data_out = 29'h0;

endmodule
