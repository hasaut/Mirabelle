module IpSysUpdate_MT25QL01G (
		output wire        busy,        //        busy.busy
		input  wire        clock,       //       clock.clk
		input  wire [31:0] data_in,     //     data_in.data_in
		output wire [31:0] data_out,    //    data_out.data_out
		input  wire [2:0]  param,       //       param.param
		input  wire        read_param,  //  read_param.read_param
		input  wire [1:0]  read_source, // read_source.read_source
		input  wire        reconfig,    //    reconfig.reconfig
		input  wire        reset,       //       reset.reset
		input  wire        reset_timer, // reset_timer.reset_timer
		input  wire        write_param  // write_param.write_param
	);

assign busy = 1'b0;
assign data_out = 32'h0;

endmodule
