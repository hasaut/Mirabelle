module Pll32_200 
 (
  input  inclk0,
  input  areset,
  output c0,
  output locked
 );


 reg FLocked;
 always @(posedge inclk0 or posedge areset)
 if (areset==1'b1)
  begin
  FLocked<=1'b0;
  end
 else
  begin
  FLocked<=1'b1;
  end

assign c0 = inclk0;
assign locked = FLocked;

endmodule
