`timescale 1ns / 1ps

module step_curve_test;

reg clk = 0;
integer clk_num = 0;
reg reset_gen = 1;

wire CAL;
wire SBI;
wire SPHI1;
wire SPHI2;
wire SEB;
wire ISSR;
wire RESET;
wire R12;
wire RBI;
wire RPHI1;
wire RPHI2;
wire LE;

always #12.5 clk = !clk;
always @(posedge clk) clk_num = clk_num + 1;

initial begin
        #20 reset_gen = 0;

        repeat (2000) @(posedge clk);
        $stop(2);
end

step_curve DUT
(
        .clk(clk),
        .CAL(CAL),
        .SBI(SBI),
        .SPHI1(SPHI1),
        .SPHI2(SPHI2),
        .SEB(SEB),
        .ISSR(ISSR),
        .RESET(RESET),
        .R12(R12),
        .RBI(RBI),
        .RPHI1(RPHI1),
        .RPHI2(RPHI2),
        .LE(LE),
        .reset_gen(reset_gen)
);

endmodule
