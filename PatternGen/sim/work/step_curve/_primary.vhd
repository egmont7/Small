library verilog;
use verilog.vl_types.all;
entity step_curve is
    port(
        clk             : in     vl_logic;
        CAL             : out    vl_logic;
        SBI             : out    vl_logic;
        SPHI1           : out    vl_logic;
        SPHI2           : out    vl_logic;
        SEB             : out    vl_logic;
        ISSR            : out    vl_logic;
        RESET           : out    vl_logic;
        R12             : out    vl_logic;
        RBI             : out    vl_logic;
        RPHI1           : out    vl_logic;
        RPHI2           : out    vl_logic;
        LE              : out    vl_logic;
        reset_gen       : in     vl_logic
    );
end step_curve;
