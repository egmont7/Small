#!/usr/bin/env python3
import sys
import os
import argparse
from datetime import datetime
from collections import defaultdict

def parse_error(message, line_number, line_content):
    raise SyntaxError("Parse Error: {},\n\t line {}: \"{}\"".format(message,line_number+1,line_content.strip()))

def parse(lines):
    signals = []
    stages = []
    dev_name = "device"
    patterns = defaultdict(str)
    current_stage = None
    for i, line_orig in enumerate(lines):
        line = line_orig.split("#")[0].strip()
        if len(line) == 0:
            continue
        if line.startswith("dev"):
            toks = line.split()
            if len(toks) != 2:
                parse_error("Device definition",i,line_orig)
            dev_name = toks[1]
            continue
        if line.startswith("sig"):
            toks = line.split()
            if len(toks) != 2:
                parse_error("Signal definition",i,line_orig)
            signals.append(toks[1])
            continue
        if line.startswith('stage'):
            toks = line.split()
            if len(toks) != 3:
                parse_error("Stage definition",i,line_orig)
            stages.append( (toks[1],int(toks[2])) )
            current_stage = toks[1]
            continue
        toks = [t.strip() for t in line.split(':')]
        if len(toks) != 2:
            parse_error("Pattern definition",i,line_orig)
        sig = toks[0]
        if sig not in signals:
            parse_error("Undefined signal: {}".format(sig),i,line_orig)
        pat = toks[1].replace(" ","").replace("-","1").replace("_","0")
        if not set(pat).issubset(set(['0','1'])):
            parse_error("Bad character",i,line_orig)
        patterns[(sig,current_stage)] += pat

    for stage,_ in stages:
        n = len(patterns[(signals[0],stage)])
        for signal in signals:
            if len(patterns[(signal,stage)]) != n:
                raise SyntaxError("Uneven pattern length for signal:{}, stage:{}".format(signal,stage))

    return dev_name, signals, stages, patterns

vhdl_strings = {"header": [
                    "library ieee;",
                    "use ieee.std_logic_1164.all;",
                    "use ieee.numeric_std.all;",
                    "",
                    "ENTITY ***DEV_NAME*** IS",
                    "\tPORT(",
                    "\t\tclk : IN std_logic;"],
                "sig_out": "\t\t{} : OUT std_logic;",
                "res_in" : "\t\treset_gen : IN std_logic",
                "body_start": [
                    "\t);",
                    "END ***DEV_NAME***;",
                    "",
                    "ARCHITECTURE ***DEV_NAME***_arch OF ***DEV_NAME*** IS",
                    "SIGNAL counter:      integer := 0;",
                    "SIGNAL stage:        integer := 0;",
                    "SIGNAL stage_iter:   integer := 0;",
                    "BEGIN",
                    "\tPROCESS (clk)",
                    "\tBEGIN",
                    "\t\tIF(rising_edge(clk)) THEN",
                    "\t\t\tIF(reset_gen = '1') THEN",
                    "\t\t\t\tcounter <= 0;",
                    "\t\t\t\tstage <= 0;",
                    "\t\t\t\tstage_iter <= 0;",
                    "\t\t\tELSE"],
                "assign" : "\t\t{} <= '{}';",
                "if_counter" : "\tIF(counter = {}) THEN",
                "end_if" : "\tEND IF;",
                "if_stage" : "IF(stage = {}) THEN",
                "body_end1" : "\tIF(counter = {}) THEN",
                "body_end2" : "\t\tIF(stage_iter = {}) THEN",
                "body_end3" : "\t\t\tstage <= (stage + 1) MOD {};",
                "body_end4" : [
                    "\t\t\tstage_iter <= 0;",
                    "\t\tELSE",
                    "\t\t\tstage_iter <= stage_iter + 1;",
                    "\t\tEND IF;",
                    "\t\tcounter <= 0;",
                    "\tELSE",
                    "\t\tcounter <= counter + 1;",
                    "\tEND IF;",
                    "END IF;"],
                "footer" : [
                    "\t\t\tEND IF;",
                    "\t\tEND IF;",
                    "\tEND PROCESS;",
                    "END ***DEV_NAME***_arch;"],
                "inner_indent" : "\t\t\t\t"
                }

verilog_strings = {"header": [
                    "`timescale 1 ns / 1 ps",
                    "",
                    "module ***DEV_NAME***",
                    "(",
                    "\tinput clk,"],
                "sig_out": "\toutput reg {},",
                "res_in" : "\tinput reset_gen",
                "body_start": [
                    ");",
                    "",
                    "reg [31:0]counter;",
                    "reg [7:0]stage;",
                    "reg [15:0]stage_iter;",
                    "",
                    "always @(posedge clk ) begin",
                    "\tif(reset_gen == 1) begin",
                    "\t\tcounter <= 0;",
                    "\t\tstage <= 0;",
                    "\t\tstage_iter <= 0;",
                    "\tend",
                    "\telse begin"],
                "assign" : "\t\t{} <= {};",
                "if_counter" : "\tif(counter == {}) begin",
                "end_if" : "\tend",
                "if_stage" : "if(stage == {}) begin",
                "body_end1" : "\tif(counter == {}) begin",
                "body_end2" : "\t\tif(stage_iter == {}) begin",
                "body_end3" : "\t\t\tstage <= (stage + 1) % {};",
                "body_end4" : [
                    "\t\t\tstage_iter <= 0;",
                    "\t\tend",
                    "\t\telse begin",
                    "\t\t\tstage_iter <= stage_iter + 1;",
                    "\t\tend",
                    "\t\tcounter <= 0;",
                    "\tend",
                    "\telse begin",
                    "\t\tcounter <= counter + 1;",
                    "\tend",
                    "end"],
                "footer" : [
                    "\tend",
                    "end",
                    "endmodule"],
                "inner_indent" : "\t\t"
                }


def make_hdl(dev_name, signals, stages, patterns, hdl_strings):
    hdl = []
    hdl.extend(hdl_strings["header"])
    for sig_name in signals:
        hdl.append(hdl_strings["sig_out"].format(sig_name))
    hdl.append(hdl_strings["res_in"])
    hdl.extend(hdl_strings["body_start"])
    
    hdl_inner = []
    for stage_num, (stage, iters) in enumerate(stages):
        stage_hdl = []
        n = len(patterns[(signals[0],stage)])
        for i in range(n):
            step_hdl = []
            for signal in signals:
                pattern = patterns[(signal,stage)]
                if i < 1 or pattern[i-1] != pattern[i]:
                    step_hdl.append(hdl_strings["assign"].format(signal,pattern[i]))
            if len(step_hdl) > 0:
                stage_hdl.append(hdl_strings["if_counter"].format(i))
                stage_hdl.extend(step_hdl)
                stage_hdl.append(hdl_strings["end_if"])
        if len(stage_hdl) > 0:
            hdl_inner.append(hdl_strings["if_stage"].format(stage_num))
            hdl_inner.extend(stage_hdl)
            hdl_inner.append(hdl_strings["body_end1"].format(n-1))
            hdl_inner.append(hdl_strings["body_end2"].format(iters-1))
            hdl_inner.append(hdl_strings["body_end3"].format(len(stages)))
            hdl_inner.extend(hdl_strings["body_end4"])
    hdl.extend([hdl_strings["inner_indent"]+x for x in hdl_inner])

    hdl.extend(hdl_strings["footer"])
    hdl = '\n'.join(hdl)
    hdl = hdl.replace("***DEV_NAME***",dev_name).replace("\t","  ")
    
    return hdl

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input",
            help = "The input waveform file")
    parser.add_argument("lang", choices = ("verilog","vhdl"),
            help = "The HDL language for the output")
    parser.add_argument("-v", "--verbose", action = "store_true",
            help = "Be Verbose")

    args = parser.parse_args()

    with open(args.input,"r") as infile:
        lines = infile.readlines()
    
    if args.lang == "vhdl":
        hdl_strings = vhdl_strings
        extension = ".vhd"
        comment = "--"
    else:
        hdl_strings = verilog_strings
        extension = ".v"
        comment = "//"

    dev_name, signals, stages, patterns = parse(lines)
    hdl = make_hdl(dev_name, signals, stages, patterns, hdl_strings)
    
    if args.verbose: print(hdl)

    outfile_name = ".".join(args.input.split('.')[:-1])+extension

    with open(outfile_name,"w") as outfile:
        outfile.write("{}Autogenerated {} on {}\n\n".format(comment, args.lang, datetime.now()))
        outfile.write(hdl)