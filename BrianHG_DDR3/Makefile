PATH := $(PATH):/opt/intelFPGA_lite/20.1/modelsim_ase/bin/

VHDL_SRCS = \
	ddr3_cmd_sequencer.vhd \
	ddr3_cmd_sequencer_tb.vhd
	
SV_SRCS = BrianHG_DDR3_CONTROLLER_top.sv BrianHG_DDR3_CMD_SEQUENCER.sv
VCOM_OPTS=-2008

sim: $(SV_SRCS) $(SRCS) 
	vlib work 
	vmap work work
	vlog -sv -work work $(SV_SRCS)
	vcom -work work $(VCOM_OPTS) $(VHDL_SRCS)
run:
	make sim
	vsim -c ddr3_cmd_sequencer_tb -do ddr3_cmd_sequencer_sim.do
vrun:
	make sim
	vsim ddr3_cmd_sequencer_tb -do ddr3_cmd_sequencer_sim.do

