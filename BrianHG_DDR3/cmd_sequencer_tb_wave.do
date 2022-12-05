onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /ddr3_cmd_sequencer_tb/cmd_clk
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/i_cmd_sequencer/reset_latch
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/i_BrianHG_DDR3_CMD_SEQUENCER/reset_latch
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/i_cmd_sequencer/reset_latch2
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/i_BrianHG_DDR3_CMD_SEQUENCER/reset_latch2
add wave -noupdate -divider out_txb
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/out_txb
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_out_txb
add wave -noupdate -divider out_read_ready
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/out_read_ready
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_out_read_ready
add wave -noupdate -divider out_read_data
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/b_cmd_sequencer/out_read_data
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_out_read_data
add wave -noupdate -divider read_cal_pat_t
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/read_cal_pat_v
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_read_cal_pat_v
add wave -noupdate -divider read_cal_pat_t
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/read_cal_pat_t
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_read_cal_pat_t
add wave -noupdate -divider in_busy
add wave -noupdate /ddr3_cmd_sequencer_tb/in_busy
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_in_busy
add wave -noupdate -divider cmd_ready
add wave -noupdate /ddr3_cmd_sequencer_tb/cmd_ready
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_ready
add wave -noupdate -divider cmd_cmd
add wave -noupdate /ddr3_cmd_sequencer_tb/cmd_cmd
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_cmd
add wave -noupdate -divider cmd_a
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/cmd_a
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_a
add wave -noupdate -divider cmd_wdata
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/cmd_wdata
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_wdata
add wave -noupdate -divider cmd_bank
add wave -noupdate /ddr3_cmd_sequencer_tb/cmd_bank
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_bank
add wave -noupdate -divider cmd_wmask
add wave -noupdate /ddr3_cmd_sequencer_tb/cmd_wmask
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_wmask
add wave -noupdate -divider cmd_vector
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/cmd_vector
add wave -noupdate -radix hexadecimal /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_cmd_vector
add wave -noupdate -divider ref_ack
add wave -noupdate /ddr3_cmd_sequencer_tb/ref_ack
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_ref_ack
add wave -noupdate -divider idle
add wave -noupdate /ddr3_cmd_sequencer_tb/idle
add wave -noupdate /ddr3_cmd_sequencer_tb/b_cmd_sequencer/sv_idle
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {93749424 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 650
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {2143 ps} {2625 ps}
