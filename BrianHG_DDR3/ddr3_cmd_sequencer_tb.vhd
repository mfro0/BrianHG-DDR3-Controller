library ieee;

use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity ddr3_cmd_sequencer_tb is
    generic
    (
        -- system clock generation and operation
        CLK_MHZ_IN          : integer := 400;

        -- DDR3 ram chip configuration settings
        DDR3_WIDTH_DQ       : integer := 8;
        DDR3_NUM_CHIPS      : integer := 1;

        DDR3_WIDTH_ADDR     : integer := 15;
        DDR3_WIDTH_BANK     : integer := 3;
        DDR3_WIDTH_CAS      : integer := 10;

        -- DDR3 controller configuration parameter settings
        PORT_VECTOR_SIZE    : integer := 16
    );
end entity ddr3_cmd_sequencer_tb;

architecture sim of ddr3_cmd_sequencer_tb is
    constant DDR3_NUM_CK    : integer := DDR3_NUM_CHIPS;
    constant USE_TOGGLE_ENA : boolean := false;
    constant USE_TOGGLE_OUT : boolean := false;

    constant DDR3_RWDQ_BITS : integer := DDR3_WIDTH_DQ * DDR3_NUM_CHIPS * 8;

    type DDR_CMD_NAME is (MRS, REF, PRE, ACT, WRI, REA, ZQC, NOP,
                          XOP1, XOP2, XOP3, XOP4, XOP5, XOP6, XOP7, NOP2);
begin

end architecture sim;
