library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity ddr3_cmd_sequencer is
    generic
    (
        USE_TOGGLE_ENA      : boolean := true;  -- when enabled, the (in_ena/in_busy) & (out_read_ready) toggle state to define the next command.
        USE_TOGGLE_OUT      : boolean := true;  -- when enabled, the (out_ready) & (out_ack) use toggle state to define the next command
        
        DDR3_WIDTH_BANK     : natural := 3;     -- use for the number of bits to address each bank.
        DDR3_WIDTH_ROW      : natural := 15;    -- use for the number of bits to address each row. *** 16 maximum.
        DDR3_WIDTH_CAS      : natural := 10;    -- use for the bits to address each column
        DDR3_RWDQ_BITS      : natural := 16;    -- must equal to total bus width accross all DDR3 ram chips. The mask width is divided by 8
        PORT_VECTOR_SIZE    : natural := 8;     -- set the width of the in_rd_vector & out_rd_vector
        CAL_WIDTH           : natural := 2;     -- should be DDR3_RWDQ_BITS / 8 -- the total bit width of the 'high' and 'low' pins
        EXTRA_SPEED         : boolean := true   -- enable for even better FMAX performance or when overclocking the core. This will increase logic cell usage
    );
    port
    (
        reset               : in std_ulogic;
        clk                 : in std_ulogic;

        in_ena              : in std_ulogic;
        in_busy             : out std_ulogic;

        in_wena             : in std_ulogic;
        in_bank             : in std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
        in_ras              : in std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
        in_cas              : in std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
        in_wdata            : in std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        in_wmask            : in std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
        in_rd_vector        : in std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
        in_refresh_t        : in std_ulogic;                                            -- invert/toggle this input once every time a refresh request is required

        out_ack             : in std_ulogic;                                            -- tells internal fifo to send another command.
        out_ready           : out std_ulogic;
        out_cmd             : out std_ulogic_vector(3 downto 0);                        -- DDR3 command out wiring order (CS#, RAS#, CAS#, WE#).
        out_txb             : out std_ulogic_vector(7 downto 0);                        -- DDR3 command out command signal bit order (nop, zqc, rea, wri, act, pre, ref, mrs).
        out_bank            : out std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
        out_a               : out std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
        out_wdata           : out std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        out_wmask           : out std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);

        in_read_rdy_t       : in std_ulogic;                                            -- from DDR3 IO phy module
        in_read_data        : in std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);        -- from DDR3 IO phy module
        out_read_ready      : out std_ulogic;
        out_read_data       : out std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        out_rd_vector       : out std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);     -- note that the 'preserve' here ensures the data latch location of the fifo's inferred memory block used for the read vector

        out_refresh_ack     : out std_ulogic;                                           -- once this outpput has become = to the in_refresh_t input, a refresh has been done.
        out_idle            : out std_ulogic;                                           -- when the DDR3 has not been sent any commands, i.e. s4_ready is always low.

        read_cal_pat_t      : out std_ulogic;                                           -- toggles after every read once the read_cal_pat_v data is valid.
        read_cal_pat_v      : out std_ulogic                                            -- valid read cal pattern detected in read.
    );
end entity ddr3_cmd_sequencer;

architecture rtl of ddr3_cmd_sequencer is
    -- multistage pipeline registers, deliberately laid out by name for visual purposes
    type pipeline_register_type is record
        wena            : std_ulogic;
        bank            : std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
        ras             : std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
        cas             : std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
        wdata           : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        wmask           : std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
        ref_req         : std_ulogic;
    end record;

    type pipeline_register_array_type is array(integer range <>) of pipeline_register_type;
    signal pipeline_registers   : pipeline_register_array_type(1 to 4) :=
        (
            /* 1 */ ('0', (others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 2 */ ('0', (others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 3 */ ('0', (others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 4 */ ('0', (others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0')
        );
begin
end architecture rtl;