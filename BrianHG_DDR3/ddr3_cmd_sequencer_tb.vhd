library ieee;

use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity ddr3_cmd_sequencer_tb is
    generic
    (
        -- system clock generation and operation
        CLK_MHZ_IN              : integer := 400;

        -- DDR3 ram chip configuration settings
        DDR3_WIDTH_DQ           : integer := 8;
        DDR3_NUM_CHIPS          : integer := 1;

        DDR3_WIDTH_ADDR         : integer := 15;
        DDR3_WIDTH_BANK         : integer := 3;
        DDR3_WIDTH_CAS          : integer := 10;
        DDR3_RWDQ_BITS          : integer := 64; -- DDR3_WIDTH_DQ * DDR3_NUM_CHIPS * 8;

        -- DDR3 controller configuration parameter settings
        PORT_VECTOR_SIZE        : integer := 16
    );
    
    port
    (
        rst_in, cmd_clk         : in std_ulogic;
        in_ena, in_wena         : in std_ulogic;
        in_bank                 : in std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
        in_ras                  : in std_ulogic_vector(DDR3_WIDTH_ADDR - 1 downto 0);
        in_cas                  : in std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
        in_wdata                : in std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        in_wmask                : in std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
        in_vector               : in std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);     -- embed multiple read request returns into the in_rd_vector
        cmd_ack                 : out std_ulogic;
        cmd_ready               : out std_ulogic;
        cmd_cmd                 : out std_ulogic_vector(3 downto 0);   -- DDR3 command out (CS#, RAS#, CAS#, WE#)
        cmd_name                : out string;
        cmd_bank                : out std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
        cmd_a                   : out std_ulogic_vector(DDR3_WIDTH_ADDR - 1 downto 0);
        cmd_wdata               : out std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        cmd_wmask               : out std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
        cmd_vector              : out std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
        
        ref_req                 : in std_ulogic;
        ref_ack                 : out std_ulogic;
        idle                    : out std_ulogic
    );
end entity ddr3_cmd_sequencer_tb;

architecture sim of ddr3_cmd_sequencer_tb is
    component ddr3_cmd_sequencer is
        generic
        (
            USE_TOGGLE_ENA          : boolean;
            USE_TOGGLE_OUT          : boolean;
            DDR3_WIDTH_BANK         : natural;
            DDR3_WIDTH_ROW          : natural;
            DDR3_WIDTH_CAS          : natural;
            DDR3_RWDQ_BITS          : natural;
            PORT_VECTOR_SIZE        : natural
        );
        port
        (
            reset                   : in std_ulogic;
            clk                     : in std_ulogic;
            in_ena                  : in std_ulogic;
            in_busy                 : in std_ulogic;
            in_wena                 : in std_ulogic;
            in_bank                 : in std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
            in_ras                  : in std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);

            in_cas                  : in std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
            in_wdata                : in std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
            in_wmask                : in std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
            in_rd_vector            : in std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
            in_refresh_t            : in std_ulogic;
            out_ack                 : in std_ulogic;
            out_ready               : out std_ulogic;
            out_cmd                 : out std_ulogic_vector(3 downto 0);
            out_txb                 : out std_ulogic_vector(7 downto 0);
            out_bank                : out std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
            out_a                   : out std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
            out_wdata               : out std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
            out_wmask               : out std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
            in_read_rdy_t           : in std_ulogic;
            in_read_data            : in std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
            out_read_ready          : out std_ulogic;
            out_read_data           : out std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
            out_rd_vector           : out std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
            out_refresh_ack         : out std_ulogic;
            out_idle                : out std_ulogic;
            read_cal_pat_t          : out std_ulogic;
            read_cal_pat_v          : out std_ulogic
        );
    end component ddr3_cmd_sequencer;

    constant DDR3_NUM_CK    : integer := DDR3_NUM_CHIPS;
    constant USE_TOGGLE_ENA : boolean := false;
    constant USE_TOGGLE_OUT : boolean := false;

    type DDR_CMD_NAME is (MRS, REF, PRE, ACT, WRI, REA, ZQC, NOP,
                          XOP1, XOP2, XOP3, XOP4, XOP5, XOP6, XOP7, NOP2);
                          
    constant tb_command_script_file : string := "DDR3_CMD_SEQ_script.txt";
    constant script_cmd             : string := "*** POWER UP ***";
    constant period                 : integer := 500000 / CLK_MHZ_IN;    
    signal script_line              : std_ulogic_vector(12 downto 0);
    
    signal auto_wait                : boolean := false;
    
    signal in_busy                  : std_ulogic;
    signal in_rd_vector             : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
    signal i_cmd_ack                : std_ulogic;

begin
    cmd_ack <= i_cmd_ack;
    
    b_cmd_sequencer : block
        signal out_txb              : std_ulogic_vector(7 downto 0);
        signal out_read_ready       : std_ulogic;
        signal read_cal_pat_t,
               read_cal_pat_v       : std_ulogic;
        signal out_read_data        : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
    begin
        i_cmd_sequencer : component ddr3_cmd_sequencer
            generic map
            (
                USE_TOGGLE_ENA          => false,
                USE_TOGGLE_OUT          => false,
                DDR3_WIDTH_BANK         => DDR3_WIDTH_BANK,
                DDR3_WIDTH_ROW          => DDR3_WIDTH_ADDR,
                DDR3_WIDTH_CAS          => DDR3_WIDTH_CAS,
                DDR3_RWDQ_BITS          => DDR3_RWDQ_BITS,
                PORT_VECTOR_SIZE        => PORT_VECTOR_SIZE
            )
            port map
            (
                reset                   => rst_in,
                clk                     => cmd_clk,
                in_ena                  => in_ena,
                in_busy                 => in_busy,
                in_wena                 => in_wena,
                in_bank                 => in_bank,
                in_ras                  => in_ras,
                in_cas                  => in_cas,
                in_wdata                => in_wdata,
                in_wmask                => in_wmask,
                in_rd_vector            => in_rd_vector,
                out_ack                 => i_cmd_ack,
                out_ready               => cmd_ready,
                out_cmd                 => cmd_cmd,
                out_txb                 => out_txb,
                out_bank                => cmd_bank,
                out_a                   => cmd_a,
                out_wdata               => cmd_wdata,
                out_wmask               => cmd_wmask,
                in_read_rdy_t           => 'Z',
                in_read_data            => (others => 'Z'),
                out_read_ready          => out_read_ready,
                out_read_data           => out_read_data,
                out_rd_vector           => cmd_vector,
                in_refresh_t            => ref_req,
                out_refresh_ack         => ref_ack,
                out_idle                => idle,
                read_cal_pat_t          => read_cal_pat_t,
                read_cal_pat_v          => read_cal_pat_v
            );
        end block b_cmd_sequencer;
end architecture sim;
