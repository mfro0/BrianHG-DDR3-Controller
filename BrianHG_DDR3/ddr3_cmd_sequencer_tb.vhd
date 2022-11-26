package generic_compare is
    generic
    (
        type enum
    );

    type keyword_type is access string;

    type assoc is record
        keyword     : keyword_type;
        selector    : enum;
    end record; 
    type assoc_array is array(natural range <>) of assoc;

    procedure lookup(variable data : assoc_array; lookup : string; selector : out enum);
end package generic_compare;

package body generic_compare is
    procedure lookup(variable data : assoc_array; lookup : string; selector : out enum) is
    begin
        for i in data'left to data'right loop
            if data(i).keyword.all = lookup then
                selector := data(i).selector;
                return;
            end if; 
        end loop;
        -- selector := enum'val(0);
    end procedure lookup;
end package body generic_compare;



library ieee;

use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use std.textio.all;

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
end entity ddr3_cmd_sequencer_tb;

architecture sim of ddr3_cmd_sequencer_tb is
    component BrianHG_DDR3_CMD_SEQUENCER is
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
            in_busy                 : out std_ulogic;
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
    end component BrianHG_DDR3_CMD_SEQUENCER;

    --
    -- tx_ddr3_cmd(src, dest, ln)
    --
    -- tx the ddr3 command_in
    --
    procedure tx_ddr3_cmd(variable ln_in : inout line; line_number : natural) is
        type cmd_type is (C_REFRESH, C_AWAIT, C_OUTENA, C_READ, C_WRITE, C_DELAY);
 
        package cmd_compare is new work.generic_compare generic map (enum => cmd_type);
        variable cmds : cmd_compare.assoc_array(0 to 5) :=
                            ((new string'("REFRESH"), C_REFRESH),
                             (new string'("AWAIT"), C_AWAIT),
                             (new string'("OUTENA"), C_OUTENA),
                             (new string'("READ"), C_READ),
                             (new string'("WRITE"), C_WRITE),
                             (new string'("DELAY"), C_DELAY));
        variable cmd_str : string(1 to 20);
        variable cmd        : cmd_type;
        variable len        : natural;
    begin
        string_read(ln_in, cmd_str, len);
        cmd_compare.lookup(cmds, cmd_str(1 to len), cmd);
        
        assert false report "command=" & cmd_type'image(cmd) severity note;
    end procedure tx_ddr3_cmd;

    --
    -- execute_ascii_file(<source ascii file name>)
    --
    -- Opens the ASCII file and scans for the '@' symbol.
    -- After each '@' symbol, a string is read as a command function.
    -- Each function then goes through a 'case command_in' which then executes
    -- the appropriate functions
    --
    procedure execute_ascii_file(source_file_name : string) is
        type cmd_type is (C_RESET, C_WAIT_IN_READY, C_LOG_FILE, C_END_LOG_FILE,
                              C_CMD, C_STOP, C_END, C_NO_COMMAND);
        package cmd_compare is new work.generic_compare generic map (enum => cmd_type);                      
        variable commands           : cmd_compare.assoc_array(0 to 6) :=
                                      ((new string'("RESET"), C_RESET),
                                       (new string'("WAIT_IN_READY"), C_WAIT_IN_READY),
                                       (new string'("LOG_FILE"), C_LOG_FILE),
                                       (new string'("END_LOG_FILE"), C_END_LOG_FILE),
                                       (new string'("CMD"), C_CMD),
                                       (new string'("STOP"), C_STOP),
                                       (new string'("END"), C_END));

        variable fin_running,
                 r                  : integer;
        variable message_string,
                 destination_file_name,
                 bmp_file_name      : line;
        variable draw_color         : natural range 0 to 255;
        variable line_number        : natural;
        file fin                    : text open READ_MODE is source_file_name;
        variable in_ln              : line;
        variable out_ln             : line;
        file fout                   : text;
        variable s                  : string(1 to 13);
        variable c                  : cmd_type;
        variable cmd                : string(1 to 20);
        variable cmd_len            : natural;
    begin
        line_number := 1;
        -- fout := output;

        assert false report source_file_name & string'(" opened") severity note;
        while not endfile(fin) loop
            readline(fin, in_ln);
            -- assert false report "in_ln=" & in_ln.all severity note;
           
            if in_ln /= null then

                string_read(in_ln, cmd, cmd_len);
                
                -- assert false report "cmd=" & cmd & "cmd_len=" & integer'image(cmd_len) severity note;
                if cmd(1) = '@' then
                    -- c := to_command(cmd(2 to cmd_len));
                    cmd_compare.lookup(commands, cmd(2 to cmd_len), c);
                    assert false report "cmd=" & cmd_type'image(c) severity note;
                
                    case c is
                        when C_CMD =>
                            assert false report "CMD requested" severity note;
                            tx_ddr3_cmd(in_ln, line_number);
                        when C_RESET =>
                            --script_line := line_number;
                            --script_cmd := command_in;
                            --send_rst;
                            assert false report "RESET requested" severity note;
                        when C_WAIT_IN_READY =>
                            assert false report "WAIT_IN_READY requested" severity note;
                        when C_LOG_FILE =>
                            assert false report "LOG_FILE requested" severity note;
                        when C_END_LOG_FILE =>
                            assert false report "END_LOG_FILE requested" severity note;
                        when C_STOP =>
                            assert false report "simulation STOP requested" severity note;
                        when C_END =>
                            assert false report "simulation END reqested" severity note;
                            std.env.stop(0);
                        when others => 
                            null;
                    end case;
                end if;
            end if;
            line_number := line_number + 1;
        end loop;   
        std.env.stop(0);        
    end procedure execute_ascii_file;

    constant DDR3_NUM_CK            : integer := DDR3_NUM_CHIPS;
    constant USE_TOGGLE_ENA         : boolean := false;
    constant USE_TOGGLE_OUT         : boolean := false;
    constant WDT_RESET_TIME         : natural := 16;
    constant SYS_IDLE_TIME          : natural := WDT_RESET_TIME - 8;

    type DDR_CMD_NAME is (MRS, REF, PRE, ACT, WRI, REA, ZQC, NOP,
                          XOP1, XOP2, XOP3, XOP4, XOP5, XOP6, XOP7, NOP2);

    constant tb_command_script_file : string := "DDR3_CMD_SEQ_script.txt";
    constant script_cmd             : string := "*** POWER UP ***";
    constant period                 : time := 500000 ns / CLK_MHZ_IN;    
    signal script_line              : std_ulogic_vector(12 downto 0);

    signal auto_wait                : boolean := false;

    signal in_busy                  : std_ulogic;
    signal in_rd_vector             : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
    signal i_cmd_ack                : std_ulogic;

    signal rst_in                   : std_ulogic;
    signal cmd_clk                  : std_ulogic := '1';
    signal in_ena, in_wena          : std_ulogic;
    signal in_bank                  : std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
    signal in_ras                   : std_ulogic_vector(DDR3_WIDTH_ADDR - 1 downto 0);
    signal in_cas                   : std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
    signal in_wdata                 : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
    signal in_wmask                 : std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
    signal in_vector                : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);     -- embed multiple read request returns into the in_rd_vector
    signal cmd_ack                  : std_ulogic;
    signal cmd_ready                : std_ulogic;
    signal cmd_cmd                  : std_ulogic_vector(3 downto 0);   -- DDR3 command out (CS#, RAS#, CAS#, WE#)
    signal cmd_name                 : string(1 to 255);
    signal cmd_bank                 : std_ulogic_vector(DDR3_WIDTH_BANK - 1 downto 0);
    signal cmd_a                    : std_ulogic_vector(DDR3_WIDTH_ADDR - 1 downto 0);
    signal cmd_wdata                : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
    signal cmd_wmask                : std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
    signal cmd_vector               : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
        
    signal ref_req                  : std_ulogic;
    signal ref_ack                  : std_ulogic;
    signal idle                     : std_ulogic;
    signal wdt_counter              : natural range 0 to 255;
begin
    b_cmd_sequencer : block
        signal out_txb              : std_ulogic_vector(7 downto 0);
        signal out_read_ready       : std_ulogic;
        signal read_cal_pat_t,
               read_cal_pat_v       : std_ulogic;
        signal out_read_data        : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
    begin
        i_cmd_ack <= cmd_ack;
        i_cmd_sequencer : BrianHG_DDR3_CMD_SEQUENCER
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

    cmd_clk <= not cmd_clk after period when not rst_in;

    -- initial begin
    initial_begin : process
    begin
        wdt_counter <= WDT_RESET_TIME;
        in_ena <= '0';
        in_wena <= '0';
        in_bank <= (others => '0');
        in_ras <= (others => '0');
        in_cas <= (others => '0');
        in_wdata <= (others => '0');
        in_wmask <= (others => '0');
        in_vector <= (others => '0');
        cmd_ack <= '0';
        auto_wait <= false;
        ref_req <= '0';

        rst_in <= '1';
        wait for 50 ns;
        rst_in <= '0';

        while (true) loop
            wait until rising_edge(cmd_clk);
            assert false report "call execute_ascii_file" severity note;
            execute_ascii_file(TB_COMMAND_SCRIPT_FILE);
        end loop;
        wait;
    end process initial_begin;    
end architecture sim;
