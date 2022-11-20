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
    
    subtype str80 is string(1 to 80);
    
    function strpad(s : string; l : natural) return string is
        variable r : string(1 to l);
    begin
        for i in s'range loop
            r(i) := s(i);
        end loop;
        for i in s'right to r'right loop
            r(i) := ' ';
        end loop;
        return r;
    end function strpad;
    --
    -- tx_ddr3_cmd(src, dest, ln)
    --
    -- tx the ddr3 command_in
    --
    procedure tx_ddr3_cmd(variable ln_in : line; line_number : natural) is
    begin
        assert false report "in_ln=" & ln_in.all severity note;
    end procedure tx_ddr3_cmd;
    
    --
    -- execute_ascii_file(<source ascii file name>)
    --
    -- opens the ASCII file and scans for the '@' symbol.
    -- After each '@' symbol, a string is read as a command function.
    -- Each function then goes through a 'case command_in' which then executes
    -- the appropriate functions
    --
    procedure execute_ascii_file(source_file_name : string) is
        variable fin_running,
                 r                  : integer;
        variable command_in,
                 message_string,
                 destination_file_name,
                 bmp_file_name      : line;
        variable char               : character;
        variable draw_color         : natural range 0 to 255;
        variable line_number        : natural;
        file fin                    : text open READ_MODE is source_file_name;
        variable in_ln              : line;
        variable out_ln             : line;
        file fout                   : text;
        variable str_index          : natural := 1;
        variable cmd_start, cmd_end : natural;
        variable s, d               : string(1 to 13);
    begin
        line_number := 1;
        -- fout := output;
        
        assert false report source_file_name & string'(" opened") severity note;
        while not endfile(fin) loop
            readline(fin, in_ln);
            if in_ln /= null then
                cmd_start := 0;
                cmd_end := 0;
                for i in in_ln'range loop
                    if in_ln(i) = '@' then
                        cmd_start := i + 1;
                    elsif cmd_start > 0 and in_ln(i) = ' ' then
                        cmd_end := i - 1;
                        exit;
                    end if;
                end loop;
                if cmd_start < cmd_end then
                    command_in := new string(1 to cmd_end - cmd_start + 1);
                    command_in.all := in_ln(cmd_start to cmd_end);
                    assert false report "cmd=" & command_in.all severity note;
                end if;
                
                if command_in /= null then
                    s := strpad(command_in.all, s'length);
                    deallocate(command_in);
                    case s is
                        when "CMD          " =>
                            tx_ddr3_cmd(in_ln, line_number);
                        when "RESET        " =>
                            --script_line := line_number;
                            --script_cmd := command_in;
                            --send_rst;
                            assert false report "reset" severity note;
                        when "WAIT_IN_READY" =>
                            --
                        when "LOG_FILE     " =>
                            --
                        when "END_LOG_FILE " =>
                            --
                        when "STOP         " =>
                            --
                        when "END          " =>
                            --
                        when others => null;
                    end case;
                end if;
            end if;
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
