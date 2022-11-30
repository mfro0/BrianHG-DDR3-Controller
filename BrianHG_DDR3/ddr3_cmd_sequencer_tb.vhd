library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use std.textio.all;

package mf_utils is
    procedure hread(variable ln : inout line; num : out integer; good : out boolean);
    procedure hread(variable ln : inout line; num : out integer);
    alias hex_read is hread [line, integer, boolean];
    alias hex_read is hread [line, integer];
end package mf_utils;

package body mf_utils is
    --
    -- implement hread() (hexadecimal read for integers)
    -- that is missing from the std.textio package.
    --
    -- Works for upper and lower case hex digits.
    -- Skips leading whitespace, stops reading when it detects an
    -- invalid hex character.
    --
    procedure hread(variable ln : inout line; num : out integer; good : out boolean) is
        constant hvals      : string := "0123456789ABCDEF0123456789abcdef";
        variable c          : character;
        variable maxdepth   : natural;

        --
        -- translate a single hex character ('0'-'f', '0'-'F')
        -- into its integer equivalent. Return -1 if character isn't
        -- identified as valid hex digit.
        --
        function hval(c : character) return integer is
        begin
            for i in hvals'range loop
                if hvals(i) = c then
                    return (i - 1) mod 16;
                end if;
            end loop;
            return -1;
        end;

        --
        -- check if character is a hex digit.
        --
        function is_hex(c : character) return boolean is
        begin
            return hval(c) /= -1;
        end function is_hex;

        --
        -- the recursive part of hread. Collects single digit's numerical
        -- values in string in a recursive descend until it hits a non-hex character. 
        -- Then, on ascent of the recursive call, adds up all the collected
        -- digits multiplied by their digit position's valence.
        --
        -- TODO: no active check for overflow
        --
        impure function recursive_hread(depth : natural) return integer is
            variable num    : integer;
            variable c      : character;
        begin
            -- peek into the line buffer to see if there is another hex character
            if ln.all'length > 0 and is_hex(ln.all(1)) then                 -- there is still another hex character to be read
                read(ln, c);
                num := hval(c);                                             -- convert it into a number
                -- assert false report "read a " & c & " character, value is " & integer'image(num) & " depth is " & integer'image(depth) severity note;
                -- recursively call ourselves until no more characters to read, then on ascent,
                -- add up what we found
                return recursive_hread(depth + 1) + num * 16 ** (maxdepth - depth);
            else
                -- walk on, nothing to see here
                maxdepth := depth - 1;
                return 0;
            end if;
        end function recursive_hread;

    begin   -- hread
        -- skip leading white space
        if ln.all'length > 0 then
            for i in ln.all'range loop
                if ln.all(1) = ' ' or ln.all(1) = HT then
                    read(ln, c);
                else
                    exit;
                end if;
            end loop;
            -- call recursive part
            num := recursive_hread(1);
            -- error checking
            if maxdepth < 1 then       -- did we read at least a single hex character?
                good := false;
            else 
                good := true;
            end if;
            -- assert false report "hread: " & ln.all & " maxdepth was " & integer'image(maxdepth) severity note;
        end if;
    end hread;

    -- the same as above but no error checking
    procedure hread(variable ln : inout line; num : out integer) is
        variable good : boolean;
    begin
        hread(ln, num, good);
    end hread;
end package body mf_utils;
--
-- generic package to provide universal keyword lookup
--
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
use work.mf_utils.all;

entity ddr3_cmd_sequencer_tb is
    generic
    (
        -- system clock generation and operation
        CLK_MHZ_IN              : natural := 400;

        -- DDR3 ram chip configuration settings
        DDR3_WIDTH_DQ           : natural := 8;
        DDR3_NUM_CHIPS          : natural := 1;

        DDR3_WIDTH_ADDR         : natural := 15;
        DDR3_WIDTH_BANK         : natural := 3;
        DDR3_WIDTH_CAS          : natural := 10;
        DDR3_RWDQ_BITS          : natural := 64; -- DDR3_WIDTH_DQ * DDR3_NUM_CHIPS * 8;

        -- DDR3 controller configuration parameter settings
        PORT_VECTOR_SIZE        : natural := 16
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
    -- define a protected counter type to implement the watchdog as a shared variable
    -- This allows us to drive the same variable from different processes safely.
    --
    type wdt_counter_type is protected
        procedure reset;
        procedure decrement;
        impure function get_counter return integer;
    end protected wdt_counter_type;

    constant WDT_RESET_TIME         : natural := 16;
    
    type wdt_counter_type is protected body
        variable wdt_counter        : natural range 0 to 255 := 0;
        procedure reset is
        begin
            wdt_counter := WDT_RESET_TIME;
        end procedure reset;
        procedure decrement is
        begin
            wdt_counter := wdt_counter - 1;
        end procedure decrement;
        impure function get_counter return natural is
        begin
            return wdt_counter;
        end function get_counter;
    end protected body wdt_counter_type;

    constant DDR3_NUM_CK            : natural := DDR3_NUM_CHIPS;
    constant USE_TOGGLE_ENA         : boolean := false;
    constant USE_TOGGLE_OUT         : boolean := false;
    constant SYS_IDLE_TIME          : natural := WDT_RESET_TIME - 8;

    type DDR_CMD_NAME is (MRS, REF, PRE, ACT, WRI, REA, ZQC, NOP,
                          XOP1, XOP2, XOP3, XOP4, XOP5, XOP6, XOP7, NOP2);

    constant tb_command_script_file : string := "DDR3_CMD_SEQ_script.txt";
    constant script_cmd             : string := "*** POWER UP ***";
    constant period                 : time := 500000 ns / CLK_MHZ_IN;    
    signal script_line              : natural;

    signal auto_wait                : boolean := false;

    signal in_busy                  : std_ulogic;
    signal in_rd_vector             : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
    signal i_cmd_ack                : std_ulogic;

    signal rst_in                   : std_ulogic := '1';
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
begin -- architecture
    
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

    cmd_clk <= not cmd_clk after period;

    p_watchdog : process
        variable wdt_counter : wdt_counter_type;
    begin
        wait until rising_edge(cmd_clk);
        if in_ena or rst_in or not idle then
            wdt_counter.reset;
        else
            wdt_counter.decrement;
        end if;
    end process;

    -- initial begin
    initial_begin : process
        procedure send_rst is
            variable wdt_counter : wdt_counter_type;
        begin
            assert false report "send_rst:" severity note;
            wait until falling_edge(cmd_clk);
            rst_in <= '1';
            -- keep in reset for four clock cycles
            wait until falling_edge(cmd_clk);
            wait until falling_edge(cmd_clk);
            wait until falling_edge(cmd_clk);
            wait until falling_edge(cmd_clk);
            rst_in <= '0';
            wait until falling_edge(cmd_clk);
        end procedure send_rst;

        --
        -- wait_rdy
        -- wait for dut_geoff input buffer ready
        --
        procedure wait_rdy is
        begin
            if not auto_wait then
                wait until falling_edge(cmd_clk);       -- wait for busy to clear
            else
                while in_busy loop
                    wait until falling_edge(cmd_clk);
                end loop;
            end if;
        end procedure wait_rdy;

        --
        -- txcmd(dest, msg, ln)
        --
        procedure txcmd is
        begin
            if auto_wait then
                wait_rdy;
            end if;
            in_ena <= '1';
            wait until falling_edge(cmd_clk);
            in_ena <= '0';
        end procedure txcmd;

        
        --
        -- tx_ddr3_cmd(src, dest, ln)
        --
        -- tx the ddr3 command_in
        --
        procedure tx_ddr3_cmd(variable ln_in : inout line; line_number : natural) is
            variable wdt_counter : wdt_counter_type;

            type cmd_type is (C_REFRESH, C_AWAIT, C_OUTENA, C_READ, C_WRITE, C_DELAY);
 
            --
            -- instantiate generic package with our specific enum type
            --
            package cmd_compare is new work.generic_compare generic map (enum => cmd_type);

            --
            -- provide the assoc list (unfortunately, this can't be a constant because
            -- of the dynamic allocation that is required to have variable length string fields)
            --
            variable cmds : cmd_compare.assoc_array(0 to 5) :=
                                (cmd_compare.assoc'(new string'("REFRESH"), C_REFRESH),
                                 cmd_compare.assoc'(new string'("AWAIT"), C_AWAIT),
                                 cmd_compare.assoc'(new string'("OUTENA"), C_OUTENA),
                                 cmd_compare.assoc'(new string'("READ"), C_READ),
                                 cmd_compare.assoc'(new string'("WRITE"), C_WRITE),
                                 cmd_compare.assoc'(new string'("DELAY"), C_DELAY));
            variable cmd_str    : string(1 to 20);  -- string length must be at least length of longest cmd string
            variable cmd        : cmd_type;
            variable len        : natural;
            variable number     : natural;
            variable sl         : bit;
        begin
            string_read(ln_in, cmd_str, len);

            cmd_compare.lookup(cmds, cmd_str(1 to len), cmd);
        
            -- assert false report "command=" & cmd_type'image(cmd) severity note;

            case cmd is
                when C_REFRESH =>
                    ref_req <= not ref_req;
            
                when C_AWAIT =>
                    hread(ln_in, number);
                    -- convert integer (0, 1) to boolean
                    auto_wait <= boolean'val(number);

                when C_OUTENA =>
                    read(ln_in, sl);
                    cmd_ack <= to_stdulogic(sl);

                when C_READ =>
                    hread(ln_in, number);
                    in_bank <= std_ulogic_vector(to_unsigned(number, in_bank'length));
                    hread(ln_in, number);
                    in_ras <= std_ulogic_vector(to_unsigned(number, in_ras'length));
                    hread(ln_in, number);
                    in_cas <= std_ulogic_vector(to_unsigned(number, in_cas'length));
                    hread(ln_in, number);
                    in_vector <= std_ulogic_vector(to_unsigned(number, in_vector'length));
                
                    -- new signal values will only been taken at next clock edge
                    -- so we just wait for it here
                    wait until rising_edge(cmd_clk);
                    assert false report "read bank: " & to_hstring(in_bank) &
                                        " ras: " & to_hstring(in_ras) &
                                        " cas: " & to_hstring(in_cas) &
                                        " to vector: "  & to_hstring(in_vector)
                                        severity note;
                    in_wena <= '0';
                    txcmd;     
            
                when C_WRITE =>
                    hread(ln_in, number);
                    in_bank <= std_ulogic_vector(to_unsigned(number, in_bank'length));

                    hread(ln_in, number);
                    in_ras <= std_ulogic_vector(to_unsigned(number, in_ras'length));
                    
                    hread(ln_in, number);
                    in_cas <= std_ulogic_vector(to_unsigned(number, in_cas'length));
                    
                    hread(ln_in, number);
                    in_wmask <= std_ulogic_vector(to_unsigned(number, in_wmask'length));
                    
                    hread(ln_in, number);
                    in_wdata <= std_ulogic_vector(to_unsigned(number, in_wdata'length));

                    wait until rising_edge(cmd_clk);    -- see above
                    assert false report "write bank: " & to_hstring(in_bank) &
                                        " ras: " & to_hstring(in_ras) &
                                        " cas: " & to_hstring(in_cas) &
                                        " wmask: " & to_hstring(in_wmask) &
                                        " wdata: " & to_hstring(in_wdata) severity note;
                    txcmd;
                
                when C_DELAY =>
                    read(ln_in, number);
                    assert false report "Delaying for " & integer'image(number) &
                                        " clock cycle(s)" severity note;
                    for i in 1 to number loop
                        wait until falling_edge(cmd_clk);
                        wdt_counter.reset;
                    end loop;

                when others =>
                    wait_rdy;
                    while wdt_counter.get_counter > SYS_IDLE_TIME loop
                        wait until falling_edge(cmd_clk);
                    end loop;
                    assert false report "unknown command" severity warning;
                    while wdt_counter.get_counter >= 2 loop
                        wait until falling_edge(cmd_clk);
                    end loop;
                    wait until falling_edge(cmd_clk);
                    std.env.stop(0);
            end case;
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
                                      (cmd_compare.assoc'(new string'("RESET"), C_RESET),
                                       cmd_compare.assoc'(new string'("WAIT_IN_READY"), C_WAIT_IN_READY),
                                       cmd_compare.assoc'(new string'("LOG_FILE"), C_LOG_FILE),
                                       cmd_compare.assoc'(new string'("END_LOG_FILE"), C_END_LOG_FILE),
                                       cmd_compare.assoc'(new string'("CMD"), C_CMD),
                                       cmd_compare.assoc'(new string'("STOP"), C_STOP),
                                       cmd_compare.assoc'(new string'("END"), C_END));

            variable line_number        : natural;
            file fin,
                 fout                   : text;
            variable in_ln              : line;
            variable c                  : cmd_type;
            variable cmd                : string(1 to 20);
            variable cmd_len            : natural;
        begin
            line_number := 1;
            file_open(fin, source_file_name, read_mode);
            -- fout := output;

            assert false report source_file_name & string'(" opened") severity note;
            while not endfile(fin) loop
                readline(fin, in_ln);
                -- assert false report "in_ln=" & in_ln.all severity note;
           
                if in_ln /= null then
                    cmd := (others => ' ');
                    string_read(in_ln, cmd, cmd_len);
                
                    -- assert false report "cmd=" & cmd & "cmd_len=" & integer'image(cmd_len) severity note;
                    if cmd(1) = '@' then
                        cmd_compare.lookup(commands, cmd(2 to cmd_len), c);
                        -- assert false report "execute_ascii_file: cmd=" & cmd_type'image(c) & " (" & cmd & ")" severity note;
                
                        case c is
                            when C_CMD =>
                                assert false report "CMD requested" severity note;
                                tx_ddr3_cmd(in_ln, line_number);
                            when C_RESET =>
                                script_line <= line_number;
                                -- script_cmd <= cmd;
                                send_rst;
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
                                assert false report "normal exit." severity note;
                                std.env.stop(0);
                            when others => 
                                assert false report "illegal command " & cmd severity error;
                        end case;
                    end if;
                end if;
                line_number := line_number + 1;
            end loop;   
            std.env.stop(0);        
        end procedure execute_ascii_file;

        variable wdt_counter : wdt_counter_type;

    begin   -- process initial_begin
        wdt_counter.reset;
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

        assert false report "call execute_ascii_file" severity note;
        execute_ascii_file(TB_COMMAND_SCRIPT_FILE);
        wait;
    end process initial_begin;
        
end architecture sim;
