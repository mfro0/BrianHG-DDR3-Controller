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
        in_bank             : in natural range 0 to 2 ** DDR3_WIDTH_BANK - 1;
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
        bank            : natural range 0 to 2 ** DDR3_WIDTH_BANK - 1;
        ras             : std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
        cas             : std_ulogic_vector(DDR3_WIDTH_CAS - 1 downto 0);
        wdata           : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0);
        wmask           : std_ulogic_vector(DDR3_RWDQ_BITS / 8 - 1 downto 0);
        ref_req         : std_ulogic;
    end record;

    type pipeline_register_array_type is array(integer range <>) of pipeline_register_type;
    signal s   : pipeline_register_array_type(1 to 4) :=
        (
            /* 1 */ ('0', 0, (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 2 */ ('0', 0, (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 3 */ ('0', 0, (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0'),
            /* 4 */ ('0', 0, (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0')
        );
    signal s1_ready,
           s2_ready,
           s3_ready,
           s4_ready     : std_ulogic := '0';
    signal s1_ack,
           s2_ack,
           s3_ack,
           s4_ack       : std_ulogic := '0';
    signal s1_load,
           s2_load,
           s3_load,
           s4_load      : std_ulogic := '0';
    signal s1_busy,
           s4_busy      : std_ulogic := '0';

    subtype bank_row_mem_type is std_ulogic_vector(DDR3_WIDTH_ROW - 1 downto 0);
    type bank_row_mem_type_array is array(0 to 2 ** DDR3_WIDTH_BANK - 1) of bank_row_mem_type;

    signal bank_row_mem : bank_row_mem_type_array := (others => (others => '0'));
    attribute preserve : boolean;
    attribute preserve of bank_row_mem : signal is true;
    signal bank_mem_in_compare,
           row_in_compare       : std_ulogic_vector(15 downto 0);

    signal s3_bank_match        : std_ulogic := '0';
    attribute preserve of s3_bank_match : signal is true;

    signal phold                : std_ulogic := '0';

    signal bank_act             : std_ulogic_vector(2 ** DDR3_WIDTH_BANK - 1 downto 0) := (others => '0');
    attribute preserve of bank_act : signal is true;

    signal bank_act_any : std_ulogic;
    attribute preserve of bank_act_any : signal is true;

    signal idle_counter : natural range 0 to 31 := 0;
    signal idle_reset   : std_ulogic := '0';

    signal ref_req,
           in_ref_req,
           in_ref_lat,
           in_ref_lat2,
           ref_hold     : std_ulogic := '0';

    signal in_read_rdy_tdl,
           s4_ready_t   : std_ulogic := '0';

    signal in_ena_dl,
           busy_t,
           in_busy_int,
           in_ena_int,
           s4_ack_t     : std_ulogic := '0';

    subtype b4 is std_ulogic_vector(3 downto 0);
    constant CMD_MRS    : b4 := std_ulogic_vector(to_unsigned(0, b4'length)); 
    constant CMD_REF    : b4 := std_ulogic_vector(to_unsigned(1, b4'length)); 
    constant CMD_PRE    : b4 := std_ulogic_vector(to_unsigned(2, b4'length)); 
    constant CMD_ACT    : b4 := std_ulogic_vector(to_unsigned(3, b4'length)); 
    constant CMD_WRI    : b4 := std_ulogic_vector(to_unsigned(4, b4'length)); 
    constant CMD_REA    : b4 := std_ulogic_vector(to_unsigned(5, b4'length)); 
    constant CMD_ZQC    : b4 := std_ulogic_vector(to_unsigned(6, b4'length)); 
    constant CMD_NOP    : b4 := std_ulogic_vector(to_unsigned(15, b4'length));

    type cmd_type is (TX_PREA_ALL, TX_REFRESH, TX_PREA, TX_ACTIVATE, TX_READ, TX_WRITE);

    subtype b8 is std_ulogic_vector(7 downto 0);
    constant TXB_MRS        : b8 := "00000001";
    constant TXB_REF        : b8 := "00000010";
    constant TXB_PRE        : b8 := "00000100";
    constant TXB_ACT        : b8 := "00001000";
    constant TXB_WRI        : b8 := "00010000";
    constant TXB_REA        : b8 := "00100000";
    constant TXB_ZQC        : b8 := "01000000";
    constant TXB_NOP        : b8 := "10000000";         -- device NOP + deselect

    signal vect_shift_out   : std_ulogic;
    signal vect_fifo_data_out   : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);

    signal reset_latch,
           reset_latch2         : std_ulogic;
    attribute preserve of reset_latch, reset_latch2 : signal is true;

    signal rcp_h                : std_ulogic_vector(3 downto 0) := (others => '0');
    signal rcp_l                : std_ulogic_vector(3 downto 0) := (others => '0');

    signal out_read_ready_p     : std_ulogic;
    signal out_read_data_p      : std_ulogic_vector(DDR3_RWDQ_BITS - 1 downto 0) := (others => '0');
    signal out_rd_vector_p      : std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0) := (others => '0');

    --
    -- FIFO ram for the vector feed through.
    --
    subtype vec_t is std_ulogic_vector(PORT_VECTOR_SIZE - 1 downto 0);
    type vec_mem_t is array(0 to 15) of vec_t;
    signal vector_pipe_mem      : vec_mem_t := (others => (others => '0'));
    signal out_rd_vector_int    : vec_t := (others => '0');
    signal vwpos, vrpos         : natural range 0 to 15;
    signal load_vect            : std_ulogic := '0';
    signal vect_data_dl         : vec_t := (others => '0');
begin -- architecture
    p_comb : process(all)
    begin
        -- read and load flags must be sorted in reverse order to simulate properly.
        -- This combinational logic generates the FIFO shifting pipe processor.
        --

        --
        -- for stage 4, this generates a BUSY flag meaning that other commands need to be inserted into the output
        -- before S3 should be allowed to send additional commands.
        --
        if in_ref_lat /= in_ref_lat2 then ref_req <= '1'; else ref_req <= '0'; end if;

        if s3_ready then
            if bank_act_any and s(3).ref_req then s4_busy <= '1';
            elsif not bank_act_any and s(3).ref_req then s4_busy <= '0';
            elsif not(s3_bank_match) and bank_act(s(3).bank) and not phold then s4_busy <= '1';
            elsif not bank_act(s(3).bank) then s4_busy <= '1';
            end if;
        else
            s4_busy <= '0';
        end if;

        s4_ack_t <= out_ack;

        -- translate the toggle input ack to positive logic logic ack.
        if not USE_TOGGLE_OUT then
            s4_ack <= out_ack;
        elsif s4_ack = s4_ready_t then
            s4_ack <= '1';
        else
            s4_ack <= '0';
        end if;

        -- assign the earlier stage flags top operate like a FIFO or elastic buffer which shows the output in
        -- advance of the '_read' signal.
        s4_load <= s3_ready and (not s4_ready or (s4_load and not s4_busy));
        s3_ack <= s3_load;

        s2_load <= s1_ready and (not s2_ready or s3_load);
        s1_ack <= s2_load;
        s1_busy <= s1_ready and not s1_ack;

        -- assign i/o ports
        in_busy_int <= s1_busy or ref_hold;

        if USE_TOGGLE_ENA then
            in_busy <= busy_t;
        else
            in_busy <= in_busy_int;
        end if;

        if USE_TOGGLE_ENA then
            if in_ena /= in_ena_dl and not (in_busy_int = '1') then
                in_ena_int <= '1';
            end if;
        else
            in_ena_int <= in_ena;
        end if;

        s1_load <= (in_ena_int and not s1_busy) or in_ref_req;

        if not USE_TOGGLE_OUT then
            out_ready <= s4_ready;          -- assign the output ready flag to s4_ready
        else
            out_ready <= s4_ready_t;        -- assign the toggle version of the output ready
        end if;

        row_in_compare <= s(2).ras;
    end process p_comb;

    gen: if EXTRA_SPEED generate
        p_proc : process
        begin
            wait until rising_edge(clk);
            out_read_data <= out_read_data_p;
            out_rd_vector <= out_rd_vector_p;
            out_read_ready <= out_read_ready_p;
        end process p_proc;
    else generate
        out_read_data <= out_read_data_p;
        out_rd_vector <= out_rd_vector_p;
        out_read_ready <= out_read_ready_p;
    end generate gen;

    p_pipeline : process
        procedure set_cas is
        begin
            out_a(9 downto 0) <= s(3).cas(9 downto 0);      -- column address at the beginning of a sequential burst
            if DDR3_WIDTH_CAS = 10 then
                out_a(11) <= '0';                           -- default 0 for additional column address
            else
                out_a(11) <= s(3).cas(10);                  -- assign the additional MSB column address ised in 4 bit DDR3 devices
            end if;
            out_a(10) <= '0';                               -- disable auto-precharge. We keep the banks open and precharge manually only when needed
            out_a(12) <= '1';                               -- set burst length to BL 8
            out_a(DDR3_WIDTH_ROW - 1 downto 13) <= (others => '0');
        end procedure set_cas;

        procedure send_cmd(cmd : cmd_type) is
        begin
            case cmd is
                when TX_REFRESH =>
                    phold <= '0';                   -- no longer needed
                    out_cmd <= CMD_REF;
                    out_txb <= TXB_REF;
                    out_refresh_ack <= in_ref_lat2;
                
                when TX_PREA_ALL =>
                    phold <= '0';                   -- no longer needed
                    out_cmd <= CMD_PRE;
                    out_txb <= TXB_PRE;
                    bank_act <= (others => '0');    -- deactivate all banks
                    bank_act_any <= '0';
                    out_a(10) <= '1';               -- all bank precharge
                
                when TX_PREA =>
                    phold <= '0';                   -- no longer needed
                    out_cmd <= CMD_PRE;
                    out_txb <= TXB_PRE;
                    bank_act(s(3).bank) <= '0';     -- deactivate the precharged bank
                    out_a(10) <= '0';               -- single bank precharge

                when TX_ACTIVATE =>
                    phold <= '0';                   -- no longer needed
                    bank_act(s(3).bank) <= '1';     -- activate the selected bank
                    bank_act_any <= '1';
                    out_a <= s(3).ras;              -- which row to activate
                    out_cmd <= CMD_ACT;
                    out_txb <= TXB_ACT;
                
                when TX_READ =>
                    phold <= '0';                   -- no longer needed
                    set_cas;                        -- output the CAS address on the DDR3 A bus
                    out_cmd <= CMD_REA;
                    out_txb <= TXB_REA;

                when TX_WRITE =>
                    phold <= '0';                   -- no longer needed
                    set_cas;                        -- output the CAS address on the DDR3 A bus
                    out_cmd <= CMD_WRI;
                    out_txb <= TXB_WRI;
            end case;
        end procedure send_cmd;

    begin
        wait until rising_edge(clk);
        reset_latch <= reset;
        reset_latch2 <= reset_latch;

        --
        -- manage read data and read-calibration test pattern.
        -- Always latch read data to output regardless of reset so that the power-up sequence may analyze the read-calibration pattern
        --
        in_read_rdy_tdl <= in_read_rdy_t;       -- detect toggle change
        read_cal_pat_t <= in_read_rdy_tdl;      -- toggle the read cal pattern
        
        if in_read_rdy_t /= in_read_rdy_tdl then
            out_read_data_p <= in_read_data;
        end if;

        for i in 0 to 3 loop
            rcp_h(i) <= out_read_data_p(i * 2 * cal_width to cal_width);

            -- TODO: translate that mess
        end loop;
        if rcp_h = "1111" and rcp_l = "1111" then
            read_cal_pat_v <= '1';
        else 
            read_cal_pat_v <= '0';
        end if;

        --
        -- vector FIFO memory FMAX accelerator by isolating its read inside a 2nd layer LC for write and read
        --
        load_vect <= s1_load and not in_ref_req and not in_wena;
        vect_data_dl <= in_rd_vector;
        out_rd_vector_int <= vector_pipe_mem(vrpos);    -- add a dff latch stage to help improve FMAX performance
        if in_read_rdy_t /= in_read_rdy_tdl then
            out_rd_vector_p <= out_rd_vector_int;       -- select the dff latch stage
        end if;

        if reset_latch2 then
            bank_act <= (others => '0');
            bank_act_any <= '0';
            phold <= '0';

            s1_ready <= '0';
            s2_ready <= '0';
            s3_ready <= '0';
            s4_ready <= '0';
            s4_ready_t <= '0';

            ref_hold <= '0';
            in_ref_req <= '0';
            in_ref_lat <= in_refresh_t;
            in_ref_lat2 <= in_ref_lat;
            out_refresh_ack <= in_ref_lat2;

            out_cmd <= CMD_NOP;
            out_txb <= (others => '0');

            out_read_ready_p <= '0';

            in_ena_dl <= in_ena;
            busy_t <= in_ena;

            vwpos <= 0;
            vrpos <= 0;
        else
            --
            -- logic for handling in_ena/in_busy if 'USE_TOGGLE_ENA' is enabled
            --

            if not in_busy_int then
                in_ena_dl <= in_ena;
            end if;
            if not in_busy_int then
                busy_t <= in_ena;
            end if;

            --
            -- generate a output which goes high after 32 clocks of nothing happening
            --
            idle_reset <= s2_ready;         -- make sure a refresh doesn't affect the idle timer
            if idle_reset then
                idle_counter <= 0;
            elsif idle_counter < 31 then
                idle_counter <= idle_counter + 1;
            end if;
            out_idle <= '0' when idle_counter < 31 else '1';

            -- latch refresh request.
            in_ref_lat <= in_refresh_t;

            --
            -- refresh management. Sets up a sequence of events where external normal command input is
            -- halted, wait for the halt to clear the last command, send a refresh down the pipe,
            -- then release the halt.
            --

            -- prevent refresh load if an external command is being sent, yet allow the insert of
            -- the refresh during an existing busy port generated by the external commands saturating
            -- the input. This way, no request commands will be lost if a refresh and input request
            -- come in at the same time.

            if ref_req and not ref_hold and not s1_load then
                ref_hold <= '1';
            elsif ref_hold and not s1_ready and not in_ref_req then
                in_ref_req <= '1';
                in_ref_lat2 <= in_ref_lat;
            elsif ref_hold and in_ref_req then
                in_ref_req <= '0';
            elsif ref_hold then
                ref_hold <= '0';
            end if;

            --
            -- Stage 1. Load input into registers, copy the previous accessed current requested bank's
            -- row into the compare register and update the bank's row register with the new row request coming in.
            --
            if s1_load then
                s(1).wena <= in_wena;
                s(1).bank <= in_bank;
                s(1).ras <= in_ras;
                s(1).cas <= in_cas;
                s(1).wdata <= in_wdata;
                s(1).wmask <= in_wmask;

                -- generate s1_ready flag
                s1_ready <= '1';
            elsif s1_ack then
                s1_ready <= '0';
            end if;
            
            if s2_load then
                bank_mem_in_compare <= bank_row_mem(s(1).bank);
                bank_row_mem(s(1).bank) <= s(1).ras;

                s(2) <= s(1);

                s2_ready <= '1';
            elsif s2_ack then
                s2_ready <= '0';
            end if;

            --
            -- Stage 3. Check if all the 4 bits of s2's match are equal and coalesce that into 1 register
            --
            if s3_load then
                s(3) <= s(2);
                s(3).wmask <= s(2).wmask xor std_ulogic_vector(to_unsigned(2 ** (DDR3_RWDQ_BITS / 8) - 1,
                                                               s(3).wmask'length));
                if bank_mem_in_compare = row_in_compare and not s(2).ref_req = '1' then 
                    s3_bank_match <=  '1';
                else
                    s3_bank_match <= '0';
                end if;
                s3_ready <= '1';
            elsif s3_ack then
                s3_ready <= '0';
            end if;

            --
            -- Stage 4. Generate commands
            --
            if s4_load then
                out_bank <= std_ulogic_vector(to_unsigned(s(3).bank, out_bank'length));
                out_wdata <= s(3).wdata;
                out_wmask <= s(3).wmask;

                if bank_act_any and s(3).ref_req then
                    send_cmd(TX_PREA_ALL);
                elsif not bank_act_any and s(3).ref_req then
                    send_cmd(TX_REFRESH);
                elsif not s3_bank_match and bank_act(s(3).bank) and not phold then
                    send_cmd(TX_PREA);
                elsif not bank_act(s(3).bank) then
                    send_cmd(TX_ACTIVATE);
                elsif not s(3).wena then
                    send_cmd(TX_READ);
                else
                    send_cmd(TX_WRITE);
                end if;
            end if;

            -- generate s4_ready flag
            if s4_load or s4_busy then
                s4_ready <= '1';
            elsif s4_ack then
                s4_ready <= '0';
            end if;

            -- generate s4_ready_t flag
            if (s4_load = '1' or s4_busy = '1') and s4_ack = s4_ready_t then
                s4_ready_t <= not s4_ready_t;
            end if;

            --
            -- DDR3 read data and vector pipeline processing
            --
            if load_vect then                                   -- a valid read command has been loaded
                vector_pipe_mem(vwpos) <= vect_data_dl;         -- load vector into pipe mem
                vwpos <= vwpos + 1;                             -- increment write position
            end if;

            if in_read_rdy_t /= in_read_rdy_tdl then            -- read data from the DDR3 has returned a read
                vrpos <= vrpos + 1;                             -- increment read position
                if not USE_TOGGLE_ENA then                      
                    out_read_ready_p <= '1';                    -- no toggle, turn on for 1 clock period
                else
                    out_read_ready_p <= not out_read_ready_p;   -- toggle output mode
                end if;
            elsif not USE_TOGGLE_ENA then
                out_read_ready_p <= '0';                        -- no toggle, turn on for one clock period
            end if;
        end if;
    end process p_pipeline;
end architecture rtl;


