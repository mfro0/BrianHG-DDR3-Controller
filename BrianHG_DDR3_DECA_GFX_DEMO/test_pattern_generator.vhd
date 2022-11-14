-- *****************************************************************
-- Demo BHG Read DDR3 display picture pattern generator.
-- IE: It draws graphic images into ram.
--
-- Buttons 0 = Draw random colored snow.
-- Buttons 1 = Use a counter to draw a colored pattern.
--
-- Switch  0 = Enable/disable random ellipse drawing engine.
-- Switch  1 = N/A. (*** Used in the separate screen scroll module ***)
--
-- Version 0.5, June 25, 2021.
--
-- Written by Brian Guralnick.
-- For public use.
--
--
-- See: https://www.eevblog.com/forum/fpga/brianhg_ddr3_controller-open-source-ddr3-controller/
--
-- *****************************************************************

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;
 
entity test_pattern_generator is
    generic
    (
        PORT_ADDR_SIZE          : integer := 24;        -- must match PORT_ADDR_SIZE
        PIXEL_WIDTH             : integer := 32
    );
    port
    (
        clk_in                  : in std_ulogic;
        cmd_clk                 : in std_ulogic;
        reset                   : in std_ulogic;
        
        disp_pixel_bytes        : in natural range 0 to 7;                  -- 4=32 bit pixels, 2=16bit pixels, 1=8bit pixels.
        disp_mem_addr           : in integer;                               -- Beginning memory address of graphic bitmap pixel position 0x0.
        disp_bitmap_width       : in integer range 0 to 2 ** 16 - 1;        -- The bitmap width of the graphic in memory.
        disp_bitmap_height      : in integer range 0 to 2 ** 16 - 1;        -- The bitmap width of the graphic in memory.

        write_busy_in           : in std_ulogic;                            -- DDR3 ram read channel #1 was selected for reading the video ram.
        write_req_out           : out std_ulogic;
        write_adr_out           : out std_ulogic_vector(PORT_ADDR_SIZE - 1 downto 0);
        write_data_out          : out std_ulogic_vector(31 downto 0);
        write_mask_out          : out std_ulogic_vector(3 downto 0);
        
        buttons                 : in std_ulogic_vector(1 downto 0);         -- 2 buttons on deca board.
        switches                : in std_ulogic_vector(1 downto 0);         -- 2 switches on deca board.
        
        rnd_out                 : out std_ulogic_vector(31 downto 0)        -- Send out random number for other uses.
    );
end entity test_pattern_generator;

architecture rtl of test_pattern_generator is
    signal pixel_byte_width         : std_ulogic_vector(1 downto 0);
    signal disp_addr,
           mem_addr                 : std_ulogic_vector(PORT_ADDR_SIZE - 1 downto 0);
    signal buttons_l,
           switches_l               : std_ulogic_vector(1 downto 0);
    
    signal rnd_num                  : std_ulogic_vector(31 downto 0);
    
    signal prog_pc                  : natural range 0 to 33 := 0;
    signal counter                  : integer := 0;
    signal ucounter                 : std_ulogic_vector(31 downto 0);
    
    signal write_adr                : std_ulogic_vector(PORT_ADDR_SIZE - 1 downto 0);
    signal write_data               : std_ulogic_vector(31 downto 0);
    
    signal color,
           xycol1,
           xycol2,
           xycol3                   : std_ulogic_vector(31 downto 0) := (others => '0');
    signal xyr1,
           xyr2,
           xyr3,
           pixel_in_pipe,
           sel_draw,
           draw_ena,
           rnd_sel                  : std_ulogic;
    signal x1,
           x2,
           x3,
           y1,
           y2,
           draw_x,
           draw_y                   : signed(13 downto 0) := (others => '0');
    signal addr1                    : std_ulogic_vector(PORT_ADDR_SIZE - 1 downto 0) := (others => '0');

    constant elli_bits              : natural := 13;                -- the number of signed bits limiting Elli's x & y coordinates
    signal elli_ena,
           elli_run,
           elli_fill                : std_ulogic := '0';
    signal elli_quad                : natural range 0 to 3 := 0;
    signal elli_xc,
           elli_yc,
           elli_xr,
           elli_yr                  : signed(elli_bits - 1 downto 0) := (others => '0');
    signal elli_busy                : std_ulogic;
    signal elli_xout,
           elli_yout                : signed(elli_bits downto 0);
    signal elli_out_rdy,
           elli_done                : std_ulogic;
    signal elli_count               : integer range 0 to 2 ** 7 - 1;
    constant elli_fill_ratio        : integer := 0;
    signal read_req_cache,
           write_req_cache          : std_ulogic;
    signal pixel_cache_busy,
           pixel_cache_empty        : std_ulogic;
    signal pixel_cache_full         : std_ulogic;
    signal pixel_byte_shift         : natural range 0 to 4;
    signal dataout                  : std_ulogic_vector(PORT_ADDR_SIZE + PIXEL_WIDTH - 1 downto 0);
begin
    write_mask_out <= (others => '1');
    
    i_rnd : entity work.rnd
        port map
        (
            clk         => cmd_clk,
            rst         => reset,
            ena         => '1',
            load        => '0',
            seed        => (others => '0'),
            std_ulogic_vector(rout)        => rnd_num
        );
    
    rnd_out <= rnd_num;
    
    -- generate a write pixel cache port
    write_pixel_cache : component altera_mf.altera_mf_components.scfifo
        generic map
        (
            add_ram_output_register         => "ON",
            almost_full_value               => 1000,
            intended_device_family          => "MAX 10",
            lpm_numwords                    => 1024,
            lpm_showahead                   => "ON",
            lpm_type                        => "scfifo",
            lpm_width                       => (PORT_ADDR_SIZE + PIXEL_WIDTH),
            lpm_widthu                      => 10,
            overflow_checking               => "ON",
            underflow_checking              => "ON",
            use_eab                         => "ON"
        )
        port map
        (
            clock                           => cmd_clk,
            data                            => std_logic_vector(write_adr & write_data),
            rdreq                           => read_req_cache,
            wrreq                           => write_req_cache,
            almost_full                     => pixel_cache_busy,
            empty                           => pixel_cache_empty,
            full                            => pixel_cache_full,
            std_ulogic_vector(q)            => dataout,
            aclr                            => reset,
            almost_empty                    => open,
            eccstatus                       => open,
            sclr                            => open,
            usedw                           => open
        );
    write_adr_out <= dataout(dataout'length - 1 downto dataout'length - write_adr_out'length);
    write_data_out <= dataout(dataout'length - write_adr_out'length - 1 downto 0);
    
    -- manage pixel write cache FIFO to ddr3 write port out
    read_req_cache <= not pixel_cache_empty and not write_busy_in;              -- Quartus doesn't know about aggregate decomposition (VHDL 2008)
    
    write_req_out <= not pixel_cache_empty and not write_busy_in;
    
    i_ellipse_generator : entity work.ellipse_generator
        generic map
        (
            BITS_RES                        => elli_bits,
            USE_ALTERA_IP                   => 1
        )
        port map
        (
            clk                             => cmd_clk,
            reset                           => reset,
            enable                          => elli_ena,
            run                             => elli_run,
            quadrant                        => elli_quad,
            ellipse_filled                  => elli_fill,
            xc                              => elli_xc,
            yc                              => elli_yc,
            xr                              => elli_xr,
            yr                              => elli_yr,
            ena_pause                       => pixel_cache_busy,
            busy                            => elli_busy,
            x_coord                         => elli_xout,
            y_coord                         => elli_yout,
            pixel_data_rdy                  => elli_out_rdy,
            ellipse_complete                => elli_done
        );
    
    -- cleanly latch buttons and switches inputs
    p_switch_latch : process(all)
    begin
        if rising_edge(clk_in) then
            buttons_l <= buttons;
            switches_l <= switches;
        end if;
    end process p_switch_latch;
    
    -- translate inputs
    -- convert the pixel width input into a bit shift for addressing pixels
    pixel_byte_shift <= 1 when disp_pixel_bytes = 2 else
                        2 when disp_pixel_bytes = 4 else
                        0;

    b_test: block (rising_edge(cmd_clk)) is
        signal latch    : std_ulogic := '0';
    begin
        latch <= '1';
    end block b_test;
    
    p_sm : process(all)
    begin
        if rising_edge(cmd_clk) then
            if reset then
                prog_pc <= 0;
                counter <= 0;
            else
                case prog_pc is
                    when 0 =>
                        counter <= 0;
                        elli_count <= 0;
                        sel_draw <= '0';
                    
                        case buttons_l is
                            when "11" => prog_pc <= prog_pc + 1;
                            when "01" => prog_pc <= 32;
                            when "10" => prog_pc <= 32;
                            when others => null;
                        end case;
                
                    when 1 =>           -- no button pressed
                        sel_draw <= '1';
                        if switches_l(0) = '1' or buttons_l /= 2d"3" then
                            prog_pc <= 0;
                        elsif elli_busy then
                            elli_ena <= '0';
                        else
                            prog_pc <= prog_pc + 1;
                        end if;
                
                    when 2 =>           -- elli is free, begin loading random 
                        if elli_count = elli_fill_ratio then
                            elli_fill <= '1';
                            elli_count <= 0;
                        else
                            elli_fill <= '0';
                            elli_count <= elli_count + 1;
                        end if;
                        elli_xc <= "0" & signed(rnd_num(rnd_num'length - 1 downto rnd_num'length - elli_bits + 1));
                        elli_yc <= "00" & signed(rnd_num(elli_yc'length - 1 downto 2));
                        prog_pc <= prog_pc + 1;

                    when 3 =>
                        elli_xr <= "0000" & signed(rnd_num(elli_bits - 4 downto 1));
                        elli_yr <= "0000" & signed(rnd_num(16 downto elli_bits - 5));
                        prog_pc <= prog_pc + 1;
                    
                    when 4 =>
                        elli_quad <= 0;
                        color <= rnd_num;
                        prog_pc <= prog_pc + 1;
                
                    when 5 =>
                        elli_ena <= '1';
                        elli_run <= '1';
                        prog_pc <= prog_pc + 1;
                
                    when 6 =>
                        elli_run <= '0';
                        prog_pc <= prog_pc + 1;
                    
                    when 7 =>
                        if not elli_busy and not pixel_in_pipe then
                            prog_pc <= prog_pc + 1;
                        end if;
                
                    when 8 =>
                        if elli_quad = 3 then
                            prog_pc <= 1;
                        else
                            elli_quad <= elli_quad + 1;
                            prog_pc <= 5;
                        end if;
                
                    when 31 =>
                        prog_pc <= 0;
                    
                    when 32 =>
                        draw_x <= (others => '0');
                        draw_y <= (others => '0');
                        rnd_sel <= buttons_l(0);
                        if buttons_l = 2d"3" then
                            prog_pc <= 0;
                            draw_ena <= '0';
                        else
                            prog_pc <= prog_pc + 1;
                        end if;
                    
                    when 33 =>
                        if not pixel_cache_busy then
                            draw_ena <= '1';
                            if rnd_sel then
                                color <= 8x"00" & ucounter(20 downto 13) & ucounter(16 downto 1);
                            else
                                color <= rnd_num;
                            end if;
                            counter <= counter + 1;
                        
                            if draw_x < disp_bitmap_width - 1 then
                                draw_x <= draw_x + 1;
                            else
                                draw_x <= (others => '0');
                                if draw_y < disp_bitmap_height - 1 then
                                    draw_y <= draw_y + 1;
                                else
                                    draw_y <= (others => '0');
                                    prog_pc <= 32;
                                end if;
                            end if;
                        else
                            draw_ena <= '0';
                        end if;
                
                    when others =>
                        prog_pc <= prog_pc + 1;
                end case;
            end if;
        end if;
    end process p_sm;

    pixel_in_pipe <= xyr1 or xyr2 or xyr3;
    
    p_pixel_out : process(all)
    begin
        if rising_edge(cmd_clk) then
            if reset then
                xyr1 <= '0';
                xyr2 <= '0';
                xyr3 <= '0';
                write_req_cache <= '0';
            else
                -- step 1, select between elli output and other graphics generators
                if sel_draw then 
                    xyr1 <= elli_out_rdy;
                    x1 <= elli_xout;
                    y1 <= elli_yout;
                else 
                    xyr1 <= draw_ena;
                    x1 <= draw_x;
                    y1 <= draw_y;
                end if;
                xycol1 <= color;
                
                -- step 2, make sure the coordinates are inside the display area
                if x1 >= 0 and x1 < disp_bitmap_width and y1 >= 0 and y1 <= disp_bitmap_height then
                    xyr2 <= xyr1;   -- if draw coordinates are inside the bitmap, allow the pixel ready through
                else
                    xyr2 <= '0';      -- otherwise, strip out
                end if;
                x2 <= x1;
                y2 <= y1;
                xycol2 <= xycol1;
                
                -- step 3, compute the base y coordinate address offset
                xyr3 <= xyr2;
                addr1 <= std_ulogic_vector(resize(unsigned(disp_mem_addr + (y2 * disp_bitmap_width)), addr1'length));
                x3 <= x2;
                xycol3 <= xycol2;
                
                -- step 4, add the x position coordinate and output pixel write fifo
                write_req_cache <= xyr3;
                write_adr <= std_ulogic_vector(shift_left(unsigned(addr1) + unsigned(x3), pixel_byte_shift));
                write_data <= xycol3;
            end if;
        end if; -- rising_edge(cmd_clk)
    end process p_pixel_out;
    
    ucounter <= std_ulogic_vector(to_unsigned(counter, ucounter'length));    
end architecture rtl;