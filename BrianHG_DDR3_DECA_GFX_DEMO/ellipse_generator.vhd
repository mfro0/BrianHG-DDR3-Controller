/*
 * ELLIPSE GENERATOR MODULE (ELLIE)
 *
 * v 0.5.001   Jan 13, 2021
 * v 0.7       June 28, 2021  -> Fill bug when coordinates exceed the edge of the screen.
 *
 * Now with horizontal fill command.
 * FMAX = 125.9 MHz compiled balanced optimized with 35 bit integer core.
 *
//
// See: https://www.eevblog.com/forum/fpga/brianhg_ddr3_controller-open-source-ddr3-controller/
//
//
// See: https://www.eevblog.com/forum/fpga/fpga-vga-controller-for-8-bit-computer/
//

 */
 
 library ieee;
 use ieee.std_logic_1164.all;
 use ieee.numeric_std.all;
 library lpm;
 use lpm.lpm_components.all;
 
 entity ellipse_generator is
    generic
    (
        BITS_RES        : natural := 12;            -- Coordinates IO port bits. 12 = -2048 to + 2047
        BITS_RAD        : natural := 11;            -- should be (BITS_RES - 1) - Bits for internal maximum radius.
                                                    -- Since the radius is positive only, 1 bit less is sufficient
        USE_ALTERA_IP   : natural := 1              -- Selects if Altera's LPM_MULT should be used
    );
    port
    (
        -- inputs
        clk             : in std_ulogic;                    -- 125 MHz pixel clock
        reset           : in std_ulogic;                    -- asynchronous reset
        enable          : in std_ulogic;                    -- logic enable
        run             : in std_ulogic;                    -- '1' to draw/run the units
        quadrant        : in natural range 0 to 3;          -- specifies which quadrant of the ellipse to draw
        ellipse_filled  : in std_ulogic;                    -- x-filling when drawing an ellipse
        xc,                                                 -- 12 bit x-coordinate for the center of the ellipse
        yc,                                                 -- 12 bit y-coordinate for the center of the ellipse
        xr,                                                 -- 12 bit x-radius - width of the ellipse
        yr              : in signed(BITS_RES - 1 downto 0); -- 12 bit y-radius - height of the ellipse
        ena_pause       : in std_ulogic;                    -- set '1' to pause ellie while it is drawing
        
        -- outputs
        busy            : out std_ulogic;                   -- '1' when line generator is running
        x_coord         : out signed(BITS_RES downto 0);    -- 13-bit x-coordinate for current pixel - extra bit needed for fills off screen bug
        y_coord         : out signed(BITS_RES downto 0);    -- 13-bit y-coordinate for current pixel - extra bit needed for fills off screen bug
        pixel_data_rdy  : out std_ulogic;                   -- '1' when coordinate outputs are valid
        ellipse_complete: out std_ulogic                    -- '1' when ellipse is completed
    );
end entity ellipse_generator;

architecture rtl of ellipse_generator is
    signal draw_line            : std_ulogic := '0';
    signal quadrant_latch       : integer range 0 to 3 := 0;                -- this logic latches which quadrant to draw when run is issued
    signal sub_function         : integer range 0 to 15 := 0;               -- this logic defines which step is running, i.e. first setup for first 45 degrees
    signal inv                  : std_ulogic := '0';                        -- draw the first 45 degrees if the radius is not 0, finish the ellipse if the remaining
    signal draw_flat            : std_ulogic := '0';                        -- radius <= 1, setup for second 45 degrees (inv), draw the second 45 degrees if the radius
    signal filled               : std_ulogic := '1';                        -- is not 0, finish the ellipse if the remaining radius <= 1, end the busy and await next command
    
    signal x,                                                                               -- internal drawing x-coordinate
           y,                                                                               -- internal drawing y-coordinate
           xcr,                                                                             -- registered input x-center
           ycr,                                                                             -- registered input y-center
           xrr,                                                                             -- registered input x-radius
           yrr                  : signed(BITS_RES - 1 downto 0) := (others => '0');         -- registered input y-radius
    signal p,                                                                               -- arc error offset / sigma
           px,                                                                              -- arc error offset / sigma
           py                   : signed(BITS_RAD * 3 + 1 downto 0) := (others => '0');     -- arc error offset/sigma
    signal rx2,                                                                             -- holds x radius ** 2
           ry2                  : signed(BITS_RAD * 2 - 1 downto 0) := (others => '0');     -- holds y radius ** 2
    
    signal alu_mult_a           : signed(BITS_RAD * 1 - 1 downto 0) := (others => '0');     -- consolidated single multiplier A-input for all multiplication in the generator
    signal alu_mult_b           : signed(BITS_RAD * 2 - 1 downto 0) := (others => '0');     -- consolidated single multiplier B-input for all multiplication in the generator
    signal alu_mult_y           : signed(BITS_RAD * 3 - 1 downto 0) := (others => '0');     -- consolidated single multiplier Y-output for all multiplication in the generator
    
    signal pixel_data_rdy_int   : std_ulogic := '0';                    -- '1' when coordinate outputs are valid
    signal busy_int             : std_ulogic := '0';                    -- '1' when coordinate outputs are valid
    signal ena_process          : std_ulogic := '0';                    -- '1' when output pixels should be computed
    
    signal freeze               : std_ulogic := '0';                    -- when performing a fill, the freeze will stop on every y count
    
begin

    mul: if USE_ALTERA_IP = 1 generate
        -- initiate Altera's megafunction 'lpm_mult' which will give us a better FMAX,
        -- and if needed, it has a pipeline feature to increase FMAX even further
        i_mult : component lpm_mult
            generic map
            (
                lpm_hint            => "MAXIMIZE_SPEED=9",
                lpm_pipeline        => 1,
                lpm_representation  => "SIGNED",
                lpm_type            => "LPM_MULT",
                lpm_widtha          => BITS_RAD * 1,
                lpm_widthb          => BITS_RAD * 2,
                lpm_widthp          => BITS_RAD * 3
            )
            port map
            (
                dataa               => std_logic_vector(alu_mult_a),
                datab               => std_logic_vector(alu_mult_b),
                clken               => ena_process,
                clock               => clk,
                signed(result)              => alu_mult_y,
                aclr                => '0',
            --  sclr                => '0',             -- omit for older versions of Modelsim
                sum                 => (others => '0')
            );
    else generate
        -- USE_ALTERA_IP is disabled, use this multiply code for the consolidated multiply instead
        p_multiply : process
        begin   
            wait until rising_edge(clk);
            if ena_process then
                alu_mult_y <= alu_mult_a * alu_mult_b;
            end if;
        end process p_multiply;
    end generate;
    
    p_pixel : process(all)
    begin
        pixel_data_rdy <= pixel_data_rdy_int and not ena_pause;         -- immediately clear the pixel_data_ready output when pause is '1'
        busy <= busy_int or run;                                        -- immediately make busy flag high when run is asserted
        ena_process <= enable and not(ena_pause and pixel_data_rdy_int);
    end process p_pixel;
    
    p_run : process(all)
    begin
        if reset then
            -- reset latches, geometry counters and flags
            draw_line <= '0';
            pixel_data_rdy_int <= '0';
            busy_int <= '0';
            ellipse_complete <= '0';
            quadrant_latch <= 0;
            sub_function <= 0;
            x <= (others => '0');
            y <= (others => '0');
            xcr <= (others => '0');
            ycr <= (others => '0');
            xrr <= (others => '0');
            yrr <= (others => '0');
            p <= (others => '0');
            rx2 <= (others => '0');
            ry2 <= (others => '0');
            px <= (others => '0');
            py <= (others => '0');
            inv <= '0';
            draw_flat <= '0';
            filled <= '0';
            freeze <= '0';
            x_coord <= (others => '0');
            y_coord <= (others => '0');
            alu_mult_a <= (others => '0');
            alu_mult_b <= (others => '0');
        elsif rising_edge(clk) then
            -- draw_busy_int must be '0' or ellipse_generator won't run
            -- when ready to output valid coordinates, the ena_pause is allowed to pause/stop the rendering process
            if ena_process then
                if not freeze then
                    if inv = '0' then
                        case quadrant_latch is
                            when 0 =>
                                x_coord <= resize(xcr + x, x_coord'length);
                                y_coord <= resize(ycr + y, y_coord'length);
                            when 1 =>
                                x_coord <= resize(xcr - x, x_coord'length);
                                y_coord <= resize(ycr + y, x_coord'length);
                            when 2 =>
                                x_coord <= resize(xcr + x, x_coord'length);
                                y_coord <= resize(ycr - y, y_coord'length);
                            when 3 =>
                                x_coord <= resize(xcr - x, x_coord'length);
                                y_coord <= resize(ycr - y, y_coord'length);
                        end case;
                    else
                        case quadrant_latch is
                            when 0 =>
                                y_coord <= resize(xcr + x, x_coord'length);
                                x_coord <= resize(ycr + y, y_coord'length);
                            when 1 =>
                                y_coord <= resize(xcr + x, x_coord'length);
                                x_coord <= resize(ycr - y, y_coord'length);
                            when 2 =>
                                y_coord <= resize(xcr - x, x_coord'length);
                                x_coord <= resize(ycr + y, y_coord'length);
                            when 3 =>
                                y_coord <= resize(xcr - x, x_coord'length);
                                x_coord <= resize(ycr - y, y_coord'length);
                        end case;
                    end if;
                else        -- not freeze => do a horizontal fill
                    if (x_coord = xcr and not inv = '1') or (x_coord = ycr and inv = '1') then freeze <= '0'; end if;           -- horizontal fill finished, unfreeze
                    if (x_coord > xcr and not inv = '1') or (x_coord > ycr and inv = '1') then x_coord <= x_coord - 1; end if;  -- fill to the left
                    if (x_coord < xcr and not inv = '1') or (x_coord < ycr and inv = '1') then x_coord <= x_coord + 1; end if;  -- fill to the right
                end if;

                case sub_function is        -- geo_sub_func = 0 is the idle state where we wait for the run to be asserted
                    when 0 =>
                        if run then
                            -- load values and begin drawing the ellipse
                            -- initialise starting coordinates and direction for immediate plotting
                            quadrant_latch <= quadrant;
                            
                            if xr = 0 and yr = 0 then       -- drawing only a single center point
                                x <= (others => '0');       -- initialise starting x pixel location - switch to x_coord
                                y <= (others => '0');       -- initialise starting y pixel location - switch to y_coord
                                pixel_data_rdy_int <= '0';  -- set pixel_data_rdy_int flag
                                ellipse_complete <= '0';    -- make sure ellipse_complete is set
                                sub_function <= 9;          -- special case to pass the center coordinates
                                draw_line <= '1';           -- no line to draw
                                busy_int <= '1';            -- the line generator is busy_int from the next cycle
                                
                                xcr <= xc;                  -- register store all coordinate inputs
                                ycr <= yc;
                                xrr <= (others => '0');
                                yrr <= (others => '0');
                                inv <= '0';
                                draw_flat <= '0';
                                freeze <= '0';
                            else
                                -- draw a full ellipse
                                
                                -- set latched registers, phase counters and flags
                                sub_function <= sub_function + 1;       -- after completing this setup, advance the sub_function to the next step
                                draw_line <= '1';                       -- start drawing the line at the next clock cycle
                                busy_int <= '1';                        -- the line generator is busy_int from the next cycle
                                pixel_data_rdy_int <= '0';              -- no valid coordinates next clock cycle
                                ellipse_complete <= '0';                -- reset ellipse_complete flag
                                
                                xcr <= xc;                              -- register store all coordinate inputs
                                ycr <= yc;
                                xrr <= xr;
                                yrr <= yr;
                                inv <= '0';
                                draw_flat <= '0';
                                filled <= ellipse_filled;
                                freeze <= '0';
                            end if; -- if not draw a single point
                        end if;
                    
                    when 1 =>
                        -- sub_function 1
                        -- step 1, setup consolidated multiplier to compute rx ** 2
                        alu_mult_a <= resize(xrr, alu_mult_a'length);
                        alu_mult_b <= resize(xrr, alu_mult_b'length);

                        x <= (others => '0');                        
                        y <= yrr;
                        draw_flat <= '0';
                        p <= (others => '0');
                        sub_function <= sub_function + 1;
                        
                    when 2 =>
                        -- step 2, setup consolidated multiplier to compute ry ** 2;
                        alu_mult_a <= resize(yrr, alu_mult_a'length);
                        alu_mult_b <= resize(yrr, alu_mult_b'length);
                        sub_function <= sub_function + 1;
                        
                    when 3 =>
                        -- step 3, store computed rx ** 2
                        rx2 <= resize(alu_mult_y, rx2'length);    -- the ALU has a 2 clock delay, 1 clock to send data in, 1 clock for the result to become valid
                        
                        -- prepare py = ry * rx2
                        alu_mult_a <= resize(yrr, alu_mult_a'length);
                        alu_mult_b <= resize(alu_mult_y, alu_mult_b'length);
                        
                        -- begin the initial preparation of 'p' -> p = (0.25 * rx ** 2) + 0.5
                        ry2 <= resize(shift_right(alu_mult_y + 2, 2), ry2'length);                       -- computes rx2 / 4 with rounding, use px as the temporary register
                        px <= (others => '0');                                                                                      -- clear temp px register
                        sub_function <= sub_function + 1;       -- advance the sub_function to the next step
                    
                    when 4 =>
                        px <= px + ry2;             -- make px = integer(0.25 * rx2 + 0.5)
                        
                        -- store computed ry ** 2
                        ry2 <= resize(alu_mult_y, ry2'length);
                        sub_function <= sub_function + 1;
                        
                    when 5 =>
                        px <= px + ry2 - alu_mult_y;                                            -- make px = px + ry2 - (rx2 * ry)
                        -- step 5, store computed rx2 * y
                        py <= resize(shift_left(alu_mult_y, 1), py'length);                     -- store py = (ry * rx2) * 2
                        sub_function <= sub_function + 1;
                    
                    when 6 =>
                        p <= px;            -- store computed px = int(0.25 * rx2 + 0.5) + ry2 - (rx2 * ry)
                        px <= (others => '0');
                        
                        if xrr < 2 then
                            sub_function <= sub_function + 2;           -- no radius, skip arc and go straight to finish straight line
                        else
                            sub_function <= sub_function + 1;           -- some radius, draw arc
                        end if;
                        
                    when 7 =>
                        -- draw ellipse
                        if not freeze then
                            if px <= py then
                                -- drawing the line *** WARNING, was originally less than equal to  <=, but this rendered an extra pixel
                                pixel_data_rdy_int <= '1';          -- pixel data ready
                                x <= x + 1;
                                px <= px + shift_left(ry2, 1);
                                if inv and filled then
                                    freeze <= '1';
                                end if;
                                
                                if p <= 0 then
                                    p <= p + ry2 + (px + shift_left(ry2, 1));
                                else
                                    if not inv and filled then
                                        freeze <= '1';
                                    end if;
                                    y <= y - 1;
                                    py <= py - shift_left(rx2, 1);
                                    p <= p + ry2 + (px + shift_left(ry2, 1)) - (py - shift_left(rx2, 1));
                                end if;
                            else
                                -- end of line has been reached
                                pixel_data_rdy_int <= '0';                  -- reset pixel_data_rdy_int flag - no more valid coordinates after this clock
                                sub_function <= sub_function + 1;           -- next function
                            end if;
                        end if;
                        
                    when 8 =>
                        if not freeze then
                            if (y < 2 or draw_flat = '1') and x <= xrr then       -- if any line remains to be drawn
                                draw_flat <= '1';                           -- stay in loop until x <= x - radius
                                y <= (others => '0');                       -- clear the y axis
                                if draw_flat then                           -- must wait for y to clear to 0 before drawing the flat portion of the ellipse
                                    pixel_data_rdy_int <= '1';              -- pixel data ready
                                    x <= x + 1;                             -- increment x coordinates
                                end if;
                            else
                                if inv then
                                    pixel_data_rdy_int <= '0';              -- reset pixel_data_rdy_int flag - no more valid coordinates after this clock
                                    sub_function <= 0;                      -- reset to idle state
                                    inv <= '0';                             -- clear the inv
                                    draw_line <= '0';                       -- last pixel - allow time for this pixel to be written by ending on next clock
                                    ellipse_complete <= '1';
                                    busy_int <= '0';                        -- line generator is no longer busy_int
                                else
                                    pixel_data_rdy_int <= '0';              -- reset pixel_data_rdy_int flag - no more valid coordinates after this clock
                                    sub_function <= 1;                      -- restart the rendering portion of the program
                                    inv <= '1';                             -- with the inv flag set
                                    xcr <= ycr;                             -- swap the x/y center coordinates
                                    ycr <= xcr;
                                    xrr <= yrr;                             -- swap the radius
                                    yrr <= xrr;
                                end if;
                            end if;
                        end if;
                        
                    when 9 =>
                        -- ellipse radius of 0, pass through center coordinate data and complete the ellipse
                        pixel_data_rdy_int <= '1';                          -- set center coordinates ready
                        sub_function <= sub_function + 1;
                    
                    when others =>
                        -- we are in an undefined function state
                        sub_function <= 0;          -- so reset the function state to 0
                        draw_line <= '0';           -- and make sure we disable the draw_line flag
                        pixel_data_rdy_int <= '0';  -- reset pixel_data_rdy_int flag - no more valid coordinates after this clock
                        ellipse_complete <= '1';
                        busy_int <= '0';            -- line generator is no longer busy_int
                end case;
                
                if not draw_line and not run then
                    pixel_data_rdy_int <= '0';      -- reset pixel_data_rdy_int flag - no more valid coordinates after this clock
                    ellipse_complete <= '0';        -- make sure ellipse_complete is a single 1 shot clock cycle
                    busy_int <= '0';                -- 
                end if;
            end if;
        end if;
    end process p_run;
end architecture rtl;
