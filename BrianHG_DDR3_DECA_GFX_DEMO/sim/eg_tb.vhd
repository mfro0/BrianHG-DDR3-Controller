entity eg_tb is
end entity eg_tb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

architecture sim of eg_tb is
    constant BITS_RES           : natural := 12;
    constant BITS_RAD           : natural := BITS_RES - 1;
    constant USE_ALTERA_IP      : natural := 1;
    
    signal clk                  : std_ulogic := '1';
    signal reset,
           ellipse_enable,
           ellipse_run          : std_ulogic;
    signal ellipse_quadrant     : std_ulogic_vector(1 downto 0);
    signal ellipse_filled       : std_ulogic;

    signal xc,
           yc,
           xr,
           yr                   : signed(BITS_RES - 1 downto 0);
    signal ena_pause,
           ellipse_busy         : std_ulogic;

    signal pixel_x,
           pixel_y              : signed(BITS_RES downto 0);
    signal pixel_rdy,
           ellipse_complete     : std_ulogic;
    signal ix, iy, icol         : integer;
    signal i_quadrant           : integer range 0 to 3;

    type state_t is (S0, S1, S2, S3, S4, S5, S6);

    signal state                : state_t := S1;
    signal rnd                  : integer;

    procedure init is
    begin
        report "init";
    end procedure init;
    --attribute foreign of init : procedure is "VHPIDIRECT ./libellipse.so init";
    
    procedure plot(x : integer; y : integer; col : integer) is
    begin
        report "plot: x=" & integer'image(x) & " y=" & integer'image(y);
    end procedure plot;
    attribute foreign of plot : procedure is "VHPIDIRECT plot";


begin
    clk <= not clk after 8 ns;

    ellipse_quadrant <= std_ulogic_vector(to_unsigned(i_quadrant, ellipse_quadrant'length));

    p_once : process
    begin
        init;
        wait;
    end process p_once;

    ix <= to_integer(pixel_x);
    iy <= to_integer(pixel_y);
    p_pixel_out : process(all)
    begin
        if pixel_rdy = '1' then
            plot(ix, iy, icol);
        end if;
    end process p_pixel_out;

    p_eg: process(all)
        variable rand           : real;
        variable sd1, sd2       : positive;
        impure function rand_int(min_val, max_val : integer) return integer is
            variable r : real;
        begin
            uniform(sd1, sd2, r);
            return integer(
                round(r * real(max_val - min_val + 1) + real(min_val) - 0.5));
        end function;
    begin
        if reset then
            state <= S0;
        elsif rising_edge(clk) then
            -- generate random number

            case state is
                when S1 =>
                    xc <= to_signed(rand_int(0, 799), xc'length);
                    yc <= to_signed(rand_int(0, 600), yc'length);
                    xr <= to_signed(rand_int(0, 100), xr'length);
                    yr <= to_signed(rand_int(0, 100), yr'length);
                    i_quadrant <= 0;

                    state <= state_t'succ(state);

                when S2 =>

                    icol <= integer(rand_int(0, 2 ** 24 - 1));
                    state <= state_t'succ(state);
                    
                when S3 =>
                    ellipse_enable <= '1';
                    ellipse_run <= '1';
                    ellipse_filled <= '1';
                    ena_pause <= '0';
                    state <= state_t'succ(state);

                when S4 =>

                    state <= state_t'succ(state);

                when S5 =>
                    if pixel_rdy = '1' then
                        state <= S4;
                    end if;
                    if ellipse_complete = '1' and i_quadrant = 3 then
                        state <= S1;
                    elsif ellipse_complete = '1' and i_quadrant /= 3 then
                        i_quadrant <= i_quadrant + 1;
                        state <= S3;
                    end if;
                when others =>
                    std.env.stop(0);
            end case;
        end if;
    end process p_eg;

    i_ellipse_generator : entity work.ellipse_generator
        generic map
        (
            BITS_RES        => BITS_RES,            -- Coordinates IO port bits. 12 = -2048 to + 2047
            BITS_RAD        => BITS_RAD,            -- should be (BITS_RES - 1) - Bits for internal ma
            USE_ALTERA_IP   => USE_ALTERA_IP        -- Selects if Altera's LPM_MULT should be used
        )
        port map
        (
            -- inputs
            clk         => clk,                     -- 125 MHz pixel clock
            reset       => reset,                   -- asynchronous reset
            enable      => ellipse_enable,          -- logic enable
            run         => ellipse_run,             -- '1' to draw/run the units
            quadrant    => ellipse_quadrant,        -- specifies which quadrant of the ellipse to draw
            ellipse_filled  => ellipse_filled,      -- x-filling when drawing an ellipse
            xc          => xc,                      -- 12 bit x-coordinate for the center of the ellipse
            yc          => yc,                      -- 12 bit y-coordinate for the center of the ellipse
            xr          => xr,                      -- 12 bit x-radius - width of the ellipse
            yr          => yr,                      -- 12 bit y-radius - height of the ellipse
            ena_pause   => ena_pause,               -- set '1' to pause ellie while it is drawing


            -- outputs
            busy        => ellipse_busy,            -- '1' when line generator is running
            x_coord     => pixel_x,                 -- 13-bit x-coordinate for current pixel 
            y_coord     => pixel_y,                 -- 13-bit y-coordinate for current pixel
            pixel_data_rdy  => pixel_rdy,           -- '1' when coordinate outputs are valid
            ellipse_complete => ellipse_complete    -- '1' when ellipse is completed
        );
end architecture sim;