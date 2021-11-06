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
           ellipse_run          : std_ulogic := '0';
    signal ellipse_quadrant     : std_ulogic_vector(1 downto 0);
    signal ellipse_filled       : std_ulogic := '0';

    signal xc,
           yc,
           xr,
           yr                   : signed(BITS_RES - 1 downto 0) := (others => '0');
    signal ena_pause,
           ellipse_busy         : std_ulogic := '0';

    signal pixel_x,
           pixel_y              : signed(BITS_RES downto 0) := (others => '0');
    signal pixel_rdy,
           ellipse_complete     : std_ulogic := '0';
    signal ix, iy, icol         : integer := 0;
    signal i_quadrant           : integer range 0 to 4 := 0;

    type state_t is (S1, S2, S3, S4, S5, S6);

    signal state                : state_t := S1;
    signal rnd                  : integer;

    procedure plot(x : integer; y : integer; col : integer) is
    begin
        report "plot: x=" & integer'image(x) & " y=" & integer'image(y);
    end procedure plot;
    attribute foreign of plot : procedure is "VHPIDIRECT plot";


begin
    clk <= not clk after 8 ns;

    ellipse_quadrant <= std_ulogic_vector(to_unsigned(i_quadrant, ellipse_quadrant'length));

    plot(ix, iy, icol);

    ix <= to_integer(pixel_x);
    iy <= to_integer(pixel_y);

    p_eg: process(all)
        variable rand           : real := 0.0;
        variable sd1, sd2       : positive := 99;

        impure function rand_int(min_val, max_val : integer) return integer is
            variable r : real;
        begin
            uniform(sd1, sd2, r);
            return integer(
                round(r * real(max_val - min_val + 1) + real(min_val) - 0.5));
        end function;

    begin
        if reset then
            state <= S1;
        elsif rising_edge(clk) then
            case state is
                when S1 =>
                    xc <= to_signed(rand_int(0, 799), xc'length);
                    yc <= to_signed(rand_int(0, 599), yc'length);
                    xr <= to_signed(rand_int(0, 400), xr'length);
                    yr <= to_signed(rand_int(0, 400), yr'length);
                    ellipse_enable <= '0';
                    ellipse_run <= '0';
                    ellipse_filled <= '1';
                    ena_pause <= '0';
                    state <= S2;

                when S2 =>
                    i_quadrant <= 0;

                    icol <= integer(rand_int(0, 2 ** 24 - 1));
                    ellipse_enable <= '1';
                    ellipse_run <= '1';
                    state <= S3;

                when S3 =>
                    ellipse_run <= '0';
                    state <= S4;

                when S4 =>
                    state <= S5;
                    
                when S5 =>
                    if not ellipse_busy then
                        state <= S6;
                    end if;

                when S6 =>
                    if i_quadrant = 3 then
                        state <= S1;
                    else
                        i_quadrant <= i_quadrant + 1;
                        ellipse_run <= '1';
                        state <= S3;
                    end if;

                when others =>
                    assert false report "state=" & state_t'image(state) severity note;
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
            clk             => clk,                 -- 125 MHz pixel clock
            reset           => reset,               -- asynchronous reset
            enable          => ellipse_enable,      -- logic enable
            run             => ellipse_run,         -- '1' to draw/run the units
            quadrant        => ellipse_quadrant,    -- specifies which quadrant of the ellipse to draw
            ellipse_filled  => ellipse_filled,      -- x-filling when drawing an ellipse
            xc              => xc,                  -- 12 bit x-coordinate for the center of the ellipse
            yc              => yc,                  -- 12 bit y-coordinate for the center of the ellipse
            xr              => xr,                  -- 12 bit x-radius - width of the ellipse
            yr              => yr,                  -- 12 bit y-radius - height of the ellipse
            ena_pause       => ena_pause,           -- set '1' to pause ellie while it is drawing


            -- outputs
            busy            => ellipse_busy,        -- '1' when line generator is running
            x_coord         => pixel_x,             -- 13-bit x-coordinate for current pixel 
            y_coord         => pixel_y,             -- 13-bit y-coordinate for current pixel
            pixel_data_rdy  => pixel_rdy,           -- '1' when coordinate outputs are valid
            ellipse_complete => ellipse_complete    -- '1' when ellipse is completed
        );
end architecture sim;
