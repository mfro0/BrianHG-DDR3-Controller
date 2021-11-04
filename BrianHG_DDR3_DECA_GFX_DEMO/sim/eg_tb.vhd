entity eg_tb is
end entity eg_tb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture sim of eg_tb is
    constant BIT_RES            : natural := 12;
    constant BIT_RAD            : natural := BIT_RES - 1;
    constant USE_ALTERA_IP      : natural := 1;
    
    signal clk                  : std_ulogic := '0';
    signal reset,
           ellipse_enable,
           ellipse_run          : std_ulogic;
    signal ellipse_quadrant     : std_ulogic_vector(1 downto 0);
    signal ellipse_filled       : std_ulogic;

    signal xc,
           yc,
           xr,
           yr                   : signed(BIT_RES - 1 downto 0);
    signal ena_pause,
           ellipse_busy         : std_ulogic;

    signal pixel_x,
           pixel_y              : signed(BIT_RES downto 0);
    signal pixel_rdy,
           ellipse_complete     : std_ulogic;

begin
    clk <= not clk after 20 ns;

    i_ellipse_generator : entity work.ellipse_generator
        generic map
        (
            BITS_RES        => 12,            -- Coordinates IO port bits. 12 = -2048 to + 2047
            BITS_RAD        => 11,            -- should be (BITS_RES - 1) - Bits for internal ma
            USE_ALTERA_IP   => 1              -- Selects if Altera's LPM_MULT should be used
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
