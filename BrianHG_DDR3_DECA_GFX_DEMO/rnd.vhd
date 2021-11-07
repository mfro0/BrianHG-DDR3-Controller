library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rnd is
    port
    (
        clk,
        rst,
        ena,
        load            : in std_ulogic;
        seed            : in unsigned(31 downto 0);
        rout            : out std_ulogic_vector(31 downto 0)
    );
end entity rnd;

architecture rtl of rnd is
    signal casr         : unsigned(36 downto 0);
    signal lfsr         : unsigned(42 downto 0);
begin

    p_rnd : process(all)
    begin
        if rst then
            casr <= 37ux"100000000";
            lfsr <= 43ux"100000000";
        elsif rising_edge(clk) then
            if load then
                casr <= resize(resize(seed, 33) or 33x"100000000", 37); -- Load seed, protect from a seed of 0.
                lfsr <= resize(resize(seed, 33) or 33x"100000000", 43); -- Load seed, protect from a seed of 0.
            elsif ena then
                casr <= casr(35 downto 0) & casr(36 downto 36) xor casr(0 downto 0)  & casr(36 downto 1) xor shift_left(casr(27 downto 27), 27);
                lfsr <= lfsr(41 downto 0) & lfsr(42 downto 42) xor shift_left(lfsr(42 downto 42), 41) xor shift_left(lfsr(42 downto 42), 20) xor shift_left(lfsr(42 downto 42), 1);
                rout <= std_ulogic_vector(lfsr(31 downto 0) xor casr(31 downto 0));
            end if;
        end if;
    end process p_rnd;
end architecture rtl;
    