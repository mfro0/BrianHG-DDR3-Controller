
library ieee;
use ieee.std_logic_1164.all;

package ddr3_pack is
    subtype port_feature_t is integer range 0 to 255;
    type port_features_t is array(integer range <>) of port_feature_t; 
    subtype port_prio_t is integer range 0 to 3;
    type port_prios_t is array(integer range <>) of port_prio_t;
    subtype port_cache_t is integer range 0 to 256;
    type port_caches_t is array(integer range <>) of port_cache_t;
    type int_vector is array(integer range <>) of integer;
end ddr3_pack;



--*********************************************************************
--
-- BrianHG_DDR3_DECA_GFX_DEMO which test runs the BrianHG_DDR3_CONTROLLER_top DDR3 memory controler system.
-- Version 1.00, August 22, 2021.
-- 400MHz Test Build with the multiport CMD IO running at Quarter speed.
--
-- Written by Brian Guralnick.
-- For public use.
-- Leave questions in the https://www.eevblog.com/forum/fpga/brianhg_ddr3_controller-open-source-ddr3-controller/
--
--************************************************************************************************************************************************************
--************************************************************************************************************************************************************
--************************************************************************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use work.ddr3_pack;

entity ddr3_deca_top is
    generic
    (
        FPGA_VENDOR         : string := "Altera";               -- (Only Altera for now) Use ALTERA, INTEL, LATTICE or XILINX.
        FPGA_FAMILY         : string := "MAX 10";               -- With Altera, use Cyclone III, Cyclone IV, Cyclone V, MAX 10,....
        BHG_OPTIMIZE_SPEED  : std_ulogic := '1';                -- Use '1' for better FMAX performance, this will increase logic cell usage in the BrianHG_DDR3_PHY_SEQ module.
                                                                -- It is recommended that you use '1' when running slowest -8 Altera fabric FPGA above 300MHz or Altera -6 fabric above 350MHz.
        BHG_EXTRA_SPEED     : std_ulogic := '1';                -- Use '1' for even better FMAX performance or when overclocking the core.  This will increase logic cell usage.

        -- ****************  System clock generation and operation.
        CLK_KHZ_IN          : natural := 50000;                 -- PLL source input clock frequency in KHz.
        CLK_IN_MULT         : natural := 32;                    -- Multiply factor to generate the DDR MTPS speed divided by 2.
        CLK_IN_DIV          : natural := 4;                     -- Divide factor.  When CLK_KHZ_IN is 25000,50000,75000,100000,125000,150000, use 2,4,6,8,10,12.
        DDR_TRICK_MTPS_CAP  : natural := 600;                   -- 0=off, Set a false PLL DDR data rate for the compiler to allow FPGA overclocking.  ***DO NOT USE.
                                                                -- If you are using this with Altera Max 10, use 600 for -6 & -7 and use 550 for -8 FPGAs.

        INTERFACE_SPEED     : string := "Quarter";              -- Either "Full", "Half", or "Quarter" speed for the user interface clock.
                                                                -- This will effect the controller's interface CMD_CLK output port frequency.

        -- ****************  DDR3 ram chip configuration settings
        DDR3_CK_MHZ         : natural := ((CLK_KHZ_IN * CLK_IN_MULT / CLK_IN_DIV) / 1000);  -- DDR3 CK clock speed in MHz.
        DDR3_SPEED_GRADE    : string := "-15E";                 -- Use 1066 / 187E, 1333 / -15E, 1600 / -125, 1866 / -107, or 2133 MHz / 093.
        DDR3_SIZE_GB        : natural := 4;                     -- Use 0,1,2,4 or 8.  (0=512mb) Caution: Must be correct as ram chip size affects the tRFC REFRESH period.
        DDR3_WIDTH_DQ       : natural := 16;                    -- Use 8 or 16.  The width of each DDR3 ram chip.

        DDR3_NUM_CHIPS      : natural := 1;                     -- 1, 2, or 4 for the number of DDR3 RAM chips.
        DDR3_NUM_CK         : natural := 1;                     -- Select the number of DDR3_CLK & DDR3_CLK# output pairs.
                                                                -- Optionally use 2 for 4 ram chips, if not 1 for each ram chip for best timing..
                                                                -- These are placed on a DDR DQ or DDR CK# IO output pins.

        DDR3_WIDTH_ADDR     : natural := 15;                    -- Use for the number of bits to address each row.
        DDR3_WIDTH_BANK     : natural := 3;                     -- Use for the number of bits to address each bank.
        DDR3_WIDTH_CAS      : natural := 10;                    -- Use for the number of bits to address each column.

        DDR3_WIDTH_DM       : natural := (DDR3_WIDTH_DQ * DDR3_NUM_CHIPS / 8);  -- The width of the write data mask. (***Double when using multiple 4 bit DDR3 ram chips.)
        DDR3_WIDTH_DQS      : natural := (DDR3_WIDTH_DQ * DDR3_NUM_CHIPS / 8);  -- The number of DQS pairs.          (***Double when using multiple 4 bit DDR3 ram chips.)
        DDR3_RWDQ_BITS      : natural := (DDR3_WIDTH_DQ * DDR3_NUM_CHIPS * 8);  -- Must equal to total bus width across all DDR3 ram chips *8.

        DDR3_ODT_RTT        : natural := 40;                    -- use 120, 60, 40, 30, 20 Ohm. or 0 to disable ODT.  (On Die Termination during write operation.)
        DDR3_RZQ            : natural := 40;                    -- use 34 or 40 Ohm. (Output Drive Strength during read operation.)
        DDR3_TEMP           : natural := 85;                    -- use 85,95,105. (Peak operating temperature in degrees Celsius.)

        DDR3_WDQ_PHASE      : natural := 270;                   -- 270, Select the write and write DQS output clock phase relative to the DDR3_CLK/CK#
        DDR3_RDQ_PHASE      : natural := 0;                     -- 0,   Select the read latch clock for the read data and DQS input relative to the DDR3_CLK.

        DDR3_MAX_REF_QUEUE  : natural := 8;                     -- Defines the size of the refresh queue where refreshes will have a higher priority than incoming SEQ_CMD_ENA command requests.
                                                                -- *** Do not go above 8, doing so may break the data sheet's maximum ACTIVATE-to-PRECHARGE command period.
        IDLE_TIME_uSx10     : natural := 10;                    -- Defines the time in 1/10uS until the command IDLE counter will allow low priority REFRESH cycles.
                                                                -- Use 10 for 1uS.  0=disable, 2 for a minimum effect, 127 maximum.

        SKIP_PUP_TIMER      : natural := 0;                     -- Skip timer during and after reset. ***ONLY use 1 for quick simulations.

        BANK_ROW_ORDER      : string := "ROW_BANK_COL";         -- Only supports "ROW_BANK_COL" or "BANK_ROW_COL".  Choose to optimize your memory access.

        PORT_ADDR_SIZE      : natural := (DDR3_WIDTH_ADDR + DDR3_WIDTH_BANK + DDR3_WIDTH_CAS + (DDR3_WIDTH_DM-1));

        -- ************************************************************************************************************************************
        -- ****************  BrianHG_DDR3_COMMANDER configuration parameter settings.
        PORT_R_TOTAL        : natural := 2;                     -- Set the total number of DDR3 controller read ports, 1 to 16 max.
        PORT_W_TOTAL        : natural := 2;                     -- Set the total number of DDR3 controller write ports, 1 to 16 max.
        PORT_VECTOR_SIZE    : natural := 16;                    -- Sets the width of each port's VECTOR input and output.

        -- ************************************************************************************************************************************
        -- ***** DO NOT CHANGE THE NEXT 4 PARAMETERS FOR THIS VERSION OF THE BrianHG_DDR3_COMMANDER.sv... *************************************
        PORT_CACHE_BITS     : natural := (8 * DDR3_WIDTH_DM * 8);   -- Note that this value must be a multiple of ' (8*DDR3_WIDTH_DQ*DDR3_NUM_CHIPS)* burst 8 '.
        CACHE_ADDR_WIDTH    : natural := INTEGER(CEIL(LOG2(REAL(PORT_CACHE_BITS / 8 + 1 ))));   -- This is the number of LSB address bits which address all the available 8 bit bytes inside the cache word.
        DDR3_VECTOR_SIZE    : natural := (PORT_ADDR_SIZE + 4);  -- Sets the width of the VECTOR for the DDR3_PHY_SEQ controller.  4 bits for 16 possible read ports.
        CACHE_ROW_BASE      : natural := (DDR3_WIDTH_CAS + (DDR3_WIDTH_DM - 1));    -- Sets the starting address bit where a new row & bank begins.
        -- ************************************************************************************************************************************
        -- PORT_'feature' = '{array a,b,c,d,..} Sets the feature for each DDR3 ram controller interface port 0 to port 15.
        PORT_R_DATA_WIDTH   : port_features_t := (  8, 128, 128, 128, 128, 128, 128, 128,
                                                  128, 128, 128, 128, 128, 128, 128, 128); 
        PORT_W_DATA_WIDTH   : port_features_t := (  8,  32, 128, 128, 128, 128, 128, 128,
                                                  128, 128, 128, 128, 128, 128, 128, 128); 
                                                            -- Use 8,16,32,64,128, or 256 bits, maximum = 'PORT_CACHE_BITS'
                                                            -- As a precaution, this will prune/ignore unused data bits and write masks bits, however,
                                                            -- all the data ports will still be 'PORT_CACHE_BITS' bits and the write masks will be 'PORT_CACHE_WMASK' bits.
                                                            -- (a 'PORT_CACHE_BITS' bit wide data bus has 32 individual mask-able bytes (8 bit words))
                                                            -- For ports sizes below 'PORT_CACHE_BITS', the data is stored and received in Big Endian.  

        PORT_R_PRIORITY     : port_prios_t := (3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
        PORT_W_PRIORITY     : port_prios_t := (3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
                                                            -- Use 1 through 6 for normal operation.  Use 7 for above refresh priority.  Use 0 for bottom
                                                            -- priority, only during free cycles once every other operation has been completed.
                                                            -- Open row policy/smart row access only works between ports with identical
                                                            -- priority.  If a port with a higher priority receives a request, even if another
                                                            -- port's request matches the current page, the higher priority port will take
                                                            -- president and force the ram controller to leave the current page.
                                                            -- *(Only use 7 for small occasional access bursts which must take president above
                                                            --   all else, yet not consume memory access beyond the extended refresh requirements.)

        PORT_R_CMD_STACK    : std_ulogic_vector := ('1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1,' '1', '1', '1');
                                                            -- Sets the size of the intermediate read command request stack.
                                                            -- 0=4 level deep.  1=8 level deep.
                                                            -- The size of the number of read commands built up in advance while the read channel waits
                                                            -- for the DDR3_PHY_SEQ to return the read request data.  (Stored in logic cells)
                                                            -- Multiple reads must be accumulated to allow an efficient continuous read burst.
                                                            -- IE: Use 8 level deep when running a small data port width like 8 or 16 so sequential read cache
                                                            -- hits continue through the command input allowing cache miss read req later-on in the req stream to be
                                                            - immediately be sent to the DDR3_PHY_SEQ before the DDR3 even returns the first read req data.

        PORT_W_CACHE_TOUT   : int_vector := (256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256)
                                                            -- A timeout for the write cache to dump it's contents to ram.
                                                            -- 0   = immediate writes, or no write cache.
                                                            -- 256 = Wait up to 256 CMD_CLK clock cycles since the previous write req.
                                                            --       to the same 'PORT_CACHE_BITS' bit block before writing to ram.  Write reqs outside
                                                            --       the current 'PORT_CACHE_BITS' bit cache block clears the timer and forces an immediate write.

        PORT_CACHE_SMART    : std_ulogic_vector := ('1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1');
                                                            -- When enabled, if an existing read cache exists at the same write request address,
                                                            -- that read's cache will immediately be updated with the new write data.  (Only on the same port number...)
                                                            -- This function may impact the FMAX for the system clock and increase LUT usage.
                                                            -- *** Disable when designing a memory read/write testing algorithm.
/*
parameter bit [8:0]  PORT_R_MAX_BURST     [0:15] = '{256, 256, 256, 256, 256, 256, 256, 256,
                                                     256, 256, 256, 256, 256, 256, 256, 256);
parameter bit [8:0]  PORT_W_MAX_BURST     [0:15] =  (256, 256, 256, 256, 256, 256, 256, 256,
                                                     256, 256, 256, 256, 256, 256, 256, 256);
                                                            -- 1 through 256, 0=No sequential burst priority.
                                                            -- Defines the maximum consecutive read or write burst commands from a single
                                                            -- port if another read/write port requests exists with the same priority level,
                                                            -- but their memory request exist in a different row.  * Every 1 counts for a BL8 burst.
                                                            -- This will prevent a single continuous stream port from hogging up all the ram access time.
                                                            -- IE: If set to 0, commander will seek if other read/write requests are ready before
                                                            -- continuing access to the same port DDR3 access.

parameter bit        SMART_BANK                  = 1        // 1=ON, 0=OFF, With SMART_BANK enabled, the BrianHG_DDR3_COMMANDER will remember which
                                                            // ROW# has been activated in each DDR3 BANK# so that when prioritizing read and write
                                                            // ports of equal priority, multiple commands across multiple banks whose ROWs have
                                                            // matching existing activation will be prioritized/coalesced as if they were part of
                                                            // the sequential burst as PRECHARGE and ACTIVATE commands are not needed when bursting
                                                            // between active banks maintaining an unbroken read/write stream.
                                                            // (Of course the BrianHG_DDR3_PHY_SEQ is able to handle smart banking as well...)
                                                            // Note that enabling this feature uses additional logic cells and may impact FMAX.
                                                            // Disabling this feature will only coalesce commands in the current access ROW.
                                                            // Parameter 'BANK_ROW_ORDER' will define which address bits define the accessed BANK number.
    */
    );
    port
    (
    );
end entity ddr_deca_top;