-- The Potato Processor - A simple processor for FPGAs
-- (c) Kristian Klomsten Skordal 2014 <kristian.skordal@wafflemail.net>
-- Report bugs and issues on <https://github.com/skordal/potato/issues>

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pp_csr.all;

--! @ingroup CSR
--! @brief ALU used for calculating new values of control and status registers.
entity pp_csr_alu is
	port(
		result        : out std_logic_vector(31 downto 0);	--! Output result.
		x, y          : in  std_logic_vector(31 downto 0);	--! Register input operand.
		immediate     : in  std_logic_vector(4 downto 0);	--! Immediate value operand.
		use_immediate : in  std_logic;				--! Whether to use the immediate value or the @c y input as operand.
		write_mode    : in  csr_write_mode			--! Register write mode, determines the ALU operation.
	);
end entity pp_csr_alu;

--! @brief Behavioural architecture of the CSR ALU.
architecture behaviour of pp_csr_alu is
	signal a : std_logic_vector(31 downto 0); --! First operand
	signal b : std_logic_vector(31 downto 0); --! Second operand
begin

	a <= x;
	b <= y when use_immediate = '0' else std_logic_vector(resize(unsigned(immediate), b'length));

	--! Calculates the new value of a control/status register.
	calculate: process(a, b, write_mode)
	begin
		case write_mode is
			when CSR_WRITE_NONE =>
				result <= a;
			when CSR_WRITE_SET =>
				result <= a or b;
			when CSR_WRITE_CLEAR =>
				result <= a and (not b);
			when CSR_WRITE_REPLACE =>
				result <= b;
		end case;
	end process calculate;

end architecture behaviour;

