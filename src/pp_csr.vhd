-- The Potato Processor - A simple processor for FPGAs
-- (c) Kristian Klomsten Skordal 2014 - 2015 <kristian.skordal@wafflemail.net>
-- Report bugs and issues on <https://github.com/skordal/potato/issues>

library ieee;
use ieee.std_logic_1164.all;

--! @defgroup CSR Control and Status Registers
--! @brief Control and status register functionality.
--! @{

--! @brief Package containing constants and utility functions relating to status and control registers.
package pp_csr is

	--! Type used for specifying control and status register addresses.
	subtype csr_address is std_logic_vector(11 downto 0);

	--! Exception cause type.
	subtype csr_exception_cause is std_logic_vector(5 downto 0); -- Upper bit is the interrupt bit

	--! Converts an exception cause to a std_logic_vector.
	function to_std_logic_vector(input : in csr_exception_cause) return std_logic_vector;

	--! Control/status register write mode.
	type csr_write_mode is (CSR_WRITE_NONE, CSR_WRITE_SET, CSR_WRITE_CLEAR, CSR_WRITE_REPLACE);

	--! @defgroup EXCEPTION_CAUSES Exception cause codes
	--! @brief Constants for the @c MCAUSE register.
	--! @details These constants contant the different values that the @c MCAUSE register can contain after an exception
	--!          has occured.
	--! @{
	constant CSR_CAUSE_INSTR_MISALIGN : csr_exception_cause := b"000000"; --! Instruction misaligned
	constant CSR_CAUSE_INSTR_FETCH    : csr_exception_cause := b"000001"; --! Instruction fetch bus error
	constant CSR_CAUSE_INVALID_INSTR  : csr_exception_cause := b"000010"; --! Invalid instruction exception
	constant CSR_CAUSE_BREAKPOINT     : csr_exception_cause := b"000011"; --! Breakpoint
	constant CSR_CAUSE_LOAD_MISALIGN  : csr_exception_cause := b"000100"; --! Data load from misaligned address
	constant CSR_CAUSE_LOAD_ERROR     : csr_exception_cause := b"000101"; --! Data load bus error
	constant CSR_CAUSE_STORE_MISALIGN : csr_exception_cause := b"000110"; --! Data store to misaligned address
	constant CSR_CAUSE_STORE_ERROR    : csr_exception_cause := b"000111"; --! Data store bus error
	constant CSR_CAUSE_ECALL          : csr_exception_cause := b"001011"; --! System call
	constant CSR_CAUSE_NONE           : csr_exception_cause := b"011111"; --! Code for no exception

	constant CSR_CAUSE_SOFTWARE_INT   : csr_exception_cause := b"100000"; --! Software interrupt triggered
	constant CSR_CAUSE_TIMER_INT      : csr_exception_cause := b"100001"; --! Timer interrupt triggered
	constant CSR_CAUSE_IRQ_BASE       : csr_exception_cause := b"110000"; --! External interrupt (IRQ) triggered
	--! @}

	--! @defgroup CSR_IDS Control Register IDs
	--! @ingroup CSR
	--! @brief Identifiers for the processor status and control registers.
	--! @details These values are used in the csr* instructions to specify which CSR to access. Writes to undefined
	--!          registers are ignored by the processor, while reads from undefined registers are returned as zero.
	--! @{
	constant CSR_CYCLE    : csr_address := x"c00"; --! Low 32 bits of the cycle counter
	constant CSR_CYCLEH   : csr_address := x"c80"; --! High 32 bits of the cycle counter
	constant CSR_TIME     : csr_address := x"c01"; --! Low 32 bits of the time counter
	constant CSR_TIMEH    : csr_address := x"c81"; --! High 32 bits of the time counter
	constant CSR_INSTRET  : csr_address := x"c02"; --! Low 32 bits of the instruction counter
	constant CSR_INSTRETH : csr_address := x"c82"; --! High 32 bits of the instruction counter

	constant CSR_MCPUID   : csr_address := x"f00"; --! CPU ID register
	constant CSR_MIMPID   : csr_address := x"f01"; --! Processor implementation ID
	constant CSR_MHARTID  : csr_address := x"f10"; --! Hardware thread ID

	constant CSR_MSTATUS  : csr_address := x"300"; --! Status register
	constant CSR_MTVEC    : csr_address := x"301"; --! Exception handler pointer register
	constant CSR_MTDELEG  : csr_address := x"302"; --! Exception delegation register
	constant CSR_MIE      : csr_address := x"304"; --! Interrupt enable register

	constant CSR_MTIMECMP : csr_address := x"321"; --! Time compare value for the internal timer interrupt
	constant CSR_MTIME    : csr_address := x"701"; --! Time value for the internal timer

	constant CSR_MSCRATCH : csr_address := x"340"; --! Operating system scratch register
	constant CSR_MEPC     : csr_address := x"341"; --! Exception PC address
	constant CSR_MCAUSE   : csr_address := x"342"; --! Exception cause, see @ref EXCEPTION_CAUSES
	constant CSR_MBADADDR : csr_address := x"343"; --! Bad address for failed memory accesses
	constant CSR_MIP      : csr_address := x"344"; --! Interrupt pending register

	constant CSR_MTOHOST   : csr_address := x"780"; --! Data-to-host register (used only for testing and verification)
	constant CSR_MFROMHOST : csr_address := x"781"; --! Data-from-host register (used only for testing and verification)

	-- Values used as control register ID in ERETs:
	constant CSR_EPC_ERET   : csr_address := x"100"; --! Immediate field value for @c ERET instructions
	--! @}

	--! Offset into the exception vector for the machine-mode exception handler
	constant CSR_MTVEC_M_OFFSET : natural := 192;

	constant CSR_SR_IE  : natural := 0; --! Index of the @c IE bit in the status register.
	constant CSR_SR_IE1 : natural := 3; --! Index of the @c IE1 bit in the status register.

	-- MIE and MIP register bit indices:
	constant CSR_MIE_MSIE : natural := 3; --! Index of the @c SIE (software interrupt enable) bit in the @c MIE register.
	constant CSR_MIE_MTIE : natural := 7; --! Index of the @c TIE (timer interrupt enable) bit int the @c MIE register.
	constant CSR_MIP_MSIP : natural := CSR_MIE_MSIE; --! Index of the @c SIP (software interrupt pending) bit in the @c MIP register.
	constant CSR_MIP_MTIP : natural := CSR_MIE_MTIE; --! Index of the @c TIP (timer interrupt pending) bit in the @c MIP register.

	--! @brief Exception context record.
	--! @details This structure contains all data necessary to handle an exception. The fields of the record is
	--!          set depending on which exception is taken and written to the relevant CSRs in one cycle if an
	--!          exception has occured.
	type csr_exception_context is
		record
			ie, ie1 : std_logic;                     --! Interrupt enable bits
			cause   : csr_exception_cause;           --! Exception cause
			badaddr : std_logic_vector(31 downto 0); --! Bad address (for failed memory accesses)
		end record;

	--! @brief Creates the value of the mstatus registers from the IE and IE1 bits.
	--! @param ie  Interrupt enable bit 0.
	--! @param ie1 Interrupt enable bit 1.
	--! @returns The @c MSTATUS register value corresponding to the values of the input bits.
	function csr_make_mstatus(ie, ie1 : in std_logic) return std_logic_vector;

end package pp_csr;

package body pp_csr is

	function to_std_logic_vector(input : in csr_exception_cause)
		return std_logic_vector is
	begin
		return (31 => input(5), 30 downto 5 => '0') & input(4 downto 0);
	end function to_std_logic_vector;

	function csr_make_mstatus(ie, ie1 : in std_logic) return std_logic_vector is
		variable retval : std_logic_vector(31 downto 0);
	begin
		retval := (
			11 downto 10 => '1', -- PRV3
			 8 downto  7 => '1', -- PRV2
			 5 downto  4 => '1', -- PRV1
			 CSR_SR_IE1 => ie1,  -- IE1
			 2 downto  1 => '1', -- PRV
			 CSR_SR_IE => ie,    -- IE
			others => '0');
		return retval;
	end function csr_make_mstatus;

end package body pp_csr;

--! @}

