// The Potato Processor Benchmark Applications
// (c) Kristian Klomsten Skordal 2015 <kristian.skordal@wafflemail.net>
// Report bugs and issues on <https://github.com/skordal/potato/issues>

#include <stdarg.h>
#include <stdbool.h>

#include "platform.h"
#include "uart.h"

void uart_puts(volatile uint32_t * base, const char * s)
{
	for(int i = 0; s[i] != 0; ++i)
		uart_putc(base, s[i]);
}

void uart_putc(volatile uint32_t * base, char c)
{
	// Wait until there is room in the transmit buffer:
	while(base[UART_STATUS >> 2] & (1 << UART_STATUS_TXBUF_FULL));
	base[UART_TX >> 2] = c & 0xff;
}

void uart_puth(volatile uint32_t * base, uint32_t n)
{
	static const char * hex_digits = "0123456789abcdef";
	uart_putc(base, '0');
	uart_putc(base, 'x');
	for(int i = 28; i >= 0; i -= 4)
		uart_putc(base, hex_digits[(n >> i) & 0xf]);
}

