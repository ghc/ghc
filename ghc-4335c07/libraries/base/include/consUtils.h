/* 
 * (c) The University of Glasgow, 2000-2002
 *
 * Win32 Console API helpers.
 */
#pragma once

extern int is_console__(int fd);
extern int set_console_buffering__(int fd, int cooked);
extern int set_console_echo__(int fd, int on);
extern int get_console_echo__(int fd);
extern int flush_input_console__ (int fd);
