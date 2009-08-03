/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * POSIX TTY-related functionality
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_TTY_H
#define RTS_TTY_H

void* __hscore_get_saved_termios(int fd);
void  __hscore_set_saved_termios(int fd, void* ts);

#endif /* RTS_TTY_H */
