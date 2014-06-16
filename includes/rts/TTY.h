/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * POSIX TTY-related functionality
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_TTY_H
#define RTS_TTY_H

void* __hscore_get_saved_termios(int fd);
void  __hscore_set_saved_termios(int fd, void* ts);

#endif /* RTS_TTY_H */
