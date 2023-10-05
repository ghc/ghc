/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * POSIX TTY-related functionality
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

void* __hscore_get_saved_termios(int fd);
void  __hscore_set_saved_termios(int fd, void* ts);
