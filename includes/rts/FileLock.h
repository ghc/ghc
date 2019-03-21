/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007-2009
 *
 * File locking support as required by Haskell
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Stg.h"

int  lockFile(int fd, StgWord64 dev, StgWord64 ino, int for_writing);
int  unlockFile(int fd);
