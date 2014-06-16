/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007-2009
 *
 * File locking support as required by Haskell
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_FILELOCK_H
#define RTS_FILELOCK_H

#include "Stg.h"

int  lockFile(int fd, StgWord64 dev, StgWord64 ino, int for_writing);
int  unlockFile(int fd);

#endif /* RTS_FILELOCK_H */
