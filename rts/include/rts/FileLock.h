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

/* Note [RTS File locking]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 *
 * The Haskell report dictates certain file locking behaviour.
 * This is specified in the Haskell98 report under: 21.2.3  File locking
 *
 * GHC does not rely on the platform it's on to implement this.
 * Instead we keep track of locked files in a data structure in
 * the RTS. This file provides the interface to this data structure.
 *
 * In the base libraries we then use this interface to "lock" files.
 * This means it's very much still possible for users outside of the
 * rts/base library to open the files in question even if they are
 * locked.
 * */

#pragma once

#include "Stg.h"

/* No valid FD would be negative, so use a word instead of int so the value
   is compatible with a Windows handle.  */
int  lockFile(StgWord64 id, StgWord64 dev, StgWord64 ino, int for_writing);
int  unlockFile(StgWord64 id);
