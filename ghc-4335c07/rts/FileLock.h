/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE void initFileLocking(void);
RTS_PRIVATE void freeFileLocking(void);
