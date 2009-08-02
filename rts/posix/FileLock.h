/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell 98
 *
 * ---------------------------------------------------------------------------*/

#ifndef POSIX_FILELOCK_H
#define POSIX_FILELOCK_H

void initFileLocking(void);
void freeFileLocking(void);

#endif /* POSIX_FILELOCK_H */
