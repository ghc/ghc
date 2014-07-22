/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell
 *
 * ---------------------------------------------------------------------------*/

#ifndef POSIX_FILELOCK_H
#define POSIX_FILELOCK_H

RTS_PRIVATE void initFileLocking(void);
RTS_PRIVATE void freeFileLocking(void);

#endif /* POSIX_FILELOCK_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
