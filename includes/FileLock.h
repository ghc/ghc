/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell 98
 *
 * ---------------------------------------------------------------------------*/

void initFileLocking(void);
void freeFileLocking(void);
int  lockFile(int fd, dev_t dev, ino_t ino, int for_writing);
int  unlockFile(int fd);
