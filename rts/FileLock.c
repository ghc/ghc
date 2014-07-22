/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "FileLock.h"
#include "Hash.h"
#include "RtsUtils.h"

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

typedef struct {
    StgWord64 device;
    StgWord64 inode;
    int   readers; // >0 : readers,  <0 : writers
} Lock;

// Two hash tables.  The first maps objects (device/inode pairs) to
// Lock objects containing the number of active readers or writers.  The
// second maps file descriptors to lock objects, so that we can unlock
// by FD without needing to fstat() again.
static HashTable *obj_hash;
static HashTable *fd_hash;

#ifdef THREADED_RTS
static Mutex file_lock_mutex;
#endif

static int cmpLocks(StgWord w1, StgWord w2)
{
    Lock *l1 = (Lock *)w1;
    Lock *l2 = (Lock *)w2;
    return (l1->device == l2->device && l1->inode == l2->inode);
}

static int hashLock(HashTable *table, StgWord w)
{
    Lock *l = (Lock *)w;
    // Just xor all 32-bit words of inode and device, hope this is good enough.
    return hashWord(table, l->inode ^ (l->inode >> 32) ^ l->device ^ (l->device >> 32));
}

void
initFileLocking(void)
{
    obj_hash = allocHashTable_(hashLock, cmpLocks);
    fd_hash  = allocHashTable(); /* ordinary word-based table */
#ifdef THREADED_RTS
    initMutex(&file_lock_mutex);
#endif
}

static void
freeLock(void *lock)
{
    stgFree(lock);
}

void
freeFileLocking(void)
{
    freeHashTable(obj_hash, freeLock);
    freeHashTable(fd_hash,  NULL);
#ifdef THREADED_RTS
    closeMutex(&file_lock_mutex);
#endif
}

int
lockFile(int fd, StgWord64 dev, StgWord64 ino, int for_writing)
{
    Lock key, *lock;

    ACQUIRE_LOCK(&file_lock_mutex);

    key.device = dev;
    key.inode  = ino;

    lock = lookupHashTable(obj_hash, (StgWord)&key);

    if (lock == NULL)
    {
        lock = stgMallocBytes(sizeof(Lock), "lockFile");
        lock->device = dev;
        lock->inode  = ino;
        lock->readers = for_writing ? -1 : 1;
        insertHashTable(obj_hash, (StgWord)lock, (void *)lock);
        insertHashTable(fd_hash, fd, lock);
        RELEASE_LOCK(&file_lock_mutex);
        return 0;
    }
    else
    {
        // single-writer/multi-reader locking:
        if (for_writing || lock->readers < 0) {
            RELEASE_LOCK(&file_lock_mutex);
            return -1;
        }
        insertHashTable(fd_hash, fd, lock);
        lock->readers++;
        RELEASE_LOCK(&file_lock_mutex);
        return 0;
    }
}

int
unlockFile(int fd)
{
    Lock *lock;

    ACQUIRE_LOCK(&file_lock_mutex);

    lock = lookupHashTable(fd_hash, fd);
    if (lock == NULL) {
        // errorBelch("unlockFile: fd %d not found", fd);
        // This is normal: we didn't know when calling unlockFile
        // whether this FD referred to a locked file or not.
        RELEASE_LOCK(&file_lock_mutex);
        return 1;
    }

    if (lock->readers < 0) {
        lock->readers++;
    } else {
        lock->readers--;
    }

    if (lock->readers == 0) {
        removeHashTable(obj_hash, (StgWord)lock, NULL);
        stgFree(lock);
    }
    removeHashTable(fd_hash, fd, NULL);

    RELEASE_LOCK(&file_lock_mutex);
    return 0;
}
