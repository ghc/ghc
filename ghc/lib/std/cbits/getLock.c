/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getLock.c,v 1.3 1998/12/02 13:27:41 simonm Exp $
 *
 * stdin/stout/stderr Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifndef FD_SETSIZE
#define FD_SETSIZE 256
#endif

typedef struct {
    dev_t device;
    ino_t inode;
    int fd;
} Lock;

static Lock readLock[FD_SETSIZE];
static Lock writeLock[FD_SETSIZE];

static int readLocks = 0;
static int writeLocks = 0;

int
lockFile(fd, exclusive)
int fd;
int exclusive;
{
    int i;
    struct stat sb;

    while (fstat(fd, &sb) < 0) {
	if (errno != EINTR) {
	    return -1;
	}
    }

    /* Only lock regular files */
    if (!S_ISREG(sb.st_mode))
	return 0;
    
    for (i = 0; i < writeLocks; i++)
	if (writeLock[i].inode == sb.st_ino && writeLock[i].device == sb.st_dev) {
	    errno = EAGAIN;
	    return -1;
	}

    if (!exclusive) {
	i = readLocks++;
	readLock[i].device = sb.st_dev;
	readLock[i].inode = sb.st_ino;
	readLock[i].fd = fd;
	return 0;
    }

    for (i = 0; i < readLocks; i++)
	if (readLock[i].inode == sb.st_ino && readLock[i].device == sb.st_dev) {
	    errno = EAGAIN;
	    return -1;
	}	    

    i = writeLocks++;
    writeLock[i].device = sb.st_dev;
    writeLock[i].inode = sb.st_ino;
    writeLock[i].fd = fd;
    return 0;
}

int
unlockFile(fd)
int fd;
{
    int i, rc;

    for (i = 0; i < readLocks; i++)
	if (readLock[i].fd == fd) {
	    while (++i < readLocks)
		readLock[i - 1] = readLock[i];
	    readLocks--;
	    return 0;
	}

    for (i = 0; i < writeLocks; i++)
	if (writeLock[i].fd == fd) {
	    while (++i < writeLocks)
		writeLock[i - 1] = writeLock[i];
	    writeLocks--;
	    return 0;
	}
     /* Signal that we did not find an entry */
    return 1;
}

StgInt
getLock(fd, exclusive)
StgInt fd;
StgInt exclusive;
{
    if (lockFile(fd, exclusive) < 0) {
	if (errno == EBADF)
	    return 0;
	else {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_EACCES:
	    case GHC_EAGAIN:
		ghc_errtype = ERR_RESOURCEBUSY;
		ghc_errstr = "file is locked";
		break;
	    }
	    /* Not so sure we want to do this, since getLock() 
	    is only called on the standard file descriptors.. */
	    /*(void) close(fd); */
	    return -1;
	}
    }
    return 1;
}
