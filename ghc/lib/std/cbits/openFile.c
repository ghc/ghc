/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: openFile.c,v 1.5 1999/01/23 17:44:40 sof Exp $
 *
 * openFile Runtime Support
 */

/* We use lstat, which is sadly not POSIX */
#define NON_POSIX_SOURCE

#include "Rts.h"
#include "stgio.h"
#include "fileObject.h"

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

IOFileObject*
openStdFile(fd,flags,rd)
StgInt fd;
StgInt flags;
StgInt rd;
{
    IOFileObject* fo;

    if ((fo = malloc(sizeof(IOFileObject))) == NULL)
       return NULL;
    fo->fd       = fd;
    fo->buf      = NULL;
    fo->bufWPtr  = 0;
    fo->bufRPtr  = 0;
    fo->flags   = flags | FILEOBJ_STD | ( rd ? FILEOBJ_READ : FILEOBJ_WRITE);
    fo->connectedTo = NULL;
    return fo;
}

#define OPENFILE_APPEND 0
#define OPENFILE_WRITE 1
#define OPENFILE_READ_ONLY 2
#define OPENFILE_READ_WRITE 3

IOFileObject*
openFile(file, how, binary, flags)
StgByteArray file;
StgInt how;
StgInt binary;
StgInt flags;
{
    FILE *fp;
    int fd;
    int oflags;
    int exclusive;
    int created = 0;
    struct stat sb;
    IOFileObject* fo;

    /*
     * Since we aren't supposed to succeed when we're opening for writing and
     * there's another writer, we can't just do an open() with O_WRONLY.
     */

    switch (how) {
      case OPENFILE_APPEND:
        oflags = O_WRONLY | O_NOCTTY | O_APPEND; 
	exclusive = 1;
	break;
      case OPENFILE_WRITE:
	oflags = O_WRONLY | O_NOCTTY;
	exclusive = 1;
	break;
    case OPENFILE_READ_ONLY:
        oflags = O_RDONLY | O_NOCTTY;
	exclusive = 0;
	break;
    case OPENFILE_READ_WRITE:
	oflags = O_RDWR | O_NOCTTY;
	exclusive = 0;
	break;
    default:
	fprintf(stderr, "openFile: unknown mode `%d'\n", how);
	exit(EXIT_FAILURE);
    }

#if HAVE_O_BINARY
    if (binary) 
      oflags |= O_BINARY;
#endif

    /* First try to open without creating */
    while ((fd = open(file, oflags, 0666)) < 0) {
	if (errno == ENOENT) {
	    if ( how == OPENFILE_READ_ONLY ) {
		/* For ReadMode, just bail out now */
		ghc_errtype = ERR_NOSUCHTHING;
		ghc_errstr = "file does not exist";
		return NULL;
	    } else {
		/* If it is a dangling symlink, break off now, too. */
		struct stat st;
		if ( lstat(file,&st) == 0) {
		   ghc_errtype = ERR_NOSUCHTHING;
		   ghc_errstr = "dangling symlink";
		   return NULL;
		}
            }
	    /* Now try to create it */
	    while ((fd = open(file, oflags | O_CREAT | O_EXCL, 0666)) < 0) {
		if (errno == EEXIST) {
		    /* Race detected; go back and open without creating it */
		    break;
		} else if (errno != EINTR) {
		    cvtErrno();
		    switch (ghc_errno) {
		    default:
			stdErrno();
			break;
		    case GHC_ENOENT:
		    case GHC_ENOTDIR:
			ghc_errtype = ERR_NOSUCHTHING;
			ghc_errstr = "no path to file";
			break;
		    case GHC_EINVAL:
			ghc_errtype = ERR_PERMISSIONDENIED;
			ghc_errstr = "unsupported owner or group";
			break;
		    }
		    return NULL;
		}
	    }
	    if (fd >= 0) {
		created = 1;
		break;
	    }
	} else if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_ENOTDIR:
		ghc_errtype = ERR_NOSUCHTHING;
		ghc_errstr = "no path to file";
		break;
	    case GHC_EINVAL:
		ghc_errtype = ERR_PERMISSIONDENIED;
		ghc_errstr = "unsupported owner or group";
		break;
	    }
	    return NULL;
	}
    }

    /* Make sure that we aren't looking at a directory */

    while (fstat(fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    if (created)
		(void) unlink(file);
	    (void) close(fd);
	    return NULL;
	}
    }
    if (S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "file is a directory";
	/* We can't have created it in this case. */
	(void) close(fd);

	return NULL;
    }
    /* Use our own personal locking */

    if (lockFile(fd, exclusive) < 0) {
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
	if (created)
	    (void) unlink(file);
	(void) close(fd);
	return NULL;
    }

    /*
     * Write mode is supposed to truncate the file.  Unfortunately, our pal
     * ftruncate() is non-POSIX, so we truncate with a second open, which may fail.
     */

    if ( how == OPENFILE_WRITE ) {
	int fd2, oflags2;

	oflags2 = oflags | O_TRUNC;
	while ((fd2 = open(file, oflags2, 0666)) < 0) {
	    if (errno != EINTR) {
		cvtErrno();
		if (created)
		    (void) unlink(file);
		(void) close(fd);
		switch (ghc_errno) {
		default:
		    stdErrno();
		    break;
		case GHC_EAGAIN:
		    ghc_errtype = ERR_RESOURCEBUSY;
		    ghc_errstr = "enforced lock prevents truncation";
		    break;
		case GHC_ENOTDIR:
		    ghc_errtype = ERR_NOSUCHTHING;
		    ghc_errstr = "no path to file";
		    break;
		case GHC_EINVAL:
		    ghc_errtype = ERR_PERMISSIONDENIED;
		    ghc_errstr = "unsupported owner or group";
		    break;
		}
		return NULL;
	    }
	}
	close(fd2);
    }

    /* Allocate a IOFileObject to hold the information
       we need to record per-handle for the various C stubs.
       This chunk of memory is wrapped up inside a foreign object,
       so it will be finalised and freed properly when we're
       through with the handle.
    */
    if ((fo = malloc(sizeof(IOFileObject))) == NULL)
       return NULL;

    fo->fd       = fd;
    fo->buf      = NULL;
    fo->bufWPtr  = 0;
    fo->bufRPtr  = 0;
    fo->flags   = flags | ( (how == OPENFILE_READ_ONLY || how == OPENFILE_READ_WRITE) ? FILEOBJ_READ  : 0)
			| ( (how == OPENFILE_APPEND    || how == OPENFILE_READ_WRITE) ? FILEOBJ_WRITE : 0);
    fo->connectedTo = NULL;
    return fo;
}

/* `Lock' file descriptor and return file object. */
IOFileObject*
openFd(fd,oflags,flags)
StgInt fd;
StgInt oflags;
StgInt flags;
{
    int exclusive;
    FILE* fp;
    IOFileObject* fo;

    if (lockFile(fd, exclusive) < 0) {
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
	return NULL;
    }

    /* See openFileObject() comment */
    if ((fo = malloc(sizeof(IOFileObject))) == NULL)
       return NULL;
    fo->fd      = fd;
    fo->buf     = NULL;
    fo->bufWPtr = 0;
    fo->bufRPtr = 0;
    fo->flags   = flags | ( oflags & O_RDONLY ? FILEOBJ_READ 
    			  : oflags & O_RDWR   ? FILEOBJ_READ 
			  : 0)
			| ( oflags & O_WRONLY ? FILEOBJ_WRITE
			  : oflags & O_RDWR   ? FILEOBJ_WRITE 
			  : 0);
    fo->connectedTo = NULL;
    return fo;
}
