/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: echoAux.c,v 1.3 1999/03/01 09:02:04 sof Exp $
 *
 * Support functions for changing echoing
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

StgInt
setTerminalEcho(ptr, on)
StgForeignPtr ptr;
StgInt on;
{
   IOFileObject* fo = (IOFileObject*)ptr;
   struct termios tios;
   int fd, rc;

   fd = fo->fd;

#ifndef mingw32_TARGET_OS
   while ( (rc = tcgetattr(fd,&tios)) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
   }

   if (on) {
     tios.c_lflag |= ECHO;
   } else {
     tios.c_lflag &= ~ECHO;
   }

   while ( (rc = tcsetattr(fd,TCSANOW,&tios)) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
   }
#endif
  return 0;
}

StgInt
getTerminalEcho(ptr)
StgForeignPtr ptr;
{
   IOFileObject* fo = (IOFileObject*)ptr;
   struct termios tios;
   int fd, rc;

   fd = fo->fd;

#ifndef mingw32_TARGET_OS
   while ( (rc = tcgetattr(fd,&tios)) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
   }
   return (tios.c_cflag & ECHO ? 1 : 0);
#else
   return 0;
#endif
}

StgInt
isTerminalDevice(ptr)
StgForeignPtr ptr;
{
   IOFileObject* fo = (IOFileObject*)ptr;
   struct termios tios;
   int fd, rc;

   fd = fo -> fd;

#ifndef mingw32_TARGET_OS
   while ( (rc = tcgetattr(fd,&tios)) == -1) {
        if (errno == ENOTTY) return 0;
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
   }
   return 1;
#else
   return 0;
#endif
}
