/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: inputReady.c,v 1.1 2001/06/28 14:15:04 simonmar Exp $
 *
 * hReady Runtime Support
 */

/* select and supporting types is not */
#ifndef _AIX
#define NON_POSIX_SOURCE  
#endif

#include "HsCore.h"

/*
 * inputReady(fd) checks to see whether input is available on the file
 * descriptor 'fd'.  Input meaning 'can I safely read at least a
 * *character* from this file object without blocking?'
 */
int
inputReady(int fd, int msecs)
{
    int maxfd, ready;
#ifndef mingw32_TARGET_OS
    fd_set rfd;
    struct timeval tv;
#endif

#ifdef mingw32_TARGET_OS
    return 1;
#else
    FD_ZERO(&rfd);
    FD_SET(fd, &rfd);

    /* select() will consider the descriptor set in the range of 0 to
     * (maxfd-1) 
     */
    maxfd = fd + 1;
    tv.tv_sec  = msecs / 1000;
    tv.tv_usec = msecs % 1000;

    while ((ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
      if (errno != EINTR ) {
          return -1;
      }
   }

    /* 1 => Input ready, 0 => not ready, -1 => error */
    return (ready);

#endif
}
