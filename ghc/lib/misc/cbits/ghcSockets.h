#ifndef GHC_SOCKETS_H
#define GHC_SOCKETS_H

#include <ctype.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#include <sys/uio.h>

/* ToDo: featurise this */
#ifndef cygwin32_TARGET_OS
#include <sys/un.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

/* acceptSocket.lc */
StgInt	acceptSocket PROTO((StgInt, StgAddr, StgAddr));

/* bindSocket.lc */
StgInt	bindSocket PROTO((StgInt, StgAddr, StgInt, StgInt));

/* connectSocket.lc */
StgInt	connectSocket PROTO((StgInt, StgAddr, StgInt, StgInt));

/* createSocket.lc */
StgInt	createSocket PROTO((StgInt, StgInt, StgInt));

/* getSockName.lc */
StgInt	getSockName PROTO((StgInt, StgAddr, StgAddr));

/* getPeerName.lc */
StgInt	getPeerName PROTO((StgInt, StgAddr, StgAddr));

/* listenSocket.lc */
StgInt	listenSocket PROTO((StgInt, StgInt));

/* shutdownSocket.lc */
StgInt	shutdownSocket PROTO((StgInt, StgInt));

/* readDescriptor.lc */
StgInt	readDescriptor PROTO((StgInt, StgAddr, StgInt));

/* writeDescriptor.lc */
StgInt	writeDescriptor PROTO((StgInt, StgAddr, StgInt));


#endif /* !GHC_SOCKETS_H */
