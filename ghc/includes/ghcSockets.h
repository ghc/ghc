#ifndef GHC_SOCKETS_H
#define GHC_SOCKETS_H

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

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
