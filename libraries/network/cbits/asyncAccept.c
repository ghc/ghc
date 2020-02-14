/*
 * (c) sof, 2003.
 */

#include "HsNet.h"
#include "HsFFI.h"

#if defined(_WIN32)

/* all the way to the end */

/*
 * To support non-blocking accept()s with WinSock, we use the asyncDoProc#
 * primop, which lets a Haskell thread call an external routine without
 * blocking the progress of other threads.
 *
 * As can readily be seen, this is a low-level mechanism.
 *
 */

typedef struct AcceptData {
    int   fdSock;
    int   newSock;
    void* sockAddr;
    int   size;
} AcceptData;

/*
 * Fill in parameter block that's passed along when the RTS invokes the
 * accept()-calling proc below (acceptDoProc())
 */
void*
newAcceptParams(int sock,
		int sz,
		void* sockaddr)
{
    AcceptData* data = (AcceptData*)malloc(sizeof(AcceptData));
    if (!data) return NULL;
    data->fdSock   = sock;
    data->newSock  = 0;
    data->sockAddr = sockaddr;
    data->size     = sz;

    return data;
}

/* Accessors for return code and accept()'s socket result. */

int
acceptNewSock(void* d)
{
    return (((AcceptData*)d)->newSock);
}

/* Routine invoked by an RTS worker thread */
int
acceptDoProc(void* param)
{
    SOCKET s;

    AcceptData* data = (AcceptData*)param;
    s = accept( data->fdSock,
		data->sockAddr,
		&data->size);
    data->newSock = s;
    if ( s == INVALID_SOCKET ) {
	return GetLastError();
    } else {
	return 0;
    }
}
#endif
