
#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"


#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK

static int winsock_inited = 0;
static int winsock_uninited = 0;

/* Initialising WinSock... */
StgInt
initWinSock ()
{
  WORD wVersionRequested;
  WSADATA wsaData;  
  int err;

  if (!winsock_inited) {
    wVersionRequested = MAKEWORD( 1, 1 );

    err = WSAStartup ( wVersionRequested, &wsaData );

    if ( err != 0 ) {
       return err;
    }

    if ( LOBYTE( wsaData.wVersion ) != 1 ||
       HIBYTE( wsaData.wVersion ) != 1 ) {
      WSACleanup();
      return (-1);
    }
    winsock_inited = 1;
  }
  return 0;
}

static void
shutdownHandler()
{
  WSACleanup();
}

void
shutdownWinSock()
{
    if (!winsock_uninited) {
	atexit(shutdownHandler);
	winsock_uninited = 1;
    }
}

#endif
