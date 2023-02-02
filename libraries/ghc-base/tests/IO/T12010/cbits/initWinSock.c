#include "HsBase.h"
#include "HsFFI.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)

static int winsock_inited = 0;

static void
shutdownHandler(void)
{
  WSACleanup();
}

/* Initialising WinSock... */
int
initWinSock ()
{
  WORD wVersionRequested;
  WSADATA wsaData;
  int err;

  if (!winsock_inited) {
    wVersionRequested = MAKEWORD( 2, 2 );

    err = WSAStartup ( wVersionRequested, &wsaData );

    if ( err != 0 ) {
       return err;
    }

    if ( LOBYTE( wsaData.wVersion ) != 2 ||
       HIBYTE( wsaData.wVersion ) != 2 ) {
      WSACleanup();
      return (-1);
    }

    atexit(shutdownHandler);
    winsock_inited = 1;
  }
  return 0;
}

#endif
