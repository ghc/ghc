
#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"


#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK

/* Initialising WinSock... */
StgInt
initWinSock ()
{
  WORD wVersionRequested;
  WSADATA wsaData;  
  int err;

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
  return 0;
}

void
shutdownWinSock()
{
 WSACleanup();
}

#endif
