#include "HsNet.h"
#include "HsFFI.h"

#if defined(_WIN32)

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

SOCKET
wsaDuplicate (SOCKET s)
{
  WSAPROTOCOL_INFOW protocolInfo;
  if (WSADuplicateSocketW (s, GetCurrentProcessId (), &protocolInfo) != 0)
    return -1;

  SOCKET res = WSASocketW(FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO,
                          FROM_PROTOCOL_INFO, &protocolInfo, 0, 0);
  if (res == SOCKET_ERROR)
    return -1;

  return res;
}

#endif
