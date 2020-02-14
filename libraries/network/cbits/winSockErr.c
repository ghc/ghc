#include "HsNet.h"
#include "HsFFI.h"

#if defined(_WIN32)
#include <stdio.h>

/* to the end */

const char*
getWSErrorDescr(int err)
{
  static char  otherErrMsg[256];

  switch (err) {
  case WSAEINTR:  return "Interrupted function call (WSAEINTR)";
  case WSAEBADF:  return "bad socket descriptor (WSAEBADF)";
  case WSAEACCES: return "Permission denied (WSAEACCESS)";
  case WSAEFAULT: return "Bad address (WSAEFAULT)";
  case WSAEINVAL: return "Invalid argument (WSAEINVAL)";
  case WSAEMFILE: return "Too many open files (WSAEMFILE)";
  case WSAEWOULDBLOCK:  return "Resource temporarily unavailable (WSAEWOULDBLOCK)";
  case WSAEINPROGRESS:  return "Operation now in progress (WSAEINPROGRESS)";
  case WSAEALREADY:     return "Operation already in progress (WSAEALREADY)";
  case WSAENOTSOCK:     return "Socket operation on non-socket (WSAENOTSOCK)";
  case WSAEDESTADDRREQ: return "Destination address required (WSAEDESTADDRREQ)";
  case WSAEMSGSIZE:    	return "Message too long (WSAEMSGSIZE)";
  case WSAEPROTOTYPE:   return "Protocol wrong type for socket (WSAEPROTOTYPE)";
  case WSAENOPROTOOPT:  return "Bad protocol option (WSAENOPROTOOPT)";
  case WSAEPROTONOSUPPORT: return "Protocol not supported (WSAEPROTONOSUPPORT)";
  case WSAESOCKTNOSUPPORT: return "Socket type not supported (WSAESOCKTNOSUPPORT)";
  case WSAEOPNOTSUPP:      return "Operation not supported (WSAEOPNOTSUPP)";
  case WSAEPFNOSUPPORT:    return "Protocol family not supported (WSAEPFNOSUPPORT)";
  case WSAEAFNOSUPPORT:    return "Address family not supported by protocol family (WSAEAFNOSUPPORT)";
  case WSAEADDRINUSE:      return "Address already in use (WSAEADDRINUSE)";
  case WSAEADDRNOTAVAIL:   return "Cannot assign requested address (WSAEADDRNOTAVAIL)";
  case WSAENETDOWN:        return "Network is down (WSAENETDOWN)";
  case WSAENETUNREACH:     return "Network is unreachable (WSAENETUNREACH)";
  case WSAENETRESET:       return "Network dropped connection on reset (WSAENETRESET)";
  case WSAECONNABORTED:    return "Software caused connection abort (WSAECONNABORTED)";
  case WSAECONNRESET:      return "Connection reset by peer (WSAECONNRESET)";
  case WSAENOBUFS:         return "No buffer space available (WSAENOBUFS)";
  case WSAEISCONN:         return "Socket is already connected (WSAEISCONN)";
  case WSAENOTCONN:        return "Socket is not connected (WSAENOTCONN)";
  case WSAESHUTDOWN:       return "Cannot send after socket shutdown (WSAESHUTDOWN)";
  case WSAETOOMANYREFS:    return "Too many references (WSAETOOMANYREFS)";
  case WSAETIMEDOUT:       return "Connection timed out (WSAETIMEDOUT)";
  case WSAECONNREFUSED:    return "Connection refused (WSAECONNREFUSED)";
  case WSAELOOP:           return "Too many levels of symbolic links (WSAELOOP)";
  case WSAENAMETOOLONG:    return "Filename too long (WSAENAMETOOLONG)";
  case WSAEHOSTDOWN:       return "Host is down (WSAEHOSTDOWN)";
  case WSAEHOSTUNREACH:    return "Host is unreachable (WSAEHOSTUNREACH)";
  case WSAENOTEMPTY:       return "Resource not empty (WSAENOTEMPTY)";
  case WSAEPROCLIM:        return "Too many processes (WSAEPROCLIM)";
  case WSAEUSERS:          return "Too many users (WSAEUSERS)";
  case WSAEDQUOT:          return "Disk quota exceeded (WSAEDQUOT)";
  case WSAESTALE:          return "Stale NFS file handle (WSAESTALE)";
  case WSAEREMOTE:         return "Too many levels of remote in path (WSAEREMOTE)";
  case WSAEDISCON:         return "Graceful shutdown in progress (WSAEDISCON)";
  case WSASYSNOTREADY:     return "Network subsystem is unavailable (WSASYSNOTREADY)";
  case WSAVERNOTSUPPORTED: return "Winsock.dll version out of range (WSAVERNOTSUPPORTED)";
  case WSANOTINITIALISED:  return "Successful WSAStartup not yet performed (WSANOTINITIALISED)";
#ifdef WSATYPE_NOT_FOUND
  case WSATYPE_NOT_FOUND:  return "Class type not found (WSATYPE_NOT_FOUND)";
#endif
  case WSAHOST_NOT_FOUND:  return "Host not found (WSAHOST_NOT_FOUND)";
  case WSATRY_AGAIN:       return "Nonauthoritative host not found (WSATRY_AGAIN)";
  case WSANO_RECOVERY:     return "This is a nonrecoverable error (WSANO_RECOVERY)";
  case WSANO_DATA:         return "Valid name, no data record of requested type (WSANO_DATA)";
  default:
    sprintf(otherErrMsg, "Unknown WinSock error: %u", err);
    return otherErrMsg;
  }
}

#endif

