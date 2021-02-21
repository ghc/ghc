/* -----------------------------------------------------------------------------
 *
 * Definitions for package `network' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSNET_H
#define HSNET_H

#include "HsNetDef.h"

#if !defined(INLINE)
# if defined(_MSC_VER)
#  define INLINE extern __inline
# elif defined(__GNUC_GNU_INLINE__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

#define _GNU_SOURCE 1 /* for struct ucred on Linux */

#if defined(_WIN32)
# include <winsock2.h>
# include <ws2tcpip.h>
# define IPV6_V6ONLY 27
#endif

#if defined(HAVE_LIMITS_H)
# include <limits.h>
#endif
#if defined(HAVE_STDLIB_H)
# include <stdlib.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
# include <sys/types.h>
#endif
#if defined(HAVE_FCNTL_H)
# include <fcntl.h>
#endif
#if defined(HAVE_SYS_UIO_H)
# include <sys/uio.h>
#endif
#if defined(HAVE_SYS_SOCKET_H)
# include <sys/socket.h>
#endif
#if defined(HAVE_NETINET_IN_H)
# include <netinet/in.h>
#endif
#if defined(HAVE_NETINET_TCP_H)
# include <netinet/tcp.h>
#endif
#if defined(HAVE_SYS_UN_H)
# include <sys/un.h>
#endif
#if defined(HAVE_ARPA_INET_H)
# include <arpa/inet.h>
#endif
#if defined(HAVE_NETDB_H)
#include <netdb.h>
#endif
#if defined(HAVE_NET_IF_H)
# include <net/if.h>
#endif
#if defined(HAVE_NETIOAPI_H)
# include <netioapi.h>
#endif

#if defined(_WIN32)
extern int   initWinSock ();
extern const char* getWSErrorDescr(int err);
extern void* newAcceptParams(int sock, int sz, void* sockaddr);
extern int   acceptNewSock(void* d);
extern int   acceptDoProc(void* param);
#else  /* _WIN32 */
extern int
sendFd(int sock, int outfd);

extern int
recvFd(int sock);
#endif /* _WIN32 */

INLINE char *
hsnet_inet_ntoa(
#if defined(_WIN32)
             u_long addr
#elif defined(HAVE_IN_ADDR_T)
             in_addr_t addr
#elif defined(HAVE_INTTYPES_H)
             u_int32_t addr
#else
             unsigned long addr
#endif
             )
{
    struct in_addr a;
    a.s_addr = addr;
    return inet_ntoa(a);
}

INLINE int
hsnet_getnameinfo(const struct sockaddr* a,socklen_t b, char* c,
# if defined(_WIN32)
                  DWORD d, char* e, DWORD f, int g)
# else
                  socklen_t d, char* e, socklen_t f, int g)
# endif
{
  return getnameinfo(a,b,c,d,e,f,g);
}

INLINE int
hsnet_getaddrinfo(const char *hostname, const char *servname,
        const struct addrinfo *hints, struct addrinfo **res)
{
    return getaddrinfo(hostname, servname, hints, res);
}

INLINE void
hsnet_freeaddrinfo(struct addrinfo *ai)
{
    freeaddrinfo(ai);
}

#if !defined(IOV_MAX)
# define IOV_MAX 1024
#endif

#if !defined(SOCK_NONBLOCK) // Missing define in Bionic libc (Android)
# define SOCK_NONBLOCK O_NONBLOCK
#endif

#endif /* HSNET_H */
