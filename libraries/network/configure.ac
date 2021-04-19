AC_INIT([Haskell network package],
        [3.1.1.1],
        [libraries@haskell.org],
        [network])

dnl See also HsNet.h
ac_includes_default="#define _GNU_SOURCE 1  /* for struct ucred on Linux */
$ac_includes_default

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
"

AC_CONFIG_SRCDIR([include/HsNet.h])

AC_CONFIG_HEADERS([include/HsNetworkConfig.h])

AC_CANONICAL_HOST

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_C_CONST

AC_CHECK_HEADERS([limits.h stdlib.h unistd.h sys/types.h fcntl.h])
AC_CHECK_HEADERS([sys/uio.h sys/socket.h netinet/in.h netinet/tcp.h])
AC_CHECK_HEADERS([sys/un.h arpa/inet.h netdb.h])
AC_CHECK_HEADERS([net/if.h netioapi.h])

AC_CHECK_TYPES([struct ucred])

AC_CHECK_FUNCS([gai_strerror gethostent accept4])
AC_CHECK_FUNCS([getpeereid])

AC_CHECK_DECLS([AI_ADDRCONFIG, AI_ALL, AI_NUMERICSERV, AI_V4MAPPED])
AC_CHECK_DECLS([IPV6_V6ONLY])
AC_CHECK_DECLS([IPPROTO_IP, IPPROTO_TCP, IPPROTO_IPV6])
AC_CHECK_DECLS([SO_PEERCRED])

AC_CHECK_MEMBERS([struct msghdr.msg_control, struct msghdr.msg_accrights])
AC_CHECK_MEMBERS([struct sockaddr.sa_len])

dnl This is a necessary hack
AC_MSG_NOTICE([creating ./network.buildinfo])
echo "install-includes: HsNetworkConfig.h" > network.buildinfo

AC_OUTPUT
