/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.3 2001/12/21 15:07:26 simonmar Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsCore.h"

/* Raw errno */
/* Covers up the fact that on Windows this is a function */

int *ghcErrno(void) {
  return &errno;
}


/* Wrappers for the individual error codes - boring */
#define ErrCode(x) HsInt prel_error_##x() { return x; }
#define ErrCode2(x,y) HsInt prel_error_##x() { return y; }

#ifdef E2BIG
ErrCode(E2BIG)
#else
ErrCode2(E2BIG,-1)
#endif

#ifdef EACCES
ErrCode(EACCES)
#else
ErrCode2(EACCES,-1)
#endif

#ifdef EADDRINUSE
ErrCode(EADDRINUSE)
#else
ErrCode2(EADDRINUSE,-1)
#endif

#ifdef EADDRNOTAVAIL
ErrCode(EADDRNOTAVAIL)
#else
ErrCode2(EADDRNOTAVAIL,-1)
#endif

#ifdef EADV
ErrCode(EADV)
#else
ErrCode2(EADV,-1)
#endif

#ifdef EAFNOSUPPORT
ErrCode(EAFNOSUPPORT)
#else
ErrCode2(EAFNOSUPPORT,-1)
#endif

#ifdef EAGAIN
ErrCode(EAGAIN)
#else
ErrCode2(EAGAIN,-1)
#endif

#ifdef EALREADY
ErrCode(EALREADY)
#else
ErrCode2(EALREADY,-1)
#endif

#ifdef EBADF
ErrCode(EBADF)
#else
ErrCode2(EBADF,-1)
#endif

#ifdef EBADMSG
ErrCode(EBADMSG)
#else
ErrCode2(EBADMSG,-1)
#endif

#ifdef EBADRPC
ErrCode(EBADRPC)
#else
ErrCode2(EBADRPC,-1)
#endif

#ifdef EBUSY
ErrCode(EBUSY)
#else
ErrCode2(EBUSY,-1)
#endif

#ifdef ECHILD
ErrCode(ECHILD)
#else
ErrCode2(ECHILD,-1)
#endif

#ifdef ECOMM
ErrCode(ECOMM)
#else
ErrCode2(ECOMM,-1)
#endif

#ifdef ECONNABORTED
ErrCode(ECONNABORTED)
#else
ErrCode2(ECONNABORTED,-1)
#endif

#ifdef ECONNREFUSED
ErrCode(ECONNREFUSED)
#else
ErrCode2(ECONNREFUSED,-1)
#endif

#ifdef ECONNRESET
ErrCode(ECONNRESET)
#else
ErrCode2(ECONNRESET,-1)
#endif

#ifdef EDEADLK
ErrCode(EDEADLK)
#else
ErrCode2(EDEADLK,-1)
#endif

#ifdef EDESTADDRREQ
ErrCode(EDESTADDRREQ)
#else
ErrCode2(EDESTADDRREQ,-1)
#endif

#ifdef EDIRTY
ErrCode(EDIRTY)
#else
ErrCode2(EDIRTY,-1)
#endif

#ifdef EDOM
ErrCode(EDOM)
#else
ErrCode2(EDOM,-1)
#endif

#ifdef EDQUOT
ErrCode(EDQUOT)
#else
ErrCode2(EDQUOT,-1)
#endif

#ifdef EEXIST
ErrCode(EEXIST)
#else
ErrCode2(EEXIST,-1)
#endif

#ifdef EFAULT
ErrCode(EFAULT)
#else
ErrCode2(EFAULT,-1)
#endif

#ifdef EFBIG
ErrCode(EFBIG)
#else
ErrCode2(EFBIG,-1)
#endif

#ifdef EFTYPE
ErrCode(EFTYPE)
#else
ErrCode2(EFTYPE,-1)
#endif

#ifdef EHOSTDOWN
ErrCode(EHOSTDOWN)
#else
ErrCode2(EHOSTDOWN,-1)
#endif

#ifdef EHOSTUNREACH
ErrCode(EHOSTUNREACH)
#else
ErrCode2(EHOSTUNREACH,-1)
#endif

#ifdef EIDRM
ErrCode(EIDRM)
#else
ErrCode2(EIDRM,-1)
#endif

#ifdef EILSEQ
ErrCode(EILSEQ)
#else
ErrCode2(EILSEQ,-1)
#endif

#ifdef EINPROGRESS
ErrCode(EINPROGRESS)
#else
ErrCode2(EINPROGRESS,-1)
#endif

#ifdef EINTR
ErrCode(EINTR)
#else
ErrCode2(EINTR,-1)
#endif

#ifdef EINVAL
ErrCode(EINVAL)
#else
ErrCode2(EINVAL,-1)
#endif

#ifdef EIO
ErrCode(EIO)
#else
ErrCode2(EIO,-1)
#endif

#ifdef EISCONN
ErrCode(EISCONN)
#else
ErrCode2(EISCONN,-1)
#endif

#ifdef EISDIR
ErrCode(EISDIR)
#else
ErrCode2(EISDIR,-1)
#endif

#ifdef ELOOP
ErrCode(ELOOP)
#else
ErrCode2(ELOOP,-1)
#endif

#ifdef EMFILE
ErrCode(EMFILE)
#else
ErrCode2(EMFILE,-1)
#endif

#ifdef EMLINK
ErrCode(EMLINK)
#else
ErrCode2(EMLINK,-1)
#endif

#ifdef EMSGSIZE
ErrCode(EMSGSIZE)
#else
ErrCode2(EMSGSIZE,-1)
#endif

#ifdef EMULTIHOP
ErrCode(EMULTIHOP)
#else
ErrCode2(EMULTIHOP,-1)
#endif

#ifdef ENAMETOOLONG
ErrCode(ENAMETOOLONG)
#else
ErrCode2(ENAMETOOLONG,-1)
#endif

#ifdef ENETDOWN
ErrCode(ENETDOWN)
#else
ErrCode2(ENETDOWN,-1)
#endif

#ifdef ENETRESET
ErrCode(ENETRESET)
#else
ErrCode2(ENETRESET,-1)
#endif

#ifdef ENETUNREACH
ErrCode(ENETUNREACH)
#else
ErrCode2(ENETUNREACH,-1)
#endif

#ifdef ENFILE
ErrCode(ENFILE)
#else
ErrCode2(ENFILE,-1)
#endif

#ifdef ENOBUFS
ErrCode(ENOBUFS)
#else
ErrCode2(ENOBUFS,-1)
#endif

#ifdef ENODATA
ErrCode(ENODATA)
#else
ErrCode2(ENODATA,-1)
#endif

#ifdef ENODEV
ErrCode(ENODEV)
#else
ErrCode2(ENODEV,-1)
#endif

#ifdef ENOENT
ErrCode(ENOENT)
#else
ErrCode2(ENOENT,-1)
#endif

#ifdef ENOEXEC
ErrCode(ENOEXEC)
#else
ErrCode2(ENOEXEC,-1)
#endif

#ifdef ENOLCK
ErrCode(ENOLCK)
#else
ErrCode2(ENOLCK,-1)
#endif

#ifdef ENOLINK
ErrCode(ENOLINK)
#else
ErrCode2(ENOLINK,-1)
#endif

#ifdef ENOMEM
ErrCode(ENOMEM)
#else
ErrCode2(ENOMEM,-1)
#endif

#ifdef ENOMSG
ErrCode(ENOMSG)
#else
ErrCode2(ENOMSG,-1)
#endif

#ifdef ENONET
ErrCode(ENONET)
#else
ErrCode2(ENONET,-1)
#endif

#ifdef ENOPROTOOPT
ErrCode(ENOPROTOOPT)
#else
ErrCode2(ENOPROTOOPT,-1)
#endif

#ifdef ENOSPC
ErrCode(ENOSPC)
#else
ErrCode2(ENOSPC,-1)
#endif

#ifdef ENOSR
ErrCode(ENOSR)
#else
ErrCode2(ENOSR,-1)
#endif

#ifdef ENOSTR
ErrCode(ENOSTR)
#else
ErrCode2(ENOSTR,-1)
#endif

#ifdef ENOSYS
ErrCode(ENOSYS)
#else
ErrCode2(ENOSYS,-1)
#endif

#ifdef ENOTBLK
ErrCode(ENOTBLK)
#else
ErrCode2(ENOTBLK,-1)
#endif

#ifdef ENOTCONN
ErrCode(ENOTCONN)
#else
ErrCode2(ENOTCONN,-1)
#endif

#ifdef ENOTDIR
ErrCode(ENOTDIR)
#else
ErrCode2(ENOTDIR,-1)
#endif

#ifdef ENOTEMPTY
ErrCode(ENOTEMPTY)
#else
ErrCode2(ENOTEMPTY,-1)
#endif

#ifdef ENOTSOCK
ErrCode(ENOTSOCK)
#else
ErrCode2(ENOTSOCK,-1)
#endif

#ifdef ENOTTY
ErrCode(ENOTTY)
#else
ErrCode2(ENOTTY,-1)
#endif

#ifdef ENXIO
ErrCode(ENXIO)
#else
ErrCode2(ENXIO,-1)
#endif

#ifdef EOPNOTSUPP
ErrCode(EOPNOTSUPP)
#else
ErrCode2(EOPNOTSUPP,-1)
#endif

#ifdef EPERM
ErrCode(EPERM)
#else
ErrCode2(EPERM,-1)
#endif

#ifdef EPFNOSUPPORT
ErrCode(EPFNOSUPPORT)
#else
ErrCode2(EPFNOSUPPORT,-1)
#endif

#ifdef EPIPE
ErrCode(EPIPE)
#else
ErrCode2(EPIPE,-1)
#endif

#ifdef EPROCLIM
ErrCode(EPROCLIM)
#else
ErrCode2(EPROCLIM,-1)
#endif

#ifdef EPROCUNAVAIL
ErrCode(EPROCUNAVAIL)
#else
ErrCode2(EPROCUNAVAIL,-1)
#endif

#ifdef EPROGMISMATCH
ErrCode(EPROGMISMATCH)
#else
ErrCode2(EPROGMISMATCH,-1)
#endif

#ifdef EPROGUNAVAIL
ErrCode(EPROGUNAVAIL)
#else
ErrCode2(EPROGUNAVAIL,-1)
#endif

#ifdef EPROTO
ErrCode(EPROTO)
#else
ErrCode2(EPROTO,-1)
#endif

#ifdef EPROTONOSUPPORT
ErrCode(EPROTONOSUPPORT)
#else
ErrCode2(EPROTONOSUPPORT,-1)
#endif

#ifdef EPROTOTYPE
ErrCode(EPROTOTYPE)
#else
ErrCode2(EPROTOTYPE,-1)
#endif

#ifdef ERANGE
ErrCode(ERANGE)
#else
ErrCode2(ERANGE,-1)
#endif

#ifdef EREMCHG
ErrCode(EREMCHG)
#else
ErrCode2(EREMCHG,-1)
#endif

#ifdef EREMOTE
ErrCode(EREMOTE)
#else
ErrCode2(EREMOTE,-1)
#endif

#ifdef EROFS
ErrCode(EROFS)
#else
ErrCode2(EROFS,-1)
#endif

#ifdef ERPCMISMATCH
ErrCode(ERPCMISMATCH)
#else
ErrCode2(ERPCMISMATCH,-1)
#endif

#ifdef ERREMOTE
ErrCode(ERREMOTE)
#else
ErrCode2(ERREMOTE,-1)
#endif

#ifdef ESHUTDOWN
ErrCode(ESHUTDOWN)
#else
ErrCode2(ESHUTDOWN,-1)
#endif

#ifdef ESOCKTNOSUPPORT
ErrCode(ESOCKTNOSUPPORT)
#else
ErrCode2(ESOCKTNOSUPPORT,-1)
#endif

#ifdef ESPIPE
ErrCode(ESPIPE)
#else
ErrCode2(ESPIPE,-1)
#endif

#ifdef ESRCH
ErrCode(ESRCH)
#else
ErrCode2(ESRCH,-1)
#endif

#ifdef ESRMNT
ErrCode(ESRMNT)
#else
ErrCode2(ESRMNT,-1)
#endif

#ifdef ESTALE
ErrCode(ESTALE)
#else
ErrCode2(ESTALE,-1)
#endif

#ifdef ETIME
ErrCode(ETIME)
#else
ErrCode2(ETIME,-1)
#endif

#ifdef ETIMEDOUT
ErrCode(ETIMEDOUT)
#else
ErrCode2(ETIMEDOUT,-1)
#endif

#ifdef ETOOMANYREFS
ErrCode(ETOOMANYREFS)
#else
ErrCode2(ETOOMANYREFS,-1)
#endif

#ifdef ETXTBSY
ErrCode(ETXTBSY)
#else
ErrCode2(ETXTBSY,-1)
#endif

#ifdef EUSERS
ErrCode(EUSERS)
#else
ErrCode2(EUSERS,-1)
#endif

#ifdef EWOULDBLOCK
ErrCode(EWOULDBLOCK)
#else
ErrCode2(EWOULDBLOCK,-1)
#endif

#ifdef EXDEV
ErrCode(EXDEV)
#else
ErrCode2(EXDEV,-1)
#endif

