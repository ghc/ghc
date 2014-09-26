/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Include this file into sources which should not need any non-Posix services.
 * That includes most RTS C sources.
 * ---------------------------------------------------------------------------*/

#ifndef POSIXSOURCE_H
#define POSIXSOURCE_H

#include <ghcplatform.h>

#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE   600
#else
#define _POSIX_SOURCE   1
#define _POSIX_C_SOURCE 199506L
#define _XOPEN_SOURCE   500
// FreeBSD takes a different approach to _ISOC99_SOURCE: on FreeBSD it
// means "I want *just* C99 things", whereas on GNU libc and Solaris
// it means "I also want C99 things".
//
// On both GNU libc and FreeBSD, _ISOC99_SOURCE is implied by
// _XOPEN_SOURCE==600, but on Solaris it is an error to omit it.
#define _ISOC99_SOURCE
// Defining __USE_MINGW_ANSI_STDIO is the most portable way to tell
// mingw that we want to use the standard %lld style format specifiers,
// rather than the Windows %I64d style
#define __USE_MINGW_ANSI_STDIO 1
#endif

#if defined(darwin_HOST_OS)
/* If we don't define this the including sysctl breaks with things like
    /usr/include/bsm/audit.h:224:0:
         error: syntax error before 'u_char'
*/
#define _DARWIN_C_SOURCE 1
#endif

#endif /* POSIXSOURCE_H */
