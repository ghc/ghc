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

#define _POSIX_SOURCE   1
#define _POSIX_C_SOURCE 199506L
#define _XOPEN_SOURCE   500
#define _ISOC99_SOURCE

/* Let's be ISO C99 too... */

#if defined(darwin_HOST_OS)
/* If we don't define this the including sysctl breaks with things like
    /usr/include/bsm/audit.h:224:0:
         error: syntax error before 'u_char'
*/
#define _DARWIN_C_SOURCE 1
#endif

#endif /* POSIXSOURCE_H */
