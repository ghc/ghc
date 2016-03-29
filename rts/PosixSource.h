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

/* We aim for C99 so we need to define following two defines in a consistent way
   with what POSIX/XOPEN provide for C99. Some OSes are particularly picky about
   the right versions defined here, e.g. Solaris
   We also settle on lowest version of POSIX/XOPEN needed for proper C99 support
   here which is POSIX.1-2001 compilation and Open Group Technical Standard,
   Issue 6 (XPG6). XPG6 itself is a result of the merge of X/Open and POSIX
   specification. It is also referred as IEEE Std. 1003.1-2001 or ISO/IEC
   9945:2002 or UNIX 03 and SUSv3.
   Please also see trac ticket #11757 for more information about switch
   to C99/C11.
*/
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE   600

#define __USE_MINGW_ANSI_STDIO 1

#if defined(darwin_HOST_OS)
/* If we don't define this the including sysctl breaks with things like
    /usr/include/bsm/audit.h:224:0:
         error: syntax error before 'u_char'
*/
#define _DARWIN_C_SOURCE 1
#endif

#endif /* POSIXSOURCE_H */
