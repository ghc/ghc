/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Include this file into sources which should not need any non-Posix services.
 * That includes most RTS C sources.
 * ---------------------------------------------------------------------------*/

#pragma once

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

   However, the use of `strnlen`, which is strictly speaking only available in
   IEEE Std 1003.1-2008 (XPG7), requires lifting the bounds, to be able to
   compile ghc on systems that are strict about enforcing the standard, e.g.
   Apples mobile platforms.

   Oracle's Solaris 11 supports only up to XPG6, hence the ifdef.
  */

#if defined(solaris2_HOST_OS)
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE   600
#else
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE   700
#endif
