/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Include this file into sources which should not need any non-Posix services.
 * That includes most RTS C sources.
 * ---------------------------------------------------------------------------*/

#ifndef POSIXSOURCE_H
#define POSIXSOURCE_H

#define _POSIX_SOURCE   1
#define _POSIX_C_SOURCE 199506L
#define _XOPEN_SOURCE   500
#define _ISOC99_SOURCE

/* Let's be ISO C99 too... */

#endif /* POSIXSOURCE_H */
