/* -----------------------------------------------------------------------------
 * $Id: PosixSource.h,v 1.2 2002/04/23 17:16:01 ken Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * Include this file into sources which should not need any non-Posix services.
 * That includes most RTS C sources.
 * ---------------------------------------------------------------------------*/

#ifndef POSIXSOURCE_H
#define POSIXSOURCE_H

#define _POSIX_SOURCE   1
#define _POSIX_C_SOURCE 199506L
#define _ISOC9X_SOURCE

/* Let's be ISO C9X too... */

#endif
