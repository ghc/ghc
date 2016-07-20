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

#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE   700

#endif /* POSIXSOURCE_H */
