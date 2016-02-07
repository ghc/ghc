/* -----------------------------------------------------------------------------
 *
 * (c) The AQUA Project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-2006
 *
 * Functions for parsing the argument list.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSFLAGS_H
#define RTSFLAGS_H

#include "BeginPrivate.h"

/* Routines that operate-on/to-do-with RTS flags: */

void initRtsFlagsDefaults (void);
void setupRtsFlags        (int *argc, char *argv[], RtsConfig rtsConfig);
void freeRtsArgs          (void);

extern RtsConfig rtsConfig;

#include "EndPrivate.h"

#endif /* RTSFLAGS_H */
