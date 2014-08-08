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
void setupRtsFlags        (int *argc, char *argv[],
                           RtsOptsEnabledEnum rtsOptsEnabled,
                           const char *ghc_rts_opts,
                           HsBool is_hs_main);
void setProgName          (char *argv[]);
void freeRtsArgs          (void);

#include "EndPrivate.h"

#endif /* RTSFLAGS_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
