/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2021
 *
 * Support for IPE
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>
#include "Rts.h"

#include "BeginPrivate.h"

void dumpIPEToEventLog(void);
void updateIpeMap(void);
void setupMutex(void);

#if defined(TRACING)
void traceIPEFromHashTable(void *data STG_UNUSED, StgWord key STG_UNUSED, const void *value);
#endif

#include "EndPrivate.h"
