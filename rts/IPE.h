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

#if defined(TRACING)
void traceIPEFromHashTable(void *data, StgWord key, const void *value);
#endif

#include "EndPrivate.h"
