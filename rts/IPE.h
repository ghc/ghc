/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for IPE
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>
#include "Rts.h"

#include "BeginPrivate.h"

void dumpIPEToEventLog(void);
void traceIPEFromHashTable(void *data STG_UNUSED, StgWord key STG_UNUSED, const void *value);
void updateIpeMap(void);

#include "EndPrivate.h"
