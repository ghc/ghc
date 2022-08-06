/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2006
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void    labelThread          (Capability  *cap,
                              StgTSO      *tso,
                              StgArrBytes *label);
void    setThreadLabel       (Capability  *cap,
                              StgTSO      *tso,
                              char        *label);

#include "EndPrivate.h"
