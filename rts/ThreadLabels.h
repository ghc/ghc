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

void    initThreadLabelTable (void);
void    freeThreadLabelTable (void);
void *  lookupThreadLabel    (StgWord key);
void    removeThreadLabel    (StgWord key);
void    labelThread          (Capability *cap,
                              StgTSO     *tso,
                              char       *label);

#include "EndPrivate.h"
