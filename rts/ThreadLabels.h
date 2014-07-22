/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2006
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#ifndef THREADLABELS_H
#define THREADLABELS_H

#include "BeginPrivate.h"

#if defined(DEBUG)
void    initThreadLabelTable (void);
void    freeThreadLabelTable (void);
void *  lookupThreadLabel    (StgWord key);
void    removeThreadLabel    (StgWord key);
#endif
void    labelThread          (Capability *cap,
                              StgTSO     *tso,
                              char       *label);

#include "EndPrivate.h"

#endif /* THREADLABELS_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
