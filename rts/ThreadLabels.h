/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/
#ifndef __THREADLABELS_H__
#define __THREADLABELS_H__

#include "Rts.h"
#include "Hash.h"

void
initThreadLabelTable(void);

void
updateThreadLabel(StgWord key, void *data);

void *
lookupThreadLabel(StgWord key);

void
removeThreadLabel(StgWord key);

#endif /* __THREADLABELS_H__ */
