/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Hash.h"

extern HashTable * threadLabels;

void
initThreadLabelTable(void);

void
updateThreadLabel(StgWord key, void *data);

void *
lookupThreadLabel(StgWord key);

void
removeThreadLabel(StgWord key);
