/* -----------------------------------------------------------------------------
 * ThreadLabels.c
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "ThreadLabels.h"
#include "RtsUtils.h"

#include <stdlib.h>

#if defined(DEBUG)
/* to the end */
static HashTable * threadLabels = NULL;

void
initThreadLabelTable(void)
{
  if (threadLabels == NULL) {
    threadLabels = allocHashTable();
  }
}

void
updateThreadLabel(StgWord key, void *data)
{
  removeThreadLabel(key);
  insertHashTable(threadLabels,key,data);
}

void *
lookupThreadLabel(StgWord key)
{
  return lookupHashTable(threadLabels,key);
}

void
removeThreadLabel(StgWord key)
{
  void * old = NULL;
  if ((old = lookupHashTable(threadLabels,key))) {
    removeHashTable(threadLabels,key,old);
    stgFree(old);
  }  
}
#endif /* DEBUG */
