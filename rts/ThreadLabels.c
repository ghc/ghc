/* -----------------------------------------------------------------------------
 * ThreadLabels.c
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "ThreadLabels.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "Trace.h"

#include <stdlib.h>
#include <string.h>

#if defined(DEBUG)

static HashTable * threadLabels = NULL;

void
initThreadLabelTable(void)
{
  if (threadLabels == NULL) {
    threadLabels = allocHashTable();
  }
}

void
freeThreadLabelTable(void)
{
    if (threadLabels != NULL) {
        freeHashTable(threadLabels, stgFree);
        threadLabels = NULL;
    }
}

static void
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

void
labelThread(Capability *cap   STG_UNUSED,
            StgTSO     *tso   STG_UNUSED,
            char       *label STG_UNUSED)
{
#if defined(DEBUG)
  int len;
  void *buf;

  /* Caveat: Once set, you can only set the thread name to "" */
  len = strlen(label)+1;
  buf = stgMallocBytes(len * sizeof(char), "Schedule.c:labelThread()");
  strncpy(buf,label,len);
  /* Update will free the old memory for us */
  updateThreadLabel(tso->id,buf);
#endif
  traceThreadLabel(cap, tso, label);
}
