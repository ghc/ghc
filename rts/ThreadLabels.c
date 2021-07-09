/* -----------------------------------------------------------------------------
 * ThreadLabels.c
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "ThreadLabels.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "Trace.h"

#include <stdlib.h>
#include <string.h>

#if defined(THREADED_RTS)
static Mutex threadLabels_mutex;
#endif /* THREADED_RTS */

static HashTable * threadLabels = NULL;

void
initThreadLabelTable(void)
{
#if defined(THREADED_RTS)
  initMutex(&threadLabels_mutex);
#endif /* THREADED_RTS */

  if (threadLabels == NULL) {
    threadLabels = allocHashTable();
  }
}

void
freeThreadLabelTable(void)
{
    ACQUIRE_LOCK(&threadLabels_mutex);

    if (threadLabels != NULL) {
        freeHashTable(threadLabels, stgFree);
        threadLabels = NULL;
    }

    RELEASE_LOCK(&threadLabels_mutex);
}

static void
updateThreadLabel(StgThreadID key, void *data)
{
  removeThreadLabel(key);

  ACQUIRE_LOCK(&threadLabels_mutex);

  insertHashTable(threadLabels,key,data);

  RELEASE_LOCK(&threadLabels_mutex);
}

void *
lookupThreadLabel(StgThreadID key)
{
  void * result;
  ACQUIRE_LOCK(&threadLabels_mutex);

  result = lookupHashTable(threadLabels,key);

  RELEASE_LOCK(&threadLabels_mutex);

  return result;
}

void
removeThreadLabel(StgThreadID key)
{
  ACQUIRE_LOCK(&threadLabels_mutex);

  void * old = NULL;
  if ((old = lookupHashTable(threadLabels,key))) {
    removeHashTable(threadLabels,key,old);
    stgFree(old);
  }

  RELEASE_LOCK(&threadLabels_mutex);
}

void
labelThread(Capability *cap   STG_UNUSED,
            StgTSO     *tso   STG_UNUSED,
            char       *label STG_UNUSED)
{
  int len;
  void *buf;

  /* Caveat: Once set, you can only set the thread name to "" */
  len = strlen(label)+1;
  buf = stgMallocBytes(len * sizeof(char), "ThreadLabels.c:labelThread()");
  strncpy(buf,label,len);

  /* Update will free the old memory for us */
  updateThreadLabel(tso->id,buf);

  traceThreadLabel(cap, tso, label);
}
