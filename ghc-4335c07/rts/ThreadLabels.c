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
updateThreadLabel(StgWord key, void *data)
{
  removeThreadLabel(key);

  ACQUIRE_LOCK(&threadLabels_mutex);

  insertHashTable(threadLabels,key,data);

  RELEASE_LOCK(&threadLabels_mutex);
}

void *
lookupThreadLabel(StgWord key)
{
  void * result;
  ACQUIRE_LOCK(&threadLabels_mutex);

  result = lookupHashTable(threadLabels,key);

  RELEASE_LOCK(&threadLabels_mutex);

  return result;
}

void
removeThreadLabel(StgWord key)
{
  ACQUIRE_LOCK(&threadLabels_mutex);

  void * old = NULL;
  if ((old = lookupHashTable(threadLabels,key))) {
    removeHashTable(threadLabels,key,old);
    stgFree(old);
  }

  RELEASE_LOCK(&threadLabels_mutex);
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
