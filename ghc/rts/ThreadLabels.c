
#include "PosixSource.h"
#include "ThreadLabels.h"

#include <stdlib.h>

static HashTable * threadLabels = NULL;

void
initThreadLabelTable(void)
{
  ASSERT(threadLabels == NULL);
  threadLabels = allocHashTable();
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
    free(old);
  }  
}
