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
