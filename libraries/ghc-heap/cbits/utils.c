#include <stdio.h>
#include "Rts.h"

bool isEndTsoQueue(StgTSO* tso){
    errorBelch("tso: %p", tso);
    errorBelch("END_TSO_QUEUE: %p", END_TSO_QUEUE);
    return tso == END_TSO_QUEUE;
}
