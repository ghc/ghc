#include "Rts.h"
#include "RtsAPI.h"

// TODO: Do not merge. Only exported for debugging.
extern void printStack( StgStack *stack );

void printy(StgStack *stack) {
    printStack(stack);
}
