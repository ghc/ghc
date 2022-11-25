#include "Rts.h"
#include "stg/Types.h"
#include <stdlib.h>

// Traverse the stack and return an arry representation of it's closure types.
StgArrBytes *foldStackToArrayClosure(StgStack *stack) {}

typedef struct ClosureTypeList_ {
  struct ClosureTypeList *next;
  StgWord closureType;
} ClosureTypeList;

// Do not traverse the whole heap. Instead add all closures that are on the
// stack itself or referenced directly by such closures.
ClosureTypeList *foldStackToList(StgStack *stack) {
  StgPtr sp = stack->sp;
  StgPtr spBottom = stack->stack + stack->stack_size;

  for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
  }
}

ClosureTypeList* create(StgWord closureType){
  ClosureTypeList *entry = malloc(sizeof(ClosureTypeList));
  entry->next=NULL;
  entry->closureType = closureType;
  return entry;
}

ClosureTypeList *add(ClosureTypeList *list, StgWord closureType) {
  ClosureTypeList *newEntry = malloc(sizeof(ClosureTypeList));
  newEntry->next = NULL;
  newEntry->closureType = closureType;
  lastEntry(list)->next = newEntry;
  return newEntry;
}

ClosureTypeList *last(ClosureTypeList *list) {
  while (list->next != NULL) {
    list = list->next;
  }
  return list;
}

void freeList(ClosureTypeList *list) {
  ClosureTypeList *tmp;
  while (list != NULL) {
    tmp = list;
    list = list->next;
    free(tmp);
  }
}

StgWord listSize(ClosureTypeList *list) {
  StgWord s = 0;
  while (list != NULL) {
    list = list->next;
    s++;
  }
  return s;
}
