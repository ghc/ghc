#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include <string.h>


void expectStacksToBeEqual(StgStack *clonedStack, StgTSO *tso) {
    StgStack *liveStack = tso->stackobj;

    if(liveStack->header.info != clonedStack->header.info){
        barf("info table pointer not equal! Expected same pointer address, but got %p and %p", liveStack->header.info, clonedStack->header.info);
    }

    StgInfoTable *info = INFO_PTR_TO_STRUCT(liveStack->header.info);

    if (info->type != STACK) {
        barf("Expected a closure of type STACK!");
    }

    if(liveStack->stack_size != clonedStack->stack_size){
        barf("Expected same stack_size!");
    }

    if(liveStack->marking != clonedStack->marking){
        barf("Expected same marking flags!");
    }

    for(StgWord i = liveStack->stack_size - 1; (liveStack->stack + i) >= liveStack->sp; i--){
        if(liveStack->stack[i] != clonedStack->stack[i]){
            barf("Expected stack word %" FMT_Word " to be equal on both stacks.", i);
        }
    }
}

void expectStackToBeNotDirty(StgStack *stack) {
    if(stack->dirty != 0) {
        barf("Expected stack to be not dirty. But dirty flag was set to %" FMT_Word, (StgWord) stack->dirty);
    }
}

void expectClosureTypes(StgStack *stack, unsigned int types[], size_t typesSize){
    StgPtr sp = stack->sp;
    StgPtr spBottom = stack->stack + stack->stack_size;

    for (StgWord i = 0; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp), i++) {
        const StgInfoTable *info = get_itbl((StgClosure *)sp);

        if(i >= typesSize) {
            barf("Stack size exceeds expectation!");
        }

        if(info->type != types[i]) {
            barf("Wrong closure type on stack! Expected %" FMT_Word " but got %" FMT_Word " in position %" FMT_Word,
                 (StgWord) types[i], (StgWord) info->type, i);
        }
    }
}

// Count all (#I 1) closures of the RET_BIG closure's payload.
static int countOnes(StgPtr spBottom, StgPtr payload,
                     StgLargeBitmap *large_bitmap, uint32_t size) {
  StgWord bmp;
  uint32_t i, j;
  int ones = 0;

  i = 0;
  for (bmp = 0; i < size; bmp++) {
    StgWord bitmap = large_bitmap->bitmap[bmp];
    j = 0;
    for (; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1) {
      if ((bitmap & 1) == 0) {
        const StgClosure *closure = UNTAG_CLOSURE((StgClosure *)payload[i]);
        const StgInfoTable *info = get_itbl(closure);

        switch (info->type) {
        case CONSTR_0_1: {
          const StgConInfoTable *con_info = get_con_itbl(closure);
          if (strcmp(GET_CON_DESC(con_info), "ghc-internal:GHC.Internal.Types.I#") == 0 &&
              closure->payload[0] == (StgClosure*) 1) {
            ones++;
          }
          break;
        }
        default: {
          break;
        }
        }
      }
    }
  }

  return ones;
}

void expectSixtyFourOnesInRetBigFrame(StgStack *stack) {
  StgPtr sp = stack->sp;
  StgPtr spBottom = stack->stack + stack->stack_size;

  for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
    const StgInfoTable *info = get_itbl((StgClosure *)sp);

    if (info->type == RET_BIG) {
      StgLargeBitmap *bitmap = GET_LARGE_BITMAP(info);
      int ones = countOnes(spBottom, (StgPtr)((StgClosure *)sp)->payload,
                           bitmap, bitmap->size);

      if (ones != 64) {
        barf("Expected 64 ones, got %i!", ones);
      }
    }
  }
}
