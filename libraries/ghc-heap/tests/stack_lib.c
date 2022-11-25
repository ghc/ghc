#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/Closures.h"
#include "stg/Types.h"
#include <stdlib.h>

typedef struct ClosureTypeList {
  struct ClosureTypeList *next;
  StgWord closureType;
} ClosureTypeList;

ClosureTypeList *last(ClosureTypeList *list) {
  while (list->next != NULL) {
    list = list->next;
  }
  return list;
}
ClosureTypeList *add(ClosureTypeList *list, StgWord closureType) {
  ClosureTypeList *newEntry = malloc(sizeof(ClosureTypeList));
  newEntry->next = NULL;
  newEntry->closureType = closureType;
  if (list != NULL) {
    last(list)->next = newEntry;
  } else {
    list = newEntry;
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

ClosureTypeList *concat(ClosureTypeList *begin, ClosureTypeList *end) {
  last(begin)->next = end;
  return begin;
}
void printSmallBitmap(StgPtr spBottom, StgPtr payload, StgWord bitmap,
                      uint32_t size);

ClosureTypeList *foldSmallBitmapToList(StgPtr spBottom, StgPtr payload,
                                       StgWord bitmap, uint32_t size) {
  ClosureTypeList *list = NULL;
  uint32_t i;

  for (i = 0; i < size; i++, bitmap >>= 1) {
    if ((bitmap & 1) == 0) {
      const StgClosure *c = (StgClosure *)payload[i];
      c = UNTAG_CONST_CLOSURE(c);
      StgInfoTable *info = get_itbl(c);
      list = add(list, info->type);
    }
    // TODO: Primitives are ignored here.
  }

  return list;
}

ClosureTypeList *foldLargeBitmapToList(StgPtr spBottom, StgPtr payload,
                                       StgLargeBitmap *large_bitmap,
                                       uint32_t size) {
  ClosureTypeList *list = NULL;
  StgWord bmp;
  uint32_t i, j;

  i = 0;
  for (bmp = 0; i < size; bmp++) {
    StgWord bitmap = large_bitmap->bitmap[bmp];
    j = 0;
    for (; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1) {
      if ((bitmap & 1) == 0) {
        StgClosure *c = (StgClosure *)payload[i];
        list = add(list, get_itbl(c)->type);
      }
      // TODO: Primitives are ignored here.
    }
  }
  return list;
}

// Do not traverse the whole heap. Instead add all closures that are on the
// stack itself or referenced directly by such closures.
ClosureTypeList *foldStackToList(StgStack *stack) {
  ClosureTypeList *result = NULL;
  StgPtr sp = stack->sp;
  StgPtr spBottom = stack->stack + stack->stack_size;

  for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
    const StgInfoTable *info = get_itbl((StgClosure *)sp);

    result = add(result, info->type);
    switch (info->type) {
    case UNDERFLOW_FRAME: {
      StgUnderflowFrame *f = (StgUnderflowFrame *)sp;
      result = concat(result, foldStackToList(f->next_chunk));
      continue;
    }
    case UPDATE_FRAME: {
      StgUpdateFrame *f = (StgUpdateFrame *)sp;
      result = add(result, get_itbl(f->updatee)->type);
      continue;
    }
    case CATCH_FRAME: {
      StgCatchFrame *f = (StgCatchFrame *)sp;
      result = add(result, get_itbl(UNTAG_CONST_CLOSURE(f->handler))->type);
      continue;
    }
    case STOP_FRAME: {
      continue;
    }
    case CATCH_STM_FRAME: {
      StgCatchSTMFrame *f = (StgCatchSTMFrame *)sp;
      result = add(result, get_itbl(f->code)->type);
      result = add(result, get_itbl(f->handler)->type);
      continue;
    }
    case ATOMICALLY_FRAME: {
      StgAtomicallyFrame *f = (StgAtomicallyFrame *)sp;
      result = add(result, get_itbl(f->code)->type);
      result = add(result, get_itbl(f->result)->type);
      continue;
    }
    case RET_SMALL: {
      StgWord bitmap = info->layout.bitmap;
      ClosureTypeList *bitmapList = foldSmallBitmapToList(
          spBottom, sp + 1, BITMAP_BITS(bitmap), BITMAP_SIZE(bitmap));
      result = concat(result, bitmapList);
      continue;
    }
    case RET_BCO: {
      StgWord c = *sp;
      StgBCO *bco = ((StgBCO *)sp[1]);
      ClosureTypeList *bitmapList = foldLargeBitmapToList(
          spBottom, sp + 2, BCO_BITMAP(bco), BCO_BITMAP_SIZE(bco));
      result = concat(result, bitmapList);
      continue;
    }
    case RET_BIG: {
      StgLargeBitmap *bitmap = GET_LARGE_BITMAP(info);
      ClosureTypeList *bitmapList = foldLargeBitmapToList(
          spBottom, (StgPtr)((StgClosure *)sp)->payload, bitmap, bitmap->size);
      result = concat(result, bitmapList);
      continue;
    }
    case RET_FUN: {
      StgRetFun *ret_fun = (StgRetFun *)sp;
      const StgFunInfoTable *fun_info =
          get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));

      switch (fun_info->f.fun_type) {
      case ARG_GEN:
        foldSmallBitmapToList(spBottom, sp + 2,
                              BITMAP_BITS(fun_info->f.b.bitmap),
                              BITMAP_SIZE(fun_info->f.b.bitmap));
        break;
      case ARG_GEN_BIG: {
        foldSmallBitmapToList(spBottom, sp + 2, GET_FUN_LARGE_BITMAP(fun_info),
                              GET_FUN_LARGE_BITMAP(fun_info)->size);
        break;
      }
      default: {
        foldSmallBitmapToList(spBottom, sp + 2,
                         BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
                         BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]));
        break;
      }
      }
    }
    default: {
      errorBelch("Unexpected closure type!");
      break;
    }
    }
  }

  return result;
}

StgArrBytes *createArrayClosure(ClosureTypeList *list) {
  Capability *cap = rts_lock();
  // Mapping closure types to StgWord is pretty generous as they would fit
  // in Bytes. However, the handling of StgWords is much simpler.
  StgWord neededWords = listSize(list);
  StgArrBytes *array =
      (StgArrBytes *)allocate(cap, sizeofW(StgArrBytes) + neededWords);
  SET_HDR(array, &stg_ARR_WORDS_info, CCCS);
  array->bytes = listSize(list);

  for (int i = 0; list != NULL; i++) {
    array->payload[i] = list->closureType;
    list = list->next;
  }
  rts_unlock(cap);
  return array;
}

// Traverse the stack and return an arry representation of it's closure
// types.
StgArrBytes *foldStackToArrayClosure(StgStack *stack) {
  ClosureTypeList *cl = foldStackToList(stack);
  StgArrBytes *arrayClosure = createArrayClosure(cl);
  freeList(cl);
  return arrayClosure;
}
