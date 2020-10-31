#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"


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
            barf("Expected stack word %lu to be equal on both stacks.", i);
        }
    }
}

void expectStackToBeNotDirty(StgStack *stack) {
    if(stack->dirty != 0) {
        barf("Expected stack to be not dirty. But dirty flag was set to %u", stack->dirty);
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
            barf("Wrong closure type on stack! Expected %u but got %u in position %i", types[i], info->type, i);
        }
    }
}
