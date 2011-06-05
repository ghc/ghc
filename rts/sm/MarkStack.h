/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2009
 *
 * Operations on the mark stack
 * 
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_MARKSTACK_H
#define SM_MARKSTACk_H

#include "BeginPrivate.h"

INLINE_HEADER void
push_mark_stack(StgClosure *p)
{
    bdescr *bd;

    *gct->mark_sp++ = (StgWord)p;

    if (((W_)gct->mark_sp & BLOCK_MASK) == 0)
    {
        if (gct->mark_stack_bd->link != NULL)
        {
            gct->mark_stack_bd = gct->mark_stack_bd->link;
        }
        else
        {
            bd = allocBlock_sync();
            bd->u.back = gct->mark_stack_bd;
            bd->link = NULL;
            gct->mark_stack_bd->link = bd; // double-link the new block on
            gct->mark_stack_bd = bd;
        }
        gct->mark_sp = gct->mark_stack_bd->start;
    }
}

INLINE_HEADER StgPtr
pop_mark_stack(void)
{
    if (((W_)gct->mark_sp & BLOCK_MASK) == 0)
    {
        if (gct->mark_stack_bd->u.back == NULL)
        {
            return NULL;
        } 
        else
        {
            gct->mark_stack_bd = gct->mark_stack_bd->u.back;
            gct->mark_sp       = gct->mark_stack_bd->start + BLOCK_SIZE_W;
        }
    }
    return (StgPtr)*--gct->mark_sp;
}

INLINE_HEADER rtsBool
mark_stack_empty(void)
{
    return (((W_)gct->mark_sp & BLOCK_MASK) == 0 && gct->mark_stack_bd->u.back == NULL);
}

#include "EndPrivate.h"

#endif /* SM_MARKSTACK_H */
