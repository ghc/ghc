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
#define SM_MARKSTACK_H

#include "BeginPrivate.h"

INLINE_HEADER void
push_mark_stack(StgPtr p)
{
    bdescr *bd;

    *mark_sp++ = (StgWord)p;

    if (((W_)mark_sp & BLOCK_MASK) == 0)
    {
        if (mark_stack_bd->u.back != NULL)
        {
            mark_stack_bd = mark_stack_bd->u.back;
        }
        else
        {
            bd = allocBlock_sync();
            bd->link = mark_stack_bd;
            bd->u.back = NULL;
            mark_stack_bd->u.back = bd; // double-link the new block on
            mark_stack_top_bd = bd;
            mark_stack_bd = bd;
        }
        mark_sp     = mark_stack_bd->start;
    }
}

INLINE_HEADER StgPtr
pop_mark_stack(void)
{
    if (((W_)mark_sp & BLOCK_MASK) == 0)
    {
        if (mark_stack_bd->link == NULL)
        {
            return NULL;
        } 
        else
        {
            mark_stack_bd = mark_stack_bd->link;
            mark_sp       = mark_stack_bd->start + BLOCK_SIZE_W;
        }
    }
    return (StgPtr)*--mark_sp;
}

INLINE_HEADER rtsBool
mark_stack_empty(void)
{
    return (((W_)mark_sp & BLOCK_MASK) == 0 && mark_stack_bd->link == NULL);
}

#include "EndPrivate.h"

#endif /* SM_MARKSTACK_H */
