/* -----------------------------------------------------------------------------
 * $Id: StoragePriv.h,v 1.14 2001/01/24 15:39:50 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Internal Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGEPRIV_H
#define STORAGEPRIV_H

#define END_OF_STATIC_LIST stgCast(StgClosure*,1)

extern generation *generations;

extern generation *g0;
extern step *g0s0;
extern generation *oldest_gen;

extern void newCAF(StgClosure*);
extern StgTSO *relocate_TSO(StgTSO *src, StgTSO *dest);

extern StgWeak    *weak_ptr_list;
extern StgClosure *caf_list;

extern bdescr *small_alloc_list;
extern bdescr *large_alloc_list;

extern StgPtr alloc_Hp;
extern StgPtr alloc_HpLim;

extern bdescr *nursery;

extern nat nursery_blocks;
extern nat alloc_blocks;
extern nat alloc_blocks_lim;

extern lnat total_allocated;

/* Nursery manipulation */
extern void     allocNurseries ( void );
extern void     resetNurseries ( void );
extern bdescr * allocNursery   ( bdescr *last_bd, nat blocks );
extern void     resizeNursery  ( nat blocks );

/* Stats 'n' stuff */
extern lnat calcAllocated  ( void );
extern lnat calcLive       ( void );
extern lnat calcNeeded     ( void );

static inline void
dbl_link_onto(bdescr *bd, bdescr **list)
{
  bd->link = *list;
  bd->back = NULL;
  if (*list) {
    (*list)->back = bd; /* double-link the list */
  }
  *list = bd;
}

/* MUTABLE LISTS
 * A mutable list is ended with END_MUT_LIST, so that we can use NULL
 * as an indication that an object is not on a mutable list.
 */
#define END_MUT_LIST ((StgMutClosure *)(void *)&stg_END_MUT_LIST_closure)

#ifdef DEBUG
extern void memInventory(void);
extern void checkSanity(nat N);
#endif

/* 
 * These three are used by the garbage collector when we have
 * dynamically-linked object modules.  (see ClosureMacros.h,
 * IS_CODE_PTR etc.). 
 * Defined in Linker.c.
 */
int is_dynamically_loaded_code_or_rodata_ptr ( void* p );
int is_dynamically_loaded_rwdata_ptr         ( void* p );
int is_not_dynamically_loaded_ptr            ( void* p );

#endif /* STORAGEPRIV_H */
