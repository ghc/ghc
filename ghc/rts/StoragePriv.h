/* -----------------------------------------------------------------------------
 * $Id: StoragePriv.h,v 1.2 1998/12/02 13:28:59 simonm Exp $
 *
 * Internal Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

extern bdescr *allocNursery (bdescr *last_bd, nat blocks);
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

