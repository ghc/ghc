/* -----------------------------------------------------------------------------
 * $Id: StablePtr.h,v 1.2 1998/12/02 13:28:49 simonm Exp $
 *
 * Stable pointers
 *
 * ---------------------------------------------------------------------------*/

extern StgPtr *stable_ptr_table;
extern StgPtr *stable_ptr_free;

extern void initStablePtrTable(void);
extern void markStablePtrTable(void);
extern void enlargeStablePtrTable(void);

static inline StgPtr
deRefStablePointer(StgInt stable_ptr)
{
  return stable_ptr_table[stable_ptr];
}

extern void   performIO(StgStablePtr stableIndex);
