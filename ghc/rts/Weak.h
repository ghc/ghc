/* -----------------------------------------------------------------------------
 * $Id: Weak.h,v 1.2 1998/12/02 13:29:01 simonm Exp $
 *
 * Weak pointers / finalisers
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *weak_ptr_list;

void finaliseWeakPointersNow(void);
void scheduleFinalisers(StgWeak *w);
void markWeakList(void);


