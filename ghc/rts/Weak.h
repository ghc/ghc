/* -----------------------------------------------------------------------------
 * $Id: Weak.h,v 1.3 1999/02/05 16:03:04 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalisers
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *weak_ptr_list;

void finaliseWeakPointersNow(void);
void scheduleFinalisers(StgWeak *w);
void markWeakList(void);


