/* -----------------------------------------------------------------------------
 * $Id: Weak.h,v 1.4 1999/02/11 17:40:28 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *weak_ptr_list;

void finalizeWeakPointersNow(void);
void scheduleFinalizers(StgWeak *w);
void markWeakList(void);


