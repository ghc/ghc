/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#ifndef WEAK_H
#define WEAK_H

void finalizeWeakPointersNow(void);
void scheduleFinalizers(StgWeak *w);
void markWeakList(void);

#endif
