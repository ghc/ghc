/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#ifndef WEAK_H
#define WEAK_H

void scheduleFinalizers(StgWeak *w);
void markWeakList(void);

#endif
