/* -----------------------------------------------------------------------------
 * $Id: StgStorage.h,v 1.5 1999/11/02 15:05:53 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * STG Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGSTORAGE_H
#define STGSTORAGE_H

/* -----------------------------------------------------------------------------
   Allocation area for compiled code

   OpenNursery(hp,hplim)        Opens the allocation area, and sets hp
   				and hplim appropriately.

   CloseNursery(hp)		Closes the allocation area.

   PleaseStopAllocating(void)   Arranges that the next call to
   				ExtendNursery() will fail, triggering
				a return to the scheduler.  This is
				useful for asynchronous interupts etc.
   -------------------------------------------------------------------------- */

#define OpenNursery(hp,hplim)				\
  (hp    = CurrentNursery->free-1,			\
   hplim = CurrentNursery->start + BLOCK_SIZE_W - 1)
  
#define CloseNursery(hp)  (CurrentNursery->free = (P_)(hp)+1)

/* -----------------------------------------------------------------------------
   Trigger a GC from Haskell land.
   -------------------------------------------------------------------------- */

extern void performGC(void);
extern void performGCWithRoots(void (*get_roots)(void));

#endif /* STGSTORAGE_H */
