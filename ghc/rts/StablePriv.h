/* -----------------------------------------------------------------------------
 * $Id: StablePriv.h,v 1.2 1999/02/05 16:02:56 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Internal RTS API for stable names and stable ptrs.
 *
 * ---------------------------------------------------------------------------*/

extern void initStablePtrTable(void);
extern void markStablePtrTable(rtsBool full);
extern void enlargeStablePtrTable(void);
extern void gcStablePtrTable(rtsBool full);
extern StgWord lookupStableName(StgPtr p);
