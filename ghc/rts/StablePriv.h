/* -----------------------------------------------------------------------------
 * $Id: StablePriv.h,v 1.1 1999/01/26 14:18:38 simonm Exp $
 *
 * Internal RTS API for stable names and stable ptrs.
 *
 * ---------------------------------------------------------------------------*/

extern void initStablePtrTable(void);
extern void markStablePtrTable(rtsBool full);
extern void enlargeStablePtrTable(void);
extern void gcStablePtrTable(rtsBool full);
extern StgWord lookupStableName(StgPtr p);
