#ifndef __DIST_H
#define __DIST_H

#ifdef DIST 

#include "Rts.h"

typedef StgWord32 StgPEId;

// interface functions for Haskell Language calls
StgWord32 cGetPECount(void);
StgPEId cGetPEId(StgWord32 n);
StgPEId cGetMyPEId(void);
StgPEId cGetCertainOwner(StgClosure *mv);
void cRevalIO(StgClosure *job,StgPEId p);
StgPEId cGetHostOwner(StgByteArray h);

#endif /* DIST */

#endif /* __DIST_H */
