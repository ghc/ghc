/* -----------------------------------------------------------------------------
 * $Id: StgRun.h,v 1.3 1999/02/05 16:02:59 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 * 
 ---------------------------------------------------------------------------- */

#ifndef STGRUN_H
#define STGRUN_H

#include "Storage.h"  /* for {Open,Close}Nursery functions */

extern StgThreadReturnCode StgRun(StgFunPtr f);
EXTFUN(StgReturn);

#endif STGRUN_H
