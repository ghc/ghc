/* -----------------------------------------------------------------------------
 * $Id: StgRun.h,v 1.4 1999/11/02 15:06:04 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 * 
 ---------------------------------------------------------------------------- */

#ifndef STGRUN_H
#define STGRUN_H

#include "Storage.h"  /* for {Open,Close}Nursery functions */

extern StgThreadReturnCode StgRun(StgFunPtr f, StgRegTable *basereg);
EXTFUN(StgReturn);

#endif STGRUN_H
