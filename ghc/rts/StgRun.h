/* -----------------------------------------------------------------------------
 * $Id: StgRun.h,v 1.2 1998/12/02 13:28:54 simonm Exp $
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
