/* -----------------------------------------------------------------------------
 * $Id: StgRun.h,v 1.5 1999/11/03 15:01:51 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 * 
 ---------------------------------------------------------------------------- */

#ifndef STGRUN_H
#define STGRUN_H

extern StgThreadReturnCode StgRun(StgFunPtr f, StgRegTable *basereg);

EXTFUN(StgReturn);

#endif STGRUN_H
