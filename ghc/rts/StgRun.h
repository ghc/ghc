/* -----------------------------------------------------------------------------
 * $Id: StgRun.h,v 1.6 2001/05/25 18:33:46 sof Exp $
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

#endif /* STGRUN_H */

