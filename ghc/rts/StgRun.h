/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 * 
 ---------------------------------------------------------------------------- */

#ifndef STGRUN_H
#define STGRUN_H

extern StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg);

RTS_FUN(StgReturn);

#endif /* STGRUN_H */
