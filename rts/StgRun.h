/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 * 
 ---------------------------------------------------------------------------- */

#ifndef STGRUN_H
#define STGRUN_H

RTS_PRIVATE StgRegTable * StgRun (StgFunPtr f, StgRegTable *basereg);

#endif /* STGRUN_H */
