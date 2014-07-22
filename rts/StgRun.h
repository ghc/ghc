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

#if defined(mingw32_HOST_OS)
StgWord8 *win32AllocStack(void);
#endif

#endif /* STGRUN_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
