/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Tiny assembler 'layer' between the C and STG worlds.
 *
 ---------------------------------------------------------------------------- */

#pragma once

RTS_PRIVATE StgRegTable * StgRun (StgFunPtr f, StgRegTable *basereg);

#if defined(mingw32_HOST_OS)
StgWord8 *win32AllocStack(void);
#endif
