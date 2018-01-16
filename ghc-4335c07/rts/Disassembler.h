/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(DEBUG)

RTS_PRIVATE int  disInstr   ( StgBCO *bco, int pc );
RTS_PRIVATE void disassemble( StgBCO *bco );

#endif
