/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef DISASSEMBLER_H
#define DISASSEMBLER_H

#ifdef DEBUG

RTS_PRIVATE int  disInstr   ( StgBCO *bco, int pc );
RTS_PRIVATE void disassemble( StgBCO *bco );

#endif

#endif /* DISASSEMBLER_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
