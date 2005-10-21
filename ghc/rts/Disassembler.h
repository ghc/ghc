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

extern int  disInstr   ( StgBCO *bco, int pc );
extern void disassemble( StgBCO *bco );

#endif

#endif /* DISASSEMBLER_H */
