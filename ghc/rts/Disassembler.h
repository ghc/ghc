/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.2 1998/12/02 13:28:16 simonm Exp $
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

extern InstrPtr disInstr   ( StgBCO *bco, InstrPtr pc );
extern void     disassemble( StgBCO *bco, char* prefix );
