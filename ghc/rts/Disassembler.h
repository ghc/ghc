/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.3 1999/02/05 16:02:37 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

extern InstrPtr disInstr   ( StgBCO *bco, InstrPtr pc );
extern void     disassemble( StgBCO *bco, char* prefix );
