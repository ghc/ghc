
/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.6 2000/12/20 14:47:22 sewardj Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

#ifdef GHCI

extern int  disInstr   ( StgBCO *bco, int pc );
extern void disassemble( StgBCO *bco );

#endif
