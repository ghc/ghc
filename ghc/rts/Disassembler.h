
/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.5 2000/12/19 16:48:35 sewardj Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

#ifdef GHCI

extern int  disInstr   ( StgBCO *bco, int pc );
extern void disassemble( StgBCO *bco, char* prefix );

#endif
