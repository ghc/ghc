/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.4 2000/12/11 12:53:44 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

extern int  disInstr   ( StgBCO *bco, int pc );
extern void disassemble( StgBCO *bco, char* prefix );
