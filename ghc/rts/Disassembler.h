
/* -----------------------------------------------------------------------------
 * $Id: Disassembler.h,v 1.7 2001/02/11 17:51:07 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prototypes for functions in Disassembler.c
 *
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG

extern int  disInstr   ( StgBCO *bco, int pc );
extern void disassemble( StgBCO *bco );

#endif
