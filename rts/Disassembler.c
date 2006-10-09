/* -----------------------------------------------------------------------------
 * Bytecode disassembler
 *
 * Copyright (c) 1994-2002.
 *
 * $RCSfile: Disassembler.c,v $
 * $Revision: 1.29 $
 * $Date: 2004/09/03 15:28:19 $
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "Closures.h"
#include "TSO.h"
#include "Schedule.h"

#include "Bytecodes.h"
#include "Printer.h"
#include "Disassembler.h"
#include "Interpreter.h"

/* --------------------------------------------------------------------------
 * Disassembler
 * ------------------------------------------------------------------------*/

int
disInstr ( StgBCO *bco, int pc )
{
   int i;
   StgWord16 instr;

   StgWord16*     instrs      = (StgWord16*)(bco->instrs->payload);

   StgArrWords*   literal_arr = bco->literals;
   StgWord*       literals    = (StgWord*)(&literal_arr->payload[0]);

   StgMutArrPtrs* ptrs_arr    = bco->ptrs;
   StgPtr*        ptrs        = (StgPtr*)(&ptrs_arr->payload[0]);

   StgArrWords*   itbls_arr   = bco->itbls;
   StgInfoTable** itbls       = (StgInfoTable**)(&itbls_arr->payload[0]);

   instr = instrs[pc++];
   switch (instr) {
      case bci_SWIZZLE:
         debugBelch("SWIZZLE stkoff %d by %d\n",
                         instrs[pc], (signed int)instrs[pc+1]);
         pc += 2; break;
      case bci_CCALL:
         debugBelch("CCALL    marshaller at 0x%lx\n", 
                         literals[instrs[pc]] );
         pc += 1; break;
      case bci_STKCHECK: 
         debugBelch("STKCHECK %d\n", instrs[pc] );
         pc += 1; break;
      case bci_PUSH_L: 
         debugBelch("PUSH_L   %d\n", instrs[pc] );
         pc += 1; break;
      case bci_PUSH_LL:
         debugBelch("PUSH_LL  %d %d\n", instrs[pc], instrs[pc+1] ); 
         pc += 2; break;
      case bci_PUSH_LLL:
         debugBelch("PUSH_LLL %d %d %d\n", instrs[pc], instrs[pc+1], 
                                                            instrs[pc+2] ); 
         pc += 3; break;
      case bci_PUSH_G:
         debugBelch("PUSH_G   " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n" );
         pc += 1; break;

      case bci_PUSH_ALTS:
         debugBelch("PUSH_ALTS  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_P:
         debugBelch("PUSH_ALTS_P  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_N:
         debugBelch("PUSH_ALTS_N  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_F:
         debugBelch("PUSH_ALTS_F  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_D:
         debugBelch("PUSH_ALTS_D  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_L:
         debugBelch("PUSH_ALTS_L  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;
      case bci_PUSH_ALTS_V:
         debugBelch("PUSH_ALTS_V  " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n");
         pc += 1; break;

      case bci_PUSH_UBX:
         debugBelch("PUSH_UBX ");
         for (i = 0; i < instrs[pc+1]; i++) 
            debugBelch("0x%lx ", literals[i + instrs[pc]] );
         debugBelch("\n");
         pc += 2; break;
      case bci_PUSH_APPLY_N:
	  debugBelch("PUSH_APPLY_N\n");
	  break;
      case bci_PUSH_APPLY_V:
	  debugBelch("PUSH_APPLY_V\n");
	  break;
      case bci_PUSH_APPLY_F:
	  debugBelch("PUSH_APPLY_F\n");
	  break;
      case bci_PUSH_APPLY_D:
	  debugBelch("PUSH_APPLY_D\n");
	  break;
      case bci_PUSH_APPLY_L:
	  debugBelch("PUSH_APPLY_L\n");
	  break;
      case bci_PUSH_APPLY_P:
	  debugBelch("PUSH_APPLY_P\n");
	  break;
      case bci_PUSH_APPLY_PP:
	  debugBelch("PUSH_APPLY_PP\n");
	  break;
      case bci_PUSH_APPLY_PPP:
	  debugBelch("PUSH_APPLY_PPP\n");
	  break;
      case bci_PUSH_APPLY_PPPP:
	  debugBelch("PUSH_APPLY_PPPP\n");
	  break;
      case bci_PUSH_APPLY_PPPPP:
	  debugBelch("PUSH_APPLY_PPPPP\n");
	  break;
      case bci_PUSH_APPLY_PPPPPP:
	  debugBelch("PUSH_APPLY_PPPPPP\n");
	  break;
      case bci_SLIDE: 
         debugBelch("SLIDE     %d down by %d\n", instrs[pc], instrs[pc+1] );
         pc += 2; break;
      case bci_ALLOC_AP:
         debugBelch("ALLOC_AP  %d words\n", instrs[pc] );
         pc += 1; break;
      case bci_ALLOC_PAP:
         debugBelch("ALLOC_PAP %d arity, %d words\n",
		 instrs[pc], instrs[pc+1] );
         pc += 2; break;
      case bci_MKAP:
         debugBelch("MKAP      %d words, %d stkoff\n", instrs[pc+1], 
                                                           instrs[pc] );
         pc += 2; break;
      case bci_MKPAP:
         debugBelch("MKPAP     %d words, %d stkoff\n", instrs[pc+1], 
                                                           instrs[pc] );
         pc += 2; break;
      case bci_UNPACK:
         debugBelch("UNPACK    %d\n", instrs[pc] );
         pc += 1; break;
      case bci_PACK:
         debugBelch("PACK      %d words with itbl ", instrs[pc+1] );
         printPtr( (StgPtr)itbls[instrs[pc]] );
         debugBelch("\n");
         pc += 2; break;

      case bci_TESTLT_I:
         debugBelch("TESTLT_I  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_I:
         debugBelch("TESTEQ_I  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_F:
         debugBelch("TESTLT_F  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_F:
         debugBelch("TESTEQ_F  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_D:
         debugBelch("TESTLT_D  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_D:
         debugBelch("TESTEQ_D  %ld, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_P:
         debugBelch("TESTLT_P  %d, fail to %d\n", instrs[pc],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_P:
         debugBelch("TESTEQ_P  %d, fail to %d\n", instrs[pc],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_CASEFAIL: 
         debugBelch("CASEFAIL\n" );
         break;
      case bci_JMP:
         debugBelch("JMP to    %d\n", instrs[pc]);
         pc += 1; break;

      case bci_ENTER:
         debugBelch("ENTER\n");
         break;

      case bci_RETURN:
         debugBelch("RETURN\n" );
	 break;
      case bci_RETURN_P:
         debugBelch("RETURN_P\n" );
	 break;
      case bci_RETURN_N:
         debugBelch("RETURN_N\n" );
	 break;
      case bci_RETURN_F:
         debugBelch("RETURN_F\n" );
	 break;
      case bci_RETURN_D:
         debugBelch("RETURN_D\n" );
	 break;
      case bci_RETURN_L:
         debugBelch("RETURN_L\n" );
	 break;
      case bci_RETURN_V:
         debugBelch("RETURN_V\n" );
	 break;

      default:
         barf("disInstr: unknown opcode %u", (unsigned int) instr);
   }
   return pc;
}


/* Something of a kludge .. how do we know where the end of the insn
   array is, since it isn't recorded anywhere?  Answer: the first
   short is the number of bytecodes which follow it.  
   See ByteCodeGen.linkBCO.insns_arr for construction ...  
*/
void disassemble( StgBCO *bco )
{
   nat i, j;
   StgWord16*     instrs    = (StgWord16*)(bco->instrs->payload);
   StgMutArrPtrs* ptrs      = bco->ptrs;
   nat            nbcs      = (int)instrs[0];
   nat            pc        = 1;

   debugBelch("BCO\n" );
   pc = 1;
   while (pc <= nbcs) {
      debugBelch("\t%2d:  ", pc );
      pc = disInstr ( bco, pc );
   }

   debugBelch("INSTRS:\n   " );
   j = 16;
   for (i = 0; i < nbcs; i++) {
      debugBelch("%3d ", (int)instrs[i] );
      j--; 
      if (j == 0) { j = 16; debugBelch("\n   "); };
   }
   debugBelch("\n");

   debugBelch("PTRS:\n   " );
   j = 8;
   for (i = 0; i < ptrs->ptrs; i++) {
      debugBelch("%8p ", ptrs->payload[i] );
      j--; 
      if (j == 0) { j = 8; debugBelch("\n   "); };
   }
   debugBelch("\n");

   debugBelch("\n");
   ASSERT(pc == nbcs+1);
}

#endif /* DEBUG */
