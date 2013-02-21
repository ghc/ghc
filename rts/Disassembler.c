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
#include "rts/Bytecodes.h"

#include "RtsUtils.h"
#include "Schedule.h"
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

   instr = instrs[pc++];
   if (instr & bci_FLAG_LARGE_ARGS) {
       debugBelch ("LARGE ");
   }

#define BCO_NEXT         instrs[pc++]
#define BCO_NEXT_32      (pc += 2)
#define BCO_READ_NEXT_32 (BCO_NEXT_32, (((StgWord) instrs[pc-2]) << 16) \
                                     + ( (StgWord) instrs[pc-1]))
#define BCO_NEXT_64      (pc += 4)
#define BCO_READ_NEXT_64 (BCO_NEXT_64, (((StgWord) instrs[pc-4]) << 48) \
                                     + (((StgWord) instrs[pc-3]) << 32) \
                                     + (((StgWord) instrs[pc-2]) << 16) \
                                     + ( (StgWord) instrs[pc-1]))
#if WORD_SIZE_IN_BITS == 32
#define BCO_NEXT_WORD BCO_NEXT_32
#define BCO_READ_NEXT_WORD BCO_READ_NEXT_32
#elif WORD_SIZE_IN_BITS == 64
#define BCO_NEXT_WORD BCO_NEXT_64
#define BCO_READ_NEXT_WORD BCO_READ_NEXT_64
#else
#error Cannot cope with WORD_SIZE_IN_BITS being nether 32 nor 64
#endif
#define BCO_GET_LARGE_ARG ((instr & bci_FLAG_LARGE_ARGS) ? BCO_READ_NEXT_WORD : BCO_NEXT)

   switch (instr & 0xff) {
      case bci_BRK_FUN:
         debugBelch ("BRK_FUN  " );  printPtr( ptrs[instrs[pc]] ); 
         debugBelch (" %d ", instrs[pc+1]); printPtr( ptrs[instrs[pc+2]] ); debugBelch("\n" );
         pc += 3;
         break;
      case bci_SWIZZLE:
         debugBelch("SWIZZLE stkoff %d by %d\n",
                         instrs[pc], (signed int)instrs[pc+1]);
         pc += 2; break;
      case bci_CCALL:
         debugBelch("CCALL    marshaller at 0x%" FMT_Word "\n", 
                         literals[instrs[pc]] );
         pc += 1; break;
     case bci_STKCHECK:  {
         StgWord stk_words_reqd = BCO_GET_LARGE_ARG + 1;
         debugBelch("STKCHECK %" FMT_Word "\n", (W_)stk_words_reqd );
         break;
     }
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
            debugBelch("0x%" FMT_Word " ", literals[i + instrs[pc]] );
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
      case bci_ALLOC_AP_NOUPD:
         debugBelch("ALLOC_AP_NOUPD %d words\n", instrs[pc] );
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
         printPtr( (StgPtr)literals[instrs[pc]] );
         debugBelch("\n");
         pc += 2; break;

      case bci_TESTLT_I: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I  %" FMT_Int ", fail to %d\n", literals[discr], failto);
          break;
      }
      case bci_TESTEQ_I:
         debugBelch("TESTEQ_I  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_F:
         debugBelch("TESTLT_F  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_F:
         debugBelch("TESTEQ_F  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_D:
         debugBelch("TESTLT_D  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_D:
         debugBelch("TESTEQ_D  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
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
   nat            nbcs      = (int)(bco->instrs->bytes / sizeof(StgWord16));
   nat            pc        = 1;

   debugBelch("BCO\n" );
   pc = 0;
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
