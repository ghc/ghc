
/* -----------------------------------------------------------------------------
 * Bytecode disassembler
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: Disassembler.c,v $
 * $Revision: 1.22 $
 * $Date: 2001/08/02 17:01:33 $
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG

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

int disInstr ( StgBCO *bco, int pc )
{
   int i;

   StgArrWords*   instr_arr   = bco->instrs;
   UShort*        instrs      = (UShort*)(&instr_arr->payload[0]);

   StgArrWords*   literal_arr = bco->literals;
   StgWord*       literals    = (StgWord*)(&literal_arr->payload[0]);

   StgMutArrPtrs* ptrs_arr    = bco->ptrs;
   StgPtr*        ptrs        = (StgPtr*)(&ptrs_arr->payload[0]);

   StgArrWords*   itbls_arr   = bco->itbls;
   StgInfoTable** itbls       = (StgInfoTable**)(&itbls_arr->payload[0]);

   switch (instrs[pc++]) {
      case bci_CCALL:
         fprintf(stderr, "CCALL    marshaller at 0x%x\n", 
                         literals[instrs[pc+1]] );
         pc += 1; break;
      case bci_STKCHECK: 
         fprintf(stderr, "STKCHECK %d\n", instrs[pc] );
         pc += 1; break;
      case bci_ARGCHECK: 
         fprintf(stderr, "ARGCHECK %d\n", instrs[pc] );
         pc += 1; break;
      case bci_PUSH_L: 
         fprintf(stderr, "PUSH_L   %d\n", instrs[pc] );
         pc += 1; break;
      case bci_PUSH_LL:
         fprintf(stderr, "PUSH_LL  %d %d\n", instrs[pc], instrs[pc+1] ); 
         pc += 2; break;
      case bci_PUSH_LLL:
         fprintf(stderr, "PUSH_LLL %d %d %d\n", instrs[pc], instrs[pc+1], 
                                                            instrs[pc+2] ); 
         pc += 3; break;
      case bci_PUSH_G:
         fprintf(stderr, "PUSH_G   " ); printPtr( ptrs[instrs[pc]] );
         fprintf(stderr, "\n" );
         pc += 1; break;
      case bci_PUSH_AS:
         fprintf(stderr, "PUSH_AS  " ); printPtr( ptrs[instrs[pc]] );
         fprintf(stderr, " 0x%x", literals[instrs[pc+1]] );
         fprintf(stderr, "\n");
         pc += 2; break;
      case bci_PUSH_UBX:
         fprintf(stderr, "PUSH_UBX ");
         for (i = 0; i < instrs[pc+1]; i++) 
            fprintf(stderr, "0x%x ", literals[i + instrs[pc]] );
         fprintf(stderr, "\n");
         pc += 2; break;
      case bci_PUSH_TAG:
         fprintf(stderr, "PUSH_TAG %d\n", instrs[pc] );
         pc += 1; break;
      case bci_SLIDE: 
         fprintf(stderr, "SLIDE    %d down by %d\n", instrs[pc], instrs[pc+1] );
         pc += 2; break;
      case bci_ALLOC:
         fprintf(stderr, "ALLOC    %d words\n", instrs[pc] );
         pc += 1; break;
      case bci_MKAP:
         fprintf(stderr, "MKAP     %d words, %d stkoff\n", instrs[pc+1], 
                                                           instrs[pc] );
         pc += 2; break;
      case bci_UNPACK:
         fprintf(stderr, "UNPACK   %d\n", instrs[pc] );
         pc += 1; break;
      case bci_UPK_TAG:
         fprintf(stderr, "UPK_TAG  %d words, %d conoff, %d stkoff\n",
                         instrs[pc], instrs[pc+1], instrs[pc+2] );
         pc += 3; break;
      case bci_PACK:
         fprintf(stderr, "PACK     %d words with itbl ", instrs[pc+1] );
         printPtr( (StgPtr)itbls[instrs[pc]] );
         fprintf(stderr, "\n");
         pc += 2; break;

      case bci_CASEFAIL: 
         fprintf(stderr, "CASEFAIL\n" );
         break;
      case bci_JMP:
         fprintf(stderr, "JMP to   %d\n", instrs[pc]);
         pc += 1; break;

      case bci_TESTLT_I:
         fprintf(stderr, "TESTLT_I %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_I:
         fprintf(stderr, "TESTEQ_I %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_F:
         fprintf(stderr, "TESTLT_F %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_F:
         fprintf(stderr, "TESTEQ_F %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_D:
         fprintf(stderr, "TESTLT_D %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_D:
         fprintf(stderr, "TESTEQ_D %d, fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_P:
         fprintf(stderr, "TESTLT_P %d, fail to %d\n", instrs[pc],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_P:
         fprintf(stderr, "TESTEQ_P %d, fail to %d\n", instrs[pc],
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_RETURN:
         fprintf(stderr, "RETURN  " ); printPtr( (StgPtr)itbls[instrs[pc]] );
         fprintf(stderr, "\n");
         pc += 1; break;
      case bci_ENTER:
         fprintf(stderr, "ENTER\n");
         break;
      default:
         barf("disInstr: unknown opcode");
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
   StgArrWords*   instr_arr = bco->instrs;
   UShort*        instrs    = (UShort*)(&instr_arr->payload[0]);
   StgMutArrPtrs* ptrs      = bco->ptrs;
   nat            nbcs      = (int)instrs[0];
   nat            pc        = 1;

   fprintf(stderr, "BCO\n" );
   pc = 1;
   while (pc <= nbcs) {
      fprintf(stderr, "\t%2d:  ", pc );
      pc = disInstr ( bco, pc );
   }

   fprintf(stderr, "INSTRS:\n   " );
   j = 16;
   for (i = 0; i < nbcs; i++) {
      fprintf(stderr, "%3d ", (int)instrs[i] );
      j--; 
      if (j == 0) { j = 16; fprintf(stderr, "\n   "); };
   }
   fprintf(stderr, "\n");

   fprintf(stderr, "PTRS:\n   " );
   j = 8;
   for (i = 0; i < ptrs->ptrs; i++) {
      fprintf(stderr, "%8p ", ptrs->payload[i] );
      j--; 
      if (j == 0) { j = 8; fprintf(stderr, "\n   "); };
   }
   fprintf(stderr, "\n");

   fprintf(stderr, "\n");
   ASSERT(pc == nbcs+1);
}

#endif /* DEBUG */
