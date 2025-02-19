/* -----------------------------------------------------------------------------
 * Bytecode disassembler
 *
 * Copyright (c) 1994-2002.
 *
 * $RCSfile: Disassembler.c,v $
 * $Revision: 1.29 $
 * $Date: 2004/09/03 15:28:19 $
 * ---------------------------------------------------------------------------*/

#if defined(DEBUG)

#include "rts/PosixSource.h"
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
   StgWord16 instr;

   StgWord16*     instrs      = (StgWord16*)(bco->instrs->payload);

   StgArrBytes*   literal_arr = bco->literals;
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
// For brevity
#define BELCH_INSTR_NAME(OP_NAME) \
   case bci_ ## OP_NAME: \
      debugBelch("OP_NAME\n"); \
      break

#define BELCH_INSTR_NAME_ALL_SIZES(OP_NAME) \
   case bci_ ## OP_NAME ## _64: \
      debugBelch("#OP_NAME" "_64\n"); \
      break; \
   case bci_ ## OP_NAME ## _32: \
      debugBelch("#OP_NAME" "_32\n"); \
      break; \
   case bci_ ## OP_NAME ## _16: \
      debugBelch("#OP_NAME" "_16\n"); \
      break; \
   case bci_ ## OP_NAME ## _08: \
      debugBelch("#OP_NAME" "_08\n"); \
      break;


   switch (instr & 0xff) {
      case bci_BRK_FUN:
         debugBelch ("BRK_FUN  " );  printPtr( ptrs[instrs[pc]] );
         debugBelch (" %d ", instrs[pc+1]); printPtr( ptrs[instrs[pc+2]] );
         CostCentre* cc = (CostCentre*)literals[instrs[pc+5]];
         if (cc) {
           debugBelch(" %s", cc->label);
         }
         debugBelch("\n");
         pc += 6;
         break;
      case bci_SWIZZLE: {
         W_     stkoff = BCO_GET_LARGE_ARG;
         StgInt by     = BCO_GET_LARGE_ARG;
         debugBelch("SWIZZLE stkoff %" FMT_Word " by %" FMT_Int "\n", stkoff, by);
         break; }
      case bci_CCALL: {
         debugBelch("CCALL    marshaller at 0x%" FMT_HexWord "\n",
                         literals[instrs[pc]] );
         pc += 1; break; }
      case bci_PRIMCALL:
         debugBelch("PRIMCALL\n");
         break;
     case bci_STKCHECK:  {
         StgWord stk_words_reqd = BCO_GET_LARGE_ARG + 1;
         debugBelch("STKCHECK %" FMT_Word "\n", (W_)stk_words_reqd );
         break;
     }
      case bci_PUSH_L: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH_L   %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH_LL: {
         W_ x1 = BCO_GET_LARGE_ARG;
         W_ x2 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH_LL  %" FMT_Word " %" FMT_Word "\n", x1, x2 );
         break; }
      case bci_PUSH_LLL: {
         W_ x1 = BCO_GET_LARGE_ARG;
         W_ x2 = BCO_GET_LARGE_ARG;
         W_ x3 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH_LLL %" FMT_Word " %" FMT_Word " %" FMT_Word "\n", x1, x2, x3);
         break; }
      case bci_PUSH8: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH8    %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH16: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH16   %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH32: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH32   %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH8_W: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH8_W  %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH16_W: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH16_W %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH32_W: {
         W_ x1 = BCO_GET_LARGE_ARG;
         debugBelch("PUSH32_W %" FMT_Word "\n", x1 );
         break; }
      case bci_PUSH_G:
         debugBelch("PUSH_G   " ); printPtr( ptrs[instrs[pc]] );
         debugBelch("\n" );
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
      case bci_PUSH_ALTS_T:
         debugBelch("PUSH_ALTS_T  ");
         printPtr( ptrs[instrs[pc]] );
         debugBelch(" 0x%" FMT_HexWord " ", literals[instrs[pc+1]] );
         printPtr( ptrs[instrs[pc+2]] );
         debugBelch("\n");
         pc += 3; break;
      case bci_PUSH_PAD8:
         debugBelch("PUSH_PAD8\n");
         pc += 1; break;
      case bci_PUSH_PAD16:
         debugBelch("PUSH_PAD16\n");
         pc += 1; break;
      case bci_PUSH_PAD32:
         debugBelch("PUSH_PAD32\n");
         pc += 1; break;
      case bci_PUSH_UBX8:
         debugBelch(
             "PUSH_UBX8 0x%" FMT_HexWord8 "\n",
             (StgWord8) literals[instrs[pc]] );
         pc += 1; break;
      case bci_PUSH_UBX16:
         debugBelch(
             "PUSH_UBX16 0x%" FMT_HexWord16 "\n",
             (StgWord16) literals[instrs[pc]] );
         pc += 1; break;
      case bci_PUSH_UBX32:
         debugBelch(
             "PUSH_UBX32 0x%" FMT_HexWord32 "\n",
             (StgWord32) literals[instrs[pc]] );
         pc += 1; break;
      case bci_PUSH_UBX: {
         debugBelch("PUSH_UBX ");
         W_ offset = BCO_GET_LARGE_ARG;
         W_ nwords = BCO_GET_LARGE_ARG;
         for (W_ i = 0; i < nwords; i++)
            debugBelch("0x%" FMT_HexWord " ", literals[i + offset] );
         debugBelch("\n");
         break; }
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
      case bci_SLIDE: {
         W_ nwords = BCO_GET_LARGE_ARG;
         W_ by     = BCO_GET_LARGE_ARG;
         debugBelch("SLIDE     %" FMT_Word " down by %" FMT_Word "\n", nwords, by );
         break; }
      case bci_ALLOC_AP: {
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("ALLOC_AP  %" FMT_Word " words\n", nwords );
         break; }
      case bci_ALLOC_AP_NOUPD: {
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("ALLOC_AP_NOUPD %" FMT_Word " words\n", nwords );
         break; }
      case bci_ALLOC_PAP: {
         W_ arity = BCO_GET_LARGE_ARG;
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("ALLOC_PAP %" FMT_Word " arity, %" FMT_Word " words\n",
                 arity, nwords );
         break; }
      case bci_MKAP: {
         W_ stkoff = BCO_GET_LARGE_ARG;
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("MKAP      %" FMT_Word " words, %" FMT_Word " stkoff\n", nwords,
                                                           stkoff );
         break; }
      case bci_MKPAP: {
         W_ stkoff = BCO_GET_LARGE_ARG;
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("MKPAP     %" FMT_Word " words, %" FMT_Word " stkoff\n", nwords,
                                                      stkoff );
         break; }
      case bci_UNPACK: {
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("UNPACK    %" FMT_Word "\n", nwords );
         break; }
      case bci_PACK: {
         int itbl = BCO_NEXT;
         W_ nwords = BCO_GET_LARGE_ARG;
         debugBelch("PACK      %" FMT_Word " words with itbl ", nwords );
         printPtr( (StgPtr)literals[itbl] );
         debugBelch("\n");
         break; }

      case bci_TESTLT_I: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I  %" FMT_Int ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_I64: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I64  %" FMT_Int64 ", fail to %d\n", *((StgInt64*)(literals+discr)), failto);
          break;
      }

      case bci_TESTLT_I32: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I32  %" FMT_Int ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_I16: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I16  %" FMT_Int ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_I8: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_I8  %" FMT_Int ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTEQ_I:
         debugBelch("TESTEQ_I  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_I64:
         debugBelch("TESTEQ_I64  %" FMT_Int64 ", fail to %d\n", *((StgInt64*)(literals+instrs[pc])),
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_I32:
         debugBelch("TESTEQ_I32  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_I16:
         debugBelch("TESTEQ_I16  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_I8:
         debugBelch("TESTEQ_I8  %" FMT_Int ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_W: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_W  %" FMT_Word ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_W64: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_W64  %" FMT_Word64 ", fail to %d\n", *((StgWord64*)(literals+discr)), failto);
          break;
      }

      case bci_TESTLT_W32: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_W32  %" FMT_Word ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_W16: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_W16  %" FMT_Word ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTLT_W8: {
          unsigned int discr  = BCO_NEXT;
          int failto = BCO_GET_LARGE_ARG;
          debugBelch("TESTLT_W8  %" FMT_Word ", fail to %d\n", literals[discr], failto);
          break;
      }

      case bci_TESTEQ_W:
         debugBelch("TESTEQ_W  %" FMT_Word ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_W64:
         debugBelch("TESTEQ_W64  %" FMT_Word64 ", fail to %d\n", *((StgWord64*)(literals+instrs[pc])),
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_W32:
         debugBelch("TESTEQ_W32  %" FMT_Word ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_W16:
         debugBelch("TESTEQ_W16  %" FMT_Word ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTEQ_W8:
         debugBelch("TESTEQ_W8  %" FMT_Word ", fail to %d\n", literals[instrs[pc]],
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_F:
         debugBelch("TESTLT_F  %f, fail to %d\n", *((StgFloat*)literals+instrs[pc]),
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_F:
         debugBelch("TESTEQ_F  %f, fail to %d\n", *((StgFloat*)literals+instrs[pc]),
                                                      instrs[pc+1]);
         pc += 2; break;

      case bci_TESTLT_D:
         debugBelch("TESTLT_D  %f, fail to %d\n", *((StgDouble*)(literals+instrs[pc])),
                                                      instrs[pc+1]);
         pc += 2; break;
      case bci_TESTEQ_D:
         debugBelch("TESTEQ_D  %f, fail to %d\n", *((StgDouble*)(literals+instrs[pc])),
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
      BELCH_INSTR_NAME(CASEFAIL);
      case bci_JMP:
         debugBelch("JMP to    %d\n", instrs[pc]);
         pc += 1; break;

      BELCH_INSTR_NAME(ENTER);
      BELCH_INSTR_NAME(RETURN_P);
      BELCH_INSTR_NAME(RETURN_N);
      BELCH_INSTR_NAME(RETURN_F);
      BELCH_INSTR_NAME(RETURN_D);
      BELCH_INSTR_NAME(RETURN_L);
      BELCH_INSTR_NAME(RETURN_V);
      BELCH_INSTR_NAME(RETURN_T);


      case bci_BCO_NAME: {
         const char *name = (const char*) literals[instrs[pc]];
         debugBelch("BCO_NAME    \"%s\"\n ", name);
         pc += 1;
         break;
      }

      BELCH_INSTR_NAME_ALL_SIZES(OP_ADD);
      BELCH_INSTR_NAME_ALL_SIZES(OP_SUB);
      BELCH_INSTR_NAME_ALL_SIZES(OP_AND);
      BELCH_INSTR_NAME_ALL_SIZES(OP_XOR);
      BELCH_INSTR_NAME_ALL_SIZES(OP_OR);
      BELCH_INSTR_NAME_ALL_SIZES(OP_NOT);
      BELCH_INSTR_NAME_ALL_SIZES(OP_NEG);
      BELCH_INSTR_NAME_ALL_SIZES(OP_MUL);
      BELCH_INSTR_NAME_ALL_SIZES(OP_SHL);
      BELCH_INSTR_NAME_ALL_SIZES(OP_ASR);
      BELCH_INSTR_NAME_ALL_SIZES(OP_LSR);

      BELCH_INSTR_NAME_ALL_SIZES(OP_NEQ);
      BELCH_INSTR_NAME_ALL_SIZES(OP_EQ);

      BELCH_INSTR_NAME_ALL_SIZES(OP_U_GT);
      BELCH_INSTR_NAME_ALL_SIZES(OP_U_LE);
      BELCH_INSTR_NAME_ALL_SIZES(OP_U_GE);
      BELCH_INSTR_NAME_ALL_SIZES(OP_U_LT);

      BELCH_INSTR_NAME_ALL_SIZES(OP_S_GT);
      BELCH_INSTR_NAME_ALL_SIZES(OP_S_LE);
      BELCH_INSTR_NAME_ALL_SIZES(OP_S_GE);
      BELCH_INSTR_NAME_ALL_SIZES(OP_S_LT);

      BELCH_INSTR_NAME_ALL_SIZES(OP_INDEX_ADDR);

      default:
         barf("disInstr: unknown opcode %u", (unsigned int) instr);
   }
   return pc;
}

void disassemble( StgBCO *bco )
{
   uint32_t i, j;
   StgWord16*     instrs  = (StgWord16*)(bco->instrs->payload);
   StgMutArrPtrs* ptrs    = bco->ptrs;
   uint32_t       nbcs    = (uint32_t)(bco->instrs->bytes / sizeof(StgWord16));
   uint32_t       pc      = 0;

   debugBelch("BCO\n" );
   while (pc < nbcs) {
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
}

#endif /* DEBUG */
