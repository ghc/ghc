
/* -----------------------------------------------------------------------------
 * Bytecode disassembler
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: Disassembler.c,v $
 * $Revision: 1.10 $
 * $Date: 1999/11/01 18:19:40 $
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef INTERPRETER

#include "RtsUtils.h"
#include "Bytecodes.h"
#include "Assembler.h"
#include "Printer.h"
#include "Disassembler.h"

/* --------------------------------------------------------------------------
 * Disassembler
 * ------------------------------------------------------------------------*/

static InstrPtr disNone         ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disInt          ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disIntInt       ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disInfo         ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disConstPtr     ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disConstInt     ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disConstChar    ( StgBCO *bco, InstrPtr pc, char* i );
static InstrPtr disConstFloat   ( StgBCO *bco, InstrPtr pc, char* i );

static InstrPtr disNone      ( StgBCO *bco, InstrPtr pc, char* i )
{
    fprintf(stderr,"%s",i);
    return pc;
}

static InstrPtr disInt       ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x = bcoInstr(bco,pc++);
    ASSERT(pc <= bco->n_instrs);
    fprintf(stderr,"%s %d",i,x);
    return pc;
}

static InstrPtr disInt16      ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x = bcoInstr16(bco,pc); pc+=2;
    ASSERT(pc <= bco->n_instrs);
    fprintf(stderr,"%s %d",i,x);
    return pc;
}

static InstrPtr disIntInt    ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x = bcoInstr(bco,pc++);
    StgInt y = bcoInstr(bco,pc++);
    fprintf(stderr,"%s %d %d",i,x,y);
    return pc;
}

static InstrPtr disIntInt16  ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x, y;
    x = bcoInstr16(bco,pc); pc += 2;
    y = bcoInstr16(bco,pc); pc += 2;
    fprintf(stderr,"%s %d %d",i,x,y);
    return pc;
}

static InstrPtr disIntPC     ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt  x;
    StgWord y;
    x = bcoInstr(bco,pc++);
    y = bcoInstr16(bco,pc); pc += 2;
    fprintf(stderr,"%s %d %d",i,x,pc+y);
    return pc;
}

static InstrPtr disPC        ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgWord y = bcoInstr16(bco,pc); pc += 2;
    fprintf(stderr,"%s %d",i,pc+y);
    return pc;
}

static InstrPtr disInfo   ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInfoTable* info = bcoConstInfoPtr(bco,bcoInstr(bco,pc++));
    /* ToDo: print contents of infotable */
    fprintf(stderr,"%s ",i);
    printPtr(stgCast(StgPtr,info));
    return pc;
}

static InstrPtr disConstPtr  ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt o = bcoInstr(bco,pc++);
    StgPtr x = bcoConstPtr(bco,o);
    fprintf(stderr,"%s [%d]=",i,o); 
    printPtr(x); /* bad way to print it... */
    return pc;
}

static InstrPtr disConstPtr16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt o; 
    StgPtr x;
    o = bcoInstr16(bco,pc); pc += 2;
    x = bcoConstPtr(bco,o);
    fprintf(stderr,"%s [%d]=",i,o); 
    printPtr(x); /* bad way to print it... */
    return pc;
}

static InstrPtr disConstInt  ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x = bcoConstInt(bco,bcoInstr(bco,pc++));
    fprintf(stderr,"%s %d (0x%x)",i,x,x);
    return pc;
}

static InstrPtr disConstInt16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgInt x = bcoConstInt(bco,bcoInstr16(bco,pc)); pc += 2;
    fprintf(stderr,"%s %d (0x%x)",i,x,x);
    return pc;
}

static InstrPtr disConstAddr ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgAddr x = bcoConstAddr(bco,bcoInstr(bco,pc++));
    fprintf(stderr,"%s ",i);
    printPtr(x);
    return pc;
}

static InstrPtr disConstAddr16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgAddr x = bcoConstAddr(bco,bcoInstr16(bco,pc)); pc += 2;
    fprintf(stderr,"%s ",i);
    printPtr(x);
    return pc;
}

static InstrPtr disConstChar ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgChar x = bcoConstChar(bco,bcoInstr(bco,pc++));
    if (isprint((int)x))
       fprintf(stderr,"%s '%c'",i,x); else
       fprintf(stderr,"%s 0x%x",i,(int)x);
    return pc;
}

static InstrPtr disConstChar16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgChar x = bcoConstChar(bco,bcoInstr16(bco,pc)); pc += 2;
    if (isprint((int)x))
       fprintf(stderr,"%s '%c'",i,x); else
       fprintf(stderr,"%s 0x%x",i,(int)x);
    return pc;
}

static InstrPtr disConstFloat ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgFloat x = bcoConstFloat(bco,bcoInstr(bco,pc++));
    fprintf(stderr,"%s %f",i,x);
    return pc;
}

static InstrPtr disConstFloat16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgFloat x = bcoConstFloat(bco,bcoInstr16(bco,pc)); pc += 2;
    fprintf(stderr,"%s %f",i,x);
    return pc;
}

static InstrPtr disConstDouble ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgDouble x = bcoConstDouble(bco,bcoInstr(bco,pc++));
    fprintf(stderr,"%s %f",i,x);
    return pc;
}

static InstrPtr disConstDouble16 ( StgBCO *bco, InstrPtr pc, char* i )
{
    StgDouble x = bcoConstDouble(bco,bcoInstr16(bco,pc)); pc += 2;
    fprintf(stderr,"%s %f",i,x);
    return pc;
}

InstrPtr disInstr( StgBCO *bco, InstrPtr pc )
{
    Instr in;
    ASSERT(pc < bco->n_instrs);
    in = bcoInstr(bco,pc++);
    switch (in) {
    case i_INTERNAL_ERROR:
            return disNone(bco,pc,"INTERNAL_ERROR");
    case i_PANIC:
            return disNone(bco,pc,"PANIC");
    case i_STK_CHECK:
            return disInt(bco,pc,"STK_CHECK");
    case i_STK_CHECK_big:
            return disInt16(bco,pc,"STK_CHECK_big");
    case i_ARG_CHECK:
            return disInt(bco,pc,"ARG_CHECK");
    case i_ALLOC_AP:
            return disInt(bco,pc,"ALLOC_AP");
    case i_ALLOC_PAP:
            return disInt(bco,pc,"ALLOC_PAP");
    case i_ALLOC_CONSTR:
            return disInfo(bco,pc,"ALLOC_CONSTR");
    case i_MKAP:
            return disIntInt(bco,pc,"MKAP");
    case i_MKAP_big:
            return disIntInt16(bco,pc,"MKAP_big");
    case i_MKPAP:
            return disIntInt(bco,pc,"MKPAP");
    case i_PACK:
            return disInt(bco,pc,"PACK");
    case i_SLIDE:
            return disIntInt(bco,pc,"SLIDE");
    case i_RV:
            return disIntInt(bco,pc,"R_V");
    case i_RVE:
            return disIntInt(bco,pc,"R_V_E");
    case i_VV:
            return disIntInt(bco,pc,"V_V");
    case i_SE:
            return disIntInt(bco,pc,"S_E");
    case i_SLIDE_big:
            return disIntInt16(bco,pc,"SLIDE_big");
    case i_ENTER:
            return disNone(bco,pc,"ENTER");
    case i_RETADDR:
            return disConstPtr(bco,pc,"RETADDR");
    case i_RETADDR_big:
            return disConstPtr16(bco,pc,"RETADDR_big");
    case i_TEST:
            return disIntPC(bco,pc,"TEST");
    case i_UNPACK:
            return disNone(bco,pc,"UNPACK");
    case i_VAR:
            return disInt(bco,pc,"VAR");
    case i_VAR_big:
            return disInt16(bco,pc,"VAR_big");
    case i_CONST:
            return disConstPtr(bco,pc,"CONST");
    case i_CONST_big:
            return disConstPtr16(bco,pc,"CONST_big");

    case i_VOID:
            return disNone(bco,pc,"VOID");

    case i_VAR_INT:
            return disInt(bco,pc,"VAR_INT");
    case i_VAR_INT_big:
            return disInt16(bco,pc,"VAR_INT_big");
    case i_CONST_INT:
            return disConstInt(bco,pc,"CONST_INT");
    case i_CONST_INT_big:
            return disConstInt16(bco,pc,"CONST_INT_big");
    case i_PACK_INT:
            return disNone(bco,pc,"PACK_INT");
    case i_UNPACK_INT:
            return disNone(bco,pc,"UNPACK_INT");
    case i_TEST_INT:
            return disPC(bco,pc,"TEST_INT");

    case i_CONST_INTEGER:
            return disConstAddr(bco,pc,"CONST_INTEGER");
    case i_CONST_INTEGER_big:
            return disConstAddr16(bco,pc,"CONST_INTEGER_big");

    case i_VAR_WORD:
            return disInt(bco,pc,"VAR_WORD");
    case i_CONST_WORD:
            return disConstInt(bco,pc,"CONST_WORD");
    case i_PACK_WORD:
            return disNone(bco,pc,"PACK_WORD");
    case i_UNPACK_WORD:
            return disNone(bco,pc,"UNPACK_WORD");

    case i_VAR_ADDR:
            return disInt(bco,pc,"VAR_ADDR");
    case i_VAR_ADDR_big:
            return disInt16(bco,pc,"VAR_ADDR_big");
    case i_CONST_ADDR:
            return disConstAddr(bco,pc,"CONST_ADDR");
    case i_CONST_ADDR_big:
            return disConstAddr16(bco,pc,"CONST_ADDR_big");
    case i_PACK_ADDR:
            return disNone(bco,pc,"PACK_ADDR");
    case i_UNPACK_ADDR:
            return disNone(bco,pc,"UNPACK_ADDR");

    case i_VAR_CHAR:
            return disInt(bco,pc,"VAR_CHAR");
    case i_VAR_CHAR_big:
            return disInt16(bco,pc,"VAR_CHAR_big");
    case i_CONST_CHAR:
            return disConstChar(bco,pc,"CONST_CHAR");
    case i_CONST_CHAR_big:
            return disConstChar16(bco,pc,"CONST_CHAR_big");
    case i_PACK_CHAR:
            return disNone(bco,pc,"PACK_CHAR");
    case i_UNPACK_CHAR:
            return disNone(bco,pc,"UNPACK_CHAR");

    case i_VAR_FLOAT:
            return disInt(bco,pc,"VAR_FLOAT");
    case i_VAR_FLOAT_big:
            return disInt16(bco,pc,"VAR_FLOAT_big");
    case i_CONST_FLOAT:
            return disConstFloat(bco,pc,"CONST_FLOAT");
    case i_CONST_FLOAT_big:
            return disConstFloat16(bco,pc,"CONST_FLOAT_big");
    case i_PACK_FLOAT:
            return disNone(bco,pc,"PACK_FLOAT");
    case i_UNPACK_FLOAT:
            return disNone(bco,pc,"UNPACK_FLOAT");

    case i_VAR_DOUBLE:
            return disInt(bco,pc,"VAR_DOUBLE");
    case i_VAR_DOUBLE_big:
            return disInt16(bco,pc,"VAR_DOUBLE_big");
    case i_CONST_DOUBLE:
            return disConstDouble(bco,pc,"CONST_DOUBLE");
    case i_CONST_DOUBLE_big:
            return disConstDouble16(bco,pc,"CONST_DOUBLE_big");
    case i_PACK_DOUBLE:
            return disNone(bco,pc,"PACK_DOUBLE");
    case i_UNPACK_DOUBLE:
            return disNone(bco,pc,"UNPACK_DOUBLE");

    case i_VAR_STABLE:
            return disInt(bco,pc,"VAR_STABLE");
    case i_PACK_STABLE:
            return disNone(bco,pc,"PACK_STABLE");
    case i_UNPACK_STABLE:
            return disNone(bco,pc,"UNPACK_STABLE");

    case i_PRIMOP1:
        {
            Primop1 op = bcoInstr(bco,pc++);
            switch (op) {
            case i_INTERNAL_ERROR1:
                    return disNone(bco,pc,"INTERNAL_ERROR1");
            case i_pushseqframe:
                    return disNone(bco,pc,"i_pushseqframe");
            case i_pushcatchframe:
                    return disNone(bco,pc,"i_pushcatchframe");
            default:
                {
                    const AsmPrim* p = asmFindPrimop(i_PRIMOP1,op);
                    if (p) {
                        return disNone(bco,pc,p->name);
                    }
                    barf("Unrecognised primop1 %d\n",op);
                }
            }
        }
    case i_PRIMOP2:
        {
            Primop2 op = bcoInstr(bco,pc++);
            switch (op) {
            case i_INTERNAL_ERROR2:
                    return disNone(bco,pc,"INTERNAL_ERROR2");
            case i_ccall_ccall_Id:
                    return disNone(bco,pc,"ccall_ccall_Id");
            case i_ccall_ccall_IO:
                    return disNone(bco,pc,"ccall_ccall_IO");
            case i_ccall_stdcall_Id:
                    return disNone(bco,pc,"ccall_stdcall_Id");
            case i_ccall_stdcall_IO:
                    return disNone(bco,pc,"ccall_stdcall_IO");
            case i_raise:
                    return disNone(bco,pc,"primRaise");
            default:
                {
                    const AsmPrim* p = asmFindPrimop(i_PRIMOP2,op);
                    if (p) {
                        return disNone(bco,pc,p->name);
                    }
                    barf("Unrecognised primop2 %d\n",op);
                }
            }
        }
    default:
            barf("Unrecognised instruction %d\n",in);
    }
}

void  disassemble( StgBCO *bco, char* prefix )
{
    int pc = 0;
    int pcLim = bco->n_instrs;
    ASSERT( get_itbl(bco)->type == BCO);
    while (pc < pcLim) {
        fprintf(stderr,"%s%d:\t",prefix,pc);
        pc = disInstr(bco,pc);
        fprintf(stderr,"\n");
    }
    if (bco->stgexpr) { 
       ppStgExpr(bco->stgexpr);
       fprintf(stderr, "\n");
    }
    else
       fprintf(stderr, "\t(no associated tree)\n" );
}

#endif /* INTERPRETER */
