/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Registers in the STG machine.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef REGS_H
#define REGS_H

/*
 * The STG machine has a collection of "registers", each one of which
 * may or may not correspond to an actual machine register when
 * running code.  
 *
 * The register set is backed by a table in memory (struct
 * StgRegTable).  If a particular STG register is not mapped to a
 * machine register, then the appropriate slot in this table is used
 * instead.  
 *
 * This table is itself pointed to by another register, BaseReg.  If
 * BaseReg is not in a machine register, then the register table is
 * used from an absolute location (MainCapability).
 *
 */

typedef struct {
  StgWord        stgEagerBlackholeInfo;
  StgFunPtr      stgGCEnter1;
  StgFunPtr      stgGCFun;
} StgFunTable;

/*
 * Vanilla registers are given this union type, which is purely so
 * that we can cast the vanilla reg to a variety of types with the
 * minimum of syntax.  eg.  R1.w instead of (StgWord)R1.
 */
typedef union {
    StgWord        w;
    StgAddr        a;
    StgChar        c;
    StgFloat       f;
    StgInt         i;
    StgPtr         p;
} StgUnion;

/* 
 * This is the table that holds shadow-locations for all the STG
 * registers.  The shadow locations are used when:
 *
 *     1) the particular register isn't mapped to a real machine
 *        register, probably because there's a shortage of real registers.
 *     2) caller-saves registers are saved across a CCall
 */
typedef struct {
  StgUnion 	  rR1;
  StgUnion   	  rR2;
  StgUnion   	  rR3;
  StgUnion   	  rR4;
  StgUnion   	  rR5;
  StgUnion   	  rR6;
  StgUnion   	  rR7;
  StgUnion   	  rR8;
  StgUnion   	  rR9;		/* used occasionally by heap/stack checks */
  StgUnion   	  rR10;		/* used occasionally by heap/stack checks */
  StgFloat 	  rF1;
  StgFloat 	  rF2;
  StgFloat 	  rF3;
  StgFloat 	  rF4;
  StgFloat 	  rF5;
  StgFloat 	  rF6;
  StgDouble 	  rD1;
  StgDouble 	  rD2;
  StgDouble 	  rD3;
  StgDouble 	  rD4;
  StgDouble 	  rD5;
  StgDouble 	  rD6;
  StgWord128 	  rXMM1;
  StgWord128 	  rXMM2;
  StgWord128 	  rXMM3;
  StgWord128 	  rXMM4;
  StgWord128 	  rXMM5;
  StgWord128 	  rXMM6;
  StgWord64       rL1;
  StgPtr 	  rSp;
  StgPtr 	  rSpLim;
  StgPtr 	  rHp;
  StgPtr 	  rHpLim;
  struct CostCentreStack_ * rCCCS;  /* current cost-centre-stack */
  struct StgTSO_ *     rCurrentTSO;
  struct nursery_ *    rNursery;
  struct bdescr_ *     rCurrentNursery; /* Hp/HpLim point into this block */
  struct bdescr_ *     rCurrentAlloc;   /* for allocation using allocate() */
  StgWord         rHpAlloc;	/* number of *bytes* being allocated in heap */
  StgWord         rRet;  /* holds the return code of the thread */
} StgRegTable;

#if IN_STG_CODE

/*
 * Registers Hp and HpLim are global across the entire system, and are
 * copied into the RegTable or registers before executing a thread.
 *
 * Registers Sp and SpLim are saved in the TSO for the thread, but are
 * copied into the RegTable or registers before executing a thread.
 *
 * All other registers are "general purpose", and are used for passing
 * arguments to functions, and returning values.  The code generator
 * knows how many of these are in real registers, and avoids
 * generating code that uses non-real registers.  General purpose
 * registers are never saved when returning to the scheduler, instead
 * we save whatever is live at the time on the stack, and restore it
 * later.  This should reduce the context switch time, amongst other
 * things.
 *
 * For argument passing, the stack will be used in preference to
 * pseudo-registers if the architecture has too few general purpose
 * registers.
 *
 * Some special RTS functions like newArray and the Integer primitives
 * expect their arguments to be in registers R1-Rn, so we use these
 * (pseudo-)registers in those cases.
 */

/* -----------------------------------------------------------------------------
 * Emit the GCC-specific register declarations for each machine
 * register being used.  If any STG register isn't mapped to a machine
 * register, then map it to an offset from BaseReg.
 *
 * First, the general purpose registers.  The idea is, if a particular
 * general-purpose STG register can't be mapped to a real machine
 * register, it won't be used at all.  Instead, we'll use the stack.
 */

/* define NO_REGS to omit register declarations - used in RTS C code
 * that needs all the STG definitions but not the global register 
 * settings.
 */
#define GLOBAL_REG_DECL(type,name,reg) register type name REG(reg);

#if defined(REG_R1) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R1,REG_R1)
#else
# define R1 (BaseReg->rR1)
#endif

#if defined(REG_R2) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R2,REG_R2)
#else
# define R2 (BaseReg->rR2)
#endif

#if defined(REG_R3) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R3,REG_R3)
#else
# define R3 (BaseReg->rR3)
#endif

#if defined(REG_R4) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R4,REG_R4)
#else
# define R4 (BaseReg->rR4)
#endif

#if defined(REG_R5) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R5,REG_R5)
#else
# define R5 (BaseReg->rR5)
#endif

#if defined(REG_R6) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R6,REG_R6)
#else
# define R6 (BaseReg->rR6)
#endif

#if defined(REG_R7) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R7,REG_R7)
#else
# define R7 (BaseReg->rR7)
#endif

#if defined(REG_R8) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R8,REG_R8)
#else
# define R8 (BaseReg->rR8)
#endif

#if defined(REG_R9) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R9,REG_R9)
#else
# define R9 (BaseReg->rR9)
#endif

#if defined(REG_R10) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgUnion,R10,REG_R10)
#else
# define R10 (BaseReg->rR10)
#endif

#if defined(REG_F1) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F1,REG_F1)
#else
#define F1 (BaseReg->rF1)
#endif

#if defined(REG_F2) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F2,REG_F2)
#else
#define F2 (BaseReg->rF2)
#endif

#if defined(REG_F3) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F3,REG_F3)
#else
#define F3 (BaseReg->rF3)
#endif

#if defined(REG_F4) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F4,REG_F4)
#else
#define F4 (BaseReg->rF4)
#endif

#if defined(REG_F5) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F5,REG_F5)
#else
#define F5 (BaseReg->rF5)
#endif

#if defined(REG_F6) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgFloat,F6,REG_F6)
#else
#define F6 (BaseReg->rF6)
#endif

#if defined(REG_D1) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D1,REG_D1)
#else
#define D1 (BaseReg->rD1)
#endif

#if defined(REG_D2) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D2,REG_D2)
#else
#define D2 (BaseReg->rD2)
#endif

#if defined(REG_D3) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D3,REG_D3)
#else
#define D3 (BaseReg->rD3)
#endif

#if defined(REG_D4) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D4,REG_D4)
#else
#define D4 (BaseReg->rD4)
#endif

#if defined(REG_D5) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D5,REG_D5)
#else
#define D5 (BaseReg->rD5)
#endif

#if defined(REG_D6) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgDouble,D6,REG_D6)
#else
#define D6 (BaseReg->rD6)
#endif

#if defined(REG_XMM1) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM1,REG_XMM1)
#else
#define XMM1 (BaseReg->rXMM1)
#endif

#if defined(REG_XMM2) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM2,REG_XMM2)
#else
#define XMM2 (BaseReg->rXMM2)
#endif

#if defined(REG_XMM3) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM3,REG_XMM3)
#else
#define XMM3 (BaseReg->rXMM3)
#endif

#if defined(REG_XMM4) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM4,REG_XMM4)
#else
#define XMM4 (BaseReg->rXMM4)
#endif

#if defined(REG_XMM5) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM5,REG_XMM5)
#else
#define XMM5 (BaseReg->rXMM5)
#endif

#if defined(REG_XMM6) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord128,XMM6,REG_XMM6)
#else
#define XMM6 (BaseReg->rXMM6)
#endif

#if defined(REG_L1) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgWord64,L1,REG_L1)
#else
#define L1 (BaseReg->rL1)
#endif

/*
 * If BaseReg isn't mapped to a machine register, just use the global
 * address of the current register table (CurrentRegTable in
 * concurrent Haskell, MainRegTable otherwise).
 */

/* A capability is a combination of a FunTable and a RegTable.  In STG
 * code, BaseReg normally points to the RegTable portion of this
 * structure, so that we can index both forwards and backwards to take
 * advantage of shorter instruction forms on some archs (eg. x86).
 * This is a cut-down version of the Capability structure; the full
 * version is defined in Capability.h.
 */
struct PartCapability_ {
    StgFunTable f;
    StgRegTable r;
};

/* No such thing as a MainCapability under THREADED_RTS - each thread must have
 * its own Capability.
 */
#if IN_STG_CODE && !(defined(THREADED_RTS) && !defined(NOSMP))
extern W_ MainCapability[];
#endif

/*
 * Assigning to BaseReg (the ASSIGN_BaseReg macro): this happens on
 * return from a "safe" foreign call, when the thread might be running
 * on a new Capability.  Obviously if BaseReg is not a register, then
 * we are restricted to a single Capability (this invariant is enforced
 * in Capability.c:initCapabilities), and assigning to BaseReg can be omitted.
 */

#if defined(REG_Base) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(StgRegTable *,BaseReg,REG_Base)
#define ASSIGN_BaseReg(e) (BaseReg = (e))
#else
#if defined(THREADED_RTS) && !defined(NOSMP)
#error BaseReg must be in a register for THREADED_RTS
#endif
#define BaseReg (&((struct PartCapability_ *)MainCapability)->r)
#define ASSIGN_BaseReg(e) (e)
#endif

#if defined(REG_Sp) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(P_,Sp,REG_Sp)
#else
#define Sp (BaseReg->rSp)
#endif

#if defined(REG_SpLim) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(P_,SpLim,REG_SpLim)
#else
#define SpLim (BaseReg->rSpLim)
#endif

#if defined(REG_Hp) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(P_,Hp,REG_Hp)
#else
#define Hp (BaseReg->rHp)
#endif

#if defined(REG_HpLim) && !defined(NO_GLOBAL_REG_DECLS)
#error HpLim cannot be in a register
#else
#define HpLim (BaseReg->rHpLim)
#endif

#if defined(REG_CCCS) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(struct CostCentreStack_ *,CCCS,REG_CCCS)
#else
#define CCCS (BaseReg->rCCCS)
#endif

#if defined(REG_CurrentTSO) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(struct _StgTSO *,CurrentTSO,REG_CurrentTSO)
#else
#define CurrentTSO (BaseReg->rCurrentTSO)
#endif

#if defined(REG_CurrentNursery) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,CurrentNursery,REG_CurrentNursery)
#else
#define CurrentNursery (BaseReg->rCurrentNursery)
#endif

#if defined(REG_HpAlloc) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,HpAlloc,REG_HpAlloc)
#else
#define HpAlloc (BaseReg->rHpAlloc)
#endif

/* -----------------------------------------------------------------------------
   Get absolute function pointers from the register table, to save
   code space.  On x86, 

       jmp  *-12(%ebx)

   is shorter than
   
       jmp absolute_address

   as long as the offset is within the range of a signed byte
   (-128..+127).  So we pick some common absolute_addresses and put
   them in the register table.  As a bonus, linking time should also
   be reduced.

   Other possible candidates in order of importance:
      
     stg_upd_frame_info
     stg_CAF_BLACKHOLE_info
     stg_IND_STATIC_info

   anything else probably isn't worth the effort.

   -------------------------------------------------------------------------- */


#define FunReg ((StgFunTable *)((void *)BaseReg - STG_FIELD_OFFSET(struct PartCapability_, r)))

#define stg_EAGER_BLACKHOLE_info  (FunReg->stgEagerBlackholeInfo)
#define stg_gc_enter_1            (FunReg->stgGCEnter1)
#define stg_gc_fun                (FunReg->stgGCFun)

#endif /* IN_STG_CODE */

#endif /* REGS_H */
