/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Registers in the STG machine.
 *
 * The STG machine has a collection of "registers", each one of which
 * may or may not correspond to an actual machine register when
 * running code.  
 *
 * The register set is backed by a table in memory (struct
 * StgRegTable).  If a particular STG register is not mapped to a
 * machine register, then the apprpriate slot in this table is used
 * instead.  
 *
 * This table is itself pointed to by another register, BaseReg.  If
 * BaseReg is not in a machine register, then the register table is
 * used from an absolute location (MainCapability).
 *
 * ---------------------------------------------------------------------------*/

#ifndef REGS_H
#define REGS_H

#include "gmp.h" // Needs MP_INT definition 

/*
 * Spark pools: used to store pending sparks 
 *  (THREADED_RTS & PARALLEL_HASKELL only)
 * This is a circular buffer.  Invariants:
 *    - base <= hd < lim
 *    - base <= tl < lim
 *    - if hd==tl, then the pool is empty.
 *    - if hd == tl+1, then the pool is full.
 * Adding to the pool is done by assigning to *tl++ (wrapping round as
 * necessary).  When adding to a full pool, we have the option of
 * throwing away either the oldest (hd++) or the most recent (tl--) entry.
 */
typedef struct StgSparkPool_ {
  StgClosure **base;
  StgClosure **lim;
  StgClosure **hd;
  StgClosure **tl;
} StgSparkPool;

#define ASSERT_SPARK_POOL_INVARIANTS(p)		\
  ASSERT((p)->base <= (p)->hd);			\
  ASSERT((p)->hd < (p)->lim);			\
  ASSERT((p)->base <= (p)->tl);			\
  ASSERT((p)->tl < (p)->lim);

typedef struct {
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
    StgInt8        i8;
    StgFloat       f;
    StgInt         i;
    StgPtr         p;
    StgClosurePtr  cl;
    StgStackOffset offset;	/* unused? */
    StgByteArray   b;
    StgTSOPtr      t;
} StgUnion;

/* 
 * This is the table that holds shadow-locations for all the STG
 * registers.  The shadow locations are used when:
 *
 *     1) the particular register isn't mapped to a real machine
 *        register, probably because there's a shortage of real registers.
 *     2) caller-saves registers are saved across a CCall
 */
typedef struct StgRegTable_ {
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
  StgDouble 	  rD1;
  StgDouble 	  rD2;
  StgWord64       rL1;
  StgPtr 	  rSp;
  StgPtr 	  rSpLim;
  StgPtr 	  rHp;
  StgPtr 	  rHpLim;
  struct StgTSO_ *rCurrentTSO;
  struct step_   *rNursery;
  struct bdescr_ *rCurrentNursery; /* Hp/HpLim point into this block */
  struct bdescr_ *rCurrentAlloc;   /* for allocation using allocate() */
  StgWord         rHpAlloc;	/* number of *bytes* being allocated in heap */
  // rmp_tmp1..rmp_result2 are only used in THREADED_RTS builds to
  // avoid per-thread temps in bss, but currently always incldue here
  // so we just run mkDerivedConstants once
  StgWord         rmp_tmp_w;
  MP_INT          rmp_tmp1;      
  MP_INT          rmp_tmp2;      
  MP_INT          rmp_result1;
  MP_INT          rmp_result2;
  StgWord         rRet;  // holds the return code of the thread
  StgSparkPool    rSparks;	/* per-task spark pool */
} StgRegTable;

#if IN_STG_CODE

/*
 * Registers Hp and HpLim are global across the entire system, and are
 * copied into the RegTable before executing a thread.
 *
 * Registers Sp and SpLim are saved in the TSO for the
 * thread, but are copied into the RegTable before executing a thread.
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

/* 
 * Locations for saving per-thread registers.
 */

#define SAVE_Sp    	    (CurrentTSO->sp)
#define SAVE_SpLim    	    (CurrentTSO->splim)

#define SAVE_Hp	    	    (BaseReg->rHp)
#define SAVE_HpLim	    (BaseReg->rHpLim)

#define SAVE_CurrentTSO     (BaseReg->rCurrentTSO)
#define SAVE_CurrentNursery (BaseReg->rCurrentNursery)
#define SAVE_HpAlloc        (BaseReg->rHpAlloc)
#define SAVE_SparkHd 	    (BaseReg->rSparks.hd)
#define SAVE_SparkTl        (BaseReg->rSparks.tl)
#define SAVE_SparkBase      (BaseReg->rSparks.base)
#define SAVE_SparkLim 	    (BaseReg->rSparks.lim)

/* We sometimes need to save registers across a C-call, eg. if they
 * are clobbered in the standard calling convention.  We define the
 * save locations for all registers in the register table.
 */

#define SAVE_R1             (BaseReg->rR1)
#define SAVE_R2             (BaseReg->rR2)
#define SAVE_R3             (BaseReg->rR3)
#define SAVE_R4             (BaseReg->rR4)
#define SAVE_R5             (BaseReg->rR5)
#define SAVE_R6             (BaseReg->rR6)
#define SAVE_R7             (BaseReg->rR7)
#define SAVE_R8             (BaseReg->rR8)
 
#define SAVE_F1             (BaseReg->rF1)
#define SAVE_F2             (BaseReg->rF2)
#define SAVE_F3             (BaseReg->rF3)
#define SAVE_F4             (BaseReg->rF4)

#define SAVE_D1             (BaseReg->rD1)
#define SAVE_D2             (BaseReg->rD2)

#define SAVE_L1             (BaseReg->rL1)

/* -----------------------------------------------------------------------------
 * Emit the GCC-specific register declarations for each machine
 * register being used.  If any STG register isn't mapped to a machine
 * register, then map it to an offset from BaseReg.
 *
 * First, the general purpose registers.  The idea is, if a particular
 * general-purpose STG register can't be mapped to a real machine
 * register, it won't be used at all.  Instead, we'll use the stack.
 *
 * This is an improvement on the way things used to be done, when all
 * registers were mapped to locations in the register table, and stuff
 * was being shifted from the stack to the register table and back
 * again for no good reason (on register-poor architectures).
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
GLOBAL_REG_DECL(P_,HpLim,REG_HpLim)
#else
#define HpLim (BaseReg->rHpLim)
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

#if defined(REG_SparkHd) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,SparkHd,REG_SparkHd)
#else
#define SparkHd (BaseReg->rSparks.hd)
#endif

#if defined(REG_SparkTl) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,SparkTl,REG_SparkTl)
#else
#define SparkTl (BaseReg->rSparks.tl)
#endif

#if defined(REG_SparkBase) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,SparkBase,REG_SparkBase)
#else
#define SparkBase (BaseReg->rSparks.base)
#endif

#if defined(REG_SparkLim) && !defined(NO_GLOBAL_REG_DECLS)
GLOBAL_REG_DECL(bdescr *,SparkLim,REG_SparkLim)
#else
#define SparkLim (BaseReg->rSparks.lim)
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


#define FunReg ((StgFunTable *)((void *)BaseReg - sizeof(StgFunTable)))

#define stg_gc_enter_1     (FunReg->stgGCEnter1)
#define stg_gc_fun         (FunReg->stgGCFun)

/* -----------------------------------------------------------------------------
   For any registers which are denoted "caller-saves" by the C calling
   convention, we have to emit code to save and restore them across C
   calls.
   -------------------------------------------------------------------------- */

#ifdef CALLER_SAVES_R1
#define CALLER_SAVE_R1    	SAVE_R1 = R1;
#define CALLER_RESTORE_R1 	R1 = SAVE_R1;
#else
#define CALLER_SAVE_R1      	/* nothing */
#define CALLER_RESTORE_R1    	/* nothing */
#endif

#ifdef CALLER_SAVES_R2
#define CALLER_SAVE_R2    	SAVE_R2 = R2;
#define CALLER_RESTORE_R2 	R2 = SAVE_R2;
#else
#define CALLER_SAVE_R2      	/* nothing */
#define CALLER_RESTORE_R2    	/* nothing */
#endif

#ifdef CALLER_SAVES_R3
#define CALLER_SAVE_R3    	SAVE_R3 = R3;
#define CALLER_RESTORE_R3 	R3 = SAVE_R3;
#else
#define CALLER_SAVE_R3      	/* nothing */
#define CALLER_RESTORE_R3    	/* nothing */
#endif

#ifdef CALLER_SAVES_R4
#define CALLER_SAVE_R4    	SAVE_R4 = R4;
#define CALLER_RESTORE_R4 	R4 = SAVE_R4;
#else
#define CALLER_SAVE_R4      	/* nothing */
#define CALLER_RESTORE_R4    	/* nothing */
#endif

#ifdef CALLER_SAVES_R5
#define CALLER_SAVE_R5    	SAVE_R5 = R5;
#define CALLER_RESTORE_R5 	R5 = SAVE_R5;
#else
#define CALLER_SAVE_R5      	/* nothing */
#define CALLER_RESTORE_R5    	/* nothing */
#endif

#ifdef CALLER_SAVES_R6
#define CALLER_SAVE_R6    	SAVE_R6 = R6;
#define CALLER_RESTORE_R6 	R6 = SAVE_R6;
#else
#define CALLER_SAVE_R6      	/* nothing */
#define CALLER_RESTORE_R6    	/* nothing */
#endif

#ifdef CALLER_SAVES_R7
#define CALLER_SAVE_R7    	SAVE_R7 = R7;
#define CALLER_RESTORE_R7 	R7 = SAVE_R7;
#else
#define CALLER_SAVE_R7      	/* nothing */
#define CALLER_RESTORE_R7    	/* nothing */
#endif

#ifdef CALLER_SAVES_R8
#define CALLER_SAVE_R8    	SAVE_R8 = R8;
#define CALLER_RESTORE_R8 	R8 = SAVE_R8;
#else
#define CALLER_SAVE_R8      	/* nothing */
#define CALLER_RESTORE_R8    	/* nothing */
#endif

#ifdef CALLER_SAVES_R9
#define CALLER_SAVE_R9    	SAVE_R9 = R9;
#define CALLER_RESTORE_R9 	R9 = SAVE_R9;
#else
#define CALLER_SAVE_R9      	/* nothing */
#define CALLER_RESTORE_R9    	/* nothing */
#endif

#ifdef CALLER_SAVES_R10
#define CALLER_SAVE_R10    	SAVE_R10 = R10;
#define CALLER_RESTORE_R10 	R10 = SAVE_R10;
#else
#define CALLER_SAVE_R10      	/* nothing */
#define CALLER_RESTORE_R10    	/* nothing */
#endif

#ifdef CALLER_SAVES_F1
#define CALLER_SAVE_F1    	SAVE_F1 = F1;
#define CALLER_RESTORE_F1 	F1 = SAVE_F1;
#else
#define CALLER_SAVE_F1    	/* nothing */
#define CALLER_RESTORE_F1 	/* nothing */
#endif

#ifdef CALLER_SAVES_F2
#define CALLER_SAVE_F2    	SAVE_F2 = F2;
#define CALLER_RESTORE_F2 	F2 = SAVE_F2;
#else
#define CALLER_SAVE_F2    	/* nothing */
#define CALLER_RESTORE_F2 	/* nothing */
#endif

#ifdef CALLER_SAVES_F3
#define CALLER_SAVE_F3    	SAVE_F3 = F3;
#define CALLER_RESTORE_F3 	F3 = SAVE_F3;
#else
#define CALLER_SAVE_F3    	/* nothing */
#define CALLER_RESTORE_F3 	/* nothing */
#endif

#ifdef CALLER_SAVES_F4
#define CALLER_SAVE_F4    	SAVE_F4 = F4;
#define CALLER_RESTORE_F4 	F4 = SAVE_F4;
#else
#define CALLER_SAVE_F4    	/* nothing */
#define CALLER_RESTORE_F4 	/* nothing */
#endif

#ifdef CALLER_SAVES_D1
#define CALLER_SAVE_D1    	SAVE_D1 = D1;
#define CALLER_RESTORE_D1 	D1 = SAVE_D1;
#else
#define CALLER_SAVE_D1    	/* nothing */
#define CALLER_RESTORE_D1 	/* nothing */
#endif

#ifdef CALLER_SAVES_D2
#define CALLER_SAVE_D2    	SAVE_D2 = D2;
#define CALLER_RESTORE_D2 	D2 = SAVE_D2;
#else
#define CALLER_SAVE_D2    	/* nothing */
#define CALLER_RESTORE_D2 	/* nothing */
#endif

#ifdef CALLER_SAVES_L1
#define CALLER_SAVE_L1    	SAVE_L1 = L1;
#define CALLER_RESTORE_L1 	L1 = SAVE_L1;
#else
#define CALLER_SAVE_L1    	/* nothing */
#define CALLER_RESTORE_L1 	/* nothing */
#endif

#ifdef CALLER_SAVES_Sp
#define CALLER_SAVE_Sp	    	SAVE_Sp = Sp;
#define CALLER_RESTORE_Sp  	Sp = SAVE_Sp;
#else
#define CALLER_SAVE_Sp	    	/* nothing */
#define CALLER_RESTORE_Sp	/* nothing */
#endif

#ifdef CALLER_SAVES_SpLim
#define CALLER_SAVE_SpLim    	SAVE_SpLim = SpLim;
#define CALLER_RESTORE_SpLim  	SpLim = SAVE_SpLim;
#else
#define CALLER_SAVE_SpLim    	/* nothing */
#define CALLER_RESTORE_SpLim	/* nothing */
#endif

#ifdef CALLER_SAVES_Hp
#define CALLER_SAVE_Hp	    	SAVE_Hp = Hp;
#define CALLER_RESTORE_Hp   	Hp = SAVE_Hp;
#else
#define CALLER_SAVE_Hp	    	/* nothing */
#define CALLER_RESTORE_Hp	/* nothing */
#endif

#ifdef CALLER_SAVES_HpLim
#define CALLER_SAVE_HpLim   	SAVE_HpLim = HpLim;
#define CALLER_RESTORE_HpLim	HpLim = SAVE_HpLim;
#else
#define CALLER_SAVE_HpLim   	/* nothing */
#define CALLER_RESTORE_HpLim   	/* nothing */
#endif

#ifdef CALLER_SAVES_Base
#ifdef THREADED_RTS
#error "Can't have caller-saved BaseReg with THREADED_RTS"
#endif
#define CALLER_SAVE_Base	/* nothing */
#define CALLER_RESTORE_Base	BaseReg = &MainRegTable;
#else
#define CALLER_SAVE_Base	/* nothing */
#define CALLER_RESTORE_Base	/* nothing */
#endif

#ifdef CALLER_SAVES_CurrentTSO
#define CALLER_SAVE_CurrentTSO   	SAVE_CurrentTSO = CurrentTSO;
#define CALLER_RESTORE_CurrentTSO	CurrentTSO = SAVE_CurrentTSO;
#else
#define CALLER_SAVE_CurrentTSO   	/* nothing */
#define CALLER_RESTORE_CurrentTSO   	/* nothing */
#endif

#ifdef CALLER_SAVES_CurrentNursery
#define CALLER_SAVE_CurrentNursery   	SAVE_CurrentNursery = CurrentNursery;
#define CALLER_RESTORE_CurrentNursery	CurrentNursery = SAVE_CurrentNursery;
#else
#define CALLER_SAVE_CurrentNursery   	/* nothing */
#define CALLER_RESTORE_CurrentNursery   /* nothing */
#endif

#ifdef CALLER_SAVES_HpAlloc
#define CALLER_SAVE_HpAlloc   		SAVE_HpAlloc = HpAlloc;
#define CALLER_RESTORE_HpAlloc		HpAlloc = SAVE_HpAlloc;
#else
#define CALLER_SAVE_HpAlloc   		/* nothing */
#define CALLER_RESTORE_HpAlloc   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkHd
#define CALLER_SAVE_SparkHd   		SAVE_SparkHd = SparkHd;
#define CALLER_RESTORE_SparkHd		SparkHd = SAVE_SparkHd;
#else
#define CALLER_SAVE_SparkHd   		/* nothing */
#define CALLER_RESTORE_SparkHd   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkTl
#define CALLER_SAVE_SparkTl   		SAVE_SparkTl = SparkTl;
#define CALLER_RESTORE_SparkTl		SparkTl = SAVE_SparkTl;
#else
#define CALLER_SAVE_SparkTl   		/* nothing */
#define CALLER_RESTORE_SparkTl   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkBase
#define CALLER_SAVE_SparkBase   	SAVE_SparkBase = SparkBase;
#define CALLER_RESTORE_SparkBase	SparkBase = SAVE_SparkBase;
#else
#define CALLER_SAVE_SparkBase   	/* nothing */
#define CALLER_RESTORE_SparkBase   	/* nothing */
#endif

#ifdef CALLER_SAVES_SparkLim
#define CALLER_SAVE_SparkLim   		SAVE_SparkLim = SparkLim;
#define CALLER_RESTORE_SparkLim		SparkLim = SAVE_SparkLim;
#else
#define CALLER_SAVE_SparkLim   		/* nothing */
#define CALLER_RESTORE_SparkLim   	/* nothing */
#endif

#endif /* IN_STG_CODE */

/* ----------------------------------------------------------------------------
   Handy bunches of saves/restores 
   ------------------------------------------------------------------------  */

#if IN_STG_CODE

#define CALLER_SAVE_USER			\
  CALLER_SAVE_R1				\
  CALLER_SAVE_R2				\
  CALLER_SAVE_R3				\
  CALLER_SAVE_R4				\
  CALLER_SAVE_R5				\
  CALLER_SAVE_R6				\
  CALLER_SAVE_R7				\
  CALLER_SAVE_R8				\
  CALLER_SAVE_F1				\
  CALLER_SAVE_F2				\
  CALLER_SAVE_F3				\
  CALLER_SAVE_F4				\
  CALLER_SAVE_D1				\
  CALLER_SAVE_D2				\
  CALLER_SAVE_L1

     /* Save Base last, since the others may
	be addressed relative to it */
#define CALLER_SAVE_SYSTEM			\
  CALLER_SAVE_Sp				\
  CALLER_SAVE_SpLim				\
  CALLER_SAVE_Hp				\
  CALLER_SAVE_HpLim				\
  CALLER_SAVE_CurrentTSO			\
  CALLER_SAVE_CurrentNursery			\
  CALLER_SAVE_SparkHd				\
  CALLER_SAVE_SparkTl				\
  CALLER_SAVE_SparkBase				\
  CALLER_SAVE_SparkLim                          \
  CALLER_SAVE_Base

#define CALLER_RESTORE_USER			\
  CALLER_RESTORE_R1				\
  CALLER_RESTORE_R2				\
  CALLER_RESTORE_R3				\
  CALLER_RESTORE_R4				\
  CALLER_RESTORE_R5				\
  CALLER_RESTORE_R6				\
  CALLER_RESTORE_R7				\
  CALLER_RESTORE_R8				\
  CALLER_RESTORE_F1				\
  CALLER_RESTORE_F2				\
  CALLER_RESTORE_F3				\
  CALLER_RESTORE_F4				\
  CALLER_RESTORE_D1				\
  CALLER_RESTORE_D2				\
  CALLER_RESTORE_L1

     /* Restore Base first, since the others may
	be addressed relative to it */
#define CALLER_RESTORE_SYSTEM			\
  CALLER_RESTORE_Base				\
  CALLER_RESTORE_Sp				\
  CALLER_RESTORE_SpLim				\
  CALLER_RESTORE_Hp				\
  CALLER_RESTORE_HpLim				\
  CALLER_RESTORE_CurrentTSO			\
  CALLER_RESTORE_CurrentNursery			\
  CALLER_RESTORE_SparkHd			\
  CALLER_RESTORE_SparkTl			\
  CALLER_RESTORE_SparkBase			\
  CALLER_RESTORE_SparkLim

#else /* not IN_STG_CODE */

#define CALLER_SAVE_USER       /* nothing */
#define CALLER_SAVE_SYSTEM     /* nothing */
#define CALLER_RESTORE_USER    /* nothing */
#define CALLER_RESTORE_SYSTEM  /* nothing */

#endif /* IN_STG_CODE */
#define CALLER_SAVE_ALL				\
  CALLER_SAVE_SYSTEM				\
  CALLER_SAVE_USER

#define CALLER_RESTORE_ALL			\
  CALLER_RESTORE_SYSTEM				\
  CALLER_RESTORE_USER

#endif /* REGS_H */
