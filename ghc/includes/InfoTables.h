/* ----------------------------------------------------------------------------
 * $Id: InfoTables.h,v 1.19 2000/04/05 15:27:59 simonmar Exp $
 * 
 * (c) The GHC Team, 1998-1999
 *
 * Info Tables
 *
 * -------------------------------------------------------------------------- */

#ifndef INFOTABLES_H
#define INFOTABLES_H

/* -----------------------------------------------------------------------------
   Profiling info
   -------------------------------------------------------------------------- */

#ifdef PROFILING

typedef struct {
    char *closure_type;
    char *closure_desc;
} StgProfInfo;

#else /* !PROFILING */

typedef struct {
  /* empty */
} StgProfInfo;

#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Parallelism info
   -------------------------------------------------------------------------- */

#if 0 && (defined(PAR) || defined(GRAN))

// CURRENTLY UNUSED
// ToDo: use this in StgInfoTable (mutually recursive) -- HWL

typedef struct {
  StgInfoTable *rbh_infoptr;     /* infoptr to the RBH  */
} StgParInfo;

#else /* !PAR */

typedef struct {
	/* empty */
} StgParInfo;

#endif /* PAR */

/*
   Copied from ghc-0.29; ToDo: check this code -- HWL

   In the parallel system, all updatable closures have corresponding
   revertible black holes.  When we are assembly-mangling, we guarantee
   that the revertible black hole code precedes the normal entry code, so
   that the RBH info table resides at a fixed offset from the normal info
   table.  Otherwise, we add the RBH info table pointer to the end of the
   normal info table and vice versa.

   Currently has to use a !RBH_MAGIC_OFFSET setting.
   Still todo: init of par.infoptr field in all infotables!!
*/

#if defined(PAR) || defined(GRAN)

# ifdef RBH_MAGIC_OFFSET

#  error magic offset not yet implemented

#  define RBH_INFO_WORDS    0
#  define INCLUDE_RBH_INFO(infoptr)

#  define RBH_INFOPTR(infoptr)	    (((P_)infoptr) - RBH_MAGIC_OFFSET)
#  define REVERT_INFOPTR(infoptr)   (((P_)infoptr) + RBH_MAGIC_OFFSET)

# else

#  define RBH_INFO_WORDS    1
#  define INCLUDE_RBH_INFO(info)    rbh_infoptr : &(info)

#  define RBH_INFOPTR(infoptr)	    (((StgInfoTable *)(infoptr))->rbh_infoptr)
#  define REVERT_INFOPTR(infoptr)   (((StgInfoTable *)(infoptr))->rbh_infoptr)

# endif

/* see ParallelRts.h */
// EXTFUN(RBH_entry);
//StgClosure *convertToRBH(StgClosure *closure);
//#if defined(GRAN)
//void convertFromRBH(StgClosure *closure);
//#elif defined(PAR)
//void convertToFetchMe(StgPtr closure, globalAddr *ga);
//#endif

#endif

/* -----------------------------------------------------------------------------
   Debugging info
   -------------------------------------------------------------------------- */

#ifdef DEBUG_CLOSURE

typedef struct {
	... whatever ...
} StgDebugInfo;

#else /* !DEBUG_CLOSURE */

typedef struct {
	/* empty */
} StgDebugInfo;

#endif /* DEBUG_CLOSURE */

/* The type flags provide quick access to certain properties of a closure. */

#define _HNF (1<<0)  /* head normal form?  */
#define _BTM (1<<1)  /* bitmap-style layout? */
#define _NS  (1<<2)  /* non-sparkable      */
#define _STA (1<<3)  /* static?            */
#define _THU (1<<4)  /* thunk?             */
#define _MUT (1<<5)  /* mutable?           */
#define _UPT (1<<6)  /* unpointed?         */
#define _SRT (1<<7)  /* has an SRT?        */

#define isSTATIC(flags)    ((flags) &_STA)
#define isMUTABLE(flags)   ((flags) &_MUT)
#define isBITMAP(flags)    ((flags) &_BTM)
#define isTHUNK(flags)     ((flags) &_THU)
#define isUNPOINTED(flags) ((flags) &_UPT)
#define hasSRT(flags)      ((flags) &_SRT)

extern StgWord16 closure_flags[];

#define closureFlags(c)         (closure_flags[get_itbl(c)->type])

#define closure_HNF(c)          (  closureFlags(c) & _HNF)
#define closure_BITMAP(c)       (  closureFlags(c) & _BTM)
#define closure_NON_SPARK(c)    ( (closureFlags(c) & _NS))
#define closure_SHOULD_SPARK(c) (!(closureFlags(c) & _NS))
#define closure_STATIC(c)       (  closureFlags(c) & _STA)
#define closure_THUNK(c)        (  closureFlags(c) & _THU)
#define closure_MUTABLE(c)      (  closureFlags(c) & _MUT)
#define closure_UNPOINTED(c)    (  closureFlags(c) & _UPT)
#define closure_SRT(c)          (  closureFlags(c) & _SRT)

/* same as above but for info-ptr rather than closure */
#define ipFlags(ip)             (closure_flags[ip->type])

#define ip_HNF(ip)               (  ipFlags(ip) & _HNF)
#define ip_BITMAP(ip)       	 (  ipFlags(ip) & _BTM)
#define ip_SHOULD_SPARK(ip) 	 (!(ipFlags(ip) & _NS))
#define ip_STATIC(ip)       	 (  ipFlags(ip) & _STA)
#define ip_THUNK(ip)        	 (  ipFlags(ip) & _THU)
#define ip_MUTABLE(ip)      	 (  ipFlags(ip) & _MUT)
#define ip_UNPOINTED(ip)    	 (  ipFlags(ip) & _UPT)
#define ip_SRT(ip)          	 (  ipFlags(ip) & _SRT)

/* -----------------------------------------------------------------------------
   Info Tables
   -------------------------------------------------------------------------- */

/* A large bitmap.  Small 32-bit ones live in the info table, but sometimes
 * 32 bits isn't enough and we have to generate a larger one.  (sizes
 * differ for 64-bit machines.
 */

typedef struct {
  StgWord size;
  StgWord bitmap[0];
} StgLargeBitmap;

/*
 * Stuff describing the closure layout.  Well, actually, it might
 * contain the selector index for a THUNK_SELECTOR.  If we're on a
 * 64-bit architecture then we can enlarge some of these fields, since
 * the union contains a pointer field.
 */

typedef union {
  struct {
#if SIZEOF_VOID_P == 8
    StgWord32 ptrs;		/* number of pointers     */
    StgWord32 nptrs;		/* number of non-pointers */
#else
    StgWord16 ptrs;		/* number of pointers     */
    StgWord16 nptrs;		/* number of non-pointers */
#endif
  } payload;

  StgWord bitmap;		/* bit pattern, 1 = pointer, 0 = non-pointer */
  StgWord selector_offset;	/* used in THUNK_SELECTORs */
  StgLargeBitmap* large_bitmap;	/* pointer to large bitmap structure */
  
} StgClosureInfo;

/*
 * Info tables.  All info tables are the same type, to simplify code
 * generation.  However, the mangler removes any unused SRT fields
 * from the asm to save space (convention: if srt_len is zero, or the
 * type is a CONSTR_ type, then the SRT field isn't present.
 */

typedef StgClosure* StgSRT[];

typedef struct _StgInfoTable {
    StgSRT         *srt;	/* pointer to the SRT table */
#if defined(PAR) || defined(GRAN)
    struct _StgInfoTable    *rbh_infoptr;
#endif
#ifdef PROFILING
    StgProfInfo     prof;
#endif
#ifdef DEBUG_CLOSURE
    StgDebugInfo    debug;
#endif
    StgClosureInfo  layout;	/* closure layout info (pointer-sized) */
#if SIZEOF_VOID_P == 8
    StgWord32       type;	/* } These 2 elements fit into 64 bits */
    StgWord32       srt_len;    /* }                                   */
#else
    StgWord         type : 16;	/* } These 2 elements fit into 32 bits */
    StgWord         srt_len : 16; /* }                                   */
#endif
#ifdef TABLES_NEXT_TO_CODE
    StgCode         code[0];
#else
    StgFunPtr       entry;
    StgFunPtr       vector[0];
#endif
} StgInfoTable;

/* Info tables are read-only, therefore we uniformly declare them with
 * C's const attribute.  This isn't just a nice thing to do: it's
 * necessary because the garbage collector has to distinguish between 
 * closure pointers and info table pointers when traversing the
 * stack.  We distinguish the two by checking whether the pointer is
 * into text-space or not.
 */
 
#define INFO_TBL_CONST  const

#endif /* INFOTABLES_H */
