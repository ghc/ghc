/* ----------------------------------------------------------------------------
 * 
 * (c) The GHC Team, 1998-2002
 *
 * Info Tables
 *
 * -------------------------------------------------------------------------- */

#ifndef INFOTABLES_H
#define INFOTABLES_H

/* -----------------------------------------------------------------------------
   Profiling info
   -------------------------------------------------------------------------- */

typedef struct {
    char *closure_type;
    char *closure_desc;
} StgProfInfo;

/* -----------------------------------------------------------------------------
   Parallelism info
   -------------------------------------------------------------------------- */

#if 0 && (defined(PAR) || defined(GRAN))

/* CURRENTLY UNUSED
   ToDo: use this in StgInfoTable (mutually recursive) -- HWL */

typedef struct {
  StgInfoTable *rbh_infoptr;     /* infoptr to the RBH  */
} StgParInfo;

#endif /* 0 */

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
/*
EXTFUN(RBH_entry);
StgClosure *convertToRBH(StgClosure *closure);
#if defined(GRAN)
void convertFromRBH(StgClosure *closure);
#elif defined(PAR)
void convertToFetchMe(StgPtr closure, globalAddr *ga);
#endif
*/

#endif

/* -----------------------------------------------------------------------------
   Ticky info

   There is no ticky-specific stuff in an info table at this time.
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
   Debugging info
   -------------------------------------------------------------------------- */

#ifdef DEBUG_CLOSURE

typedef struct {
	... whatever ...
} StgDebugInfo;

#else /* !DEBUG_CLOSURE */

/* There is no DEBUG-specific stuff in an info table at this time. */

#endif /* DEBUG_CLOSURE */

/* -----------------------------------------------------------------------------
   Closure flags
   -------------------------------------------------------------------------- */

/* The type flags provide quick access to certain properties of a closure. */

#define _HNF (1<<0)  /* head normal form?    */
#define _BTM (1<<1)  /* bitmap-style layout? */
#define _NS  (1<<2)  /* non-sparkable        */
#define _STA (1<<3)  /* static?              */
#define _THU (1<<4)  /* thunk?               */
#define _MUT (1<<5)  /* mutable?             */
#define _UPT (1<<6)  /* unpointed?           */
#define _SRT (1<<7)  /* has an SRT?          */
#define _IND (1<<8)  /* is an indirection?   */

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
#define closure_IND(c)          (  closureFlags(c) & _IND)

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
#define ip_IND(ip)          	 (  ipFlags(ip) & _IND)

/* -----------------------------------------------------------------------------
   Bitmaps

   These are used to describe the pointerhood of a sequence of words
   (usually on the stack) to the garbage collector.  The two primary
   uses are for stack frames, and functions (where we need to describe
   the layout of a PAP to the GC).

   In these bitmaps: 0 == ptr, 1 == non-ptr.
   -------------------------------------------------------------------------- */

/*
 * Small bitmaps:  for a small bitmap, we store the size and bitmap in 
 * the same word, using the following macros.  If the bitmap doesn't
 * fit in a single word, we use a pointer to an StgLargeBitmap below.
 */
#define MK_SMALL_BITMAP(size,bits) (((bits)<<BITMAP_BITS_SHIFT) | (size))

#define BITMAP_SIZE(bitmap) ((bitmap) & BITMAP_SIZE_MASK)
#define BITMAP_BITS(bitmap) ((bitmap) >> BITMAP_BITS_SHIFT)

/*
 * A large bitmap.
 */
typedef struct {
  StgWord size;
  StgWord bitmap[FLEXIBLE_ARRAY];
} StgLargeBitmap;

/* -----------------------------------------------------------------------------
   SRTs  (Static Reference Tables)

   These tables are used to keep track of the static objects referred
   to by the code for a closure or stack frame, so that we can follow
   static data references from code and thus accurately
   garbage-collect CAFs.
   -------------------------------------------------------------------------- */

/* An SRT is just an array of closure pointers: */
typedef StgClosure* StgSRT[];

/*
 * Each info table refers to some subset of the closure pointers in an
 * SRT.  It does this using a pair of an StgSRT pointer and a
 * half-word bitmap.  If the half-word bitmap isn't large enough, then
 * we fall back to a large SRT, including an unbounded bitmap.  If the
 * half-word bitmap is set to all ones (0xffff), then the StgSRT
 * pointer instead points to an StgLargeSRT:
 */
typedef struct StgLargeSRT_ {
    StgSRT *srt;
    StgLargeBitmap l;
} StgLargeSRT;

/* ----------------------------------------------------------------------------
   Info Tables
   ------------------------------------------------------------------------- */

/*
 * Stuff describing the closure layout.  Well, actually, it might
 * contain the selector index for a THUNK_SELECTOR.  This union is one
 * word long.
 */
typedef union {
    struct {			/* Heap closure payload layout: */
	StgHalfWord ptrs;	/* number of pointers */
	StgHalfWord nptrs;	/* number of non-pointers */
    } payload;
    
    StgWord bitmap;		  /* word-sized bit pattern describing */
				  /*  a stack frame: see below */

#ifndef TABLES_NEXT_TO_CODE
    StgLargeBitmap* large_bitmap; /* pointer to large bitmap structure */
#else
    StgWord large_bitmap_offset;  /* offset from info table to large bitmap structure */
#endif
    
    StgWord selector_offset;	  /* used in THUNK_SELECTORs */

} StgClosureInfo;


/*
 * The "standard" part of an info table.  Every info table has this bit.
 */
typedef struct _StgInfoTable {

#ifndef TABLES_NEXT_TO_CODE
    StgFunPtr       entry;	/* pointer to the entry code */
#endif

#if defined(PAR) || defined(GRAN)
    struct _StgInfoTable    *rbh_infoptr;
#endif
#ifdef PROFILING
    StgProfInfo     prof;
#endif
#ifdef TICKY
  /* Ticky-specific stuff would go here. */
#endif
#ifdef DEBUG_CLOSURE
  /* Debug-specific stuff would go here. */
#endif

    StgClosureInfo  layout;	/* closure layout info (one word) */

    StgHalfWord     type;	/* closure type */
    StgHalfWord     srt_bitmap;    /* number of entries in SRT (or constructor tag) */

#ifdef TABLES_NEXT_TO_CODE
    StgCode         code[FLEXIBLE_ARRAY];
#endif
} StgInfoTable;


/* -----------------------------------------------------------------------------
   Function info tables

   This is the general form of function info tables.  The compiler
   will omit some of the fields in common cases:

   -  If fun_type is not ARG_GEN or ARG_GEN_BIG, then the slow_apply
      and bitmap fields may be left out (they are at the end, so omitting
      them doesn't affect the layout).
      
   -  If srt_bitmap (in the std info table part) is zero, then the srt
      field may be omitted.  This only applies if the slow_apply and
      bitmap fields have also been omitted.
   -------------------------------------------------------------------------- */

typedef struct _StgFunInfoExtraRev {
    StgWord        slow_apply_offset; /* apply to args on the stack */
    StgWord        bitmap;	/* arg ptr/nonptr bitmap */
    StgWord        srt_offset;	/* pointer to the SRT table */
    StgHalfWord    fun_type;    /* function type */
    StgHalfWord    arity;       /* function arity */
} StgFunInfoExtraRev;

typedef struct _StgFunInfoExtraFwd {
    StgHalfWord    fun_type;    /* function type */
    StgHalfWord    arity;       /* function arity */
    StgSRT         *srt;	/* pointer to the SRT table */
    StgWord        bitmap;	/* arg ptr/nonptr bitmap */
    StgFun         *slow_apply; /* apply to args on the stack */
} StgFunInfoExtraFwd;

typedef struct {
#if defined(TABLES_NEXT_TO_CODE)
    StgFunInfoExtraRev f;
    StgInfoTable i;
#else
    StgInfoTable i;
    StgFunInfoExtraFwd f;
#endif
} StgFunInfoTable;

/* -----------------------------------------------------------------------------
   Return info tables
   -------------------------------------------------------------------------- */

/*
 * When info tables are laid out backwards, we can omit the SRT
 * pointer iff srt_bitmap is zero.
 */

typedef struct {
#if defined(TABLES_NEXT_TO_CODE)
    StgWord      srt_offset;	/* offset to the SRT table */
    StgInfoTable i;
#else
    StgInfoTable i;
    StgSRT      *srt;	/* pointer to the SRT table */
    StgFunPtr vector[FLEXIBLE_ARRAY];
#endif
} StgRetInfoTable;

/* -----------------------------------------------------------------------------
   Thunk info tables
   -------------------------------------------------------------------------- */

/*
 * When info tables are laid out backwards, we can omit the SRT
 * pointer iff srt_bitmap is zero.
 */

typedef struct _StgThunkInfoTable {
#if !defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif
#if defined(TABLES_NEXT_TO_CODE)
    StgWord        srt_offset;	/* offset to the SRT table */
#else
    StgSRT         *srt;	/* pointer to the SRT table */
#endif
#if defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif
} StgThunkInfoTable;


/* -----------------------------------------------------------------------------
   Accessor macros for fields that might be offsets (C version)
   -------------------------------------------------------------------------- */

/*
 * GET_SRT(info)
 * info must be a Stg[Ret|Thunk]InfoTable* (an info table that has a SRT)
 */
#ifdef TABLES_NEXT_TO_CODE
#define GET_SRT(info) ((StgSRT*) (((StgWord) ((info)+1)) + (info)->srt_offset))
#else
#define GET_SRT(info) ((info)->srt)
#endif

/*
 * GET_FUN_SRT(info)
 * info must be a StgFunInfoTable*
 */
#ifdef TABLES_NEXT_TO_CODE
#define GET_FUN_SRT(info) ((StgSRT*) (((StgWord) ((info)+1)) + (info)->f.srt_offset))
#else
#define GET_FUN_SRT(info) ((info)->f.srt)
#endif

#ifdef TABLES_NEXT_TO_CODE
#define GET_LARGE_BITMAP(info) ((StgLargeBitmap*) (((StgWord) ((info)+1)) \
                                        + (info)->layout.large_bitmap_offset))
#else
#define GET_LARGE_BITMAP(info) ((info)->layout.large_bitmap)
#endif

#ifdef TABLES_NEXT_TO_CODE
#define GET_FUN_LARGE_BITMAP(info) ((StgLargeBitmap*) (((StgWord) ((info)+1)) \
                                        + (info)->f.bitmap))
#else
#define GET_FUN_LARGE_BITMAP(info) ((StgLargeBitmap*) ((info)->f.bitmap))
#endif


#endif /* INFOTABLES_H */
