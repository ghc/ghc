/* ----------------------------------------------------------------------------
 * $Id: InfoTables.h,v 1.13 1999/03/15 16:30:25 simonm Exp $
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

#define PROF_INFO_WORDS n

typedef struct {
  /* nothing yet */
} StgProfInfo;

#else /* !PROFILING */

#define PROF_INFO_WORDS 0

typedef struct {
  /* empty */
} StgProfInfo;

#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   Parallelism info
   -------------------------------------------------------------------------- */

#ifdef PAR

#define PAR_INFO_WORDS 0

typedef struct {
       /* empty */
} StgParInfo;

#else /* !PAR */

#define PAR_INFO_WORDS 0

typedef struct {
	/* empty */
} StgParInfo;

#endif /* PAR */

/* -----------------------------------------------------------------------------
   Debugging info
   -------------------------------------------------------------------------- */

#ifdef DEBUG_CLOSURE

#define DEBUG_INFO_WORDS n

typedef struct {
	... whatever ...
} StgDebugInfo;

#else /* !DEBUG_CLOSURE */

#define DEBUG_INFO_WORDS 0

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

#define closure_STATIC(c)       (  closureFlags(c) & _STA)
#define closure_SHOULD_SPARK(c) (!(closureFlags(c) & _NS))
#define closure_MUTABLE(c)      (  closureFlags(c) & _MUT)
#define closure_UNPOINTED(c)    (  closureFlags(c) & _UPT)


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
#if SIZEOF_VOID_P == 8
  struct {
    StgWord32 ptrs;		/* number of pointers     */
    StgWord32 nptrs;		/* number of non-pointers */
  } payload;
#else
  struct {
    StgWord16 ptrs;		/* number of pointers     */
    StgWord16 nptrs;		/* number of non-pointers */
  } payload;

  StgWord bitmap;		/* bit pattern, 1 = pointer, 0 = non-pointer */
  StgWord selector_offset;	/* used in THUNK_SELECTORs */
  StgLargeBitmap* large_bitmap;	/* pointer to large bitmap structure */

#endif
  
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
#ifdef PAR
    StgParInfo	    par;
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
#if USE_MINIINTERPRETER
    StgFunPtr       (*vector)[];
    StgFunPtr       entry;
#else
    StgCode         code[0];
#endif
} StgInfoTable;

#endif /* INFOTABLES_H */
