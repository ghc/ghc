/* ----------------------------------------------------------------------------
 * $Id: InfoTables.h,v 1.8 1999/02/05 12:41:32 sof Exp $
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

/* -----------------------------------------------------------------------------
   Closure Types

   If you add or delete any closure types, don't forget to update
   ClosureTypes.h for the native code generator.  This is a temporary
   measure (I hope).
   -------------------------------------------------------------------------- */

typedef enum {

    INVALID_OBJECT /* Object tag 0 raises an internal error */

    , CONSTR
    , CONSTR_1_0
    , CONSTR_0_1
    , CONSTR_2_0
    , CONSTR_1_1
    , CONSTR_0_2
    , CONSTR_INTLIKE
    , CONSTR_CHARLIKE
    , CONSTR_STATIC
    , CONSTR_NOCAF_STATIC

    , FUN
    , FUN_1_0
    , FUN_0_1
    , FUN_2_0
    , FUN_1_1
    , FUN_0_2
    , FUN_STATIC

    , THUNK
    , THUNK_1_0
    , THUNK_0_1
    , THUNK_2_0
    , THUNK_1_1
    , THUNK_0_2
    , THUNK_STATIC
    , THUNK_SELECTOR

    , BCO
    , AP_UPD

    , PAP  /* should be called AP_NUPD */

    , IND
    , IND_OLDGEN
    , IND_PERM
    , IND_OLDGEN_PERM
    , IND_STATIC

    , CAF_UNENTERED
    , CAF_ENTERED
    , CAF_BLACKHOLE

    , RET_BCO
    , RET_SMALL
    , RET_VEC_SMALL
    , RET_BIG
    , RET_VEC_BIG
    , RET_DYN
    , UPDATE_FRAME
    , CATCH_FRAME
    , STOP_FRAME
    , SEQ_FRAME

    , BLACKHOLE
    , BLACKHOLE_BQ

    , MVAR

    , ARR_WORDS
    , MUT_ARR_WORDS

    , MUT_ARR_PTRS
    , MUT_ARR_PTRS_FROZEN

    , MUT_VAR

    , WEAK
    , FOREIGN
    , STABLE_NAME

    , TSO

    , BLOCKED_FETCH
    , FETCH_ME

    , EVACUATED

    , N_CLOSURE_TYPES		/* number of distinct closure types */

} StgClosureType;

/* The type flags provide quick access to certain properties of a closure. */

#define _HNF (1<<0)  /* head normal form?  */
#define _BTM (1<<1)  /* bitmap-style layout? */
#define _NS  (1<<2)  /* non-sparkable      */
#define _STA (1<<3)  /* static?            */
#define _THU (1<<4)  /* thunk?             */
#define _MUT (1<<5)  /* mutable?           */
#define _UPT (1<<6)  /* unpointed?         */
#define _SRT (1<<7)  /* has an SRT?        */

#define isSTATIC(flags)  ((flags)&_STA)
#define isMUTABLE(flags) ((flags) &_MUT)

#define closure_STATIC(closure)       (  get_itbl(closure)->flags & _STA)
#define closure_SHOULD_SPARK(closure) (!(get_itbl(closure)->flags & _NS))
#define closure_MUTABLE(closure)      (  get_itbl(closure)->flags & _MUT)
#define closure_UNPOINTED(closure)    (  get_itbl(closure)->flags & _UPT)

/*				    HNF  BTM   NS  STA  THU MUT UPT SRT */
				                                    
#define FLAGS_CONSTR  		   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_1_0	   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_0_1	   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_2_0	   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_1_1	   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_0_2	   (_HNF|     _NS                        )	
#define FLAGS_CONSTR_STATIC	   (_HNF|     _NS|_STA                   )	
#define FLAGS_CONSTR_NOCAF_STATIC  (_HNF|     _NS|_STA                   )	
#define FLAGS_FUN		   (_HNF|     _NS|                  _SRT )	
#define FLAGS_FUN_1_0		   (_HNF|     _NS                        )	
#define FLAGS_FUN_0_1		   (_HNF|     _NS                        )	
#define FLAGS_FUN_2_0		   (_HNF|     _NS                        )	
#define FLAGS_FUN_1_1		   (_HNF|     _NS                        )	
#define FLAGS_FUN_0_2		   (_HNF|     _NS                        )	
#define FLAGS_FUN_STATIC	   (_HNF|     _NS|_STA|             _SRT )	
#define FLAGS_THUNK		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_1_0		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_0_1		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_2_0		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_1_1		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_0_2		   (     _BTM|         _THU|        _SRT )	
#define FLAGS_THUNK_STATIC	   (     _BTM|    _STA|_THU|        _SRT )	
#define FLAGS_THUNK_SELECTOR	   (     _BTM|         _THU|        _SRT )	
#define FLAGS_BCO		   (_HNF|     _NS                        )	
#define FLAGS_CAF_UNENTERED        0 /* Do we still use these? */
#define FLAGS_CAF_ENTERED          0
#define FLAGS_CAF_BLACKHOLE        ( 	 _BTM|_NS|              _UPT     )
#define FLAGS_AP_UPD		   (     _BTM|         _THU              )	
#define FLAGS_PAP		   (_HNF|     _NS                        )	
#define FLAGS_IND		   0
#define FLAGS_IND_OLDGEN	   0
#define FLAGS_IND_PERM		   0
#define FLAGS_IND_OLDGEN_PERM	   0
#define FLAGS_IND_STATIC	   (              _STA                   )	
#define FLAGS_EVACUATED		   0
#define FLAGS_ARR_WORDS		   (_HNF|     _NS|              _UPT     )	
#define FLAGS_MUT_ARR_WORDS	   (_HNF|     _NS|         _MUT|_UPT     )	
#define FLAGS_MUT_ARR_PTRS	   (_HNF|     _NS|         _MUT|_UPT     )	
#define FLAGS_MUT_ARR_PTRS_FROZEN  (_HNF|     _NS|         _MUT|_UPT     )	
#define FLAGS_MUT_VAR		   (_HNF|     _NS|         _MUT|_UPT     )	
#define FLAGS_FOREIGN		   (_HNF|     _NS|              _UPT     )	
#define FLAGS_STABLE_NAME	   (_HNF|     _NS|              _UPT     )	
#define FLAGS_WEAK		   (_HNF|     _NS|              _UPT     )	
#define FLAGS_BLACKHOLE		   ( 	      _NS|              _UPT     )	
#define FLAGS_BLACKHOLE_BQ	   ( 	      _NS|         _MUT|_UPT     )	
#define FLAGS_MVAR		   (_HNF|     _NS|         _MUT|_UPT     )	
#define FLAGS_FETCH_ME		   (_HNF|     _NS                        )	
#define FLAGS_TSO                  (_HNF|     _NS|         _MUT|_UPT     )
#define FLAGS_RET_BCO		   (     _BTM                            )
#define FLAGS_RET_SMALL		   (     _BTM|                       _SRT)
#define FLAGS_RET_VEC_SMALL	   (     _BTM|                       _SRT)
#define FLAGS_RET_BIG		   (                                 _SRT)
#define FLAGS_RET_VEC_BIG	   (                                 _SRT)
#define FLAGS_RET_DYN		   (                                 _SRT)
#define FLAGS_CATCH_FRAME	   (     _BTM                            )
#define FLAGS_STOP_FRAME	   (     _BTM                            )
#define FLAGS_SEQ_FRAME 	   (     _BTM                            )
#define FLAGS_UPDATE_FRAME         (     _BTM                            )

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

  StgWord bitmap;		/* bit pattern, 1 = pointer, 0 = non-pointer */
  StgWord selector_offset;	/* used in THUNK_SELECTORs */
  StgLargeBitmap* large_bitmap;	/* pointer to large bitmap structure */

#if SIZEOF_VOID_P == 8
  struct {
    StgNat32 ptrs;		/* number of pointers     */
    StgNat32 nptrs;		/* number of non-pointers */
  } payload;
#else
  struct {
    StgNat16 ptrs;		/* number of pointers     */
    StgNat16 nptrs;		/* number of non-pointers */
  } payload;
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
    StgNat16        flags;	/* }                                   */
    StgClosureType  type : 16;	/* } These 4 elements fit into 64 bits */
    StgNat32        srt_len;    /* }                                   */
#else
    StgNat8         flags;	/* }                                   */
    StgClosureType  type : 8;	/* } These 4 elements fit into 32 bits */
    StgNat16        srt_len;    /* }                                   */
#endif
#if USE_MINIINTERPRETER
    StgFunPtr       (*vector)[];
    StgFunPtr       entry;
#else
    StgCode         code[0];
#endif
} StgInfoTable;

#endif /* INFOTABLES_H */
