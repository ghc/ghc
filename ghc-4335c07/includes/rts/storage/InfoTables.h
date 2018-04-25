/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Info Tables
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* ----------------------------------------------------------------------------
   Relative pointers

   Several pointer fields in info tables are expressed as offsets
   relative to the info pointer, so that we can generate
   position-independent code.

   Note [x86-64-relative]
   There is a complication on the x86_64 platform, where pointers are
   64 bits, but the tools don't support 64-bit relative relocations.
   However, the default memory model (small) ensures that all symbols
   have values in the lower 2Gb of the address space, so offsets all
   fit in 32 bits.  Hence we can use 32-bit offset fields.

   Somewhere between binutils-2.16.1 and binutils-2.16.91.0.6,
   support for 64-bit PC-relative relocations was added, so maybe this
   hackery can go away sometime.
   ------------------------------------------------------------------------- */

#if defined(x86_64_TARGET_ARCH)
#define OFFSET_FIELD(n) StgHalfInt n; StgHalfWord __pad_##n
#else
#define OFFSET_FIELD(n) StgInt n
#endif

/* -----------------------------------------------------------------------------
   Profiling info
   -------------------------------------------------------------------------- */

typedef struct {
#if !defined(TABLES_NEXT_TO_CODE)
    char *closure_type;
    char *closure_desc;
#else
    OFFSET_FIELD(closure_type_off);
    OFFSET_FIELD(closure_desc_off);
#endif
} StgProfInfo;

/* -----------------------------------------------------------------------------
   Closure flags
   -------------------------------------------------------------------------- */

/* The type flags provide quick access to certain properties of a closure. */

#define _HNF (1<<0)  /* head normal form?    */
#define _BTM (1<<1)  /* uses info->layout.bitmap */
#define _NS  (1<<2)  /* non-sparkable        */
#define _THU (1<<3)  /* thunk?               */
#define _MUT (1<<4)  /* mutable?             */
#define _UPT (1<<5)  /* unpointed?           */
#define _SRT (1<<6)  /* has an SRT?          */
#define _IND (1<<7)  /* is an indirection?   */

#define isMUTABLE(flags)   ((flags) &_MUT)
#define isBITMAP(flags)    ((flags) &_BTM)
#define isTHUNK(flags)     ((flags) &_THU)
#define isUNPOINTED(flags) ((flags) &_UPT)
#define hasSRT(flags)      ((flags) &_SRT)

extern StgWord16 closure_flags[];

#define closureFlags(c)         (closure_flags[get_itbl \
                                    (UNTAG_CONST_CLOSURE(c))->type])

#define closure_HNF(c)          (  closureFlags(c) & _HNF)
#define closure_BITMAP(c)       (  closureFlags(c) & _BTM)
#define closure_NON_SPARK(c)    ( (closureFlags(c) & _NS))
#define closure_SHOULD_SPARK(c) (!(closureFlags(c) & _NS))
#define closure_THUNK(c)        (  closureFlags(c) & _THU)
#define closure_MUTABLE(c)      (  closureFlags(c) & _MUT)
#define closure_UNPOINTED(c)    (  closureFlags(c) & _UPT)
#define closure_SRT(c)          (  closureFlags(c) & _SRT)
#define closure_IND(c)          (  closureFlags(c) & _IND)

/* same as above but for info-ptr rather than closure */
#define ipFlags(ip)             (closure_flags[ip->type])

#define ip_HNF(ip)               (  ipFlags(ip) & _HNF)
#define ip_BITMAP(ip)            (  ipFlags(ip) & _BTM)
#define ip_SHOULD_SPARK(ip)      (!(ipFlags(ip) & _NS))
#define ip_THUNK(ip)             (  ipFlags(ip) & _THU)
#define ip_MUTABLE(ip)           (  ipFlags(ip) & _MUT)
#define ip_UNPOINTED(ip)         (  ipFlags(ip) & _UPT)
#define ip_SRT(ip)               (  ipFlags(ip) & _SRT)
#define ip_IND(ip)               (  ipFlags(ip) & _IND)

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
  StgWord bitmap[];
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
    struct {                    /* Heap closure payload layout: */
        StgHalfWord ptrs;       /* number of pointers */
        StgHalfWord nptrs;      /* number of non-pointers */
    } payload;

    StgWord bitmap;               /* word-sized bit pattern describing */
                                  /*  a stack frame: see below */

#if !defined(TABLES_NEXT_TO_CODE)
    StgLargeBitmap* large_bitmap; /* pointer to large bitmap structure */
#else
    OFFSET_FIELD(large_bitmap_offset);  /* offset from info table to large bitmap structure */
#endif

    StgWord selector_offset;      /* used in THUNK_SELECTORs */

} StgClosureInfo;


/*
 * The "standard" part of an info table.  Every info table has this bit.
 */
typedef struct StgInfoTable_ {

#if !defined(TABLES_NEXT_TO_CODE)
    StgFunPtr       entry;      /* pointer to the entry code */
#endif

#if defined(PROFILING)
    StgProfInfo     prof;
#endif

    StgClosureInfo  layout;     /* closure layout info (one word) */

    StgHalfWord     type;       /* closure type */
    StgHalfWord     srt_bitmap;
       /* In a CONSTR:
            - the constructor tag
          In a FUN/THUNK
            - a bitmap of SRT entries
       */

#if defined(TABLES_NEXT_TO_CODE)
    StgCode         code[];
#endif
} *StgInfoTablePtr; // StgInfoTable defined in rts/Types.h


/* -----------------------------------------------------------------------------
   Function info tables

   This is the general form of function info tables.  The compiler
   will omit some of the fields in common cases:

   -  If fun_type is not ARG_GEN or ARG_GEN_BIG, then the slow_apply
      and bitmap fields may be left out (they are at the end, so omitting
      them doesn't affect the layout).

   -  If srt_bitmap (in the std info table part) is zero, then the srt
      field needn't be set.  This only applies if the slow_apply and
      bitmap fields have also been omitted.
   -------------------------------------------------------------------------- */

/*
   Note [Encoding static reference tables]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   As static reference tables appear frequently in code, we use a special
   compact encoding for the common case of a module defining only a few CAFs: We
   produce one table containing a list of CAFs in the module and then include a
   bitmap in each info table describing which entries of this table the closure
   references.
 */

typedef struct StgFunInfoExtraRev_ {
    OFFSET_FIELD(slow_apply_offset); /* apply to args on the stack */
    union {
        StgWord bitmap;
        OFFSET_FIELD(bitmap_offset);    /* arg ptr/nonptr bitmap */
    } b;
    OFFSET_FIELD(srt_offset);   /* pointer to the SRT table */
    StgHalfWord    fun_type;    /* function type */
    StgHalfWord    arity;       /* function arity */
} StgFunInfoExtraRev;

typedef struct StgFunInfoExtraFwd_ {
    StgHalfWord    fun_type;    /* function type */
    StgHalfWord    arity;       /* function arity */
    StgSRT         *srt;        /* pointer to the SRT table */
    union { /* union for compat. with TABLES_NEXT_TO_CODE version */
        StgWord        bitmap;  /* arg ptr/nonptr bitmap */
    } b;
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

// canned bitmap for each arg type, indexed by constants in FunTypes.h
extern const StgWord stg_arg_bitmaps[];

/* -----------------------------------------------------------------------------
   Return info tables
   -------------------------------------------------------------------------- */

/*
 * When info tables are laid out backwards, we can omit the SRT
 * pointer iff srt_bitmap is zero.
 */

typedef struct {
#if defined(TABLES_NEXT_TO_CODE)
    OFFSET_FIELD(srt_offset);   /* offset to the SRT table */
    StgInfoTable i;
#else
    StgInfoTable i;
    StgSRT      *srt;   /* pointer to the SRT table */
#endif
} StgRetInfoTable;

/* -----------------------------------------------------------------------------
   Thunk info tables
   -------------------------------------------------------------------------- */

/*
 * When info tables are laid out backwards, we can omit the SRT
 * pointer iff srt_bitmap is zero.
 */

typedef struct StgThunkInfoTable_ {
#if !defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif
#if defined(TABLES_NEXT_TO_CODE)
    OFFSET_FIELD(srt_offset);   /* offset to the SRT table */
#else
    StgSRT         *srt;        /* pointer to the SRT table */
#endif
#if defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif
} StgThunkInfoTable;

/* -----------------------------------------------------------------------------
   Constructor info tables
   -------------------------------------------------------------------------- */

typedef struct StgConInfoTable_ {
#if !defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif

#if defined(TABLES_NEXT_TO_CODE)
    OFFSET_FIELD(con_desc); // the name of the data constructor
                            // as: Package:Module.Name
#else
    char *con_desc;
#endif

#if defined(TABLES_NEXT_TO_CODE)
    StgInfoTable i;
#endif
} StgConInfoTable;


/* -----------------------------------------------------------------------------
   Accessor macros for fields that might be offsets (C version)
   -------------------------------------------------------------------------- */

/*
 * GET_SRT(info)
 * info must be a Stg[Ret|Thunk]InfoTable* (an info table that has a SRT)
 */
#if defined(TABLES_NEXT_TO_CODE)
#define GET_SRT(info) ((StgSRT*) (((StgWord) ((info)+1)) + (info)->srt_offset))
#else
#define GET_SRT(info) ((info)->srt)
#endif

/*
 * GET_CON_DESC(info)
 * info must be a StgConInfoTable*.
 */
#if defined(TABLES_NEXT_TO_CODE)
#define GET_CON_DESC(info) \
            ((const char *)((StgWord)((info)+1) + (info->con_desc)))
#else
#define GET_CON_DESC(info) ((const char *)(info)->con_desc)
#endif

/*
 * GET_FUN_SRT(info)
 * info must be a StgFunInfoTable*
 */
#if defined(TABLES_NEXT_TO_CODE)
#define GET_FUN_SRT(info) ((StgSRT*) (((StgWord) ((info)+1)) + (info)->f.srt_offset))
#else
#define GET_FUN_SRT(info) ((info)->f.srt)
#endif

#if defined(TABLES_NEXT_TO_CODE)
#define GET_LARGE_BITMAP(info) ((StgLargeBitmap*) (((StgWord) ((info)+1)) \
                                        + (info)->layout.large_bitmap_offset))
#else
#define GET_LARGE_BITMAP(info) ((info)->layout.large_bitmap)
#endif

#if defined(TABLES_NEXT_TO_CODE)
#define GET_FUN_LARGE_BITMAP(info) ((StgLargeBitmap*) (((StgWord) ((info)+1)) \
                                        + (info)->f.b.bitmap_offset))
#else
#define GET_FUN_LARGE_BITMAP(info) ((StgLargeBitmap*) ((info)->f.b.bitmap))
#endif

/*
 * GET_PROF_TYPE, GET_PROF_DESC
 */
#if defined(TABLES_NEXT_TO_CODE)
#define GET_PROF_TYPE(info) ((char *)((StgWord)((info)+1) + (info->prof.closure_type_off)))
#else
#define GET_PROF_TYPE(info) ((info)->prof.closure_type)
#endif
#if defined(TABLES_NEXT_TO_CODE)
#define GET_PROF_DESC(info) ((char *)((StgWord)((info)+1) + (info->prof.closure_desc_off)))
#else
#define GET_PROF_DESC(info) ((info)->prof.closure_desc)
#endif
