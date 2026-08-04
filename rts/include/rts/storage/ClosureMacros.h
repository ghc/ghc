/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * Macros for building and manipulating closures
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* -----------------------------------------------------------------------------
   Info tables are slammed up against the entry code, and the label
   for the info table is at the *end* of the table itself.  This
   inline function adjusts an info pointer to point to the beginning
   of the table, so we can use standard C structure indexing on it.

   Note: this works for SRT info tables as long as you don't want to
   access the SRT, since they are laid out the same with the SRT
   pointer as the first word in the table.

   NOTES ABOUT MANGLED C VS. MINI-INTERPRETER:

   A couple of definitions:

       "info pointer"    The first word of the closure.  Might point
                         to either the end or the beginning of the
                         info table, depending on whether we're using
                         the mini interpreter or not.  GET_INFO(c)
                         retrieves the info pointer of a closure.

       "info table"      The info table structure associated with a
                         closure.  This is always a pointer to the
                         beginning of the structure, so we can
                         use standard C structure indexing to pull out
                         the fields.  get_itbl(c) returns a pointer to
                         the info table for closure c.

   An address of the form xxxx_info points to the end of the info
   table or the beginning of the info table depending on whether we're
   mangling or not respectively.  So,

         c->header.info = xxx_info

   makes absolute sense, whether mangling or not.

   -------------------------------------------------------------------------- */

EXTERN_INLINE void SET_INFO(StgClosure *c, const StgInfoTable *info);
EXTERN_INLINE void SET_INFO(StgClosure *c, const StgInfoTable *info) {
    c->header.info = info;
}

EXTERN_INLINE void SET_INFO_RELAXED(StgClosure *c, const StgInfoTable *info);
EXTERN_INLINE void SET_INFO_RELAXED(StgClosure *c, const StgInfoTable *info) {
    RELAXED_STORE(&c->header.info, info);
}

EXTERN_INLINE void SET_INFO_RELEASE(StgClosure *c, const StgInfoTable *info);
EXTERN_INLINE void SET_INFO_RELEASE(StgClosure *c, const StgInfoTable *info) {
    RELEASE_STORE(&c->header.info, info);
}
EXTERN_INLINE const StgInfoTable *GET_INFO(StgClosure *c);
EXTERN_INLINE const StgInfoTable *GET_INFO(StgClosure *c) {
    return RELAXED_LOAD(&c->header.info);
}

EXTERN_INLINE StgInfoTable      *INFO_PTR_TO_STRUCT     (const StgInfoTable *info);
EXTERN_INLINE StgRetInfoTable   *RET_INFO_PTR_TO_STRUCT (const StgInfoTable *info);
EXTERN_INLINE StgFunInfoTable   *FUN_INFO_PTR_TO_STRUCT (const StgInfoTable *info);
EXTERN_INLINE StgThunkInfoTable *THUNK_INFO_PTR_TO_STRUCT(const StgInfoTable *info);
EXTERN_INLINE StgConInfoTable   *CON_INFO_PTR_TO_STRUCT (const StgInfoTable *info);
EXTERN_INLINE StgFunInfoTable   *itbl_to_fun_itbl       (const StgInfoTable *i);
EXTERN_INLINE StgRetInfoTable   *itbl_to_ret_itbl       (const StgInfoTable *i);
EXTERN_INLINE StgThunkInfoTable *itbl_to_thunk_itbl     (const StgInfoTable *i);
EXTERN_INLINE StgConInfoTable   *itbl_to_con_itbl       (const StgInfoTable *i);

#if defined(TABLES_NEXT_TO_CODE)
NO_WARN(-Warray-bounds,
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgInfoTable *)info - 1;}
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgRetInfoTable *)info - 1;}
EXTERN_INLINE StgFunInfoTable *FUN_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgFunInfoTable *)info - 1;}
EXTERN_INLINE StgThunkInfoTable *THUNK_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgThunkInfoTable *)info - 1;}
EXTERN_INLINE StgConInfoTable *CON_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgConInfoTable *)info - 1;}
EXTERN_INLINE StgFunInfoTable *itbl_to_fun_itbl(const StgInfoTable *i) {return (StgFunInfoTable *)(i + 1) - 1;}
EXTERN_INLINE StgRetInfoTable *itbl_to_ret_itbl(const StgInfoTable *i) {return (StgRetInfoTable *)(i + 1) - 1;}
EXTERN_INLINE StgThunkInfoTable *itbl_to_thunk_itbl(const StgInfoTable *i) {return (StgThunkInfoTable *)(i + 1) - 1;}
EXTERN_INLINE StgConInfoTable *itbl_to_con_itbl(const StgInfoTable *i) {return (StgConInfoTable *)(i + 1) - 1;}
)
#else
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgInfoTable *)info;}
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgRetInfoTable *)info;}
EXTERN_INLINE StgFunInfoTable *FUN_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgFunInfoTable *)info;}
EXTERN_INLINE StgThunkInfoTable *THUNK_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgThunkInfoTable *)info;}
EXTERN_INLINE StgConInfoTable *CON_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgConInfoTable *)info;}
EXTERN_INLINE StgFunInfoTable *itbl_to_fun_itbl(const StgInfoTable *i) {return (StgFunInfoTable *)i;}
EXTERN_INLINE StgRetInfoTable *itbl_to_ret_itbl(const StgInfoTable *i) {return (StgRetInfoTable *)i;}
EXTERN_INLINE StgThunkInfoTable *itbl_to_thunk_itbl(const StgInfoTable *i) {return (StgThunkInfoTable *)i;}
EXTERN_INLINE StgConInfoTable *itbl_to_con_itbl(const StgInfoTable *i) {return (StgConInfoTable *)i;}
#endif

EXTERN_INLINE const StgInfoTable *get_itbl(const StgClosure *c);
EXTERN_INLINE const StgInfoTable *get_itbl(const StgClosure *c)
{
    return INFO_PTR_TO_STRUCT(RELAXED_LOAD(&c->header.info));
}

EXTERN_INLINE const StgInfoTable *get_itbl_acquire(const StgClosure *c);
EXTERN_INLINE const StgInfoTable *get_itbl_acquire(const StgClosure *c)
{
    return INFO_PTR_TO_STRUCT(ACQUIRE_LOAD(&c->header.info));
}

EXTERN_INLINE const StgRetInfoTable *get_ret_itbl(const StgClosure *c);
EXTERN_INLINE const StgRetInfoTable *get_ret_itbl(const StgClosure *c)
{
    return RET_INFO_PTR_TO_STRUCT(RELAXED_LOAD(&c->header.info));
}

EXTERN_INLINE const StgFunInfoTable *get_fun_itbl(const StgClosure *c);
EXTERN_INLINE const StgFunInfoTable *get_fun_itbl(const StgClosure *c)
{
    return FUN_INFO_PTR_TO_STRUCT(RELAXED_LOAD(&c->header.info));
}

EXTERN_INLINE const StgThunkInfoTable *get_thunk_itbl(const StgClosure *c);
EXTERN_INLINE const StgThunkInfoTable *get_thunk_itbl(const StgClosure *c)
{
    return THUNK_INFO_PTR_TO_STRUCT(RELAXED_LOAD(&c->header.info));
}

EXTERN_INLINE const StgConInfoTable *get_con_itbl(const StgClosure *c);
EXTERN_INLINE const StgConInfoTable *get_con_itbl(const StgClosure *c)
{
    return CON_INFO_PTR_TO_STRUCT(RELAXED_LOAD(&c->header.info));
}

EXTERN_INLINE StgHalfWord GET_TAG(const StgClosure *con);
EXTERN_INLINE StgHalfWord GET_TAG(const StgClosure *con)
{
    return get_itbl(con)->srt;
}

/* ----------------------------------------------------------------------------
   Macros for untagging and retagging closure pointers
   For more information look at the comments in Cmm.h
   ------------------------------------------------------------------------- */

EXTERN_INLINE StgWord GET_CLOSURE_TAG(const StgClosure * p);
EXTERN_INLINE StgWord GET_CLOSURE_TAG(const StgClosure * p)
{
    return (StgWord)p & TAG_MASK;
}

EXTERN_INLINE StgClosure *UNTAG_CLOSURE(StgClosure * p);
EXTERN_INLINE StgClosure *UNTAG_CLOSURE(StgClosure * p)
{
    return (StgClosure*)((StgWord)p & ~TAG_MASK);
}

EXTERN_INLINE const StgClosure *UNTAG_CONST_CLOSURE(const StgClosure * p);
EXTERN_INLINE const StgClosure *UNTAG_CONST_CLOSURE(const StgClosure * p)
{
    return (const StgClosure*)((StgWord)p & ~TAG_MASK);
}

EXTERN_INLINE StgClosure *TAG_CLOSURE(StgWord tag,StgClosure * p);
EXTERN_INLINE StgClosure *TAG_CLOSURE(StgWord tag,StgClosure * p)
{
    return (StgClosure*)((StgWord)p | tag);
}

// Compute the pointer tag for the constructor and tag the pointer;
// see Note [Data constructor dynamic tags] in GHC.StgToCmm.Closure.
//
// Note: we need to update this if we change the tagging strategy.
EXTERN_INLINE StgClosure *tagConstr(StgClosure *con);
EXTERN_INLINE StgClosure *tagConstr(StgClosure *con)
{
    return TAG_CLOSURE(stg_min(TAG_MASK, 1 + GET_TAG(con)), con);
}

/* -----------------------------------------------------------------------------
   Macros for building closures
   -------------------------------------------------------------------------- */

#if defined(PROFILING)

/*
  These prototypes are in RtsFlags.h. We can't include RtsFlags.h here
  because that's a private header, but we do need these prototypes to
  be duplicated here, otherwise there will be some
  -Wimplicit-function-declaration compilation errors. Especially when
  GHC compiles out-of-tree cbits that rely on SET_HDR in RTS API.

  However when RtsFlags.h is imported, we don't want to redefine them to avoid
  spurious warnings (#24918).
*/
#if !defined(RTS_FLAGS_DOING_PROFILING)
#define RTS_FLAGS_DOING_PROFILING 1
bool doingLDVProfiling(void);
bool doingRetainerProfiling(void);
bool doingErasProfiling(void);
#endif

/*
  The following macro works for both retainer profiling and LDV profiling. For
 retainer profiling, we set 'trav' to 0, which is an invalid
 RetainerSet.
 */

/*
  MP: Various other places use the check era > 0 to check whether LDV profiling
  is enabled. The use of these predicates here is the reason for including RtsFlags.h in
  a lot of places.

  We could also check user_era > 0 for eras profiling, which would remove the need
  for so many includes.
*/
#define SET_PROF_HDR(c, ccs_) \
  { \
    (c)->header.prof.ccs = ccs_; \
    if (doingLDVProfiling()) { \
      LDV_RECORD_CREATE((c)); \
    } else if (doingRetainerProfiling()) { \
      (c)->header.prof.hp.trav = 0; \
    } else if (doingErasProfiling()){ \
      ERA_RECORD_CREATE((c)); \
    } \
  }

#else
#define SET_PROF_HDR(c,ccs)
#endif

#define SET_HDR(c,_info,ccs)                            \
   {                                                    \
        SET_PROF_HDR((StgClosure *)(c),ccs);            \
        RELAXED_STORE(&(c)->header.info, _info);        \
   }

#define SET_HDR_RELEASE(c,_info,ccs)                    \
   {                                                    \
        SET_PROF_HDR((StgClosure *)(c),ccs);            \
        RELEASE_STORE(&(c)->header.info, _info);        \
   }

#define SET_ARR_HDR(c,info,costCentreStack,n_bytes)     \
   (c)->bytes = n_bytes;                                \
   SET_HDR(c,info,costCentreStack);

// Use when changing a closure from one kind to another
#define OVERWRITE_INFO(c, new_info)                             \
    OVERWRITING_CLOSURE((StgClosure *)(c));                     \
    SET_INFO_RELAXED((StgClosure *)(c), (new_info));                    \
    /* MP: Should this be SET_PROF_HEADER?  */ \
    LDV_RECORD_CREATE(c);

/* -----------------------------------------------------------------------------
   How to get hold of the static link field for a static closure.
   -------------------------------------------------------------------------- */

/* These are hard-coded. */
#define THUNK_STATIC_LINK(p) (&(p)->payload[1])
#define IND_STATIC_LINK(p)   (&(p)->payload[1])

EXTERN_INLINE StgClosure **STATIC_LINK(const StgInfoTable *info, StgClosure *p);
EXTERN_INLINE StgClosure **
STATIC_LINK(const StgInfoTable *info, StgClosure *p)
{
    switch (info->type) {
    case THUNK_STATIC:
        return THUNK_STATIC_LINK(p);
    case IND_STATIC:
        return IND_STATIC_LINK(p);
    default:
        return &p->payload[info->layout.payload.ptrs +
                           info->layout.payload.nptrs];
    }
}

/* -----------------------------------------------------------------------------
   INTLIKE and CHARLIKE closures.
   -------------------------------------------------------------------------- */

EXTERN_INLINE P_ CHARLIKE_CLOSURE(int n);
EXTERN_INLINE P_ CHARLIKE_CLOSURE(int n) {
    return (P_)&stg_CHARLIKE_closure[(n)-MIN_CHARLIKE];
}
EXTERN_INLINE P_ INTLIKE_CLOSURE(int n);
EXTERN_INLINE P_ INTLIKE_CLOSURE(int n) {
    return (P_)&stg_INTLIKE_closure[(n)-MIN_INTLIKE];
}

/* -----------------------------------------------------------------------------
   Forwarding pointers
   -------------------------------------------------------------------------- */

#define IS_FORWARDING_PTR(p) ((((StgWord)p) & 1) != 0)
#define MK_FORWARDING_PTR(p) (((StgWord)p) | 1)
#define UN_FORWARDING_PTR(p) (((StgWord)p) - 1)

/*
 * Note [Debugging predicates for pointers]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * LOOKS_LIKE_PTR(p)         returns False if p is definitely not a valid pointer
 * LOOKS_LIKE_INFO_PTR(p)    returns False if p is definitely not an info ptr
 * LOOKS_LIKE_CLOSURE_PTR(p) returns False if p is definitely not a closure ptr
 *
 * These macros are complete but not sound.  That is, they might
 * return false positives.  Do not rely on them to distinguish info
 * pointers from closure pointers, for example.
 *
 * We for the most part don't use address-space predicates these days, for
 * portability reasons, and the fact that code/data can be scattered about the
 * address space in a dynamically-linked environment.  Our best option is to
 * look at the alleged info table and see whether it seems to make sense.
 *
 * The one exception here is the use of INVALID_GHC_POINTER, which catches
 * the bit-pattern used by `+RTS -DZ` to zero freed memory (that is 0xaaaaa...).
 * In the case of most 64-bit platforms, this INVALID_GHC_POINTER is a
 * kernel-mode address, making this check free of false-negatives. On the other
 * hand, on 32-bit platforms this typically isn't the case. Consequently, we
 * only use this check in the DEBUG RTS.
 */

EXTERN_INLINE bool LOOKS_LIKE_PTR (const void* p);
EXTERN_INLINE bool LOOKS_LIKE_PTR (const void* p)
{
    return p && (p != (const void*) INVALID_GHC_POINTER);
}

EXTERN_INLINE bool LOOKS_LIKE_INFO_PTR_NOT_NULL (StgWord p);
EXTERN_INLINE bool LOOKS_LIKE_INFO_PTR_NOT_NULL (StgWord p)
{
    StgInfoTable *info = INFO_PTR_TO_STRUCT((StgInfoTable *)p);
    return info->type != INVALID_OBJECT && info->type < N_CLOSURE_TYPES;
}

EXTERN_INLINE bool LOOKS_LIKE_INFO_PTR (StgWord p);
EXTERN_INLINE bool LOOKS_LIKE_INFO_PTR (StgWord p)
{
    return LOOKS_LIKE_PTR((const void*) p) && (IS_FORWARDING_PTR(p) || LOOKS_LIKE_INFO_PTR_NOT_NULL(p));
}

EXTERN_INLINE bool LOOKS_LIKE_CLOSURE_PTR (const void *p);
EXTERN_INLINE bool LOOKS_LIKE_CLOSURE_PTR (const void *p)
{
    if (!LOOKS_LIKE_PTR(p)) return false;
    const StgInfoTable *info = RELAXED_LOAD(&UNTAG_CONST_CLOSURE((const StgClosure *) (p))->header.info);
    return LOOKS_LIKE_INFO_PTR((StgWord) info);
}

/* -----------------------------------------------------------------------------
   Macros for calculating the size of a closure
   -------------------------------------------------------------------------- */

EXTERN_INLINE StgOffset PAP_sizeW   ( uint32_t n_args );
EXTERN_INLINE StgOffset PAP_sizeW   ( uint32_t n_args )
{ return sizeofW(StgPAP) + n_args; }

EXTERN_INLINE StgOffset AP_sizeW   ( uint32_t n_args );
EXTERN_INLINE StgOffset AP_sizeW   ( uint32_t n_args )
{ return sizeofW(StgAP) + n_args; }

EXTERN_INLINE StgOffset AP_STACK_sizeW ( uint32_t size );
EXTERN_INLINE StgOffset AP_STACK_sizeW ( uint32_t size )
{ return sizeofW(StgAP_STACK) + size; }

EXTERN_INLINE StgWord CONTINUATION_sizeW(StgWord stack_size);
EXTERN_INLINE StgWord CONTINUATION_sizeW(StgWord stack_size)
{ return sizeofW(StgContinuation) + stack_size; }

EXTERN_INLINE StgOffset CONSTR_sizeW( uint32_t p, uint32_t np );
EXTERN_INLINE StgOffset CONSTR_sizeW( uint32_t p, uint32_t np )
{ return sizeofW(StgHeader) + p + np; }

EXTERN_INLINE StgOffset THUNK_SELECTOR_sizeW ( void );
EXTERN_INLINE StgOffset THUNK_SELECTOR_sizeW ( void )
{ return sizeofW(StgSelector); }

EXTERN_INLINE StgOffset BLACKHOLE_sizeW ( void );
EXTERN_INLINE StgOffset BLACKHOLE_sizeW ( void )
{ return sizeofW(StgInd); } // a BLACKHOLE is a kind of indirection

/* -----------------------------------------------------------------------------
   Blackhole predicates
   -------------------------------------------------------------------------- */

#define IS_BLACKHOLE_INFO(info) \
    ((info) == &stg_BLACKHOLE_info || \
     (info) == &__stg_EAGER_BLACKHOLE_info || \
     (info) == &stg_CAF_BLACKHOLE_info)

#define IS_BLACKHOLE_OR_WHITEHOLE_INFO(info) \
    (IS_BLACKHOLE_INFO(info) || (info) == &stg_WHITEHOLE_info)

/* --------------------------------------------------------------------------
   Sizes of closures
   ------------------------------------------------------------------------*/

EXTERN_INLINE StgOffset sizeW_fromITBL( const StgInfoTable* itbl );
EXTERN_INLINE StgOffset sizeW_fromITBL( const StgInfoTable* itbl )
{ return sizeofW(StgClosure)
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

EXTERN_INLINE StgOffset thunk_sizeW_fromITBL( const StgInfoTable* itbl );
EXTERN_INLINE StgOffset thunk_sizeW_fromITBL( const StgInfoTable* itbl )
{ return sizeofW(StgThunk)
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

EXTERN_INLINE StgOffset ap_stack_sizeW( StgAP_STACK* x );
EXTERN_INLINE StgOffset ap_stack_sizeW( StgAP_STACK* x )
{ return AP_STACK_sizeW(x->size); }

EXTERN_INLINE StgOffset ap_sizeW( StgAP* x );
EXTERN_INLINE StgOffset ap_sizeW( StgAP* x )
{ return AP_sizeW(x->n_args); }

EXTERN_INLINE StgOffset pap_sizeW( StgPAP* x );
EXTERN_INLINE StgOffset pap_sizeW( StgPAP* x )
{ return PAP_sizeW(x->n_args); }

EXTERN_INLINE StgWord continuation_sizeW(StgContinuation *x);
EXTERN_INLINE StgWord continuation_sizeW(StgContinuation *x)
{ return CONTINUATION_sizeW(x->stack_size); }

EXTERN_INLINE StgWord arr_words_words( StgArrBytes* x);
EXTERN_INLINE StgWord arr_words_words( StgArrBytes* x)
{ return ROUNDUP_BYTES_TO_WDS(x->bytes); }

EXTERN_INLINE StgOffset arr_words_sizeW( StgArrBytes* x );
EXTERN_INLINE StgOffset arr_words_sizeW( StgArrBytes* x )
{ return sizeofW(StgArrBytes) + arr_words_words(x); }

EXTERN_INLINE StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x );
EXTERN_INLINE StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x )
{ return sizeofW(StgMutArrPtrs) + x->size; }

EXTERN_INLINE StgOffset small_mut_arr_ptrs_sizeW( StgSmallMutArrPtrs* x );
EXTERN_INLINE StgOffset small_mut_arr_ptrs_sizeW( StgSmallMutArrPtrs* x )
{ return sizeofW(StgSmallMutArrPtrs) + x->ptrs; }

EXTERN_INLINE StgWord stack_sizeW ( StgStack *stack );
EXTERN_INLINE StgWord stack_sizeW ( StgStack *stack )
{ return sizeofW(StgStack) + stack->stack_size; }

EXTERN_INLINE StgWord bco_sizeW ( StgBCO *bco );
EXTERN_INLINE StgWord bco_sizeW ( StgBCO *bco )
{ return bco->size; }

EXTERN_INLINE StgWord compact_nfdata_full_sizeW ( StgCompactNFData *str );
EXTERN_INLINE StgWord compact_nfdata_full_sizeW ( StgCompactNFData *str )
{ return str->totalW; }

/*
 * TODO: Consider to switch return type from 'uint32_t' to 'StgWord' #8742
 *
 * (Also for 'closure_sizeW' below)
 */
uint32_t
closure_sizeW_ (const StgClosure *p, const StgInfoTable *info);

// The definitive way to find the size, in words, of a heap-allocated closure
EXTERN_INLINE uint32_t closure_sizeW (const StgClosure *p);
EXTERN_INLINE uint32_t closure_sizeW (const StgClosure *p)
{
    return closure_sizeW_(p, get_itbl(p));
}

/* -----------------------------------------------------------------------------
   Sizes of stack frames
   -------------------------------------------------------------------------- */

EXTERN_INLINE StgWord stack_frame_sizeW( StgClosure *frame );
EXTERN_INLINE StgWord stack_frame_sizeW( StgClosure *frame )
{
    const StgRetInfoTable *info;

    info = get_ret_itbl(frame);
    switch (info->i.type) {

    case RET_FUN:
        return sizeofW(StgRetFun) + ((StgRetFun *)frame)->size;

    case RET_BIG:
        return 1 + GET_LARGE_BITMAP(&info->i)->size;

    case RET_BCO:
        return 2 + BCO_BITMAP_SIZE((StgBCO *)((P_)frame)[1]);

    default:
        return 1 + BITMAP_SIZE(info->i.layout.bitmap);
    }
}

/* -----------------------------------------------------------------------------
   StgMutArrPtrs macros

   An StgMutArrPtrs has a card table to indicate which elements are
   dirty for the generational GC.  The card table is an array of
   bytes, where each byte covers (1 << MUT_ARR_PTRS_CARD_BITS)
   elements.  The card table is directly after the array data itself.
   -------------------------------------------------------------------------- */

// The number of card bytes needed
EXTERN_INLINE W_ mutArrPtrsCards (W_ elems);
EXTERN_INLINE W_ mutArrPtrsCards (W_ elems)
{
    return (W_)((elems + (1 << MUT_ARR_PTRS_CARD_BITS) - 1)
                           >> MUT_ARR_PTRS_CARD_BITS);
}

// The number of words in the card table
EXTERN_INLINE W_ mutArrPtrsCardTableSize (W_ elems);
EXTERN_INLINE W_ mutArrPtrsCardTableSize (W_ elems)
{
    return ROUNDUP_BYTES_TO_WDS(mutArrPtrsCards(elems));
}

// The address of the card for a particular card number
EXTERN_INLINE StgWord8 *mutArrPtrsCard (StgMutArrPtrs *a, W_ n);
EXTERN_INLINE StgWord8 *mutArrPtrsCard (StgMutArrPtrs *a, W_ n)
{
    return ((StgWord8 *)&(a->payload[a->ptrs]) + n);
}

/* -----------------------------------------------------------------------------
   Replacing a closure with a different one.  We must call
   OVERWRITING_CLOSURE(p) on the old closure that is about to be
   overwritten.
 */

 /*
   Note [marking slop when overwriting immutable closures]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   When we overwrite a closure in the heap with a smaller one, we need to mark
   the "slop" -- the memory that is left unoccupied -- so that the heap can be
   linearly scanned. See Note [slop on the heap]

   For mutable closures (e.g. shrinking arrays), slop is always marked
   unconditionally via writeSlopMarker, in all build modes.
   See Note [shrink-array slop marker] in PrimOps.cmm.

   For immutable closures (e.g. thunks overwritten with indirections), slop
   marking is only needed for:

    - full-heap sanity checks (DEBUG, and +RTS -DS),

    - LDV profiling (PROFILING, and +RTS -hb)

   However we can get into trouble if we're marking slop for immutable closures
   when using multiple threads, since there is nothing preventing another thread
   from still being in the process of reading the memory we're about to
   overwrite.

   Thus, with the THREADED RTS and +RTS -N2 or greater we must not mark
   immutable closure's slop. Similarly, the concurrent GC's mark thread
   may race with a mutator during slop marking. Consequently, we also disable
   marking of immutable closures when the non-moving GC is in use.

   Hence, an immutable closure's slop is marked when either:

    - PROFILING && era > 0 (LDV is on) && !nonmoving-gc-enabled or
    - !THREADED && DEBUG

   Additionally:

    - LDV profiling and +RTS -N2 are incompatible,

    - full-heap sanity checks are disabled for the THREADED RTS, at least when
      they don't run right after GC when there is no slop.
      See Note [heap sanity checking with SMP].

   -------------------------------------------------------------------------- */

#if defined(PROFILING) || defined(DEBUG)
#define OVERWRITING_CLOSURE(c) \
    overwritingClosure(c)
#define OVERWRITING_CLOSURE_SIZE(c, size) \
    overwritingClosureSize(c, size)
#else
#define OVERWRITING_CLOSURE(c) \
    do { (void) sizeof(c); } while(0)
#define OVERWRITING_CLOSURE_SIZE(c, size) \
    do { (void) sizeof(c); (void) sizeof(size); } while(0)
#endif

#if defined(PROFILING)
void LDV_recordDead (const StgClosure *c, uint32_t size);
RTS_PRIVATE bool isInherentlyUsed ( StgHalfWord closure_type );
#endif

// Note [Slop marker memory ordering]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The non-moving GC mark thread reads SmallMutArrPtrs payload elements
// concurrently with the mutator, which may shrink the array via
// stg_shrinkSmallMutableArrayzh.  Shrinking writes a slop marker over the
// vacated elements (see Note [shrink-array slop marker] in PrimOps.cmm):
//
//   n == 1:  slop[0] = 0
//   n >= 2:  slop[0] = -1   (sentinel)
//            slop[1] = n-2  (count of further slop words)
//
// The mark thread reads elements with ACQUIRE_LOAD and, at each position i>0,
// re-reads element i-1 after reading i to detect a concurrently written -1
// sentinel (see rts/sm/NonMovingMark.c).  For this check to be sound the
// store of -1 to slop[0] must be visible to the reader when it observes the
// skip count at slop[1].  This is guaranteed by the release-acquire pairing:
// the RELEASE_STORE of n-2 to slop[1] ensures that the prior RELAXED_STORE
// of -1 to slop[0] is visible to any thread that performs an ACQUIRE_LOAD of
// slop[1] and sees the skip count.
//
// The re-read of element i-1 is only performed when the value c just read at
// position i could plausibly be a skip count, i.e. when (StgWord)c < n-i
// (a skip count at position i must satisfy i + skip <= n-1, so skip < n-i).
// Values at or above that bound are valid closure pointers, so no re-read is
// needed.  In practice closure pointers are word-aligned kernel addresses and
// far exceed any plausible skip count, so this eliminates virtually all of
// the redundant re-reads.
//
// The non-moving GC only runs in the threaded RTS, where RELEASE_STORE and
// ACQUIRE_LOAD are both __atomic_* operations.  The _ALWAYS variants are not
// needed here.
INLINE_HEADER void writeSlopMarker(StgWord *slop, StgWord n)
{
    // See Note [Slop marker memory ordering]
    if (n == 1) {
        RELAXED_STORE(&slop[0], (StgWord)0);
    } else if (n >= 2) {
        RELAXED_STORE(&slop[0], (StgWord)(-1));
        RELEASE_STORE(&slop[1], n - 2);
    }
}

// Note [Skipping slop when scanning the heap]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Every linear scan of the heap (heap census, LDV census, sanity checker,
// findPtr) has to step over slop between closures.  Slop comes in two forms:
//
//  1. Zero words: alignment padding for large/pinned objects, zeroed
//     explicitly (see MEMSET_SLOP_W in allocatePinned).
//
//  2. Slop markers: written by writeSlopMarker whenever a closure is
//     overwritten by a smaller one.  See Note [shrink-array slop marker]
//     in PrimOps.cmm for the encoding.
//
// A closure can be shrunk repeatedly, leaving consecutive slop regions, so we
// must loop until reaching a word that can't be slop.  Info pointers are never
// 0 or (StgWord)(-1), so such a word starts the next closure.
//
// All scanners must use skipSlop: if any one of them keeps its own copy of
// this loop it will go out of sync with the encoding (#27585).
INLINE_HEADER StgPtr skipSlop(StgPtr p, StgPtr end)
{
    while (p < end) {
        if (!*p) {
            // single-word slop region, or alignment padding
            p++;
        } else if (*p == (StgWord)(-1)) {
            // Multi-word slop region: sentinel, count, then that many words.
            // writeSlopMarker only writes the sentinel for n >= 2, so the count
            // word is always within the region.  We assert rather than bail out
            // so that a corrupt heap is reported instead of silently skipped.
            ASSERT(p + 1 < end);
            p += 2 + *(p + 1);
        } else {
            break;
        }
    }
    return p;
}

INLINE_HEADER void
markImmutableSlop (StgClosure *p,
          uint32_t offset,    /*< offset to start marking at, in words */
          uint32_t size       /*< total closure size, in words */
         )
{
    // see Note [marking slop when overwriting immutable closures], also #8402

    const bool want_to_mark = false
        // Sanity checking (-DS) is enabled
        || RTS_DEREF(RtsFlags).DebugFlags.sanity
#if defined(PROFILING)
        // LDV profiler is enabled
        || era > 0
#endif
        ;

    const bool can_mark =
        // Only if we're running single threaded.
        getNumCapabilities() == 1
        && !RTS_DEREF(RtsFlags).GcFlags.useNonmoving; // see #23170

    if(!(want_to_mark && can_mark))
        return;

    // Write a slop marker so that the heap profiler and sanity checker can skip
    // over the slop without reading stale heap pointers.
    // See Note [shrink-array slop marker] in PrimOps.cmm for the encoding.
    writeSlopMarker((StgWord *)p + offset, size - offset);
}

// N.B. the stg_* variants of the utilities below are only for calling from
// Cmm. The INLINE_HEADER functions should be used when in C.
void stg_writeSlopMarker (StgWord *slop, StgWord n);
void stg_overwritingClosure (StgClosure *p);
INLINE_HEADER void overwritingClosure (StgClosure *p)
{
    W_ size = closure_sizeW(p);
#if defined(PROFILING)
    if(era > 0 && !isInherentlyUsed(get_itbl(p)->type))
        LDV_recordDead(p, size);
#endif
    markImmutableSlop(p, sizeofW(StgThunkHeader), size);
}


// Version of 'overwritingClosure' which takes closure size as argument.
void stg_overwritingClosureSize (StgClosure *p, uint32_t size /* in words */);
INLINE_HEADER void overwritingClosureSize (StgClosure *p, uint32_t size)
{
    // This function is only called from stg_AP_STACK so we can assume it's not
    // inherently used.
#if defined(PROFILING)
    ASSERT(isInherentlyUsed(get_itbl(p)->type) == false);
    if(era > 0)
        LDV_recordDead(p, size);
#endif
    markImmutableSlop(p, sizeofW(StgThunkHeader), size);
}
