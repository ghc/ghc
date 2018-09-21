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

INLINE_HEADER void SET_INFO(StgClosure *c, const StgInfoTable *info) {
    c->header.info = info;
}
INLINE_HEADER const StgInfoTable *GET_INFO(StgClosure *c) {
    return c->header.info;
}

#define GET_ENTRY(c)  (ENTRY_CODE(GET_INFO(c)))

#if defined(TABLES_NEXT_TO_CODE)
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info);
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgInfoTable *)info - 1;}
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info);
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgRetInfoTable *)info - 1;}
INLINE_HEADER StgFunInfoTable *FUN_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgFunInfoTable *)info - 1;}
INLINE_HEADER StgThunkInfoTable *THUNK_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgThunkInfoTable *)info - 1;}
INLINE_HEADER StgConInfoTable *CON_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgConInfoTable *)info - 1;}
INLINE_HEADER StgFunInfoTable *itbl_to_fun_itbl(const StgInfoTable *i) {return (StgFunInfoTable *)(i + 1) - 1;}
INLINE_HEADER StgRetInfoTable *itbl_to_ret_itbl(const StgInfoTable *i) {return (StgRetInfoTable *)(i + 1) - 1;}
INLINE_HEADER StgThunkInfoTable *itbl_to_thunk_itbl(const StgInfoTable *i) {return (StgThunkInfoTable *)(i + 1) - 1;}
INLINE_HEADER StgConInfoTable *itbl_to_con_itbl(const StgInfoTable *i) {return (StgConInfoTable *)(i + 1) - 1;}
#else
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info);
EXTERN_INLINE StgInfoTable *INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgInfoTable *)info;}
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info);
EXTERN_INLINE StgRetInfoTable *RET_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgRetInfoTable *)info;}
INLINE_HEADER StgFunInfoTable *FUN_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgFunInfoTable *)info;}
INLINE_HEADER StgThunkInfoTable *THUNK_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgThunkInfoTable *)info;}
INLINE_HEADER StgConInfoTable *CON_INFO_PTR_TO_STRUCT(const StgInfoTable *info) {return (StgConInfoTable *)info;}
INLINE_HEADER StgFunInfoTable *itbl_to_fun_itbl(const StgInfoTable *i) {return (StgFunInfoTable *)i;}
INLINE_HEADER StgRetInfoTable *itbl_to_ret_itbl(const StgInfoTable *i) {return (StgRetInfoTable *)i;}
INLINE_HEADER StgThunkInfoTable *itbl_to_thunk_itbl(const StgInfoTable *i) {return (StgThunkInfoTable *)i;}
INLINE_HEADER StgConInfoTable *itbl_to_con_itbl(const StgInfoTable *i) {return (StgConInfoTable *)i;}
#endif

EXTERN_INLINE const StgInfoTable *get_itbl(const StgClosure *c);
EXTERN_INLINE const StgInfoTable *get_itbl(const StgClosure *c)
{
   return INFO_PTR_TO_STRUCT(c->header.info);
}

EXTERN_INLINE const StgRetInfoTable *get_ret_itbl(const StgClosure *c);
EXTERN_INLINE const StgRetInfoTable *get_ret_itbl(const StgClosure *c)
{
   return RET_INFO_PTR_TO_STRUCT(c->header.info);
}

INLINE_HEADER const StgFunInfoTable *get_fun_itbl(const StgClosure *c)
{
   return FUN_INFO_PTR_TO_STRUCT(c->header.info);
}

INLINE_HEADER const StgThunkInfoTable *get_thunk_itbl(const StgClosure *c)
{
   return THUNK_INFO_PTR_TO_STRUCT(c->header.info);
}

INLINE_HEADER const StgConInfoTable *get_con_itbl(const StgClosure *c)
{
   return CON_INFO_PTR_TO_STRUCT((c)->header.info);
}

INLINE_HEADER StgHalfWord GET_TAG(const StgClosure *con)
{
    return get_itbl(con)->srt;
}

/* -----------------------------------------------------------------------------
   Macros for building closures
   -------------------------------------------------------------------------- */

#if defined(PROFILING)
#if defined(DEBUG_RETAINER)
/*
  For the sake of debugging, we take the safest way for the moment. Actually, this
  is useful to check the sanity of heap before beginning retainer profiling.
  flip is defined in RetainerProfile.c, and declared as extern in RetainerProfile.h.
  Note: change those functions building Haskell objects from C datatypes, i.e.,
  all rts_mk???() functions in RtsAPI.c, as well.
 */
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_, (c)->header.prof.hp.rs = (retainerSet *)((StgWord)NULL | flip))
#else
/*
  For retainer profiling only: we do not have to set (c)->header.prof.hp.rs to
  NULL | flip (flip is defined in RetainerProfile.c) because even when flip
  is 1, rs is invalid and will be initialized to NULL | flip later when
  the closure *c is visited.
 */
/*
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_, (c)->header.prof.hp.rs = NULL)
 */
/*
  The following macro works for both retainer profiling and LDV profiling:
  for retainer profiling, ldvTime remains 0, so rs fields are initialized to 0.
  See the invariants on ldvTime.
 */
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_,   \
        LDV_RECORD_CREATE((c)))
#endif /* DEBUG_RETAINER */
#else
#define SET_PROF_HDR(c,ccs)
#endif

#define SET_HDR(c,_info,ccs)                            \
   {                                                    \
        (c)->header.info = _info;                       \
        SET_PROF_HDR((StgClosure *)(c),ccs);            \
   }

#define SET_ARR_HDR(c,info,costCentreStack,n_bytes)     \
   SET_HDR(c,info,costCentreStack);                     \
   (c)->bytes = n_bytes;

// Use when changing a closure from one kind to another
#define OVERWRITE_INFO(c, new_info)                             \
    OVERWRITING_CLOSURE((StgClosure *)(c));                     \
    SET_INFO((StgClosure *)(c), (new_info));                    \
    LDV_RECORD_CREATE(c);

/* -----------------------------------------------------------------------------
   How to get hold of the static link field for a static closure.
   -------------------------------------------------------------------------- */

/* These are hard-coded. */
#define THUNK_STATIC_LINK(p) (&(p)->payload[1])
#define IND_STATIC_LINK(p)   (&(p)->payload[1])

INLINE_HEADER StgClosure **
STATIC_LINK(const StgInfoTable *info, StgClosure *p)
{
    switch (info->type) {
    case THUNK_STATIC:
        return THUNK_STATIC_LINK(p);
    case IND_STATIC:
        return IND_STATIC_LINK(p);
    default:
        return &(p)->payload[info->layout.payload.ptrs +
                             info->layout.payload.nptrs];
    }
}

/* -----------------------------------------------------------------------------
   INTLIKE and CHARLIKE closures.
   -------------------------------------------------------------------------- */

INLINE_HEADER P_ CHARLIKE_CLOSURE(int n) {
    return (P_)&stg_CHARLIKE_closure[(n)-MIN_CHARLIKE];
}
INLINE_HEADER P_ INTLIKE_CLOSURE(int n) {
    return (P_)&stg_INTLIKE_closure[(n)-MIN_INTLIKE];
}

/* ----------------------------------------------------------------------------
   Macros for untagging and retagging closure pointers
   For more information look at the comments in Cmm.h
   ------------------------------------------------------------------------- */

static inline StgWord
GET_CLOSURE_TAG(const StgClosure * p)
{
    return (StgWord)p & TAG_MASK;
}

static inline StgClosure *
UNTAG_CLOSURE(StgClosure * p)
{
    return (StgClosure*)((StgWord)p & ~TAG_MASK);
}

static inline const StgClosure *
UNTAG_CONST_CLOSURE(const StgClosure * p)
{
    return (const StgClosure*)((StgWord)p & ~TAG_MASK);
}

static inline StgClosure *
TAG_CLOSURE(StgWord tag,StgClosure * p)
{
    return (StgClosure*)((StgWord)p | tag);
}

/* -----------------------------------------------------------------------------
   Forwarding pointers
   -------------------------------------------------------------------------- */

#define IS_FORWARDING_PTR(p) ((((StgWord)p) & 1) != 0)
#define MK_FORWARDING_PTR(p) (((StgWord)p) | 1)
#define UN_FORWARDING_PTR(p) (((StgWord)p) - 1)

/* -----------------------------------------------------------------------------
   DEBUGGING predicates for pointers

   LOOKS_LIKE_INFO_PTR(p)    returns False if p is definitely not an info ptr
   LOOKS_LIKE_CLOSURE_PTR(p) returns False if p is definitely not a closure ptr

   These macros are complete but not sound.  That is, they might
   return false positives.  Do not rely on them to distinguish info
   pointers from closure pointers, for example.

   We don't use address-space predicates these days, for portability
   reasons, and the fact that code/data can be scattered about the
   address space in a dynamically-linked environment.  Our best option
   is to look at the alleged info table and see whether it seems to
   make sense...
   -------------------------------------------------------------------------- */

INLINE_HEADER bool LOOKS_LIKE_INFO_PTR_NOT_NULL (StgWord p)
{
    StgInfoTable *info = INFO_PTR_TO_STRUCT((StgInfoTable *)p);
    return info->type != INVALID_OBJECT && info->type < N_CLOSURE_TYPES;
}

INLINE_HEADER bool LOOKS_LIKE_INFO_PTR (StgWord p)
{
    return p && (IS_FORWARDING_PTR(p) || LOOKS_LIKE_INFO_PTR_NOT_NULL(p));
}

INLINE_HEADER bool LOOKS_LIKE_CLOSURE_PTR (const void *p)
{
    return LOOKS_LIKE_INFO_PTR((StgWord)
            (UNTAG_CONST_CLOSURE((const StgClosure *)(p)))->header.info);
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

EXTERN_INLINE StgOffset CONSTR_sizeW( uint32_t p, uint32_t np );
EXTERN_INLINE StgOffset CONSTR_sizeW( uint32_t p, uint32_t np )
{ return sizeofW(StgHeader) + p + np; }

EXTERN_INLINE StgOffset THUNK_SELECTOR_sizeW ( void );
EXTERN_INLINE StgOffset THUNK_SELECTOR_sizeW ( void )
{ return sizeofW(StgSelector); }

EXTERN_INLINE StgOffset BLACKHOLE_sizeW ( void );
EXTERN_INLINE StgOffset BLACKHOLE_sizeW ( void )
{ return sizeofW(StgInd); } // a BLACKHOLE is a kind of indirection

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
EXTERN_INLINE uint32_t
closure_sizeW_ (const StgClosure *p, const StgInfoTable *info);
EXTERN_INLINE uint32_t
closure_sizeW_ (const StgClosure *p, const StgInfoTable *info)
{
    switch (info->type) {
    case THUNK_0_1:
    case THUNK_1_0:
        return sizeofW(StgThunk) + 1;
    case FUN_0_1:
    case CONSTR_0_1:
    case FUN_1_0:
    case CONSTR_1_0:
        return sizeofW(StgHeader) + 1;
    case THUNK_0_2:
    case THUNK_1_1:
    case THUNK_2_0:
        return sizeofW(StgThunk) + 2;
    case FUN_0_2:
    case CONSTR_0_2:
    case FUN_1_1:
    case CONSTR_1_1:
    case FUN_2_0:
    case CONSTR_2_0:
        return sizeofW(StgHeader) + 2;
    case THUNK:
        return thunk_sizeW_fromITBL(info);
    case THUNK_SELECTOR:
        return THUNK_SELECTOR_sizeW();
    case AP_STACK:
        return ap_stack_sizeW((StgAP_STACK *)p);
    case AP:
        return ap_sizeW((StgAP *)p);
    case PAP:
        return pap_sizeW((StgPAP *)p);
    case IND:
        return sizeofW(StgInd);
    case ARR_WORDS:
        return arr_words_sizeW((StgArrBytes *)p);
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        return small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
    case TSO:
        return sizeofW(StgTSO);
    case STACK:
        return stack_sizeW((StgStack*)p);
    case BCO:
        return bco_sizeW((StgBCO *)p);
    case TREC_CHUNK:
        return sizeofW(StgTRecChunk);
    default:
        return sizeW_fromITBL(info);
    }
}

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
INLINE_HEADER W_ mutArrPtrsCards (W_ elems)
{
    return (W_)((elems + (1 << MUT_ARR_PTRS_CARD_BITS) - 1)
                           >> MUT_ARR_PTRS_CARD_BITS);
}

// The number of words in the card table
INLINE_HEADER W_ mutArrPtrsCardTableSize (W_ elems)
{
    return ROUNDUP_BYTES_TO_WDS(mutArrPtrsCards(elems));
}

// The address of the card for a particular card number
INLINE_HEADER StgWord8 *mutArrPtrsCard (StgMutArrPtrs *a, W_ n)
{
    return ((StgWord8 *)&(a->payload[a->ptrs]) + n);
}

/* -----------------------------------------------------------------------------
   Replacing a closure with a different one.  We must call
   OVERWRITING_CLOSURE(p) on the old closure that is about to be
   overwritten.

   Note [zeroing slop]

   In some scenarios we write zero words into "slop"; memory that is
   left unoccupied after we overwrite a closure in the heap with a
   smaller closure.

   Zeroing slop is required for:

    - full-heap sanity checks (DEBUG, and +RTS -DS)
    - LDV profiling (PROFILING, and +RTS -hb)

   Zeroing slop must be disabled for:

    - THREADED_RTS with +RTS -N2 and greater, because we cannot
      overwrite slop when another thread might be reading it.

   Hence, slop is zeroed when either:

    - PROFILING && era <= 0 (LDV is on)
    - !THREADED_RTS && DEBUG

   And additionally:

    - LDV profiling and +RTS -N2 are incompatible
    - full-heap sanity checks are disabled for THREADED_RTS

   -------------------------------------------------------------------------- */

#if defined(PROFILING)
#define ZERO_SLOP_FOR_LDV_PROF 1
#else
#define ZERO_SLOP_FOR_LDV_PROF 0
#endif

#if defined(DEBUG) && !defined(THREADED_RTS)
#define ZERO_SLOP_FOR_SANITY_CHECK 1
#else
#define ZERO_SLOP_FOR_SANITY_CHECK 0
#endif

#if ZERO_SLOP_FOR_LDV_PROF || ZERO_SLOP_FOR_SANITY_CHECK
#define OVERWRITING_CLOSURE(c) overwritingClosure(c)
#define OVERWRITING_CLOSURE_OFS(c,n) overwritingClosureOfs(c,n)
#else
#define OVERWRITING_CLOSURE(c) /* nothing */
#define OVERWRITING_CLOSURE_OFS(c,n) /* nothing */
#endif

#if defined(PROFILING)
void LDV_recordDead (const StgClosure *c, uint32_t size);
#endif

EXTERN_INLINE void overwritingClosure_ (StgClosure *p,
                                        uint32_t offset /* in words */,
                                        uint32_t size /* closure size, in words */);
EXTERN_INLINE void overwritingClosure_ (StgClosure *p, uint32_t offset, uint32_t size)
{
#if ZERO_SLOP_FOR_LDV_PROF && !ZERO_SLOP_FOR_SANITY_CHECK
    // see Note [zeroing slop], also #8402
    if (era <= 0) return;
#endif

    // For LDV profiling, we need to record the closure as dead
#if defined(PROFILING)
    LDV_recordDead(p, size);
#endif

    for (uint32_t i = offset; i < size; i++) {
        ((StgWord *)p)[i] = 0;
    }
}

EXTERN_INLINE void overwritingClosure (StgClosure *p);
EXTERN_INLINE void overwritingClosure (StgClosure *p)
{
    overwritingClosure_(p, sizeofW(StgThunkHeader), closure_sizeW(p));
}

// Version of 'overwritingClosure' which overwrites only a suffix of a
// closure.  The offset is expressed in words relative to 'p' and shall
// be less than or equal to closure_sizeW(p), and usually at least as
// large as the respective thunk header.
//
// Note: As this calls LDV_recordDead() you have to call LDV_RECORD()
//       on the final state of the closure at the call-site
EXTERN_INLINE void overwritingClosureOfs (StgClosure *p, uint32_t offset);
EXTERN_INLINE void overwritingClosureOfs (StgClosure *p, uint32_t offset)
{
    overwritingClosure_(p, offset, closure_sizeW(p));
}

// Version of 'overwritingClosure' which takes closure size as argument.
EXTERN_INLINE void overwritingClosureSize (StgClosure *p, uint32_t size /* in words */);
EXTERN_INLINE void overwritingClosureSize (StgClosure *p, uint32_t size)
{
    overwritingClosure_(p, sizeofW(StgThunkHeader), size);
}
