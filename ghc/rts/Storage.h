/* -----------------------------------------------------------------------------
 * $Id: Storage.h,v 1.24 2001/01/26 14:36:40 simonpj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGE_H
#define STORAGE_H

#include "Block.h"
#include "BlockAlloc.h"
#include "StoragePriv.h"

/* -----------------------------------------------------------------------------
   Initialisation / De-initialisation
   -------------------------------------------------------------------------- */

extern void initStorage(void);
extern void exitStorage(void);

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocate(int n)       Allocates a chunk of contiguous store
   				n words long, returning a pointer to
				the first word.  Always succeeds.
				
				Don't forget to TICK_ALLOC_XXX(...)
				after calling allocate, for the
				benefit of the ticky-ticky profiler.

   rtsBool doYouWantToGC(void)  Returns True if the storage manager is
   				ready to perform a GC, False otherwise.

   lnat  allocated_bytes(void)  Returns the number of bytes allocated
                                via allocate() since the last GC.
				Used in the reoprting of statistics.

   SMP: allocate and doYouWantToGC can be used from STG code, they are
   surrounded by a mutex.
   -------------------------------------------------------------------------- */

extern StgPtr  allocate(nat n);
static inline rtsBool doYouWantToGC(void)
{
  return (alloc_blocks >= alloc_blocks_lim);
}
extern lnat allocated_bytes(void);

/* -----------------------------------------------------------------------------
   ExtendNursery(hp,hplim)      When hplim is reached, try to grab
   				some more allocation space.  Returns
				False if the allocation space is
				exhausted, and the application should
				call GarbageCollect().
  -------------------------------------------------------------------------- */

#define ExtendNursery(hp,hplim)			\
  (CurrentNursery->free = (P_)(hp)+1,		\
   CurrentNursery->link == NULL ? rtsFalse :	\
   (CurrentNursery = CurrentNursery->link,	\
    OpenNursery(hp,hplim),			\
    rtsTrue))

extern void PleaseStopAllocating(void);

/* -----------------------------------------------------------------------------
   Performing Garbage Collection

   GarbageCollect(get_roots)    Performs a garbage collection.  
				'get_roots' is called to find all the 
				roots that the system knows about.

   StgClosure 			Called by get_roots on each root.	
   MarkRoot(StgClosure *p)	Returns the new location of the root.
   -------------------------------------------------------------------------- */

extern void   GarbageCollect(void (*get_roots)(void),rtsBool force_major_gc);
extern StgClosure *MarkRoot(StgClosure *p);

/* Temporary measure to ensure we retain all the dynamically-loaded CAFs */
#ifdef GHCI
extern void markCafs( void );
#endif

/* -----------------------------------------------------------------------------
   Generational garbage collection support

   recordMutable(StgPtr p)       Informs the garbage collector that a
				 previously immutable object has
				 become (permanently) mutable.  Used
				 by thawArray and similar.

   updateWithIndirection(p1,p2)  Updates the object at p1 with an
				 indirection pointing to p2.  This is
				 normally called for objects in an old
				 generation (>0) when they are updated.

   updateWithPermIndirection(p1,p2)  As above but uses a permanent indir.

   -------------------------------------------------------------------------- */

/* ToDo: shouldn't recordMutable and recordOldToNewPtrs acquire some
 * kind of lock in the SMP case?
 */
static inline void
recordMutable(StgMutClosure *p)
{
  bdescr *bd;

#ifdef SMP
  ASSERT(p->header.info == &stg_WHITEHOLE_info || closure_MUTABLE(p));
#else
  ASSERT(closure_MUTABLE(p));
#endif

  bd = Bdescr((P_)p);
  if (bd->gen->no > 0) {
    p->mut_link = bd->gen->mut_list;
    bd->gen->mut_list = p;
  }
}

static inline void
recordOldToNewPtrs(StgMutClosure *p)
{
  bdescr *bd;
  
  bd = Bdescr((P_)p);
  if (bd->gen->no > 0) {
    p->mut_link = bd->gen->mut_once_list;
    bd->gen->mut_once_list = p;
  }
}

#ifndef DEBUG
#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    bd = Bdescr((P_)p1);						\
    if (bd->gen->no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&stg_IND_info);					\
      TICK_UPD_NEW_IND();						\
    } else {								\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      if (info != &stg_BLACKHOLE_BQ_info) {				\
        ACQUIRE_LOCK(&sm_mutex);					\
        ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;	\
        bd->gen->mut_once_list = (StgMutClosure *)p1;			\
        RELEASE_LOCK(&sm_mutex);					\
      }									\
      SET_INFO(p1,&stg_IND_OLDGEN_info);				\
      TICK_UPD_OLD_IND();						\
    }									\
  }
#else

/* In the DEBUG case, we also zero out the slop of the old closure,
 * so that the sanity checker can tell where the next closure is.
 */
#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    bd = Bdescr((P_)p1);						\
    if (bd->gen->no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&stg_IND_info);					\
      TICK_UPD_NEW_IND();						\
    } else {								\
      if (info != &stg_BLACKHOLE_BQ_info) {				\
	{ 								\
          StgInfoTable *inf = get_itbl(p1);				\
	  nat np = inf->layout.payload.ptrs,				\
	      nw = inf->layout.payload.nptrs, i;			\
	  for (i = np; i < np + nw; i++) {				\
	     ((StgClosure *)p1)->payload[i] = 0;			\
          }								\
        }								\
        ACQUIRE_LOCK(&sm_mutex);					\
        ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;	\
        bd->gen->mut_once_list = (StgMutClosure *)p1;			\
        RELEASE_LOCK(&sm_mutex);					\
      }									\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      SET_INFO(p1,&stg_IND_OLDGEN_info);				\
      TICK_UPD_OLD_IND();						\
    }									\
  }
#endif

/* Static objects all live in the oldest generation
 */
#define updateWithStaticIndirection(info, p1, p2)			\
  {									\
    ASSERT( ((StgMutClosure*)p1)->mut_link == NULL );			\
									\
    ACQUIRE_LOCK(&sm_mutex);						\
    ((StgMutClosure *)p1)->mut_link = oldest_gen->mut_once_list;	\
    oldest_gen->mut_once_list = (StgMutClosure *)p1;			\
    RELEASE_LOCK(&sm_mutex);						\
									\
    ((StgInd *)p1)->indirectee = p2;					\
    SET_INFO((StgInd *)p1, &stg_IND_STATIC_info);			\
    TICK_UPD_STATIC_IND();						\
  }

#if defined(TICKY_TICKY) || defined(PROFILING)
static inline void
updateWithPermIndirection(const StgInfoTable *info, StgClosure *p1, StgClosure *p2) 
{
  bdescr *bd;

  bd = Bdescr((P_)p1);
  if (bd->gen->no == 0) {
    ((StgInd *)p1)->indirectee = p2;
    SET_INFO(p1,&stg_IND_PERM_info);
    TICK_UPD_NEW_PERM_IND(p1);
  } else {
    ((StgIndOldGen *)p1)->indirectee = p2;
    if (info != &stg_BLACKHOLE_BQ_info) {
      ACQUIRE_LOCK(&sm_mutex);
      ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;
      bd->gen->mut_once_list = (StgMutClosure *)p1;
      RELEASE_LOCK(&sm_mutex);
    }
    SET_INFO(p1,&stg_IND_OLDGEN_PERM_info);
    TICK_UPD_OLD_PERM_IND();
  }
}
#endif

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs
   -------------------------------------------------------------------------- */

#if defined(INTERPRETER)
typedef struct StgCAFTabEntry_ {
    StgClosure*   closure;
    StgInfoTable* origItbl;
} StgCAFTabEntry;

extern void addToECafTable ( StgClosure* closure, StgInfoTable* origItbl );
extern void clearECafTable ( void );

extern StgCAF*         ecafList;
extern StgCAFTabEntry* ecafTable;
extern StgInt          usedECafTable;
extern StgInt          sizeECafTable;
#endif

#if defined(DEBUG)
void printMutOnceList(generation *gen);
void printMutableList(generation *gen);
#endif DEBUG

/* --------------------------------------------------------------------------
                      Address space layout macros
   --------------------------------------------------------------------------

   Here are the assumptions GHC makes about address space layout.
   Broadly, it thinks there are three sections:

     CODE    Read-only.  Contains code and read-only data (such as
                info tables)
             Also called "text"

     DATA    Read-write data.  Contains static closures (and on some
                architectures, info tables too)

     HEAP    Dynamically-allocated closures

     USER    None of the above.  The only way USER things arise right 
             now is when GHCi allocates a constructor info table, which
	     it does by mallocing them.

   Three macros identify these three areas:
     IS_CODE(p), IS_DATA(p), HEAP_ALLOCED(p)

   HEAP_ALLOCED is called FOR EVERY SINGLE CLOSURE during GC.
   It needs to be FAST.

   Implementation of HEAP_ALLOCED
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Concerning HEAP, most of the time (certainly under [Static] and [GHCi],
   we ensure that the heap is allocated above some fixed address HEAP_BASE
   (defined in MBlock.h).  In this case we set TEXT_BEFORE_HEAP, and we
   get a nice fast test.

   Sometimes we can't be quite sure.  For example in Windows, we can't 
   fix where our heap address space comes from.  In this case we un-set 
   TEXT_BEFORE_HEAP. That makes it more expensive to test whether a pointer
   comes from the HEAP section, because we need to look at the allocator's
   address maps (see HEAP_ALLOCED macro)

   Implementation of CODE and DATA
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Concerning CODE and DATA, there are three main regimes:

     [Static] Totally      The segments are contiguous, and laid out 
     statically linked     exactly as above

     [GHCi] Static,        GHCi may load new modules, but it knows the
     except for GHCi       address map, so for any given address it can
                           still tell which section it belongs to

     [DLL] OS-supported    Chunks of CODE and DATA may be mixed in 
     dynamic loading       the address space, and we can't tell how


   For the [Static] case, we assume memory is laid out like this
   (in order of increasing addresses)

       Start of memory
           CODE section
       TEXT_SECTION_END_MARKER   (usually _etext)
           DATA section
       DATA_SECTION_END_MARKER   (usually _end)
           USER section
       HEAP_BASE
           HEAP section

   For the [GHCi] case, we have to consult GHCi's dynamic linker's
   address maps, which is done by macros
         is_dynamically_loaded_code_or_rodata_ptr
         is_dynamically_loaded_code_or_rwdata_ptr

   For the [DLL] case, IS_CODE and IS_DATA are really not usable at all.
 */


#undef TEXT_BEFORE_HEAP
#ifndef mingw32_TARGET_OS
#define TEXT_BEFORE_HEAP 1
#endif

extern void* TEXT_SECTION_END_MARKER_DECL;
extern void* DATA_SECTION_END_MARKER_DECL;

#if defined(INTERPRETER) || defined(GHCI)
/* Take into account code sections in dynamically loaded object files. */
#define IS_CODE_PTR(p) (  ((P_)(p) < (P_)&TEXT_SECTION_END_MARKER) \
                       || is_dynamically_loaded_code_or_rodata_ptr((char *)p) )
#define IS_DATA_PTR(p) ( ((P_)(p) >= (P_)&TEXT_SECTION_END_MARKER && \
                          (P_)(p) < (P_)&DATA_SECTION_END_MARKER) \
                       || is_dynamically_loaded_rwdata_ptr((char *)p) )
#define IS_USER_PTR(p) ( ((P_)(p) >= (P_)&DATA_SECTION_END_MARKER) \
                       && is_not_dynamically_loaded_ptr((char *)p) )
#else
#define IS_CODE_PTR(p) ((P_)(p) < (P_)&TEXT_SECTION_END_MARKER)
#define IS_DATA_PTR(p) ((P_)(p) >= (P_)&TEXT_SECTION_END_MARKER && (P_)(p) < (P_)&DATA_SECTION_END_MARKER)
#define IS_USER_PTR(p) ((P_)(p) >= (P_)&DATA_SECTION_END_MARKER)
#endif

/* The HEAP_ALLOCED test below is called FOR EVERY SINGLE CLOSURE
 * during GC.  It needs to be FAST.
 *
 * BEWARE: when we're dynamically loading code (for GHCi), make sure
 * that we don't load any code above HEAP_BASE, or this test won't work.
 */
#ifdef TEXT_BEFORE_HEAP
# define HEAP_ALLOCED(x)  ((StgPtr)(x) >= (StgPtr)(HEAP_BASE))
#else
extern int is_heap_alloced(const void* x);
# define HEAP_ALLOCED(x)  (is_heap_alloced(x))
#endif


/* --------------------------------------------------------------------------
   Macros for distinguishing data pointers from code pointers
   --------------------------------------------------------------------------

  Specification
  ~~~~~~~~~~~~~
  The garbage collector needs to make some critical distinctions between pointers.
  In particular we need
 
     LOOKS_LIKE_GHC_INFO(p)          p points to an info table

  For both of these macros, p is
      *either* a pointer to a closure (static or heap allocated)
      *or* a return address on the (Haskell) stack

  (Return addresses are in fact info-pointers, so that the Haskell stack
  looks very like a chunk of heap.)

  The garbage collector uses LOOKS_LIKE_GHC_INFO when walking the stack, as it
  walks over the "pending arguments" on its way to the next return address.
  It is called moderately often, but not as often as HEAP_ALLOCED

  ToDo: LOOKS_LIKE_GHC_INFO(p) does not return True when p points to a
  constructor info table allocated by GHCi.  We should really rename 
  LOOKS_LIKE_GHC_INFO to LOOKS_LIKE_GHC_RETURN_INFO.

  Implementation
  ~~~~~~~~~~~~~~
  LOOKS_LIKE_GHC_INFO is more complicated because of the need to distinguish 
  between static closures and info tables.  It's a known portability problem.
  We have three approaches:

  Plan A: Address-space partitioning.  
    Keep info tables in the (single, contiguous) text segment:    IS_CODE_PTR(p)
    and static closures in the (single, contiguous) data segment: IS_DATA_PTR(p)

  Plan A can fail for two reasons:
    * In many environments (eg. dynamic loading),
      text and data aren't in a single contiguous range.  
    * When we compile through vanilla C (no mangling) we sometimes
      can't guaranteee to put info tables in the text section.  This
      happens eg. on MacOS where the C compiler refuses to put const
      data in the text section if it has any code pointers in it
      (which info tables do *only* when we're compiling without
      TABLES_NEXT_TO_CODE).
    
  Hence, Plan B: (compile-via-C-with-mangling, or native code generation)
    Put a zero word before each static closure.
    When compiling to native code, or via C-with-mangling, info tables
    are laid out "backwards" from the address specified in the info pointer
    (the entry code goes forward from the info pointer).  Hence, the word
    before the one referenced the info pointer is part of the info table,
    and is guaranteed non-zero.

    For reasons nobody seems to fully understand, the statically-allocated tables
    of INTLIKE and CHARLIKE closures can't have this zero word, so we
    have to test separately for them.

    Plan B fails altogether for the compile-through-vanilla-C route, because
    info tables aren't laid out backwards.


  Hence, Plan C: (unregisterised, compile-through-vanilla-C route only)
    If we didn't manage to get info tables into the text section, then
    we can distinguish between a static closure pointer and an info
    pointer as follows:  the first word of an info table is a code pointer,
    and therefore in text space, whereas the first word of a closure pointer
    is an info pointer, and therefore not.  Shazam!
*/


/* When working with Win32 DLLs, static closures are identified by
   being prefixed with a zero word. This is needed so that we can
   distinguish between pointers to static closures and (reversed!)
   info tables.

   This 'scheme' breaks down for closure tables such as CHARLIKE,
   so we catch these separately.
  
   LOOKS_LIKE_STATIC_CLOSURE() 
       - discriminates between static closures and info tbls
         (needed by LOOKS_LIKE_GHC_INFO() below - [Win32 DLLs only.])
   LOOKS_LIKE_STATIC() 
       - distinguishes between static and heap allocated data.
 */
#if defined(ENABLE_WIN32_DLL_SUPPORT) && !defined(INTERPRETER)
            /* definitely do not enable for mingw DietHEP */
#define LOOKS_LIKE_STATIC(r) (!(HEAP_ALLOCED(r)))

/* Tiresome predicates needed to check for pointers into the closure tables */
#define IS_CHARLIKE_CLOSURE(p) \
    ( (P_)(p) >= (P_)stg_CHARLIKE_closure && \
      (char*)(p) <= ((char*)stg_CHARLIKE_closure + \
                     (MAX_CHARLIKE-MIN_CHARLIKE) * sizeof(StgIntCharlikeClosure)) )
#define IS_INTLIKE_CLOSURE(p) \
    ( (P_)(p) >= (P_)stg_INTLIKE_closure && \
      (char*)(p) <= ((char*)stg_INTLIKE_closure + \
                     (MAX_INTLIKE-MIN_INTLIKE) * sizeof(StgIntCharlikeClosure)) )

#define LOOKS_LIKE_STATIC_CLOSURE(r) (((*(((unsigned long *)(r))-1)) == 0) || IS_CHARLIKE_CLOSURE(r) || IS_INTLIKE_CLOSURE(r))
#else
#define LOOKS_LIKE_STATIC(r) IS_DATA_PTR(r)
#define LOOKS_LIKE_STATIC_CLOSURE(r) IS_DATA_PTR(r)
#endif


/* -----------------------------------------------------------------------------
   Macros for distinguishing infotables from closures.
   
   You'd think it'd be easy to tell an info pointer from a closure pointer:
   closures live on the heap and infotables are in read only memory.  Right?
   Wrong!  Static closures live in read only memory and Hugs allocates
   infotables for constructors on the (writable) C heap.
   -------------------------------------------------------------------------- */

#ifdef INTERPRETER
#  ifdef USE_MINIINTERPRETER
     /* yoiks: one of the dreaded pointer equality tests */
#    define IS_HUGS_CONSTR_INFO(info) \
            (((StgInfoTable *)(info))->entry == (StgFunPtr)&Hugs_CONSTR_entry)
#  else
#    define IS_HUGS_CONSTR_INFO(info) 0 /* ToDo: more than mildly bogus */
#  endif
#elif GHCI
   /* not accurate by any means, but stops the assertions failing... */
#  define IS_HUGS_CONSTR_INFO(info)  IS_USER_PTR(info)
#else
#  define IS_HUGS_CONSTR_INFO(info) 0 /* ToDo: more than mildly bogus */
#endif

/* LOOKS_LIKE_GHC_INFO is called moderately often during GC, but
 * Certainly not as often as HEAP_ALLOCED.
 */
#ifdef TEXT_BEFORE_HEAP /* needed for mingw DietHEP */
# define LOOKS_LIKE_GHC_INFO(info) IS_CODE_PTR(info)
#else
# define LOOKS_LIKE_GHC_INFO(info) (!HEAP_ALLOCED(info) \
                                    && !LOOKS_LIKE_STATIC_CLOSURE(info))
#endif


/* -----------------------------------------------------------------------------
   Macros for calculating how big a closure will be (used during allocation)
   -------------------------------------------------------------------------- */

/* ToDo: replace unsigned int by nat.  The only fly in the ointment is that
 * nat comes from Rts.h which many folk dont include.  Sigh!
 */
static __inline__ StgOffset AP_sizeW    ( unsigned int n_args )              
{ return sizeofW(StgAP_UPD) + n_args; }

static __inline__ StgOffset PAP_sizeW   ( unsigned int n_args )              
{ return sizeofW(StgPAP)    + n_args; }

static __inline__ StgOffset CONSTR_sizeW( unsigned int p, unsigned int np )  
{ return sizeofW(StgHeader) + p + np; }

static __inline__ StgOffset THUNK_SELECTOR_sizeW ( void )                    
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

static __inline__ StgOffset BLACKHOLE_sizeW ( void )                    
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

static __inline__ StgOffset CAF_sizeW ( void )                    
{ return sizeofW(StgCAF); }

/* --------------------------------------------------------------------------
 * Sizes of closures
 * ------------------------------------------------------------------------*/

static __inline__ StgOffset sizeW_fromITBL( const StgInfoTable* itbl ) 
{ return sizeofW(StgClosure) 
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

static __inline__ StgOffset pap_sizeW( StgPAP* x )
{ return PAP_sizeW(x->n_args); }

static __inline__ StgOffset arr_words_sizeW( StgArrWords* x )
{ return sizeofW(StgArrWords) + x->words; }

static __inline__ StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x )
{ return sizeofW(StgMutArrPtrs) + x->ptrs; }

static __inline__ StgWord tso_sizeW ( StgTSO *tso )
{ return TSO_STRUCT_SIZEW + tso->stack_size; }

#endif STORAGE_H

