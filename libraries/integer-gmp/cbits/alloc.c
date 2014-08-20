/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2012
 *
 * ---------------------------------------------------------------------------*/

#include <string.h>

#include "Rts.h"

#include "gmp.h"

void * stgAllocForGMP   (size_t size_in_bytes);
void * stgReallocForGMP (void *ptr, size_t old_size, size_t new_size);
void   stgDeallocForGMP (void *ptr STG_UNUSED, size_t size STG_UNUSED);

static void initAllocForGMP( void ) __attribute__((constructor));

/* -----------------------------------------------------------------------------
   Tell GMP to use our custom heap allocation functions.

   Our allocation strategy is to use GHC heap allocations rather than malloc
   and co. The heap objects we use are ByteArray#s which of course have their
   usual header word or two. But gmp doesn't know about ghc heap objects and
   header words. So our allocator has to make a ByteArray# and return a pointer
   to its interior! When the gmp function returns we receive that interior
   pointer. Then we look back a couple words to get the proper ByteArray#
   pointer (which then gets returned as a ByteArray# and thus get tracked
   properly by the GC).

   WARNING!! WARNING!! WARNING!!

     It is absolutely vital that this initialisation function be called before
     any of the gmp functions are called. We'd still be looking back a couple
     words for the ByteArray# header, but if we were accidentally using malloc
     then it'd all go wrong because of course there would be no ByteArray#
     header, just malloc's own internal book keeping info. To make things worse
     we would not notice immediately, it'd only be when the GC comes round to
     inspect things... BANG!

     > Program received signal SIGSEGV, Segmentation fault.
     > [Switching to Thread 0x7f5a9ebc76f0 (LWP 17838)]
     > evacuate1 (p=0x7f5a99acd2e0) at rts/sm/Evac.c:375
     > 375       switch (info->type) {

   -------------------------------------------------------------------------- */

static void initAllocForGMP( void )
{
  mp_set_memory_functions(stgAllocForGMP, stgReallocForGMP, stgDeallocForGMP);
}


/* -----------------------------------------------------------------------------
   Allocation functions for GMP.

   These all use the allocate() interface - we can't have any garbage
   collection going on during a gmp operation, so we use allocate()
   which always succeeds.  The gmp operations which might need to
   allocate will ask the storage manager (via doYouWantToGC()) whether
   a garbage collection is required, in case we get into a loop doing
   only allocate() style allocation.
   -------------------------------------------------------------------------- */

void *
stgAllocForGMP (size_t size_in_bytes)
{
  StgArrWords* arr;
  nat data_size_in_words, total_size_in_words;
  Capability *cap;

  /* round up to a whole number of words */
  data_size_in_words  = ROUNDUP_BYTES_TO_WDS(size_in_bytes);
  total_size_in_words = sizeofW(StgArrWords) + data_size_in_words;

  /* allocate and fill it in. */
  cap = rts_unsafeGetMyCapability();
  arr = (StgArrWords *)allocate(cap, total_size_in_words);
  SET_ARR_HDR(arr, &stg_ARR_WORDS_info, ((CapabilityPublic*)cap)->r.rCCCS, size_in_bytes);

  /* and return a ptr to the goods inside the array */
  return arr->payload;
}

void *
stgReallocForGMP (void *ptr, size_t old_size, size_t new_size)
{
  size_t min_size = old_size < new_size ? old_size : new_size;

  return memcpy(stgAllocForGMP(new_size), ptr, min_size);
}

void
stgDeallocForGMP (void *ptr STG_UNUSED, size_t size STG_UNUSED)
{
    /* easy for us: the garbage collector does the dealloc'n */
}
