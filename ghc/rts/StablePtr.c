/* -----------------------------------------------------------------------------
 * $Id: StablePtr.c,v 1.2 1998/12/02 13:28:48 simonm Exp $
 *
 * Stable pointers
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StablePtr.h"
#include "GC.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "RtsAPI.h"
#include "RtsFlags.h"

/* Comment from ADR's implementation in old RTS:

  This files (together with @ghc/runtime/storage/PerformIO.lhc@ and a
  small change in @HpOverflow.lc@) consists of the changes in the
  runtime system required to implement "Stable Pointers". But we're
  getting a bit ahead of ourselves --- what is a stable pointer and what
  is it used for?

  When Haskell calls C, it normally just passes over primitive integers,
  floats, bools, strings, etc.  This doesn't cause any problems at all
  for garbage collection because the act of passing them makes a copy
  from the heap, stack or wherever they are onto the C-world stack.
  However, if we were to pass a heap object such as a (Haskell) @String@
  and a garbage collection occured before we finished using it, we'd run
  into problems since the heap object might have been moved or even
  deleted.

  So, if a C call is able to cause a garbage collection or we want to
  store a pointer to a heap object between C calls, we must be careful
  when passing heap objects. Our solution is to keep a table of all
  objects we've given to the C-world and to make sure that the garbage
  collector collects these objects --- updating the table as required to
  make sure we can still find the object.


  Of course, all this rather begs the question: why would we want to
  pass a boxed value?

  One very good reason is to preserve laziness across the language
  interface. Rather than evaluating an integer or a string because it
  {\em might\/} be required by the C function, we can wait until the C
  function actually wants the value and then force an evaluation.

  Another very good reason (the motivating reason!) is that the C code
  might want to execute an object of sort $IO ()$ for the side-effects
  it will produce. For example, this is used when interfacing to an X
  widgets library to allow a direct implementation of callbacks.


  The @makeStablePointer :: a -> IO (StablePtr a)@ function
  converts a value into a stable pointer.  It is part of the @PrimIO@
  monad, because we want to be sure we don't allocate one twice by
  accident, and then only free one of the copies.

  \begin{verbatim}
  makeStablePtr#  :: a -> State# RealWorld -> (# RealWorld, a #)
  freeStablePtr#  :: StablePtr# a -> State# RealWorld -> State# RealWorld
  deRefStablePtr# :: StablePtr# a -> State# RealWorld -> 
        (# State# RealWorld, a #)
  \end{verbatim}
  There is also a C procedure @FreeStablePtr@ which frees a stable pointer.

  There may be additional functions on the C side to allow evaluation,
  application, etc of a stable pointer.

  When Haskell calls C, it normally just passes over primitive integers,
  floats, bools, strings, etc.  This doesn't cause any problems at all
  for garbage collection because the act of passing them makes a copy
  from the heap, stack or wherever they are onto the C-world stack.
  However, if we were to pass a heap object such as a (Haskell) @String@
  and a garbage collection occured before we finished using it, we'd run
  into problems since the heap object might have been moved or even
  deleted.

  So, if a C call is able to cause a garbage collection or we want to
  store a pointer to a heap object between C calls, we must be careful
  when passing heap objects. Our solution is to keep a table of all
  objects we've given to the C-world and to make sure that the garbage
  collector collects these objects --- updating the table as required to
  make sure we can still find the object.
*/


StgPtr *stable_ptr_table;
StgPtr *stable_ptr_free;

static nat SPT_size;

#define INIT_SPT_SIZE 64

static inline void
initFreeList(StgPtr *table, nat n, StgPtr *free)
{
  StgPtr *p;

  for (p = table + n - 1; p >= table; p--) {
    *p = (P_)free;
    free = p;
  }
  stable_ptr_free = table;
}

void
initStablePtrTable(void)
{
  SPT_size = INIT_SPT_SIZE;
  stable_ptr_table = stgMallocWords(SPT_size, "initStablePtrTable");

  initFreeList(stable_ptr_table,INIT_SPT_SIZE,NULL);
}

void
enlargeStablePtrTable(void)
{
  nat old_SPT_size = SPT_size;
  
  SPT_size *= 2;
  stable_ptr_table = stgReallocWords(stable_ptr_table, SPT_size, 
				     "enlargeStablePtrTable");
  
  initFreeList(stable_ptr_table + old_SPT_size, old_SPT_size, NULL);
}

void
markStablePtrTable(void)
{
  StgPtr *p, q, *end_stable_ptr_table;
  
  end_stable_ptr_table = &stable_ptr_table[SPT_size];

  for (p = stable_ptr_table; p < end_stable_ptr_table; p++) {
    q = *p;
    /* internal pointers or NULL are free slots */
    if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {
      (StgClosure *)*p = MarkRoot((StgClosure *)q);
    }
  }
}

/* -----------------------------------------------------------------------------
   performIO

   This is a useful function for calling from C land (or Haskell land
   with _ccall_GC) which runs an arbitrary Haskell IO computation in a
   new thread.

   The closure to evaluate is passed in as a stable pointer, and
   should have type StablePtr (IO ()).  No checking is done on the
   type, so be careful!

   The thread will be run in the context of the existing system;
   ie. running threads will continue to run etc.
   -------------------------------------------------------------------------- */

void
performIO(StgStablePtr io)
{
  rts_evalIO((StgClosure *)deRefStablePointer(io), NULL);
}

