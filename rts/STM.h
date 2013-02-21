/*----------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * STM interface definition
 *
 *----------------------------------------------------------------------

  STM.h defines the C-level interface to the STM.  

  The design follows that of the PPoPP 2005 paper "Composable memory
  transactions" extended to include fine-grained locking of TVars.

  Three different implementations can be built.  In overview:
  
  STM_UNIPROC  -- no locking at all: not safe for concurrent invocations
 
  STM_CG_LOCK  -- coarse-grained locking : a single mutex protects all
                  TVars
 
  STM_FG_LOCKS -- per-TVar exclusion : each TVar can be owned by at
                  most one TRec at any time.  This allows dynamically
                  non-conflicting transactions to commit in parallel.
                  The implementation treats reads optimisitcally --
                  extra versioning information is retained in the 
                  saw_update_by field of the TVars so that they do not 
                  need to be locked for reading.

  STM.C contains more details about the locking schemes used.

*/

#ifndef STM_H
#define STM_H

#ifdef THREADED_RTS
//#define STM_CG_LOCK
#define STM_FG_LOCKS
#else
#define STM_UNIPROC
#endif

#include "BeginPrivate.h"

/*----------------------------------------------------------------------

   GC interaction
   --------------
*/

void stmPreGCHook(Capability *cap);

/*----------------------------------------------------------------------

   Transaction context management
   ------------------------------

*/

/* Create and enter a new transaction context */

StgTRecHeader *stmStartTransaction(Capability *cap, StgTRecHeader *outer);
StgTRecHeader *stmStartNestedTransaction(Capability *cap, StgTRecHeader *outer
);

/*
 * Roll back the current transatcion context.  NB: if this is a nested tx
 * then we merge its read set into its parents.  This is because a change
 * to that read set could change whether or not the tx should abort.
 */

void stmAbortTransaction(Capability *cap, StgTRecHeader *trec);
void stmFreeAbortedTRec(Capability *cap, StgTRecHeader *trec);

/*
 * Ensure that a subsequent commit / validation will fail.  We use this 
 * in our current handling of transactions that may have become invalid
 * and started looping.  We strip their stack back to the ATOMICALLY_FRAME,
 * and, when the thread is next scheduled, discover it to be invalid and
 * re-execute it.  However, we need to force the transaction to stay invalid
 * in case other threads' updates make it valid in the mean time.
 */

void stmCondemnTransaction(Capability *cap, StgTRecHeader *trec);

/*----------------------------------------------------------------------

   Validation
   ----------

  Test whether the specified transaction record, and all those within which
  it is nested, are still valid.

  Note: the caller can assume that once stmValidateTransaction has
  returned FALSE for a given trec then that transaction will never
  again be valid -- we rely on this in Schedule.c when kicking invalid
  threads at GC (in case they are stuck looping)
*/

StgBool stmValidateNestOfTransactions(Capability *cap, StgTRecHeader *trec);

/*----------------------------------------------------------------------

   Commit/wait/rewait operations
   -----------------------------

   These four operations return boolean results which should be interpreted
   as follows:

   true  => The transaction record was definitely valid 

   false => The transaction record may not have been valid

   Note that, for nested operations, validity here is solely in terms
   of the specified trec: it does not say whether those that it may be
   nested are themselves valid.  Callers can check this with 
   stmValidateNestOfTransactions.

   The user of the STM should ensure that it is always safe to assume that a
   transaction context is not valid when in fact it is (i.e. to return false in
   place of true, with side-effects as defined below).  This may cause
   needless retries of transactions (in the case of validate and commit), or it
   may cause needless spinning instead of blocking (in the case of wait and
   rewait).

   In defining the behaviour of wait and rewait we distinguish between two
   different aspects of a thread's runnability:

    - We say that a thread is "blocked" when it is not running or
      runnable as far as the scheduler is concerned.

    - We say that a thread is "waiting" when its StgTRecHeader is linked on an
      tvar's wait queue.

   Considering only STM operations, (blocked) => (waiting).  The user of the STM
   should ensure that they are prepared for threads to be unblocked spuriously
   and for wait/reWait to return false even when the previous transaction context
   is actually still valid.
*/

/*
 * Fill in the trec's list of invariants that might be violated by the current
 * transaction.  
 */

StgInvariantCheckQueue *stmGetInvariantsToCheck(Capability *cap, 
                                                StgTRecHeader *trec);

void stmAddInvariantToCheck(Capability *cap, 
                            StgTRecHeader *trec,
                            StgClosure *code);

/*
 * Test whether the current transaction context is valid and, if so,
 * commit its memory accesses to the heap.  stmCommitTransaction must
 * unblock any threads which are waiting on tvars that updates have
 * been committed to.
 */

StgBool stmCommitTransaction(Capability *cap, StgTRecHeader *trec);
StgBool stmCommitNestedTransaction(Capability *cap, StgTRecHeader *trec);

/*
 * Test whether the current transaction context is valid and, if so,
 * start the thread waiting for updates to any of the tvars it has
 * ready from and mark it as blocked.  It is an error to call stmWait
 * if the thread is already waiting.  
 */

StgBool stmWait(Capability *cap, StgTSO *tso, StgTRecHeader *trec);

void stmWaitUnlock(Capability *cap, StgTRecHeader *trec);

/*
 * Test whether the current transaction context is valid and, if so,
 * leave the thread waiting and mark it as blocked again.  If the
 * transaction context is no longer valid then stop the thread waiting
 * and leave it as unblocked.  It is an error to call stmReWait if the
 * thread is not waiting.
 */

StgBool stmReWait(Capability *cap, StgTSO *tso);

/*----------------------------------------------------------------------

   Data access operations
   ----------------------
*/

/*
 * Return the logical contents of 'tvar' within the context of the
 * thread's current transaction.
 */

StgClosure *stmReadTVar(Capability *cap,
                        StgTRecHeader *trec, 
                        StgTVar *tvar);

/* Update the logical contents of 'tvar' within the context of the
 * thread's current transaction.
 */

void stmWriteTVar(Capability *cap,
                  StgTRecHeader *trec,
                  StgTVar *tvar, 
                  StgClosure *new_value);

/*----------------------------------------------------------------------*/

/* NULLs */

#define END_STM_WATCH_QUEUE ((StgTVarWatchQueue *)(void *)&stg_END_STM_WATCH_QUEUE_closure)
#define END_INVARIANT_CHECK_QUEUE ((StgInvariantCheckQueue *)(void *)&stg_END_INVARIANT_CHECK_QUEUE_closure)
#define END_STM_CHUNK_LIST ((StgTRecChunk *)(void *)&stg_END_STM_CHUNK_LIST_closure)

#define NO_TREC ((StgTRecHeader *)(void *)&stg_NO_TREC_closure)

/*----------------------------------------------------------------------*/

#include "EndPrivate.h"

#endif /* STM_H */

