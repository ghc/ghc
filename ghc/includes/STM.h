/*----------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * STM interface definition
 *
 *----------------------------------------------------------------------

  STM.h defines the C-level interface to the STM.  

  The interface is designed so that all of the operations return
  directly: if the specified StgTSO should block then the Haskell
  scheduler's data structures are updated within the STM
  implementation, rather than blocking the native thread.

  This interface can be supported by many different implementations,
  in particular it is left unspecified:

   - Whether nested transactions are fully supported.
 
     A simple implementation would count the number of
     stmStartTransaction operations that a thread invokes and only
     attempt to really commit it to the heap when the corresponding
     number of stmCommitTransaction calls have been made.  This
     prevents enclosed transactions from being aborted without also
     aborting all of the outer ones.  
 
     The current implementation does support proper nesting.

   - Whether stmWait and stmReWait are blocking.

     A simple implementation would always return 'false' from these
     operations, signalling that the calling thread should immediately
     retry its transaction.

     A fuller implementation would block the thread and return 'True'
     when it is safe for the thread to block.

     The current implementation does provide stmWait and stmReWait 
     operations which can block the caller's TSO.

   - Whether the transactional read, write, commit and validate
     operations are blocking or non-blocking.

     A simple implementation would use an internal lock to prevent
     concurrent execution of any STM operations.  (This does not
     prevent multiple threads having concurrent transactions, merely
     the concurrent execution of say stmCommitTransaction by two
     threads at the same time). 

     A fuller implementation would offer obstruction-free or lock-free
     progress guarantees, as in our OOPSLA 2003 paper.

     The current implementation is lock-free for simple uncontended
     operations, but uses an internal lock on SMP systems in some
     cases.  This aims to provide good performance on uniprocessors:
     it substantially streamlines the design, when compared with the
     OOPSLA paper, and on a uniprocessor we can be sure that threads
     are never pre-empted within STM operations.
*/

#ifndef STM_H
#define STM_H

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------------------------------------------------

   Start of day
   ------------

*/

extern void initSTM(void);

extern void stmPreGCHook(void);

/*----------------------------------------------------------------------

   Transaction context management
   ------------------------------

*/

/* Create and enter a new transaction context */

extern StgTRecHeader *stmStartTransaction(StgTRecHeader *outer);

/*
 * Exit the current transaction context, abandoning any read/write
 * operations performed within it and removing the thread from any
 * tvar wait queues if it was waitin.  Note that if nested transactions
 * are not fully supported then this may leave the enclosing
 * transaction contexts doomed to abort.
 */

extern void stmAbortTransaction(StgTRecHeader *trec);

/*
 * Ensure that a subsequent commit / validation will fail.  We use this 
 * in our current handling of transactions that may have become invalid
 * and started looping.  We strip their stack back to the ATOMICALLY_FRAME,
 * and, when the thread is next scheduled, discover it to be invalid and
 * re-execute it.  However, we need to force the transaction to stay invalid
 * in case other threads' updates make it valid in the mean time.
 */

extern void stmCondemnTransaction(StgTRecHeader *trec);

/*
 * Return the trec within which the specified trec was created (not
 * valid if trec==NO_TREC).
 */

extern StgTRecHeader *stmGetEnclosingTRec(StgTRecHeader *trec);

/*----------------------------------------------------------------------

   Validate/commit/wait/rewait operations
   --------------------------------------


   These four operations return boolean results which should be interpreted
   as follows:

   true  => The transaction context was definitely valid 

   false => The transaction context may not have been valid

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
 * Test whether the current transaction context is valid, i.e. whether
 * it is still possible for it to commit successfully.  Note: we assume that
 * once stmValidateTransaction has returned FALSE for a given transaction then
 * that transaction will never again be valid -- we rely on this in Schedule.c when
 * kicking invalid threads at GC (in case they are stuck looping)
 */

extern StgBool stmValidateTransaction(StgTRecHeader *trec);

/*
 * Test whether the current transaction context is valid and, if so,
 * commit its memory accesses to the heap.  stmCommitTransaction must
 * unblock any threads which are waiting on tvars that updates have
 * been committed to.
 */

extern StgBool stmCommitTransaction(StgTRecHeader *trec);

/*
 * Test whether the current transaction context is valid and, if so,
 * start the thread waiting for updates to any of the tvars it has
 * ready from and mark it as blocked.  It is an error to call stmWait
 * if the thread is already waiting.
 */

extern StgBool stmWait(StgTSO *tso, StgTRecHeader *trec);

/*
 * Test whether the current transaction context is valid and, if so,
 * leave the thread waiting and mark it as blocked again.  If the
 * transaction context is no longer valid then stop the thread waiting
 * and leave it as unblocked.  It is an error to call stmReWait if the
 * thread is not waiting.
 */

extern StgBool stmReWait(StgTSO *tso);

/*
 * Merge the accesses made so far in the second trec into the first trec.
 * Note that the resulting trec is only intended to be used in wait operations.
 * This avoids defining what happens if "trec" and "other" contain conflicting
 * updates.
 */

extern StgBool stmMergeForWaiting(StgTRecHeader *trec, StgTRecHeader *other);


/*----------------------------------------------------------------------

   Data access operations
   ----------------------
*/

/*
 * Return the logical contents of 'tvar' within the context of the
 * thread's current transaction.
 */

extern StgClosure *stmReadTVar(StgTRecHeader *trec, 
			       StgTVar *tvar);

/* Update the logical contents of 'tvar' within the context of the
 * thread's current transaction.
 */

extern void stmWriteTVar(StgTRecHeader *trec,
			 StgTVar *tvar, 
			 StgClosure *new_value);

/*----------------------------------------------------------------------*/

/* NULLs */

#define END_STM_WAIT_QUEUE ((StgTVarWaitQueue *)(void *)&stg_END_STM_WAIT_QUEUE_closure)
#define END_STM_CHUNK_LIST ((StgTRecChunk *)(void *)&stg_END_STM_CHUNK_LIST_closure)
#define NO_TREC ((StgTRecHeader *)(void *)&stg_NO_TREC_closure)

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif

#endif /* STM_H */

