/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2004
 * 
 * STM implementation.
 *
 * This implementation is designed for a many-threads, few-CPUs case.  This leads
 * to a number of design choices:
 *
 *  - We use a simple design which does not aim to be lock-free -- SMP builds use
 *    a mutex to protect all the TVars and STM datastructures, non-SMP builds 
 *    do not require any locking.  The goal is to make fast-path uncontended 
 *    operations fast because, with few CPUs, contention betwen operations on the 
 *    STM interface is expected rarely.
 *
 *  - Each thread is responsible for adding/removing itself to/from the queues
 *    associated with tvars.  This reduces the work that is necessary when a
 *    large number of threads are waiting on a single tvar and where the update
 *    to that tvar is really only releasing a single thread.
 *
 * Ideas for future experimentation:
 *
 *  - Read/write operations here involve a linear search of the trec.  Consider
 *    adding a cache to map tvars to existing entries in the trec.
 *
 *  - Consider whether to defer unparking more than one thread.  On a uniprocessor
 *    the deferment could be made until a thread switch from the first thread
 *    released in the hope that it restores the location to a value on which
 *    other threads were waiting.  That would avoid a stampede on e.g. multiple
 *    threads blocked reading from a single-cell shared buffer.
 *
 *  - Consider whether to provide a link from a StgTVarWaitQueue to the TRecEntry
 *    associated with the waiter.  This would allow unpark_waiters_on to be
 *    more selective and avoid unparking threads whose expected value for that 
 *    tvar co-incides with the value now stored there.  Does this happen often?
 *    
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "STM.h"
#include "Storage.h"

#include <stdlib.h>
#include <stdio.h>

#define FALSE 0
#define TRUE  1

#if defined(DEBUG)
#define SHAKE
#define TRACE(_x...) IF_DEBUG(stm, debugBelch ( _x ))
#else
#define TRACE(_x...) /*Nothing*/
#endif

// If SHAKE is defined then validation will sometime spuriously fail.  They helps test
// unusualy code paths if genuine contention is rare

#ifdef SHAKE
static const int do_shake = TRUE;
#else
static const int do_shake = FALSE;
#endif
static int shake_ctr = 0;

/*......................................................................*/

static int shake(void) {
  if (do_shake) {
    if (((shake_ctr++) % 47) == 0) {
      return TRUE;
    } 
    return FALSE;
  } else {
    return FALSE;
  }
}

/*......................................................................*/

// Helper macros for iterating over entries within a transaction
// record

#define FOR_EACH_ENTRY(_t,_x,CODE) do {		\
  StgTRecHeader *__t = (_t);			\
  StgTRecChunk *__c = __t -> current_chunk;		\
  StgWord __limit = __c -> next_entry_idx;		\
  TRACE("trec=%p chunk=%p limit=%ld\n", __t, __c, __limit); \
  while (__c != END_STM_CHUNK_LIST) {		\
    StgWord __i;                                  \
    for (__i = 0; __i < __limit; __i ++) {		\
      TRecEntry *_x = &(__c -> entries[__i]);	\
      do { CODE } while (0);  		        \
    }						\
    __c = __c -> prev_chunk;			\
    __limit = TREC_CHUNK_NUM_ENTRIES;		\
  }						\
 exit_for_each:                                 \
  if (FALSE) goto exit_for_each;                \
} while (0)

#define BREAK_FOR_EACH goto exit_for_each
     
/*......................................................................*/

// Private cache of must-be-unreachable trec headers and chunks

static StgTRecHeader *cached_trec_headers = NO_TREC;
static StgTRecChunk *cached_trec_chunks = END_STM_CHUNK_LIST;
static StgTVarWaitQueue *cached_tvar_wait_queues = END_STM_WAIT_QUEUE;

static void recycle_tvar_wait_queue(StgTVarWaitQueue *q STG_UNUSED) {
#if 0
  if (shake()) {
    TRACE("Shake: not re-using wait queue %p\n", q);
    return;
  }

  q -> next_queue_entry = cached_tvar_wait_queues;
  cached_tvar_wait_queues = q;
#endif
}

static void recycle_closures_from_trec (StgTRecHeader *t STG_UNUSED) {
#if 0
  if (shake()) {
    TRACE("Shake: not re-using closures from %p\n", t);
    return;
  }

  t -> enclosing_trec = cached_trec_headers;
  cached_trec_headers = t;
  t -> enclosing_trec = NO_TREC;

  while (t -> current_chunk != END_STM_CHUNK_LIST) {
    StgTRecChunk *c = t -> current_chunk;
    t -> current_chunk = c -> prev_chunk;
    c -> prev_chunk = cached_trec_chunks;
    cached_trec_chunks = c;
  }
#endif
}

/*......................................................................*/

// Helper functions for managing internal STM state.  This lock is only held
// for a 'short' time, in the sense that it is never held when any of the 
// external functions returns.

static void lock_stm(void) {
  // Nothing
}

static void unlock_stm(void) {
  // Nothing
}

/*......................................................................*/

// Helper functions for thread blocking and unblocking

static void park_tso(StgTSO *tso) {
  ASSERT(tso -> why_blocked == NotBlocked);
  tso -> why_blocked = BlockedOnSTM;
  tso -> block_info.closure = (StgClosure *) END_TSO_QUEUE;
  TRACE("park_tso on tso=%p\n", tso);
}

static void unpark_tso(StgTSO *tso) {
  // We will continue unparking threads while they remain on one of the wait
  // queues: it's up to the thread itself to remove it from the wait queues
  // if it decides to do so when it is scheduled.
  if (tso -> why_blocked == BlockedOnSTM) {
    TRACE("unpark_tso on tso=%p\n", tso);
    tso -> why_blocked = NotBlocked;
    PUSH_ON_RUN_QUEUE(tso);
  } else {
    TRACE("spurious unpark_tso on tso=%p\n", tso);
  }
}

static void unpark_waiters_on(StgTVar *s) {
  StgTVarWaitQueue *q;
  TRACE("unpark_waiters_on tvar=%p\n", s);
  for (q = s -> first_wait_queue_entry; 
       q != END_STM_WAIT_QUEUE; 
       q = q -> next_queue_entry) {
    unpark_tso(q -> waiting_tso);
  }
}

/*......................................................................*/

// Helper functions for allocation and initialization

static StgTVarWaitQueue *new_stg_tvar_wait_queue(StgTSO *waiting_tso) {
  StgTVarWaitQueue *result;
  if (cached_tvar_wait_queues != END_STM_WAIT_QUEUE) {
    result = cached_tvar_wait_queues;
    cached_tvar_wait_queues = result -> next_queue_entry;
  } else {
    result = (StgTVarWaitQueue *)allocate(sizeofW(StgTVarWaitQueue));
    SET_HDR (result, &stg_TVAR_WAIT_QUEUE_info, CCS_SYSTEM);
  }
  result -> waiting_tso = waiting_tso;
  return result;
}

static StgTRecChunk *new_stg_trec_chunk(void) {
  StgTRecChunk *result;
  if (cached_trec_chunks != END_STM_CHUNK_LIST) {
    result = cached_trec_chunks;
    cached_trec_chunks = result -> prev_chunk;
  } else {
    result = (StgTRecChunk *)allocate(sizeofW(StgTRecChunk));
    SET_HDR (result, &stg_TREC_CHUNK_info, CCS_SYSTEM);
  }
  result -> prev_chunk = END_STM_CHUNK_LIST;
  result -> next_entry_idx = 0;
  TRACE("prev from %p is %p\n", result, result -> prev_chunk);
  return result;
}

static StgTRecHeader *new_stg_trec_header(StgTRecHeader *enclosing_trec) {
  StgTRecHeader *result;
  if (cached_trec_headers != NO_TREC) {
    result = cached_trec_headers;
    cached_trec_headers = result -> enclosing_trec;
  } else {
    result = (StgTRecHeader *) allocate(sizeofW(StgTRecHeader));
    SET_HDR (result, &stg_TREC_HEADER_info, CCS_SYSTEM);
  }
  result -> enclosing_trec = enclosing_trec;
  result -> current_chunk = new_stg_trec_chunk();

  if (enclosing_trec == NO_TREC) {
    result -> state = TREC_ACTIVE;
  } else {
    ASSERT(enclosing_trec -> state == TREC_ACTIVE ||
           enclosing_trec -> state == TREC_MUST_ABORT ||
           enclosing_trec -> state == TREC_CANNOT_COMMIT);
    result -> state = enclosing_trec -> state;
  }

  TRACE("new_stg_trec_header creating %p nidx=%ld chunk=%p enclosing_trec=%p state=%d\n",
        result, result->current_chunk->next_entry_idx, result -> current_chunk, enclosing_trec, result->state);
  return result;  
}

/*......................................................................*/

// Helper functions for managing waiting lists

static void start_tso_waiting_on_trec(StgTSO *tso, StgTRecHeader *trec) {
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT(trec -> state == TREC_ACTIVE || trec -> state == TREC_CANNOT_COMMIT);
  FOR_EACH_ENTRY(trec, e, {
    StgTVar *s;
    StgTVarWaitQueue *q;
    StgTVarWaitQueue *fq;
    s = e -> tvar;
    TRACE("Adding tso=%p to wait queue for tvar=%p\n", tso, s);
    ASSERT(s -> current_value == e -> expected_value);
    fq = s -> first_wait_queue_entry;
    q = new_stg_tvar_wait_queue(tso);
    q -> next_queue_entry = fq;
    q -> prev_queue_entry = END_STM_WAIT_QUEUE;
    if (fq != END_STM_WAIT_QUEUE) {
      fq -> prev_queue_entry = q;
    }
    s -> first_wait_queue_entry = q;
    e -> new_value = (StgClosure *) q;
  });
}

static void stop_tsos_waiting_on_trec(StgTRecHeader *trec) {
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT(trec -> state == TREC_WAITING ||
         trec -> state == TREC_MUST_ABORT);
  TRACE("stop_tsos_waiting in state=%d\n", trec -> state);
  FOR_EACH_ENTRY(trec, e, {
    StgTVar *s;
    StgTVarWaitQueue *pq;
    StgTVarWaitQueue *nq;
    StgTVarWaitQueue *q;
    s = e -> tvar;
    q = (StgTVarWaitQueue *) (e -> new_value);
    TRACE("Removing tso=%p from wait queue for tvar=%p\n", q -> waiting_tso, s);
    nq = q -> next_queue_entry;
    pq = q -> prev_queue_entry;
    TRACE("pq=%p nq=%p q=%p\n", pq, nq, q);
    if (nq != END_STM_WAIT_QUEUE) {
      nq -> prev_queue_entry = pq;
    }
    if (pq != END_STM_WAIT_QUEUE) {
      pq -> next_queue_entry = nq;
    } else {
      ASSERT (s -> first_wait_queue_entry == q);
      s -> first_wait_queue_entry = nq;
    }
    recycle_tvar_wait_queue(q);
  });
}
 
/*......................................................................*/
 
static TRecEntry *get_new_entry(StgTRecHeader *t) {
  TRecEntry *result;
  StgTRecChunk *c;
  int i;

  c = t -> current_chunk;
  i = c -> next_entry_idx;
  ASSERT(c != END_STM_CHUNK_LIST);

  if (i < TREC_CHUNK_NUM_ENTRIES) {
    // Continue to use current chunk
    result = &(c -> entries[i]);
    c -> next_entry_idx ++;
  } else {
    // Current chunk is full: allocate a fresh one
    StgTRecChunk *nc;
    nc = new_stg_trec_chunk();
    nc -> prev_chunk = c;
    nc -> next_entry_idx = 1;
    t -> current_chunk = nc;
    result = &(nc -> entries[0]);
  }

  return result;
}

/*......................................................................*/

static void merge_update_into(StgTRecHeader *t,
                              StgTVar *tvar,
                              StgClosure *expected_value,
                              StgClosure *new_value,
                              int merging_sibling) {
  int found;
  
  // Look for an entry in this trec
  found = FALSE;
  FOR_EACH_ENTRY(t, e, {
    StgTVar *s;
    s = e -> tvar;
    if (s == tvar) {
      found = TRUE;
      if (merging_sibling) {
        if (e -> expected_value != expected_value) {
          // Must abort if the two entries start from different values
          TRACE("Siblings inconsistent at %p (%p vs %p)\n", 
                tvar, e -> expected_value, expected_value);
          t -> state = TREC_MUST_ABORT;
        } else if (e -> new_value != new_value) {
          // Cannot commit if the two entries lead to different values (wait still OK)
          TRACE("Siblings trying conflicting writes to %p (%p vs %p)\n", 
                tvar, e -> new_value, new_value);
          t -> state = TREC_CANNOT_COMMIT;
        }
      } else {
        // Otherwise merging child back into parent
        ASSERT (e -> new_value == expected_value);
      }
      TRACE("        trec=%p exp=%p new=%p\n", t, e->expected_value, e->new_value);
      e -> new_value = new_value;
      BREAK_FOR_EACH;
    }
  });

  if (!found) {
    // No entry so far in this trec
    TRecEntry *ne;
    ne = get_new_entry(t);
    ne -> tvar = tvar;
    ne -> expected_value = expected_value;
    ne -> new_value = new_value;
  }
}

/*......................................................................*/

static StgClosure *read_current_value_seen_from(StgTRecHeader *t,
                                                StgTVar *tvar) {
  int found;
  StgClosure *result = NULL;

  // Look for any relevent trec entries
  found = FALSE;
  while (t != NO_TREC) {
    FOR_EACH_ENTRY(t, e, {
      StgTVar *s;
      s = e -> tvar;
      if (s == tvar) {
        found = TRUE;
        result = e -> new_value;
        BREAK_FOR_EACH;
      }
    });
    if (found) break;
    t = t -> enclosing_trec;
  }

  if (!found) {
    // Value not yet held in a trec
    result = tvar -> current_value;
  }

  return result;
}
 
/*......................................................................*/

static int transaction_is_valid (StgTRecHeader *t) {
  StgTRecHeader *et;
  int result;

  if (shake()) {
    TRACE("Shake: pretending transaction trec=%p is invalid when it may not be\n", t);
    return FALSE;
  }

  et = t -> enclosing_trec;
  ASSERT ((t -> state == TREC_ACTIVE) || 
	  (t -> state == TREC_WAITING) ||
	  (t -> state == TREC_MUST_ABORT) ||
          (t -> state == TREC_CANNOT_COMMIT));
  result = !((t -> state) == TREC_MUST_ABORT);
  if (result) {
    FOR_EACH_ENTRY(t, e, {
      StgTVar *s;
      s = e -> tvar;
      if (e -> expected_value != read_current_value_seen_from(et, s)) {
	result = FALSE;
	BREAK_FOR_EACH;
      }
    });
  }
  return result;
}

/************************************************************************/

/* 
 * External functions below this point are repsonsible for:
 *
 * - acquiring/releasing the STM lock 
 *
 * - all updates to the trec status field
 *  ASSERT(t != NO_TREC);

 * By convention we increment entry_count when starting a new
 * transaction and we decrement it at the point where we can discard
 * the contents of the trec when exiting the outermost transaction.
 * This means that stmWait and stmRewait decrement the count whenever
 * they return FALSE (they do so exactly once for each transaction
 * that doesn't remain blocked forever).
 */

/************************************************************************/

void stmPreGCHook() {
  TRACE("stmPreGCHook\n");
  cached_trec_headers = NO_TREC;
  cached_trec_chunks = END_STM_CHUNK_LIST;
  cached_tvar_wait_queues = END_STM_WAIT_QUEUE;
}

/************************************************************************/

void initSTM() {
  TRACE("initSTM, NO_TREC=%p\n", NO_TREC);
  /* Nothing */
}

/*......................................................................*/

StgTRecHeader *stmStartTransaction(StgTRecHeader *outer) {
  StgTRecHeader *t;
  TRACE("stmStartTransaction current-trec=%p\n", outer);
  t = new_stg_trec_header(outer);
  TRACE("stmStartTransaction new-trec=%p\n", t);
  return t;
}

/*......................................................................*/

void stmAbortTransaction(StgTRecHeader *trec) {
  TRACE("stmAbortTransaction trec=%p\n", trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) || 
          (trec -> state == TREC_MUST_ABORT) ||
          (trec -> state == TREC_WAITING) ||
          (trec -> state == TREC_CANNOT_COMMIT));
  if (trec -> state == TREC_WAITING) {
    ASSERT (trec -> enclosing_trec == NO_TREC);
    TRACE("stmAbortTransaction aborting waiting transaction\n");
    stop_tsos_waiting_on_trec(trec);
  } 
  trec -> state = TREC_ABORTED;

  // Outcome now reflected by status field; no need for log
  recycle_closures_from_trec(trec);

  TRACE("stmAbortTransaction trec=%p done\n", trec);
}

/*......................................................................*/

void stmCondemnTransaction(StgTRecHeader *trec) {
  TRACE("stmCondemnTransaction trec=%p\n", trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) || 
          (trec -> state == TREC_MUST_ABORT) ||
          (trec -> state == TREC_WAITING) ||
          (trec -> state == TREC_CANNOT_COMMIT));

  if (trec -> state == TREC_WAITING) {
    ASSERT (trec -> enclosing_trec == NO_TREC);
    TRACE("stmCondemnTransaction condemning waiting transaction\n");
    stop_tsos_waiting_on_trec(trec);
  } 

  trec -> state = TREC_MUST_ABORT;

  TRACE("stmCondemnTransaction trec=%p done\n", trec);
}

/*......................................................................*/

StgTRecHeader *stmGetEnclosingTRec(StgTRecHeader *trec) {
  StgTRecHeader *outer;
  TRACE("stmGetEnclosingTRec trec=%p\n", trec);
  outer = trec -> enclosing_trec;
  TRACE("stmGetEnclosingTRec outer=%p\n", outer);
  return outer;
}

/*......................................................................*/

StgBool stmValidateTransaction(StgTRecHeader *trec) {
  int result;
  TRACE("stmValidateTransaction trec=%p\n", trec);
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) || 
         (trec -> state == TREC_MUST_ABORT) ||
         (trec -> state == TREC_CANNOT_COMMIT) ||
         (trec -> state == TREC_WAITING));

  lock_stm();
  result = transaction_is_valid(trec);

  if (!result && trec -> state != TREC_WAITING) {
    trec -> state = TREC_MUST_ABORT; 
  }

  unlock_stm();

  TRACE("stmValidateTransaction trec=%p result=%d\n", trec, result);
  return result;
}

/*......................................................................*/

StgBool stmCommitTransaction(StgTRecHeader *trec) {
  StgTRecHeader *et;
  int result;
  TRACE("stmCommitTransaction trec=%p trec->enclosing_trec=%p\n", trec, trec->enclosing_trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) || 
          (trec -> state == TREC_MUST_ABORT) ||
          (trec -> state == TREC_CANNOT_COMMIT));

  lock_stm();
  result = transaction_is_valid(trec);
  if (result) {
    et = trec -> enclosing_trec;
    if (trec -> state == TREC_CANNOT_COMMIT && et == NO_TREC) {
      TRACE("Cannot commit trec=%p at top level\n", trec);
      trec -> state = TREC_MUST_ABORT;
      result = FALSE;
    } else {
      if (et == NO_TREC) {
        TRACE("Non-nesting commit, NO_TREC=%p\n", NO_TREC);
      } else {
        TRACE("Nested commit into %p, NO_TREC=%p\n", et, NO_TREC);
      }
      
      FOR_EACH_ENTRY(trec, e, {
        StgTVar *s;
        s = e -> tvar;
        if (et == NO_TREC) {
          s -> current_value = e -> new_value;
          unpark_waiters_on(s);
        } else {
          merge_update_into(et, s, e -> expected_value, e -> new_value, FALSE);
        }
      });


      if (trec->state == TREC_CANNOT_COMMIT && et -> state == TREC_ACTIVE) {
        TRACE("Propagating TREC_CANNOT_COMMIT into %p\n", et);
        et -> state = TREC_CANNOT_COMMIT;
      }
    }
  } 

  // Outcome now reflected by status field; no need for log
  recycle_closures_from_trec(trec);
  
  unlock_stm();

  TRACE("stmCommitTransaction trec=%p result=%d\n", trec, result);

  return result;
}

/*......................................................................*/

StgBool stmMergeForWaiting(StgTRecHeader *trec, StgTRecHeader *other) {
  int result;
  TRACE("stmMergeForWaiting trec=%p (%d) other=%p (%d)\n", trec, trec -> state, other, other->state);
  ASSERT(trec != NO_TREC);
  ASSERT(other != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) || 
         (trec -> state == TREC_MUST_ABORT) ||
         (trec -> state == TREC_CANNOT_COMMIT));
  ASSERT((other -> state == TREC_ACTIVE) || 
         (other -> state == TREC_MUST_ABORT) ||
         (other -> state == TREC_CANNOT_COMMIT));

  lock_stm();
  result = (transaction_is_valid(trec));
  TRACE("stmMergeForWaiting initial result=%d\n", result);
  if (result) {
    result = transaction_is_valid(other);
    TRACE("stmMergeForWaiting after both result=%d\n", result);
    if (result) {
      // Individually the two transactions may be valid.  Now copy entries from
      // "other" into "trec".  This may cause "trec" to become invalid if it
      // contains an update that conflicts with one from "other"
      FOR_EACH_ENTRY(other, e, {
        StgTVar *s = e -> tvar;
        TRACE("Merging trec=%p exp=%p new=%p\n", other, e->expected_value, e->new_value);
        merge_update_into(trec, s, e-> expected_value, e -> new_value, TRUE);
      });
      result = (trec -> state != TREC_MUST_ABORT);
    } 
  }

  if (!result) {
    trec -> state = TREC_MUST_ABORT;
  }

  unlock_stm();

  TRACE("stmMergeForWaiting result=%d\n", result);
  return result;
}

/*......................................................................*/

StgBool stmWait(StgTSO *tso, StgTRecHeader *trec) {
  int result;
  TRACE("stmWait tso=%p trec=%p\n", tso, trec);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> enclosing_trec == NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) || 
          (trec -> state == TREC_MUST_ABORT) ||
          (trec -> state == TREC_CANNOT_COMMIT));

  lock_stm();
  result = transaction_is_valid(trec);
  if (result) {
    // The transaction is valid so far so we can actually start waiting.
    // (Otherwise the transaction was not valid and the thread will have to
    // retry it).
    start_tso_waiting_on_trec(tso, trec);
    park_tso(tso);
    trec -> state = TREC_WAITING;
  }  else {
    // Outcome now reflected by status field; no need for log
    recycle_closures_from_trec(trec);
  }
  unlock_stm();

  TRACE("stmWait trec=%p result=%d\n", trec, result);
  return result;
}

/*......................................................................*/

StgBool stmReWait(StgTSO *tso) {
  int result;
  StgTRecHeader *trec = tso->trec;

  TRACE("stmReWait trec=%p\n", trec);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> enclosing_trec == NO_TREC);
  ASSERT ((trec -> state == TREC_WAITING) || 
          (trec -> state == TREC_MUST_ABORT));

  lock_stm();
  result = transaction_is_valid(trec);
  TRACE("stmReWait trec=%p result=%d\n", trec, result);
  if (result) {
    // The transaction remains valid -- do nothing because it is already on
    // the wait queues
    ASSERT (trec -> state == TREC_WAITING);
    park_tso(tso);
  } else {
    // The transcation has become invalid.  We can now remove it from the wait
    // queues.
    if (trec -> state != TREC_MUST_ABORT) {
	  stop_tsos_waiting_on_trec (trec);

	  // Outcome now reflected by status field; no need for log
	  recycle_closures_from_trec(trec);
    }

  }
  unlock_stm();

  TRACE("stmReWait trec=%p result=%d\n", trec, result);
  return result;
}

/*......................................................................*/

StgClosure *stmReadTVar(StgTRecHeader *trec, 
			StgTVar *tvar) {
  StgTRecHeader *et;
  StgClosure *result = NULL; // Suppress unassignment warning
  int found = FALSE;
  TRecEntry *ne = NULL;

  TRACE("stmReadTVar trec=%p tvar=%p\n", trec, tvar);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> state == TREC_ACTIVE || 
          trec -> state == TREC_MUST_ABORT ||
          trec -> state == TREC_CANNOT_COMMIT);

  lock_stm();
  found = FALSE;

  // Look for an existing entry in our trec or in an enclosing trec
  et = trec;
  while (et != NO_TREC) {
    FOR_EACH_ENTRY(et, e, {
      TRACE("testing e=%p\n", e);
      if (e -> tvar == tvar) {
        found = TRUE;
        result = e -> new_value;
        BREAK_FOR_EACH;
      }
    });
    if (found) break;
    et = et -> enclosing_trec;
  }

  if (found && et != trec) {
    // Entry found in another trec
    ASSERT (result != NULL);
    TRACE("duplicating entry\n");
    ne = get_new_entry(trec);
    ne -> tvar = tvar;
    ne -> expected_value = result;
    ne -> new_value = result;
  } else if (!found) {
    // No entry found
    ASSERT (result == NULL);
    TRACE("need new entry\n");
    ne = get_new_entry(trec);
    TRACE("got ne=%p\n", ne);
    result = tvar -> current_value;
    ne -> tvar = tvar;
    ne -> expected_value = result;
    ne -> new_value = result;
  }

  unlock_stm();
  ASSERT (result != NULL);
  TRACE("stmReadTVar trec=%p result=%p\n", trec, result);

  return result;
}

/*......................................................................*/

void stmWriteTVar(StgTRecHeader *trec,
		  StgTVar *tvar, 
		  StgClosure *new_value) {
  StgTRecHeader *et;
  TRecEntry *ne;
  TRecEntry *entry = NULL;
  int found;
  TRACE("stmWriteTVar trec=%p tvar=%p new_value=%p\n", trec, tvar, new_value);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> state == TREC_ACTIVE || 
          trec -> state == TREC_MUST_ABORT ||
          trec -> state == TREC_CANNOT_COMMIT);

  lock_stm();
  found = FALSE;

  // Look for an existing entry in our trec or in an enclosing trec
  et = trec;
  while (et != NO_TREC) {
    FOR_EACH_ENTRY(et, e, {
      if (e -> tvar == tvar) {
        found = TRUE;
        entry = e;
        BREAK_FOR_EACH;
      }
    });
    if (found) break;
    et = et -> enclosing_trec;
  }

  if (found && et == trec) {
    // Entry found in our trec
    entry -> new_value = new_value;
  } else if (found) {
    // Entry found in another trec
    ne = get_new_entry(trec);
    ne -> tvar = tvar;
    ne -> expected_value = entry -> new_value;
    ne -> new_value = new_value;
  } else {
    // No entry found
    ne = get_new_entry(trec);
    ne -> tvar = tvar;
    ne -> expected_value = tvar -> current_value;
    ne -> new_value = new_value;
  }

  unlock_stm();
  TRACE("stmWriteTVar trec=%p done\n", trec);
}


/*......................................................................*/

