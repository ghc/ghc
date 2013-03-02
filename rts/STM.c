/* -----------------------------------------------------------------------------
 * (c) The GHC Team 1998-2005
 *
 * STM implementation.
 *
 * Overview
 * --------
 *
 * See the PPoPP 2005 paper "Composable memory transactions".  In summary,
 * each transcation has a TRec (transaction record) holding entries for each of the
 * TVars (transactional variables) that it has accessed.  Each entry records
 * (a) the TVar, (b) the expected value seen in the TVar, (c) the new value that
 * the transaction wants to write to the TVar, (d) during commit, the identity of
 * the TRec that wrote the expected value.
 *
 * Separate TRecs are used for each level in a nest of transactions.  This allows
 * a nested transaction to be aborted without condemning its enclosing transactions.
 * This is needed in the implementation of catchRetry.  Note that the "expected value"
 * in a nested transaction's TRec is the value expected to be *held in memory* if
 * the transaction commits -- not the "new value" stored in one of the enclosing
 * transactions.  This means that validation can be done without searching through
 * a nest of TRecs.
 *
 * Concurrency control
 * -------------------
 *
 * Three different concurrency control schemes can be built according to the settings
 * in STM.h:
 *
 * STM_UNIPROC assumes that the caller serialises invocations on the STM interface.
 * In the Haskell RTS this means it is suitable only for non-THREADED_RTS builds.
 *
 * STM_CG_LOCK uses coarse-grained locking -- a single 'stm lock' is acquired during
 * an invocation on the STM interface.  Note that this does not mean that
 * transactions are simply serialized -- the lock is only held *within* the
 * implementation of stmCommitTransaction, stmWait etc.
 *
 * STM_FG_LOCKS uses fine-grained locking -- locking is done on a per-TVar basis
 * and, when committing a transaction, no locks are acquired for TVars that have
 * been read but not updated.
 *
 * Concurrency control is implemented in the functions:
 *
 *    lock_stm
 *    unlock_stm
 *    lock_tvar / cond_lock_tvar
 *    unlock_tvar
 *
 * The choice between STM_UNIPROC / STM_CG_LOCK / STM_FG_LOCKS affects the
 * implementation of these functions.
 *
 * lock_stm & unlock_stm are straightforward : they acquire a simple spin-lock
 * using STM_CG_LOCK, and otherwise they are no-ops.
 *
 * lock_tvar / cond_lock_tvar and unlock_tvar are more complex because they
 * have other effects (present in STM_UNIPROC and STM_CG_LOCK builds) as well
 * as the actual business of maniupultaing a lock (present only in STM_FG_LOCKS
 * builds).  This is because locking a TVar is implemented by writing the lock
 * holder's TRec into the TVar's current_value field:
 *
 *   lock_tvar - lock a specified TVar (STM_FG_LOCKS only), returning the value
 *               it contained.
 *
 *   cond_lock_tvar - lock a specified TVar (STM_FG_LOCKS only) if it
 *               contains a specified value.  Return TRUE if this succeeds,
 *               FALSE otherwise.
 *
 *   unlock_tvar - release the lock on a specified TVar (STM_FG_LOCKS only),
 *               storing a specified value in place of the lock entry.
 *
 * Using these operations, the typcial pattern of a commit/validate/wait operation
 * is to (a) lock the STM, (b) lock all the TVars being updated, (c) check that
 * the TVars that were only read from still contain their expected values,
 * (d) release the locks on the TVars, writing updates to them in the case of a
 * commit, (e) unlock the STM.
 *
 * Queues of waiting threads hang off the first_watch_queue_entry
 * field of each TVar.  This may only be manipulated when holding that
 * TVar's lock.  In particular, when a thread is putting itself to
 * sleep, it mustn't release the TVar's lock until it has added itself
 * to the wait queue and marked its TSO as BlockedOnSTM -- this makes
 * sure that other threads will know to wake it.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Schedule.h"
#include "STM.h"
#include "Trace.h"
#include "Threads.h"
#include "sm/Storage.h"

#include <stdio.h>

#define TRUE 1
#define FALSE 0

// ACQ_ASSERT is used for assertions which are only required for
// THREADED_RTS builds with fine-grained locking.

#if defined(STM_FG_LOCKS)
#define ACQ_ASSERT(_X) ASSERT(_X)
#define NACQ_ASSERT(_X) /*Nothing*/
#else
#define ACQ_ASSERT(_X) /*Nothing*/
#define NACQ_ASSERT(_X) ASSERT(_X)
#endif

/*......................................................................*/

// If SHAKE is defined then validation will sometime spuriously fail.  They helps test
// unusualy code paths if genuine contention is rare

#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x)

#ifdef SHAKE
static const int do_shake = TRUE;
#else
static const int do_shake = FALSE;
#endif
static int shake_ctr = 0;
static int shake_lim = 1;

static int shake(void) {
  if (do_shake) {
    if (((shake_ctr++) % shake_lim) == 0) {
      shake_ctr = 1;
      shake_lim ++;
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

#define FOR_EACH_ENTRY(_t,_x,CODE) do {                                         \
  StgTRecHeader *__t = (_t);                                                    \
  StgTRecChunk *__c = __t -> current_chunk;                                     \
  StgWord __limit = __c -> next_entry_idx;                                      \
  TRACE("%p : FOR_EACH_ENTRY, current_chunk=%p limit=%ld", __t, __c, __limit);  \
  while (__c != END_STM_CHUNK_LIST) {                                           \
    StgWord __i;                                                                \
    for (__i = 0; __i < __limit; __i ++) {                                      \
      TRecEntry *_x = &(__c -> entries[__i]);                                   \
      do { CODE } while (0);                                                    \
    }                                                                           \
    __c = __c -> prev_chunk;                                                    \
    __limit = TREC_CHUNK_NUM_ENTRIES;                                           \
  }                                                                             \
 exit_for_each:                                                                 \
  if (FALSE) goto exit_for_each;                                                \
} while (0)

#define BREAK_FOR_EACH goto exit_for_each

/*......................................................................*/

// if REUSE_MEMORY is defined then attempt to re-use descriptors, log chunks,
// and wait queue entries without GC

#define REUSE_MEMORY

/*......................................................................*/

#define IF_STM_UNIPROC(__X)  do { } while (0)
#define IF_STM_CG_LOCK(__X)  do { } while (0)
#define IF_STM_FG_LOCKS(__X) do { } while (0)

#if defined(STM_UNIPROC)
#undef IF_STM_UNIPROC
#define IF_STM_UNIPROC(__X)  do { __X } while (0)
static const StgBool config_use_read_phase = FALSE;

static void lock_stm(StgTRecHeader *trec STG_UNUSED) {
  TRACE("%p : lock_stm()", trec);
}

static void unlock_stm(StgTRecHeader *trec STG_UNUSED) {
  TRACE("%p : unlock_stm()", trec);
}

static StgClosure *lock_tvar(StgTRecHeader *trec STG_UNUSED,
                             StgTVar *s STG_UNUSED) {
  StgClosure *result;
  TRACE("%p : lock_tvar(%p)", trec, s);
  result = s -> current_value;
  return result;
}

static void unlock_tvar(Capability *cap,
                        StgTRecHeader *trec STG_UNUSED,
                        StgTVar *s,
                        StgClosure *c,
                        StgBool force_update) {
  TRACE("%p : unlock_tvar(%p)", trec, s);
  if (force_update) {
    s -> current_value = c;
    dirty_TVAR(cap,s);
  }
}

static StgBool cond_lock_tvar(StgTRecHeader *trec STG_UNUSED,
                              StgTVar *s STG_UNUSED,
                              StgClosure *expected) {
  StgClosure *result;
  TRACE("%p : cond_lock_tvar(%p, %p)", trec, s, expected);
  result = s -> current_value;
  TRACE("%p : %s", trec, (result == expected) ? "success" : "failure");
  return (result == expected);
}

static StgBool lock_inv(StgAtomicInvariant *inv STG_UNUSED) {
  // Nothing -- uniproc
  return TRUE;
}

static void unlock_inv(StgAtomicInvariant *inv STG_UNUSED) {
  // Nothing -- uniproc
}
#endif

#if defined(STM_CG_LOCK) /*........................................*/

#undef IF_STM_CG_LOCK
#define IF_STM_CG_LOCK(__X)  do { __X } while (0)
static const StgBool config_use_read_phase = FALSE;
static volatile StgTRecHeader *smp_locked = NULL;

static void lock_stm(StgTRecHeader *trec) {
  while (cas(&smp_locked, NULL, trec) != NULL) { }
  TRACE("%p : lock_stm()", trec);
}

static void unlock_stm(StgTRecHeader *trec STG_UNUSED) {
  TRACE("%p : unlock_stm()", trec);
  ASSERT (smp_locked == trec);
  smp_locked = 0;
}

static StgClosure *lock_tvar(StgTRecHeader *trec STG_UNUSED,
                             StgTVar *s STG_UNUSED) {
  StgClosure *result;
  TRACE("%p : lock_tvar(%p)", trec, s);
  ASSERT (smp_locked == trec);
  result = s -> current_value;
  return result;
}

static void *unlock_tvar(Capability *cap,
                         StgTRecHeader *trec STG_UNUSED,
                         StgTVar *s,
                         StgClosure *c,
                         StgBool force_update) {
  TRACE("%p : unlock_tvar(%p, %p)", trec, s, c);
  ASSERT (smp_locked == trec);
  if (force_update) {
    s -> current_value = c;
    dirty_TVAR(cap,s);
  }
}

static StgBool cond_lock_tvar(StgTRecHeader *trec STG_UNUSED,
                               StgTVar *s STG_UNUSED,
                               StgClosure *expected) {
  StgClosure *result;
  TRACE("%p : cond_lock_tvar(%p, %p)", trec, s, expected);
  ASSERT (smp_locked == trec);
  result = s -> current_value;
  TRACE("%p : %d", result ? "success" : "failure");
  return (result == expected);
}

static StgBool lock_inv(StgAtomicInvariant *inv STG_UNUSED) {
  // Nothing -- protected by STM lock
  return TRUE;
}

static void unlock_inv(StgAtomicInvariant *inv STG_UNUSED) {
  // Nothing -- protected by STM lock
}
#endif

#if defined(STM_FG_LOCKS) /*...................................*/

#undef IF_STM_FG_LOCKS
#define IF_STM_FG_LOCKS(__X) do { __X } while (0)
static const StgBool config_use_read_phase = TRUE;

static void lock_stm(StgTRecHeader *trec STG_UNUSED) {
  TRACE("%p : lock_stm()", trec);
}

static void unlock_stm(StgTRecHeader *trec STG_UNUSED) {
  TRACE("%p : unlock_stm()", trec);
}

static StgClosure *lock_tvar(StgTRecHeader *trec,
                             StgTVar *s STG_UNUSED) {
  StgClosure *result;
  TRACE("%p : lock_tvar(%p)", trec, s);
  do {
    do {
      result = s -> current_value;
    } while (GET_INFO(UNTAG_CLOSURE(result)) == &stg_TREC_HEADER_info);
  } while (cas((void *)&(s -> current_value),
	       (StgWord)result, (StgWord)trec) != (StgWord)result);
  return result;
}

static void unlock_tvar(Capability *cap,
                        StgTRecHeader *trec STG_UNUSED,
                        StgTVar *s,
                        StgClosure *c,
                        StgBool force_update STG_UNUSED) {
  TRACE("%p : unlock_tvar(%p, %p)", trec, s, c);
  ASSERT(s -> current_value == (StgClosure *)trec);
  s -> current_value = c;
  dirty_TVAR(cap,s);
}

static StgBool cond_lock_tvar(StgTRecHeader *trec,
                              StgTVar *s,
                              StgClosure *expected) {
  StgClosure *result;
  StgWord w;
  TRACE("%p : cond_lock_tvar(%p, %p)", trec, s, expected);
  w = cas((void *)&(s -> current_value), (StgWord)expected, (StgWord)trec);
  result = (StgClosure *)w;
  TRACE("%p : %s", trec, result ? "success" : "failure");
  return (result == expected);
}

static StgBool lock_inv(StgAtomicInvariant *inv) {
  return (cas(&(inv -> lock), 0, 1) == 0);
}

static void unlock_inv(StgAtomicInvariant *inv) {
  ASSERT(inv -> lock == 1);
  inv -> lock = 0;
}
#endif

/*......................................................................*/

static StgBool watcher_is_tso(StgTVarWatchQueue *q) {
  StgClosure *c = q -> closure;
  StgInfoTable *info = get_itbl(c);
  return (info -> type) == TSO;
}

static StgBool watcher_is_invariant(StgTVarWatchQueue *q) {
  StgClosure *c = q -> closure;
  return (c->header.info == &stg_ATOMIC_INVARIANT_info);
}

/*......................................................................*/

// Helper functions for thread blocking and unblocking

static void park_tso(StgTSO *tso) {
  ASSERT(tso -> why_blocked == NotBlocked);
  tso -> why_blocked = BlockedOnSTM;
  tso -> block_info.closure = (StgClosure *) END_TSO_QUEUE;
  TRACE("park_tso on tso=%p", tso);
}

static void unpark_tso(Capability *cap, StgTSO *tso) {
    // We will continue unparking threads while they remain on one of the wait
    // queues: it's up to the thread itself to remove it from the wait queues
    // if it decides to do so when it is scheduled.

    // Unblocking a TSO from BlockedOnSTM is done under the TSO lock,
    // to avoid multiple CPUs unblocking the same TSO, and also to
    // synchronise with throwTo(). The first time the TSO is unblocked
    // we mark this fact by setting block_info.closure == STM_AWOKEN.
    // This way we can avoid sending further wakeup messages in the
    // future.
    lockTSO(tso);
    if (tso->why_blocked == BlockedOnSTM &&
        tso->block_info.closure == &stg_STM_AWOKEN_closure) {
      TRACE("unpark_tso already woken up tso=%p", tso);
    } else if (tso -> why_blocked == BlockedOnSTM) {
      TRACE("unpark_tso on tso=%p", tso);
      tso->block_info.closure = &stg_STM_AWOKEN_closure;
      tryWakeupThread(cap,tso);
    } else {
      TRACE("spurious unpark_tso on tso=%p", tso);
    }
    unlockTSO(tso);
}

static void unpark_waiters_on(Capability *cap, StgTVar *s) {
  StgTVarWatchQueue *q;
  StgTVarWatchQueue *trail;
  TRACE("unpark_waiters_on tvar=%p", s);
  // unblock TSOs in reverse order, to be a bit fairer (#2319)
  for (q = s -> first_watch_queue_entry, trail = q;
       q != END_STM_WATCH_QUEUE;
       q = q -> next_queue_entry) {
    trail = q;
  }
  q = trail;
  for (;
       q != END_STM_WATCH_QUEUE;
       q = q -> prev_queue_entry) {
    if (watcher_is_tso(q)) {
      unpark_tso(cap, (StgTSO *)(q -> closure));
    }
  }
}

/*......................................................................*/

// Helper functions for downstream allocation and initialization

static StgInvariantCheckQueue *new_stg_invariant_check_queue(Capability *cap,
							     StgAtomicInvariant *invariant) {
  StgInvariantCheckQueue *result;
  result = (StgInvariantCheckQueue *)allocate(cap, sizeofW(StgInvariantCheckQueue));
  SET_HDR (result, &stg_INVARIANT_CHECK_QUEUE_info, CCS_SYSTEM);
  result -> invariant = invariant;
  result -> my_execution = NO_TREC;
  return result;
}

static StgTVarWatchQueue *new_stg_tvar_watch_queue(Capability *cap,
						   StgClosure *closure) {
  StgTVarWatchQueue *result;
  result = (StgTVarWatchQueue *)allocate(cap, sizeofW(StgTVarWatchQueue));
  SET_HDR (result, &stg_TVAR_WATCH_QUEUE_info, CCS_SYSTEM);
  result -> closure = closure;
  return result;
}

static StgTRecChunk *new_stg_trec_chunk(Capability *cap) {
  StgTRecChunk *result;
  result = (StgTRecChunk *)allocate(cap, sizeofW(StgTRecChunk));
  SET_HDR (result, &stg_TREC_CHUNK_info, CCS_SYSTEM);
  result -> prev_chunk = END_STM_CHUNK_LIST;
  result -> next_entry_idx = 0;
  return result;
}

static StgTRecHeader *new_stg_trec_header(Capability *cap,
                                          StgTRecHeader *enclosing_trec) {
  StgTRecHeader *result;
  result = (StgTRecHeader *) allocate(cap, sizeofW(StgTRecHeader));
  SET_HDR (result, &stg_TREC_HEADER_info, CCS_SYSTEM);

  result -> enclosing_trec = enclosing_trec;
  result -> current_chunk = new_stg_trec_chunk(cap);
  result -> invariants_to_check = END_INVARIANT_CHECK_QUEUE;

  if (enclosing_trec == NO_TREC) {
    result -> state = TREC_ACTIVE;
  } else {
    ASSERT(enclosing_trec -> state == TREC_ACTIVE ||
           enclosing_trec -> state == TREC_CONDEMNED);
    result -> state = enclosing_trec -> state;
  }

  return result;
}

/*......................................................................*/

// Allocation / deallocation functions that retain per-capability lists
// of closures that can be re-used

static StgInvariantCheckQueue *alloc_stg_invariant_check_queue(Capability *cap,
							       StgAtomicInvariant *invariant) {
  StgInvariantCheckQueue *result = NULL;
  if (cap -> free_invariant_check_queues == END_INVARIANT_CHECK_QUEUE) {
    result = new_stg_invariant_check_queue(cap, invariant);
  } else {
    result = cap -> free_invariant_check_queues;
    result -> invariant = invariant;
    result -> my_execution = NO_TREC;
    cap -> free_invariant_check_queues = result -> next_queue_entry;
  }
  return result;
}

static StgTVarWatchQueue *alloc_stg_tvar_watch_queue(Capability *cap,
						     StgClosure *closure) {
  StgTVarWatchQueue *result = NULL;
  if (cap -> free_tvar_watch_queues == END_STM_WATCH_QUEUE) {
    result = new_stg_tvar_watch_queue(cap, closure);
  } else {
    result = cap -> free_tvar_watch_queues;
    result -> closure = closure;
    cap -> free_tvar_watch_queues = result -> next_queue_entry;
  }
  return result;
}

static void free_stg_tvar_watch_queue(Capability *cap,
				      StgTVarWatchQueue *wq) {
#if defined(REUSE_MEMORY)
  wq -> next_queue_entry = cap -> free_tvar_watch_queues;
  cap -> free_tvar_watch_queues = wq;
#endif
}

static StgTRecChunk *alloc_stg_trec_chunk(Capability *cap) {
  StgTRecChunk *result = NULL;
  if (cap -> free_trec_chunks == END_STM_CHUNK_LIST) {
    result = new_stg_trec_chunk(cap);
  } else {
    result = cap -> free_trec_chunks;
    cap -> free_trec_chunks = result -> prev_chunk;
    result -> prev_chunk = END_STM_CHUNK_LIST;
    result -> next_entry_idx = 0;
  }
  return result;
}

static void free_stg_trec_chunk(Capability *cap,
                                StgTRecChunk *c) {
#if defined(REUSE_MEMORY)
  c -> prev_chunk = cap -> free_trec_chunks;
  cap -> free_trec_chunks = c;
#endif
}

static StgTRecHeader *alloc_stg_trec_header(Capability *cap,
                                            StgTRecHeader *enclosing_trec) {
  StgTRecHeader *result = NULL;
  if (cap -> free_trec_headers == NO_TREC) {
    result = new_stg_trec_header(cap, enclosing_trec);
  } else {
    result = cap -> free_trec_headers;
    cap -> free_trec_headers = result -> enclosing_trec;
    result -> enclosing_trec = enclosing_trec;
    result -> current_chunk -> next_entry_idx = 0;
    result -> invariants_to_check = END_INVARIANT_CHECK_QUEUE;
    if (enclosing_trec == NO_TREC) {
      result -> state = TREC_ACTIVE;
    } else {
      ASSERT(enclosing_trec -> state == TREC_ACTIVE ||
             enclosing_trec -> state == TREC_CONDEMNED);
      result -> state = enclosing_trec -> state;
    }
  }
  return result;
}

static void free_stg_trec_header(Capability *cap,
                                 StgTRecHeader *trec) {
#if defined(REUSE_MEMORY)
  StgTRecChunk *chunk = trec -> current_chunk -> prev_chunk;
  while (chunk != END_STM_CHUNK_LIST) {
    StgTRecChunk *prev_chunk = chunk -> prev_chunk;
    free_stg_trec_chunk(cap, chunk);
    chunk = prev_chunk;
  }
  trec -> current_chunk -> prev_chunk = END_STM_CHUNK_LIST;
  trec -> enclosing_trec = cap -> free_trec_headers;
  cap -> free_trec_headers = trec;
#endif
}

/*......................................................................*/

// Helper functions for managing waiting lists

static void build_watch_queue_entries_for_trec(Capability *cap,
					       StgTSO *tso,
					       StgTRecHeader *trec) {
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT(trec -> state == TREC_ACTIVE);

  TRACE("%p : build_watch_queue_entries_for_trec()", trec);

  FOR_EACH_ENTRY(trec, e, {
    StgTVar *s;
    StgTVarWatchQueue *q;
    StgTVarWatchQueue *fq;
    s = e -> tvar;
    TRACE("%p : adding tso=%p to watch queue for tvar=%p", trec, tso, s);
    ACQ_ASSERT(s -> current_value == (StgClosure *)trec);
    NACQ_ASSERT(s -> current_value == e -> expected_value);
    fq = s -> first_watch_queue_entry;
    q = alloc_stg_tvar_watch_queue(cap, (StgClosure*) tso);
    q -> next_queue_entry = fq;
    q -> prev_queue_entry = END_STM_WATCH_QUEUE;
    if (fq != END_STM_WATCH_QUEUE) {
      fq -> prev_queue_entry = q;
    }
    s -> first_watch_queue_entry = q;
    e -> new_value = (StgClosure *) q;
    dirty_TVAR(cap,s); // we modified first_watch_queue_entry
  });
}

static void remove_watch_queue_entries_for_trec(Capability *cap,
						StgTRecHeader *trec) {
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT(trec -> state == TREC_WAITING ||
         trec -> state == TREC_CONDEMNED);

  TRACE("%p : remove_watch_queue_entries_for_trec()", trec);

  FOR_EACH_ENTRY(trec, e, {
    StgTVar *s;
    StgTVarWatchQueue *pq;
    StgTVarWatchQueue *nq;
    StgTVarWatchQueue *q;
    StgClosure *saw;
    s = e -> tvar;
    saw = lock_tvar(trec, s);
    q = (StgTVarWatchQueue *) (e -> new_value);
    TRACE("%p : removing tso=%p from watch queue for tvar=%p",
	  trec,
	  q -> closure,
	  s);
    ACQ_ASSERT(s -> current_value == (StgClosure *)trec);
    nq = q -> next_queue_entry;
    pq = q -> prev_queue_entry;
    if (nq != END_STM_WATCH_QUEUE) {
      nq -> prev_queue_entry = pq;
    }
    if (pq != END_STM_WATCH_QUEUE) {
      pq -> next_queue_entry = nq;
    } else {
      ASSERT (s -> first_watch_queue_entry == q);
      s -> first_watch_queue_entry = nq;
      dirty_TVAR(cap,s); // we modified first_watch_queue_entry
    }
    free_stg_tvar_watch_queue(cap, q);
    unlock_tvar(cap, trec, s, saw, FALSE);
  });
}

/*......................................................................*/

static TRecEntry *get_new_entry(Capability *cap,
                                StgTRecHeader *t) {
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
    nc = alloc_stg_trec_chunk(cap);
    nc -> prev_chunk = c;
    nc -> next_entry_idx = 1;
    t -> current_chunk = nc;
    result = &(nc -> entries[0]);
  }

  return result;
}

/*......................................................................*/

static void merge_update_into(Capability *cap,
                              StgTRecHeader *t,
                              StgTVar *tvar,
                              StgClosure *expected_value,
                              StgClosure *new_value) {
  int found;

  // Look for an entry in this trec
  found = FALSE;
  FOR_EACH_ENTRY(t, e, {
    StgTVar *s;
    s = e -> tvar;
    if (s == tvar) {
      found = TRUE;
      if (e -> expected_value != expected_value) {
        // Must abort if the two entries start from different values
        TRACE("%p : update entries inconsistent at %p (%p vs %p)",
              t, tvar, e -> expected_value, expected_value);
        t -> state = TREC_CONDEMNED;
      }
      e -> new_value = new_value;
      BREAK_FOR_EACH;
    }
  });

  if (!found) {
    // No entry so far in this trec
    TRecEntry *ne;
    ne = get_new_entry(cap, t);
    ne -> tvar = tvar;
    ne -> expected_value = expected_value;
    ne -> new_value = new_value;
  }
}

/*......................................................................*/

static void merge_read_into(Capability *cap,
                            StgTRecHeader *trec,
			    StgTVar *tvar,
                            StgClosure *expected_value)
{
  int found;
  StgTRecHeader *t;

  found = FALSE;

  //
  // See #7493
  //
  // We need to look for an existing entry *anywhere* in the stack of
  // nested transactions.  Otherwise, in stmCommitNestedTransaction()
  // we can't tell the difference between
  //
  //   (1) a read-only entry
  //   (2) an entry that writes back the original value
  //
  // Since in both cases e->new_value == e->expected_value. But in (1)
  // we want to do nothing, and in (2) we want to update e->new_value
  // in the outer transaction.
  //
  // Here we deal with the first possibility: we never create a
  // read-only entry in an inner transaction if there is an existing
  // outer entry; so we never have an inner read and an outer update.
  // So then in stmCommitNestedTransaction() we know we can always
  // write e->new_value over the outer entry, because the inner entry
  // is the most up to date.
  //
  for (t = trec; !found && t != NO_TREC; t = t -> enclosing_trec)
  {
    FOR_EACH_ENTRY(t, e, {
      if (e -> tvar == tvar) {
        found = TRUE;
        if (e -> expected_value != expected_value) {
            // Must abort if the two entries start from different values
            TRACE("%p : read entries inconsistent at %p (%p vs %p)",
                  t, tvar, e -> expected_value, expected_value);
            t -> state = TREC_CONDEMNED;
        }
        BREAK_FOR_EACH;
      }
    });
  }

  if (!found) {
    // No entry found
    TRecEntry *ne;
    ne = get_new_entry(cap, trec);
    ne -> tvar = tvar;
    ne -> expected_value = expected_value;
    ne -> new_value = expected_value;
  }
}

/*......................................................................*/

static StgBool entry_is_update(TRecEntry *e) {
  StgBool result;
  result = (e -> expected_value != e -> new_value);
  return result;
}

#if defined(STM_FG_LOCKS)
static StgBool entry_is_read_only(TRecEntry *e) {
  StgBool result;
  result = (e -> expected_value == e -> new_value);
  return result;
}

static StgBool tvar_is_locked(StgTVar *s, StgTRecHeader *h) {
  StgClosure *c;
  StgBool result;
  c = s -> current_value;
  result = (c == (StgClosure *) h);
  return result;
}
#endif

// revert_ownership : release a lock on a TVar, storing back
// the value that it held when the lock was acquired.  "revert_all"
// is set in stmWait and stmReWait when we acquired locks on all of
// the TVars involved.  "revert_all" is not set in commit operations
// where we don't lock TVars that have been read from but not updated.

static void revert_ownership(Capability *cap STG_UNUSED,
                             StgTRecHeader *trec STG_UNUSED,
                             StgBool revert_all STG_UNUSED) {
#if defined(STM_FG_LOCKS)
  FOR_EACH_ENTRY(trec, e, {
    if (revert_all || entry_is_update(e)) {
      StgTVar *s;
      s = e -> tvar;
      if (tvar_is_locked(s, trec)) {
          unlock_tvar(cap, trec, s, e -> expected_value, TRUE);
      }
    }
  });
#endif
}

/*......................................................................*/

// validate_and_acquire_ownership : this performs the twin functions
// of checking that the TVars referred to by entries in trec hold the
// expected values and:
//
//   - locking the TVar (on updated TVars during commit, or all TVars
//     during wait)
//
//   - recording the identity of the TRec who wrote the value seen in the
//     TVar (on non-updated TVars during commit).  These values are
//     stashed in the TRec entries and are then checked in check_read_only
//     to ensure that an atomic snapshot of all of these locations has been
//     seen.

static StgBool validate_and_acquire_ownership (Capability *cap,
                                               StgTRecHeader *trec,
                                               int acquire_all,
                                               int retain_ownership) {
  StgBool result;

  if (shake()) {
    TRACE("%p : shake, pretending trec is invalid when it may not be", trec);
    return FALSE;
  }

  ASSERT ((trec -> state == TREC_ACTIVE) ||
	  (trec -> state == TREC_WAITING) ||
	  (trec -> state == TREC_CONDEMNED));
  result = !((trec -> state) == TREC_CONDEMNED);
  if (result) {
    FOR_EACH_ENTRY(trec, e, {
      StgTVar *s;
      s = e -> tvar;
      if (acquire_all || entry_is_update(e)) {
        TRACE("%p : trying to acquire %p", trec, s);
        if (!cond_lock_tvar(trec, s, e -> expected_value)) {
          TRACE("%p : failed to acquire %p", trec, s);
          result = FALSE;
          BREAK_FOR_EACH;
        }
      } else {
        ASSERT(config_use_read_phase);
        IF_STM_FG_LOCKS({
          TRACE("%p : will need to check %p", trec, s);
          if (s -> current_value != e -> expected_value) {
            TRACE("%p : doesn't match", trec);
            result = FALSE;
            BREAK_FOR_EACH;
          }
          e -> num_updates = s -> num_updates;
          if (s -> current_value != e -> expected_value) {
            TRACE("%p : doesn't match (race)", trec);
            result = FALSE;
            BREAK_FOR_EACH;
          } else {
            TRACE("%p : need to check version %ld", trec, e -> num_updates);
          }
        });
      }
    });
  }

  if ((!result) || (!retain_ownership)) {
      revert_ownership(cap, trec, acquire_all);
  }

  return result;
}

// check_read_only : check that we've seen an atomic snapshot of the
// non-updated TVars accessed by a trec.  This checks that the last TRec to
// commit an update to the TVar is unchanged since the value was stashed in
// validate_and_acquire_ownership.  If no udpate is seen to any TVar than
// all of them contained their expected values at the start of the call to
// check_read_only.
//
// The paper "Concurrent programming without locks" (under submission), or
// Keir Fraser's PhD dissertation "Practical lock-free programming" discuss
// this kind of algorithm.

static StgBool check_read_only(StgTRecHeader *trec STG_UNUSED) {
  StgBool result = TRUE;

  ASSERT (config_use_read_phase);
  IF_STM_FG_LOCKS({
    FOR_EACH_ENTRY(trec, e, {
      StgTVar *s;
      s = e -> tvar;
      if (entry_is_read_only(e)) {
        TRACE("%p : check_read_only for TVar %p, saw %ld", trec, s, e -> num_updates);
        if (s -> num_updates != e -> num_updates) {
          // ||s -> current_value != e -> expected_value) {
          TRACE("%p : mismatch", trec);
          result = FALSE;
          BREAK_FOR_EACH;
        }
      }
    });
  });

  return result;
}


/************************************************************************/

void stmPreGCHook (Capability *cap) {
  lock_stm(NO_TREC);
  TRACE("stmPreGCHook");
  cap->free_tvar_watch_queues = END_STM_WATCH_QUEUE;
  cap->free_trec_chunks = END_STM_CHUNK_LIST;
  cap->free_trec_headers = NO_TREC;
  unlock_stm(NO_TREC);
}

/************************************************************************/

// check_read_only relies on version numbers held in TVars' "num_updates"
// fields not wrapping around while a transaction is committed.  The version
// number is incremented each time an update is committed to the TVar
// This is unlikely to wrap around when 32-bit integers are used for the counts,
// but to ensure correctness we maintain a shared count on the maximum
// number of commit operations that may occur and check that this has
// not increased by more than 2^32 during a commit.

#define TOKEN_BATCH_SIZE 1024

static volatile StgInt64 max_commits = 0;

#if defined(THREADED_RTS)
static volatile StgBool token_locked = FALSE;

static void getTokenBatch(Capability *cap) {
  while (cas((void *)&token_locked, FALSE, TRUE) == TRUE) { /* nothing */ }
  max_commits += TOKEN_BATCH_SIZE;
  TRACE("%p : cap got token batch, max_commits=%" FMT_Int64, cap, max_commits);
  cap -> transaction_tokens = TOKEN_BATCH_SIZE;
  token_locked = FALSE;
}

static void getToken(Capability *cap) {
  if (cap -> transaction_tokens == 0) {
    getTokenBatch(cap);
  }
  cap -> transaction_tokens --;
}
#else
static void getToken(Capability *cap STG_UNUSED) {
  // Nothing
}
#endif

/*......................................................................*/

StgTRecHeader *stmStartTransaction(Capability *cap,
                                   StgTRecHeader *outer) {
  StgTRecHeader *t;
  TRACE("%p : stmStartTransaction with %d tokens",
        outer,
        cap -> transaction_tokens);

  getToken(cap);

  t = alloc_stg_trec_header(cap, outer);
  TRACE("%p : stmStartTransaction()=%p", outer, t);
  return t;
}

/*......................................................................*/

void stmAbortTransaction(Capability *cap,
                         StgTRecHeader *trec) {
  StgTRecHeader *et;
  TRACE("%p : stmAbortTransaction", trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) ||
          (trec -> state == TREC_WAITING) ||
          (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);

  et = trec -> enclosing_trec;
  if (et == NO_TREC) {
    // We're a top-level transaction: remove any watch queue entries that
    // we may have.
    TRACE("%p : aborting top-level transaction", trec);

    if (trec -> state == TREC_WAITING) {
      ASSERT (trec -> enclosing_trec == NO_TREC);
      TRACE("%p : stmAbortTransaction aborting waiting transaction", trec);
      remove_watch_queue_entries_for_trec(cap, trec);
    }

  } else {
    // We're a nested transaction: merge our read set into our parent's
    TRACE("%p : retaining read-set into parent %p", trec, et);

    FOR_EACH_ENTRY(trec, e, {
      StgTVar *s = e -> tvar;
      merge_read_into(cap, et, s, e -> expected_value);
    });
  }

  trec -> state = TREC_ABORTED;
  unlock_stm(trec);

  TRACE("%p : stmAbortTransaction done", trec);
}

/*......................................................................*/

void stmFreeAbortedTRec(Capability *cap,
			StgTRecHeader *trec) {
  TRACE("%p : stmFreeAbortedTRec", trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_CONDEMNED) ||
	  (trec -> state == TREC_ABORTED));

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmFreeAbortedTRec done", trec);
}

/*......................................................................*/

void stmCondemnTransaction(Capability *cap,
                           StgTRecHeader *trec) {
  TRACE("%p : stmCondemnTransaction", trec);
  ASSERT (trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) ||
          (trec -> state == TREC_WAITING) ||
          (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);
  if (trec -> state == TREC_WAITING) {
    ASSERT (trec -> enclosing_trec == NO_TREC);
    TRACE("%p : stmCondemnTransaction condemning waiting transaction", trec);
    remove_watch_queue_entries_for_trec(cap, trec);
  }
  trec -> state = TREC_CONDEMNED;
  unlock_stm(trec);

  TRACE("%p : stmCondemnTransaction done", trec);
}

/*......................................................................*/

StgBool stmValidateNestOfTransactions(Capability *cap, StgTRecHeader *trec) {
  StgTRecHeader *t;
  StgBool result;

  TRACE("%p : stmValidateNestOfTransactions", trec);
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);

  t = trec;
  result = TRUE;
  while (t != NO_TREC) {
    result &= validate_and_acquire_ownership(cap, t, TRUE, FALSE);
    t = t -> enclosing_trec;
  }

  if (!result && trec -> state != TREC_WAITING) {
    trec -> state = TREC_CONDEMNED;
  }

  unlock_stm(trec);

  TRACE("%p : stmValidateNestOfTransactions()=%d", trec, result);
  return result;
}

/*......................................................................*/

static TRecEntry *get_entry_for(StgTRecHeader *trec, StgTVar *tvar, StgTRecHeader **in) {
  TRecEntry *result = NULL;

  TRACE("%p : get_entry_for TVar %p", trec, tvar);
  ASSERT(trec != NO_TREC);

  do {
    FOR_EACH_ENTRY(trec, e, {
      if (e -> tvar == tvar) {
        result = e;
        if (in != NULL) {
          *in = trec;
        }
        BREAK_FOR_EACH;
      }
    });
    trec = trec -> enclosing_trec;
  } while (result == NULL && trec != NO_TREC);

  return result;
}

/*......................................................................*/

/*
 * Add/remove links between an invariant TVars.  The caller must have
 * locked the TVars involved and the invariant.
 */

static void disconnect_invariant(Capability *cap,
				 StgAtomicInvariant *inv) {
  StgTRecHeader *last_execution = inv -> last_execution;

  TRACE("unhooking last execution inv=%p trec=%p", inv, last_execution);

  FOR_EACH_ENTRY(last_execution, e, {
    StgTVar *s = e -> tvar;
    StgTVarWatchQueue *q = s -> first_watch_queue_entry;
    DEBUG_ONLY( StgBool found = FALSE );
    TRACE("  looking for trec on tvar=%p", s);
    for (q = s -> first_watch_queue_entry;
	 q != END_STM_WATCH_QUEUE;
	 q = q -> next_queue_entry) {
      if (q -> closure == (StgClosure*)inv) {
	StgTVarWatchQueue *pq;
	StgTVarWatchQueue *nq;
	nq = q -> next_queue_entry;
	pq = q -> prev_queue_entry;
	if (nq != END_STM_WATCH_QUEUE) {
	  nq -> prev_queue_entry = pq;
	}
	if (pq != END_STM_WATCH_QUEUE) {
	  pq -> next_queue_entry = nq;
	} else {
	  ASSERT (s -> first_watch_queue_entry == q);
	  s -> first_watch_queue_entry = nq;
          dirty_TVAR(cap,s); // we modified first_watch_queue_entry
        }
	TRACE("  found it in watch queue entry %p", q);
	free_stg_tvar_watch_queue(cap, q);
	DEBUG_ONLY( found = TRUE );
	break;
      }
    }
    ASSERT(found);
  });
  inv -> last_execution = NO_TREC;
}

static void connect_invariant_to_trec(Capability *cap,
				      StgAtomicInvariant *inv,
				      StgTRecHeader *my_execution) {
  TRACE("connecting execution inv=%p trec=%p", inv, my_execution);

  ASSERT(inv -> last_execution == NO_TREC);

  FOR_EACH_ENTRY(my_execution, e, {
    StgTVar *s = e -> tvar;
    StgTVarWatchQueue *q = alloc_stg_tvar_watch_queue(cap, (StgClosure*)inv);
    StgTVarWatchQueue *fq = s -> first_watch_queue_entry;

    // We leave "last_execution" holding the values that will be
    // in the heap after the transaction we're in the process
    // of committing has finished.
    TRecEntry *entry = get_entry_for(my_execution -> enclosing_trec, s, NULL);
    if (entry != NULL) {
      e -> expected_value = entry -> new_value;
      e -> new_value = entry -> new_value;
    }

    TRACE("  linking trec on tvar=%p value=%p q=%p", s, e -> expected_value, q);
    q -> next_queue_entry = fq;
    q -> prev_queue_entry = END_STM_WATCH_QUEUE;
    if (fq != END_STM_WATCH_QUEUE) {
      fq -> prev_queue_entry = q;
    }
    s -> first_watch_queue_entry = q;
    dirty_TVAR(cap,s); // we modified first_watch_queue_entry
  });

  inv -> last_execution = my_execution;
}

/*
 * Add a new invariant to the trec's list of invariants to check on commit
 */
void stmAddInvariantToCheck(Capability *cap,
			    StgTRecHeader *trec,
			    StgClosure *code) {
  StgAtomicInvariant *invariant;
  StgInvariantCheckQueue *q;
  TRACE("%p : stmAddInvariantToCheck closure=%p", trec, code);
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> state == TREC_ACTIVE ||
	 trec -> state == TREC_CONDEMNED);


  // 1. Allocate an StgAtomicInvariant, set last_execution to NO_TREC
  //    to signal that this is a new invariant in the current atomic block

  invariant = (StgAtomicInvariant *) allocate(cap, sizeofW(StgAtomicInvariant));
  TRACE("%p : stmAddInvariantToCheck allocated invariant=%p", trec, invariant);
  SET_HDR (invariant, &stg_ATOMIC_INVARIANT_info, CCS_SYSTEM);
  invariant -> code = code;
  invariant -> last_execution = NO_TREC;
  invariant -> lock = 0;

  // 2. Allocate an StgInvariantCheckQueue entry, link it to the current trec

  q = alloc_stg_invariant_check_queue(cap, invariant);
  TRACE("%p : stmAddInvariantToCheck allocated q=%p", trec, q);
  q -> invariant = invariant;
  q -> my_execution = NO_TREC;
  q -> next_queue_entry = trec -> invariants_to_check;
  trec -> invariants_to_check = q;

  TRACE("%p : stmAddInvariantToCheck done", trec);
}

/*
 * Fill in the trec's list of invariants that might be violated by the
 * current transaction.
 */

StgInvariantCheckQueue *stmGetInvariantsToCheck(Capability *cap, StgTRecHeader *trec) {
  StgTRecChunk *c;
  TRACE("%p : stmGetInvariantsToCheck, head was %p",
	trec,
	trec -> invariants_to_check);

  ASSERT(trec != NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) ||
	  (trec -> state == TREC_WAITING) ||
	  (trec -> state == TREC_CONDEMNED));
  ASSERT(trec -> enclosing_trec == NO_TREC);

  lock_stm(trec);
  c = trec -> current_chunk;
  while (c != END_STM_CHUNK_LIST) {
    unsigned int i;
    for (i = 0; i < c -> next_entry_idx; i ++) {
      TRecEntry *e = &(c -> entries[i]);
      if (entry_is_update(e)) {
	StgTVar *s = e -> tvar;
	StgClosure *old = lock_tvar(trec, s);

	// Pick up any invariants on the TVar being updated
	// by entry "e"

	StgTVarWatchQueue *q;
	TRACE("%p : checking for invariants on %p", trec, s);
	for (q = s -> first_watch_queue_entry;
	     q != END_STM_WATCH_QUEUE;
	     q = q -> next_queue_entry) {
	  if (watcher_is_invariant(q)) {
	    StgBool found = FALSE;
	    StgInvariantCheckQueue *q2;
	    TRACE("%p : Touching invariant %p", trec, q -> closure);
	    for (q2 = trec -> invariants_to_check;
		 q2 != END_INVARIANT_CHECK_QUEUE;
		 q2 = q2 -> next_queue_entry) {
	      if (q2 -> invariant == (StgAtomicInvariant*)(q -> closure)) {
		TRACE("%p : Already found %p", trec, q -> closure);
		found = TRUE;
		break;
	      }
	    }

	    if (!found) {
	      StgInvariantCheckQueue *q3;
	      TRACE("%p : Not already found %p", trec, q -> closure);
	      q3 = alloc_stg_invariant_check_queue(cap,
						   (StgAtomicInvariant*) q -> closure);
	      q3 -> next_queue_entry = trec -> invariants_to_check;
	      trec -> invariants_to_check = q3;
	    }
	  }
	}

        unlock_tvar(cap, trec, s, old, FALSE);
      }
    }
    c = c -> prev_chunk;
  }

  unlock_stm(trec);

  TRACE("%p : stmGetInvariantsToCheck, head now %p",
	trec,
	trec -> invariants_to_check);

  return (trec -> invariants_to_check);
}

/*......................................................................*/

StgBool stmCommitTransaction(Capability *cap, StgTRecHeader *trec) {
  int result;
  StgInt64 max_commits_at_start = max_commits;
  StgBool touched_invariants;
  StgBool use_read_phase;

  TRACE("%p : stmCommitTransaction()", trec);
  ASSERT (trec != NO_TREC);

  lock_stm(trec);

  ASSERT (trec -> enclosing_trec == NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) ||
          (trec -> state == TREC_CONDEMNED));

  // touched_invariants is true if we've written to a TVar with invariants
  // attached to it, or if we're trying to add a new invariant to the system.

  touched_invariants = (trec -> invariants_to_check != END_INVARIANT_CHECK_QUEUE);

  // If we have touched invariants then (i) lock the invariant, and (ii) add
  // the invariant's read set to our own.  Step (i) is needed to serialize
  // concurrent transactions that attempt to make conflicting updates
  // to the invariant's trec (suppose it read from t1 and t2, and that one
  // concurrent transcation writes only to t1, and a second writes only to
  // t2).  Step (ii) is needed so that both transactions will lock t1 and t2
  // to gain access to their wait lists (and hence be able to unhook the
  // invariant from both tvars).

  if (touched_invariants) {
    StgInvariantCheckQueue *q = trec -> invariants_to_check;
    TRACE("%p : locking invariants", trec);
    while (q != END_INVARIANT_CHECK_QUEUE) {
      StgTRecHeader *inv_old_trec;
      StgAtomicInvariant *inv;
      TRACE("%p : locking invariant %p", trec, q -> invariant);
      inv = q -> invariant;
      if (!lock_inv(inv)) {
        TRACE("%p : failed to lock %p", trec, inv);
        trec -> state = TREC_CONDEMNED;
        break;
      }

      inv_old_trec = inv -> last_execution;
      if (inv_old_trec != NO_TREC) {
        StgTRecChunk *c = inv_old_trec -> current_chunk;
        while (c != END_STM_CHUNK_LIST) {
          unsigned int i;
          for (i = 0; i < c -> next_entry_idx; i ++) {
            TRecEntry *e = &(c -> entries[i]);
            TRACE("%p : ensuring we lock TVars for %p", trec, e -> tvar);
            merge_read_into (cap, trec, e -> tvar, e -> expected_value);
          }
          c = c -> prev_chunk;
        }
      }
      q = q -> next_queue_entry;
    }
    TRACE("%p : finished locking invariants", trec);
  }

  // Use a read-phase (i.e. don't lock TVars we've read but not updated) if
  // (i) the configuration lets us use a read phase, and (ii) we've not
  // touched or introduced any invariants.
  //
  // In principle we could extend the implementation to support a read-phase
  // and invariants, but it complicates the logic: the links between
  // invariants and TVars are managed by the TVar watch queues which are
  // protected by the TVar's locks.

  use_read_phase = ((config_use_read_phase) && (!touched_invariants));

  result = validate_and_acquire_ownership(cap, trec, (!use_read_phase), TRUE);
  if (result) {
    // We now know that all the updated locations hold their expected values.
    ASSERT (trec -> state == TREC_ACTIVE);

    if (use_read_phase) {
      StgInt64 max_commits_at_end;
      StgInt64 max_concurrent_commits;
      TRACE("%p : doing read check", trec);
      result = check_read_only(trec);
      TRACE("%p : read-check %s", trec, result ? "succeeded" : "failed");

      max_commits_at_end = max_commits;
      max_concurrent_commits = ((max_commits_at_end - max_commits_at_start) +
                                (n_capabilities * TOKEN_BATCH_SIZE));
      if (((max_concurrent_commits >> 32) > 0) || shake()) {
        result = FALSE;
      }
    }

    if (result) {
      // We now know that all of the read-only locations held their exepcted values
      // at the end of the call to validate_and_acquire_ownership.  This forms the
      // linearization point of the commit.

      // 1. If we have touched or introduced any invariants then unhook them
      //    from the TVars they depended on last time they were executed
      //    and hook them on the TVars that they now depend on.
      if (touched_invariants) {
        StgInvariantCheckQueue *q = trec -> invariants_to_check;
        while (q != END_INVARIANT_CHECK_QUEUE) {
          StgAtomicInvariant *inv = q -> invariant;
          if (inv -> last_execution != NO_TREC) {
            disconnect_invariant(cap, inv);
          }

          TRACE("%p : hooking up new execution trec=%p", trec, q -> my_execution);
          connect_invariant_to_trec(cap, inv, q -> my_execution);

          TRACE("%p : unlocking invariant %p", trec, inv);
          unlock_inv(inv);

          q = q -> next_queue_entry;
        }
      }

      // 2. Make the updates required by the transaction
      FOR_EACH_ENTRY(trec, e, {
          StgTVar *s;
          s = e -> tvar;
          if ((!use_read_phase) || (e -> new_value != e -> expected_value)) {
          // Either the entry is an update or we're not using a read phase:
          // write the value back to the TVar, unlocking it if necessary.

          ACQ_ASSERT(tvar_is_locked(s, trec));
          TRACE("%p : writing %p to %p, waking waiters", trec, e -> new_value, s);
          unpark_waiters_on(cap,s);
          IF_STM_FG_LOCKS({
                          s -> num_updates ++;
                          });
          unlock_tvar(cap, trec, s, e -> new_value, TRUE);
          }
          ACQ_ASSERT(!tvar_is_locked(s, trec));
          });
    } else {
      revert_ownership(cap, trec, FALSE);
    }
  }

  unlock_stm(trec);

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmCommitTransaction()=%d", trec, result);

  return result;
}

/*......................................................................*/

StgBool stmCommitNestedTransaction(Capability *cap, StgTRecHeader *trec) {
  StgTRecHeader *et;
  int result;
  ASSERT (trec != NO_TREC && trec -> enclosing_trec != NO_TREC);
  TRACE("%p : stmCommitNestedTransaction() into %p", trec, trec -> enclosing_trec);
  ASSERT ((trec -> state == TREC_ACTIVE) || (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);

  et = trec -> enclosing_trec;
  result = validate_and_acquire_ownership(cap, trec, (!config_use_read_phase), TRUE);
  if (result) {
    // We now know that all the updated locations hold their expected values.

    if (config_use_read_phase) {
      TRACE("%p : doing read check", trec);
      result = check_read_only(trec);
    }
    if (result) {
      // We now know that all of the read-only locations held their exepcted values
      // at the end of the call to validate_and_acquire_ownership.  This forms the
      // linearization point of the commit.

      TRACE("%p : read-check succeeded", trec);
      FOR_EACH_ENTRY(trec, e, {
	// Merge each entry into the enclosing transaction record, release all
	// locks.

	StgTVar *s;
	s = e -> tvar;
	if (entry_is_update(e)) {
            unlock_tvar(cap, trec, s, e -> expected_value, FALSE);
	}
	merge_update_into(cap, et, s, e -> expected_value, e -> new_value);
	ACQ_ASSERT(s -> current_value != (StgClosure *)trec);
      });
    } else {
        revert_ownership(cap, trec, FALSE);
    }
  }

  unlock_stm(trec);

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmCommitNestedTransaction()=%d", trec, result);

  return result;
}

/*......................................................................*/

StgBool stmWait(Capability *cap, StgTSO *tso, StgTRecHeader *trec) {
  int result;
  TRACE("%p : stmWait(%p)", trec, tso);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> enclosing_trec == NO_TREC);
  ASSERT ((trec -> state == TREC_ACTIVE) ||
          (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);
  result = validate_and_acquire_ownership(cap, trec, TRUE, TRUE);
  if (result) {
    // The transaction is valid so far so we can actually start waiting.
    // (Otherwise the transaction was not valid and the thread will have to
    // retry it).

    // Put ourselves to sleep.  We retain locks on all the TVars involved
    // until we are sound asleep : (a) on the wait queues, (b) BlockedOnSTM
    // in the TSO, (c) TREC_WAITING in the Trec.
    build_watch_queue_entries_for_trec(cap, tso, trec);
    park_tso(tso);
    trec -> state = TREC_WAITING;

    // We haven't released ownership of the transaction yet.  The TSO
    // has been put on the wait queue for the TVars it is waiting for,
    // but we haven't yet tidied up the TSO's stack and made it safe
    // to wake up the TSO.  Therefore, we must wait until the TSO is
    // safe to wake up before we release ownership - when all is well,
    // the runtime will call stmWaitUnlock() below, with the same
    // TRec.

  } else {
    unlock_stm(trec);
    free_stg_trec_header(cap, trec);
  }

  TRACE("%p : stmWait(%p)=%d", trec, tso, result);
  return result;
}


void
stmWaitUnlock(Capability *cap, StgTRecHeader *trec) {
    revert_ownership(cap, trec, TRUE);
    unlock_stm(trec);
}

/*......................................................................*/

StgBool stmReWait(Capability *cap, StgTSO *tso) {
  int result;
  StgTRecHeader *trec = tso->trec;

  TRACE("%p : stmReWait", trec);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> enclosing_trec == NO_TREC);
  ASSERT ((trec -> state == TREC_WAITING) ||
          (trec -> state == TREC_CONDEMNED));

  lock_stm(trec);
  result = validate_and_acquire_ownership(cap, trec, TRUE, TRUE);
  TRACE("%p : validation %s", trec, result ? "succeeded" : "failed");
  if (result) {
    // The transaction remains valid -- do nothing because it is already on
    // the wait queues
    ASSERT (trec -> state == TREC_WAITING);
    park_tso(tso);
    revert_ownership(cap, trec, TRUE);
  } else {
    // The transcation has become invalid.  We can now remove it from the wait
    // queues.
    if (trec -> state != TREC_CONDEMNED) {
      remove_watch_queue_entries_for_trec (cap, trec);
    }
    free_stg_trec_header(cap, trec);
  }
  unlock_stm(trec);

  TRACE("%p : stmReWait()=%d", trec, result);
  return result;
}

/*......................................................................*/

static StgClosure *read_current_value(StgTRecHeader *trec STG_UNUSED, StgTVar *tvar) {
  StgClosure *result;
  result = tvar -> current_value;

#if defined(STM_FG_LOCKS)
  while (GET_INFO(UNTAG_CLOSURE(result)) == &stg_TREC_HEADER_info) {
    TRACE("%p : read_current_value(%p) saw %p", trec, tvar, result);
    result = tvar -> current_value;
  }
#endif

  TRACE("%p : read_current_value(%p)=%p", trec, tvar, result);
  return result;
}

/*......................................................................*/

StgClosure *stmReadTVar(Capability *cap,
                        StgTRecHeader *trec,
			StgTVar *tvar) {
  StgTRecHeader *entry_in = NULL;
  StgClosure *result = NULL;
  TRecEntry *entry = NULL;
  TRACE("%p : stmReadTVar(%p)", trec, tvar);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> state == TREC_ACTIVE ||
          trec -> state == TREC_CONDEMNED);

  entry = get_entry_for(trec, tvar, &entry_in);

  if (entry != NULL) {
    if (entry_in == trec) {
      // Entry found in our trec
      result = entry -> new_value;
    } else {
      // Entry found in another trec
      TRecEntry *new_entry = get_new_entry(cap, trec);
      new_entry -> tvar = tvar;
      new_entry -> expected_value = entry -> expected_value;
      new_entry -> new_value = entry -> new_value;
      result = new_entry -> new_value;
    }
  } else {
    // No entry found
    StgClosure *current_value = read_current_value(trec, tvar);
    TRecEntry *new_entry = get_new_entry(cap, trec);
    new_entry -> tvar = tvar;
    new_entry -> expected_value = current_value;
    new_entry -> new_value = current_value;
    result = current_value;
  }

  TRACE("%p : stmReadTVar(%p)=%p", trec, tvar, result);
  return result;
}

/*......................................................................*/

void stmWriteTVar(Capability *cap,
                  StgTRecHeader *trec,
		  StgTVar *tvar,
		  StgClosure *new_value) {

  StgTRecHeader *entry_in = NULL;
  TRecEntry *entry = NULL;
  TRACE("%p : stmWriteTVar(%p, %p)", trec, tvar, new_value);
  ASSERT (trec != NO_TREC);
  ASSERT (trec -> state == TREC_ACTIVE ||
          trec -> state == TREC_CONDEMNED);

  entry = get_entry_for(trec, tvar, &entry_in);

  if (entry != NULL) {
    if (entry_in == trec) {
      // Entry found in our trec
      entry -> new_value = new_value;
    } else {
      // Entry found in another trec
      TRecEntry *new_entry = get_new_entry(cap, trec);
      new_entry -> tvar = tvar;
      new_entry -> expected_value = entry -> expected_value;
      new_entry -> new_value = new_value;
    }
  } else {
    // No entry found
    StgClosure *current_value = read_current_value(trec, tvar);
    TRecEntry *new_entry = get_new_entry(cap, trec);
    new_entry -> tvar = tvar;
    new_entry -> expected_value = current_value;
    new_entry -> new_value = new_value;
  }

  TRACE("%p : stmWriteTVar done", trec);
}

/*......................................................................*/

StgTVar *stmNewTVar(Capability *cap,
                    StgClosure *new_value) {
  StgTVar *result;
  result = (StgTVar *)allocate(cap, sizeofW(StgTVar));
  SET_HDR (result, &stg_TVAR_DIRTY_info, CCS_SYSTEM);
  result -> current_value = new_value;
  result -> first_watch_queue_entry = END_STM_WATCH_QUEUE;
#if defined(THREADED_RTS)
  result -> num_updates = 0;
#endif
  return result;
}

/*......................................................................*/
