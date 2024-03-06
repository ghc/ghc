/* -----------------------------------------------------------------------------
 * (c) The GHC Team 1998-2005
 *
 * STM implementation.
 *
 * Overview
 * --------
 *
 * See the PPoPP 2005 paper "Composable memory transactions".  In summary, each
 * transaction has a TRec (transaction record) holding entries for each of the
 * TVars (transactional variables) that it has accessed.  Each entry records (a)
 * the TVar, (b) the expected value seen in the TVar, (c) the new value that the
 * transaction wants to write to the TVar, (d) during commit, the identity of
 * the TRec that wrote the expected value.
 *
 * Separate TRecs are used for each level in a nest of transactions.  This
 * allows a nested transaction to be aborted without condemning its enclosing
 * transactions.  This is needed in the implementation of catchRetry.  Note that
 * the "expected value" in a nested transaction's TRec is the value expected to
 * be *held in memory* if the transaction commits -- not the "new value" stored
 * in one of the enclosing transactions.  This means that validation can be done
 * without searching through a nest of TRecs.
 *
 * Concurrency control
 * -------------------
 *
 * Three different concurrency control schemes can be built according to the
 * settings in STM.h:
 *
 * STM_UNIPROC assumes that the caller serialises invocations on the STM
 * interface.  In the Haskell RTS this means it is suitable only for
 * non-THREADED_RTS builds.
 *
 * STM_CG_LOCK was a historic locking mode using coarse-grained locking
 * It has been removed, look at the git history if you are interest in it.
 *
 * STM_FG_LOCKS uses fine-grained locking -- locking is done on a per-TVar basis
 * and, when committing a transaction, no locks are acquired for TVars that have
 * been read but not updated.
 *
 * Concurrency control is implemented in the functions:
 *
 *    lock_tvar / cond_lock_tvar
 *    unlock_tvar
 *
 * The choice between STM_UNIPROC / STM_FG_LOCKS affects the
 * implementation of these functions.
 *
 * lock_tvar / cond_lock_tvar and unlock_tvar are more complex because they have
 * other effects (present in STM_UNIPROC builds) as well as the
 * actual business of manipulating a lock (present only in STM_FG_LOCKS builds).
 * This is because locking a TVar is implemented by writing the lock holder's
 * TRec into the TVar's current_value field:
 *
 *   lock_tvar - lock a specified TVar (STM_FG_LOCKS only), returning the value
 *               it contained.
 *
 *   cond_lock_tvar - lock a specified TVar (STM_FG_LOCKS only) if it
 *               contains a specified value.  Return true if this succeeds,
 *               false otherwise.
 *
 *   unlock_tvar - release the lock on a specified TVar (STM_FG_LOCKS only),
 *               storing a specified value in place of the lock entry.
 *
 * Using these operations, the typical pattern of a commit/validate/wait
 * operation is to (a) lock the STM, (b) lock all the TVars being updated, (c)
 * check that the TVars that were only read from still contain their expected
 * values, (d) release the locks on the TVars, writing updates to them in the
 * case of a commit, (e) unlock the STM.
 *
 * Queues of waiting threads hang off the first_watch_queue_entry field of each
 * TVar.  This may only be manipulated when holding that TVar's lock.  In
 * particular, when a thread is putting itself to sleep, it mustn't release the
 * TVar's lock until it has added itself to the wait queue and marked its TSO as
 * BlockedOnSTM -- this makes sure that other threads will know to wake it.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"

#include "RtsUtils.h"
#include "Schedule.h"
#include "STM.h"
#include "Trace.h"
#include "Threads.h"
#include "sm/Storage.h"
#include "SMPClosureOps.h"

#include <stdio.h>

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

#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x)

// If SHAKE is defined then validation will sometimes spuriously fail.  They help test
// unusual code paths if genuine contention is rare
#if defined(SHAKE)
static int shake_ctr = 0;
static int shake_lim = 1;

static int shake(void) {
    if (((shake_ctr++) % shake_lim) == 0) {
      shake_ctr = 1;
      shake_lim ++;
      return true;
    }
    return false;
}
#else
static int shake(void) {
    return false;
}
#endif

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
  if (false) goto exit_for_each;                                                \
} while (0)

#define BREAK_FOR_EACH goto exit_for_each

/*......................................................................*/

// if REUSE_MEMORY is defined then attempt to re-use descriptors, log chunks,
// and wait queue entries without GC

#define REUSE_MEMORY

/*......................................................................*/

#define IF_STM_UNIPROC(__X)  do { } while (0)
#define IF_STM_FG_LOCKS(__X) do { } while (0)

#if defined(STM_UNIPROC)
#undef IF_STM_UNIPROC
#define IF_STM_UNIPROC(__X)  do { __X } while (0)
static const StgBool config_use_read_phase = false;

static StgClosure *lock_tvar(Capability *cap STG_UNUSED,
                             StgTRecHeader *trec STG_UNUSED,
                             StgTVar *s STG_UNUSED) {
  StgClosure *result;
  TRACE("%p : lock_tvar(%p)", trec, s);
  result = ACQUIRE_LOAD(&s->current_value);
  return result;
}

static void unlock_tvar(Capability *cap,
                        StgTRecHeader *trec STG_UNUSED,
                        StgTVar *s,
                        StgClosure *c,
                        StgBool force_update) {
  TRACE("%p : unlock_tvar(%p)", trec, s);
  if (force_update) {
    StgClosure *old_value = ACQUIRE_LOAD(&s->current_value);
    RELEASE_STORE(&s->current_value, c);
    dirty_TVAR(cap, s, old_value);
  }
}

static StgBool cond_lock_tvar(Capability *cap STG_UNUSED,
                              StgTRecHeader *trec STG_UNUSED,
                              StgTVar *s STG_UNUSED,
                              StgClosure *expected) {
  StgClosure *result;
  // TRACE("%p : cond_lock_tvar(%p, %p)", trec, s, expected);
  result = ACQUIRE_LOAD(&s->current_value);
  // TRACE("%p : %s", trec, (result == expected) ? "success" : "failure");
  return (result == expected);
}
#endif

#if defined(STM_FG_LOCKS) /*...................................*/

#undef IF_STM_FG_LOCKS
#define IF_STM_FG_LOCKS(__X) do { __X } while (0)
static const StgBool config_use_read_phase = true;

static StgClosure *lock_tvar(Capability *cap,
                             StgTRecHeader *trec,
                             StgTVar *s STG_UNUSED) {
  StgClosure *result;
  // TRACE("%p : lock_tvar(%p)", trec, s);
  do {
    const StgInfoTable *info;
    do {
      result = ACQUIRE_LOAD(&s->current_value);
      info = GET_INFO(UNTAG_CLOSURE(result));
    } while (info == &stg_TREC_HEADER_info);
  } while (cas((void *) &s->current_value,
               (StgWord)result, (StgWord)trec) != (StgWord)result);


  IF_NONMOVING_WRITE_BARRIER_ENABLED {
      if (result)
          updateRemembSetPushClosure(cap, result);
  }
  return result;
}

static void unlock_tvar(Capability *cap,
                        StgTRecHeader *trec STG_UNUSED,
                        StgTVar *s,
                        StgClosure *c,
                        StgBool force_update STG_UNUSED) {
  // TRACE("%p : unlock_tvar(%p, %p)", trec, s, c);
  ASSERT(ACQUIRE_LOAD(&s->current_value) == (StgClosure *)trec);
  RELEASE_STORE(&s->current_value, c);
  dirty_TVAR(cap, s, (StgClosure *) trec);
}

static StgBool cond_lock_tvar(Capability *cap,
                              StgTRecHeader *trec,
                              StgTVar *s,
                              StgClosure *expected) {
  StgClosure *result;
  StgWord w;
  // TRACE("%p : cond_lock_tvar(%p, %p)", trec, s, expected);
  w = cas((void *)&(s -> current_value), (StgWord)expected, (StgWord)trec);
  result = (StgClosure *)w;
  IF_NONMOVING_WRITE_BARRIER_ENABLED {
      if (result)
          updateRemembSetPushClosure(cap, expected);
  }
  // TRACE("%p : %s", trec, result ? "success" : "failure");
  return (result == expected);
}
#endif

/*......................................................................*/

// Helper functions for thread blocking and unblocking

static void park_tso(StgTSO *tso) {
  ASSERT(tso -> why_blocked == NotBlocked);
  tso -> block_info.closure = (StgClosure *) END_TSO_QUEUE;
  RELEASE_STORE(&tso -> why_blocked, BlockedOnSTM);
  TRACE("park_tso on tso=%p", tso);
}

static void unpark_tso(Capability *cap, StgTSO *tso) {
    // We will continue unparking threads while they remain on one of the wait
    // queues: it's up to the thread itself to remove it from the wait queues
    // if it decides to do so when it is scheduled.

    // Only the capability that owns this TSO may unblock it. We can
    // call tryWakeupThread() which will either unblock it directly if
    // it belongs to this cap, or send a message to the owning cap
    // otherwise.

    // TODO: This sends multiple messages if we write to the same TVar multiple
    // times and the owning cap hasn't yet woken up the thread and removed it
    // from the TVar's watch list. We tried to optimise this in D4961, but that
    // patch was incorrect and broke other things, see #15544 comment:17. See
    // #15626 for the tracking ticket.

    // Safety Note: we hold the TVar lock at this point, so we know
    // that this thread is definitely still blocked, since the first
    // thing a thread will do when it runs is remove itself from the
    // TVar watch queues, and to do that it would need to lock the
    // TVar.

    tryWakeupThread(cap,tso);
}

static void unpark_waiters_on(Capability *cap, StgTVar *s) {
  StgTVarWatchQueue *q;
  StgTVarWatchQueue *trail;
  TRACE("unpark_waiters_on tvar=%p", s);
  // unblock TSOs in reverse order, to be a bit fairer (#2319)
  for (q = ACQUIRE_LOAD(&s->first_watch_queue_entry), trail = q;
       q != END_STM_WATCH_QUEUE;
       q = q -> next_queue_entry) {
    trail = q;
  }
  q = trail;
  for (;
       q != END_STM_WATCH_QUEUE;
       q = q -> prev_queue_entry) {
      unpark_tso(cap, (StgTSO *)(q -> closure));
  }
}

/*......................................................................*/

// Helper functions for downstream allocation and initialization

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
    ACQ_ASSERT(ACQUIRE_LOAD(&s->current_value) == (StgClosure *)trec);
    NACQ_ASSERT(ACQUIRE_LOAD(&s->current_value) == e -> expected_value);
    fq = ACQUIRE_LOAD(&s->first_watch_queue_entry);
    q = alloc_stg_tvar_watch_queue(cap, (StgClosure*) tso);
    q -> next_queue_entry = fq;
    q -> prev_queue_entry = END_STM_WATCH_QUEUE;
    if (fq != END_STM_WATCH_QUEUE) {
      fq -> prev_queue_entry = q;
    }
    RELEASE_STORE(&s->first_watch_queue_entry, q);
    e -> new_value = (StgClosure *) q;
    dirty_TVAR(cap, s, (StgClosure *) fq); // we modified first_watch_queue_entry
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
    saw = lock_tvar(cap, trec, s);
    q = (StgTVarWatchQueue *) (e -> new_value);
    TRACE("%p : removing tso=%p from watch queue for tvar=%p",
          trec,
          q -> closure,
          s);
    ACQ_ASSERT(ACQUIRE_LOAD(&s->current_value) == (StgClosure *)trec);
    nq = q -> next_queue_entry;
    pq = q -> prev_queue_entry;
    if (nq != END_STM_WATCH_QUEUE) {
      nq -> prev_queue_entry = pq;
    }
    if (pq != END_STM_WATCH_QUEUE) {
      pq -> next_queue_entry = nq;
    } else {
      ASSERT(ACQUIRE_LOAD(&s->first_watch_queue_entry) == q);
      RELEASE_STORE(&s->first_watch_queue_entry, nq);
      dirty_TVAR(cap, s, (StgClosure *) q); // we modified first_watch_queue_entry
    }
    free_stg_tvar_watch_queue(cap, q);
    unlock_tvar(cap, trec, s, saw, false);
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
                              StgClosure *new_value)
{
  // Look for an entry in this trec
  bool found = false;
  FOR_EACH_ENTRY(t, e, {
    StgTVar *s;
    s = e -> tvar;
    if (s == tvar) {
      found = true;
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
  StgTRecHeader *t;
  bool found = false;

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
        found = true;
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
  c = ACQUIRE_LOAD(&s->current_value);
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
          unlock_tvar(cap, trec, s, e -> expected_value, true);
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
  TRACE("cap %d, trec %p : validate_and_acquire_ownership, all: %d, retrain: %d",
         cap->no, trec, acquire_all, retain_ownership);

  if (shake()) {
    TRACE("%p : shake, pretending trec is invalid when it may not be", trec);
    return false;
  }

  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));
  result = !((trec -> state) == TREC_CONDEMNED);
  if (result) {
    FOR_EACH_ENTRY(trec, e, {
      StgTVar *s;
      s = e -> tvar;
      if (acquire_all || entry_is_update(e)) {
        TRACE("%p : trying to acquire %p", trec, s);
        if (!cond_lock_tvar(cap, trec, s, e -> expected_value)) {
          TRACE("%p : failed to acquire %p", trec, s);
          result = false;
          BREAK_FOR_EACH;
        }
      } else {
        ASSERT(config_use_read_phase);
        IF_STM_FG_LOCKS({
          TRACE("%p : will need to check %p", trec, s);
          // The memory ordering here must ensure that we have two distinct
          // reads to current_value, with the read from num_updates between
          // them.
          if (ACQUIRE_LOAD(&s->current_value) != e -> expected_value) {
            TRACE("%p : doesn't match", trec);
            result = false;
            BREAK_FOR_EACH;
          }
          e->num_updates = SEQ_CST_LOAD(&s->num_updates);
          if (ACQUIRE_LOAD(&s->current_value) != e -> expected_value) {
            TRACE("%p : doesn't match (race)", trec);
            result = false;
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

  // TRACE("%p : validate_and_acquire_ownership, result: %d", trec, result);
  return result;
}

// check_read_only : check that we've seen an atomic snapshot of the
// non-updated TVars accessed by a trec.  This checks that the last TRec to
// commit an update to the TVar is unchanged since the value was stashed in
// validate_and_acquire_ownership.  If no update is seen to any TVar then
// all of them contained their expected values at the start of the call to
// check_read_only.
//
// The paper "Concurrent programming without locks" (under submission), or
// Keir Fraser's PhD dissertation "Practical lock-free programming" discuss
// this kind of algorithm.

static StgBool check_read_only(StgTRecHeader *trec STG_UNUSED) {
  StgBool result = true;

  ASSERT(config_use_read_phase);
  IF_STM_FG_LOCKS({
    FOR_EACH_ENTRY(trec, e, {
      StgTVar *s;
      s = e -> tvar;
      if (entry_is_read_only(e)) {
        TRACE("%p : check_read_only for TVar %p, saw %ld", trec, s, e -> num_updates);

        // We must first load current_value then num_updates; this is inverse of
        // the order of the stores in stmCommitTransaction.
        StgClosure *current_value = ACQUIRE_LOAD(&s->current_value);
        StgInt num_updates = SEQ_CST_LOAD(&s->num_updates);

        // Note we need both checks and in this order as the TVar could be
        // locked by another transaction that is committing but has not yet
        // incremented `num_updates` (See #7815).
        if (current_value != e->expected_value ||
            num_updates != e->num_updates) {
          TRACE("%p : mismatch", trec);
          result = false;
          BREAK_FOR_EACH;
        }
      }
    });
  });

  return result;
}


/************************************************************************/

void stmPreGCHook (Capability *cap) {
  TRACE("stmPreGCHook");
  cap->free_tvar_watch_queues = END_STM_WATCH_QUEUE;
  cap->free_trec_chunks = END_STM_CHUNK_LIST;
  cap->free_trec_headers = NO_TREC;
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

#if defined(THREADED_RTS)

static volatile StgInt64 max_commits = 0;

static volatile StgWord token_locked = false;

static StgInt64 getMaxCommits(void) {
  return RELAXED_LOAD(&max_commits);
}

static void getTokenBatch(Capability *cap) {
  while (cas((void *)&token_locked, false, true) == true) { /* nothing */ }
  NONATOMIC_ADD(&max_commits, TOKEN_BATCH_SIZE);
  TRACE("%p : cap got token batch, max_commits=%" FMT_Int64, cap, RELAXED_LOAD(&max_commits));
  cap -> transaction_tokens = TOKEN_BATCH_SIZE;
  RELEASE_STORE(&token_locked, false);
}

static void getToken(Capability *cap) {
  if (cap -> transaction_tokens == 0) {
    getTokenBatch(cap);
  }
  cap -> transaction_tokens --;
}
#else
static StgInt64 getMaxCommits(void) {
    return 0;
}

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
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));

  et = trec -> enclosing_trec;
  if (et == NO_TREC) {
    // We're a top-level transaction: remove any watch queue entries that
    // we may have.
    TRACE("%p : aborting top-level transaction", trec);

    if (trec -> state == TREC_WAITING) {
      ASSERT(trec -> enclosing_trec == NO_TREC);
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
  TRACE("%p : stmAbortTransaction done", trec);
}

/*......................................................................*/

void stmFreeAbortedTRec(Capability *cap,
                        StgTRecHeader *trec) {
  TRACE("%p : stmFreeAbortedTRec", trec);
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_CONDEMNED) ||
         (trec -> state == TREC_ABORTED));

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmFreeAbortedTRec done", trec);
}

/*......................................................................*/

void stmCondemnTransaction(Capability *cap,
                           StgTRecHeader *trec) {
  TRACE("%p : stmCondemnTransaction", trec);
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));

  if (trec -> state == TREC_WAITING) {
    ASSERT(trec -> enclosing_trec == NO_TREC);
    TRACE("%p : stmCondemnTransaction condemning waiting transaction", trec);
    remove_watch_queue_entries_for_trec(cap, trec);
  }
  trec -> state = TREC_CONDEMNED;

  TRACE("%p : stmCondemnTransaction done", trec);
}

/*......................................................................*/

// Check if a transaction is known to be invalid by this point.
// Currently we use this to:
// * Eagerly abort invalid transactions from the scheduler.
// * If an exception occured inside a transaction, decide weither or not to
//   abort by checking if the transaction was valid.
StgBool stmValidateNestOfTransactions(Capability *cap, StgTRecHeader *trec) {
  StgTRecHeader *t;

  TRACE("%p : stmValidateNestOfTransactions", trec);
  ASSERT(trec != NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));

  t = trec;
  StgBool result = true;
  while (t != NO_TREC) {
    // TODO: I don't think there is a need to lock any tvars here, all even less so.
    result &= validate_and_acquire_ownership(cap, t, true, false);
    t = t -> enclosing_trec;
  }

  if (!result && trec -> state != TREC_WAITING) {
    trec -> state = TREC_CONDEMNED;
  }

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

StgBool stmCommitTransaction(Capability *cap, StgTRecHeader *trec) {
  StgInt64 max_commits_at_start = getMaxCommits();

  TRACE("%p : stmCommitTransaction()", trec);
  ASSERT(trec != NO_TREC);

  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_CONDEMNED));

  // Use a read-phase (i.e. don't lock TVars we've read but not updated) if
  // the configuration lets us use a read phase.

  bool result = validate_and_acquire_ownership(cap, trec, (!config_use_read_phase), true);
  if (result) {
    // We now know that all the updated locations hold their expected values.
    ASSERT(trec -> state == TREC_ACTIVE);

    if (config_use_read_phase) {
      StgInt64 max_commits_at_end;
      StgInt64 max_concurrent_commits;
      TRACE("%p : doing read check", trec);
      result = check_read_only(trec);
      TRACE("%p : read-check %s", trec, result ? "succeeded" : "failed");

      max_commits_at_end = getMaxCommits();
      max_concurrent_commits = ((max_commits_at_end - max_commits_at_start) +
                                (getNumCapabilities() * TOKEN_BATCH_SIZE));
      if (((max_concurrent_commits >> 32) > 0) || shake()) {
        TRACE("STM - Max commit number exceeded");
        result = false;
      }
    }

    if (result) {
      // We now know that all of the read-only locations held their expected values
      // at the end of the call to validate_and_acquire_ownership.  This forms the
      // linearization point of the commit.

      // Make the updates required by the transaction.
      FOR_EACH_ENTRY(trec, e, {
        StgTVar *s;
        s = e -> tvar;
        if ((!config_use_read_phase) || (e -> new_value != e -> expected_value)) {
          // Either the entry is an update or we're not using a read phase:
          // write the value back to the TVar, unlocking it if necessary.

          ACQ_ASSERT(tvar_is_locked(s, trec));
          TRACE("%p : writing %p to %p, waking waiters", trec, e -> new_value, s);
          unpark_waiters_on(cap,s);
          IF_STM_FG_LOCKS({
            // We have locked the TVar therefore nonatomic addition is sufficient
            NONATOMIC_ADD(&s->num_updates, 1);
          });
          unlock_tvar(cap, trec, s, e -> new_value, true);
        }
        ACQ_ASSERT(!tvar_is_locked(s, trec));
      });
    } else {
        revert_ownership(cap, trec, false);
    }
  }

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmCommitTransaction()=%d", trec, result);

  return result;
}

/*......................................................................*/

StgBool stmCommitNestedTransaction(Capability *cap, StgTRecHeader *trec) {
  StgTRecHeader *et;
  ASSERT(trec != NO_TREC && trec -> enclosing_trec != NO_TREC);
  TRACE("%p : stmCommitNestedTransaction() into %p", trec, trec -> enclosing_trec);
  ASSERT((trec -> state == TREC_ACTIVE) || (trec -> state == TREC_CONDEMNED));

  et = trec -> enclosing_trec;
  bool result = validate_and_acquire_ownership(cap, trec, (!config_use_read_phase), true);
  if (result) {
    // We now know that all the updated locations hold their expected values.

    if (config_use_read_phase) {
      TRACE("%p : doing read check", trec);
      result = check_read_only(trec);
    }
    if (result) {
      // We now know that all of the read-only locations held their expected values
      // at the end of the call to validate_and_acquire_ownership.  This forms the
      // linearization point of the commit.

      TRACE("%p : read-check succeeded", trec);
      FOR_EACH_ENTRY(trec, e, {
        // Merge each entry into the enclosing transaction record, release all
        // locks.

        StgTVar *s;
        s = e -> tvar;
        if (entry_is_update(e)) {
            unlock_tvar(cap, trec, s, e -> expected_value, false);
        }
        merge_update_into(cap, et, s, e -> expected_value, e -> new_value);
        ACQ_ASSERT(ACQUIRE_LOAD(&s->current_value) != (StgClosure *)trec);
      });
    } else {
        revert_ownership(cap, trec, false);
    }
  }

  free_stg_trec_header(cap, trec);

  TRACE("%p : stmCommitNestedTransaction()=%d", trec, result);

  return result;
}

/*......................................................................*/

StgBool stmWait(Capability *cap, StgTSO *tso, StgTRecHeader *trec) {
  TRACE("%p : stmWait(%p)", trec, tso);
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT((trec -> state == TREC_ACTIVE) ||
         (trec -> state == TREC_CONDEMNED));

  bool result = validate_and_acquire_ownership(cap, trec, true, true);
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
    free_stg_trec_header(cap, trec);
  }

  TRACE("%p : stmWait(%p)=%d", trec, tso, result);
  return result;
}


void
stmWaitUnlock(Capability *cap, StgTRecHeader *trec) {
    revert_ownership(cap, trec, true);
}

/*......................................................................*/

StgBool stmReWait(Capability *cap, StgTSO *tso) {
  StgTRecHeader *trec = tso->trec;

  TRACE("%p : stmReWait", trec);
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> enclosing_trec == NO_TREC);
  ASSERT((trec -> state == TREC_WAITING) ||
         (trec -> state == TREC_CONDEMNED));

  bool result = validate_and_acquire_ownership(cap, trec, true, true);
  TRACE("%p : validation %s", trec, result ? "succeeded" : "failed");
  if (result) {
    // The transaction remains valid -- do nothing because it is already on
    // the wait queues
    ASSERT(trec -> state == TREC_WAITING);
    park_tso(tso);
    revert_ownership(cap, trec, true);
  } else {
    // The transaction has become invalid.  We can now remove it from the wait
    // queues.
    if (trec -> state != TREC_CONDEMNED) {
      remove_watch_queue_entries_for_trec (cap, trec);
    }
    free_stg_trec_header(cap, trec);
  }

  TRACE("%p : stmReWait()=%d", trec, result);
  return result;
}

/*......................................................................*/

static StgClosure *read_current_value(StgTRecHeader *trec STG_UNUSED, StgTVar *tvar) {
  StgClosure *result;
  result = ACQUIRE_LOAD(&tvar->current_value);

#if defined(STM_FG_LOCKS)
  while (GET_INFO(UNTAG_CLOSURE(result)) == &stg_TREC_HEADER_info) {
    TRACE("%p : read_current_value(%p) saw %p", trec, tvar, result);
    result = ACQUIRE_LOAD(&tvar->current_value);
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
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> state == TREC_ACTIVE ||
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
  ASSERT(trec != NO_TREC);
  ASSERT(trec -> state == TREC_ACTIVE ||
         trec -> state == TREC_CONDEMNED);

  entry = get_entry_for(trec, tvar, &entry_in);

  if (entry != NULL) {
    if (entry_in == trec) {
      // Entry found in our trec
      IF_NONMOVING_WRITE_BARRIER_ENABLED {
        updateRemembSetPushClosure(cap, (StgClosure *) entry->new_value);
      }
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
