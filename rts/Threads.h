/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#ifndef THREADS_H
#define THREADS_H

StgTSO * unblockOne (Capability *cap, StgTSO *tso);
StgTSO * unblockOne_ (Capability *cap, StgTSO *tso, rtsBool allow_migrate);

void awakenBlockedQueue (Capability *cap, StgTSO *tso);

void removeThreadFromMVarQueue (Capability *cap, StgMVar *mvar, StgTSO *tso);
void removeThreadFromQueue     (Capability *cap, StgTSO **queue, StgTSO *tso);
void removeThreadFromDeQueue   (Capability *cap, StgTSO **head, StgTSO **tail, StgTSO *tso);

StgBool isThreadBound (StgTSO* tso);

#ifdef DEBUG
void printThreadBlockage (StgTSO *tso);
void printThreadStatus (StgTSO *t);
void printAllThreads (void);
void printThreadQueue (StgTSO *t);
# if defined(PARALLEL_HASKELL)
void print_bq (StgClosure *node);
void print_bqe (StgBlockingQueueElement *bqe);
nat  run_queue_len (void);
# elif defined(GRAN)
void print_bq (StgClosure *node);
# endif
#endif

#endif /* THREADS_H */
