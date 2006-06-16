/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#ifndef THREADS_H
#define THREADS_H

#if defined(GRAN) || defined(PARALLEL_HASKELL)
StgBlockingQueueElement * unblockOne (StgBlockingQueueElement *bqe, 
				      StgClosure *node);
#else
StgTSO * unblockOne (Capability *cap, StgTSO *tso);
StgTSO * unblockOne_ (Capability *cap, StgTSO *tso, rtsBool allow_migrate);
#endif

#if defined(GRAN) || defined(PARALLEL_HASKELL)
void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#else
void awakenBlockedQueue (Capability *cap, StgTSO *tso);
#endif

void removeThreadFromMVarQueue (StgMVar *mvar, StgTSO *tso);
void removeThreadFromQueue     (StgTSO **queue, StgTSO *tso);
void removeThreadFromDeQueue   (StgTSO **head, StgTSO **tail, StgTSO *tso);

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
