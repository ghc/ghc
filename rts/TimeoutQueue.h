/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Prototypes for functions in TimeoutQueue.c
 *
 * A data structure used to efficiently manage a collection of timeouts.
 *
 * This is used to support the timeout functionality in some of the I/O manager
 * implementations. Currently this is only the select()-based impl, but it
 * could be used for any/all I/O managers that are implemented within the RTS
 * (as opposed to those implemented primarily on the Haskell side).
 *
 * It is a priority queue based on absolute expiry time. It uses 64bit
 * high-precision Time for the keys (see Time.h). The values are normal
 * closures which allows for example using MVars for unblocking.
 *
 * It is common in many applications for timeouts to be created and then
 * deleted or altered before they expire. Thus the choice of data structure for
 * timeouts should support this efficiently. The implementation choice here is
 * a leftist heap with the extra feature that it supports deleting arbitrary
 * elements, provided the caller retain a pointer to the element. While the
 * deleteMin operation takes O(log n) time, as in all heap structures, the
 * delete operation for arbitrary elements /typically/ takes O(1), and only
 * O(log n) in the worst case. In practice, when managing thousands of timeouts
 * it can be a factor of 10 faster to delete a random timeout queue element
 * than to remove the minimum element. This supports the common use case.
 *
 * https://en.wikipedia.org/wiki/Leftist_tree#Deletion_of_an_arbitrary_element_from_a_Min_HBLT
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* We often want to refer to individual elements of the timeout queue.
 * They are actually the same type as the overall queue (because it's just a
 * tree of nodes, and the overall queue is just a reference to the root node).
 *
 * So by convention we use StgTimeout for individual elements, and use
 * StgTimeoutQueue when we refer to the overall queue.
 */
typedef StgTimeoutQueue StgTimeout;

/* An empty queue.
 *
 * This is actually just a pointer to an empty nullary leaf constructor.
 * Use this to initialise the tree root:
 *
 * StgTimeoutQueue *root = emptyTimeoutQueue();
 *
 * The other operations need to be passed a pointer to this root, which they
 * may modify, if the operation needs to update the queue root.
 *
 * This is a GC-heap managed data structure. Callers /must/ arrange to make
 * the queue root a GC root in the mark phase. No special markTimeoutQueue is
 * provided since it is just a matter of marking the root.
 */
INLINE_HEADER
StgTimeoutQueue *emptyTimeoutQueue(void);


/* Initialise a newly allocated queue element, prior to inserting it into a
 * queue. The caller is responsible for allocating the space on the GC heap.
 *
 * The timeout notification is provided when the element is created, but the
 * wake time is specified each time the queue element is inserted into the
 * queue. This allows the timeout to be deleted from the queue and re-inserted
 * with a different wake time, either immediate or some time later. This
 * supports the common pattern of pushing back a timeout to a later time. It
 * also supports temporarily suspending a timeout and optionally re-instating
 * it later.
 */
void initElemTimeoutQueue(StgTimeout *t,
                          union NotifyCompletion notify,
                          enum  NotifyCompletionType notify_type,
                          CostCentreStack *ccs);


/* Is the queue empty?
 */
INLINE_HEADER
bool isEmptyTimeoutQueue(StgTimeoutQueue *root);


/* Find the minimum wakeup time in the queue.
 * The queue must not be empty.
 */
INLINE_HEADER
Time findMinWaketimeTimeoutQueue(StgTimeoutQueue *root);


/* Find the value with the minimum wakeup time in the queue.
 * The queue must not be empty.
 */
INLINE_HEADER
StgTimeout *findMinTimeoutQueue(StgTimeoutQueue *root);


/* Insert a new entry in the queue. The new entry must already be allocated,
 * and the timeout and waketime fields must be filled in. All the other fields
 * will be filled by insertTimeoutQueue.
 *
 * This is O(log n) worst case, but typically O(1).
 */
void insertTimeoutQueue(StgTimeoutQueue **root, StgTimeout *t, Time waketime);


/* Delete and return the minimum entry in the queue.
 *
 * This is O(log n).
 *
 * The returned timeout becomes the caller's responsiblity, in particular for
 * GC since the timeout will no longer be accessible from the queue root.
 */
void deleteMinTimeoutQueue(StgTimeoutQueue **root, StgTimeout **t);


/* Delete the given entry in the queue.
 *
 * This is O(log n) worst case, but typically O(1).
 *
 * The timeout remains the caller's responsiblity (in particular for GC).
 */
void deleteTimeoutQueue(StgTimeoutQueue **root, StgTimeout *t);


/* -----------------------------------------------------------------------------
 * Private from here on down.
 * -----------------------------------------------------------------------------
 */

INLINE_HEADER
StgTimeoutQueue *emptyTimeoutQueue(void)
{
    return ((StgTimeoutQueue *) &stg_TIMEOUT_QUEUE_EMPTY_closure);
}

INLINE_HEADER
bool isEmptyTimeoutQueue(StgTimeoutQueue *root)
{
    return (root == ((StgTimeoutQueue *) &stg_TIMEOUT_QUEUE_EMPTY_closure));
}

INLINE_HEADER
Time findMinWaketimeTimeoutQueue(StgTimeoutQueue *root)
{
    return (root->waketime);
}

INLINE_HEADER
StgTimeout *findMinTimeoutQueue(StgTimeoutQueue *root)
{
    return root;
}

#include "EndPrivate.h"

