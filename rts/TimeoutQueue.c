#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h" // needed by SET_HDR macro

#include "TimeoutQueue.h"

/* The implementation is a leftist heap. The reference implementation is the
   simple purely functional version, e.g. as described by Okasaki '98:

data Heap a = E | T Int a (Heap a) (Heap a)

rank :: Heap a -> Int
rank  E          = 0
rank (T r _ _ _) = r

insert :: Ord a => a -> Heap a -> Heap a
insert x    E           = T 1 x E E
insert x h@(T _ y a b)
  | x <= y              = T 1 x h E
  | otherwise           = makeT y a (insert x b)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin  E          = error "deleteMin: empty"
deleteMin (T _ _ a b) = merge a b

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1)
      h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a
*/

/******************************************************************************
 * Sanity checks
 */

/* Sanity check that the size of the C struct StgTimeoutQueue matches the
 * corresponding info table declaration in StgMiscClosures.cmm:
 * INFO_TABLE_CONSTR(stg_TIMEOUT_QUEUE, ...)
 */
GHC_STATIC_ASSERT(sizeof(StgTimeoutQueue)
               == sizeof(StgHeader)
                + sizeof(StgPtr)  * stg_TIMEOUT_QUEUE_NUM_PTRS
                + sizeof(StgWord) * stg_TIMEOUT_QUEUE_NUM_NONPTRS,
                "sizeof(StgTimeoutQueue) does not match expected size");

/******************************************************************************
 * Forward declarations of the worker functions. See below.
 */

static void insert_down (StgTimeoutQueue **root, StgTimeoutQueue *hnew);

static StgTimeoutQueue *merge_down (StgTimeoutQueue *h1,
                                    StgTimeoutQueue *h2,
                                    StgTimeoutQueue *hparent,
                                    StgTimeoutQueue **hparent_child);

static void leftify_up_to       (StgTimeoutQueue *h, StgTimeoutQueue *target);
static void leftify_up_shortcut (StgTimeoutQueue *h);

#if defined(__clang__)
/* Don't error on unfixable clang warnings about 'unneeded' functions used only
 * in assertsions. They may be 'unneeded' but they are still used and cannot be
 * removed (or defined conditionally). */
#pragma clang diagnostic warning "-Wunneeded-internal-declaration"
#endif

static bool sanity_check_root    (StgTimeoutQueue *root);
static bool sanity_check_element (StgTimeout *h);
static bool sanity_check_member  (StgTimeoutQueue *root, StgTimeout *h);

#define EMPTY ((StgTimeoutQueue *) &stg_TIMEOUT_QUEUE_EMPTY_closure)

typedef uint32_t Rank; // type to match StgTimeoutQueue.rank

/******************************************************************************
 * The main functions, declared in the TimeoutQueue.h
 */


void initElemTimeoutQueue(StgTimeout *t,
                          union NotifyCompletion notify,
                          enum  NotifyCompletionType notify_type,
                          CostCentreStack *ccs USED_IF_PROFILING)
{
    SET_HDR(t, &stg_TIMEOUT_QUEUE_info, ccs);
    t->parent      = EMPTY;
    t->a           = EMPTY;
    t->b           = EMPTY;
    t->notify      = notify;
    t->notify_type = notify_type;
}


void insertTimeoutQueue(StgTimeoutQueue **root, StgTimeout *hnew, Time waketime)
{
    ASSERT(sanity_check_root(*root));
    ASSERT(hnew->parent == EMPTY && hnew->a == EMPTY && hnew->b == EMPTY);

   /* This is a non-recursive/loop-based implementation, based on the
    * recursive version:
    *
    * insert x  E            = T 1 x E E
    * insert x h@(T _ y a b)
    *   | x <= y             = T 1 x h E
    *   | otherwise          = makeT y a (insert x b)
    *
    * makeT x a b
    *   | rank a >= rank b = T (rank b + 1) x a b
    *   | otherwise        = T (rank a + 1) x b a
    *
    * We will do this in three parts:
    * 1. Set up the common parts of the new node: (T 1 x E E)
    * 2. A loop for the "down" part of the recursion: insert_down
    * 3. A loop for the "up" part of the recursion leftify_up_shortcut
    *
    * We can do this without a stack because we link the nodes in both
    * directions, parent pointers as well as tree child pointers.
    */

   /* Set up the common parts of the new node: (T 1 x E E)
    * In one case below we overwrite a to get: (T 1 x h E)
    */
    hnew->rank     = 1;
    hnew->waketime = waketime;
    hnew->a        = EMPTY;
    hnew->b        = EMPTY;

   /* Walk down and insert at the correct point. This corresponds to
    *
    * insert x  E            = T 1 x E E
    * insert x h@(T _ y a b)
    *   | x <= y             = T 1 x h E
    *   | otherwise          = (...) (insert x b)
    */
    insert_down(root, hnew);

   /* At this point the entry 'hnew' is linked into the right place.
    * We now need to restore the leftist-heap rank property. We iterate
    * back up towards the tree root, adjusting ranks and swapping as needed.
    *
    * ... = makeT y a (...)
    *
    * makeT x a b
    *   | rank a >= rank b = T (rank b + 1) x a b
    *   | otherwise        = T (rank a + 1) x b a
    *
    * We start with the parent h of the new node (T 1 x E E) or (T 1 x h E).
    */
    leftify_up_shortcut(hnew->parent);

   /* Note it is ok that we use leftify_up_shortcut here as an optimisation.
    * We do not have to use leftify_up_to(hnew->parent, EMPTY) all the way to
    * the root. See leftify_up_shortcut below for a discussion of the
    * optimisation. In practice this doesn't make a big difference in time
    * since it just reduces the number of nodes we touch a second time. Since
    * such nodes are already in the cache then the saving is not huge.
    */

    /* Structure sanity check: either the new heap node is the root,
     * or the new heap node has a parent
     */
    ASSERT(sanity_check_root(*root));
    ASSERT(sanity_check_element(hnew));
    ASSERT(sanity_check_member(*root, hnew));
}

/* Deletes and returns the root node, containing the minimum key value.
 *
 * Like all normal heap algorithms, it costs O(log n).
 *
 * The returned timeout becomes the caller's responsiblity, in particular for
 * GC since the timeout will no longer be accessible from the queue root.
 */
void deleteMinTimeoutQueue(StgTimeoutQueue **root, StgTimeout **t)
{
    ASSERT(sanity_check_root(*root));

   /* This is a non-recursive/loop-based version of the recursive version:
    *
    * deleteMin (T _ _ a b) = merge a b
    *
    * merge h E = h
    * merge E h = h
    * merge h1@(T _ x a1 b1)
    *       h2@(T _ y a2 b2)
    *   | x <= y    = makeT x a1 (merge b1 h2)
    *   | otherwise = makeT y a2 (merge h1 b2)
    *
    * makeT x a b
    *   | rank a >= rank b = T (rank b + 1) x a b
    *   | otherwise        = T (rank a + 1) x b a
    *
    * We will do this in two parts:
    * 1. A loop for the "down" part of the recursion: merge_down
    * 2. A loop for the "up" part of the recursion: leftify_up_to
    */

    StgTimeoutQueue *oldroot = *root;
    StgTimeoutQueue *a = oldroot->a;
    StgTimeoutQueue *b = oldroot->b;
    StgTimeoutQueue *htmp;

    oldroot->a = EMPTY;
    oldroot->b = EMPTY;
    ASSERT(oldroot->parent == EMPTY);
    *t = oldroot; /* return the deleted old root */

    htmp = merge_down(a, b, EMPTY, root);
    ASSERT(*root == EMPTY || (*root)->parent == EMPTY);

    leftify_up_to(htmp, EMPTY);

    ASSERT(sanity_check_root(*root));
}

/* Delete the given node in the heap.
 *
 * This is typically (on average) O(1) and worse case O(log n), thus it is
 * typically much faster than deleteMin. This makes it good for things like
 * timeouts that often get deleted before they expire.
 *
 * The intuition for this is that the cost is proportional to the rank of the
 * node, and most nodes are leaf nodes or very close to leaf nodes.
 *
 * This does *not* use updateRemembSetPushClosure on the timeout that is
 * removed. The timeout is still accessible to the caller, and it remains the
 * caller's responsiblity. This supports the use case of long-lived timeouts
 * that are used many times, adjusting their expiry time and re-inserting.
 */
void deleteTimeoutQueue(StgTimeoutQueue **root, StgTimeout *h)
{
    ASSERT(sanity_check_root(*root));
    ASSERT(sanity_check_element(h));
    ASSERT(sanity_check_member(*root, h));

   /* This is much like deleteMin, but with some extra subtlety. We start with
    * the same merge operation as deleteMin, albeit with extra details to get
    * the parent links right. We could then naively fix up the leftist-heap
    * property all the way back to the root. That would work but would be a
    * O(log n) algorithm in the typical case, since we'd fixup all the way
    * from a leaf node to the root.
    *
    * Instead we do the normal fixup but only back to the location where we
    * deleted the node. From there up, we can use a different termination
    * condition that usually means we need to walk very few links up the tree.
    * See leftify_up for details on the termination condition.
    *
    * So we will do this in three parts:
    * 1. A loop for the "down" part of the recursion: merge_down
    * 2. A loop for the first "up" part of the recursion: leftify_up_to
    * 3. A loop for the second "up" part of the recursion: leftify_up_shortcut
    */

    StgTimeoutQueue *a       = h->a;
    StgTimeoutQueue *b       = h->b;
    StgTimeoutQueue *hparent = h->parent;
    StgTimeoutQueue **hparent_child;

    /* Unlink h in the outbound direction. Fixing the links in other direction
       will be done by merge_down. */
    h->a      = EMPTY;
    h->b      = EMPTY;
    h->parent = EMPTY;

    /* Establish which hparent_child to use for merge_down to use to fixup the
       link from h->parent down to the new merged node. If h has a parent node
       then the link to fixup will be either h->parent->a or h->parent->b, or
       otherwise it is the overall heap root. */
    if (hparent != EMPTY) {
        if (hparent->a == h) {
            hparent_child = &hparent->a;
        } else {
            ASSERT(hparent->b == h);
            hparent_child = &hparent->b;
        }
    } else {
        /* We're deleting the root */
        ASSERT(h == *root);
        hparent_child = root;
    }

    StgTimeoutQueue *htmp = merge_down(a, b, hparent, hparent_child);
    ASSERT(*root == EMPTY || (*root)->parent == EMPTY);
    leftify_up_to(htmp, hparent);
    leftify_up_shortcut(hparent);

#ifdef ASSERTS_ENABLED
    h->rank = 1; /* needed for sanity_check_element(h) */
#endif
    ASSERT(sanity_check_root(*root));
    ASSERT(sanity_check_element(h));
}


/******************************************************************************
 * The definitions of the worker functions.
 */

static inline Rank rank (StgTimeoutQueue *h)
{
    return (h == EMPTY ? 0 : h->rank);
}

/* some platforms (e.g. mingw) have a min macro that clashes here */
#ifdef min
#undef min
#endif
static inline Rank min (Rank x, Rank y)
{
    return (x < y ? x : y);
}

static inline void swap(StgTimeoutQueue **a, StgTimeoutQueue **b)
{
    StgTimeoutQueue *tmp;
    tmp = *a;
    *a  = *b;
    *b  = tmp;
}


/* Algorithmic performance counters. Used to count the number of loop
 * iterations we do, to assess algorithmic optimisations. */
#if defined(DEBUG)
int insert_down_count;
int merge_down_count;
int leftify_up_to_count;
int leftify_up_shortcut_count;
#endif


void insert_down (StgTimeoutQueue **root, StgTimeoutQueue *hnew)
{
    StgTimeoutQueue *hparent        = EMPTY;
    StgTimeoutQueue **hparent_child = root;
    StgTimeoutQueue *h              = *root;

    Time x = hnew->waketime;
    while (1) {
        /* insert x E = T 1 x E E */
        if (h == EMPTY) {
            break;
        }

        /* insert x h@(T _ y _ _) | x <= y = T 1 x h E
         */
        Time y = h->waketime;
        if (x <= y) {
            /* link h underneath the left tree of 'hnew' (T 1 x h E) */
            hnew->a   = h;
            h->parent = hnew;
            break;
        }
        /* insert x (T _ _ _ b)
             | otherwise = (...) (insert x b)
         * So recurse with x := x, h := b, and keep h as parent of b
         */
        hparent       = h;
        hparent_child = &h->b;
        h             = h->b;
#if defined(DEBUG)
        insert_down_count++;
#endif
    }
    /* link 'hnew' underneath the (right) tree of the parent */
    hnew->parent = hparent;
    *hparent_child = hnew;
}


/* Merge together the trees h1 and h2, and link the result to a parent.
 *
 * Precondition:
 *   assertInvariant (h1)
 *   assertInvariant (h2)
 *   hparent_child != EMPTY
 *
 * Postcondition:
 *   returns a node h, such that
 *     h == h1 || h == h2
 *     h->parent == hparent
 *     *hparent_child == h
 *
 * That is, the node h resulting from merging h1 & h2 will have hparent as its
 * parent, and also *hparent_child will point to h.
 *
 * This operation maintains the heap property, and child/parent links, but it
 * does _not_ maintain the leftist-heap rank property for the part of the tree
 * touched by merge.
 *
 * We use this to merge when we delete the root of a heap or an interior node.
 */
static StgTimeoutQueue *merge_down (StgTimeoutQueue *h1,
                                    StgTimeoutQueue *h2,
             /* link upward:   */   StgTimeoutQueue *hparent,
             /* link downward: */   StgTimeoutQueue **hparent_child)
{
    ASSERT(*hparent_child != EMPTY);

    /* This is a non-recursive/loop-based version of the recursive version:
     *
     * merge h E = h
     * merge E h = h
     * merge h1@(T _ x a1 b1)
     *       h2@(T _ y a2 b2)
     *   | x <= y    = makeT x a1 (merge b1 h2)
     *   | otherwise = makeT y a2 (merge h1 b2)
     *
     * makeT x a b
     *   | rank a >= rank b = T (rank b + 1) x a b
     *   | otherwise        = T (rank a + 1) x b a
     *
     * We will do this in two parts:
     * 1. A loop for the "down" part of the recursion
     * 2. A loop for the "up" part of the recursion
     *
     * We can do this without a stack because we link the nodes in both
     * directions, parent pointers as well as tree child pointers. We could
     * do it without parent pointers, but we want the parent pointers anyway
     * elsewhere so we make use of them here.
     *
     * We extend merge with an extra param: the parent node to use for the node
     * resulting from merging h1 and h2. This is EMPTY for the root.
     */

    StgTimeoutQueue *h;
    while (1) {
        /* merge h E = h */
        if (h2 == EMPTY) {
            /* The common special case of deleting a leaf node results in
               merging two empty trees. This can only happen on the first
               iteration.
             */
            if (h1 == EMPTY) {
                *hparent_child = EMPTY;
                return hparent;
            }

            /* h1 is the new h at this level */
            h = h1;
            /* Do the final link of h and hparent both ways */
            h->parent = hparent;
            *hparent_child = h;
            break;
        }

        /* merge E h = h */
        if (h1 == EMPTY) {
            /* h2 is the new h at this level */
            h = h2;
            /* Do the final link of h and hparent both ways */
            h->parent = hparent;
            *hparent_child = h;
            break;
        }

        /* merge h1@(T _ x a1 b1)
                 h2@(T _ y a2 b2) = ... */
        Time x = h1->waketime;
        Time y = h2->waketime;
        StgTimeoutQueue *b1 = h1->b;
        StgTimeoutQueue *b2 = h2->b;

        /* | x <= y    = makeT x a1 (merge b1 h2)
           | otherwise = makeT y a2 (merge h1 b2) */
        if (x <= y) {
            /* We're going to keep h1 at this level and merge b1 h2 as the
               b sub-tree of h1.
             */
            h  = h1;
            h1 = b1;
         /* h2 = h2; */
        }
        else {
            /* We're going to keep h2 at this level and merge h1 b2 as the
               b sub-tree of h2.
             */
            h  = h2;
         /* h1 = h1; */
            h2 = b2;
        }
        /* Now we know which of of h1 & h2 is the new h at this level,
           we can link h and hparent both ways round.
         */
        h->parent = hparent;
        *hparent_child = h;

        /* h is the parent for the next iteration */
        hparent = h;

        /* We will link the result of the next iteration under h->b */
        hparent_child = &h->b;

        /* On the next iteration we should not be merging empty trees.
           That should only happen on the first iteration if we started by
           deleting a leaf node.
         */
        ASSERT(h1 != EMPTY || h2 != EMPTY);

#if defined(DEBUG)
        merge_down_count++;
#endif
    }
    return h;
}


/* Leftify: re-establish the leftist-heap rank property.
 *
 * Re-establish the leftist-heap rank property, starting from a node h, and
 * moving upwards to a target node (or EMPTY for the root). It fixes up the
 * rank values and swaps child nodes as needed.
 *
 * This is used after merge_down to complete the merge operation. This version
 * of leftify goes upto a target node. In the merge operation the target is the
 * location (or rather parent of) where the merge started. We have to go all
 * the way to this point and cannot short-cut it, because merge can move things
 * about quite a bit.
 *
 * This corresponds in the functional version to the "upward" part of the
 * recursion where makeT is applied to the result of the "downward" recursion.
 *
 *   | x <= y    = makeT x a1 (merge b1 h2)
 *   | otherwise = makeT y a2 (merge h1 b2)
 *
 * makeT x a b
 *   | rank a >= rank b = T (rank b + 1) x a b
 *   | otherwise        = T (rank a + 1) x b a
 *
 * Note that we already sorted out the things that depended on the x <= y
 * conditional on the loop down. So we don't need to remember which way
 * the condition went on the loop back up.
 *
 * The merge_down has already sorted out the child and parent links. So we do
 * not need to return anything.
 *
 */
static void leftify_up_to (StgTimeoutQueue *h, StgTimeoutQueue *target)
{
    while (h != target) {
        Rank ra = rank(h->a);
        Rank rb = rank(h->b);
        if (ra >= rb) {
            h->rank = rb + 1;
        } else {
            h->rank = ra + 1;
            swap(&h->a, &h->b);
        }
        ASSERT(h->rank == min(rank(h->a), rank(h->b)) + 1);
        h = h->parent;
#if defined(DEBUG)
        leftify_up_to_count++;
#endif
    }
}


/* A variant on leftify_up_to, with a short-cut termination condition.
 *
 * Like leftify_up_to, it re-establishes the leftist-heap rank property,
 * starting from a node h, and moving upwards. It terminates the walk upwards
 * when it encounters a node where the rank info is already correct, or when it
 * reaches the root.
 */
static void leftify_up_shortcut (StgTimeoutQueue *h)
{
    while (h != EMPTY) {
        Rank ra = rank(h->a);
        Rank rb = rank(h->b);
        if (ra >= rb) {
            if (h->rank == rb + 1) break; /* short-cut condition */
            h->rank = rb + 1;
        } else {
            if (h->rank == ra + 1) break; /* short-cut condition */
            h->rank = ra + 1;
            swap(&h->a, &h->b);
        }
        ASSERT(h->rank == min(rank(h->a), rank(h->b)) + 1);
        h = h->parent;
#if defined(DEBUG)
        leftify_up_shortcut_count++;
#endif
    }
}


static bool sanity_check_root (StgTimeoutQueue *root)
{
    return root == EMPTY
        || (sanity_check_element(root) && root->parent == EMPTY);
}

static bool sanity_check_element (StgTimeout *h)
{
    if (h == EMPTY) return false;

    if (h->header.info != &stg_TIMEOUT_QUEUE_info) return false;

    if (h->parent != EMPTY) {
      if (h->parent->a != h && h->parent->b != h) return false;
    }

    if (h->rank != (min(rank(h->a), rank(h->b)) + 1)) return false;

    if (h->a != EMPTY && h->a->parent != h) return false;
    if (h->b != EMPTY && h->b->parent != h) return false;

    return true;

/* Or for serious debugging, full recursive checks:
    return (h->a == EMPTY || sanity_check_element(h->a))
        && (h->b == EMPTY || sanity_check_element(h->b));
*/
}

static bool sanity_check_member (StgTimeoutQueue *root, StgTimeout *h)
{
    StgTimeout *i = h;
    while (i->parent != EMPTY) i = i->parent;
    return (i == root);
}
