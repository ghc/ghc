#include "rts/PosixSource.h"
#include "Rts.h"

#include "TimeoutQueue.h"
#include "rts/Time.h"
#include "GetTime.h"

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h> // for the PRI* macros for printf for types like int64_t

/* Tests and benchmarks for the TimeoutQueue.
 *
 * Run with cli arg "--show-timing" to enable timing. Otherwise it doesn't show
 * times, so the output is deterministic and can be used as a regression test.
 *
 * Compile with -DDEBUG and link with the -debug RTS to enable assertions.
 * Compile with -DDEBUG to report algorithmic counters, ie algorithm steps.
 * This is useful for understanding the performance characteristics, and for
 * regression testing.
 */

#define EMPTY ((StgTimeoutQueue *) &stg_TIMEOUT_QUEUE_EMPTY_closure)

/* some platforms (e.g. mingw) have a min macro that clashes here */
#ifdef min
#undef min
#endif
static inline int min (int x, int y)
{
    return (x < y ? x : y);
}

static inline int rank (StgTimeoutQueue *h)
{
    return (h == EMPTY ? 0 : h->rank);
}

/* We use a local prng impl rather than relying on the system C library's
 * random() or rand() functions, because different C libs have different
 * implementations, which means we would not get the same test output on all
 * platforms, thus breaking the test. The implementation here is the example
 * implementation from the C11 spec. It produces results in the range 0..2^15-1
 * which is adequate for our use case.
 */
static unsigned long int next = 1;
static int prng(void) // RAND_MAX assumed to be 32767
{
    next = next * 1103515245 + 12345;
    return (unsigned int)(next/65536) % 32768;
}

static void assertInvariant (StgTimeoutQueue *h)
{
    if (h == EMPTY) return;

    /* heap order property */
    if (h->a != EMPTY) { ASSERT(h->waketime <= h->a->waketime); };
    if (h->b != EMPTY) { ASSERT(h->waketime <= h->b->waketime); };

    /* parent pointer symmetry */
    if (h->a != EMPTY) { ASSERT(h->a->parent == h); };
    if (h->b != EMPTY) { ASSERT(h->b->parent == h); };

    /* leftist-heap rank property */
    ASSERT(h->rank == min(rank(h->a), rank(h->b)) + 1);

    /* the same, recursively */
    assertInvariant(h->a);
    assertInvariant(h->b); /* the b side is shorter */
}

/* Algorithmic performance counters. Used to count the number of loop
 * iterations we do, to assess algorithmic optimisations. */
#if defined(DEBUG)
extern int insert_down_count;
extern int merge_down_count;
extern int leftify_up_to_count;
extern int leftify_up_shortcut_count;
#endif

static void reset_counters(void)
{
#if defined(DEBUG)
    insert_down_count   = 0;
    merge_down_count    = 0;
    leftify_up_to_count = 0;
    leftify_up_shortcut_count = 0;
#endif
}

/* Handy for debugging
void printh(StgTimeoutQueue *elems, int N)
{
    for (int i = 0; i < N; i++) {
        printf("&elem[%i] = %p, elem[%i] = { .waketime=%" PRIi64 ", .rank=%i, a=%10p, b=%10p, p=%10p }\n",
               i, &elems[i], i, elems[i].waketime, elems[i].rank,
               elems[i].a, elems[i].b, elems[i].parent);
    }
}
*/

#if defined(DEBUG)
static int size (StgTimeoutQueue *h)
{
    return (h == EMPTY ? 0 : (1 + size(h->a) + size(h->b)));
}
#endif

/* Used to compute the average rank, which tells us the average distance from a
   node to a leaf. It shows us just what a high fraction of nodes are leaf node
   or are very close to leaves. This is what makes deleting random elements so
   cheap.
 */
static int sumrank (StgTimeoutQueue *h)
{
    if (h != EMPTY) {
      return (sumrank(h->a) + sumrank(h->b) + h->rank);
    } else {
      return 0;
    }
}

int main_test (void)
{
    const int N = 100;
    const int M = 20;
    StgTimeoutQueue *root = EMPTY;

    /* Init */
    StgTimeoutQueue *elems = calloc(N, sizeof(StgTimeoutQueue));
    Time            *keys  = calloc(N, sizeof(Time));
    int             *rperm = calloc(N, sizeof(int));
    for (int i = 0; i < N; i++) {
        keys[i] = prng() % M;
        /* we'll never actually notify, so we can fill it in as empty */
        union NotifyCompletion notify = { .mvar = (StgMVar *) EMPTY };
        initElemTimeoutQueue(&elems[i], notify, NotifyMVar, NULL /*CCS*/);
    }
    /* random permutation of [0..N-1] */
    for (int i = 0; i < N; i++) {
        rperm[i] = i;
    }
    for (int i = N-1; i > 0; i--) {
        int j = prng() % (i+1);
        int temp = rperm[i];
        rperm[i] = rperm[j];
        rperm[j] = temp;
    }

    /* Insert */
    printf("===== Test insert =====\n");
    for (int i = 0; i < N; i++) {
        reset_counters();
        insertTimeoutQueue(&root, &elems[i], keys[i]);

        ASSERT(root->parent == EMPTY);
        assertInvariant(root);
#if defined(DEBUG)
        int sz = size(root);
        printf("i = %3i, x = %3" PRIi64 ", size(root) = %3i, loop1: %i, loop2: %i\n",
               i, keys[i], sz, insert_down_count, leftify_up_shortcut_count);
        ASSERT(sz == i+1);
#endif
    }

    /* Delete in min-heap order */
    printf("===== Test delete in min-heap order =====\n");
    for (int i = 0; i < N; i++) {

#if defined(DEBUG)
        Time x = findMinWaketimeTimeoutQueue(root);
#endif
        reset_counters();
        StgTimeout *unused_min;
        deleteMinTimeoutQueue(&root, &unused_min);
        /* We're not using the GC, so no need to update the remembered set. */

        ASSERT(root == EMPTY || root->parent == EMPTY);
        assertInvariant(root);
#if defined(DEBUG)
        int sz = size(root);
        printf("i = %3i, x = %3" PRIi64 ", size(root) = %3i, loop1: %i, loop2: %i\n",
               i, x, sz, merge_down_count, leftify_up_to_count);
        ASSERT(sz == N - (i+1));
#endif
    }

    /* Insert then delete in random order */
    printf("===== Test insert then delete in random order =====\n");
    for (int i = 0; i < N; i++) {
        insertTimeoutQueue(&root, &elems[i], keys[i]);
    }
    ASSERT(root->parent == EMPTY);
    assertInvariant(root);
    for (int i = 0; i < N; i++) {
        reset_counters();
        deleteTimeoutQueue(&root, &elems[rperm[i]]);

        ASSERT(root == EMPTY || root->parent == EMPTY);
        assertInvariant(root);
#if defined(DEBUG)
        int sz = size(root);
        printf("i = %3i, x = %3" PRIi64 ", size(root) = %3i, loop1: %i, loop2: %i, loop3: %i\n",
               i, keys[i], sz, merge_down_count,
               leftify_up_to_count, leftify_up_shortcut_count);
        ASSERT(sz == N - (i+1));
#endif
    }

    return 0;
}

int main_bench (bool showtiming)
{
    const int N = 100000;
    const int M = N; /* range of keys */
    /* WARNING: if you use very big N (heap size) and very small M (key range)
       then the depth of the tree gets very big. The assertInvariant function
       is recursive and can end up running out of stack space.

       See ulimit -S -s for the default max stack size in kb. It is possible to
       change but probably not worth it. Just avoid those combinations.
     */

    StgTimeoutQueue *root = EMPTY;
    Time before, after;
    initializeTimer();

    /* Init */
    StgTimeoutQueue *elems = calloc(N, sizeof(StgTimeoutQueue));
    Time            *keys  = calloc(N, sizeof(Time));
    int             *rperm = calloc(N, sizeof(int));
    for (int i = 0; i < N; i++) {
        keys[i] = prng() % M;
        /* we'll never actually notify, so we can fill it in as empty */
        union NotifyCompletion notify = { .mvar = (StgMVar *) EMPTY };
        initElemTimeoutQueue(&elems[i], notify, NotifyMVar, NULL /*CCS*/);
    }
    /* random permutation of [0..N-1] */
    for (int i = 0; i < N; i++) {
        rperm[i] = i;
    }
    for (int i = N-1; i > 0; i--) {
        int j = prng() % (i+1);
        int temp = rperm[i];
        rperm[i] = rperm[j];
        rperm[j] = temp;
    }

    /* Insert N elements */
    printf("===== Benchmark insert %i elements (random in [0..%i]) =====\n", N, M-1);
    reset_counters();
    before = getProcessElapsedTime();
    for (int i = 0; i < N; i++) {
        insertTimeoutQueue(&root, &elems[i], keys[i]);
    }
    after = getProcessElapsedTime();
    assertInvariant(root);

    if (showtiming) {
      Time ns = after - before;
      printf("completed in %" PRIi64 " nsec, %.1f ns per op\n", ns, (double)ns/N);
    }
#if defined(DEBUG)
    printf("insert down count:  %8i, per-op: %.2f\n",
            insert_down_count, (double)insert_down_count/N);
    printf("leftify upto count: %8i, per-op: %.2f\n",
            leftify_up_to_count, (double)leftify_up_to_count/N);
    printf("leftify up count:   %8i, per-op: %.2f\n",
            leftify_up_shortcut_count, (double)leftify_up_shortcut_count/N);
#endif
    printf("root node rank:     %i\n", root->rank);
    double avgrank = (double) sumrank(root) / (double) N;
    printf("average node rank:  %.2f\n", avgrank);

    /* Delete N elements in min-heap order */
    printf("===== Benchmark delete %i elements in min-heap order =====\n", N);
    reset_counters();
    before = getProcessElapsedTime();
    StgTimeout *unused_min;
    for (int i = 0; i < N; i++) {
        deleteMinTimeoutQueue(&root, &unused_min);
        /* We're not using the GC, so no need to update the remembered set. */
    }
    after = getProcessElapsedTime();
    assertInvariant(root);

    if (showtiming) {
      Time ns = after - before;
      printf("completed in %" PRIi64 " nsec, %.1f ns per op\n", ns, (double)ns/N);
    }
#if defined(DEBUG)
    printf("merge down count:   %8i, per-op: %.2f\n",
            merge_down_count, (double)merge_down_count/N);
    printf("leftify upto count: %8i, per-op: %.2f\n",
            leftify_up_to_count, (double)leftify_up_to_count/N);
#endif

    /* Delete N elements in random order */
    printf("===== Benchmark delete %i elements in random order =====\n", N);
    for (int i = 0; i < N; i++) {
        insertTimeoutQueue(&root, &elems[i], keys[i]);
    }
    reset_counters();
    before = getProcessElapsedTime();
    for (int i = 0; i < N; i++) {
        deleteTimeoutQueue(&root, &elems[rperm[i]]);
    }
    after = getProcessElapsedTime();
    assertInvariant(root);

    if (showtiming) {
      Time ns = after - before;
      printf("completed in %" PRIi64 " nsec, %.1f ns per op\n", ns, (double)ns/N);
    }
#if defined(DEBUG)
    printf("merge down count:   %8i, per-op: %.2f\n",
            merge_down_count, (double)merge_down_count/N);
    printf("leftify upto count: %8i, per-op: %.2f\n",
            leftify_up_to_count, (double)leftify_up_to_count/N);
    printf("leftify up count:   %8i, per-op: %.2f\n",
            leftify_up_shortcut_count, (double)leftify_up_shortcut_count/N);
#endif

    return 0;
}

int main (int argc, char *argv[])
{
    bool showtiming = argc > 1 ? strcmp(argv[1], "--show-timing") == 0 : false;

    main_test();
    main_bench(showtiming);
    return 0;
}

