#include <string.h>
#include "rts/SyntheticHeap.h"
#include "rts/TraverseHeap.h"

static StgClosure *c10, *c11, *c20, *c30;

synthHeap sh;

static void initializeTestHeap(void)
{
    sh = allocSynthHeap();
    StgPtr p = sh.heap->free;
    StgPtr d = sh.descr->start;

/*
  1.0) Just a simple case to start with.

   1
  /
  0---2
  \
   3
*/
    node0(1003);
    node0(1002);
    node0(1001);
    node3(1000,
          1001,
          1002,
          1003);

    c10 = n1000;


/*
  1.1) Now with a cycle

   1
  /` \,
  0--->2
  \,
   3
*/
    node0(1103);
    node0(1102);
    node1(1101,
          1102);
    node3(1100,
          1101,
          1102,
          1103);

    c11 = n1100;


/*
  2.0) This tests the chain optimization.

   1     6
  /     /
  0-2-4-5-7
  \     \
   3     8
*/

    node0(2006);
    node0(2007);
    node0(2008);

    node3(2005,
          2006,
          2007,
          2008);

    node1(2004,
          2005);

    node0(2003);
    node1(2002,
          2004);
    node0(2001);

    node3(2000,
          2001,
          2002,
          2003);

    c20 = n2000;


/*
  3.0) Some new closures which show up with zeroed prof header but when
  flip=1. Note: We check that we get the first_visit value right by observing
  that n3001 gets visited.
 */
    node0(3001);
    node1(3000, 3001);

    c30 = n3000;

    sh.heap->free = p;
}


static void
testReturn(StgClosure *c, const stackAccum acc,
           StgClosure *c_parent, stackAccum *acc_parent)
{
    (void) acc;
    (void) c_parent;
    (void) acc_parent;

    printf("return %lu\n", synthClosureId(&sh, c));

    return;
}

static bool
testVisit(StgClosure *c, const StgClosure *cp,
          const stackData data, const bool first_visit,
          stackAccum *acc, stackData *child_data)
{
    (void) cp;
    (void) data;
    (void) acc;
    (void) child_data;

    printf("visit  %lu\n", synthClosureId(&sh, c));

    return first_visit;
}

static traverseState state;

int main(int argc, char *argv[])
{
    hs_init(0, NULL);

    traverseState *ts = &state;

    initializeTestHeap();

    StgClosure *tests[] = {
        c10, c11, c20
    };

    {
        printf("with return\n");

        initializeTraverseStack(ts);
        traverseInvalidateAllClosureData(ts);

        for(size_t i=0; i < (sizeof(tests)/sizeof(*tests)); i++) {
            stackElement se;
            memset(&se, 0, sizeof(se));

            printf("\n\npush   %lu\n", synthClosureId(&sh, tests[i]));
            traversePushClosure(ts, tests[i], tests[i], &se, nullStackData);
            traverseWorkStack(ts, &testVisit, &testReturn);
        }

        closeTraverseStack(ts);
    }

    {
        printf("\n\n\n\njust visit\n");

        initializeTraverseStack(ts);
        traverseInvalidateAllClosureData(ts);

        for(size_t i=0; i < (sizeof(tests)/sizeof(*tests)); i++) {
            printf("\n\npush   %lu\n", synthClosureId(&sh, tests[i]));
            traversePushClosure(ts, tests[i], tests[i], NULL, nullStackData);
            traverseWorkStack(ts, &testVisit, NULL);
        }

        closeTraverseStack(ts);

    }

    {
        printf("\n\n\n\nnew closures\n");

        initializeTraverseStack(ts);
        traverseInvalidateAllClosureData(ts);

        if(ts->flip != 1)
            abort();

        printf("\n\npush   %lu\n", synthClosureId(&sh, c30));
        traversePushClosure(ts, c30, c30, NULL, nullStackData);
        traverseWorkStack(ts, &testVisit, NULL);

        closeTraverseStack(ts);
    }

    freeSynthHeap(sh);

    hs_exit();

    return 0;
}
