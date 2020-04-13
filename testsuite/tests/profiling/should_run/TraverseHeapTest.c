#include <string.h>
#include "rts/SyntheticHeap.h"
#include "rts/TraverseHeap.h"
#include "TestHeap.h"

static synthHeap sh;

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

    printf("visit  %lu%s\n", synthClosureId(&sh, c), first_visit ? " f" : "");

    return first_visit;
}

static traverseState state;

int main(int argc, char *argv[])
{
    hs_init(0, NULL);

    traverseState *ts = &state;

    StgClosure **tests[] = {
            &c10, &c11, &c20
    };

    {
        printf("with return\n");

        sh = initializeTestHeap();
        initializeTraverseStack(ts);

        for(size_t i=0; i < (sizeof(tests)/sizeof(*tests)); i++) {
            stackElement se;
            memset(&se, 0, sizeof(se));

            printf("\n\npush   %lu\n", synthClosureId(&sh, *tests[i]));
            traversePushClosure(ts, *tests[i], *tests[i], &se, nullStackData);
            traverseWorkStack(ts, &testVisit, &testReturn);
        }

        closeTraverseStack(ts);
        freeSynthHeap(sh);
    }

    {
        printf("\n\n\n\njust visit\n");

        sh = initializeTestHeap();
        initializeTraverseStack(ts);

        for(size_t i=0; i < (sizeof(tests)/sizeof(*tests)); i++) {
            printf("\n\npush   %lu\n", synthClosureId(&sh, *tests[i]));
            traversePushClosure(ts, *tests[i], *tests[i], NULL, nullStackData);
            traverseWorkStack(ts, &testVisit, NULL);
        }

        closeTraverseStack(ts);
        freeSynthHeap(sh);
    }

    hs_exit();

    return 0;
}
