
#if defined(PROFILING) && defined(DEBUG)

#include "rts/PosixSource.h"
#include <string.h>
#include <Rts.h>
#include <rts/storage/Closures.h>
#include "TraverseHeap.h"

#define container_of(ptr, type, member) ({                        \
            const typeof( ((type *)0)->member ) *__mptr = (ptr);  \
            (type *)( (char *)__mptr - offsetof(type,member) );})

static StgInfoTable info_weak     = { .type = WEAK };
static StgInfoTable info_selector = { .type = THUNK_SELECTOR };
static StgInfoTable info_arrwords = { .type = ARR_WORDS };

struct node {
    unsigned int id;
    union node_union {
        StgClosure cls;
        StgWeak weak;
        StgSelector selector;
        StgArrBytes arrbytes;
    } u;
};

// See INFO_PTR_TO_STRUCT in ClosureMacros.h
#if defined(TABLES_NEXT_TO_CODE)
#define INFO(ptr) ((StgInfoTable *)ptr + 1)
#else
#define INFO(ptr) ((StgInfoTable *)ptr)
#endif

#define node3(_id, a,b,c)                               \
    static struct node n##_id = {                       \
        .id = _id,                                      \
        .u.weak = {                                     \
            .header = { .info = INFO(&info_weak) },     \
            .key       = (StgClosure*)&(n##a.u),        \
            .value     = (StgClosure*)&(n##b.u),        \
            .finalizer = (StgClosure*)&(n##c.u),        \
        }                                               \
    };

#define node1(_id, a)                                   \
    static struct node n##_id = {                       \
        .id = _id,                                      \
        .u.selector = {                                 \
            .header = { .info = INFO(&info_selector) }, \
            .selectee = (StgClosure*)&(n##a.u),         \
        }                                               \
    }

#define node0(_id)                                      \
    static struct node n##_id = {                       \
        .id = _id,                                      \
        .u.arrbytes = {                                 \
            .header = { .info = INFO(&info_arrwords) }, \
        }                                               \
    }


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


static void
testReturn(StgClosure *c, const stackAccum acc,
           StgClosure *c_parent, stackAccum *acc_parent)
{
    (void) acc;
    (void) c_parent;
    (void) acc_parent;

    struct node *n = container_of(c, struct node, u.cls);

    printf("return %u\n", n->id);

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

    struct node *n = container_of(c, struct node, u.cls);

    printf("visit  %u\n", n->id);

    return first_visit;
}

static struct node* const g_tests[] = {
    &n1000, &n1100,
    &n2000,
};

static traverseState state;

void traverseHeapRunTests(void);
void traverseHeapRunTests(void)
{
    traverseState *ts = &state;

    {
        printf("with return\n");

        state.return_cb = &testReturn;

        initializeTraverseStack(ts);
        traverseInvalidateClosureData(ts);

        for(size_t i=0; i < (sizeof(g_tests)/sizeof(*g_tests)); i++) {
            struct node *n = g_tests[i];

            stackElement se;
            memset(&se, 0, sizeof(se));

            printf("\n\npush   %u\n", n->id);
            traversePushClosure(ts, &n->u.cls, &n->u.cls, &se, nullStackData);
            traverseWorkStack(ts, &testVisit);
        }

        closeTraverseStack(ts);
    }

    {
        printf("\n\n\n\njust visit\n");

        state.return_cb = NULL;

        initializeTraverseStack(ts);
        traverseInvalidateClosureData(ts);

        for(size_t i=0; i < (sizeof(g_tests)/sizeof(*g_tests)); i++) {
            struct node *n = g_tests[i];

            printf("\n\npush   %u\n", n->id);
            traversePushClosure(ts, &n->u.cls, &n->u.cls, NULL, nullStackData);
            traverseWorkStack(ts, &testVisit);
        }

        closeTraverseStack(ts);

    }
}

#endif
