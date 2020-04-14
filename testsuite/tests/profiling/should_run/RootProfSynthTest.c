#include <string.h>
#include <Rts.h>
#include <rts/storage/Closures.h>
#include "rts/SyntheticHeap.h"
#include "rts/TraverseHeap.h"
#include "rts/RootProfile.h"
#include "rts/ProfHeap.h"
#include "rts/StablePtr.h"
#include "TestHeap.h"

synthHeap sh;

extern FILE* hp_debug_file;
extern int g_rootProfileDebugLevel; // for debugging

int main(int c, char *v[])
{
    char *argv[] = { v[0], "+RTS", "-ho" }, **argvp = argv;
    int argc = sizeof(argv)/sizeof(*argv);
    hs_init(&argc, &argvp);

    hp_debug_file = stdout;

    sh = initializeTestHeap();

    HsStablePtr p10 = getStablePtr((StgPtr)c10);
    HsStablePtr p11 = getStablePtr((StgPtr)c11);
    HsStablePtr p1102 = getStablePtr((StgPtr)c1102);

    HsStablePtr ptrs[] = { p10, p11, p1102 };
    char *descs[] = { malloc(16), malloc(16), malloc(16) };
    int i=0;
    strcpy(descs[i++], "c10");
    strcpy(descs[i++], "c11");
    strcpy(descs[i++], "c1102");
    // we're not adding c20 on purpose to test NOROOT

    StgWord n_roots = sizeof(ptrs)/sizeof(*ptrs);
    setRootProfPtrs(n_roots, ptrs, (const char**)descs);

    bdescr *block_list[] = { sh.heap };
    Census *census = performHeapCensus(0, 1, block_list);
    endHeapCensus(census);

    // Remove references to synth heap because hs_exit will call
    // GarbageCollect
    setRootProfPtrs(0, NULL, NULL);

    freeSynthHeap(sh);

    hs_exit();
}
