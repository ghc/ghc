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

int main(int c, char *v[])
{
    char *argv[] = { v[0], "+RTS", "-hT" }, **argvp = argv;
    int argc = sizeof(argv)/sizeof(*argv);
    hs_init(&argc, &argvp);

    hp_debug_file = stdout;

    sh = initializeTestHeap();

    bdescr *block_list[] = { sh.heap };
    Census *census = performHeapCensus(0, 1, block_list);
    endHeapCensus(census);

    setRootProfPtrs(0, NULL, NULL);

    freeSynthHeap(sh);

    hs_exit();
}
