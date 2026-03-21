// Test that the RTS linker executes .init_array entries on load and
// .fini_array entries on unload.  The loaded module increments a
// counter in its initializer and decrements it in its finalizer.

#include "Rts.h"
#include <stdio.h>

#if defined(mingw32_HOST_OS)
#define PATH_STR(str) L##str
#else
#define PATH_STR(str) str
#endif

int init_counter = 0;

int main(int argc, char *argv[]) {
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &argv, conf);

    initLinker_(0);
    insertSymbol(PATH_STR("main"), "init_counter", &init_counter);

    printf("counter before load: %d\n", init_counter);
    fflush(stdout);

    int ok;
    ok = loadObj(PATH_STR("Lib.o"));
    if (!ok) {
        errorBelch("loadObj(Lib.o) failed");
        return 1;
    }
    ok = resolveObjs();
    if (!ok) {
        errorBelch("resolveObjs() failed");
        return 1;
    }

    printf("counter after load: %d\n", init_counter);
    fflush(stdout);

    ok = unloadObj(PATH_STR("Lib.o"));
    if (!ok) {
        errorBelch("unloadObj(Lib.o) failed");
        return 1;
    }

    // GC triggers actual unloading and finalizer execution.
    performMajorGC();
    performMajorGC();

    printf("counter after unload: %d\n", init_counter);
    fflush(stdout);

    hs_exit();
    return 0;
}
