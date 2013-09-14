#include <stdio.h>
#include "Rts.h"

#define ITERATIONS 10000
#define OBJPATH "Test.o"

typedef int testfun(int);

#define BASE "/home/simon/code-all/work/ghc-validate/libraries/base/dist-install/build/libHSbase-4.7.0.0.a"
#define GHCPRIM "/home/simon/code-all/work/ghc-validate/libraries/ghc-prim/dist-install/build/libHSghc-prim-0.3.1.0.a"

void loadPkg(char *path)
{
    int r;

    r = loadArchive(path);
    if (!r) {
        errorBelch("loadObjs(%s) failed", path);
        exit(1);
    }
}

int main (int argc, char *argv[])
{
    testfun *f;
    int i, r;

    hs_init(&argc, &argv);

    initLinker();

    for (i=1; i < argc; i++) {
        loadPkg(argv[i]);
    }

    for (i=0; i < ITERATIONS; i++) {
        r = loadObj(OBJPATH);
        if (!r) {
            errorBelch("loadObj(%s) failed", OBJPATH);
            exit(1);
        }
        r = resolveObjs();
        if (!r) {
            errorBelch("resolveObjs failed");
            exit(1);
        }
#if LEADING_UNDERSCORE
        f = lookupSymbol("_f");
#else
        f = lookupSymbol("f");
#endif
        if (!f) {
            errorBelch("lookupSymbol failed");
            exit(1);
        }
        r = f(3);
        if (r != 4) {
            errorBelch("call failed; %d", r);
            exit(1);
        }
        unloadObj(OBJPATH);
        performMajorGC();
        printf("%d ", i);
        fflush(stdout);
    }
}
