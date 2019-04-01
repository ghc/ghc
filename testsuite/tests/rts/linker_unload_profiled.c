#include "ghcconfig.h"
#include <stdio.h>
#include <stdlib.h>
#include "Rts.h"
#include <string.h>

#define ITERATIONS 100

#if defined(mingw32_HOST_OS)
#define OBJPATH L"Test.o"
#else
#define OBJPATH "Test.o"
#endif

typedef int testfun(int);

extern void loadPackages(void);

int main (int argc, char *argv[])
{
    testfun *f;
    int i, r;
    OStatus st;

    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &argv, conf);

    initLinker_(0);

    loadPackages();

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

        st = getObjectLoadStatus(OBJPATH);
        if (st != OBJECT_RESOLVED) {
            errorBelch("%d: object status != OBJECT_RESOLVED", i);
            exit(1);
        }

        unloadObj(OBJPATH);

        st = getObjectLoadStatus(OBJPATH);
        if (st != OBJECT_UNLOADED) {
            errorBelch("%d: object status != OBJECT_UNLOADED", i);
            exit(1);
        }

        performMajorGC();

        st = getObjectLoadStatus(OBJPATH);
        if (st != OBJECT_NOT_LOADED) {
            errorBelch("%d: object status != OBJECT_NOT_LOADED", i);
            exit(1);
        }

        printf("%d ", i);
        fflush(stdout);
    }

    hs_exit();
    exit(0);
}
