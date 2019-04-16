#include "ghcconfig.h"
#include <stdio.h>
#include <stdlib.h>
#include "Rts.h"
#include <string.h>

#define ITERATIONS 1000

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
        unloadObj(OBJPATH);
        performMajorGC();
        printf("%d ", i);
        fflush(stdout);
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
        // check that we can purge first, then unload
        purgeObj(OBJPATH);
        performMajorGC();
        unloadObj(OBJPATH);
        performMajorGC();
        printf("%d ", i);
        fflush(stdout);
    }

    hs_exit();
    exit(0);
}
