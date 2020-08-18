#include "ghcconfig.h"
#include <stdio.h>
#include <stdlib.h>
#include "Rts.h"
#include <string.h>
#include <dlfcn.h>

// poke into linker internals
extern void *objects;

#define ITERATIONS 1000

#if defined(mingw32_HOST_OS)
#define OBJPATH L"Test.so"
#define OBJPATH2 L"Test2.so"
#else
#define OBJPATH "./Test.so"
#define OBJPATH2 "./Test2.so"
#endif

typedef int testfun(int);

extern void loadPackages(void);

int main (int argc, char *argv[])
{
    testfun *f, *f2;
    int i, r;

    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    // we want to preserve static CAFs and unload dynamic CAFs
    conf.keep_cafs = true;
    setHighMemDynamic();
    hs_init_ghc(&argc, &argv, conf);

    initLinker_(0);

    loadPackages();

    for (i=0; i < ITERATIONS; i++) {
        char* errmsg;
        // load 2 libraries at once
        void* handle = loadNativeObj(OBJPATH, &errmsg);
        if (!handle) {
            errorBelch("loadNativeObj(%s) failed: %s", OBJPATH, errmsg);
            free(errmsg);
            exit(1);
        }

        void* handle2 = loadNativeObj(OBJPATH2, &errmsg);
        if (!handle2) {
            errorBelch("loadNativeObj(%s) failed: %s", OBJPATH2, errmsg);
            free(errmsg);
            exit(1);
        }
#if LEADING_UNDERSCORE
        f = dlsym(handle, "_f");
        f2 = dlsym(handle2, "_f");
#else
        f = dlsym(handle, "f");
        f2 = dlsym(handle2, "f");
#endif
        if (!f) {
            errorBelch("dlsym failed");
            exit(1);
        }
        r = f(3);
        if (r != 4) {
            errorBelch("call failed; %d", r);
            exit(1);
        }
        if (!f2) {
            errorBelch("dlsym failed");
            exit(1);
        }
        r = f2(3);
        if (r != 4) {
            errorBelch("call failed; %d", r);
            exit(1);
        }
        unloadNativeObj(handle);
        unloadNativeObj(handle2);
        performMajorGC();
        printf("%d ", i);
        fflush(stdout);
    }

    // Verify that Test.so isn't still loaded.
    int res = getObjectLoadStatus("Test.so") != OBJECT_NOT_LOADED;
    hs_exit();
    exit(res);
}
