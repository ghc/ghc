#include "ghcconfig.h"
#include "Rts.h"
#include <stdio.h>
#include <stdlib.h>
#if defined(mingw32_HOST_OS)
#include <malloc.h>
#endif

#define ITERATIONS 10

typedef int testfun(int);

int main (int argc, char *argv[])
{
    testfun *f;
    int i, r;
#if defined(mingw32_HOST_OS)
    wchar_t *obj;
#else
    char *obj;
#endif

    hs_init(&argc, &argv);

    initLinker_(0);

    // Load object file argv[1] repeatedly

    if (argc != 2) {
        errorBelch("syntax: linker_error <object-file>");
        exit(1);
    }

#if defined(mingw32_HOST_OS)
    size_t len = mbstowcs(NULL, argv[1], 0) + 1;
    if (len == -1) {
        errorBelch("invalid multibyte sequence in argument %d: %s", i, argv[i]);
        exit(1);
    }
    wchar_t *buf = (wchar_t*)_alloca(len * sizeof(wchar_t));
    size_t len2 = mbstowcs(buf, argv[1], len);
    if (len != len2 + 1) {
        errorBelch("something fishy is going on in argument %d: %s", i, argv[i]);
        exit(1);
    }
    obj = buf;
#else
    obj = argv[1];
#endif

    for (i=0; i < ITERATIONS; i++) {
        r = loadObj(obj);
        if (!r) {
            debugBelch("loadObj(%s) failed\n", obj);
            continue;
        }
        r = resolveObjs();
        if (!r) {
            debugBelch("resolveObjs failed\n");
            // Mark the object as unloadable:
            unloadObj(obj);
            // Actually unload it:
            performMajorGC();
            continue;
        }
        errorBelch("loading succeeded");
        exit(1);
    }

    hs_exit();
    return 0;
}
