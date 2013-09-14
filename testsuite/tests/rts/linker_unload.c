#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "Rts.h"

#define ITERATIONS 10000

#if defined(mingw32_HOST_OS)
#define OBJPATH L"Test.o"
#else
#define OBJPATH "Test.o"
#endif

typedef int testfun(int);

void loadPkg(pathchar *path)
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
#if defined(mingw32_HOST_OS)
        size_t len = mbstowcs(NULL, argv[i], 0) + 1;
        if (len == -1) {
            errorBelch("invalid multibyte sequence in argument %d: %s", i, argv[i]);
            exit(1);
        }
        wchar_t *buf = (wchar_t*)_alloca(len * sizeof(wchar_t));
        size_t len2 = mbstowcs(buf, argv[i], len);
        if (len != len2 + 1) {
            errorBelch("something fishy is going on in argument %d: %s", i, argv[i]);
            exit(1);
        }
        loadPkg(buf);
#else
        loadPkg(argv[i]);
#endif
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
