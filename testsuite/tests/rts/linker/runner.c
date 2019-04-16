#include <Rts.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define MAX_SYMNAME 256

typedef long (*fun_t)(void);

int main(int argc, char *argv[])
{
    fun_t f;
    int ok;
    char * objpath;

    hs_init(&argc, &argv);

    initLinker();

    if (argc < 3) {
            errorBelch("usage: runner <objpath> <symname>");
            exit(1);
    }
    objpath = argv[1];

#if defined(darwin_HOST_OS)
    char symname[MAX_SYMNAME + 1];
    symname[0] = '_';
    strncpy(&symname[1], argv[2], MAX_SYMNAME);
#else
    char * symname = argv[2];
#endif

    printf("Linking: path = %s, symname = %s\n", objpath, symname);

    ok = loadObj(objpath);
    if (!ok) {
            errorBelch("loadObj(%s) failed", objpath);
            exit(1);
    }

    ok = resolveObjs();
    if (!ok) {
            errorBelch("resolveObjs failed");
            exit(1);
    }

    f = lookupSymbol(symname);
    if (!f) {
            errorBelch("lookupSymbol failed");
            exit(1);
    }

    printf("%ld\n", f());

    ok = unloadObj(objpath);
    if (!ok) {
            errorBelch("unloadObj(%s) failed", objpath);
            exit(1);
    }

    return 0;
}

