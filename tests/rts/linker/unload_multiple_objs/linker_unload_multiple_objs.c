#include "Rts.h"
#include "ghcconfig.h"
#include "HsFFI.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void loadPackages(void);

#define NUM_OBJS 4

static char *objs[NUM_OBJS] = {"A.o", "B.o", "C.o", "D.o"};

pathchar* toPathchar(char* path)
{
#if defined(mingw32_HOST_OS)
    size_t required = strlen(path);
    pathchar *ret = (pathchar*)malloc(sizeof(pathchar) * (required + 1));
    if (mbstowcs(ret, path, required) == (size_t)-1)
    {
        errorBelch("toPathchar failed converting char* to wchar_t*: %s", path);
        exit(1);
    }
    ret[required] = '\0';
    return ret;
#else
    return path;
#endif
}

void load_and_resolve_all_objects() {
    int i, r;
    for (i = 0; i < NUM_OBJS; i++) {
        r = loadObj(toPathchar(objs[i]));
        if (!r) {
            errorBelch("loadObj(%s) failed", objs[i]);
            exit(1);
        }
    }

    r = resolveObjs();
    if (!r) {
        errorBelch("resolveObjs failed");
        exit(1);
    }

    for (i = 0; i < NUM_OBJS; i++) {
        char sym_name[138] = {0};
#if LEADING_UNDERSCORE
        sprintf(sym_name, "_createHeapObject%c", 'A'+i);
#else
        sprintf(sym_name, "createHeapObject%c", 'A'+i);
#endif
        void *sym_addr = lookupSymbol(sym_name);
        if (!sym_addr) {
            errorBelch("lookupSymbol(%s) failed", sym_name);
            exit(1);
        }
    }
}

void check_object_freed(char *obj_path) {
    OStatus st;
    st = getObjectLoadStatus(toPathchar(obj_path));
    if (st != OBJECT_NOT_LOADED) {
        errorBelch("object %s status != OBJECT_NOT_LOADED, is %d instead", obj_path, st);
        exit(1);
    }
}

void check_object_unloaded_but_not_freed(char *obj_path) {
    OStatus st;
    st = getObjectLoadStatus(toPathchar(obj_path));
    if (st != OBJECT_UNLOADED) {
        errorBelch("object %s status != OBJECT_UNLOADED, is %d instead", obj_path, st);
        exit(1);
    }
}

void test_no_dangling_references_to_unloaded_objects()
{
    load_and_resolve_all_objects();

    unloadObj(toPathchar("A.o"));
    unloadObj(toPathchar("B.o"));
    unloadObj(toPathchar("C.o"));
    unloadObj(toPathchar("D.o"));
    performMajorGC();

    check_object_freed("A.o");
    check_object_freed("B.o");
    check_object_freed("C.o");
    check_object_freed("D.o");

}

typedef HsStablePtr stableptrfun_t(void);
typedef void freeptrfun_t(HsStablePtr);

void test_still_has_references_to_unloaded_objects()
{
    load_and_resolve_all_objects();
#if LEADING_UNDERSCORE
    stableptrfun_t *createHeapObject = lookupSymbol("_createHeapObjectD");
    freeptrfun_t *freeHeapObject = lookupSymbol("_freeHeapObjectD");
#else
    stableptrfun_t *createHeapObject = lookupSymbol("createHeapObjectD");
    freeptrfun_t *freeHeapObject = lookupSymbol("freeHeapObjectD");
#endif
    HsStablePtr ptr = createHeapObject();

    unloadObj(toPathchar("A.o"));
    unloadObj(toPathchar("B.o"));
    unloadObj(toPathchar("C.o"));
    unloadObj(toPathchar("D.o"));
    performMajorGC();

    check_object_freed("A.o");
    check_object_freed("B.o");
    check_object_freed("C.o");
    check_object_unloaded_but_not_freed("D.o");


    freeHeapObject(ptr);
    performMajorGC();

    check_object_freed("A.o");
    check_object_freed("B.o");
    check_object_freed("C.o");
    check_object_freed("D.o");
}

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &argv, conf);

    initLinker_(0);
    loadPackages();

    test_still_has_references_to_unloaded_objects();
    test_no_dangling_references_to_unloaded_objects();

    hs_exit();
    exit(0);
}
