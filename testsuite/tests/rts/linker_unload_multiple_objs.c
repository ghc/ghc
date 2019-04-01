#include "ghcconfig.h"
#include <stdio.h>
#include <stdlib.h>
#include "Rts.h"
#include <string.h>
#include "HsFFI.h"

extern void loadPackages(void);

#define NUM_OBJS 4

static char *objs[NUM_OBJS] = {"A.o", "B.o", "C.o", "D.o"};

void load_and_resolve_all_objects() {
    int i, r;
    for (i = 0; i < NUM_OBJS; i++) {
        r = loadObj(objs[i]);
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
        char sym_name[8] = {0};
        sprintf(sym_name, "%c_id%d_closure", 'A'+i, i+1);
        void *sym_addr = lookupSymbol(sym_name);
        if (!sym_addr) {
            errorBelch("lookupSymbol(%s) failed", sym_name);
            exit(1);
        }
    }
}

void check_object_freed(char *obj_path) {
    OStatus st;
    st = getObjectLoadStatus(obj_path);
    if (st != OBJECT_NOT_LOADED) {
        errorBelch("object %s status != OBJECT_NOT_LOADED", obj_path);
        exit(1);
    }
}

void check_object_unloaded_but_not_freed(char *obj_path) {
    OStatus st;
    st = getObjectLoadStatus(obj_path);
    if (st != OBJECT_UNLOADED) {
        errorBelch("object %s status != OBJECT_UNLOADED, is %d instead", obj_path, st);
        exit(1);
    }
}

void test_no_dangling_references_to_unloaded_objects()
{
    load_and_resolve_all_objects();

    unloadObj("A.o");
    unloadObj("B.o");
    unloadObj("C.o");
    unloadObj("D.o");
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
    stableptrfun_t *createHeapObject = lookupSymbol("createHeapObjectD");
    freeptrfun_t *freeHeapObject = lookupSymbol("freeHeapObjectD");
    HsStablePtr ptr = createHeapObject();

    unloadObj("A.o");
    unloadObj("B.o");
    unloadObj("C.o");
    unloadObj("D.o");
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
