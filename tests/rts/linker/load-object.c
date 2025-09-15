#include "ghcconfig.h"
#include "Rts.h"
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
    int r;
    char *obj;

    hs_init(&argc, &argv);

    initLinker_(0);

    // Load object file argv[1] once

    if (argc != 2) {
        errorBelch("usage: load-object <object-file>");
        exit(1);
    }

    obj = argv[1];

    r = loadObj(obj);
    if (!r) {
        debugBelch("loadObj(%s) failed\n", obj);
        exit(1);
    }
    r = resolveObjs();
    if (!r) {
        debugBelch("resolveObjs failed\n");
        unloadObj(obj);
        exit(1);
    }
    debugBelch("loading succeeded");

    hs_exit();
    return 0;
}
