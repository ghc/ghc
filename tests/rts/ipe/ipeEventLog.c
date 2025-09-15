#include "Rts.h"
#include "RtsAPI.h"
#include "rts/IPE.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ipe_lib.h"

int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    Capability *cap = rts_lock();

    IpeBufferListNode *list1 = makeAnyProvEntries(cap, 0, 10);
    IpeBufferListNode *list2 = makeAnyProvEntries(cap, 0, 10);

    registerInfoProvList(list1);
    registerInfoProvList(list2);

    // Trace all IPE events. Expected count (see Makefile): 381 + 2 = 383
    dumpIPEToEventLog();

    rts_unlock(cap);
    hs_exit();
}
