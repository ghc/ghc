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

    HaskellObj one = rts_mkInt(cap, 1);

    IpeBufferListNode *list1 = makeAnyProvEntries(cap, 0, 10);
    IpeBufferListNode *list2 = makeAnyProvEntries(cap, 0, 10);

    registerInfoProvList(list1);
    registerInfoProvList(list2);

    // Query an IPE to initialize the underlying hash map.
    lookupIPE(list1->tables[0]);

    // Trace all IPE events.
    dumpIPEToEventLog();

    rts_unlock(cap);
    hs_exit();
}
