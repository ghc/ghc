#include "Rts.h"
#include "RtsAPI.h"
#include "rts/IPE.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void dumpIPEToEventLog(void);
InfoProvEnt *makeAnyProvEntry(Capability *cap, int i);

int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    Capability *cap = rts_lock();

    HaskellObj one = rts_mkInt(cap, 1);

    InfoProvEnt *provEnt_0 = makeAnyProvEntry(cap, 0);
    InfoProvEnt *provEnt_1 = makeAnyProvEntry(cap, 1);

    InfoProvEnt **ipeList_1 = malloc(sizeof(InfoProvEnt *) * 3);
    ipeList_1[0] = provEnt_0;
    ipeList_1[1] = provEnt_1;
    ipeList_1[2] = NULL;

    registerInfoProvList(ipeList_1);

    // Query an IPE to initialize the underlying hash map.
    lookupIPE(ipeList_1[0]->info);

    // Trace all IPE events.
    dumpIPEToEventLog();

    rts_unlock(cap);
    hs_exit();
}
