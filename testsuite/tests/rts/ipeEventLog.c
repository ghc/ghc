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

    // Force the creation of 4 IpeBufferListNodes (381 IPEs)
    for (int i = 0; i < 381; i++) {

        InfoProvEnt **ipeList_1 = malloc(sizeof(InfoProvEnt *) * 2);
        ipeList_1[0] = makeAnyProvEntry(cap, i);
        ipeList_1[1] = NULL;

        registerInfoProvList(ipeList_1);
    }

    // Register an IPE list with two elements
    HaskellObj one = rts_mkInt(cap, 1);

    InfoProvEnt *provEntA = malloc(sizeof(InfoProvEnt));
    provEntA->info = (StgInfoTable *)one->header.info;
    provEntA->prov.table_name = "table_name_a";
    provEntA->prov.closure_desc = "closure_desc_a";
    provEntA->prov.ty_desc = "ty_desc_a";
    provEntA->prov.label = "label_a";
    provEntA->prov.module = "module_a";
    provEntA->prov.srcloc = "srcloc_a";

    HaskellObj two = rts_mkInt(cap, 2);

    InfoProvEnt *provEntB = malloc(sizeof(InfoProvEnt));
    provEntB->info = (StgInfoTable *)two->header.info;
    provEntB->prov.table_name = "table_name_b";
    provEntB->prov.closure_desc = "closure_desc_b";
    provEntB->prov.ty_desc = "ty_desc_b";
    provEntB->prov.label = "label_b";
    provEntB->prov.module = "module_b";
    provEntB->prov.srcloc = "srcloc_b";

    InfoProvEnt **ipeList_2 = malloc(sizeof(InfoProvEnt *) * 3);
    ipeList_2[0] = provEntA;
    ipeList_2[1] = provEntB;
    ipeList_2[2] = NULL;

    registerInfoProvList(ipeList_2);

    // Trace all IPE events. Expected count (see Makefile): 381 + 2 = 383
    dumpIPEToEventLog();

    rts_unlock(cap);
    hs_exit();
}
