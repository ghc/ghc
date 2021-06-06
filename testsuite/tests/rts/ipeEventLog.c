#include "Rts.h"
#include "RtsAPI.h"
#include "rts/IPE.h"
#include <stdio.h>
#include <stdlib.h>

extern void dumpIPEToEventLog(void);

int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    Capability *cap = rts_lock();

    // Force the creation of 4 IpeBufferListNodes (381 IPEs)
    for (int i = 0; i <= 126 * 3 + 1; i++) {
        HaskellObj fourtyTwo = rts_mkInt(cap, 42);

        InfoProvEnt *provEnt = malloc(sizeof(InfoProvEnt));
        provEnt->info = (StgInfoTable *)fourtyTwo->header.info;

        char *tableName = malloc(sizeof(char) * 14);
        snprintf(tableName, 14, "table_name_%i", i);
        provEnt->prov.table_name = tableName;

        char *closureDesc = malloc(sizeof(char) * 16);
        snprintf(closureDesc, 16, "closure_desc_%i", i);
        provEnt->prov.closure_desc = closureDesc;

        char *tyDesc = malloc(sizeof(char) * 11);
        snprintf(tyDesc, 11, "ty_desc_%i", i);
        provEnt->prov.ty_desc = tyDesc;

        char *label = malloc(sizeof(char) * 9);
        snprintf(label, 9, "label_%i", i);
        provEnt->prov.label = label;

        char *module = malloc(sizeof(char) * 10);
        snprintf(module, 10, "module_%i", i);
        provEnt->prov.module = module;

        char *srcLoc = malloc(sizeof(char) * 10);
        snprintf(srcLoc, 10, "srcloc_%i", i);
        provEnt->prov.srcloc = srcLoc;

        InfoProvEnt **ipeList_1 = malloc(sizeof(InfoProvEnt *) * 2);
        ipeList_1[0] = provEnt;
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
