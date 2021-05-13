#include "Rts.h"
#include "rts/IPE.h"
#include <string.h>

InfoProvEnt *makeAnyProvEntry(Capability *cap, int i) {
    HaskellObj fourtyTwo = rts_mkInt(cap, 42);

    InfoProvEnt *provEnt = malloc(sizeof(InfoProvEnt));
    provEnt->info = (StgInfoTable *)fourtyTwo->header.info;

    unsigned int tableNameLength = strlen("table_name_") + 3 /* digits */ + 1 /* null character */;
    char *tableName = malloc(sizeof(char) * tableNameLength);
    snprintf(tableName, tableNameLength, "table_name_%03i", i);
    provEnt->prov.table_name = tableName;

    unsigned int closureDescLength = strlen("closure_desc_") + 3 /* digits */ + 1 /* null character */;
    char *closureDesc = malloc(sizeof(char) * closureDescLength);
    snprintf(closureDesc, closureDescLength, "closure_desc_%03i", i);
    provEnt->prov.closure_desc = closureDesc;

    unsigned int tyDescLength = strlen("ty_desc_") + 3 /* digits */ + 1 /* null character */;
    char *tyDesc = malloc(sizeof(char) * tyDescLength);
    snprintf(tyDesc, tyDescLength, "ty_desc_%03i", i);
    provEnt->prov.ty_desc = tyDesc;

    unsigned int labelLength = strlen("label_") + 3 /* digits */ + 1 /* null character */;
    char *label = malloc(sizeof(char) * labelLength);
    snprintf(label, labelLength, "label_%03i", i);
    provEnt->prov.label = label;

    unsigned int moduleLength = strlen("module_") + 3 /* digits */ + 1 /* null character */;
    char *module = malloc(sizeof(char) * labelLength);
    snprintf(module, moduleLength, "module_%03i", i);
    provEnt->prov.module = module;

    unsigned int srcLocLength = strlen("srcloc_") + 3 /* digits */ + 1 /* null character */;
    char *srcLoc = malloc(sizeof(char) * srcLocLength);
    snprintf(srcLoc, srcLocLength, "srcloc_%03i", i);
    provEnt->prov.srcloc = srcLoc;

    return provEnt;
}
