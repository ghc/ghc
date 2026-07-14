
#include "Rts.h"
#include "rts/RtsSymbols.h"

#include <stdio.h>

void dumpDeclaredSymbols(void);

void dumpDeclaredSymbols(void) {
    const RtsSymbolVal * declaredSyms = getRtsSymbols();
    for (const RtsSymbolVal * sym = &declaredSyms[0]; sym->lbl; sym++) {
        // Ignore hidden
        if (sym->type & SYM_TYPE_HIDDEN) continue;

        char * type = NULL;
        if (sym->type & SYM_TYPE_CODE) {
            type = "code";
        } else if (sym->type & (SYM_TYPE_DATA | SYM_TYPE_INDIRECT_DATA)) {
            type = "data";
        }

        if (type) {
            printf("%s %s\n", sym->lbl, type);
        }
    }
}
