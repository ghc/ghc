#include "linker/elf_util.h"

#if defined(OBJFORMAT_ELF)

ElfSymbolTable *
findSymbolTable(ObjectCode * oc, unsigned symolTableIndex) {
    for(ElfSymbolTable * t=oc->info->symbolTables; t != NULL; t = t->next)
        if(t->index == symolTableIndex)
            return t;
    return NULL;
}

ElfSymbol *
findSymbol(ObjectCode * oc, unsigned symbolTableIndex,
           unsigned long symbolIndex) {
    ElfSymbolTable * t = findSymbolTable(oc, symbolTableIndex);
    if(NULL != t && symbolIndex < t->n_symbols) {
        return &t->symbols[symbolIndex];
    }
    return NULL;
}

#endif
