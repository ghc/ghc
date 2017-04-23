#include "linker/elf_util.h"
#include "ElfTypes.h"

#if defined(OBJFORMAT_ELF)

ElfSymbolTable *
find_symbol_table(ObjectCode * oc, unsigned symolTableIndex) {
    for(ElfSymbolTable * t=oc->info->symbolTables; t != NULL; t = t->next)
        if(t->index == symolTableIndex)
            return t;
    return NULL;
}

ElfSymbol *
find_symbol(ObjectCode * oc, unsigned symbolTableIndex, unsigned long
symbolIndex) {
    ElfSymbolTable * t = find_symbol_table(oc, symbolTableIndex);
    if(NULL != t && symbolIndex < t->n_symbols) {
        return &t->symbols[symbolIndex];
    }
    return NULL;
}

#endif
