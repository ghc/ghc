#include "Rts.h"
#include "elf_got.h"

#include <string.h>

#if defined(OBJFORMAT_ELF)
/*
 * Check if we need a global offset table slot for a
 * given symbol
 */
bool
needGotSlot(Elf_Sym * symbol) {
    /* using global here should give an upper bound */
    /* I don't believe we need to relocate STB_LOCAL
     * symbols via the GOT; however I'm unsure about
     * STB_WEAK.
     *
     * Any more restrictive filter here would result
     * in a smaller GOT, which is preferrable.
     */
    return ELF_ST_BIND(symbol->st_info) == STB_GLOBAL
        || ELF_ST_BIND(symbol->st_info) == STB_WEAK;
}

bool
makeGot(ObjectCode * oc) {
    size_t got_slots = 0;

    /* we need to find all symbol tables (elf can have multiple)
     * and need to iterate over all symbols, to check how many
     * got slots we need at most
     */
    ASSERT( oc->info != NULL );
    ASSERT( oc->info->sectionHeader != NULL );
    for(int i=0; i < oc->n_sections; i++) {
        if(SHT_SYMTAB == oc->info->sectionHeader[i].sh_type) {
            Elf_Sym *symTab =
                (Elf_Sym*)((uint8_t*)oc->info->elfHeader
                                   + oc->info->sectionHeader[i].sh_offset);
            size_t n_symbols = oc->info->sectionHeader[i].sh_size
                               / sizeof(Elf_Sym);
            for(size_t j=0; j < n_symbols; j++) {
                if(needGotSlot(&symTab[j])) {
                    got_slots += 1;
                }
            }
        }
    }
    if(got_slots > 0) {
        oc->info->got_size = got_slots * sizeof(void *);
         void * mem = mmap(NULL, oc->info->got_size,
                           PROT_READ | PROT_WRITE,
                           MAP_ANON | MAP_PRIVATE,
                           -1, 0);
        if (mem == MAP_FAILED) {
            errorBelch("MAP_FAILED. errno=%d", errno);
            return EXIT_FAILURE;
        }

        oc->info->got_start = (void*)mem;
        /* update got_addr */
        size_t slot = 0;
        for(ElfSymbolTable *symTab = oc->info->symbolTables;
            symTab != NULL; symTab = symTab->next) {

            for(size_t i=0; i < symTab->n_symbols; i++)
                if(needGotSlot(symTab->symbols[i].elf_sym))
                    symTab->symbols[i].got_addr
                            = (uint8_t *)oc->info->got_start
                              + (slot++ * sizeof(void*));
        }
    }
    return EXIT_SUCCESS;
}

bool
fillGot(ObjectCode * oc) {
    /* fill the GOT table */
    for(ElfSymbolTable *symTab = oc->info->symbolTables;
        symTab != NULL; symTab = symTab->next) {

        for(size_t i=0; i < symTab->n_symbols; i++) {
            ElfSymbol * symbol = &symTab->symbols[i];

            if(needGotSlot(symbol->elf_sym)) {

                /* no type are undefined symbols */
                if(   STT_NOTYPE == ELF_ST_TYPE(symbol->elf_sym->st_info)
                   || STB_WEAK   == ELF_ST_BIND(symbol->elf_sym->st_info)) {
                    if(0x0 == symbol->addr) {
                        symbol->addr = lookupDependentSymbol(symbol->name, oc);
                        if(0x0 == symbol->addr) {
                            if(0 == strncmp(symbol->name,"_GLOBAL_OFFSET_TABLE_",21)) {
                                symbol->addr = oc->info->got_start;
                            } else {
                                errorBelch("Failed to lookup symbol: %s\n",
                                           symbol->name);
                                return EXIT_FAILURE;
                            }
                        }
                    } else {
                        // we already have the address.
                    }
                } /* else it was defined somewhere in the same object, and
                  * we should have the address already.
                  */

                if(0x0 == symbol->addr) {
                    errorBelch(
                        "Something went wrong! Symbol %s has null address.\n",
                            symbol->name);
                    return EXIT_FAILURE;
                }

                if(0x0 == symbol->got_addr) {
                    errorBelch("Not good either!");
                    return EXIT_FAILURE;
                }

                *(void**)symbol->got_addr = symbol->addr;
            }
        }
    }

    // We are done initializing the GOT; freeze it.
    if(mprotect(oc->info->got_start, oc->info->got_size, PROT_READ) != 0) {
        sysErrorBelch("unable to protect memory");
    }
    return EXIT_SUCCESS;
}

bool
verifyGot(ObjectCode * oc) {
    for(ElfSymbolTable *symTab = oc->info->symbolTables;
        symTab != NULL; symTab = symTab->next) {
        for(size_t i=0; i < symTab->n_symbols; i++) {
            ElfSymbol * symbol = &symTab->symbols[i];
            if(symbol->got_addr) {
                ASSERT((void*)(*(void**)symbol->got_addr)
                       == (void*)symbol->addr);
            }
            ASSERT(0 == ((uintptr_t)symbol->addr & 0xffff000000000000));
        }
    }
    return EXIT_SUCCESS;
}

void
freeGot(ObjectCode * oc) {
//    munmap(oc->info->got_start, oc->info->got_size);
    oc->info->got_start = 0x0;
    oc->info->got_size = 0;
}
#endif
