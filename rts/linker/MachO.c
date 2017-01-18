#include "Rts.h"

#ifdef darwin_HOST_OS

#include "RtsUtils.h"
#include "GetEnv.h"
#include "LinkerInternals.h"
#include "linker/MachO.h"
#include "linker/CacheFlush.h"
#include "linker/SymbolExtras.h"

#include <string.h>
#include <regex.h>
#include <mach/machine.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>

#if defined(HAVE_SYS_MMAN_H)
#  include <sys/mman.h>
#endif

#if defined(powerpc_HOST_ARCH)
#  include <mach-o/ppc/reloc.h>
#endif

#if defined(x86_64_HOST_ARCH)
#  include <mach-o/x86_64/reloc.h>
#endif

/*
  Support for MachO linking on Darwin/MacOS X
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)

  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) implement ocVerifyImage_MachO
  *) add still more sanity checks.
*/

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
#define mach_header mach_header_64
#define segment_command segment_command_64
#define section section_64
#define nlist nlist_64
#endif

static int
resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist);

#if defined(powerpc_HOST_ARCH)
int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Find out the first and last undefined external
                // symbol, so we don't have to allocate too many
            // jump islands/GOT entries.

            struct symtab_command *symLC = (struct symtab_command *) lc;
            unsigned min = symLC->nsyms, max = 0;
            struct nlist *nlist =
                symLC ? (struct nlist*) ((char*) oc->image + symLC->symoff)
                      : NULL;

            for (i = 0; i < symLC->nsyms; i++) {

                if (nlist[i].n_type & N_STAB) {
                    ;
                } else if (nlist[i].n_type & N_EXT) {

                    if((nlist[i].n_type & N_TYPE) == N_UNDF
                        && (nlist[i].n_value == 0)) {

                        if (i < min) {
                            min = i;
                        }

                        if (i > max) {
                            max = i;
                    }
                }
            }
            }

            if (max >= min) {
                return ocAllocateSymbolExtras(oc, max - min + 1, min);
            }

            break;
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    return ocAllocateSymbolExtras(oc,0,0);
}

#elif defined(x86_64_HOST_ARCH)

int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Just allocate one entry for every symbol
            struct symtab_command *symLC = (struct symtab_command *) lc;

            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocate %d symbols\n", symLC->nsyms));
            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
            return ocAllocateSymbolExtras(oc, symLC->nsyms, 0);
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocated no symbols\n"));
    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
    return ocAllocateSymbolExtras(oc,0,0);
}

#else
#error Unknown MachO architecture
#endif /* HOST_ARCH */

int
ocVerifyImage_MachO(ObjectCode * oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;

    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: start\n"));

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header->magic != MH_MAGIC_64) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (64bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC_64, header->magic,
                   header->magic == MH_MAGIC ? " (32bit)." : ".");
        return 0;
    }
#else
    if(header->magic != MH_MAGIC) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (32bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC, header->magic,
                   header->magic == MH_MAGIC_64 ? " (64bit)." : ".");
        return 0;
    }
#endif

    // FIXME: do some more verifying here
    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: done\n"));
    return 1;
}

static int
resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist)
{
    unsigned i;
    size_t itemSize = 4;

    IF_DEBUG(linker, debugBelch("resolveImports: start\n"));

#if i386_HOST_ARCH
    int isJumpTable = 0;

    if (strcmp(sect->sectname,"__jump_table") == 0) {
        isJumpTable = 1;
        itemSize = 5;
        ASSERT(sect->reserved2 == itemSize);
    }

#endif

    for(i = 0; i * itemSize < sect->size; i++)
    {
        // according to otool, reserved1 contains the first index into the indirect symbol table
        struct nlist *symbol = &nlist[indirectSyms[sect->reserved1+i]];
        SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
        SymbolAddr* addr = NULL;

        IF_DEBUG(linker, debugBelch("resolveImports: resolving %s\n", nm));

        if ((symbol->n_type & N_TYPE) == N_UNDF
            && (symbol->n_type & N_EXT) && (symbol->n_value != 0)) {
            addr = (SymbolAddr*) (symbol->n_value);
            IF_DEBUG(linker, debugBelch("resolveImports: undefined external %s has value %p\n", nm, addr));
        } else {
            addr = lookupSymbol_(nm);
            IF_DEBUG(linker, debugBelch("resolveImports: looking up %s, %p\n", nm, addr));
        }

        if (addr == NULL)
        {
            errorBelch("\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, nm);
            return 0;
        }
        ASSERT(addr);

#if i386_HOST_ARCH
        if (isJumpTable) {
            checkProddableBlock(oc,image + sect->offset + i*itemSize, 5);

            *(image + sect->offset + i * itemSize) = 0xe9; // jmp opcode
            *(unsigned*)(image + sect->offset + i*itemSize + 1)
                = (SymbolAddr*)addr - (image + sect->offset + i*itemSize + 5);
        }
        else
#endif
        {
            checkProddableBlock(oc,
                                ((void**)(image + sect->offset)) + i,
                                sizeof(void *));
            ((void**)(image + sect->offset))[i] = addr;
        }
    }

    IF_DEBUG(linker, debugBelch("resolveImports: done\n"));
    return 1;
}

static unsigned long
relocateAddress(
    ObjectCode* oc,
    int nSections,
    struct section* sections,
    unsigned long address)
{
    int i;
    IF_DEBUG(linker, debugBelch("relocateAddress: start\n"));
    for (i = 0; i < nSections; i++)
    {
            IF_DEBUG(linker, debugBelch("    relocating address in section %d\n", i));
        if (sections[i].addr <= address
            && address < sections[i].addr + sections[i].size)
        {
            return (unsigned long)oc->image
                    + sections[i].offset + address - sections[i].addr;
        }
    }
    barf("Invalid Mach-O file:"
         "Address out of bounds while relocating object file");
    return 0;
}

static int
relocateSection(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC, struct nlist *nlist,
    int nSections, struct section* sections, struct section *sect)
{
    struct relocation_info *relocs;
    int i, n;

    IF_DEBUG(linker, debugBelch("relocateSection: start\n"));

    if(!strcmp(sect->sectname,"__la_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__nl_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr2"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr3"))
        return 1;

    n = sect->nreloc;
    IF_DEBUG(linker, debugBelch("relocateSection: number of relocations: %d\n", n));

    relocs = (struct relocation_info*) (image + sect->reloff);

    for(i = 0; i < n; i++)
    {
#ifdef x86_64_HOST_ARCH
        struct relocation_info *reloc = &relocs[i];

        char    *thingPtr = image + sect->offset + reloc->r_address;
        uint64_t thing;
        /* We shouldn't need to initialise this, but gcc on OS X 64 bit
           complains that it may be used uninitialized if we don't */
        uint64_t value = 0;
        uint64_t baseValue;
        int type = reloc->r_type;

        IF_DEBUG(linker, debugBelch("relocateSection: relocation %d\n", i));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));
        IF_DEBUG(linker, debugBelch("               : address   = %d\n", reloc->r_address));
        IF_DEBUG(linker, debugBelch("               : symbolnum = %u\n", reloc->r_symbolnum));
        IF_DEBUG(linker, debugBelch("               : pcrel     = %d\n", reloc->r_pcrel));
        IF_DEBUG(linker, debugBelch("               : length    = %d\n", reloc->r_length));
        IF_DEBUG(linker, debugBelch("               : extern    = %d\n", reloc->r_extern));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));

        switch(reloc->r_length)
        {
            case 0:
                checkProddableBlock(oc,thingPtr,1);
                thing = *(uint8_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 1;
                break;
            case 1:
                checkProddableBlock(oc,thingPtr,2);
                thing = *(uint16_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 2;
                break;
            case 2:
                checkProddableBlock(oc,thingPtr,4);
                thing = *(uint32_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 4;
                break;
            case 3:
                checkProddableBlock(oc,thingPtr,8);
                thing = *(uint64_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 8;
                break;
            default:
                barf("Unknown size.");
        }

        IF_DEBUG(linker,
                 debugBelch("relocateSection: length = %d, thing = %" PRId64 ", baseValue = %p\n",
                            reloc->r_length, thing, (char *)baseValue));

        if (type == X86_64_RELOC_GOT
         || type == X86_64_RELOC_GOT_LOAD)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: making jump island for %s, extern = %d, X86_64_RELOC_GOT\n", nm, reloc->r_extern));

            ASSERT(reloc->r_extern);
            if (reloc->r_extern == 0) {
                    errorBelch("\nrelocateSection: global offset table relocation for symbol with r_extern == 0\n");
            }

            if (symbol->n_type & N_EXT) {
                    // The external bit is set, meaning the symbol is exported,
                    // and therefore can be looked up in this object module's
                    // symtab, or it is undefined, meaning dlsym must be used
                    // to resolve it.

                    addr = lookupSymbol_(nm);
                    IF_DEBUG(linker, debugBelch("relocateSection: looked up %s, "
                                                "external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n", nm));
                    IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));

                    if (addr == NULL) {
                            errorBelch("\nlookupSymbol failed in relocateSection (RELOC_GOT)\n"
                                       "%s: unknown symbol `%s'", oc->fileName, nm);
                            return 0;
                    }
            } else {
                    IF_DEBUG(linker, debugBelch("relocateSection: %s is not an exported symbol\n", nm));

                    // The symbol is not exported, or defined in another
                    // module, so it must be in the current object module,
                    // at the location given by the section index and
                    // symbol address (symbol->n_value)

                    if ((symbol->n_type & N_TYPE) == N_SECT) {
                            addr = (void *)relocateAddress(oc, nSections, sections, symbol->n_value);
                            IF_DEBUG(linker, debugBelch("relocateSection: calculated relocation %p of "
                                                        "non-external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n",
                                                        (void *)symbol->n_value));
                            IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));
                    } else {
                            errorBelch("\nrelocateSection: %s is not exported,"
                                       " and should be defined in a section, but isn't!\n", nm);
                    }
            }

            value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, (unsigned long)addr)->addr;

            type = X86_64_RELOC_SIGNED;
        }
        else if (reloc->r_extern)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: looking up external symbol %s\n", nm));
            IF_DEBUG(linker, debugBelch("               : type  = %d\n", symbol->n_type));
            IF_DEBUG(linker, debugBelch("               : sect  = %d\n", symbol->n_sect));
            IF_DEBUG(linker, debugBelch("               : desc  = %d\n", symbol->n_desc));
            IF_DEBUG(linker, debugBelch("               : value = %p\n", (void *)symbol->n_value));

            if ((symbol->n_type & N_TYPE) == N_SECT) {
                value = relocateAddress(oc, nSections, sections,
                                        symbol->n_value);
                IF_DEBUG(linker, debugBelch("relocateSection, defined external symbol %s, relocated address %p\n", nm, (void *)value));
            }
            else {
                addr = lookupSymbol_(nm);
                if (addr == NULL)
                {
                     errorBelch("\nlookupSymbol failed in relocateSection (relocate external)\n"
                                "%s: unknown symbol `%s'", oc->fileName, nm);
                     return 0;
                }

                value = (uint64_t) addr;
                IF_DEBUG(linker, debugBelch("relocateSection: external symbol %s, address %p\n", nm, (void *)value));
            }
        }
        else
        {
            // If the relocation is not through the global offset table
            // or external, then set the value to the baseValue.  This
            // will leave displacements into the __const section
            // unchanged (as they ought to be).

            value = baseValue;
        }

        IF_DEBUG(linker, debugBelch("relocateSection: value = %p\n", (void *)value));

        if (type == X86_64_RELOC_BRANCH)
        {
            if((int32_t)(value - baseValue) != (int64_t)(value - baseValue))
            {
                ASSERT(reloc->r_extern);
                value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, value)
                                        -> jumpIsland;
            }
            ASSERT((int32_t)(value - baseValue) == (int64_t)(value - baseValue));
            type = X86_64_RELOC_SIGNED;
        }

        switch(type)
        {
            case X86_64_RELOC_UNSIGNED:
                ASSERT(!reloc->r_pcrel);
                thing += value;
                break;
            case X86_64_RELOC_SIGNED:
            case X86_64_RELOC_SIGNED_1:
            case X86_64_RELOC_SIGNED_2:
            case X86_64_RELOC_SIGNED_4:
                ASSERT(reloc->r_pcrel);
                thing += value - baseValue;
                break;
            case X86_64_RELOC_SUBTRACTOR:
                ASSERT(!reloc->r_pcrel);
                thing -= value;
                break;
            default:
                barf("unkown relocation");
        }

        switch(reloc->r_length)
        {
            case 0:
                *(uint8_t*)thingPtr = thing;
                break;
            case 1:
                *(uint16_t*)thingPtr = thing;
                break;
            case 2:
                *(uint32_t*)thingPtr = thing;
                break;
            case 3:
                *(uint64_t*)thingPtr = thing;
                break;
        }
#else /* x86_64_HOST_ARCH */
        if(relocs[i].r_address & R_SCATTERED)
        {
            struct scattered_relocation_info *scat =
                (struct scattered_relocation_info*) &relocs[i];

            if(!scat->r_pcrel)
            {
                if(scat->r_length == 2)
                {
                    unsigned long word = 0;
                    unsigned long* wordPtr = (unsigned long*) (image + sect->offset + scat->r_address);

                    /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                       on powerpc_HOST_ARCH */
                    checkProddableBlock(oc,wordPtr,sizeof(unsigned long));

                    // Note on relocation types:
                    // i386 uses the GENERIC_RELOC_* types,
                    // while ppc uses special PPC_RELOC_* types.
                    // *_RELOC_VANILLA and *_RELOC_PAIR have the same value
                    // in both cases, all others are different.
                    // Therefore, we use GENERIC_RELOC_VANILLA
                    // and GENERIC_RELOC_PAIR instead of the PPC variants,
                    // and use #ifdefs for the other types.

                    // Step 1: Figure out what the relocated value should be
                    if (scat->r_type == GENERIC_RELOC_VANILLA) {
                        word = *wordPtr
                             + (unsigned long) relocateAddress(oc,
                                                                nSections,
                                                                sections,
                                                                scat->r_value)
                                        - scat->r_value;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_SECTDIFF
                        || scat->r_type == PPC_RELOC_LO16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HI16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HA16_SECTDIFF
                        || scat->r_type == PPC_RELOC_LOCAL_SECTDIFF)
#else /* powerpc_HOST_ARCH */
                    else if(scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif /* powerpc_HOST_ARCH */
                    {
                        struct scattered_relocation_info *pair =
                                (struct scattered_relocation_info*) &relocs[i+1];

                        if (!pair->r_scattered || pair->r_type != GENERIC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "RELOC_*_SECTDIFF not followed by RELOC_PAIR");
                        }

                        word = (unsigned long)
                               (relocateAddress(oc, nSections, sections, scat->r_value)
                              - relocateAddress(oc, nSections, sections, pair->r_value));
                        i++;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_HI16
                         || scat->r_type == PPC_RELOC_LO16
                         || scat->r_type == PPC_RELOC_HA16
                         || scat->r_type == PPC_RELOC_LO14)
                    {   // these are generated by label+offset things
                        struct relocation_info *pair = &relocs[i+1];

                        if ((pair->r_address & R_SCATTERED) || pair->r_type != PPC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "PPC_RELOC_* not followed by PPC_RELOC_PAIR");
                        }

                        if(scat->r_type == PPC_RELOC_LO16)
                        {
                            word = ((unsigned short*) wordPtr)[1];
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_LO14)
                        {
                            barf("Unsupported Relocation: PPC_RELOC_LO14");
                            word = ((unsigned short*) wordPtr)[1] & 0xFFFC;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_HI16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                        }
                        else if(scat->r_type == PPC_RELOC_HA16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                        }


                        word += (unsigned long) relocateAddress(oc, nSections, sections, scat->r_value)
                                                - scat->r_value;

                        i++;
                    }
#endif /* powerpc_HOST_ARCH */
                    else {
                        barf ("Don't know how to handle this Mach-O "
                              "scattered relocation entry: "
                              "object file %s; entry type %ld; "
                              "address %#lx\n",
                              OC_INFORMATIVE_FILENAME(oc),
                              scat->r_type,
                              scat->r_address);
                        return 0;
                     }

#ifdef powerpc_HOST_ARCH
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == PPC_RELOC_SECTDIFF)
#else /* powerpc_HOST_ARCH */
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif /* powerpc_HOST_ARCH */
                    {
                        *wordPtr = word;
                    }
#ifdef powerpc_HOST_ARCH
                    else if (scat->r_type == PPC_RELOC_LO16_SECTDIFF
                          || scat->r_type == PPC_RELOC_LO16)
                    {
                        ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HI16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HI16)
                    {
                        ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HA16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HA16)
                    {
                        ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                            + ((word & (1<<15)) ? 1 : 0);
                    }
#endif /* powerpc_HOST_ARCH */
                }
                else
                {
                    barf("Can't handle Mach-O scattered relocation entry "
                         "with this r_length tag: "
                         "object file %s; entry type %ld; "
                         "r_length tag %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         scat->r_type,
                         scat->r_length,
                         scat->r_address);
                    return 0;
                }
            }
            else /* scat->r_pcrel */
            {
                barf("Don't know how to handle *PC-relative* Mach-O "
                     "scattered relocation entry: "
                     "object file %s; entry type %ld; address %#lx\n",
                     OC_INFORMATIVE_FILENAME(oc),
                     scat->r_type,
                     scat->r_address);
               return 0;
            }

        }
        else /* !(relocs[i].r_address & R_SCATTERED) */
        {
            struct relocation_info *reloc = &relocs[i];
            if (reloc->r_pcrel && !reloc->r_extern) {
                IF_DEBUG(linker, debugBelch("relocateSection: pc relative but not external, skipping\n"));
                continue;
            }

            if (reloc->r_length == 2) {
                unsigned long word = 0;
#ifdef powerpc_HOST_ARCH
                unsigned long jumpIsland = 0;
                long offsetToJumpIsland = 0xBADBAD42; // initialise to bad value
                                                      // to avoid warning and to catch
                                                      // bugs.
#endif /* powerpc_HOST_ARCH */

                unsigned long* wordPtr = (unsigned long*) (image + sect->offset + reloc->r_address);

                /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                   on powerpc_HOST_ARCH */
                checkProddableBlock(oc,wordPtr, sizeof(unsigned long));

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    word = *wordPtr;
                }
#ifdef powerpc_HOST_ARCH
                else if (reloc->r_type == PPC_RELOC_LO16) {
                    word = ((unsigned short*) wordPtr)[1];
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                }
                else if (reloc->r_type == PPC_RELOC_HI16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_HA16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_BR24) {
                    word = *wordPtr;
                    word = (word & 0x03FFFFFC) | ((word & 0x02000000) ? 0xFC000000 : 0);
                }
#endif /* powerpc_HOST_ARCH */
                else {
                    barf("Can't handle this Mach-O relocation entry "
                         "(not scattered): "
                         "object file %s; entry type %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         reloc->r_type,
                         reloc->r_address);
                    return 0;
                }

                if (!reloc->r_extern) {
                    long delta = sections[reloc->r_symbolnum-1].offset
                        - sections[reloc->r_symbolnum-1].addr
                        + ((long) image);

                    word += delta;
                }
                else {
                    struct nlist *symbol = &nlist[reloc->r_symbolnum];
                    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
                    void *symbolAddress = lookupSymbol_(nm);

                    if (!symbolAddress) {
                        errorBelch("\nunknown symbol `%s'", nm);
                        return 0;
                    }

                    if (reloc->r_pcrel) {
#ifdef powerpc_HOST_ARCH
                            // In the .o file, this should be a relative jump to NULL
                            // and we'll change it to a relative jump to the symbol
                        ASSERT(word + reloc->r_address == 0);
                        jumpIsland = (unsigned long)
                                        &makeSymbolExtra(oc,
                                                         reloc->r_symbolnum,
                                                         (unsigned long) symbolAddress)
                                         -> jumpIsland;
                        if (jumpIsland != 0) {
                            offsetToJumpIsland = word + jumpIsland
                                - (((long)image) + sect->offset - sect->addr);
                        }
#endif /* powerpc_HOST_ARCH */
                        word += (unsigned long) symbolAddress
                                - (((long)image) + sect->offset - sect->addr);
                    }
                    else {
                        word += (unsigned long) symbolAddress;
                    }
                }

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    *wordPtr = word;
                    continue;
                }
#ifdef powerpc_HOST_ARCH
                else if(reloc->r_type == PPC_RELOC_LO16)
                {
                    ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HI16)
                {
                    ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HA16)
                {
                    ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                        + ((word & (1<<15)) ? 1 : 0);
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_BR24)
                {
                    if ((word & 0x03) != 0) {
                        barf("%s: unconditional relative branch with a displacement "
                             "which isn't a multiple of 4 bytes: %#lx",
                             OC_INFORMATIVE_FILENAME(oc),
                             word);
                    }

                    if((word & 0xFE000000) != 0xFE000000 &&
                        (word & 0xFE000000) != 0x00000000) {
                        // The branch offset is too large.
                        // Therefore, we try to use a jump island.
                        if (jumpIsland == 0) {
                            barf("%s: unconditional relative branch out of range: "
                                 "no jump island available: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                        }

                        word = offsetToJumpIsland;

                        if((word & 0xFE000000) != 0xFE000000 &&
                            (word & 0xFE000000) != 0x00000000) {
                            barf("%s: unconditional relative branch out of range: "
                                 "jump island out of range: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                    }
                    }
                    *wordPtr = (*wordPtr & 0xFC000003) | (word & 0x03FFFFFC);
                    continue;
                }
#endif /* powerpc_HOST_ARCH */
            }
            else
            {
                 barf("Can't handle Mach-O relocation entry (not scattered) "
                      "with this r_length tag: "
                      "object file %s; entry type %ld; "
                      "r_length tag %ld; address %#lx\n",
                      OC_INFORMATIVE_FILENAME(oc),
                      reloc->r_type,
                      reloc->r_length,
                      reloc->r_address);
                 return 0;
            }
        }
#endif /* x86_64_HOST_ARCH */
    }

    IF_DEBUG(linker, debugBelch("relocateSection: done\n"));
    return 1;
}

int
ocGetNames_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i,curSymbol = 0;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct nlist *nlist;
    unsigned long commonSize = 0;
    SymbolAddr* commonStorage = NULL;
    unsigned long commonCounter;

    IF_DEBUG(linker,debugBelch("ocGetNames_MachO: start\n"));

    for(i=0;i<header->ncmds;i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if (!segLC) {
        barf("ocGetNames_MachO: no segment load command");
    }

    Section *secArray;
    secArray = (Section*)stgCallocBytes(
         sizeof(Section),
         segLC->nsects,
         "ocGetNames_MachO(sections)");
   oc->sections = secArray;
   oc->n_sections = segLC->nsects;

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: will load %d sections\n", segLC->nsects));
    for(i=0;i<segLC->nsects;i++)
    {
        IF_DEBUG(linker, debugBelch("ocGetNames_MachO: section %d\n", i));

        if (sections[i].size == 0) {
            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: found a zero length section, skipping\n"));
            continue;
        }

        if((sections[i].flags & SECTION_TYPE) == S_ZEROFILL) {
            char * zeroFillArea;
            if (RTS_LINKER_USE_MMAP) {
                zeroFillArea = mmapForLinker(sections[i].size, MAP_ANONYMOUS,
                                            -1, 0);
                if (zeroFillArea == NULL) return 0;
                memset(zeroFillArea, 0, sections[i].size);
            }
            else {
                zeroFillArea = stgCallocBytes(1,sections[i].size,
                                      "ocGetNames_MachO(common symbols)");
            }
            sections[i].offset = zeroFillArea - image;
        }

        SectionKind kind = SECTIONKIND_OTHER;

        if (0==strcmp(sections[i].sectname,"__text")) {
            kind = SECTIONKIND_CODE_OR_RODATA;
        }
        else if (0==strcmp(sections[i].sectname,"__const") ||
                 0==strcmp(sections[i].sectname,"__data") ||
                 0==strcmp(sections[i].sectname,"__bss") ||
                 0==strcmp(sections[i].sectname,"__common") ||
                 0==strcmp(sections[i].sectname,"__mod_init_func")) {
            kind = SECTIONKIND_RWDATA;
        }

        addSection(&secArray[i], kind, SECTION_NOMEM,
                   (void *)(image + sections[i].offset),
                   sections[i].size,
                   0, 0, 0);

        addProddableBlock(oc,
                          (void *) (image + sections[i].offset),
                                        sections[i].size);
    }

        // count external symbols defined here
    oc->n_symbols = 0;
    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if (nlist[i].n_type & N_STAB) {
                ;
            }
            else if(nlist[i].n_type & N_EXT)
            {
                if((nlist[i].n_type & N_TYPE) == N_UNDF
                    && (nlist[i].n_value != 0))
                {
                    commonSize += nlist[i].n_value;
                    oc->n_symbols++;
                }
                else if((nlist[i].n_type & N_TYPE) == N_SECT)
                    oc->n_symbols++;
            }
        }
    }
    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: %d external symbols\n", oc->n_symbols));
    oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(SymbolName*),
                                   "ocGetNames_MachO(oc->symbols)");

    if(symLC)
    {
        for(i=0;i<symLC->nsyms;i++)
        {
            if(nlist[i].n_type & N_STAB)
                ;
            else if((nlist[i].n_type & N_TYPE) == N_SECT)
            {
                if(nlist[i].n_type & N_EXT)
                {
                    SymbolName* nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                    if ((nlist[i].n_desc & N_WEAK_DEF) && lookupSymbol_(nm)) {
                        // weak definition, and we already have a definition
                        IF_DEBUG(linker, debugBelch("    weak: %s\n", nm));
                    }
                    else
                    {
                            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting %s\n", nm));
                            SymbolAddr* addr = image
                                       + sections[nlist[i].n_sect - 1].offset
                                       - sections[nlist[i].n_sect - 1].addr
                                       + nlist[i].n_value;

                            ghciInsertSymbolTable( oc->fileName
                                                 , symhash
                                                 , nm
                                                 , addr
                                                 , HS_BOOL_FALSE
                                                 , oc);

                            oc->symbols[curSymbol] = nm;
                            curSymbol++;
                    }
                }
                else
                {
                    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not external, skipping\n"));
                }
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not defined in this section, skipping\n"));
            }
        }
    }

    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;

    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if((nlist[i].n_type & N_TYPE) == N_UNDF
             && (nlist[i].n_type & N_EXT)
             && (nlist[i].n_value != 0)) {

                SymbolName* nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                unsigned long sz = nlist[i].n_value;

                nlist[i].n_value = commonCounter;

                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting common symbol: %s\n", nm));
                ghciInsertSymbolTable(oc->fileName, symhash, nm,
                                       (void*)commonCounter, HS_BOOL_FALSE, oc);
                oc->symbols[curSymbol] = nm;
                curSymbol++;

                commonCounter += sz;
            }
        }
    }

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: done\n"));
    return 1;
}

int
ocResolve_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct dysymtab_command *dsymLC = NULL;
    struct nlist *nlist;

    IF_DEBUG(linker, debugBelch("ocResolve_MachO: start\n"));
    for (i = 0; i < header->ncmds; i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a 32 or 64 bit segment load command\n"));
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a symbol table load command\n"));
        }
        else if (lc->cmd == LC_DYSYMTAB) {
            dsymLC = (struct dysymtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a dynamic symbol table load command\n"));
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if(dsymLC)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (image + dsymLC->indirectsymoff);

        IF_DEBUG(linker, debugBelch("ocResolve_MachO: resolving dsymLC\n"));
        for (i = 0; i < segLC->nsects; i++)
        {
            if(    !strcmp(sections[i].sectname,"__la_symbol_ptr")
                || !strcmp(sections[i].sectname,"__la_sym_ptr2")
                || !strcmp(sections[i].sectname,"__la_sym_ptr3"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__nl_symbol_ptr")
                ||  !strcmp(sections[i].sectname,"__pointers"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__jump_table"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocResolve_MachO: unknown section\n"));
            }
        }
    }

    for(i=0;i<segLC->nsects;i++)
    {
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: relocating section %d\n", i));

        if (!relocateSection(oc,image,symLC,nlist,segLC->nsects,sections,&sections[i]))
            return 0;
    }

#if defined (powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

    return 1;
}

int
ocRunInit_MachO ( ObjectCode *oc )
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    struct segment_command *segLC = NULL;
    struct section *sections;
    uint32_t i;

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }
    if (!segLC) {
        barf("ocRunInit_MachO: no segment load command");
    }
    sections = (struct section*) (segLC+1);

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (i = 0; i < segLC->nsects; i++) {
        // ToDo: replace this with a proper check for the S_MOD_INIT_FUNC_POINTERS
        // flag.  We should do this elsewhere in the Mach-O linker code
        // too.  Note that the system linker will *refuse* to honor
        // sections which don't have this flag, so this could cause
        // weird behavior divergence (albeit reproducible).
        if (0 == strcmp(sections[i].sectname,"__mod_init_func")) {
            char *init_startC = image + sections[i].offset;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)(init_startC + sections[i].size);
            for (; init < init_end; init++) {
                (*init)(argc, argv, envv);
            }
        }
    }

    freeProgEnvv(envc, envv);
    return 1;
}

#ifdef powerpc_HOST_ARCH
/*
 * The Mach-O object format uses leading underscores. But not everywhere.
 * There is a small number of runtime support functions defined in
 * libcc_dynamic.a whose name does not have a leading underscore.
 * As a consequence, we can't get their address from C code.
 * We have to use inline assembler just to take the address of a function.
 * Yuck.
 */

extern void* symbolsWithoutUnderscore[];

void
machoInitSymbolsWithoutUnderscore(void)
{
    void **p = symbolsWithoutUnderscore;
    __asm__ volatile(".globl _symbolsWithoutUnderscore\n.data\n_symbolsWithoutUnderscore:");

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto

#define SymI_NeedsProto(x)  \
    __asm__ volatile(".long " # x);

#define SymI_NeedsDataProto(x) \
    SymI_NeedsProto(x)

    RTS_MACHO_NOUNDERLINE_SYMBOLS

    __asm__ volatile(".text");

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto

#define SymI_NeedsProto(x)  \
    ghciInsertSymbolTable("(GHCi built-in symbols)", symhash, #x, *p++, HS_BOOL_FALSE, NULL);

#define SymI_NeedsDataProto(x) \
    SymI_NeedsProto(x)

    RTS_MACHO_NOUNDERLINE_SYMBOLS

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto
}
#endif

/*
 * Figure out by how much to shift the entire Mach-O file in memory
 * when loading so that its single segment ends up 16-byte-aligned
 */
int
machoGetMisalignment( FILE * f )
{
    struct mach_header header;
    int misalignment;

    {
        int n = fread(&header, sizeof(header), 1, f);
        if (n != 1) {
            barf("machoGetMisalignment: can't read the Mach-O header");
        }
    }
    fseek(f, -sizeof(header), SEEK_CUR);

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header.magic != MH_MAGIC_64) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC_64, header.magic);
    }
#else
    if(header.magic != MH_MAGIC) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC, header.magic);
    }
#endif

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    return misalignment ? (16 - misalignment) : 0;
}

#endif /* darwin_HOST_OS */
