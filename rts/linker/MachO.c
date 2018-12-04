#include "Rts.h"

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)

#if defined(ios_HOST_OS)
#if !RTS_LINKER_USE_MMAP
#error "ios must use mmap and mprotect!"
#endif
/* for roundUpToPage */
#include "sm/OSMem.h"
#endif

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

#if defined(HAVE_SYS_MMAN_H) && RTS_LINKER_USE_MMAP
#  include <sys/mman.h>
#endif

#if defined(powerpc_HOST_ARCH)
#  include <mach-o/ppc/reloc.h>
#endif

#if defined(x86_64_HOST_ARCH)
#  include <mach-o/x86_64/reloc.h>
#endif

#if defined(aarch64_HOST_ARCH)
#  include <mach-o/arm64/reloc.h>
#endif

/*
  Support for MachO linking on Darwin/MacOS X
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)

  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) implement ocVerifyImage_MachO
  *) add still more sanity checks.
*/
#if defined(aarch64_HOST_ARCH)
/* aarch64 linker by moritz angermann <moritz@lichtzwerge.de> */

/* often times we need to extend some value of certain number of bits
 * int an int64_t for e.g. relative offsets.
 */
int64_t signExtend(uint64_t val, uint8_t bits);
/* Helper functions to check some instruction properties */
bool isVectorPp(uint32_t *p);
bool isLoadStore(uint32_t *p);

/* aarch64 relocations may contain an addend alreay in the position
 * where we want to write the address offset to. Thus decoding as well
 * as encoding is needed.
 */
bool fitsBits(size_t bits, int64_t value);
int64_t decodeAddend(ObjectCode * oc, Section * section,
                     MachORelocationInfo * ri);
void encodeAddend(ObjectCode * oc, Section * section,
                  MachORelocationInfo * ri, int64_t addend);

/* finding and making stubs. We don't need to care about the symbol they
 * represent. As long as two stubs point to the same address, they are identical
 */
bool findStub(Section * section, void ** addr);
bool makeStub(Section * section, void ** addr);
void freeStubs(Section * section);

/* Global Offset Table logic */
bool isGotLoad(MachORelocationInfo * ri);
bool needGotSlot(MachONList * symbol);
bool makeGot(ObjectCode * oc);
void freeGot(ObjectCode * oc);
#endif /* aarch64_HOST_ARCH */

#if defined(ios_HOST_OS)
/* on iOS we need to ensure we only have r+w or r+x pages hence we need to mmap
 * pages r+w and r+x mprotect them later on.
 */
bool ocMprotect_MachO( ObjectCode *oc );
#endif /* ios_HOST_OS */

/*
 * Initialize some common data in the object code so we don't have to
 * continuously look up the addresses.
 */
void
ocInit_MachO(ObjectCode * oc)
{
    oc->info = (struct ObjectCodeFormatInfo*)stgCallocBytes(
                1, sizeof *oc->info,
                "ocInit_MachO(ObjectCodeFormatInfo)");
    oc->info->header  = (MachOHeader *) oc->image;
    oc->info->symCmd  = NULL;
    oc->info->segCmd  = NULL;
    oc->info->dsymCmd = NULL;

    MachOLoadCommand *lc = (MachOLoadCommand*)(oc->image + sizeof(MachOHeader));
    for(size_t i = 0; i < oc->info->header->ncmds; i++) {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            oc->info->segCmd = (MachOSegmentCommand*) lc;
        }
        else if (lc->cmd == LC_SYMTAB) {
            oc->info->symCmd = (MachOSymtabCommand*) lc;
        }
        else if (lc->cmd == LC_DYSYMTAB) {
            oc->info->dsymCmd = (MachODsymtabCommand*) lc;
        }
        lc = (MachOLoadCommand *) ( ((char*)lc) + lc->cmdsize );
    }
    if (NULL == oc->info->segCmd) {
        barf("ocGetNames_MachO: no segment load command");
    }

    oc->info->macho_sections = (MachOSection*) (oc->info->segCmd+1);
    oc->n_sections = oc->info->segCmd->nsects;

    oc->info->nlist = oc->info->symCmd == NULL
              ? NULL
              : (MachONList *)(oc->image + oc->info->symCmd->symoff);
    oc->info->names = oc->info->symCmd == NULL
              ? NULL
              : (oc->image + oc->info->symCmd->stroff);

    /* If we have symbols, allocate and fill the macho_symbols
     * This will make relocation easier.
     */
    oc->info->n_macho_symbols = 0;
    oc->info->macho_symbols = NULL;

    if(NULL != oc->info->nlist) {
        oc->info->n_macho_symbols = oc->info->symCmd->nsyms;
        oc->info->macho_symbols = (MachOSymbol*)stgCallocBytes(
                                    oc->info->symCmd->nsyms,
                                    sizeof(MachOSymbol),
                                    "ocInit_MachO(MachOSymbol)");
        for(uint32_t i = 0; i < oc->info->symCmd->nsyms; i++) {
            oc->info->macho_symbols[i].name  = oc->info->names
                                             + oc->info->nlist[i].n_un.n_strx;
            oc->info->macho_symbols[i].nlist = &oc->info->nlist[i];
             /* we don't have an address for this symbol yet; this will be
              * populated during ocGetNames_MachO. hence addr = NULL
              */
            oc->info->macho_symbols[i].addr  = NULL;
        }
    }
}

void
ocDeinit_MachO(ObjectCode * oc) {
    if(oc->info->n_macho_symbols > 0) {
        stgFree(oc->info->macho_symbols);
    }
#if defined(aarch64_HOST_ARCH)
    freeGot(oc);
    for(int i = 0; i < oc->n_sections; i++) {
        freeStubs(&oc->sections[i]);
    }
#endif
    stgFree(oc->info);
}

static int
resolveImports(
    ObjectCode* oc,
    MachOSection *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms);

#if NEED_SYMBOL_EXTRAS
#if defined(powerpc_HOST_ARCH)
int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    // Find out the first and last undefined external
    // symbol, so we don't have to allocate too many
    // jump islands/GOT entries.

    unsigned min = oc->info->symCmd->nsyms, max = 0;

    for (unsigned i = 0; i < oc->info->symCmd->nsyms; i++) {

        if (oc->info->nlist[i].n_type & N_STAB) {
            ;
        } else if (oc->info->nlist[i].n_type & N_EXT) {

            if((oc->info->nlist[i].n_type & N_TYPE) == N_UNDF
                && (oc->info->nlist[i].n_value == 0)) {

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

    return ocAllocateSymbolExtras(oc,0,0);
}

#elif defined(x86_64_HOST_ARCH) || defined(aarch64_HOST_ARCH)

int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    if (NULL != oc->info->symCmd) {
        IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocate %d symbols\n", oc->info->symCmd->nsyms));
        IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
        return ocAllocateSymbolExtras(oc, oc->info->symCmd->nsyms, 0);
    }

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocated no symbols\n"));
    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
    return ocAllocateSymbolExtras(oc,0,0);
}

#else
#error Unknown MachO architecture
#endif /* HOST_ARCH */
#endif /* NEED_SYMBOL_EXTRAS */

int
ocVerifyImage_MachO(ObjectCode * oc)
{
    char *image = (char*) oc->image;
    MachOHeader *header = (MachOHeader*) image;

    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: start\n"));

#if defined(x86_64_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(aarch64_HOST_ARCH)
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
    MachOSection *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms)
{
    size_t itemSize = 4;

    IF_DEBUG(linker, debugBelch("resolveImports: start\n"));

#if defined(i386_HOST_ARCH)
    int isJumpTable = 0;

    if (strcmp(sect->sectname,"__jump_table") == 0) {
        isJumpTable = 1;
        itemSize = 5;
        ASSERT(sect->reserved2 == itemSize);
    }

#endif

    for(unsigned i = 0; i * itemSize < sect->size; i++)
    {
        // according to otool, reserved1 contains the first index into the
        // indirect symbol table
        unsigned long indirectSymbolIndex = indirectSyms[sect->reserved1+i];
        MachOSymbol *symbol = &oc->info->macho_symbols[indirectSymbolIndex];
        SymbolAddr* addr = NULL;

        IF_DEBUG(linker, debugBelch("resolveImports: resolving %s\n", symbol->name));

        if ((symbol->nlist->n_type & N_TYPE) == N_UNDF
            && (symbol->nlist->n_type & N_EXT) && (symbol->nlist->n_value != 0)) {
            addr = (SymbolAddr*) (symbol->nlist->n_value);
            IF_DEBUG(linker, debugBelch("resolveImports: undefined external %s has value %p\n", symbol->name, addr));
        } else {
            addr = lookupSymbol_(symbol->name);
            IF_DEBUG(linker, debugBelch("resolveImports: looking up %s, %p\n", symbol->name, addr));
        }

        if (addr == NULL)
        {
            errorBelch("\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, symbol->name);
            return 0;
        }
        ASSERT(addr);

#if defined(i386_HOST_ARCH)
        if (isJumpTable) {
            checkProddableBlock(oc,oc->image + sect->offset + i*itemSize, 5);

            *(oc->image + sect->offset + i * itemSize) = 0xe9; // jmp opcode
            *(unsigned*)(oc->image + sect->offset + i*itemSize + 1)
                = (SymbolAddr*)addr - (oc->image + sect->offset + i*itemSize + 5);
        }
        else
#endif
        {
            checkProddableBlock(oc,
                                ((void**)(oc->image + sect->offset)) + i,
                                sizeof(void *));
            ((void**)(oc->image + sect->offset))[i] = addr;
        }
    }

    IF_DEBUG(linker, debugBelch("resolveImports: done\n"));
    return 1;
}

#if defined(aarch64_HOST_ARCH)
/* aarch64 linker by moritz angermann <moritz@lichtzwerge.de> */

int64_t
signExtend(uint64_t val, uint8_t bits) {
    return (int64_t)(val << (64-bits)) >> (64-bits);
}

bool
isVectorOp(uint32_t *p) {
    return (*p & 0x04800000) == 0x04800000;
}

bool
isLoadStore(uint32_t *p) {
    return (*p & 0x3B000000) == 0x39000000;
}

int64_t
decodeAddend(ObjectCode * oc, Section * section, MachORelocationInfo * ri) {

    /* the instruction. It is 32bit wide */
    uint32_t * p = (uint32_t*)((uint8_t*)section->start + ri->r_address);

    checkProddableBlock(oc, (void*)p, 1 << ri->r_length);

    switch(ri->r_type) {
        case ARM64_RELOC_UNSIGNED:
        case ARM64_RELOC_SUBTRACTOR: {
            switch (ri->r_length) {
                case 0: return signExtend(*(uint8_t*)p,  8 * (1 << ri->r_length));
                case 1: return signExtend(*(uint16_t*)p, 8 * (1 << ri->r_length));
                case 2: return signExtend(*(uint32_t*)p, 8 * (1 << ri->r_length));
                case 3: return signExtend(*(uint64_t*)p, 8 * (1 << ri->r_length));
                default:
                    barf("Unsupported r_length (%d) for SUBTRACTOR relocation",
                         ri->r_length);
            }
        }
        case ARM64_RELOC_BRANCH26:
            /* take the lower 26 bits and shift them by 2. The last two are
             * implicilty 0 (as the instructions must be aligned!) and sign
             * extend to 64 bits.
             */
            return signExtend( (*p & 0x03FFFFFF) << 2, 28 );
        case ARM64_RELOC_PAGE21:
        case ARM64_RELOC_GOT_LOAD_PAGE21:
            /* take the instruction bits masked with 0x6 (0110), and push them
             * down. into the last two bits, and mask in the
             *
             * the 21 bits are encoded as follows in the instruction
             *
             * -**- ---* **** **** **** **** ***-- ----
             *  ^^
             *  ''-- these are the low two bits.
             */
            return signExtend(   (*p & 0x60000000) >> 29
                               | ((*p & 0x01FFFFE0) >> 3) << 12, 33);
        case ARM64_RELOC_PAGEOFF12:
        case ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
            /* the 12 bits for the page offset are encoded from bit 11 onwards
             *
             * ---- ---- --** **** **** **-- ---- ----
             */
            int64_t a = (*p & 0x003FFC00) >> 10;
            int shift = 0;
            if (isLoadStore(p)) {
                shift = (*p >> 30) & 0x3;
                if(0 == shift && isVectorOp(p)) {
                    shift = 4;
                }
            }
            return a << shift;
        }
    }
    barf("unsupported relocation type: %d\n", ri->r_type);
}

inline bool
fitsBits(size_t bits, int64_t value) {
    if(bits == 64) return true;
    if(bits > 64) barf("fits_bits with %d bits and an 64bit integer!", bits);
    return  0 == (value >> bits)   // All bits off: 0
        || -1 == (value >> bits);  // All bits on: -1
}

void
encodeAddend(ObjectCode * oc, Section * section,
             MachORelocationInfo * ri, int64_t addend) {
    uint32_t * p = (uint32_t*)((uint8_t*)section->start + ri->r_address);

    checkProddableBlock(oc, (void*)p, 1 << ri->r_length);

    switch (ri->r_type) {
        case ARM64_RELOC_UNSIGNED:
        case ARM64_RELOC_SUBTRACTOR: {
            if(!fitsBits(8 << ri->r_length, addend))
                barf("Relocation out of range for UNSIGNED/SUBTRACTOR");
            switch (ri->r_length) {
                case 0: *(uint8_t*)p  = (uint8_t)addend; break;
                case 1: *(uint16_t*)p = (uint16_t)addend; break;
                case 2: *(uint32_t*)p = (uint32_t)addend; break;
                case 3: *(uint64_t*)p = (uint64_t)addend; break;
                default:
                    barf("Unsupported r_length (%d) for SUBTRACTOR relocation",
                         ri->r_length);
            }
            return;
        }
        case ARM64_RELOC_BRANCH26: {
            /* We can only store 26 bits in the instruction, due to alignment we
             * do not need the last two bits of the value. If the value >> 2
             * still exceeds 26bits, we won't be able to reach it.
             */
            if(!fitsBits(26, addend >> 2))
                barf("Relocation target for BRACH26 out of range.");
            *p = (*p & 0xFC000000) | ((uint32_t)(addend >> 2) & 0x03FFFFFF);
            return;
        }
        case ARM64_RELOC_PAGE21:
        case ARM64_RELOC_GOT_LOAD_PAGE21: {
            /* We store 21bits, in bits 6 to 24, and bits 30 and 31.
             * The encoded value describes a multiple of 4k pages, and together
             * with the PAGEOFF12 relocation allows to address a relative range
             * of +-4GB.
             */
            if(!fitsBits(21, addend >> 12))
                barf("Relocation target for PAGE21 out of range.");
            *p = (*p & 0x9F00001F) | (uint32_t)((addend << 17) & 0x60000000)
                                   | (uint32_t)((addend >> 9)  & 0x00FFFFE0);
            return;
        }
        case ARM64_RELOC_PAGEOFF12:
        case ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
            /* Store an offset into a page (4k). Depending on the instruction
             * the bits are stored at slightly different positions.
             */
            if(!fitsBits(12, addend))
                barf("Relocation target for PAGEOFF12 out or range.");

            int shift = 0;
            if(isLoadStore(p)) {
                shift = (*p >> 30) & 0x3;
                if(0 == shift && isVectorOp(p)) {
                    shift = 4;
                }
            }
            *p = (*p & 0xFFC003FF)
               | ((uint32_t)(addend >> shift << 10) & 0x003FFC00);
            return;
        }
    }
    barf("unsupported relocation type: %d\n", ri->r_type);
}

bool
isGotLoad(struct relocation_info * ri) {
    return ri->r_type == ARM64_RELOC_GOT_LOAD_PAGE21
    ||  ri->r_type == ARM64_RELOC_GOT_LOAD_PAGEOFF12;
}

/* This is very similar to makeSymbolExtra
 * However, as we load sections into different
 * pages, that may be further appart than
 * branching allows, we'll use some extra
 * space at the end of each section allocated
 * for stubs.
 */
bool
findStub(Section * section, void ** addr) {

    for(Stub * s = section->info->stubs; s != NULL; s = s->next) {
        if(s->target == *addr) {
            *addr = s->addr;
            return EXIT_SUCCESS;
        }
    }
    return EXIT_FAILURE;
}

bool
makeStub(Section * section, void ** addr) {

    Stub * s = stgCallocBytes(1, sizeof(Stub), "makeStub(Stub)");
    s->target = *addr;
    s->addr = (uint8_t*)section->info->stub_offset
            + ((8+8)*section->info->nstubs) + 8;
    s->next = NULL;

     /* target address */
    *(uint64_t*)((uint8_t*)s->addr - 8) = (uint64_t)s->target;
    /* ldr x16, - (8 bytes) */
    *(uint32_t*)(s->addr)               = (uint32_t)0x58ffffd0;
    /* br x16 */
    *(uint32_t*)((uint8_t*)s->addr + 4) = (uint32_t)0xd61f0200;

    if(section->info->nstubs == 0) {
        /* no stubs yet, let's just create this one */
        section->info->stubs = s;
    } else {
        Stub * tail = section->info->stubs;
        while(tail->next != NULL) tail = tail->next;
        tail->next = s;
    }
    section->info->nstubs += 1;
    *addr = s->addr;
    return EXIT_SUCCESS;
}
void
freeStubs(Section * section) {
    if(section->info->nstubs == 0)
        return;
    Stub * last = section->info->stubs;
    while(last->next != NULL) {
        Stub * t = last;
        last = last->next;
        stgFree(t);
    }
    section->info->stubs = NULL;
    section->info->nstubs = 0;
}

/*
 * Check if we need a global offset table slot for a
 * given symbol
 */
bool
needGotSlot(MachONList * symbol) {
    return (symbol->n_type & N_EXT)             /* is an external symbol      */
        && (N_UNDF == (symbol->n_type & N_TYPE) /* and is undefined           */
            || NO_SECT != symbol->n_sect);      /*     or is defined in a
                                                 *        different section   */
}

bool
makeGot(ObjectCode * oc) {
    size_t got_slots = 0;

    for(size_t i=0; i < oc->info->n_macho_symbols; i++)
        if(needGotSlot(oc->info->macho_symbols[i].nlist))
            got_slots += 1;

    if(got_slots > 0) {
        oc->info->got_size =  got_slots * sizeof(void*);
        oc->info->got_start = mmap(NULL, oc->info->got_size,
                                   PROT_READ | PROT_WRITE,
                                   MAP_ANON | MAP_PRIVATE,
                                   -1, 0);
        if( oc->info->got_start == MAP_FAILED ) {
            barf("MAP_FAILED. errno=%d", errno );
            return EXIT_FAILURE;
        }
        /* update got_addr */
        size_t slot = 0;
        for(size_t i=0; i < oc->info->n_macho_symbols; i++)
            if(needGotSlot(oc->info->macho_symbols[i].nlist))
                oc->info->macho_symbols[i].got_addr
                    = ((uint8_t*)oc->info->got_start)
                    + (slot++ * sizeof(void *));
    }
    return EXIT_SUCCESS;
}

void
freeGot(ObjectCode * oc) {
    munmap(oc->info->got_start, oc->info->got_size);
    oc->info->got_start = NULL;
    oc->info->got_size = 0;
}

static int
relocateSectionAarch64(ObjectCode * oc, Section * section)
{
    if(section->size == 0)
        return 1;
    /* at this point, we have:
     *
     * - loaded the sections (potentially into non-contiguous memory),
     *   (in ocGetNames_MachO)
     * - registered exported sybmols
     *   (in ocGetNames_MachO)
     * - and fixed the nlist[i].n_value for common storage symbols (N_UNDF,
     *   N_EXT and n_value != 0) so that they point into the common storage.
     *   (in ocGetNames_MachO)
     * - All oc->symbols however should now point at the right place.
     */

    /* we need to care about the explicit addend */
    int64_t explicit_addend = 0;
    size_t  nreloc = section->info->macho_section->nreloc;

    for(size_t i = 0; i < nreloc; i++) {
        MachORelocationInfo * ri = &section->info->relocation_info[i];
        switch (ri->r_type) {
            case ARM64_RELOC_UNSIGNED: {
                MachOSymbol* symbol = &oc->info->macho_symbols[ri->r_symbolnum];
                int64_t addend = decodeAddend(oc, section, ri);
                uint64_t value = 0;
                if(symbol->nlist->n_type & N_EXT) {
                    /* external symbols should be able to be
                     * looked up via the lookupSymbol_ function.
                     * Either through the global symbol hashmap
                     * or asking the system, if not found
                     * in the symbol hashmap
                     */
                    value = (uint64_t)lookupSymbol_((char*)symbol->name);
                    if(!value)
                        barf("Could not lookup symbol: %s!", symbol->name);
                } else {
                    value = (uint64_t)symbol->addr;    // address of the symbol.
                }
                encodeAddend(oc, section, ri, value + addend);
                break;
            }
            case ARM64_RELOC_SUBTRACTOR:
            {
                MachOSymbol* symbol = &oc->info->macho_symbols[ri->r_symbolnum];
                // subtractor and unsigned are called in tandem:
                // first  pc <- pc - symbol address (SUBTRACTOR)
                // second pc <- pc + symbol address (UNSIGNED)
                // to achieve pc <- pc + target - base.
                //
                // the current implementation uses absolute addresses,
                // which is simpler than trying to do this section
                // relative, but could more easily lead to overflow.
                //
                if(!(i+1 < nreloc)
                   || !(section->info->relocation_info[i+1].r_type
                          == ARM64_RELOC_UNSIGNED))
                    barf("SUBTRACTOR relocation *must* be followed by UNSIGNED relocation.");

                int64_t addend = decodeAddend(oc, section, ri);
                int64_t value = (uint64_t)symbol->addr;
                encodeAddend(oc, section, ri, addend - value);
                break;
            }
            case ARM64_RELOC_BRANCH26: {
                MachOSymbol* symbol = &oc->info->macho_symbols[ri->r_symbolnum];

                // pre-existing addend
                int64_t addend = decodeAddend(oc, section, ri);
                // address of the branch (b/bl) instruction.
                uint64_t pc = (uint64_t)section->start + ri->r_address;
                uint64_t value = 0;
                if(symbol->nlist->n_type & N_EXT) {
                    value = (uint64_t)lookupSymbol_((char*)symbol->name);
                    if(!value)
                        barf("Could not lookup symbol: %s!", symbol->name);
                } else {
                    value = (uint64_t)symbol->addr;    // address of the symbol.
                }
                if((value - pc + addend) >> (2 + 26)) {
                    /* we need a stub */
                    /* check if we already have that stub */
                    if(findStub(section, (void**)&value)) {
                        /* did not find it. Crete a new stub. */
                        if(makeStub(section, (void**)&value)) {
                            barf("could not find or make stub");
                        }
                    }
                }
                encodeAddend(oc, section, ri, value - pc + addend);
                break;
            }
            case ARM64_RELOC_PAGE21:
            case ARM64_RELOC_GOT_LOAD_PAGE21: {
                MachOSymbol* symbol = &oc->info->macho_symbols[ri->r_symbolnum];
                int64_t addend = decodeAddend(oc, section, ri);
                if(!(explicit_addend == 0 || addend == 0))
                    barf("explicit_addend and addend can't be set at the same time.");
                uint64_t pc = (uint64_t)section->start + ri->r_address;
                uint64_t value = (uint64_t)(isGotLoad(ri) ? symbol->got_addr : symbol->addr);
                encodeAddend(oc, section, ri, ((value + addend + explicit_addend) & (-4096)) - (pc & (-4096)));

                // reset, just in case.
                explicit_addend = 0;
                break;
            }
            case ARM64_RELOC_PAGEOFF12:
            case ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
                MachOSymbol* symbol = &oc->info->macho_symbols[ri->r_symbolnum];
                int64_t addend = decodeAddend(oc, section, ri);
                if(!(explicit_addend == 0 || addend == 0))
                    barf("explicit_addend and addend can't be set at the same time.");
                uint64_t value = (uint64_t)(isGotLoad(ri) ? symbol->got_addr : symbol->addr);
                encodeAddend(oc, section, ri, 0xFFF & (value + addend + explicit_addend));

                // reset, just in case.
                explicit_addend = 0;
                break;
            }
            case ARM64_RELOC_ADDEND: {
                explicit_addend = signExtend(ri->r_symbolnum, 24);
                if(!(i+1 < nreloc)
                   || !(section->info->relocation_info[i+1].r_type == ARM64_RELOC_PAGE21
                        || section->info->relocation_info[i+1].r_type == ARM64_RELOC_PAGEOFF12))
                    barf("ADDEND relocation *must* be followed by PAGE or PAGEOFF relocation");
                break;
            }
            default: {
                barf("Relocation of type: %d not (yet) supported!\n", ri->r_type);
            }
        }
    }
    return 1;
}

#else /* non aarch64_HOST_ARCH branch -- aarch64 doesn't use relocateAddress */

/*
 * Try to find the final loaded address for some addres.
 * Look through all sections, locating the section that
 * contains the address and compute the absolue address.
 */
static unsigned long
relocateAddress(
                ObjectCode* oc,
                int nSections,
                MachOSection* sections,
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

#endif /* aarch64_HOST_ARCH */

#if !defined(aarch64_HOST_ARCH)
static int
relocateSection(
    ObjectCode* oc,
    char *image,
    MachOSymtabCommand *symLC, MachONList *nlist,
    int nSections, MachOSection* sections, MachOSection *sect)
{
    MachORelocationInfo *relocs;
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

    relocs = (MachORelocationInfo*) (image + sect->reloff);

    for(i = 0; i < n; i++)
    {
#if defined(x86_64_HOST_ARCH)
        MachORelocationInfo *reloc = &relocs[i];

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
            MachONList *symbol = &nlist[reloc->r_symbolnum];
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
            MachONList *symbol = &nlist[reloc->r_symbolnum];
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
                barf("unknown relocation");
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
            MachOScatteredRelocationInfo *scat =
                (MachOScatteredRelocationInfo*) &relocs[i];

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
#if defined(powerpc_HOST_ARCH)
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
                        MachOScatteredRelocationInfo *pair =
                                (MachOScatteredRelocationInfo*) &relocs[i+1];

                        if (!pair->r_scattered || pair->r_type != GENERIC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "RELOC_*_SECTDIFF not followed by RELOC_PAIR");
                        }

                        word = (unsigned long)
                               (relocateAddress(oc, nSections, sections, scat->r_value)
                              - relocateAddress(oc, nSections, sections, pair->r_value));
                        i++;
                    }
#if defined(powerpc_HOST_ARCH)
                    else if(scat->r_type == PPC_RELOC_HI16
                         || scat->r_type == PPC_RELOC_LO16
                         || scat->r_type == PPC_RELOC_HA16
                         || scat->r_type == PPC_RELOC_LO14)
                    {   // these are generated by label+offset things
                        MachORelocationInfo *pair = &relocs[i+1];

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

#if defined(powerpc_HOST_ARCH)
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
#if defined(powerpc_HOST_ARCH)
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
            MachORelocationInfo *reloc = &relocs[i];
            if (reloc->r_pcrel && !reloc->r_extern) {
                IF_DEBUG(linker, debugBelch("relocateSection: pc relative but not external, skipping\n"));
                continue;
            }

            if (reloc->r_length == 2) {
                unsigned long word = 0;
#if defined(powerpc_HOST_ARCH)
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
#if defined(powerpc_HOST_ARCH)
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
                    MachONList *symbol = &nlist[reloc->r_symbolnum];
                    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
                    void *symbolAddress = lookupSymbol_(nm);

                    if (!symbolAddress) {
                        errorBelch("\nunknown symbol `%s'", nm);
                        return 0;
                    }

                    if (reloc->r_pcrel) {
#if defined(powerpc_HOST_ARCH)
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
#if defined(powerpc_HOST_ARCH)
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
#endif /* aarch64_HOST_ARCH */

/* Note [mmap r+w+x]
 * ~~~~~~~~~~~~~~~~~
 *
 * iOS does not permit to mmap r+w+x, hence wo only mmap r+w, and later change
 * to r+x via mprotect.  While this could would be nice to have for all hosts
 * and not just for iOS, it entail that the rest of the linker code supports
 * that, this includes:
 *
 * - mmap and mprotect need to be available.
 * - text and data sections need to be mapped into different pages. Ideally
 *   the text and data sections would be aggregated, to prevent using a single
 *   page for every section, however tiny.
 * - the relocation code for each object file format / architecture, needs to
 *   respect the (now) non-contiguousness of the sections.
 * - with sections being mapped potentially far apart from each other, it must
 *   be made sure that the pages are reachable within the architectures
 *   addressability for relative or absolute access.
 */

int
ocGetNames_MachO(ObjectCode* oc)
{
    unsigned curSymbol = 0;

    unsigned long commonSize = 0;
    SymbolAddr* commonStorage = NULL;
    unsigned long commonCounter;

    IF_DEBUG(linker,debugBelch("ocGetNames_MachO: start\n"));

    Section *secArray;
    secArray = (Section*)stgCallocBytes(
         sizeof(Section),
         oc->info->segCmd->nsects,
         "ocGetNames_MachO(sections)");

    oc->sections = secArray;

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: will load %d sections\n",
                                oc->n_sections));
    for(int i=0; i < oc->n_sections; i++)
    {
        MachOSection * section = &oc->info->macho_sections[i];

        IF_DEBUG(linker, debugBelch("ocGetNames_MachO: section %d\n", i));

        if (section->size == 0) {
            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: found a zero length section, skipping\n"));
            continue;
        }

        // XXX, use SECTION_TYPE attributes, instead of relying on the name?

        SectionKind kind = SECTIONKIND_OTHER;

        if (0==strcmp(section->sectname,"__text")) {
            kind = SECTIONKIND_CODE_OR_RODATA;
        }
        else if (0==strcmp(section->sectname,"__const") ||
                 0==strcmp(section->sectname,"__data") ||
                 0==strcmp(section->sectname,"__bss") ||
                 0==strcmp(section->sectname,"__common") ||
                 0==strcmp(section->sectname,"__mod_init_func")) {
            kind = SECTIONKIND_RWDATA;
        }

        switch(section->flags & SECTION_TYPE) {
#if defined(ios_HOST_OS)
            case S_ZEROFILL:
            case S_GB_ZEROFILL: {
                // See Note [mmap r+w+x]
                void * mem = mmap(NULL, section->size,
                                  PROT_READ | PROT_WRITE,
                                  MAP_ANON | MAP_PRIVATE,
                                  -1, 0);
                if( mem == MAP_FAILED ) {
                    barf("failed to mmap allocate memory for zerofill section %d of size %d. errno = %d", i, section->size, errno);
                }
                addSection(&secArray[i], kind, SECTION_MMAP, mem, section->size,
                           0, mem, roundUpToPage(section->size));
                addProddableBlock(oc, mem, (int)section->size);

                secArray[i].info->nstubs = 0;
                secArray[i].info->stub_offset = NULL;
                secArray[i].info->stub_size = 0;
                secArray[i].info->stubs = NULL;

                secArray[i].info->macho_section = section;
                secArray[i].info->relocation_info
                  = (MachORelocationInfo*)(oc->image + section->reloff);
                break;
            }
            default: {
                // The secion should have a non-zero offset. As the offset is
                // relativ to the image, and must be somewhere after the header.
                if(section->offset == 0) barf("section with zero offset!");
                /* on iOS, we must allocate the code in r+x sections and
                 * the data in r+w sections, as the system does not allow
                 * for r+w+x, we must allocate each section in a new page
                 * range.
                 *
                 * copy the sections's memory to some page-aligned place via
                 * mmap and memcpy. This will later allow us to selectively
                 * use mprotect on pages with data (r+w) and pages text (r+x).
                 * We initially start with r+w, so that we can modify the
                 * pages during relocations, prior to setting it r+x.
                 */

                /* We also need space for stubs. As pages can be assigned
                 * randomly in the addressable space, we need to keep the
                 * stubs close to the section.  The strategy we are going
                 * to use is to allocate them right after the section. And
                 * we are going to be generous and allocare a stub slot
                 * for each relocation to keep it simple.
                 */
                size_t n_ext_sec_sym = section->nreloc; /* number of relocations
                                                         * for this section. Should
                                                         * be a good upper bound
                                                         */
                size_t stub_space = /* eight bytes for the 64 bit address,
                                     * and another eight bytes for the two
                                     * instructions (ldr, br) for each relocation.
                                     */ 16 * n_ext_sec_sym;
                // See Note [mmap r+w+x]
                void * mem = mmap(NULL, section->size+stub_space,
                                  PROT_READ | PROT_WRITE,
                                  MAP_ANON | MAP_PRIVATE,
                                  -1, 0);
                if( mem == MAP_FAILED ) {
                    barf("failed to mmap allocate memory to load section %d. errno = %d", i, errno );
                }
                memcpy( mem, oc->image + section->offset, section->size);

                addSection(&secArray[i], kind, SECTION_MMAP,
                           mem, section->size,
                           0, mem, roundUpToPage(section->size+stub_space));
                addProddableBlock(oc, mem, (int)section->size);

                secArray[i].info->nstubs = 0;
                secArray[i].info->stub_offset = ((uint8_t*)mem) + section->size;
                secArray[i].info->stub_size = stub_space;
                secArray[i].info->stubs = NULL;

                secArray[i].info->macho_section = section;
                secArray[i].info->relocation_info
                  = (MachORelocationInfo*)(oc->image + section->reloff);
                break;
            }

#else /* any other host */
            case S_ZEROFILL:
            case S_GB_ZEROFILL: {
                char * zeroFillArea;
                if (RTS_LINKER_USE_MMAP) {
                    zeroFillArea = mmapForLinker(section->size, MAP_ANONYMOUS,
                                                 -1, 0);
                    if (zeroFillArea == NULL) return 0;
                    memset(zeroFillArea, 0, section->size);
                }
                else {
                    zeroFillArea = stgCallocBytes(1,section->size,
                                                  "ocGetNames_MachO(common symbols)");
                }
                section->offset = zeroFillArea - oc->image;

                addSection(&secArray[i], kind, SECTION_NOMEM,
                           (void *)(oc->image + section->offset),
                           section->size,
                           0, 0, 0);

                addProddableBlock(oc,
                                  (void *) (oc->image + section->offset),
                                  section->size);

                secArray[i].info->nstubs = 0;
                secArray[i].info->stub_offset = NULL;
                secArray[i].info->stub_size = 0;
                secArray[i].info->stubs = NULL;

                secArray[i].info->macho_section = section;
                secArray[i].info->relocation_info
                = (MachORelocationInfo*)(oc->image + section->reloff);
                FALLTHROUGH;
            }
            default: {
                // just set the pointer to the loaded image.
                addSection(&secArray[i], kind, SECTION_NOMEM,
                           (void *)(oc->image + section->offset),
                           section->size,
                           0, 0, 0);

                addProddableBlock(oc,
                                  (void *) (oc->image + section->offset),
                                  section->size);

                secArray[i].info->nstubs = 0;
                secArray[i].info->stub_offset = NULL;
                secArray[i].info->stub_size = 0;
                secArray[i].info->stubs = NULL;

                secArray[i].info->macho_section = section;
                secArray[i].info->relocation_info
                = (MachORelocationInfo*)(oc->image + section->reloff);
            }
#endif
        }

    }
    /* now, as all sections have been loaded, we can resolve the absolute
     * address of symbols defined in those sections.
     */
    for(size_t i=0; i < oc->info->n_macho_symbols; i++) {
        MachOSymbol * s = &oc->info->macho_symbols[i];
        if( N_SECT == (s->nlist->n_type & N_TYPE) ) {
            /* section is given */
            uint8_t n = s->nlist->n_sect - 1;
            if(0 == oc->info->macho_sections[n].size) {
                continue;
            }
            if(s->nlist->n_sect == NO_SECT)
                barf("Symbol with N_SECT type, but no section.");

            /* addr <-   offset in memory where this section resides
             *         - address rel. to the image where this section is stored
             *         + symbol offset in the image
             */
            s->addr = (uint8_t*)oc->sections[n].start
                              - oc->info->macho_sections[n].addr
                              + s->nlist->n_value;
            if(NULL == s->addr)
                barf("Failed to compute address for symbol %s", s->name);
        }
    }

    // count external symbols defined here
    oc->n_symbols = 0;
    if (oc->info->symCmd) {
        for (size_t i = 0; i < oc->info->n_macho_symbols; i++) {
            if (oc->info->nlist[i].n_type & N_STAB) {
                ;
            }
            else if(oc->info->nlist[i].n_type & N_EXT)
            {
                if((oc->info->nlist[i].n_type & N_TYPE) == N_UNDF
                    && (oc->info->nlist[i].n_value != 0))
                {
                    commonSize += oc->info->nlist[i].n_value;
                    oc->n_symbols++;
                }
                else if((oc->info->nlist[i].n_type & N_TYPE) == N_SECT)
                    oc->n_symbols++;
            }
        }
    }
    /* allocate space for the exported symbols
     * in the object code. This is used to track
     * which symbols will have to be removed when
     * this object code is unloaded
     */
    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: %d external symbols\n",
                                oc->n_symbols));
    oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(Symbol_t),
                                   "ocGetNames_MachO(oc->symbols)");

    if (oc->info->symCmd) {
        for (size_t i = 0; i < oc->info->n_macho_symbols; i++) {
            SymbolName* nm = oc->info->macho_symbols[i].name;
            if(oc->info->nlist[i].n_type & N_STAB)
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: Skip STAB: %s\n", nm));
            }
            else if((oc->info->nlist[i].n_type & N_TYPE) == N_SECT)
            {
                if(oc->info->nlist[i].n_type & N_EXT)
                {
                    if (   (oc->info->nlist[i].n_desc & N_WEAK_DEF)
                        && lookupSymbol_(nm)) {
                        // weak definition, and we already have a definition
                        IF_DEBUG(linker, debugBelch("    weak: %s\n", nm));
                    }
                    else
                    {
                            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting %s\n", nm));
                            SymbolAddr* addr = oc->info->macho_symbols[i].addr;

                            ghciInsertSymbolTable( oc->fileName
                                                 , symhash
                                                 , nm
                                                 , addr
                                                 , HS_BOOL_FALSE
                                                 , oc);

                            oc->symbols[curSymbol].name = nm;
                            oc->symbols[curSymbol].addr = addr;
                            curSymbol++;
                    }
                }
                else
                {
                    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not external, skipping %s\n", nm));
                }
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not defined in this section, skipping %s\n", nm));
            }
        }
    }

    /* setup the common storage */
    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;

    if (oc->info->symCmd) {
        for (size_t i = 0; i < oc->info->n_macho_symbols; i++) {
            SymbolName* nm = oc->info->macho_symbols[i].name;
            MachONList *nlist = &oc->info->nlist[i];
            if((nlist->n_type & N_TYPE) == N_UNDF
             && (nlist->n_type & N_EXT)
             && (nlist->n_value != 0)) {
                unsigned long sz = nlist->n_value;

                nlist->n_value = commonCounter;

                /* also set the final address to the macho_symbol */
                oc->info->macho_symbols[i].addr = (void*)commonCounter;

                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting common symbol: %s\n", nm));
                ghciInsertSymbolTable(oc->fileName, symhash, nm,
                                       (void*)commonCounter, HS_BOOL_FALSE, oc);
                oc->symbols[curSymbol].name = nm;
                oc->symbols[curSymbol].addr = oc->info->macho_symbols[i].addr;
                curSymbol++;

                commonCounter += sz;
            }
        }
    }
#if defined(aarch64_HOST_ARCH)
    /* Setup the global offset table
     * This is for symbols that are external, and not defined here.
     * So that we can load their address indirectly.
     *
     * We will get GOT request for any symbol that is
     * - EXT and UNDF
     * - EXT and not in the same section.
     *
     * As sections are not necessarily contiguous and can live
     * anywhere in the addressable space. This obviously makes
     * sense.  However it took me a while to figure this out.
     */
    makeGot(oc);

    /* at this point, macho_symbols, should know the addresses for
     * all symbols defined by this object code.
     * - those that are defined in sections.
     * - those that are undefined, but have a value (common storage).
     */
#endif
    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: done\n"));
    return 1;
}

#if defined(ios_HOST_OS)
bool
ocMprotect_MachO( ObjectCode *oc ) {
    for(int i=0; i < oc->n_sections; i++) {
        Section * section = &oc->sections[i];
        if(section->size == 0) continue;
        if(   (section->info->macho_section->flags & SECTION_ATTRIBUTES_USR)
           == S_ATTR_PURE_INSTRUCTIONS) {
            if( 0 != mprotect(section->start,
                              section->size + section->info->stub_size,
                              PROT_READ | PROT_EXEC) ) {
                barf("mprotect failed! errno = %d", errno);
                return false;
            }
        }
    }
    return true;
}
#endif

int
ocResolve_MachO(ObjectCode* oc)
{
    IF_DEBUG(linker, debugBelch("ocResolve_MachO: start\n"));

    if(NULL != oc->info->dsymCmd)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (oc->image + oc->info->dsymCmd->indirectsymoff);

        IF_DEBUG(linker, debugBelch("ocResolve_MachO: resolving dsymLC\n"));
        for (int i = 0; i < oc->n_sections; i++)
        {
            const char * sectionName = oc->info->macho_sections[i].sectname;
            if(    !strcmp(sectionName,"__la_symbol_ptr")
                || !strcmp(sectionName,"__la_sym_ptr2")
                || !strcmp(sectionName,"__la_sym_ptr3"))
            {
                if(!resolveImports(oc,&oc->info->macho_sections[i],
                                   indirectSyms))
                    return 0;
            }
            else if(!strcmp(sectionName,"__nl_symbol_ptr")
                ||  !strcmp(sectionName,"__pointers"))
            {
                if(!resolveImports(oc,&oc->info->macho_sections[i],
                                   indirectSyms))
                    return 0;
            }
            else if(!strcmp(sectionName,"__jump_table"))
            {
                if(!resolveImports(oc,&oc->info->macho_sections[i],
                                   indirectSyms))
                    return 0;
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocResolve_MachO: unknown section\n"));
            }
        }
    }
#if defined(aarch64_HOST_ARCH)
    /* fill the GOT table */
    for(size_t i = 0; i < oc->info->n_macho_symbols; i++) {
        MachOSymbol * symbol = &oc->info->macho_symbols[i];
        if(needGotSlot(symbol->nlist)) {
            if(N_UNDF == (symbol->nlist->n_type & N_TYPE)) {
                /* an undefined symbol. So we need to ensure we
                 * have the address.
                 */
                if(NULL == symbol->addr) {
                    symbol->addr = lookupSymbol_((char*)symbol->name);
                    if(NULL == symbol->addr)
                        barf("Failed to lookup symbol: %s", symbol->name);
                } else {
                    // we already have the address.
                }
            } /* else it was defined in the same object,
               * just a different section. We should have
               * the address as well already
               */
            if(NULL == symbol->addr) {
                barf("Something went wrong!");
            }
            if(NULL == symbol->got_addr) {
                barf("Not good either!");
            }
            *(uint64_t*)symbol->got_addr = (uint64_t)symbol->addr;
        }
    }
#endif

    for(int i = 0; i < oc->n_sections; i++)
    {
        IF_DEBUG(linker, debugBelch("ocResolve_MachO: relocating section %d\n", i));

#if defined aarch64_HOST_ARCH
        if (!relocateSectionAarch64(oc, &oc->sections[i]))
            return 0;
#else
        if (!relocateSection(oc,oc->image,oc->info->symCmd,oc->info->nlist,
                             oc->info->segCmd->nsects,oc->info->macho_sections,
                             &oc->info->macho_sections[i]))
            return 0;
#endif
    }
#if defined(ios_HOST_OS)
    if(!ocMprotect_MachO ( oc ))
        return 0;
#endif

#if defined (powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

    return 1;
}

int
ocRunInit_MachO ( ObjectCode *oc )
{
    if (NULL == oc->info->segCmd) {
        barf("ocRunInit_MachO: no segment load command");
    }

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (int i = 0; i < oc->n_sections; i++) {
        // ToDo: replace this with a proper check for the S_MOD_INIT_FUNC_POINTERS
        // flag.  We should do this elsewhere in the Mach-O linker code
        // too.  Note that the system linker will *refuse* to honor
        // sections which don't have this flag, so this could cause
        // weird behavior divergence (albeit reproducible).
        if (0 == strcmp(oc->info->macho_sections[i].sectname,
                        "__mod_init_func")) {

            void *init_startC = oc->sections[i].start;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)((uint8_t*)init_startC
                             + oc->sections[i].info->macho_section->size);
            for (; init < init_end; init++) {
                (*init)(argc, argv, envv);
            }
        }
    }

    freeProgEnvv(envc, envv);
    return 1;
}

#if defined(powerpc_HOST_ARCH)
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
    MachOHeader header;
    int misalignment;

    {
        size_t n = fread(&header, sizeof(header), 1, f);
        if (n != 1) {
            barf("machoGetMisalignment: can't read the Mach-O header");
        }
    }
    fseek(f, -sizeof(header), SEEK_CUR);

#if defined(x86_64_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(aarch64_HOST_ARCH)
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

#endif /* darwin_HOST_OS, ios_HOST_OS */
