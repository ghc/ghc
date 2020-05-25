#include "Rts.h"

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)

/* for roundUpToPage */
#include "sm/OSMem.h"

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

/* aarch64 relocations may contain an addend already in the position
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

/*
 * Initialize some common data in the object code so we don't have to
 * continuously look up the addresses.
 */
void
ocInit_MachO(ObjectCode * oc)
{
    ocDeinit_MachO(oc);

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
             /* We don't have an address for this symbol yet; this
              * will be populated during ocGetNames_MachO. Hence init
              * with NULL
              */
            oc->info->macho_symbols[i].addr  = NULL;
            oc->info->macho_symbols[i].got_addr = NULL;
        }
    }
}

void
ocDeinit_MachO(ObjectCode * oc) {
    if (oc->info != NULL) {
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
        oc->info = NULL;
    }
}

static int
resolveImports(
    ObjectCode* oc,
    MachOSection *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms);

#if NEED_SYMBOL_EXTRAS
#if defined(x86_64_HOST_ARCH) || defined(aarch64_HOST_ARCH)

int
ocAllocateExtras_MachO(ObjectCode* oc)
{
    IF_DEBUG(linker, debugBelch("ocAllocateExtras_MachO: start\n"));

    if (NULL != oc->info->symCmd) {
        IF_DEBUG(linker,
            debugBelch("ocAllocateExtras_MachO: allocate %d symbols\n",
                oc->info->symCmd->nsyms));
        IF_DEBUG(linker, debugBelch("ocAllocateExtras_MachO: done\n"));
        return ocAllocateExtras(oc, oc->info->symCmd->nsyms, 0, 0);
    }

    IF_DEBUG(linker,
        debugBelch("ocAllocateExtras_MachO: allocated no symbols\n"));
    IF_DEBUG(linker, debugBelch("ocAllocateExtras_MachO: done\n"));
    return ocAllocateExtras(oc, 0, 0, 0);
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

    if(header->magic != MH_MAGIC_64) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (64bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC_64, header->magic,
                   header->magic == MH_MAGIC ? " (32bit)." : ".");
        return 0;
    }

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
            addr = lookupDependentSymbol(symbol->name, oc);
            IF_DEBUG(linker, debugBelch("resolveImports: looking up %s, %p\n", symbol->name, addr));
        }

        if (addr == NULL)
        {
            errorBelch("\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, symbol->name);
            return 0;
        }
        ASSERT(addr);

        checkProddableBlock(oc,
                            ((void**)(oc->image + sect->offset)) + i,
                            sizeof(void *));
        ((void**)(oc->image + sect->offset))[i] = addr;
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
             * implicitly 0 (as the instructions must be aligned!) and sign
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
 * pages, that may be further apart than
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
     * - registered exported symbols
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
                     * looked up via the lookupDependentSymbol function.
                     * Either through the global symbol hashmap
                     * or asking the system, if not found
                     * in the symbol hashmap
                     */
                    value = (uint64_t)lookupDependentSymbol((char*)symbol->name, oc);
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
                    value = (uint64_t)lookupDependentSymbol((char*)symbol->name, oc);
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
#endif /* aarch64_HOST_ARCH */

#if defined(x86_64_HOST_ARCH)
static int
relocateSection(ObjectCode* oc, int curSection)
{
    Section * sect = &oc->sections[curSection];

    IF_DEBUG(linker, debugBelch("relocateSection %d, info: %x\n", curSection, sect->info));

    // empty sections (without segments), won't have their info filled.
    // there is no relocation to be done for them.
    if(sect->info == NULL)
        return 1;

    MachOSection * msect = sect->info->macho_section; // for access convenience
    MachORelocationInfo * relocs = sect->info->relocation_info;
    MachOSymbol * symbols = oc->info->macho_symbols;

    IF_DEBUG(linker, debugBelch("relocateSection %d (%s, %s): start\n",
                                curSection, msect->segname, msect->sectname));

    if(!strcmp(msect->sectname,"__la_symbol_ptr"))
        return 1;
    else if(!strcmp(msect->sectname,"__nl_symbol_ptr"))
        return 1;
    else if(!strcmp(msect->sectname,"__la_sym_ptr2"))
        return 1;
    else if(!strcmp(msect->sectname,"__la_sym_ptr3"))
        return 1;

    IF_DEBUG(linker, debugBelch("relocateSection: number of relocations: %d\n", msect->nreloc));

    for(uint32_t i = 0; i < msect->nreloc; i++)
    {
        MachORelocationInfo *reloc = &relocs[i];

        char    *thingPtr = (char *) sect->start + reloc->r_address;
        uint64_t thing;
        /* We shouldn't need to initialise this, but gcc on OS X 64 bit
           complains that it may be used uninitialized if we don't */
        uint64_t value = 0;
        uint64_t baseValue;
        int type = reloc->r_type;
        int relocLenBytes;
        int nextInstrAdj = 0;

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
                thing = *(uint8_t*)thingPtr;
                relocLenBytes = 1;
                break;
            case 1:
                thing = *(uint16_t*)thingPtr;
                relocLenBytes = 2;
                break;
            case 2:
                thing = *(uint32_t*)thingPtr;
                relocLenBytes = 4;
                break;
            case 3:
                thing = *(uint64_t*)thingPtr;
                relocLenBytes = 8;
                break;
            default:
                barf("Unknown size.");
        }
        checkProddableBlock(oc,thingPtr,relocLenBytes);

        /*
         * With SIGNED_N the relocation is not at the end of the
         * instruction and baseValue needs to be adjusted accordingly.
         */
        switch (type) {
            case X86_64_RELOC_SIGNED_1:
                nextInstrAdj = 1;
                break;
            case X86_64_RELOC_SIGNED_2:
                nextInstrAdj = 2;
                break;
            case X86_64_RELOC_SIGNED_4:
                nextInstrAdj = 4;
                break;
        }
        baseValue = (uint64_t)thingPtr + relocLenBytes + nextInstrAdj;



        IF_DEBUG(linker,
                 debugBelch("relocateSection: length = %d, thing = %" PRId64 ", baseValue = %p\n",
                            reloc->r_length, thing, (char *)baseValue));

        if (type == X86_64_RELOC_GOT
         || type == X86_64_RELOC_GOT_LOAD)
        {
            MachOSymbol *symbol = &symbols[reloc->r_symbolnum];
            SymbolName* nm = symbol->name;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: making jump island for %s, extern = %d, X86_64_RELOC_GOT\n",
                                        nm, reloc->r_extern));

            if (reloc->r_extern == 0) {
                    errorBelch("\nrelocateSection: global offset table relocation for symbol with r_extern == 0\n");
            }

            if (symbol->nlist->n_type & N_EXT) {
                    // The external bit is set, meaning the symbol is exported,
                    // and therefore can be looked up in this object module's
                    // symtab, or it is undefined, meaning dlsym must be used
                    // to resolve it.

                    addr = lookupDependentSymbol(nm, oc);
                    IF_DEBUG(linker, debugBelch("relocateSection: looked up %s, "
                                                "external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n"
                                                "               : addr = %p\n", nm, addr));

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

                    if ((symbol->nlist->n_type & N_TYPE) == N_SECT) {
                        if (symbol->addr == NULL) {
                            errorBelch("relocateSection: address of internal symbol %s was not resolved\n", nm);
                            return 0;
                        }

                        addr = symbol->addr;

                        IF_DEBUG(linker, debugBelch("relocateSection: calculated relocation of "
                                                    "non-external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n"));
                        IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));
                    } else {
                        errorBelch("\nrelocateSection: %s is not exported,"
                                   " and should be defined in a section, but isn't!\n", nm);
                        return 0;
                    }
            }

            // creates a jump island for every relocation entry for a symbol
            // TODO (AP): use got_addr to store the loc. of a jump island to reuse later
            value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, (unsigned long)addr)->addr;

            type = X86_64_RELOC_SIGNED;
        }
        else if (reloc->r_extern)
        {
            MachOSymbol *symbol = &symbols[reloc->r_symbolnum];
            SymbolName* nm = symbol->name;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: looking up external symbol %s\n", nm));
            IF_DEBUG(linker, debugBelch("               : type  = %d\n", symbol->nlist->n_type));
            IF_DEBUG(linker, debugBelch("               : sect  = %d\n", symbol->nlist->n_sect));
            IF_DEBUG(linker, debugBelch("               : desc  = %d\n", symbol->nlist->n_desc));
            IF_DEBUG(linker, debugBelch("               : value = %p\n", (void *)symbol->nlist->n_value));

            if ((symbol->nlist->n_type & N_TYPE) == N_SECT) {
                ASSERT(symbol->addr != NULL);
                value = (uint64_t) symbol->addr;
                IF_DEBUG(linker, debugBelch("relocateSection, defined external symbol %s, relocated address %p\n",
                                            nm, (void *)value));
            }
            else {
                addr = lookupDependentSymbol(nm, oc);
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
            /* Since the relocation is internal, r_symbolnum contains a section
             * number relative to which the relocation is.  Depending on whether
             * the relocation is unsigned or signed, the given displacement is
             * relative to the image or the section respectively.
             *
             * For instance, in a signed case:
             * thing = <displ. to to section r_symbolnum *in the image*> (1)
             *       + <offset within r_symbolnum section>
             * (1) needs to be updated due to different section placement in memory.
             */

            CHECKM(reloc->r_symbolnum > 0,
                   "relocateSection: unsupported r_symbolnum = %" PRIu32 " < 1 for internal relocation",
                   reloc->r_symbolnum);

            int targetSecNum = reloc->r_symbolnum - 1; // sec numbers start with 1
            Section * targetSec = &oc->sections[targetSecNum];
            MachOSection * targetMacho = targetSec->info->macho_section;

            IF_DEBUG(linker,
                     debugBelch("relocateSection: internal relocation relative to section %d (%s, %s)\n",
                                targetSecNum, targetMacho->segname, targetMacho->sectname));

            switch (type) {
            case X86_64_RELOC_UNSIGNED: {
                CHECKM(thing >= targetMacho->addr,
                       "relocateSection: unsigned displacement %" PRIx64 "before target section start address %" PRIx64 "\n",
                       thing, (uint64_t) targetMacho->addr);

                uint64_t thingRelativeOffset = thing - targetMacho->addr;
                IF_DEBUG(linker, debugBelch("                 "
                                            "unsigned displacement %" PRIx64 " with section relative offset %" PRIx64 "\n",
                                            thing, thingRelativeOffset));

                thing = (uint64_t) targetSec->start + thingRelativeOffset;
                IF_DEBUG(linker, debugBelch("                 "
                                            "relocated address is %p\n", (void *) thing));

                /* Compared to external relocation we don't need to adjust value
                 * any further since thing already has absolute address.
                 */
                value = 0;
                break;
            }
            case X86_64_RELOC_SIGNED:
            case X86_64_RELOC_SIGNED_1:
            case X86_64_RELOC_SIGNED_2:
            case X86_64_RELOC_SIGNED_4: {
                uint32_t baseValueOffset = reloc->r_address + relocLenBytes + nextInstrAdj;
                uint64_t imThingLoc = msect->addr + baseValueOffset + (int64_t) thing;

                CHECKM(imThingLoc >= targetMacho->addr,
                       "relocateSection: target location %p in image before target section start address %p\n",
                       (void *) imThingLoc, (void *) targetMacho->addr);

                int64_t thingRelativeOffset = imThingLoc - targetMacho->addr;
                IF_DEBUG(linker,
                     debugBelch("                 "
                                "original displacement %" PRId64 " to %p with section relative offset %" PRIu64 "\n",
                                thing, (void *) imThingLoc, thingRelativeOffset));

                thing = (int64_t) ((uint64_t) targetSec->start + thingRelativeOffset)
                                - ((uint64_t) sect->start + baseValueOffset);
                value = baseValue; // so that it further cancels out with baseValue
                IF_DEBUG(linker,
                         debugBelch("                 "
                                    "relocated displacement %" PRId64 " to %p\n",
                                    (int64_t) thing, (void *) (baseValue + thing)));
                break;
            }
            default:
                barf("relocateSection: unexpected internal relocation type %d\n", type);
                return 0;
            }
        }

        IF_DEBUG(linker, debugBelch("relocateSection: value = %p\n", (void *) value));

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

        IF_DEBUG(linker, debugBelch("relocateSection: thing = %p\n", (void *) thing));

        /* Thing points to memory within one of the relocated sections. We can
         * probe the first byte to sanity check internal relocations.
         */
        if (0 == reloc->r_extern) {
            if (reloc->r_pcrel) {
                checkProddableBlock(oc, (void *)((char *)thing + baseValue), 1);
            } else {
                checkProddableBlock(oc, (void *)thing, 1);
            }
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
    }

    IF_DEBUG(linker, debugBelch("relocateSection: done\n"));
    return 1;
}
#endif /* x86_64_HOST_ARCH */

SectionKind
getSectionKind_MachO(MachOSection *section)
{
    SectionKind kind;

    /* todo: Use section flags instead */
    if (0==strcmp(section->sectname,"__text")) {
        kind = SECTIONKIND_CODE_OR_RODATA;
    } else if (0==strcmp(section->sectname,"__const") ||
               0==strcmp(section->sectname,"__data") ||
               0==strcmp(section->sectname,"__bss") ||
               0==strcmp(section->sectname,"__common") ||
               0==strcmp(section->sectname,"__mod_init_func")) {
        kind = SECTIONKIND_RWDATA;
    } else {
        kind = SECTIONKIND_OTHER;
    }

    return kind;
}

/* Calculate the # of active segments and their sizes based on section
 * sizes and alignments. This is done in 2 passes over sections:
 * 1. Calculate how many sections is going to be in each segment and
 * the total segment size.
 * 2. Fill in segment's sections_idx arrays.
 *
 * gbZerofillSegment is there because of this comment in mach-o/loader.h:
 * The gigabyte zero fill sections, those with the section type
 * S_GB_ZEROFILL, can only be in a segment with sections of this
 * type. These segments are then placed after all other segments.
 */
int
ocBuildSegments_MachO(ObjectCode *oc)
{
    int n_rxSections = 0;
    size_t size_rxSegment = 0;
    Segment *rxSegment = NULL;

    int n_rwSections = 0;
    size_t size_rwSegment = 0;
    Segment *rwSegment = NULL;

    int n_gbZerofills = 0;
    size_t size_gbZerofillSegment = 0;
    Segment *gbZerofillSegment = NULL;

    int n_activeSegments = 0;
    int curSegment = 0;
    size_t size_compound;

    Segment *segments = NULL;
    void *mem = NULL, *curMem = NULL;

    for (int i = 0; i < oc->n_sections; i++) {
        MachOSection *macho = &oc->info->macho_sections[i];
        if (0 == macho->size) {
            IF_DEBUG(linker, debugBelch("ocBuildSegments_MachO: found a zero length section, skipping\n"));
            continue;
        }

        size_t alignment = 1 << macho->align;

        if (S_GB_ZEROFILL == (macho->flags & SECTION_TYPE)) {
            size_gbZerofillSegment = roundUpToAlign(size_gbZerofillSegment, alignment);
            size_gbZerofillSegment += macho->size;
            n_gbZerofills++;
        } else if (getSectionKind_MachO(macho) == SECTIONKIND_CODE_OR_RODATA) {
            size_rxSegment = roundUpToAlign(size_rxSegment, alignment);
            size_rxSegment += macho->size;
            n_rxSections++;
        } else {
            size_rwSegment = roundUpToAlign(size_rwSegment, alignment);
            size_rwSegment += macho->size;
            n_rwSections++;
        }
    }

    size_compound = roundUpToPage(size_rxSegment) +
        roundUpToPage(size_rwSegment) +
        roundUpToPage(size_gbZerofillSegment);

    if (n_rxSections > 0) {
        n_activeSegments++;
    }
    if (n_rwSections > 0) {
        n_activeSegments++;
    }
    if (n_gbZerofills > 0) {
        n_activeSegments++;
    }

    // N.B. it's possible that there is nothing mappable in an object. In this
    // case we avoid the mmap call and segment allocation/building since it will
    // fail either here or further down the road, e.g. on size > 0 assert in
    // addProddableBlock. See #16701.
    if (0 == size_compound) {
        IF_DEBUG(linker, debugBelch("ocBuildSegments_MachO: all segments are empty, skipping\n"));
        return 1;
    }

    mem = mmapForLinker(size_compound, MAP_ANON, -1, 0);
    if (NULL == mem) return 0;

    IF_DEBUG(linker, debugBelch("ocBuildSegments: allocating %d segments\n", n_activeSegments));
    segments = (Segment*)stgCallocBytes(n_activeSegments, sizeof(Segment),
                                        "ocBuildSegments_MachO(segments)");
    curMem = mem;

    /* Allocate space for RX segment */
    if (n_rxSections > 0) {
        rxSegment = &segments[curSegment];
        initSegment(rxSegment,
                    curMem,
                    roundUpToPage(size_rxSegment),
                    SEGMENT_PROT_RX,
                    n_rxSections);
        IF_DEBUG(linker, debugBelch("ocBuildSegments_MachO: init segment %d (RX) at %p size %zu\n",
                                    curSegment, rxSegment->start, rxSegment->size));
        curMem = (char *)curMem + rxSegment->size;
        curSegment++;
    }

    /* Allocate space for RW segment */
    if (n_rwSections > 0) {
        rwSegment = &segments[curSegment];
        initSegment(rwSegment,
                    curMem,
                    roundUpToPage(size_rwSegment),
                    SEGMENT_PROT_RWO,
                    n_rwSections);
        IF_DEBUG(linker, debugBelch("ocBuildSegments_MachO: init segment %d (RWO) at %p size %zu\n",
                                    curSegment, rwSegment->start, rwSegment->size));
        curMem = (char *)curMem + rwSegment->size;
        curSegment++;
    }

    /* Allocate space for GB_ZEROFILL segment */
    if (n_gbZerofills > 0) {
        gbZerofillSegment = &segments[curSegment];
        initSegment(gbZerofillSegment,
                    curMem,
                    roundUpToPage(size_gbZerofillSegment),
                    SEGMENT_PROT_RWO,
                    n_gbZerofills);
        IF_DEBUG(linker, debugBelch("ocBuildSegments_MachO: init segment %d (GB_ZEROFILL) at %p size %zu\n",
                                    curSegment, gbZerofillSegment->start, gbZerofillSegment->size));
        curMem = (char *)curMem + gbZerofillSegment->size;
        curSegment++;
    }

    /* Second pass over sections to fill in sections_idx arrays */
    for (int i = 0, rx = 0, rw = 0, gb = 0;
         i < oc->n_sections;
         i++)
    {
        MachOSection *macho = &oc->info->macho_sections[i];
        // Skip zero size sections here as well since there was no place
        // allocated for them in Segment's sections_idx array
        if (0 == macho->size) {
            continue;
        }

        if (S_GB_ZEROFILL == (macho->flags & SECTION_TYPE)) {
            gbZerofillSegment->sections_idx[gb++] = i;
        } else if (getSectionKind_MachO(macho) == SECTIONKIND_CODE_OR_RODATA) {
            rxSegment->sections_idx[rx++] = i;
        } else {
            rwSegment->sections_idx[rw++] = i;
        }
    }

    oc->segments = segments;
    oc->n_segments = n_activeSegments;

    return 1;
}

int
ocGetNames_MachO(ObjectCode* oc)
{
    unsigned curSymbol = 0;

    unsigned long commonSize = 0;
    SymbolAddr* commonStorage = NULL;
    unsigned long commonCounter;

    IF_DEBUG(linker,debugBelch("ocGetNames_MachO: %s start\n",  OC_INFORMATIVE_FILENAME(oc)));

    Section *secArray;
    secArray = (Section*)stgCallocBytes(
        oc->info->segCmd->nsects,
        sizeof(Section),
        "ocGetNames_MachO(sections)");

    oc->sections = secArray;

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: will load %d sections\n",
                                oc->n_sections));

    CHECKM(ocBuildSegments_MachO(oc), "ocGetNames_MachO: failed to build segments\n");

    for (int seg_n = 0; seg_n < oc->n_segments; seg_n++) {
        Segment *segment = &oc->segments[seg_n];
        void *curMem = segment->start;

        IF_DEBUG(linker,
                 debugBelch("ocGetNames_MachO: loading segment %d "
                            "(address = %p, size = %zu) "
                            "with %d sections\n",
                            seg_n, segment->start, segment->size, segment->n_sections));

        for (int sec_n = 0; sec_n < segment->n_sections; sec_n++) {
            int sec_idx = segment->sections_idx[sec_n];
            MachOSection *section = &oc->info->macho_sections[sec_idx];

            size_t alignment = 1 << section->align;
            SectionKind kind = getSectionKind_MachO(section);

            void *secMem = (void *)roundUpToAlign((size_t)curMem, alignment);

            IF_DEBUG(linker,
                     debugBelch("ocGetNames_MachO: loading section %d in segment %d "
                                "(#%d, %s %s)\n"
                                "                  skipped %zu bytes due to alignment of %zu\n",
                                sec_n, seg_n, sec_idx, section->segname, section->sectname,
                                (char *)secMem - (char *)curMem, alignment));

            switch (section->flags & SECTION_TYPE) {
            case S_ZEROFILL:
            case S_GB_ZEROFILL:
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: memset to 0 a ZEROFILL section\n"));
                memset(secMem, 0, section->size);
                break;
            default:
                IF_DEBUG(linker,
                         debugBelch("ocGetNames_MachO: copying from %p to %p"
                                    " a block of %" PRIu64 " bytes\n",
                                    (void *) (oc->image + section->offset), secMem, section->size));

                memcpy(secMem, oc->image + section->offset, section->size);
            }

            /* SECTION_NOMEM since memory is already allocated in segments */
            addSection(&secArray[sec_idx], kind, SECTION_NOMEM,
                       secMem, section->size,
                       0, 0, 0);
            addProddableBlock(oc, secMem, section->size);

            curMem = (char*) secMem + section->size;

            secArray[sec_idx].info->nstubs = 0;
            secArray[sec_idx].info->stub_offset = NULL;
            secArray[sec_idx].info->stub_size = 0;
            secArray[sec_idx].info->stubs = NULL;

            secArray[sec_idx].info->macho_section = section;
            secArray[sec_idx].info->relocation_info
                = (MachORelocationInfo*)(oc->image + section->reloff);

        }

    }

    /* now, as all sections have been loaded, we can resolve the absolute
     * address of symbols defined in those sections.
     */
    for(size_t i=0; i < oc->info->n_macho_symbols; i++) {
        MachOSymbol * s = &oc->info->macho_symbols[i];
        if( N_SECT == (s->nlist->n_type & N_TYPE) ) {
            if( NO_SECT == s->nlist->n_sect )
                barf("Symbol with N_SECT type, but no section.");

            /* section is given, and n_sect is >0 */
            uint8_t n = s->nlist->n_sect - 1;
            if(0 == oc->info->macho_sections[n].size) {
                continue;
            }

            /* addr <-   address in memory where the relocated section resides | (a)
             *         - section's address in the image | (b)
             *         + symbol's address in the image  | (c)
             * (c) - (b) gives symbol's offset relative to section start
             * (a) - (b) + (c) gives symbol's address for the relocated section
             *
             * (c) and (b) are not _real_ addresses and not equal
             * to file offsets in the image.
             * Rather they are (virtual) aligned addresses within
             * a single segment of MH_OBJECT object file.
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
            if (oc->info->nlist[i].n_type & N_STAB)
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: Skip STAB: %s\n", nm));
            }
            else if ((oc->info->nlist[i].n_type & N_TYPE) == N_SECT)
            {
                if (oc->info->nlist[i].n_type & N_EXT)
                {
                    if (   (oc->info->nlist[i].n_desc & N_WEAK_DEF)
                        && lookupDependentSymbol(nm, oc)) {
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

static bool
ocMprotect_MachO( ObjectCode *oc )
{
    for(int i=0; i < oc->n_segments; i++) {
        Segment *segment = &oc->segments[i];
        if(segment->size == 0) continue;

        if(segment->prot == SEGMENT_PROT_RX) {
            mmapForLinkerMarkExecutable(segment->start, segment->size);
        }
    }
    return true;
}

int
ocResolve_MachO(ObjectCode* oc)
{
    IF_DEBUG(linker, debugBelch("ocResolve_MachO: %s start\n", OC_INFORMATIVE_FILENAME(oc)));

    if(NULL != oc->info->dsymCmd)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (oc->image + oc->info->dsymCmd->indirectsymoff);

        IF_DEBUG(linker, debugBelch("ocResolve_MachO: resolving dsymLC\n"));
        for (int i = 0; i < oc->n_sections; i++)
        {
            const char * sectionName = oc->info->macho_sections[i].sectname;

            IF_DEBUG(linker, debugBelch("ocResolve_MachO: section %d/%d: %s\n", i, oc->n_sections, sectionName));

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
                IF_DEBUG(linker, debugBelch("ocResolve_MachO: unknown section %d/%d\n", i, oc->n_sections));
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
                    symbol->addr = lookupDependentSymbol((char*)symbol->name, oc);
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
        IF_DEBUG(linker, debugBelch("ocResolve_MachO: relocating section %d/%d\n", i, oc->n_sections));

#if defined(aarch64_HOST_ARCH)
        if (!relocateSectionAarch64(oc, &oc->sections[i]))
            return 0;
#else
        if (!relocateSection(oc, i))
            return 0;
#endif
    }
    if(!ocMprotect_MachO ( oc ))
        return 0;

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
        IF_DEBUG(linker, debugBelch("ocRunInit_MachO: checking section %d\n", i));

        // ToDo: replace this with a proper check for the S_MOD_INIT_FUNC_POINTERS
        // flag.  We should do this elsewhere in the Mach-O linker code
        // too.  Note that the system linker will *refuse* to honor
        // sections which don't have this flag, so this could cause
        // weird behavior divergence (albeit reproducible).
        if (0 == strcmp(oc->info->macho_sections[i].sectname, "__mod_init_func")) {
            IF_DEBUG(linker, debugBelch("ocRunInit_MachO:     running mod init functions\n"));

            void *init_startC = oc->sections[i].start;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)((uint8_t*)init_startC
                             + oc->sections[i].info->macho_section->size);

            for (int pn = 0; init < init_end; init++, pn++) {
                IF_DEBUG(linker, debugBelch("ocRunInit_MachO:     function pointer %d at %p to %p\n",
                                            pn, (void *) init, (void *) *init));
                (*init)(argc, argv, envv);
            }
        }
    }

    freeProgEnvv(envc, envv);
    return 1;
}

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

    if(header.magic != MH_MAGIC_64) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC_64, header.magic);
    }

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    IF_DEBUG(linker, debugBelch("mach-o misalignment %d\n", misalignment));
    return misalignment ? (16 - misalignment) : 0;
}

#endif /* darwin_HOST_OS || ios_HOST_OS */
