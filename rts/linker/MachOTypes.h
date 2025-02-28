#pragma once

#if defined(OBJFORMAT_MACHO)

#include "ghcplatform.h"

#include <mach-o/loader.h>

#if defined(x86_64_HOST_ARCH) \
 || defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH)
typedef struct mach_header_64     MachOHeader;
typedef struct segment_command_64 MachOSegmentCommand;
typedef struct section_64         MachOSection;
typedef struct nlist_64           MachONList;
#else
#error Unknown Darwin architecture
#endif
typedef struct load_command     MachOLoadCommand;
typedef struct symtab_command   MachOSymtabCommand;
typedef struct dysymtab_command MachODsymtabCommand;
typedef struct relocation_info  MachORelocationInfo;
typedef struct scattered_relocation_info MachOScatteredRelocationInfo;

/* Dealing with nlist symbol entries can become
 * painful.  We'll have our own Symbol struct that
 * mirrors the symbol from the nlist and can carry
 * some more information (like addr).
 */
typedef struct _MachOSymbol {
    SymbolName * name;  /* the name of the symbol. */
    SymbolAddr * addr;  /* the final resting place of the symbol */
    void * got_addr;    /* address of the got slot for this symbol, if any */
    MachONList * nlist; /* the nlist symbol entry */
    bool needs_got;     /* See Note [Symbols in need of GOT entries] */
} MachOSymbol;

struct ObjectCodeFormatInfo {
    // while we have the image
    // we can store some pointers
    // into it, so we don't have
    // recompute them each time.
    /* the object header */
    MachOHeader          *header;
    MachOSymtabCommand   *symCmd;
    MachOSegmentCommand  *segCmd;
    MachODsymtabCommand  *dsymCmd;
    /* points to the first nlist in the image */
    MachONList           *nlist;
    /* points to the names offset in the image */
    char                 *names;

    /* points to the start of the sections */
    MachOSection         *macho_sections;

    /* A richer nlist type */
    MachOSymbol          *macho_symbols;
    size_t               n_macho_symbols;

    /* pointer to the global offset table */
    void                 *got_start;
    size_t                got_size;
};

/* When loading sections of the macho
 * into different pages, such that the
 * pages can be marked r+x for text and
 * r+w for data, relocation may need
 * to be done indirectly, as the symbol
 * is not within reach of the of the
 * call site. E.g. B/BL instructions on
 * aarch64 have +-128mb relative
 * range.  When pages are mmap'd they
 * may end up at random positions.
 *
 * Hence we reserve space for the stub
 * slots right after the text section
 *
 *  .----------. - page start
 *  |          |
 *  |  __TEXT  |
 *  |          |
 *  |----------|
 *  |  Stubs   |
 *  '----------'
 *
 * Therefore, unless the __TEXT section
 * grows beyond 128mb-|Stubs|, we can
 * always reach the corresponding stub
 * for a symbol.
 *
 * Stubs will be rendered as
 * - 8 bytes: target address
 * - 4 bytes: relative load at -8bytes
 instruction
 * - 4 bytes: branch instruction
 *
 * These are very similar to the SymbolExtras
 * below.  However the SymbolExtras are allocated
 * per ObjectCode and not per Section.
 *
 * TODO: Merge SymbolExtras and Stubs.
 */
typedef
struct _Stub {
    void * addr;
    void * target;
    /* flags can hold architecture specific information they are used during
     * lookup of stubs as well. Thus two stubs for the same target with
     * different flags are considered unequal.
    */
    uint8_t flags;
    struct _Stub * next;
}
Stub;

struct SectionFormatInfo {
    /*
     * The following fields are relevant for stubs next to sections only.
     */
    void * stub_offset;
    size_t stub_size;
    size_t nstubs;
    Stub * stubs;

    /*
     * The following fields make working with mach-o objects much easier.
     */
    MachOSection * macho_section;
    MachORelocationInfo * relocation_info;
};

#endif /* OBJECTFORMAT_MACHO */
