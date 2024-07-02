#if !defined(ElfTypes_h)
#define ElfTypes_h

#if defined(OBJFORMAT_ELF)

#include "ghcplatform.h"

#include <elf.h>
#include "linker/InitFini.h"

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#  define ELF_TARGET_AMD64 /* Used inside <elf.h> on Solaris 11 */

/* __LP64__ is a rough proxy if a platform is ELFCLASS64 */
#if defined(__LP64__) || defined(_LP64)
#  define ELF_64BIT
#else
#  define ELF_32BIT
#endif

#if defined(ELF_64BIT)
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Half    Elf64_Half
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#if !defined(ELF_ST_VISIBILITY)
#define ELF_ST_VISIBILITY ELF64_ST_VISIBILITY
#endif
#if !defined(ELF_ST_TYPE)
#define ELF_ST_TYPE ELF64_ST_TYPE
#endif
#if !defined(ELF_ST_BIND)
#define ELF_ST_BIND ELF64_ST_BIND
#endif
#if !defined(ELF_R_TYPE)
#define ELF_R_TYPE  ELF64_R_TYPE
#endif
#if !defined(ELF_R_SYM)
#define ELF_R_SYM   ELF64_R_SYM
#endif
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Half    Elf32_Half
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#if !defined(ELF_ST_VISIBILITY)
#define ELF_ST_VISIBILITY ELF32_ST_VISIBILITY
#endif /* ELF_ST_VISIBILITY */
#if !defined(ELF_ST_TYPE)
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif /* ELF_ST_TYPE */
#if !defined(ELF_ST_BIND)
#define ELF_ST_BIND ELF32_ST_BIND
#endif /* ELF_ST_BIND */
#if !defined(ELF_R_TYPE)
#define ELF_R_TYPE  ELF32_R_TYPE
#endif /* ELF_R_TYPE */
#if !defined(ELF_R_SYM)
#define ELF_R_SYM   ELF32_R_SYM
#endif /* ELF_R_SYM */
#endif /* ELF_64BIT */

typedef struct _ElfSymbol {
    SymbolName * name;  /* the name of the symbol. */
    SymbolAddr * addr;  /* the final resting place of the symbol */
    void * got_addr;    /* address of the got slot for this symbol, if any */
    Elf_Sym * elf_sym;  /* the elf symbol entry */
} ElfSymbol;

typedef struct _ElfSymbolTable {
    unsigned  index;               /* the index of the underlying symtab */
    ElfSymbol * symbols;
    size_t n_symbols;
    char * names;                  /* strings table for this symbol table */
    struct _ElfSymbolTable * next; /* there may be multiple symbol tables */
} ElfSymbolTable;

typedef struct _ElfRelocationTable {
    unsigned index;
    unsigned targetSectionIndex;
    Elf_Shdr *sectionHeader;
    Elf_Rel  *relocations;
    size_t n_relocations;
    struct _ElfRelocationTable *next;
} ElfRelocationTable;

typedef struct _ElfRelocationATable {
    unsigned index;
    unsigned targetSectionIndex;
    Elf_Shdr *sectionHeader;
    Elf_Rela  *relocations;
    size_t n_relocations;
    struct _ElfRelocationATable *next;
} ElfRelocationATable;

/*
 * Just a quick ELF recap:
 *
 * .-----------------.
 * | ELF Header      |
 * |-----------------|
 * | Program Header  |
 * |-----------------|   .
 * | Section 1       |   |
 * |-----------------|   | Segment 1
 * | Section 2       |   |
 * |-----------------|   :
 * | ...             |   |
 * |-----------------|   | Segment n
 * | Section n       |   '
 * |-----------------|
 * | Section Header  |
 * '-----------------'
 *
 *
 * The Program Header will inform us about the Segments.  Whereas the Section
 * Header provides Information about the sections.
 *
 */
struct ObjectCodeFormatInfo {
    Elf_Ehdr             *elfHeader;
    Elf_Phdr             *programHeader;
    Elf_Shdr             *sectionHeader;
    char                 *sectionHeaderStrtab;

    ElfSymbolTable       *symbolTables;
    ElfRelocationTable   *relTable;
    ElfRelocationATable  *relaTable;

    struct InitFiniList* init; // Freed by ocRunInit_PEi386
    struct InitFiniList* fini; // Freed by ocRunFini_PEi386

    /* pointer to the global offset table */
    void *                got_start;
    size_t                got_size;
};

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
} Stub;

struct SectionFormatInfo {
    /*
     * The following fields are relevant for stubs next to sections only.
     */
    void * stub_offset;
    size_t stub_size;
    size_t nstubs;
    Stub * stubs;

    const char * name;

    Elf_Shdr *sectionHeader;
};
#endif /* OBJECTFORMAT_ELF */
#endif /* ElfTypes_h */
