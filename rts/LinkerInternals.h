/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#ifndef LINKERINTERNALS_H
#define LINKERINTERNALS_H

#include "Rts.h"
#include "Hash.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

#include "BeginPrivate.h"

typedef void SymbolAddr;
typedef char SymbolName;

typedef struct _SectionFormatInfo SectionFormatInfo;
typedef struct _ObjectCodeFormatInfo ObjectCodeFormatInfo;

/* See Linker.c Note [runtime-linker-phases] */
typedef enum {
    OBJECT_LOADED,
    OBJECT_NEEDED,
    OBJECT_RESOLVED,
    OBJECT_UNLOADED,
    OBJECT_DONT_RESOLVE
} OStatus;

/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
*/
typedef
   enum { SECTIONKIND_CODE_OR_RODATA,
          SECTIONKIND_RWDATA,
          SECTIONKIND_INIT_ARRAY,
          SECTIONKIND_OTHER,
          SECTIONKIND_NOINFOAVAIL }
   SectionKind;

typedef
   enum { SECTION_NOMEM,
          SECTION_M32,
          SECTION_MMAP,
          SECTION_MALLOC,
        }
   SectionAlloc;

typedef
   struct _Section {
      void*    start;              /* actual start of section in memory */
      StgWord  size;               /* actual size of section in memory */
      SectionKind kind;
      SectionAlloc alloc;

      /*
       * The following fields are relevant for SECTION_MMAP sections only
       */
      StgWord mapped_offset;      /* offset from the image of mapped_start */
      void* mapped_start;         /* start of mmap() block */
      StgWord mapped_size;        /* size of mmap() block */

      /* A customizable type to augment the Section type. */
       SectionFormatInfo* info;
   }
   Section;

typedef
   struct _ProddableBlock {
      void* start;
      int   size;
      struct _ProddableBlock* next;
   }
   ProddableBlock;

/*
 * We must keep track of the StablePtrs that are created for foreign
 * exports by constructor functions when the module is loaded, so that
 * we can free them again when the module is unloaded.  If we don't do
 * this, then the StablePtr will keep the module alive indefinitely.
 */
typedef struct ForeignExportStablePtr_ {
    StgStablePtr stable_ptr;
    struct ForeignExportStablePtr_ *next;
} ForeignExportStablePtr;

#if powerpc_HOST_ARCH || x86_64_HOST_ARCH || arm_HOST_ARCH
/* ios currently uses adjacent got tables, and no symbol extras */
#if !defined(ios_HOST_OS)
#define NEED_SYMBOL_EXTRAS 1
#endif /* ios_HOST_OS */
#endif

/* Jump Islands are sniplets of machine code required for relative
 * address relocations on the PowerPC, x86_64 and ARM.
 */
typedef struct {
#ifdef powerpc_HOST_ARCH
    struct {
        short lis_r12, hi_addr;
        short ori_r12_r12, lo_addr;
        long mtctr_r12;
        long bctr;
    } jumpIsland;
#elif x86_64_HOST_ARCH
    uint64_t    addr;
    uint8_t     jumpIsland[6];
#elif arm_HOST_ARCH
    uint8_t     jumpIsland[16];
#endif
} SymbolExtra;


/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
typedef struct _ObjectCode {
    OStatus    status;
    pathchar  *fileName;
    int        fileSize;     /* also mapped image size when using mmap() */
    char*      formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

    /* If this object is a member of an archive, archiveMemberName is
     * like "libarchive.a(object.o)". Otherwise it's NULL.
     */
    char*      archiveMemberName;

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    char** symbols;
    int    n_symbols;

    /* ptr to mem containing the object file image */
    char*      image;

    /* A customizable type, that formats can use to augment ObjectCode */
    ObjectCodeFormatInfo *info;

    /* non-zero if the object file was mmap'd, otherwise malloc'd */
    int        imageMapped;

    /* flag used when deciding whether to unload an object file */
    int        referenced;

    /* record by how much image has been deliberately misaligned
       after allocation, so that we can use realloc */
    int        misalignment;

    /* The section-kind entries for this object module.  Linked
       list. */
    int n_sections;
    Section* sections;

    /* Allow a chain of these things */
    struct _ObjectCode * next;

    /* SANITY CHECK ONLY: a list of the only memory regions which may
       safely be prodded during relocation.  Any attempt to prod
       outside one of these is an error in the linker. */
    ProddableBlock* proddables;

#ifdef ia64_HOST_ARCH
    /* Procedure Linkage Table for this object */
    void *plt;
    unsigned int pltIndex;
#endif

#if NEED_SYMBOL_EXTRAS
    SymbolExtra    *symbol_extras;
    unsigned long   first_symbol_extra;
    unsigned long   n_symbol_extras;
#endif

    ForeignExportStablePtr *stable_ptrs;

    /* Holds the list of symbols in the .o file which
       require extra information.*/
    HashTable *extraInfos;

} ObjectCode;

#define OC_INFORMATIVE_FILENAME(OC)             \
    ( (OC)->archiveMemberName ?                 \
      (OC)->archiveMemberName :                 \
      (OC)->fileName                            \
    )

extern ObjectCode *objects;
extern ObjectCode *unloaded_objects;

#ifdef THREADED_RTS
extern Mutex linker_mutex;
extern Mutex linker_unloaded_mutex;
#endif

/* Type of the initializer */
typedef void (*init_t) (int argc, char **argv, char **env);

/* SymbolInfo tracks a symbol's address, the object code from which
   it originated, and whether or not it's weak.

   RtsSymbolInfo is used to track the state of the symbols currently
   loaded or to be loaded by the Linker.

   Where the information in the `ObjectCode` is used to track the
   original status of the symbol inside the `ObjectCode`.

   A weak symbol that has been used will still be marked as weak
   in the `ObjectCode` but in the `RtsSymbolInfo` it won't be.
*/
typedef struct _RtsSymbolInfo {
    SymbolAddr* value;
    ObjectCode *owner;
    HsBool weak;
} RtsSymbolInfo;

void exitLinker( void );

void freeObjectCode (ObjectCode *oc);
SymbolAddr* loadSymbol(SymbolName *lbl, RtsSymbolInfo *pinfo);

void *mmapForLinker (size_t bytes, uint32_t flags, int fd, int offset);

void addProddableBlock ( ObjectCode* oc, void* start, int size );
void checkProddableBlock (ObjectCode *oc, void *addr, size_t size );
void freeProddableBlocks (ObjectCode *oc);

void addSection (Section *s, SectionKind kind, SectionAlloc alloc,
                 void* start, StgWord size, StgWord mapped_offset,
                 void* mapped_start, StgWord mapped_size);

HsBool ghciLookupSymbolInfo(HashTable *table,
                            const SymbolName* key, RtsSymbolInfo **result);

int ghciInsertSymbolTable(
    pathchar* obj_name,
    HashTable *table,
    const SymbolName* key,
    SymbolAddr* data,
    HsBool weak,
    ObjectCode *owner);

/* lock-free version of lookupSymbol */
SymbolAddr* lookupSymbol_ (SymbolName* lbl);

extern /*Str*/HashTable *symhash;


/*************************************************
 * Various bits of configuration
 *************************************************/

/* PowerPC and ARM have relative branch instructions with only 24 bit
 * displacements and therefore need jump islands contiguous with each object
 * code module.
 */
#if defined(powerpc_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif
#if defined(arm_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif

#if (RTS_LINKER_USE_MMAP && defined(SHORT_REL_BRANCH) && defined(linux_HOST_OS))
#define USE_CONTIGUOUS_MMAP 1
#else
#define USE_CONTIGUOUS_MMAP 0
#endif

HsInt isAlreadyLoaded( pathchar *path );
HsInt loadOc( ObjectCode* oc );
ObjectCode* mkOc( pathchar *path, char *image, int imageSize,
                  bool mapped, char *archiveMemberName,
                  int misalignment
                  );

#if defined (mingw32_HOST_OS)
/* We use myindex to calculate array addresses, rather than
   simply doing the normal subscript thing.  That's because
   some of the above structs have sizes which are not
   a whole number of words.  GCC rounds their sizes up to a
   whole number of words, which means that the address calcs
   arising from using normal C indexing or pointer arithmetic
   are just plain wrong.  Sigh.
*/
INLINE_HEADER unsigned char *
myindex ( int scale, void* base, int index )
{
    return
        ((unsigned char*)base) + scale * index;
}

// Defined in linker/PEi386.c
char *cstring_from_section_name(
    unsigned char* name,
    unsigned char* strtab);
#endif /* mingw32_HOST_OS */

/* MAP_ANONYMOUS is MAP_ANON on some systems,
   e.g. OS X (before Sierra), OpenBSD etc */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* Which object file format are we targetting? */
#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
|| defined(linux_android_HOST_OS) \
|| defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) \
|| defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) \
|| defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#  define OBJFORMAT_ELF
typedef struct _ObjectCodeFormatInfo { void* placeholder;} ObjectCodeFormatInfo;
typedef struct _SectionFormatInfo { void* placeholder; } SectionFormatInfo;
#elif defined (mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
typedef struct _ObjectCodeFormatInfo { void* placeholder;} ObjectCodeFormatInfo;
typedef struct _SectionFormatInfo { void* placeholder; } SectionFormatInfo;
#elif defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#  define OBJFORMAT_MACHO
#  include "linker/MachOTypes.h"
#else
#error "Unknown OBJECT_FORMAT for HOST_OS"
#endif

/* In order to simplify control flow a bit, some references to mmap-related
   definitions are blocked off by a C-level if statement rather than a CPP-level
   #if statement. Since those are dead branches when !RTS_LINKER_USE_MMAP, we
   just stub out the relevant symbols here
*/
#if !RTS_LINKER_USE_MMAP
#define munmap(x,y) /* nothing */
#define MAP_ANONYMOUS 0
#endif

#include "EndPrivate.h"
#endif /* LINKERINTERNALS_H */
