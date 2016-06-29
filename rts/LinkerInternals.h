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
#define NEED_SYMBOL_EXTRAS 1
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

void exitLinker( void );

void freeObjectCode (ObjectCode *oc);

#if defined(mingw32_HOST_OS)

typedef unsigned char          UChar;
typedef unsigned short         UInt16;
typedef short                  Int16;
typedef unsigned int           UInt32;
typedef          int           Int32;
typedef unsigned long long int UInt64;


typedef
struct {
    UInt16 Machine;
    UInt16 NumberOfSections;
    UInt32 TimeDateStamp;
    UInt32 PointerToSymbolTable;
    UInt32 NumberOfSymbols;
    UInt16 SizeOfOptionalHeader;
    UInt16 Characteristics;
}
COFF_header;

#define sizeof_COFF_header 20

/* Section 7.1 PE Specification */
typedef
struct {
    UInt16 Sig1;
    UInt16 Sig2;
    UInt16 Version;
    UInt16 Machine;
    UInt32 TimeDateStamp;
    UInt32 SizeOfData;
    UInt16 Ordinal;
    UInt16 Type_NameType_Reserved;
}
COFF_import_header;

#define sizeof_COFF_import_Header 20

typedef
struct {
    UChar  Name[8];
    UInt32 VirtualSize;
    UInt32 VirtualAddress;
    UInt32 SizeOfRawData;
    UInt32 PointerToRawData;
    UInt32 PointerToRelocations;
    UInt32 PointerToLinenumbers;
    UInt16 NumberOfRelocations;
    UInt16 NumberOfLineNumbers;
    UInt32 Characteristics;
}
COFF_section;

#define sizeof_COFF_section 40


typedef
struct {
    UChar  Name[8];
    UInt32 Value;
    Int16  SectionNumber;
    UInt16 Type;
    UChar  StorageClass;
    UChar  NumberOfAuxSymbols;
}
COFF_symbol;

#define sizeof_COFF_symbol 18


typedef
struct {
    UInt32 VirtualAddress;
    UInt32 SymbolTableIndex;
    UInt16 Type;
}
COFF_reloc;

#define sizeof_COFF_reloc 10

/* From PE spec doc, section 3.3.2 */
/* Note use of MYIMAGE_* since IMAGE_* are already defined in
windows.h -- for the same purpose, but I want to know what I'm
getting, here. */
#define MYIMAGE_FILE_RELOCS_STRIPPED        0x0001
#define MYIMAGE_FILE_EXECUTABLE_IMAGE       0x0002
#define MYIMAGE_FILE_DLL                    0x2000
#define MYIMAGE_FILE_SYSTEM                 0x1000
#define MYIMAGE_FILE_BYTES_REVERSED_HI      0x8000
#define MYIMAGE_FILE_BYTES_REVERSED_LO      0x0080
#define MYIMAGE_FILE_32BIT_MACHINE          0x0100

/* From PE spec doc, section 5.4.2 and 5.4.4 */
#define MYIMAGE_SYM_CLASS_EXTERNAL          2
#define MYIMAGE_SYM_CLASS_STATIC            3
#define MYIMAGE_SYM_UNDEFINED               0
#define MYIMAGE_SYM_CLASS_SECTION           104
#define MYIMAGE_SYM_CLASS_WEAK_EXTERNAL     105

/* From PE spec doc, section 3.1 */
#define MYIMAGE_SCN_CNT_CODE                0x00000020
#define MYIMAGE_SCN_CNT_INITIALIZED_DATA    0x00000040
#define MYIMAGE_SCN_CNT_UNINITIALIZED_DATA  0x00000080
#define MYIMAGE_SCN_LNK_COMDAT              0x00001000
#define MYIMAGE_SCN_LNK_NRELOC_OVFL         0x01000000
#define MYIMAGE_SCN_LNK_REMOVE              0x00000800
#define MYIMAGE_SCN_MEM_DISCARDABLE         0x02000000

/* From PE spec doc, section 5.2.1 */
#define MYIMAGE_REL_I386_DIR32              0x0006
#define MYIMAGE_REL_I386_DIR32NB            0x0007
#define MYIMAGE_REL_I386_REL32              0x0014

#endif /* OBJFORMAT_PEi386 */

#endif /* LINKERINTERNALS_H */
