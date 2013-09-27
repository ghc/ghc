/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#ifndef LINKERINTERNALS_H
#define LINKERINTERNALS_H

typedef enum {
    OBJECT_LOADED,
    OBJECT_RESOLVED,
    OBJECT_UNLOADED
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
   struct _Section { 
      void* start; 
      void* end; 
      SectionKind kind;
      struct _Section* next;
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
    int        fileSize;
    char*      formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

    /* If this object is a member of an archive, archiveMemberName is
     * like "libarchive.a(object.o)". Otherwise it's NULL.
     */
    char*      archiveMemberName;

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    char**     symbols;
    int        n_symbols;

    /* ptr to malloc'd lump of memory holding the obj file */
    char*      image;

    /* flag used when deciding whether to unload an object file */
    int        referenced;

#ifdef darwin_HOST_OS
    /* record by how much image has been deliberately misaligned
       after allocation, so that we can use realloc */
    int        misalignment;
#endif

    /* The section-kind entries for this object module.  Linked
       list. */
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

#if powerpc_HOST_ARCH || x86_64_HOST_ARCH || arm_HOST_ARCH
    SymbolExtra    *symbol_extras;
    unsigned long   first_symbol_extra;
    unsigned long   n_symbol_extras;
#endif

    ForeignExportStablePtr *stable_ptrs;

} ObjectCode;

#define OC_INFORMATIVE_FILENAME(OC)             \
    ( (OC)->archiveMemberName ?                 \
      (OC)->archiveMemberName :                 \
      (OC)->fileName                            \
    )

extern ObjectCode *objects;
extern ObjectCode *unloaded_objects;

void exitLinker( void );

void freeObjectCode (ObjectCode *oc);

#endif /* LINKERINTERNALS_H */
