/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#ifndef LINKERINTERNALS_H
#define LINKERINTERNALS_H

typedef enum { OBJECT_LOADED, OBJECT_RESOLVED } OStatus;

/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
*/
typedef 
   enum { SECTIONKIND_CODE_OR_RODATA,
          SECTIONKIND_RWDATA,
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

/* Jump Islands are sniplets of machine code required for relative
 * address relocations on the PowerPC.
 */
#ifdef powerpc_HOST_ARCH
typedef struct {
    short lis_r12, hi_addr;
    short ori_r12_r12, lo_addr;
    long mtctr_r12;
    long bctr;
} ppcJumpIsland;
#endif

/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
typedef struct _ObjectCode {
    OStatus    status;
    char*      fileName;
    int        fileSize;
    char*      formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    char**     symbols;
    int        n_symbols;

    /* ptr to malloc'd lump of memory holding the obj file */
    char*      image;

#ifdef darwin_HOST_OS
    /* record by how much image has been deliberately misaligned
       after allocation, so that we can use realloc */
    int        misalignment;
#endif

    /* The section-kind entries for this object module.  Linked
       list. */
    Section* sections;

    /* A private hash table for local symbols. */
    HashTable* lochash;
    
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

#ifdef powerpc_HOST_ARCH
    ppcJumpIsland   *jump_islands;
    unsigned long   island_start_symbol;
    unsigned long   n_islands;
#endif

} ObjectCode;

extern ObjectCode *objects;

#endif /* LINKERINTERNALS_H */
