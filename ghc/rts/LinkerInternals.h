/* -----------------------------------------------------------------------------
 * $Id: LinkerInternals.h,v 1.2 2001/02/12 12:46:23 sewardj Exp $
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

/* A bucket in the symbol hash-table.  Primarily, maps symbol names to
 * absolute addresses.  All symbols from a given module are linked
 * together, so they can be freed at the same time.  There's also a
 * bucket link field for the hash table.
 */
typedef struct _SymbolVal {
    char   *lbl;
    void   *addr;
} SymbolVal;

typedef enum { OBJECT_LOADED, OBJECT_RESOLVED } OStatus;

/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
*/
typedef enum { SECTIONKIND_CODE_OR_RODATA,
               SECTIONKIND_RWDATA,
               SECTIONKIND_OTHER,
               SECTIONKIND_NOINFOAVAIL } 
   SectionKind;

typedef struct { void* start; void* end; SectionKind kind; } 
   Section;

/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
typedef struct _ObjectCode {
    OStatus    status;
    char*      fileName;
    int        fileSize;
    char*      formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

    SymbolVal  *symbols;
    int        n_symbols;

    /* ptr to malloc'd lump of memory holding the obj file */
    void*      image;

    /* The section-kind entries for this object module.  Dynamically expands. */
    Section*   sections;
    int        n_sections;

    /* A private hash table for local symbols. */
    HashTable* lochash;
    
    /* Allow a chain of these things */
    struct _ObjectCode * next;
} ObjectCode;

extern ObjectCode *objects;
