/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * Scape DWARF and .debug_ghc information out of loaded modules and place in
 * event log
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "Trace.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "LibdwScrape.h"

#if defined(USE_LIBDW) && defined(HAVE_DWARF_CU_GETDWARF) && defined(TRACING)

#include <unistd.h>
#include <elfutils/libdwfl.h>

#define DW_TAG_lexical_block       0x000b
#define DW_TAG_ghc_src_note        0x5b00

#define DW_AT_ghc_tick_parent      0x2b20

#define DW_AT_ghc_span_file        0x2b00
#define DW_AT_ghc_span_start_line  0x2b01
#define DW_AT_ghc_span_start_col   0x2b02
#define DW_AT_ghc_span_end_line    0x2b03
#define DW_AT_ghc_span_end_col     0x2b04


/*
 * Note [Reconstructing tick parents]
 *
 * For reasons explained elsewhere (see Note [Splitting DebugBlocks])
 * GHC's DIE tree doesn't match the tick tree of the C-- from which it
 * was generated. For procedures that were removed from their parent,
 * GHC has recoreded the original parent in a DW_AT_ghc_tick_parent
 * attribute. Here we need use this to reconstruct the original tick parentage
 * relationships.
 *
 * This is a two-step process involving two traversals of the DIE tree,
 *
 *   1. Traverse the DIE tree looking for DIEs with ghc_tick_parent
 *      attributes. Record these in HashTable (the parent_table) which
 *      maps from each parent DIE, identified by its offset in .debug_info to a
 *      list of its children.
 *
 *   2. Traverse the DIE tree emitting the information contained within
 *      to the eventlog. For each DIE we not only emit its direct
 *      children, but also any children recorded in the parent_table.
 *
 */

typedef struct ChildList_ {
    Dwarf_Off child_die;
    struct ChildList_ *next;
} ChildList;

static void insertParent(Dwarf_Die *parent, Dwarf_Die *child,
                         HashTable *parent_table)
{
    ChildList *new = stgMallocBytes(sizeof(ChildList), "insert_parent");
    StgWord parent_key = dwarf_dieoffset(parent);
    new->child_die = dwarf_dieoffset(child);
    new->next = removeHashTable(parent_table, parent_key, NULL);
    insertHashTable(parent_table, parent_key, new);
}

static void freeChildList(void *ptr)
{
    ChildList *list = (ChildList *) ptr;
    while (list != NULL) {
        ChildList *next = list->next;
        free(list);
        list = next;
    }
}

static void freeParentTable(HashTable *parent_table)
{
    freeHashTable(parent_table, freeChildList);
}

static int emit_source_note(Dwarf_Die *srcnote)
{
    Dwarf_Attribute attr;
    Dwarf_Word start_line, start_col, end_line, end_col;

    dwarf_attr(srcnote, DW_AT_ghc_span_file, &attr);
    const char *file = dwarf_formstring(&attr);
    if (file == NULL)
        return 1;

    dwarf_attr(srcnote, DW_AT_ghc_span_start_line, &attr);
    if (dwarf_formudata(&attr, &start_line))
        return 1;

    dwarf_attr(srcnote, DW_AT_ghc_span_start_col, &attr);
    if (dwarf_formudata(&attr, &start_col))
        return 1;

    dwarf_attr(srcnote, DW_AT_ghc_span_end_line, &attr);
    if (dwarf_formudata(&attr, &end_line))
        return 1;

    dwarf_attr(srcnote, DW_AT_ghc_span_end_col, &attr);
    if (dwarf_formudata(&attr, &end_col))
        return 1;

    traceProcSourceNote(file, start_line, start_col, end_line, end_col);
    return 0;
}

// handle a lexical_block or subprogram DIE produced by GHC
static int emit_block(Dwarf_Die *blk, const HashTable *parent_table)
{
    Dwarf_Addr basep, startp, endp;
    const char *name = dwarf_diename(blk);
    if (name == NULL)
        return DWARF_CB_OK;
    traceProc(name);

    // Output source notes
    Dwarf_Die die;
    int res = dwarf_child(blk, &die);
    while (res == 0) {
        int tag = dwarf_tag(&die);
        switch (tag) {
        case DW_TAG_lexical_block:
            emit_block(&die, parent_table);
            break;

        case DW_TAG_ghc_src_note:
            if (emit_source_note(&die)) {
                sysErrorBelch("Malformed source note in DWARF for %s", name);
            };
            break;
        }
        res = dwarf_siblingof(&die, &die);
    }

    {
        // Emit removed children.
        // See Note [Reconstructing tick parents]
        ChildList *child = lookupHashTable(parent_table, (StgWord) blk);
        Dwarf *dbg = dwarf_cu_getdwarf(blk->cu);
        while (child != NULL) {
            Dwarf_Die child_die;
            if (dwarf_offdie(dbg, child->child_die, &child_die) != NULL) {
                emit_block(&child_die, parent_table);
            } else {
                sysErrorBelch("Error looking up child DIE");
            }
            child = child->next;
        }
    }

    // Output address ranges
    int offset = 0;
    while (true) {
        offset = dwarf_ranges(blk, offset, &basep, &startp, &endp);
        if (offset <= 0)
            break;
        traceProcRange(startp, endp);
    }

    if (res < 0)
        sysErrorBelch("Error while looking up source notes: %s",
                      dwarf_errmsg(dwarf_errno()));

    traceProcEnd();

    return DWARF_CB_OK;
}

static int emit_subprogram_cb(Dwarf_Die *subprg, void *cbdata)
{
    const HashTable *parent_table = (HashTable *) cbdata;
    return emit_block(subprg, parent_table);
}

static int find_parents_block(Dwarf_Die *blk, HashTable *parent_table)
{
    Dwarf_Die die;
    int res = dwarf_child(blk, &die);
    while (res == 0) {
        if (dwarf_tag(&die) == DW_TAG_lexical_block) {
            find_parents_block(&die, parent_table);
        }
        res = dwarf_siblingof(&die, &die);
    }

    Dwarf_Attribute attr;
    if (dwarf_attr(blk, DW_AT_ghc_tick_parent, &attr) != NULL) {
        Dwarf_Die parent;
        if (! dwarf_formref_die(&attr, &parent))
            return 1;
        insertParent(&parent, blk, parent_table);
    }

    return DWARF_CB_OK;
}

static int find_parents_subprogram_cb(Dwarf_Die *subprg, void *cbdata)
{
    HashTable *parent_table = (HashTable *) cbdata;
    return find_parents_block(subprg, parent_table);
}

static int module_cb(Dwfl_Module *mod, void **user_data STG_UNUSED,
                     const char *name STG_UNUSED, Dwarf_Addr start STG_UNUSED,
                     void *cbdata STG_UNUSED)
{
    Dwarf_Die *cu_die = NULL;
    while (true) {
        Dwarf_Addr bias;
        cu_die = dwfl_module_nextcu(mod, cu_die, &bias);
        if (cu_die == NULL)
            break;

        HashTable *parent_table = allocHashTable();

        int res = dwarf_getfuncs(cu_die, find_parents_subprogram_cb,
                                 parent_table, 0);
        if (res < 0)
            sysErrorBelch("Error while enumerating DWARF information in %s: %s",
                          dwarf_diename(cu_die), dwfl_errmsg(dwfl_errno()));

        res = dwarf_getfuncs(cu_die, emit_subprogram_cb,
                                 parent_table, 0);
        if (res < 0)
            sysErrorBelch("Error while enumerating DWARF information in %s: %s",
                          dwarf_diename(cu_die), dwfl_errmsg(dwfl_errno()));

        freeParentTable(parent_table);
    }

    return DWARF_CB_OK;
}

int libdwScrapeToEventlog()
{
    static char *debuginfo_path;
    static const Dwfl_Callbacks proc_callbacks =
        {
            .find_debuginfo = dwfl_standard_find_debuginfo,
            .debuginfo_path = &debuginfo_path,
            .find_elf = dwfl_linux_proc_find_elf,
        };

    Dwfl *dwfl = dwfl_begin (&proc_callbacks);
    if (dwfl == NULL) {
        sysErrorBelch("dwfl_begin failed: %s", dwfl_errmsg(dwfl_errno()));
        return 1;
    }

    // Report the loaded modules
    int ret = dwfl_linux_proc_report(dwfl, getpid());
    if (ret < 0) {
        sysErrorBelch("dwfl_linux_proc_report failed: %s",
                      dwfl_errmsg(dwfl_errno()));
        return 1;
    }
    if (dwfl_report_end (dwfl, NULL, NULL) != 0) {
        sysErrorBelch("dwfl_report_end failed: %s", dwfl_errmsg(dwfl_errno()));
        return 1;
    }

    // Emit the module debug data to the event log
    if (dwfl_getmodules(dwfl, module_cb, NULL, 0) != 0) {
        sysErrorBelch("dwfl_getmodules failed: %s", dwfl_errmsg(dwfl_errno()));
    }

    dwfl_end(dwfl);
    return 0;
}

#else

int libdwScrapeToEventlog()
{
    return 0;
}

#endif /* TRACING && HAVE_DWARF_CU_GETDWARF && USE_LIBDW */
