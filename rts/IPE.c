/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2021
 *
 * Support for mapping info table pointers to source locations
 *
 * ---------------------------------------------------------------------------*/


#include "rts/PosixSource.h"
#include "Rts.h"

#include "Capability.h"
#include "Hash.h"
#include "IPE.h"
#include "Printer.h"
#include "Profiling.h"
#include "RtsUtils.h"

#include <fs_rts.h>
#include <string.h>

#if HAVE_LIBZSTD == 1
#include <zstd.h>
#endif

#if defined(TRACING)
#include "Trace.h"
#endif

/*
Note [The Info Table Provenance Entry (IPE) Map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IPEs are stored in a hash map from info table address (pointer) to IPE. This
ensures cheap lookup and traversal.

Unfortunately, inserting into the hash map is relatively expensive. To keep
startup times low, there's a temporary data structure that is optimized for
collecting IPE lists on registration.

It's a singly linked list of IPE list buffers (IpeBufferListNode). These are
emitted by the code generator, with generally one produced per module. Each
contains a pointer to a list of IPE entries, a pointer to a list of info
table pointers, and a link field (which is used to link buffers onto the
pending list.

For reasons of space efficiency, IPE entries are represented slightly
differently in the object file than the InfoProvEnt which we ultimately expose
to the user. Specifically, the IPEs in IpeBufferListNode are represented by
IpeBufferEntrys, along with a corresponding string table. The string fields
of InfoProvEnt are represented in IpeBufferEntry as 32-bit offsets into the
string table. This allows us to halve the size of the buffer entries on
64-bit machines while significantly reducing the number of needed
relocations, reducing linking cost. Moreover, the code generator takes care
to deduplicate strings when generating the string table.

Building the hash map is done lazily, i.e. on first lookup or traversal. For
this all IPE lists of all IpeBufferListNode are traversed to insert all IPEs.
This involves allocating a IpeMapEntry for each IPE entry, pointing to the
entry's containing IpeBufferListNode and its index in that node.

When the user looks up an IPE entry, we convert it to the user-facing
InfoProvEnt representation.

Note [Stable identifiers for IPE entries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each IPE entry is given a stable identifier which remains the same across
different runs of the executable (unlike the address of the info table).

The identifier is a 64-bit word which consists of two parts.

* The high 32-bits are a per-node identifier.
* The low 32-bits are the index of the entry in the node.

When a node is queued in the pending list by `registerInfoProvList` it is
given a unique identifier from an incrementing global variable.

The unique key can be computed by using the `IPE_ENTRY_KEY` macro.

*/

typedef struct {
    IpeBufferListNode *node;
    uint32_t idx;
} IpeMapEntry;

// See Note [Stable identifiers for IPE entries]
#define IPE_ENTRY_KEY(entry) \
    MAKE_IPE_KEY((entry).node->node_id, (entry).idx)

#define MAKE_IPE_KEY(module_id, idx) \
    ((((uint64_t)(module_id)) << 32) | ((uint64_t)(idx)))

#if defined(THREADED_RTS)
static Mutex ipeMapLock = MUTEX_INIT;
#endif
// Protected by ipeMapLock
static HashTable *ipeMap = NULL;

// Accessed atomically
static IpeBufferListNode *ipeBufferList = NULL;

// A global counter which is used to give an IPE entry a unique value across runs.
static StgWord next_module_id = 1; // Start at 1 to reserve 0 as "invalid"

static void decompressIPEBufferListNodeIfCompressed(IpeBufferListNode*);
static void updateIpeMap(void);

// Check whether the IpeBufferListNode has the relevant magic words.
// See Note [IPE Stripping and magic words]
static inline bool ipe_node_valid(const IpeBufferListNode *node) {
    return node &&
           node->entries_block &&
           node->string_table_block &&
           node->entries_block->magic == IPE_MAGIC_WORD &&
           node->string_table_block->magic == IPE_MAGIC_WORD;
}

static InfoProvEnt ipeBufferEntryToIpe(const IpeBufferListNode *node, uint32_t idx)
{
    CHECK(idx < node->count);
    CHECK(!node->compressed);
    const char *strings = node->string_table_block->string_table;
    const IpeBufferEntry *ent = &node->entries_block->entries[idx];
    return (InfoProvEnt) {
            .info = node->tables[idx],
            .prov = {
                .info_prov_id  = MAKE_IPE_KEY(node->node_id, idx),
                .table_name = &strings[ent->table_name],
                .closure_desc = ent->closure_desc,
                .ty_desc = &strings[ent->ty_desc],
                .label = &strings[ent->label],
                .unit_id = &strings[node->unit_id],
                .module = &strings[node->module_name],
                .src_file = &strings[ent->src_file],
                .src_span = &strings[ent->src_span]
            }
    };
}


#if defined(TRACING)
static void traceIPEFromHashTable(void *data STG_UNUSED, StgWord key STG_UNUSED,
                                  const void *value) {
    const IpeMapEntry *map_ent = (const IpeMapEntry *)value;
    if (ipe_node_valid(map_ent->node)){
      const InfoProvEnt ipe = ipeBufferEntryToIpe(map_ent->node, map_ent->idx);
      traceIPE(&ipe);
    }
}

void dumpIPEToEventLog(void) {
    // Dump pending entries
    IpeBufferListNode *node = RELAXED_LOAD(&ipeBufferList);
    while (node != NULL) {
        if (ipe_node_valid(node)){
          decompressIPEBufferListNodeIfCompressed(node);

          for (uint32_t i = 0; i < node->count; i++) {
              const InfoProvEnt ent = ipeBufferEntryToIpe(node, i);
              traceIPE(&ent);
          }
        }
        node = node->next;
    }

    // Dump entries already in hashmap
    ACQUIRE_LOCK(&ipeMapLock);
    if (ipeMap != NULL) {
        mapHashTable(ipeMap, NULL, &traceIPEFromHashTable);
    }
    RELEASE_LOCK(&ipeMapLock);
}


#else

void dumpIPEToEventLog(void) { }

#endif

/* Registering IPEs

Adds the ent_list to the temporary buffer structure described in
Note [The Info Table Provenance Entry (IPE) Map].

Statically initialized IPE lists are registered at startup by a C constructor
function generated by the compiler (CodeOutput.hs) in a *.c file for each
module. Since this is called in a static initializer we cannot rely on
ipeMapLock; we instead use atomic CAS operations to add to the list.

A performance test for IPE registration and lookup can be found here:
https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5724#note_370806

Note that IPEs are still regiestered even if the .ipe section is stripped. That's
because you may still want to query what the unique identifier for an info table is
so it can be reconciled with previously extracted metadata information. For example,
when `-hi` profiling or using `whereFrom`.

*/
void registerInfoProvList(IpeBufferListNode *node) {

        // Grab a fresh module_id
    uint32_t module_id;
    StgWord temp_module_id;
    while (true) {
        temp_module_id = next_module_id;
        if (cas(&next_module_id, temp_module_id, temp_module_id+1) == temp_module_id) {
            module_id = (uint32_t) temp_module_id;
            break;
        }

    }
    while (true) {
        IpeBufferListNode *old = RELAXED_LOAD(&ipeBufferList);
        node->next = old;
        node->node_id = module_id;
        if (cas_ptr((volatile void **) &ipeBufferList, old, node) == (void *) old) {
            return;
        }
    }
}

void formatClosureDescIpe(const InfoProvEnt *ipe_buf, char *str_buf) {
    snprintf(str_buf, CLOSURE_DESC_BUFFER_SIZE, "%u", ipe_buf->prov.closure_desc);
}

bool lookupIPE(const StgInfoTable *info, InfoProvEnt *out) {
    updateIpeMap();
    IpeMapEntry *map_ent = (IpeMapEntry *) lookupHashTable(ipeMap, (StgWord)info);
    if (map_ent && ipe_node_valid(map_ent->node)) {
        *out = ipeBufferEntryToIpe(map_ent->node, map_ent->idx);
        return true;
    } else {
        return false;
    }
}

// Returns 0 when the info table is not present in the info table map.
// See Note [Stable identifiers for IPE entries]
uint64_t lookupIPEId(const StgInfoTable *info) {
    updateIpeMap();
    IpeMapEntry *map_ent = (IpeMapEntry *) lookupHashTable(ipeMap, (StgWord)(info));
    if (map_ent){
        return IPE_ENTRY_KEY(*map_ent);
    } else {
        return 0;
    }
}

void updateIpeMap(void) {
    // Check if there's any work at all. If not so, we can circumvent locking,
    // which decreases performance.
    IpeBufferListNode *pending = xchg_ptr((void **) &ipeBufferList, NULL);
    if (ipeMap != NULL && pending == NULL) {
        return;
    }

    ACQUIRE_LOCK(&ipeMapLock);

    if (ipeMap == NULL) {
        ipeMap = allocHashTable();
    }

    while (pending != NULL) {
        IpeBufferListNode *node = pending;

        // Decompress if compressed
        decompressIPEBufferListNodeIfCompressed(node);

        // Insert entries into ipeMap
        IpeMapEntry *map_ents = stgMallocBytes(node->count * sizeof(IpeMapEntry), "updateIpeMap: ip_ents");
        for (uint32_t i = 0; i < node->count; i++) {
            const StgInfoTable *tbl = node->tables[i];
            map_ents[i].node = node;
            map_ents[i].idx = i;
            insertHashTable(ipeMap, (StgWord) tbl, &map_ents[i]);
        }

        pending = node->next;
    }

    RELEASE_LOCK(&ipeMapLock);
}

/* Decompress the IPE data and strings table referenced by an IPE buffer list
 * node if it is compressed. After returning node->compressed with be 0 and the
 * string_table and entries fields will have their uncompressed values.
 */
void decompressIPEBufferListNodeIfCompressed(IpeBufferListNode *node) {
    if (node->compressed == 1) {
        node->compressed = 0;

        // The IPE list buffer node indicates that the strings table and
        // entries list has been compressed. If zstd is not available, fail.
        // If zstd is available, decompress.
#if HAVE_LIBZSTD == 0
        barf("An IPE buffer list node has been compressed, but the "
             "decompression library (zstd) is not available.");
#else
        // Decompress string table
        size_t compressed_sz = ZSTD_findFrameCompressedSize(
            node->string_table_block->string_table,
            node->string_table_size
        );
        IpeStringTableBlock *decompressed_strings = stgMallocBytes(
            sizeof(IpeStringTableBlock) + node->string_table_size,
            "updateIpeMap: decompressed_strings"
        );
        decompressed_strings->magic = IPE_MAGIC_WORD;
        ZSTD_decompress(
            decompressed_strings->string_table,
            node->string_table_size,
            node->string_table_block->string_table,
            compressed_sz
        );
        node->string_table_block = decompressed_strings;

        // Decompress the IPE data
        compressed_sz = ZSTD_findFrameCompressedSize(
            node->entries_block->entries,
            node->entries_size
        );
        IpeBufferEntryBlock *decompressed_entries = stgMallocBytes(
            sizeof(IpeBufferEntryBlock) + node->entries_size,
            "updateIpeMap: decompressed_entries"
        );
        decompressed_entries->magic = IPE_MAGIC_WORD;
        ZSTD_decompress(
            decompressed_entries->entries,
            node->entries_size,
            node->entries_block->entries,
            compressed_sz
        );
        node->entries_block = decompressed_entries;
#endif // HAVE_LIBZSTD == 0

    }
}

#if defined(DEBUG)
void printIPE(const StgInfoTable *info) {
    InfoProvEnt ipe;
    if (lookupIPE(info, &ipe)) {
        debugBelch("%p:\n", info);
        debugBelch("    name:    %s\n", ipe.prov.table_name);
        debugBelch("    desc:    %" PRIu32 "\n", ipe.prov.closure_desc);
        debugBelch("    type:    %s\n", ipe.prov.ty_desc);
        debugBelch("    label:   %s\n", ipe.prov.label);
        debugBelch("    module:  %s:%s\n", ipe.prov.unit_id, ipe.prov.module);
        debugBelch("    src loc: %s:%s\n", ipe.prov.src_file, ipe.prov.src_span);
    } else {
        debugBelch("%p: no IPE entry\n", info);
    }
}
#endif
