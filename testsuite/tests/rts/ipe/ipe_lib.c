#include "Rts.h"
#include "rts/IPE.h"
#include <string.h>
#include "ipe_lib.h"

void init_string_table(StringTable *st) {
    st->size = 128;
    st->n = 0;
    st->buffer = malloc(st->size);
}

uint32_t add_string(StringTable *st, const char *s) {
    const size_t len = strlen(s);
    const uint32_t n = st->n;
    if (st->n + len + 1 > st->size) {
        const size_t new_size = 2*st->size + len;
        st->buffer = realloc(st->buffer, new_size);
        st->size = new_size;
    }

    memcpy(&st->buffer[st->n], s, len);
    st->n += len;
    st->buffer[st->n] = '\0';
    st->n += 1;
    return n;
}

IpeBufferEntry makeAnyProvEntry(Capability *cap, StringTable *st, int i) {
    IpeBufferEntry provEnt;

    unsigned int tableNameLength = strlen("table_name_") + 3 /* digits */ + 1 /* null character */;
    char *tableName = malloc(sizeof(char) * tableNameLength);
    snprintf(tableName, tableNameLength, "table_name_%03i", i);
    provEnt.table_name = add_string(st, tableName);

    provEnt.closure_desc = i;

    unsigned int tyDescLength = strlen("ty_desc_") + 3 /* digits */ + 1 /* null character */;
    char *tyDesc = malloc(sizeof(char) * tyDescLength);
    snprintf(tyDesc, tyDescLength, "ty_desc_%03i", i);
    provEnt.ty_desc = add_string(st, tyDesc);

    unsigned int labelLength = strlen("label_") + 3 /* digits */ + 1 /* null character */;
    char *label = malloc(sizeof(char) * labelLength);
    snprintf(label, labelLength, "label_%03i", i);
    provEnt.label = add_string(st, label);

    unsigned int srcFileLength = strlen("src_file_") + 3 /* digits */ + 1 /* null character */;
    char *srcFile = malloc(sizeof(char) * srcFileLength);
    snprintf(srcFile, srcFileLength, "src_file_%03i", i);
    provEnt.src_file = add_string(st, srcFile);

    unsigned int srcSpanLength = strlen("src_span_") + 3 /* digits */ + 1 /* null character */;
    char *srcSpan = malloc(sizeof(char) * srcSpanLength);
    snprintf(srcSpan, srcSpanLength, "src_span_%03i", i);
    provEnt.src_span = add_string(st, srcSpan);

    return provEnt;
}

IpeBufferListNode *makeAnyProvEntries(Capability *cap, int start, int end) {
    const int n = end - start;

    // Allocate buffers for IpeBufferListNode
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->tables = malloc(sizeof(StgInfoTable *) * n);
    node->entries = malloc(sizeof(IpeBufferEntry) * n);

    StringTable st;
    init_string_table(&st);

    unsigned int unitIdLength = strlen("unit_id_") + 3 /* digits */ + 1 /* null character */;
    char *unitId = malloc(sizeof(char) * unitIdLength);
    snprintf(unitId, unitIdLength, "unit_id_%03i", start);
    node->unit_id = add_string(&st, unitId);

    unsigned int moduleLength = strlen("module_") + 3 /* digits */ + 1 /* null character */;
    char *module = malloc(sizeof(char) * moduleLength);
    snprintf(module, moduleLength, "module_%03i", start);
    node->module_name = add_string(&st, module);

    // Make the entries and fill the buffers
    for (int i=start; i < end; i++) {
        HaskellObj closure = rts_mkInt(cap, 42);
        node->tables[i]  = get_itbl(closure);
        node->entries[i] = makeAnyProvEntry(cap, &st, i);
    }

    // Set the rest of the fields
    node->next = NULL;
    node->compressed = 0;
    node->count = n;
    node->string_table = st.buffer;

    return node;
}
