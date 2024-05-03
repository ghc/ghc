#include <stdlib.h>
#include <string.h>

#include "Rts.h"
#include "ipe_lib.h"

void assertStringsEqual(const char *s1, const char *s2);
void shouldFindNothingInAnEmptyIPEMap(Capability *cap);
HaskellObj shouldFindOneIfItHasBeenRegistered(Capability *cap);
void shouldFindTwoIfTwoHaveBeenRegistered(Capability *cap, HaskellObj fortyTwo);
void shouldFindTwoFromTheSameList(Capability *cap);
void shouldDealWithAnEmptyList(Capability *cap, HaskellObj);

// This is a unit test for IPE.c, the IPE map.
// Due to the nature of IPE having static state, the test cases are not
// independent of each other!
int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    Capability *cap = rts_lock();

    shouldFindNothingInAnEmptyIPEMap(cap);
    HaskellObj fortyTwo = shouldFindOneIfItHasBeenRegistered(cap);
    shouldFindTwoIfTwoHaveBeenRegistered(cap, fortyTwo);
    shouldFindTwoFromTheSameList(cap);
    shouldDealWithAnEmptyList(cap, fortyTwo);

    rts_unlock(cap);
    hs_exit();
}

static InfoProvEnt lookupIPE_(const char *where, const StgInfoTable *itbl) {
    InfoProvEnt ent;
    if (!lookupIPE(itbl, &ent)) {
        barf("%s: Expected to find IPE entry", where);
    }
    return ent;
}

void shouldFindNothingInAnEmptyIPEMap(Capability *cap) {
    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));
    InfoProvEnt ent;
    if (lookupIPE(get_itbl(fortyTwo), &ent)) {
        barf("Found entry in an empty IPE map!");
    }
}

HaskellObj shouldFindOneIfItHasBeenRegistered(Capability *cap) {
    // Allocate buffers for IPE buffer list node
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->tables = malloc(sizeof(StgInfoTable *));
    node->entries = malloc(sizeof(IpeBufferEntry));

    StringTable st;
    init_string_table(&st);

    node->unit_id = add_string(&st, "unit-id");
    node->module_name = add_string(&st, "TheModule");

    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));
    node->next = NULL;
    node->compressed = 0;
    node->count = 1;
    node->tables[0] = get_itbl(fortyTwo);
    node->entries[0] = makeAnyProvEntry(cap, &st, 42);
    node->entries_size = sizeof(IpeBufferEntry);
    node->string_table = st.buffer;
    node->string_table_size = st.size;

    registerInfoProvList(node);

    const InfoProvEnt result = lookupIPE_("shouldFindOneIfItHasBeenRegistered", get_itbl(fortyTwo));

    char closure_desc_buf[CLOSURE_DESC_BUFFER_SIZE] = {};
    formatClosureDescIpe(&result, closure_desc_buf);

    assertStringsEqual(result.prov.table_name, "table_name_042");
    assertStringsEqual(closure_desc_buf, "42");
    assertStringsEqual(result.prov.ty_desc, "ty_desc_042");
    assertStringsEqual(result.prov.label, "label_042");
    assertStringsEqual(result.prov.unit_id, "unit-id");
    assertStringsEqual(result.prov.module, "TheModule");
    assertStringsEqual(result.prov.src_file, "src_file_042");
    assertStringsEqual(result.prov.src_span, "src_span_042");

    return fortyTwo;
}

void shouldFindTwoIfTwoHaveBeenRegistered(Capability *cap,
                                          HaskellObj fortyTwo) {
    // Allocate buffers for IPE buffer list node
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->tables = malloc(sizeof(StgInfoTable *));
    node->entries = malloc(sizeof(IpeBufferEntry));

    StringTable st;
    init_string_table(&st);

    node->unit_id = add_string(&st, "unit-id");
    node->module_name = add_string(&st, "TheModule");

    HaskellObj twentyThree = UNTAG_CLOSURE(rts_mkInt8(cap, 23));
    node->next = NULL;
    node->compressed = 0;
    node->count = 1;
    node->tables[0] = get_itbl(twentyThree);
    node->entries[0] = makeAnyProvEntry(cap, &st, 23);
    node->entries_size = sizeof(IpeBufferEntry);
    node->string_table = st.buffer;
    node->string_table_size = st.size;

    registerInfoProvList(node);

    InfoProvEnt resultFortyTwo = lookupIPE_("shouldFindTwoIfTwoHaveBeenRegistered", get_itbl(fortyTwo));
    assertStringsEqual(resultFortyTwo.prov.table_name, "table_name_042");

    InfoProvEnt resultTwentyThree = lookupIPE_("shouldFindTwoIfTwoHaveBeenRegistered", get_itbl(twentyThree));
    assertStringsEqual(resultTwentyThree.prov.table_name, "table_name_023");
}

void shouldFindTwoFromTheSameList(Capability *cap) {
    // Allocate buffers for IPE buffer list node
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->tables = malloc(sizeof(StgInfoTable *) * 2);
    node->entries = malloc(sizeof(IpeBufferEntry) * 2);

    StringTable st;
    init_string_table(&st);

    HaskellObj one = UNTAG_CLOSURE(rts_mkInt16(cap, 1));
    HaskellObj two = UNTAG_CLOSURE(rts_mkInt32(cap, 2));
    node->next = NULL;
    node->compressed = 0;
    node->count = 2;
    node->tables[0] = get_itbl(one);
    node->tables[1] = get_itbl(two);
    node->entries[0] = makeAnyProvEntry(cap, &st, 1);
    node->entries[1] = makeAnyProvEntry(cap, &st, 2);
    node->entries_size = sizeof(IpeBufferEntry) * 2;
    node->string_table = st.buffer;
    node->string_table_size = st.size;

    registerInfoProvList(node);

    InfoProvEnt resultOne = lookupIPE_("shouldFindTwoFromTheSameList", get_itbl(one));
    assertStringsEqual(resultOne.prov.table_name, "table_name_001");

    InfoProvEnt resultTwo = lookupIPE_("shouldFindTwoFromTheSameList", get_itbl(two));
    assertStringsEqual(resultTwo.prov.table_name, "table_name_002");
}

void shouldDealWithAnEmptyList(Capability *cap, HaskellObj fortyTwo) {
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->count = 0;
    node->next = NULL;
    node->string_table = "";

    registerInfoProvList(node);

    InfoProvEnt resultFortyTwo = lookupIPE_("shouldDealWithAnEmptyList", get_itbl(fortyTwo));
    assertStringsEqual(resultFortyTwo.prov.table_name, "table_name_042");
}

void assertStringsEqual(const char *s1, const char *s2) {
    if (strcmp(s1, s2) != 0) {
        errorBelch("%s != %s", s1, s2);
        exit(1);
    }
}
