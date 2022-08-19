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

void shouldFindNothingInAnEmptyIPEMap(Capability *cap) {
    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));

    InfoProvEnt *result = lookupIPE(get_itbl(fortyTwo));

    if (result != NULL) {
        errorBelch("Found entry in an empty IPE map!");
        exit(1);
    }
}

HaskellObj shouldFindOneIfItHasBeenRegistered(Capability *cap) {
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode) + sizeof(IpeBufferEntry));
    StringTable st;
    init_string_table(&st);

    HaskellObj fortyTwo = UNTAG_CLOSURE(rts_mkInt(cap, 42));
    node->entries[0] = makeAnyProvEntry(cap, &st, fortyTwo, 42);
    node->count = 1;
    node->next = NULL;
    node->string_table = st.buffer;

    registerInfoProvList(node);

    InfoProvEnt *result = lookupIPE(get_itbl(fortyTwo));

    if (result == NULL) {
        errorBelch("shouldFindOneIfItHasBeenRegistered: Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(result->prov.table_name, "table_name_042");
    assertStringsEqual(result->prov.closure_desc, "closure_desc_042");
    assertStringsEqual(result->prov.ty_desc, "ty_desc_042");
    assertStringsEqual(result->prov.label, "label_042");
    assertStringsEqual(result->prov.module, "module_042");
    assertStringsEqual(result->prov.src_file, "src_file_042");
    assertStringsEqual(result->prov.src_span, "src_span_042");

    return fortyTwo;
}

void shouldFindTwoIfTwoHaveBeenRegistered(Capability *cap,
                                          HaskellObj fortyTwo) {
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode) + sizeof(IpeBufferEntry));
    StringTable st;
    init_string_table(&st);

    HaskellObj twentyThree = UNTAG_CLOSURE(rts_mkInt8(cap, 23));
    node->entries[0] = makeAnyProvEntry(cap, &st, twentyThree, 23);
    node->count = 1;
    node->next = NULL;
    node->string_table = st.buffer;

    registerInfoProvList(node);

    InfoProvEnt *resultFortyTwo =
      lookupIPE(get_itbl(fortyTwo));
    InfoProvEnt *resultTwentyThree =
      lookupIPE(get_itbl(twentyThree));

    if (resultFortyTwo == NULL) {
        errorBelch("shouldFindTwoIfTwoHaveBeenRegistered(42): Found no entry in IPE map!");
        exit(1);
    }
    if (resultTwentyThree == NULL) {
        errorBelch("shouldFindTwoIfTwoHaveBeenRegistered(23): Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultFortyTwo->prov.table_name, "table_name_042");
    assertStringsEqual(resultTwentyThree->prov.table_name, "table_name_023");
}

void shouldFindTwoFromTheSameList(Capability *cap) {
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode) + 2 * sizeof(IpeBufferEntry));
    StringTable st;
    init_string_table(&st);

    HaskellObj one = UNTAG_CLOSURE(rts_mkInt16(cap, 1));
    HaskellObj two = UNTAG_CLOSURE(rts_mkInt32(cap, 2));
    node->entries[0] = makeAnyProvEntry(cap, &st, one, 1);
    node->entries[1] = makeAnyProvEntry(cap, &st, two, 2);
    node->count = 2;
    node->next = NULL;
    node->string_table = st.buffer;

    registerInfoProvList(node);

    InfoProvEnt *resultOne = lookupIPE(get_itbl(one));
    InfoProvEnt *resultTwo = lookupIPE(get_itbl(two));

    if (resultOne == NULL) {
        errorBelch("shouldFindTwoFromTheSameList(1): Found no entry in IPE map!");
        exit(1);
    }
    if (resultTwo == NULL) {
        errorBelch("shouldFindTwoFromTheSameList(2): Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultOne->prov.table_name, "table_name_001");
    assertStringsEqual(resultTwo->prov.table_name, "table_name_002");
}

void shouldDealWithAnEmptyList(Capability *cap, HaskellObj fortyTwo) {
    IpeBufferListNode *node = malloc(sizeof(IpeBufferListNode));
    node->count = 0;
    node->next = NULL;
    node->string_table = "";

    registerInfoProvList(node);

    InfoProvEnt *resultFortyTwo =
        lookupIPE(get_itbl(fortyTwo));

    if (resultFortyTwo == NULL) {
        errorBelch("shouldDealWithAnEmptyList: Found no entry in IPE map!");
        exit(1);
    }

    assertStringsEqual(resultFortyTwo->prov.table_name, "table_name_042");
}

void assertStringsEqual(const char *s1, const char *s2) {
    if (strcmp(s1, s2) != 0) {
        errorBelch("%s != %s", s1, s2);
        exit(1);
    }
}
