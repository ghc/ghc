#if defined(PROFILING)

#include "Rts.h"
#include "rts/prof/IndexTable.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "rts/PosixSource.h"
#include "rts/prof/CCS.h"
#include "Hash.h"
#include "assert.h"

#include "Profiling.h"
#include "Arena.h"

#include <fs_rts.h>
#include <string.h>

#if defined(DEBUG) || defined(PROFILING)
#include "Trace.h"
#endif


typedef struct IndexTable_ IndexTable;

void
freeIndexTable(IndexTable * it) {
  assert(it != EMPTY_TABLE);
  if (it != EMPTY_TABLE) {
    freeHashTable(it->children, NULL);
    it->children = NULL;
  }
}

STATIC_INLINE int
compareWord(StgWord key1, StgWord key2)
{
    return (key1 == key2);
}

CostCentreStack *
isInIndexTable(IndexTable *it, CostCentre *cc) {
    if (EMPTY_TABLE == it) {
        return EMPTY_TABLE;
    }
    // IF_DEBUG(prof,
    //         traceBegin("isInIndexTable %s ", cc->label);
    //         debugBelch("<%d>", keyCountHashTable(it->children));
    //         traceEnd(););

    IndexTableNode * node;
    node = (IndexTableNode *) lookupHashTable_(it->children, (StgWord) cc->ccID, hashWord, compareWord);
    if (node == NULL) {
      /* Not found */
      return EMPTY_TABLE;
    }
    return node->ccs;
}


IndexTable *
addToIndexTable(IndexTable *it, CostCentreStack *new_ccs,
                CostCentre *cc, bool back_edge) {
    if (it == EMPTY_TABLE) {
      it = arenaAlloc(prof_arena, sizeof(IndexTable));
      it->children = allocHashTable();
    }
    assert(it != EMPTY_TABLE);

    IndexTableNode *node;
    node = arenaAlloc(prof_arena, sizeof(IndexTableNode));

    node->cc = cc;
    node->ccs = new_ccs;
    node->back_edge = back_edge;

    insertHashTable_(it->children, (StgWord) node->cc->ccID, (const void *) node, hashWord);

    return it;
}

struct IndexTableIter_ {
    struct HashIterator_ *iterator;
};

IndexTableIter*
indexTableIterator(IndexTable *it) {
    IndexTableIter *iter;
    HashIterator *hashIter = NULL;
    iter = arenaAlloc(prof_arena, sizeof(IndexTableIter));

    if (it != EMPTY_TABLE) {
        hashIter = arenaAlloc(prof_arena, sizeof(struct HashIterator_));
        initHashIterator(it->children, hashIter);
    }

    iter->iterator = hashIter;
    return iter;
}

int
indexTableIterNext (IndexTableIter *iter) {
    assert(iter != NULL);
    if (iter->iterator == NULL) {
      return 0;
    }
    return hashIteratorNext(iter->iterator);
};


IndexTableNode*
indexTableIterItem(IndexTableIter *it) {
    assert(it != NULL);
    if (it->iterator == NULL) {
      return EMPTY_TABLE;
    }
    return (IndexTableNode *) hashIteratorItem(it->iterator);
}

#endif /* PROFILING */
