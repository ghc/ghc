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
  if (it->children != NULL) {
    freeHashTable(it->children, NULL);
    it->children = NULL;
  }
}

CostCentreStack *
isInIndexTable(IndexTable *it, CostCentre *cc) {
    if (EMPTY_TABLE == it) {
        return EMPTY_TABLE;
    }
    if (NULL == it->children) {
        return EMPTY_TABLE;
    }

    CostCentreStack * node;
    node = (CostCentreStack *) lookupHashTable(it->children, cc->ccID);
    if (node == NULL) {
      /* Not found */
      return EMPTY_TABLE;
    }
    return node;
}


IndexTable *
addToIndexTable(IndexTable *it, CostCentreStack *new_ccs,
                CostCentre *cc, bool back_edge) {
    if (it == EMPTY_TABLE) {
      it = arenaAlloc(prof_arena, sizeof(IndexTable));
      it->children = NULL;
    }
    assert(it != EMPTY_TABLE);

    IndexTableNode *node;
    node = arenaAlloc(prof_arena, sizeof(IndexTableNode));

    node->cc = cc;
    node->ccs = new_ccs;
    node->back_edge = back_edge;

    if (it->children == NULL) {
      it->children = allocHashTable();
    }

    insertHashTable(it->children, (StgWord) node->cc->ccID, (const void *) node);

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

    if (it != EMPTY_TABLE && it->children != NULL) {
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
