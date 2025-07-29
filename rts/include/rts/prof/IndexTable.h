#pragma once

/* -----------------------------------------------------------------------------
 * Data Structures
 * ---------------------------------------------------------------------------*/

// IndexTable is the list of children of a CCS. (Alternatively it is a
// cache of the results of pushing onto a CCS, so that the second and
// subsequent times we push a certain CC on a CCS we get the same
// result).

typedef struct IndexTableNode_ {
    // Just a linked list of (cc, ccs) pairs, where the `ccs` is the result of
    // pushing `cc` to the owner of the index table (another CostCentreStack).
    CostCentre *cc;
    CostCentreStack *ccs;
    // back_edge is true when `cc` is already in the stack, so pushing it
    // truncates or drops (see RECURSION_DROPS and RECURSION_TRUNCATES in
    // Profiling.c).
    bool back_edge;
} IndexTableNode;

typedef struct IndexTableNode_ IndexTableNode;

typedef struct IndexTable_ {
    // IndexTableNode *node;
    // // Just a linked list of (cc, ccs) pairs, where the `ccs` is the result of
    // // pushing `cc` to the owner of the index table (another CostCentreStack).
    // CostCentre *cc;
    // CostCentreStack *ccs;
    // // back_edge is true when `cc` is already in the stack, so pushing it
    // // truncates or drops (see RECURSION_DROPS and RECURSION_TRUNCATES in
    // // Profiling.c).
    // bool back_edge;
    struct hashtable *children;
} IndexTable;

typedef struct IndexTable_ IndexTable;

IndexTable *      allocateIndexTable( void );
void              freeIndexTable( IndexTable * );
CostCentreStack * isInIndexTable  ( IndexTable *, CostCentre * );
IndexTable *      addToIndexTable ( IndexTable *, CostCentreStack *,
                                    CostCentre *, bool );

typedef struct IndexTableIter_ IndexTableIter;


IndexTableIter* indexTableIterator ( IndexTable * );
int             indexTableIterNext ( IndexTableIter * );
IndexTableNode* indexTableIterItem ( IndexTableIter * );
