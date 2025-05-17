/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

// An interval set on uintptr_t.
struct _ProddableBlock;

typedef struct {
    struct _ProddableBlock *head;
} ProddableBlockSet;

void initProddableBlockSet ( ProddableBlockSet* set );

// Insert an interval.
void addProddableBlock ( ProddableBlockSet* set, void* start, int size );

// Check that an address belongs to the set.
void checkProddableBlock (ProddableBlockSet *set, void *addr, size_t size );

// Free a set.
void freeProddableBlocks (ProddableBlockSet *set);
