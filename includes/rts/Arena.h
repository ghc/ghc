#pragma once

// Abstract type of arenas
typedef struct _Arena Arena;

// Start a new arena
Arena * newArena   ( void );

// Allocate memory in an arena
void  * arenaAlloc ( Arena *, size_t );

// Free an entire arena
void arenaFree  ( Arena * );

// Check if a pointer is in an arena
bool inArena( Arena *, StgPtr);


