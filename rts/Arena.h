/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#pragma once

// Abstract type of arenas
typedef struct _Arena Arena;

// Start a new arena
Arena * newArena   ( void );

// Allocate memory in an arena
void  * arenaAlloc ( Arena *, size_t );

// Free an entire arena
void arenaFree  ( Arena * );

// For internal use only:
RTS_PRIVATE unsigned long arenaBlocks( void );

#if defined(DEBUG)
void checkPtrInArena( StgPtr p, Arena *arena );
#endif
