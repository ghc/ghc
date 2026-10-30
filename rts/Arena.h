/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#pragma once

// Abstract type of arenas
typedef struct _Arena Arena;

// Start a new arena
RTS_PUBLIC Arena * newArena   ( void );

// Allocate memory in an arena
RTS_PUBLIC void  * arenaAlloc ( Arena *, size_t );

// Free an entire arena
RTS_PUBLIC void arenaFree  ( Arena * );

// For internal use only:
RTS_PRIVATE unsigned long arenaBlocks( void );

#if defined(DEBUG)
RTS_PUBLIC void checkPtrInArena( StgPtr p, Arena *arena );
#endif
