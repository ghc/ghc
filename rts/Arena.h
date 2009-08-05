/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#ifndef ARENA_H
#define ARENA_H

// Abstract type of arenas
typedef struct _Arena Arena;

// Start a new arena
RTS_PRIVATE Arena * newArena   ( void );

// Allocate memory in an arena
RTS_PRIVATE void  * arenaAlloc ( Arena *, size_t );

// Free an entire arena
RTS_PRIVATE void arenaFree  ( Arena * );

// For internal use only:
RTS_PRIVATE unsigned long arenaBlocks( void );

#endif /* ARENA_H */
