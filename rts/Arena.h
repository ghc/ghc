/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#ifndef ARENA_H
#define ARENA_H

// Abstract type of arenas
typedef struct _Arena Arena;

// Start a new arena
extern Arena * newArena   ( void );

// Allocate memory in an arena
extern void  * arenaAlloc ( Arena *, size_t );

// Free an entire arena
extern void    arenaFree  ( Arena * );

// For internal use only:
extern unsigned long arenaBlocks( void );

#endif /* ARENA_H */
