/* -----------------------------------------------------------------------------
   $Id: Arena.h,v 1.1 2001/10/18 14:41:01 simonmar Exp $ 
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#ifndef ARENA_H

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

#endif // ARENA_H
