/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 2001

   Arena allocation interface.
   -------------------------------------------------------------------------- */

#pragma once

// For internal use only:
RTS_PRIVATE unsigned long arenaBlocks( void );

#if defined(DEBUG)
void checkPtrInArena( StgPtr p, Arena *arena );
#endif
