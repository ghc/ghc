/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.3 1999/01/13 17:25:41 simonm Exp $
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

extern lnat mblocks_allocated;

extern void * getMBlock(void);
extern void * getMBlocks(nat n);
