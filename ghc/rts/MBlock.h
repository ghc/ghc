/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.4 1999/02/05 16:02:45 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

extern lnat mblocks_allocated;

extern void * getMBlock(void);
extern void * getMBlocks(nat n);
