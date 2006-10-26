/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: utilities
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * --------------------------------------------------------------------------*/

bdescr *gc_alloc_block(step *stp);
bdescr *gc_alloc_scavd_block(step *stp);
