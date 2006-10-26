/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

void scavenge                ( step * );
void scavenge_mark_stack     ( void );
void scavenge_large          ( step * );
void scavenge_static         ( void );
void scavenge_mutable_list   ( generation *g );
