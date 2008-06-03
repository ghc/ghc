/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

void    scavenge_loop (void);
void    scavenge_mutable_list (generation *g);

#ifdef THREADED_RTS
void    scavenge_loop1 (void);
void    scavenge_mutable_list1 (generation *g);
#endif
