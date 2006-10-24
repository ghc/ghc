/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector
 *
 * ---------------------------------------------------------------------------*/

#ifndef GC_H
#define GC_H

extern nat N;
extern rtsBool major_gc;
extern nat evac_gen;
extern rtsBool eager_promotion;
extern rtsBool failed_to_evac;

extern StgClosure* static_objects;
extern StgClosure* scavenged_static_objects;

extern bdescr *mark_stack_bdescr;
extern StgPtr *mark_stack;
extern StgPtr *mark_sp;
extern StgPtr *mark_splim;

extern rtsBool mark_stack_overflowed;
extern bdescr *oldgen_scan_bd;
extern StgPtr  oldgen_scan;

extern lnat new_blocks;		 // blocks allocated during this GC 
extern lnat new_scavd_blocks;	 // ditto, but depth-first blocks

#ifdef DEBUG
extern nat mutlist_MUTVARS, mutlist_MUTARRS, mutlist_OTHERS;
#endif

StgClosure * isAlive(StgClosure *p);

#endif /* GC_H */
