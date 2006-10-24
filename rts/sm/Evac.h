/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Generational garbage collector: evacuation functions
 *
 * ---------------------------------------------------------------------------*/

// Use a register argument for evacuate, if available.
#if __GNUC__ >= 2
#define REGPARM1 __attribute__((regparm(1)))
#else
#define REGPARM1
#endif

REGPARM1 StgClosure * evacuate (StgClosure *q);

extern lnat thunk_selector_depth;
