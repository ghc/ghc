/* -*- mode: hugs-c; -*- */

#if DERIVE_SHOW | DERIVE_READ
extern List cfunSfuns;                  /* List of (Cfun,[SelectorVar])    */
#endif

extern List deriveEq      Args((Tycon));
extern List deriveOrd     Args((Tycon));
extern List deriveIx      Args((Tycon));
extern List deriveEnum    Args((Tycon));
extern List deriveShow    Args((Tycon));
extern List deriveRead    Args((Cell));
extern List deriveBounded Args((Tycon));
