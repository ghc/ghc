/* -----------------------------------------------------------------------------
 * $Id: StgLdvProf.h,v 1.2 2001/11/26 16:54:22 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGLDVPROF_H
#define STGLDVPROF_H

/*
  An LDV word is divided into 3 parts: state bits (LDV_STATE_MASK), creation 
  time bits (LDV_CREATE_MASK), and last use time bits (LDV_LAST_MASK). 
 */
#if SIZEOF_VOID_P == 8
#define LDV_SHIFT               30
#define LDV_STATE_MASK          0x1000000000000000
#define LDV_CREATE_MASK         0x0FFFFFFFC0000000
#define LDV_LAST_MASK           0x000000003FFFFFFF
#define LDV_STATE_CREATE        0x0000000000000000
#define LDV_STATE_USE           0x1000000000000000
#else
#define LDV_SHIFT               15
#define LDV_STATE_MASK          0x40000000 
#define LDV_CREATE_MASK         0x3FFF8000
#define LDV_LAST_MASK           0x00007FFF
#define LDV_STATE_CREATE        0x00000000
#define LDV_STATE_USE           0x40000000
#endif  // SIZEOF_VOID_P

#ifdef PROFILING

extern nat era;

// retrieves the LDV word from closure c
#define LDVW(c)                 (((StgClosure *)(c))->header.prof.hp.ldvw)

// Stores the creation time for closure c. 
// This macro is called at the very moment of closure creation.
//
// NOTE: this initializes LDVW(c) to zero, which ensures that there
// is no conflict between retainer profiling and LDV profiling,
// because retainer profiling also expects LDVW(c) to be initialised
// to zero.
#define LDV_recordCreate(c)   \
  LDVW((c)) = (era << LDV_SHIFT) | LDV_STATE_CREATE

// Stores the last use time for closure c.
// This macro *must* be called whenever a closure is used, that is, it is 
// entered.
#define LDV_recordUse(c)				\
  {							\
    if (era > 0)					\
      LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |	\
                  era |					\
                  LDV_STATE_USE;			\
  }

// Macros called when a closure is entered. 
// The closure is not an 'inherently used' one.
// The closure is not IND or IND_OLDGEN because neither is considered for LDV
// profiling.
#define LDV_ENTER(c)            LDV_recordUse((c))

#else  // !PROFILING

#define LDV_ENTER(c)            

#endif // PROFILING
#endif // STGLDVPROF_H
