/* -----------------------------------------------------------------------------
 * $Id: StgLdvProf.h,v 1.1 2001/11/22 14:25:11 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGLDVPROF_H
#define STGLDVPROF_H

#ifdef PROFILING

// Engine

// declared in LdvProfile.c
extern nat ldvTime;

// LdvGenInfo stores the statistics for one specific census. 
typedef struct {
  double time;    // the time in MUT time at the corresponding census is made

  // We employ int instead of nat, for some values may be negative temporarily,
  // e.g., dragNew.

  // computed at each census
  int inherentlyUsed;   // total size of 'inherently used' closures
  int notUsed;          // total size of 'never used' closures
  int used;             // total size of 'used at least once' closures

  /*
    voidNew and dragNew are updated when a closure is destroyed.
    For instance, when a 'never used' closure of size s and creation time 
    t is destroyed at time u, voidNew of eras t through u - 1 is increased
    by s. 
    Likewise, when a 'used at least once' closure of size s and last use time
    t is destroyed at time u, dragNew of eras t + 1 through u - 1 is increase
    by s.
    In our implementation, voidNew and dragNew are computed indirectly: instead
    of updating voidNew or dragNew of all intervening eras, we update that
    of the end two eras (one is increased and the other is decreased). 
   */
  int voidNew;  // current total size of 'destroyed without being used' closures
  int dragNew;  // current total size of 'used at least once and waiting to die'
                // closures

  // computed post-mortem
  int voidTotal;  // total size of closures in 'void' state
  // lagTotal == notUsed - voidTotal    // in 'lag' state
  int dragTotal;  // total size of closures in 'drag' state 
  // useTotal == used - dragTotal       // in 'use' state
} LdvGenInfo;

extern LdvGenInfo *gi;

// retrieves the LDV word from closure c
#define LDVW(c)                 (((StgClosure *)(c))->header.prof.hp.ldvw)

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

// Stores the creation time for closure c. 
// This macro is called at the very moment of closure creation.
//
// NOTE: this initializes LDVW(c) to zero, which ensures that there
// is no conflict between retainer profiling and LDV profiling,
// because retainer profiling also expects LDVW(c) to be initialised
// to zero.
#define LDV_recordCreate(c)   \
  LDVW((c)) = (ldvTime << LDV_SHIFT) | LDV_STATE_CREATE

// Stores the last use time for closure c.
// This macro *must* be called whenever a closure is used, that is, it is 
// entered.
#define LDV_recordUse(c)                                \
  {                                                     \
    if (ldvTime > 0)                                    \
      LDVW((c)) = (LDVW((c)) & LDV_CREATE_MASK) |       \
                  ldvTime |                             \
                  LDV_STATE_USE;                        \
  }

// Creates a 0-filled slop of size 'howManyBackwards' backwards from the
// address 'from'. 
//
// Invoked when: 
//   1) Hp is incremented and exceeds HpLim (in Updates.hc).
//   2) copypart() is called (in GC.c).
#define FILL_SLOP(from, howManyBackwards)    \
  if (ldvTime > 0) {                                    \
    int i;                                              \
    for (i = 0;i < (howManyBackwards); i++)             \
      ((StgWord *)(from))[-i] = 0;                      \
  }

// Informs the LDV profiler that closure c has just been evacuated.
// Evacuated objects are no longer needed, so we just store its original size in
// the LDV field.
#define SET_EVACUAEE_FOR_LDV(c, size)   \
    LDVW((c)) = (size)

// Macros called when a closure is entered. 
// The closure is not an 'inherently used' one.
// The closure is not IND or IND_OLDGEN because neither is considered for LDV
// profiling.
#define LDV_ENTER(c)            LDV_recordUse((c))

#else  // !PROFILING

#define LDV_ENTER(c)            

#endif // PROFILING
#endif // STGLDVPROF_H
