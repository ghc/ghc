/* -----------------------------------------------------------------------------
 * $Id: CCall.h,v 1.4 2000/01/13 14:34:00 hwloidl Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Macros for performing C calls from the STG world.
 *
 * -------------------------------------------------------------------------- */

#ifndef CCALL_H
#define CCALL_H

/* 
 * Most C-Calls made from STG land are of the 'unsafe' variety.
 * An unsafe C-Call is one where we trust the C function not to do
 * anything nefarious while it has control.
 *
 * Nefarious actions include doing allocation on the Haskell heap,
 * garbage collecting, creating/deleting threads, re-entering the
 * scheduler, and messing with runtime system data structures.
 * 
 * For these calls, the code generator will kindly provide CALLER_SAVE
 * and CALLER_RESTORE macros for any registers that are live across the
 * call.  These macros may expand into saves of the relevant registers
 * if those registers are designated caller-saves by the C calling
 * convention, otherwise they will expand to nothing.
 */

/* Unsafe C-Calls have no macros: we just use a straightforward call.
 */

/*
 * An STGCALL<n> is used when we want the relevant registers to be
 * saved automatically.  An STGCALL doesn't return a result, there's
 * an alternative set of RET_STGCALL<n> macros for that (and we hope
 * that the restoring of the caller-saves registers doesn't clobber
 * the result!)
 */

#define STGCALL0(f) \
  CALLER_SAVE_ALL (void) f(); CALLER_RESTORE_ALL

#define STGCALL1(f,a) \
  CALLER_SAVE_ALL (void) f(a); CALLER_RESTORE_ALL

#define STGCALL2(f,a,b) \
  CALLER_SAVE_ALL (void) f(a,b); CALLER_RESTORE_ALL

#define STGCALL3(f,a,b,c) \
  CALLER_SAVE_ALL (void) f(a,b,c); CALLER_RESTORE_ALL

#define STGCALL4(f,a,b,c,d) \
  CALLER_SAVE_ALL (void) f(a,b,c,d); CALLER_RESTORE_ALL

#define STGCALL5(f,a,b,c,d,e) \
  CALLER_SAVE_ALL (void) f(a,b,c,d,e); CALLER_RESTORE_ALL

#define STGCALL6(f,a,b,c,d,e,z) \
  CALLER_SAVE_ALL (void) f(a,b,c,d,e,z); CALLER_RESTORE_ALL


#define RET_STGCALL0(t,f) \
  ({ t _r; CALLER_SAVE_ALL _r = f(); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL1(t,f,a) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL2(t,f,a,b) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a,b); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL3(t,f,a,b,c) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a,b,c); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL4(t,f,a,b,c,d) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a,b,c,d); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL5(t,f,a,b,c,d,e) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a,b,c,d,e); CALLER_RESTORE_ALL; _r; })

#define RET_STGCALL6(t,f,a,b,c,d,e,z) \
  ({ t _r; CALLER_SAVE_ALL _r = f(a,b,c,d,e,z); CALLER_RESTORE_ALL; _r; })


/*
 * A PRIM_STGCALL is used when we have arranged to save the R<n>,
 * F<n>, and D<n> registers already, we only need the "system"
 * registers saved for us.  These are used in PrimOps, where the
 * compiler has a good idea of what registers are live, and so doesn't
 * need to save all of them.
 */

#define PRIM_STGCALL0(f) \
  CALLER_SAVE_SYSTEM (void) f(); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL1(f,a) \
  CALLER_SAVE_SYSTEM (void) f(a); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL2(f,a,b) \
  CALLER_SAVE_SYSTEM (void) f(a,b); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL3(f,a,b,c) \
  CALLER_SAVE_SYSTEM (void) f(a,b,c); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL4(f,a,b,c,d) \
  CALLER_SAVE_SYSTEM (void) f(a,b,c,d); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL5(f,a,b,c,d,e) \
  CALLER_SAVE_SYSTEM (void) f(a,b,c,d,e); CALLER_RESTORE_SYSTEM

#define PRIM_STGCALL6(f,a,b,c,d,e,z) \
  CALLER_SAVE_SYSTEM (void) f(a,b,c,d,e,z); CALLER_RESTORE_SYSTEM


#define RET_PRIM_STGCALL0(t,f) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL1(t,f,a) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL2(t,f,a,b) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a,b); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL3(t,f,a,b,c) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a,b,c); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL4(t,f,a,b,c,d) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a,b,c,d); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL5(t,f,a,b,c,d,e) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a,b,c,d,e); CALLER_RESTORE_SYSTEM; _r; })

#define RET_PRIM_STGCALL6(t,f,a,b,c,d,e,z) \
  ({ t _r; CALLER_SAVE_SYSTEM _r = f(a,b,c,d,e,z); CALLER_RESTORE_SYSTEM; _r; })

/* ToDo: ccalls that might garbage collect - do we need to return to
 * the scheduler to perform these?  Similarly, ccalls that might want
 * to call Haskell right back, or start a new thread or something.
 */

#endif /* CCALL_H */

