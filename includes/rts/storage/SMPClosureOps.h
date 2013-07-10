/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005-2013
 *
 * Macros for THREADED_RTS support
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_STORAGE_SMPCLOSUREOPS_H
#define RTS_STORAGE_SMPCLOSUREOPS_H

#ifdef CMINUSMINUS

/* Lock closure, equivalent to ccall lockClosure but the condition is inlined.
 * Arguments are swapped for uniformity with unlockClosure. */
#if defined(THREADED_RTS)
#define LOCK_CLOSURE(closure, info)                             \
    if (CInt[n_capabilities] == 1 :: CInt) {                    \
        info = GET_INFO(closure);                               \
    } else {                                                    \
        ("ptr" info) = ccall reallyLockClosure(closure "ptr");  \
    }
#else
#define LOCK_CLOSURE(closure, info) info = GET_INFO(closure)
#endif

#define unlockClosure(ptr,info)                 \
    prim_write_barrier;                         \
    StgHeader_info(ptr) = info;

#else

INLINE_HEADER StgInfoTable *lockClosure(StgClosure *p);
EXTERN_INLINE StgInfoTable *reallyLockClosure(StgClosure *p);
EXTERN_INLINE StgInfoTable *tryLockClosure(StgClosure *p);
EXTERN_INLINE void unlockClosure(StgClosure *p, const StgInfoTable *info);

#if defined(THREADED_RTS)

/* -----------------------------------------------------------------------------
 * Locking/unlocking closures
 *
 * This is used primarily in the implementation of MVars.
 * -------------------------------------------------------------------------- */

// We want a callable copy of reallyLockClosure() so that we can refer to it
// from .cmm files compiled using the native codegen, so these are given
// EXTERN_INLINE.  C-- should use LOCK_CLOSURE not lockClosure, so we've
// kept it INLINE_HEADER.
EXTERN_INLINE StgInfoTable *reallyLockClosure(StgClosure *p)
{
    StgWord info;
    do {
        nat i = 0;
        do {
            info = xchg((P_)(void *)&p->header.info, (W_)&stg_WHITEHOLE_info);
            if (info != (W_)&stg_WHITEHOLE_info) return (StgInfoTable *)info;
        } while (++i < SPIN_COUNT);
        yieldThread();
    } while (1);
}

INLINE_HEADER StgInfoTable *lockClosure(StgClosure *p)
{
    if (n_capabilities == 1) {
        return (StgInfoTable *)p->header.info;
    }
    else {
        return reallyLockClosure(p);
    }
}

// ToDo: consider splitting tryLockClosure into reallyTryLockClosure,
// same as lockClosure
EXTERN_INLINE StgInfoTable *tryLockClosure(StgClosure *p)
{
    StgWord info;
    if (n_capabilities == 1) {
        return (StgInfoTable *)p->header.info;
    }
    else {
        info = xchg((P_)(void *)&p->header.info, (W_)&stg_WHITEHOLE_info);
        if (info != (W_)&stg_WHITEHOLE_info) {
            return (StgInfoTable *)info;
        } else {
            return NULL;
        }
    }
}

#else /* !THREADED_RTS */

EXTERN_INLINE StgInfoTable *
reallyLockClosure(StgClosure *p)
{ return (StgInfoTable *)p->header.info; }

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{ return (StgInfoTable *)p->header.info; }

EXTERN_INLINE StgInfoTable *
tryLockClosure(StgClosure *p)
{ return (StgInfoTable *)p->header.info; }

#endif /* THREADED_RTS */

EXTERN_INLINE void unlockClosure(StgClosure *p, const StgInfoTable *info)
{
    // This is a strictly ordered write, so we need a write_barrier():
    write_barrier();
    p->header.info = info;
}

// Handy specialised versions of lockClosure()/unlockClosure()
INLINE_HEADER void lockTSO(StgTSO *tso);
INLINE_HEADER void lockTSO(StgTSO *tso)
{ lockClosure((StgClosure *)tso); }

INLINE_HEADER void unlockTSO(StgTSO *tso);
INLINE_HEADER void unlockTSO(StgTSO *tso)
{ unlockClosure((StgClosure*)tso, (const StgInfoTable *)&stg_TSO_info); }

#endif /* CMINUSMINUS */

#endif /* RTS_STORAGE_SMPCLOSUREOPS_H */
