/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005
 *
 * Macros for THREADED_RTS support
 *
 * -------------------------------------------------------------------------- */

#ifndef SMPCLOSUREOPS_H
#define SMPCLOSUREOPS_H

#if defined(THREADED_RTS)

/* -----------------------------------------------------------------------------
 * Locking/unlocking closures
 *
 * This is used primarily in the implementation of MVars.
 * -------------------------------------------------------------------------- */

#define SPIN_COUNT 4000

#ifdef KEEP_LOCKCLOSURE
// We want a callable copy of lockClosure() so that we can refer to it
// from .cmm files compiled using the native codegen.
extern StgInfoTable *lockClosure(StgClosure *p);
INLINE_ME
#else
INLINE_HEADER
#endif
StgInfoTable *
lockClosure(StgClosure *p)
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

INLINE_HEADER void
unlockClosure(StgClosure *p, const StgInfoTable *info)
{
    // This is a strictly ordered write, so we need a write_barrier():
    write_barrier();
    p->header.info = info;
}

#else /* !THREADED_RTS */

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{ return (StgInfoTable *)p->header.info; }

INLINE_HEADER void
unlockClosure(StgClosure *p STG_UNUSED, const StgInfoTable *info STG_UNUSED)
{ /* nothing */ }

#endif /* THREADED_RTS */

// Handy specialised versions of lockClosure()/unlockClosure()
INLINE_HEADER void lockTSO(StgTSO *tso)
{ lockClosure((StgClosure *)tso); }

INLINE_HEADER void unlockTSO(StgTSO *tso)
{ unlockClosure((StgClosure*)tso, (const StgInfoTable *)&stg_TSO_info); }

#endif /* SMPCLOSUREOPS_H */
