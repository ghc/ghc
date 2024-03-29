/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005
 *
 * RTS entry points as mandated by the FFI section of the Haskell report
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "HsFFI.h"

#include "StablePtr.h"
#include "Task.h"

// hs_init and hs_exit are defined in RtsStartup.c

void
hs_set_argv(int argc, char *argv[])
{
    setProgArgv(argc,argv);
}

void
hs_perform_gc(void)
{
    /* Hmmm, the FFI spec is a bit vague, but it seems to imply a blocking major GC... */
    performBlockingMajorGC();
}

// Lock the stable pointer table
void hs_lock_stable_ptr_table (void)
{
    stablePtrLock();
}

// Deprecated version of hs_lock_stable_ptr_table
void hs_lock_stable_tables (void)
{
    stablePtrLock();
}

// Unlock the stable pointer table
void hs_unlock_stable_ptr_table (void)
{
    stablePtrUnlock();
}

// Deprecated version of hs_unlock_stable_ptr_table
void hs_unlock_stable_tables (void)
{
    stablePtrUnlock();
}

void
hs_free_stable_ptr(HsStablePtr sp)
{
    /* The cast is for clarity only, both HsStablePtr and StgStablePtr are
       typedefs for void*. */
    freeStablePtr((StgStablePtr)sp);
}

void
hs_free_stable_ptr_unsafe(HsStablePtr sp)
{
    /* The cast is for clarity only, both HsStablePtr and StgStablePtr are
       typedefs for void*. */
    freeStablePtrUnsafe((StgStablePtr)sp);
}

void
hs_free_fun_ptr(HsFunPtr fp)
{
    /* I simply *love* all these similar names... */
    freeHaskellFunctionPtr(fp);
}

void
hs_thread_done(void)
{
    freeMyTask();
}
