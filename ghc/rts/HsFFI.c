/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2002
 *
 * RTS entry points as mandated by the FFI addendum to the Haskell 98 report
 *
 * ---------------------------------------------------------------------------*/

#include "HsFFI.h"
#include "Rts.h"

// hs_init and hs_exit are defined in RtsStartup.c

void
hs_set_argv(int argc, char *argv[])
{
    prog_argc = argc;
    prog_argv = argv;
}

void
hs_perform_gc(void)
{
    /* Hmmm, the FFI spec is a bit vague, but it seems to imply a major GC... */
    performMajorGC();
}

void
hs_free_stable_ptr(HsStablePtr sp)
{
    /* The cast is for clarity only, both HsStablePtr and StgStablePtr are
       typedefs for void*. */
    freeStablePtr((StgStablePtr)sp);
}

void
hs_free_fun_ptr(HsFunPtr fp)
{
    /* I simply *love* all these similar names... */
    freeHaskellFunctionPtr(fp);
}
