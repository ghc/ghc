/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Rts settings.
 *
 * NOTE: assumes #include "ghcconfig.h"
 *
 * NB: THIS FILE IS INCLUDED IN NON-C CODE AND DATA!  #defines only please.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(TICKY_TICKY) && defined(THREADED_RTS)
#error TICKY_TICKY is incompatible with THREADED_RTS
#endif

/*
 * Whether the runtime system will use libbfd for debugging purposes.
 */
#if defined(DEBUG) && defined(HAVE_BFD_H) && defined(HAVE_LIBBFD) && !defined(_WIN32)
#define USING_LIBBFD 1
#endif

/*
 * We previously only offer the eventlog in a subset of RTS ways; we now
 * enable it unconditionally to simplify packaging. See #18948.
 */
#define TRACING

/* DEBUG implies TICKY_TICKY */
#if defined(DEBUG)
#if !defined(TICKY_TICKY)
#define TICKY_TICKY
#endif
#endif


/* -----------------------------------------------------------------------------
   Signals - supported on non-PAR versions of the runtime.  See RtsSignals.h.
   -------------------------------------------------------------------------- */

/*
Note [Lack of signals on wasm32-wasi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the wasm32-wasi spec, there is no process/thread model, no
preemptive execution, and no posix-style asynchronous signals. See
WASI ticket #166 for upstream discussion about signal handling.

In wasi-libc, signal.h includes an #error pragma in the default
setting that says: "wasm lacks signal support; to enable minimal
signal emulation, compile with -D_WASI_EMULATED_SIGNAL and link with
-lwasi-emulated-signal". It is possible to enable these flags when
configuring wasm32-wasi-ghc, but it still makes little sense, since
the emulation is really primitive and we might as well just stop
pretending signals exist at all.

Therefore, in the entire GHC tree, whenever we define functionality
related to posix signals, we should add the CPP guards. When the
target lacks signals, we can't set signal handlers at all, and raising
a signal should degrade to exiting with the signal number as the exit
code.
*/

#if defined(HAVE_SIGNAL_H)
#define RTS_USER_SIGNALS 1
#endif

/* Profile spin locks */

#if defined(DEBUG)
#define PROF_SPIN
#endif

#if defined(THREADED_RTS)
/*
 * See Note [Capabilities array sizing] in rts/Capability.c.
 * Update the note in docs/users_guide/using-concurrent.rst when updating this.
 */
#define MAX_N_CAPABILITIES 256
#else
#define MAX_N_CAPABILITIES 1
#endif
