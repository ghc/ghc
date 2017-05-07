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
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
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
 * DEBUG implies TRACING and TICKY_TICKY
 */
#if defined(DEBUG)
#if !defined(TRACING)
#define TRACING
#endif
#if !defined(TICKY_TICKY)
#define TICKY_TICKY
#endif
#endif

/* Statistical profiler: implied by TRACING for the time being */
#if defined(TRACING)
#define STAT_PROFILE 1
#endif

#if defined(STAT_PROFILE)
#define STAT_PROFILE_HEAP_SAMPLE_BUFFER_SIZE 4096
#define STAT_PROFILE_BLACKHOLE_SAMPLE_BUFFER_SIZE 4096
#endif

/*
 * Signals - supported on non-PAR versions of the runtime.  See RtsSignals.h.
 */
#define RTS_USER_SIGNALS 1

/* Profile spin locks */
#define PROF_SPIN
