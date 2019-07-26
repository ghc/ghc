/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * Reduced-functionality entry point for standalone Haskell programs.
 *
 * See Note [Simple main] in RtsMain.c.
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdbool.h>

struct StgClosure_;

typedef enum {
    RtsOptsNone,         // +RTS causes an error
    RtsOptsIgnore,       // Ignore command line arguments
    RtsOptsIgnoreAll,    // Ignore command line and Environment arguments
    RtsOptsSafeOnly,     // safe RTS options allowed; others cause an error
    RtsOptsAll           // all RTS options allowed
  } RtsOptsEnabledEnum;

struct RtsSimpleConfig {
    RtsOptsEnabledEnum rts_opts_enabled;
    bool rts_opts_suggestions;
    bool keep_cafs;
    const char *rts_opts;
};

#if defined(__GNUC__)
// N.B. Don't use GNU_ATTRIBUTE to avoid dependency on Stg.h.
__attribute__((noreturn))
#endif
void hs_simple_main (int argc, char *argv[],
                     struct StgClosure_ *main_closure,
                     struct RtsSimpleConfig rts_config);

