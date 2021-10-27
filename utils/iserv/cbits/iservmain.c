#include <ghcversion.h>
#if MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)
#  include <rts/PosixSource.h>
#else // PosixSource.h not yet exposed, hacky inline for now.
#  include <ghcplatform.h>
#  if defined(solaris2_HOST_OS)
#  define _POSIX_C_SOURCE 200112L
#  define _XOPEN_SOURCE   600
#  else
#  define _POSIX_C_SOURCE 200809L
#  define _XOPEN_SOURCE   700
#  endif
#endif
#include <Rts.h>

#include <HsFFI.h>

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;

    // We never know what symbols GHC will look up in the future, so
    // we must retain CAFs for running interpreted code.
    conf.keep_cafs = 1;

    conf.rts_opts_enabled = RtsOptsAll;
    extern StgClosure ZCMain_main_closure;
    hs_main(argc, argv, &ZCMain_main_closure, conf);
}
