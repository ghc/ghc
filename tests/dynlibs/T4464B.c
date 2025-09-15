
#include <Rts.h>

void HsStart(void) {
    int argc = 3;
    char* argv[] = {"ghcDll", "+RTS", "-H50M", NULL}; // argv must end with NULL

    // Initialize Haskell runtime
    char** args = argv;
    {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RTSOPTS; // RTSOPTS defined on the
                                         // command line with -DRTSOPTS=...
        hs_init_ghc(&argc, &args, conf);
    }
}

void HsEnd(void) {
    hs_exit();
}

