
#include <Rts.h>

extern void __stginit_T4464H(void);

void HsStart(void) {
    int argc = 3;
    char* argv[] = {"ghcDll", "+RTS", "-H50M", NULL}; // argv must end with NULL

    // Initialize Haskell runtime
    char** args = argv;
#if __GLASGOW_HASKELL__ >= 703
    {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RTSOPTS; // RTSOPTS defined on the
                                         // command line with -DRTSOPTS=...
        hs_init_ghc(&argc, &args, conf);
    }
#else
    hs_init(&argc, &args);
#endif

    // Tell Haskell about all root modules
    hs_add_root(__stginit_T4464H);
}

void HsEnd(void) {
    hs_exit();
}

