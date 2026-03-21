// Test that GHC-generated finalizers actually run on wasm32
//
// We use --wrap=hs_spt_remove to intercept calls from the GHC-generated
// finalizer and verify they happen during exit().

#include "Rts.h"
#include <stdio.h>

extern int hs_spt_key_count(void);

// --wrap=hs_spt_remove: the linker redirects all calls to hs_spt_remove
// through our wrapper, and provides __real_hs_spt_remove for the original.
extern void __real_hs_spt_remove(StgWord64 key[2]);

void __wrap_hs_spt_remove(StgWord64 key[2]) {
    printf("finalizer: hs_spt_remove called\n");
    fflush(stdout);
    __real_hs_spt_remove(key);
}

int main(int argc, char *argv[]) {
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &argv, conf);

    printf("SPT entries after init: %d\n", hs_spt_key_count());
    fflush(stdout);

    // Do NOT call hs_exit(). Return normally so exit() fires the
    // __cxa_atexit registered handlers.
    return 0;
}
