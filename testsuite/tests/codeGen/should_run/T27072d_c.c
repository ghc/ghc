// Test that GHC-generated module initializers and finalizer registrations
// work correctly on Darwin.
//
// On Darwin, GHC lowers finalizers as __cxa_atexit registrations from an
// initializer placed in __DATA,__mod_init_func (see Note [Finalizers via
// __cxa_atexit] in GHC.Types.ForeignStubs).
//
// This test verifies the mechanism by checking that:
//  1. The SPT initializer runs at load time (entries are inserted).
//  2. The SPT finalizer (registered via __cxa_atexit from __mod_init_func)
//     fires during exit() and removes the entries.
//
// We verify (2) by registering our own __cxa_atexit checker from a
// constructor in a dylib that is loaded before the main executable's
// initializers run. Since __cxa_atexit handlers fire in LIFO order,
// a handler registered earlier runs later — so our checker runs after the
// GHC-generated finalizer, and can observe that SPT entries were removed.
//
// The Apple linker does not support --wrap, so this is the Darwin
// equivalent of T27072w's approach.

#include "Rts.h"
#include <stdio.h>

extern int hs_spt_key_count(void);

int main(int argc, char *argv[]) {
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &argv, conf);

    printf("SPT entries after init: %d\n", hs_spt_key_count());
    fflush(stdout);

    // Do NOT call hs_exit(). Return normally so __cxa_atexit handlers fire,
    // which includes the GHC-generated finalizer registered during init.
    return 0;
}
