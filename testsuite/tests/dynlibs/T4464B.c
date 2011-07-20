
#include <Rts.h>

extern void __stginit_T4464H(void);

void HsStart(void) {
    int argc = 3;
    char* argv[] = {"ghcDll", "+RTS", "-H50M", NULL}; // argv must end with NULL

    // Initialize Haskell runtime
    char** args = argv;
    hs_init(&argc, &args);

    // Tell Haskell about all root modules
    hs_add_root(__stginit_T4464H);
}

void HsEnd(void) {
    hs_exit();
}

