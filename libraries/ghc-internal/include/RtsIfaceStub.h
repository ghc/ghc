#include <stdio.h>
void __attribute__((weak)) init_ghc_hs_iface(void) {
    fprintf(stderr, "init_ghc_hs_iface: weak constructor stub for init_ghc_hs_iface was not overriden by the actual constructor. Obviously wrong! This stub should never make it into the final object except for in the Cabal checkForeignDeps configuration checks!");
};
