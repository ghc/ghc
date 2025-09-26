/*
 * (c) The GHC Team, 2025-2026
 *
 * RTS/ghc-internal interface
 *
 * See Note [RTS/ghc-internal interface].
 */

#include "Rts.h"

void init_ghc_hs_iface(void) __attribute__((constructor));

// Forward declarations
#define CLOSURE(module, symbol) \
    extern StgClosure ghczminternal_##module##_##symbol;

#define UNDEF_CLOSURE(module, symbol)

#define INFO_TBL(module, symbol) \
    extern StgInfoTable ghczminternal_##module##_##symbol;

#include "RtsIfaceSymbols.h"

#undef CLOSURE
#undef UNDEF_CLOSURE
#undef INFO_TBL

// HsIface definition
#define CLOSURE(module, symbol) \
    .symbol = &ghczminternal_##module##_##symbol,

#define UNDEF_CLOSURE(module, symbol) \
    .symbol = NULL,

#define INFO_TBL(module, symbol) \
    .symbol = &ghczminternal_##module##_##symbol,

static const HsIface the_ghc_hs_iface = {
#include "RtsIfaceSymbols.h"
};

void init_ghc_hs_iface(void)
{
    /*
     * N.B. ghc-internal may be load multiple times, e.g., when the
     * RTS linker is in use. For this reason we explicitly refuse to
     * override ghc_hs_iface if it has already been initialized.
     */
    if (ghc_hs_iface == NULL) {
        ghc_hs_iface = &the_ghc_hs_iface;
    }
}
