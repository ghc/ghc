/*
 * (c) The GHC Team, 2025-2026
 *
 * RTS/ghc-internal interface
 *
 *
 * Note [RTS/ghc-internal interface]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * The runtime system depends upon a variety of symbols defined by Haskell
 * modules living in `ghc-internal`. To avoid cyclic dependencies between
 * ghc-internal and the RTS, these symbols are referenced indirectly via the
 * the `HsIface` structure (specifically `ghc_hs_iface`):
 *
 *     struct HsIface {
 *         StgClosure *runIO; // GHC.Internal.TopHandler.runIO
 *         StgClosure *Z0T; // GHC.Internal.Tuple.()
 *         // etc.
 *     };
 *
 * `ghc_hs_iface` is initialized via the `init_ghc_hs_iface` function
 * in `ghc-internal` cbits, when it is invoked by `hs_init_ghc` during
 * GHC RTS initialization:
 *
 *     struct HsIface the_hs_iface = {
 *         .runIO = &ghczminternal_GHCziInternalziTopHandler_runIO_closure,
 *         .Z0T = &ghczminternal_GHCziInternalziTuple_Z0T_closure,
 *         // etc.
 *     };
 *
 *     void init_ghc_hs_iface(void) {
 *         ghc_hs_iface = &the_hs_iface;
 *     }
 *
 * This effectively breaks the RTS's link-time dependency, replacing it with a
 * run-time dependency at the cost of an indirection. It also has the pleasant
 * side-effect of making the interface between the RTS and `ghc-internal`
 * explicit.
 *
 * Note that the init_ghc_hs_iface symbol is explicitly included in the final
 * binary via the linker flag `-uinit_ghc_hs_iface`, when an executable is
 * linked against ghc-internal by GHC.
 *
 * `-uinit_ghc_hs_iface` ensures that the `init_ghc_hs_iface` symbol is
 * included in the final link, even when we link against `ghc-internal.a`
 * (since, otherwise, only objects members which provide undefined symbols
 * needed by the executable are included in the final object).
 *
 * CAUTION: there may be constructors invoked before hs_init_ghc, and
 * those constructors will see an uninitialized ghc_hs_iface! For now
 * we're fine: when GHC generates a constructor that references
 * certain closures or info tables, the generated references don't go
 * through ghc_hs_iface redirection, and they are not forward
 * references to other objects not compiled yet. Still, always keep
 * the no-ghc_hs_iface restriction in mind when doing work related to
 * constructors, be it the compiler-generated ones or explicitly
 * written ones in RTS.
 */

#include "Rts.h"

// This captures the symbols provided by ghc-internal which
// are needed by the RTS.
HsIface *ghc_hs_iface = NULL;
