#include <stddef.h>

/*
 * GHC sometimes include references to symbols found in `base` package: e.g. to
 * support unboxed sums (cf absentSumFieldError) or pattern match errors (cf
 * patError). Usually it's not an issue because we always link or preload
 * `base`.
 *
 * But for ghc-bignum it is an issue: `base` depends on `ghc-bignum` to provide
 * Integer/Natural. Hence we get a cyclic dependency: ghc-bignum <--> base
 *
 * In general it's still not an issue because linkers support this when .a/.so
 * are used. But to make GHCi faster, we provide some prelinked objects (e.g.
 * HSghc-bignum-1.0.o) and those are the ones that don't work properly: the
 * linker chokes on missing symbols from `base`. See #17791
 *
 * An approach that works is to define those symbols here as `weak` symbols just
 * to break the cycle and to make the linker happy.
 */

void * base_ControlziExceptionziBase_patError_closure __attribute__((weak)) = NULL;
void * base_ControlziExceptionziBase_patError_info __attribute__((weak)) = NULL;
void * base_ControlziExceptionziBase_absentSumFieldError_closure __attribute__((weak)) = NULL;
