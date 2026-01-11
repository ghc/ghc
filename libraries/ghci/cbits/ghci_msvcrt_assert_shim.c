#include <stdio.h>
#include <stdlib.h>

/*
  ghci_msvcrt_assert_shim.c

  Why this exists:
  - When running the Windows iserv under wine, the interactive linker loads
    import libraries such as libmingwex.a from the llvm-mingw toolchain.
  - libmingwex.a references the import thunk __imp___msvcrt_assert, and GHC's
    in-tree libraries reference _assert. Under our cross + wine setup, those
    symbols are not resolved by the dynamic loader, even though the toolchain
    targets UCRT for normal operation.
  - The resulting unresolved symbol prevents the interactive linker from
    resolving further dependencies, breaking GHCi (e.g. missing
    disableBuffering).

  What this does:
  - Provide a minimal fallback implementation that prints a message and aborts.
  - Export __imp___msvcrt_assert as a pointer to that implementation.
  - Do NOT export _assert: llvm-mingw's libmingwex already provides it, and
    exporting our own would produce duplicate-definition errors in the
    interactive linker when loading larger packages (e.g. -package ghc).

  This is a local shim only used by the ghci library. It does NOT force loading
  msvcrt.dll; it simply satisfies the import thunk at link/load time so the
  interactive linker can proceed.
*/

static void ghci_msvcrt_assert_fallback(const char *msg, const char *file, unsigned int line)
{
  if (msg != NULL && file != NULL) {
    fprintf(stderr, "assertion failed: %s, file %s, line %u\n", msg, file, line);
  }
  abort();
}

__declspec(dllexport) void (*__imp___msvcrt_assert)(const char *, const char *, unsigned int) =
  ghci_msvcrt_assert_fallback;

/*
  Dummy symbol to ensure this object is linked into libHSghci.
*/
__declspec(dllexport) void ghci_msvcrt_assert_shim(void)
{
  /* no-op */
}
