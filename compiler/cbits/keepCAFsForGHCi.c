#include <Rts.h>
#include <ghcversion.h>

// Note [keepCAFsForGHCi]
// ~~~~~~~~~~~~~~~~~~~~~~
// This file is only included in the dynamic library.
// It contains an __attribute__((constructor)) function (run prior to main())
// which sets the keepCAFs flag in the RTS, before any Haskell code is run.
// This is required so that GHCi can use dynamic libraries instead of HSxyz.o
// files.
//
// For static builds we have to guarantee that the linker loads this object file
// to ensure the constructor gets run and not discarded. If the object is part of
// an archive and not otherwise referenced the linker would ignore the object.
// To avoid this:
// * When initializing a GHC session in initGhcMonad we assert keeping cafs has been
//   enabled by calling keepCAFsForGHCi.
// * This causes the GHC module from the ghc package to carry a reference to this object
//   file.
// * Which in turn ensures the linker doesn't discard this object file, causing
//   the constructor to be run, allowing the assertion to succeed in the first place
//   as keepCAFs will have been set already during initialization of constructors.



bool keepCAFsForGHCi(void) __attribute__((constructor));

bool keepCAFsForGHCi(void)
{
    bool was_set = keepCAFs;
    setKeepCAFs();
    return was_set;
}


