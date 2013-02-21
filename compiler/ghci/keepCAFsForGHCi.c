#include "Rts.h"

// This file is only included in the dynamic library.
// It contains an __attribute__((constructor)) function (run prior to main())
// which sets the keepCAFs flag in the RTS, before any Haskell code is run.
// This is required so that GHCi can use dynamic libraries instead of HSxyz.o
// files.

static void keepCAFsForGHCi(void) __attribute__((constructor));

static void keepCAFsForGHCi(void)
{
    keepCAFs = 1;
}

