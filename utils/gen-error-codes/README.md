`gen-error-codes`
=================

This utility gives you new diagnostic codes to use if you
are adding a new error to GHC. See Note [Diagnostic codes]
in GHC.Types.Error.

To use:

    cabal run --with-compiler=path/to/your/built/ghc   # this will produce one new error code

    cabal run --with-compiler=path/to/your/built/ghc -- gen-error-codes <number>   # this will produce <number> new error codes

Error codes are generated pseudo-randomly in the range `1` through
`10^GHC.Types.Error.numDigitsInGhcDiagnosticCode - 1`, inclusive.
Error codes are guaranteed to be distinct from codes already in use, as discovered
by querying `usedDiagnosticCodes @GhcMessage ++ retiredDiagnosticCodes @GhcMessage`.
The seed for the pseudo-random number generator is computed from the set
of used error codes combined with the name of the git branch that is currently
checked out. This should allow collaborators to get the same codes generated,
but contributors on different branches will get different codes, unlikely to conflict.
