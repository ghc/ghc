Extending ghc-bignum
====================

We can easily add a new backend to ghc-bignum:

1. Backends only have to provide the implementation for some BigNat functions.

2. The documentation of these BigNat functions is given in the FFI backend
   source file but is valid for every backend.

3. Backends can always use the Natie backend as a fallback implementation (e.g.
   to implement functions they don't provide). The Native backend is always
   compiled in.

4. Backend selection is easily done via CPP and cabal flags.
