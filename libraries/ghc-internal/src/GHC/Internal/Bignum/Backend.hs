{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Selected backend
module GHC.Internal.Bignum.Backend
   ( module Backend
   )
where

#if defined(BIGNUM_NATIVE)
import GHC.Internal.Bignum.Backend.Native as Backend

#elif defined(BIGNUM_GMP)
import GHC.Internal.Bignum.Backend.GMP as Backend

#else
#error Undefined BigNum backend. Use a flag to select it (e.g. gmp, native)`
#endif
