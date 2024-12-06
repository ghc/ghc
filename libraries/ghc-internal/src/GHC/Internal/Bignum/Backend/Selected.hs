{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Selected backend
--
-- We need this module in addition to GHC.Internal.Bignum.Backend to avoid module loops with
-- Check backend.
module GHC.Internal.Bignum.Backend.Selected
   ( module Backend
   )
where

#if defined(BIGNUM_NATIVE)
import GHC.Internal.Bignum.Backend.Native as Backend

#elif defined(BIGNUM_FFI)
import GHC.Internal.Bignum.Backend.FFI as Backend

#elif defined(BIGNUM_GMP)
import GHC.Internal.Bignum.Backend.GMP as Backend

#else
#error Undefined BigNum backend. Use a flag to select it (e.g. gmp, native, ffi)`
#endif
