{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Selected backend
--
-- We need this module in addition to GHC.Num.Backend to avoid module loops with
-- Check backend.
module GHC.Num.Backend.Selected
   ( module Backend
   )
where

#if defined(BIGNUM_NATIVE)
import GHC.Num.Backend.Native as Backend

#elif defined(BIGNUM_FFI)
import GHC.Num.Backend.FFI as Backend

#elif defined(BIGNUM_GMP)
import GHC.Num.Backend.GMP as Backend

#else
#error Undefined BigNum backend. Use a flag to select it (e.g. gmp, native, ffi)`
#endif
