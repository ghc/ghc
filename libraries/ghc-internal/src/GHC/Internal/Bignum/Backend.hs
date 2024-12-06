{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Selected backend
module GHC.Internal.Bignum.Backend
   ( module Backend
   )
where

#if defined(BIGNUM_CHECK)
import GHC.Internal.Bignum.Backend.Check    as Backend
#else
import GHC.Internal.Bignum.Backend.Selected as Backend
#endif

