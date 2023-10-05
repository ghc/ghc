{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Selected backend
module GHC.Num.Backend
   ( module Backend
   )
where

#if defined(BIGNUM_CHECK)
import GHC.Num.Backend.Check    as Backend
#else
import GHC.Num.Backend.Selected as Backend
#endif

