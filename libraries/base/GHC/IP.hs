{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | /Since: 4.6.0.0/
module GHC.IP (IP(..)) where

import GHC.TypeLits

-- | The syntax @?x :: a@ is desugared into @IP "x" a@
class IP (x :: Symbol) a | x -> a where
  ip :: a


