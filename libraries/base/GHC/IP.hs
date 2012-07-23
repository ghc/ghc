{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module GHC.IP (IP(..)) where

import GHC.TypeLits

-- | The syntax @?x :: a@ is desugared into @IP "x" a@
class IP (x :: Symbol) a | x -> a where
  ip :: a


