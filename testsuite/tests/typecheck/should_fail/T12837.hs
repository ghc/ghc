{-# LANGUAGE FlexibleInstances #-}

module T12837 where

import GHC.TypeLits
import Data.Typeable

data K = K

instance Typeable K
instance KnownNat n
instance KnownSymbol n
