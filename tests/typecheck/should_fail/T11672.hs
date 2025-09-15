{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module BadError where

import GHC.TypeLits
import Data.Proxy

f :: Proxy (a :: Symbol) -> Int
f _ = f (Proxy :: Proxy (Int -> Bool))
