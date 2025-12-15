{-# LANGUAGE DataKinds, FunctionalDependencies, UndecidableInstances

#-}

module T9106 where

import GHC.TypeLits

class FunctorN (n :: Nat) f (a :: *) (fa :: *) | n f a -> fa where

instance FunctorN 0 f a a where

instance FunctorN n f a (f fa)

