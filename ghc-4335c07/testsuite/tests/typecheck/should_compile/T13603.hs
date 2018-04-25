{-# Language PolyKinds, TypeInType, UndecidableInstances #-}
module T13603 where

import GHC.Exts (TYPE, RuntimeRep)

class        A (a :: TYPE rep)
class A a => B (a :: TYPE rep)

instance A b => A (a -> (b :: TYPE rep))
instance B b => B (a -> (b :: TYPE rep))
