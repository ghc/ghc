{-# LANGUAGE TypeOperators #-}
module T15457 where

import Data.Type.Equality

data a ! b; infix 0 !
data a + b; infix 9 +

fixityProof :: (Int ! Int + Int) :~: (Int ! (Int + Int))
fixityProof = Refl

data Foo a b = MkFoo (a ! b) !Int !(Bool ! Bool)
