{-# OPTIONS_GHC -fshow-hole-constraints #-}
{-# LANGUAGE GADTs, TypeOperators #-}
module HoleConstraints where
import Data.Type.Equality hiding (castWith)

-- "from the signature of f1"
f1 :: Eq a => a
f1 = _

-- "from the signature of f2", only once
f2 :: (Show a, Eq a) => a
f2 = _

-- "from the instance declaration"
class C a where f3 :: a
instance Eq a => C [a] where f3 = _

-- "from a pattern with constructor ... in an equation for 'castWith'"
castWith :: a :~: b -> a -> b
castWith Refl x = _

data AnyShow where
  AnyShow :: Show a => a -> AnyShow

-- "from a pattern with constructor ... in a case alternative"
foo :: AnyShow -> String
foo a = case a of AnyShow x -> _
