{-# LANGUAGE PatternSynonyms, MonoLocalBinds #-}
module T20485a where

import Data.Type.Equality

f :: a :~: b -> a -> b
f Refl x = x

pattern ReflPat = Refl

g :: a :~: b -> a -> b
g ReflPat x = x
