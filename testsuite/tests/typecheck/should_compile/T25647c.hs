{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T25647c where

import GHC.Exts
import Data.Kind

-- testing the behavior of anonymous wildcards in type family instances

-- class position wildcard matching an non-type variable
-- free position wildcard in the type family instance
class CW2 a b where
  type TW2 a b c
  fun2 :: TW2 a b c -> Int
instance CW2 Int Int where
  type TW2 _ Int _ = Int
  fun2 :: TW2 Int Int Int -> Int
  fun2 _ = 1

-- class wildcard matching an type variable
class CW5 a b where
  type TW5 a b
  fun5 :: TW5 a b -> Int
instance CW5 a Int where
  type TW5 _ Int = Int
  fun5 :: TW5 a Int -> Int
  fun5 _ = 1

-- class position signature wildcard matching non-type variable
class CW3 (a :: RuntimeRep) b where
  type TW3 (b :: TYPE a)
  fun3 :: TW3 b -> Int
instance CW3 'IntRep Int# where
  type TW3 (_ :: _) = Int
  fun3 :: TW3 Int# -> Int
  fun3 _ = 1

-- free wildcard, class wildcard, both position signature wildcard
class CW4 b where
  type TW4 a b
  fun4 :: TW4 a b -> TW4 d b -> Int
instance CW4 Int# where
  type TW4 (_ :: _) (_ :: _) = Int
  fun4 :: Int -> Int -> Int
  fun4 1 _ = 1

-- class position wildcard matching type variable
-- class position signature wildcard matching type variable
-- free position signature wildcard
class CW7 (a :: RuntimeRep) b where
  type TW7 (b :: TYPE a) (c :: RuntimeRep) (d::TYPE c)
  fun7 :: TW7 b c d-> Int
  funa7 :: TW7 b c d -> Int
instance CW7 aa bb where
  type TW7 (_::TYPE _) _ _ = Int
  funa7 :: TW7 Int 'IntRep d -> Int
  funa7 _ = 1
  fun7 :: TW7 Int#  LiftedRep d -> Int
  fun7 _ = 1

-- class position wildcard matching non-type-variable
-- class position signature wildcard matching non-type-variable
-- free position signature wildcard
class CW8 (a :: RuntimeRep) b where
  type TW8 (b :: TYPE a) (c :: RuntimeRep) (d::TYPE c)
  fun8 :: TW8 b c d -> Int
instance CW8 'IntRep Int# where
  type TW8 (_::TYPE _) _ _ = Int
  fun8 :: TW8 Int# LiftedRep Int -> Int
  fun8 _ = 1
