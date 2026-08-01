{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}
module T27583d where

data T a where
  MkT :: forall a -> a -> T a

-- A required type argument is checked against the signature, with or without
-- the 'type' herald. The variable/wildcard patterns x, _, (type x), (type _)
-- can't mismatch the signature. Concrete types are in T27583e and T27583f.

pattern P1 :: Int -> T Int
pattern P1 n = MkT x n

pattern P2 :: Int -> T Int
pattern P2 n = MkT (type x) n

pattern P3 :: Int -> T Int
pattern P3 n = MkT _ n

pattern P4 :: Int -> T Int
pattern P4 n = MkT (type _) n
