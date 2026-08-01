{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}
module T27583f where

data T a where
  MkT :: forall a -> a -> T a

-- A required type argument is checked against the signature, with or without
-- the 'type' herald. Here the type written in that position does not agree
-- with the signature. T27583e is the same four patterns with a type that does.

pattern P1 :: Int -> T Int
pattern P1 n = MkT Bool n

pattern P2 :: Int -> T Int
pattern P2 n = MkT (type Bool) n

pattern P3 :: Maybe Int -> T (Maybe Int)
pattern P3 n = MkT (Maybe Bool) n

pattern P4 :: Maybe Int -> T (Maybe Int)
pattern P4 n = MkT (type (Maybe Bool)) n
