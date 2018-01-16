{-# LANGUAGE GADTs, KindSignatures, PatternSynonyms #-}
module ShouldCompile where

data T a b where
  MkT :: a -> T a Bool

pattern P :: T Bool b
pattern P <- MkT True

pattern D :: a -> T (Maybe a) Bool
pattern D x = MkT (Just x)
