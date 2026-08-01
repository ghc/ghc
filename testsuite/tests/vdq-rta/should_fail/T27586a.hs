{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}

module T27586a where

data T a where
  MkT :: forall a -> T a

pattern P :: Int -> T Int
pattern P x = MkT x
