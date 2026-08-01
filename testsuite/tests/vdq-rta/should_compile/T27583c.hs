{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}
module T27583c where

data T a where
  MkT :: forall a -> T a

pattern P :: T Int
pattern P = MkT (type Int)
