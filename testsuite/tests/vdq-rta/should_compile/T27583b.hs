{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}
module T27583b where

data T a where
  MkT :: forall a -> T a

pattern P :: T a
pattern P = MkT a
