{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}
module T27583a where

data T a where
  MkT :: forall a -> T a

pattern P :: T a
pattern P = MkT (type a)
