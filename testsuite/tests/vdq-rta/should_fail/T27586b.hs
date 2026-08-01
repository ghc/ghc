{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms #-}

module T27586b where

data T a where
  MkT :: forall a -> T a

pattern P x = MkT x
