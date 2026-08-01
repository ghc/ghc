{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms, TypeAbstractions #-}
module T27440e where

data T a where
  MkT :: forall a -> T a

pattern P :: a -> T a
pattern P x = MkT @a x
