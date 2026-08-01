{-# LANGUAGE PatternSynonyms, TypeAbstractions #-}
module T27440b where

data T a = MkT a

pattern P :: a -> T a
pattern P x = MkT @a x
