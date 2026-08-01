{-# LANGUAGE ExistentialQuantification, PatternSynonyms, TypeAbstractions #-}
module T27440d where

data S = forall a. Show a => MkS a

pattern P :: Show a => a -> S
pattern P x = MkS @a x
