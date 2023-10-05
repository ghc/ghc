{-# language PatternSynonyms #-}

module PatSynExistential where

pattern P :: () => forall x. x -> Maybe x
pattern P <- _
