{-# language PatternSynonyms #-}
module T16682a where

pattern Unit = ()

{-# complete Unit #-}

f Unit = () -- No warnings
