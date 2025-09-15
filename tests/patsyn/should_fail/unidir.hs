{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

pattern Head x = x:_
