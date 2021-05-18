{-# LANGUAGE PatternSynonyms #-}
module Mod where

data T = C

pattern D = C

{-# COMPLETE D :: T #-}
