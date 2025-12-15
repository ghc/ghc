{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeFamilies #-}

module T12819 where

type family F a  -- F :: * -> *
data T :: (* -> *) -> *

pattern Q :: T F -> String
pattern Q x <- (undefined -> x)
