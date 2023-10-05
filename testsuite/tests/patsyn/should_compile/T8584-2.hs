{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE PatternSynonyms #-}

module ShouldCompile where

pattern Single :: Show a => a -> [a]
pattern Single x = [x]

f :: (Show a) => [a] -> a
f (Single x) = x
