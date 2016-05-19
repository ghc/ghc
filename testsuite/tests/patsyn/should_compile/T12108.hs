{-# LANGUAGE PatternSynonyms #-}

module T12108 where

type Endo a = a -> a

pattern Id :: Endo a
pattern Id x = x
