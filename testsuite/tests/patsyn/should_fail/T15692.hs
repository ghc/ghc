{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module T15692 where

data F x where
  FS :: F (f a) -> F a

pattern FS' = FS False
