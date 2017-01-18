{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Completesig15 where

class C f where
  foo :: f a -> ()

pattern P :: C f => f a
pattern P <- (foo -> ())

{-# COMPLETE P #-}
