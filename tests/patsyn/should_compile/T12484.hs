{-# OPTIONS_GHC -Wmissing-pattern-synonym-signatures #-}
{-# LANGUAGE PatternSynonyms #-}

module T12484 where

pattern RP x = (x, True)

