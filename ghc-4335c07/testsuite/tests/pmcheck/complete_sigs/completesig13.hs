{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Completesig11 where

class LL f where
  go :: f a -> ()

instance LL [] where
  go _ = ()

pattern T :: LL f => f a
pattern T <- (go -> ())

{-# COMPLETE T :: [] #-}

foo :: [a] -> Int
foo T = 5
