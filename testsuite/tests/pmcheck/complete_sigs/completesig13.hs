{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Completesig11 where

class LL f where
  go :: f a -> ()

instance LL [] where
  go _ = ()

pattern T :: LL f => f a
pattern T <- (go -> ())

{-# COMPLETE T :: [a] #-}

-- No warning should be generated here
foo :: [t] -> Int
foo T = 5

data List a = Nil | Cons a (List a)

instance LL List where
  go _ = ()

-- This should be warned about, since the COMPLETE pragma above only applies to
-- the Prelude [] type, not List.
bar :: List t -> Int
bar T = 5
