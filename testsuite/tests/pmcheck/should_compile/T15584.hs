{-# LANGUAGE EmptyCase #-}
{-# OPTIONS -Wincomplete-patterns #-}
module T15584 where

import Data.Void

data V = MkV !Void
data    S1 = MkS1 !V
newtype S2 = MkS2 V

s1 :: S1 -> a
s1 x = case x of {}

s2 :: S2 -> a
s2 x = case x of {}
