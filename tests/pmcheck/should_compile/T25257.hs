{-# LANGUAGE OverloadedLists, RebindableSyntax #-}
{-# OPTIONS -Wincomplete-patterns #-}

module T25257 where

import Prelude (Bool(..), IO, print)
import GHC.Exts (IsList(fromListN, toList))

null :: [a] -> Bool
null [] = True
null (_:_) = False
