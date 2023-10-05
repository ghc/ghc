{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE OverloadedLists #-}

module Weird where

import Data.Sequence

f :: Seq Int -> ()
f [0] = ()

