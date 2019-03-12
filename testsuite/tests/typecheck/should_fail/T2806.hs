
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wunbanged-strict-patterns #-}

-- #2806

module Foo where

import GHC.Base

foo :: Int
foo = 3
    where (I# _x) = 4
