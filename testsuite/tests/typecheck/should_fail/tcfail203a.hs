-- trac #2806

{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

module Foo where

import GHC.Base

fail10 = 'a'
    where !(b, ~(c, (I# x))) = (True, (False, 5))
