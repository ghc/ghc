{-# LANGUAGE UnboxedTuples #-}

module T11736 where

import Data.Proxy

foo :: Proxy (#,#)
foo = Proxy
