{-# LANGUAGE RebindableSyntax #-}
module T15607 where

import Prelude hiding (pure, return)

t = do { pure 5 }
