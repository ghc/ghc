
{-# LANGUAGE RecursiveDo #-}

module Foo where

import Control.Monad.Fix

z :: Maybe [Int]
z = mdo x <- return (1:x)
        return (take 4 x)

